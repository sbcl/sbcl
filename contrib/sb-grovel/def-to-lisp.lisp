(in-package #:sb-grovel)

(defvar *default-c-stream* nil)

(defun escape-for-string (string)
  (c-escape string))

(defun split-cflags (string)
  (remove-if (lambda (flag)
               (zerop (length flag)))
             (loop
                for start = 0 then (if end (1+ end) nil)
                for end = (and start (position #\Space string :start start))
                while start
                collect (subseq string start end))))

(defun c-escape (string &optional (dangerous-chars '(#\")) (escape-char #\\))
  "Escape DANGEROUS-CHARS in STRING, with ESCAPE-CHAR."
  (declare (simple-string string))
  (coerce (loop for c across string
                if (member c dangerous-chars) collect escape-char
                collect c)
          'string))

(defun as-c (&rest args)
  "Pretty-print ARGS into the C source file, separated by #\Space"
  (format *default-c-stream* "~A~{ ~A~}~%" (first args) (rest args)))

(defun printf (formatter &rest args)
  "Emit C code to fprintf the quoted code, via FORMAT.
The first argument is the C string that should be passed to
printf.

The rest of the arguments are consumed by FORMAT clauses, until
there are no more FORMAT clauses to fill. If there are more
arguments, they are emitted as printf arguments.

There is no error checking done, unless you pass too few FORMAT
clause args. I recommend using this formatting convention in
code:

 (printf \"string ~A ~S %d %d\" format-arg-1 format-arg-2
         printf-arg-1 printf-arg-2)"
  (let ((*print-pretty* nil))
    (apply #'format *default-c-stream*
           "    fprintf (out, \"~@?\\n\"~@{, ~A~});~%"
           (c-escape formatter)
           args)))

(defun c-for-enum (lispname elements export)
  (printf "(cl:eval-when (:compile-toplevel :load-toplevel :execute) (sb-alien:define-alien-type ~A (sb-alien:enum nil" lispname)
  (dolist (element elements)
    (destructuring-bind (lisp-element-name c-element-name) element
      (printf " (~S %d)" lisp-element-name c-element-name)))
  (printf ")))")
  (when export
    (dolist (element elements)
      (destructuring-bind (lisp-element-name c-element-name) element
        (declare (ignore c-element-name))
        (unless (keywordp lisp-element-name)
          (printf "(export '~S)" lisp-element-name))))))

(defun c-for-structure (lispname cstruct)
  (destructuring-bind (cname &rest elements) cstruct
    (printf "(cl:eval-when (:compile-toplevel :load-toplevel :execute) (sb-grovel::define-c-struct ~A %d" lispname
            (format nil "sizeof(~A)" cname))
    (dolist (e elements)
      (destructuring-bind (lisp-type lisp-el-name c-type c-el-name &key distrust-length) e
        (printf " (~A ~A \"~A\"" lisp-el-name lisp-type c-type)
        ;; offset
        (as-c "{" cname "t;")
        (printf "  %d"
                (format nil "((unsigned long)&(t.~A)) - ((unsigned long)&(t))" c-el-name))
        (as-c "}")
        ;; length
        (if distrust-length
            (printf "  0)")
            (progn
              (as-c "{" cname "t;")
              (printf "  %d)"
                      (format nil "sizeof(t.~A)" c-el-name))
              (as-c "}")))))
    (printf "))")))

(defun print-c-source (stream headers definitions package-name)
  (declare (ignorable definitions package-name))
  (let ((*default-c-stream* stream)
        (*print-right-margin* nil))
    (loop for i in (cons "stdio.h" headers)
          do (format stream "#include <~A>~%" i))
    (as-c "#define SIGNEDP(x) (((x)-1)<0)")
    (as-c "#define SIGNED_(x) (SIGNEDP(x)?\"\":\"un\")")
    (as-c "int main(int argc, char *argv[]) {")
    (as-c "    FILE *out;")
    (as-c "    if (argc != 2) {")
    (as-c "        printf(\"Invalid argcount!\");")
    (as-c "        return 1;")
    (as-c "    } else")
    (as-c "        out = fopen(argv[1], \"w\");")
    (as-c "    if (!out) {")
    (as-c "        printf(\"Error opening output file!\");")
    (as-c "        return 1;")
    (as-c "    }")
    (printf "(cl:in-package #:~A)" package-name)
    (printf "(cl:eval-when (:compile-toplevel)")
    (printf "  (cl:defparameter *integer-sizes* (cl:make-hash-table))")
    (dolist (type '("char" "short" "long" "int"
                    #+nil"long long" ; TODO: doesn't exist in sb-alien yet
                    ))
      (printf "  (cl:setf (cl:gethash %d *integer-sizes*) 'sb-alien:~A)" (substitute #\- #\Space type)
              (format nil "sizeof(~A)" type)))
    (printf ")")
    (dolist (def definitions)
      (destructuring-bind (type lispname cname &optional doc export) def
        (case type
          ((:integer :errno)
           (as-c "#ifdef" cname)
           (printf "(cl:defconstant ~A %d \"~A\")" lispname doc
                   cname)
           (when (eql type :errno)
             (printf "(cl:setf (get '~A 'errno) t)" lispname))
           (as-c "#else")
           (printf "(sb-int:style-warn \"Couldn't grovel for ~A (unknown to the C compiler).\")" cname)
           (as-c "#endif"))
          (:enum
           (c-for-enum lispname cname export))
          (:type
           (printf "(cl:eval-when (:compile-toplevel :load-toplevel :execute) (sb-alien:define-alien-type ~A (sb-alien:%ssigned %d)))" lispname
                   (format nil "SIGNED_(~A)" cname)
                   (format nil "(8*sizeof(~A))" cname)))
          (:string
           (printf "(cl:defparameter ~A %s \"~A\"" lispname doc
                   cname))
          (:function
           (printf "(cl:declaim (cl:inline ~A))" lispname)
           (destructuring-bind (f-cname &rest definition) cname
             (printf "(sb-grovel::define-foreign-routine (\"~A\" ~A)" f-cname lispname)
             (printf "~{  ~W~^\\n~})" definition)))
          (:structure
           (c-for-structure lispname cname))
          (otherwise
           ;; should we really not sprechen espagnol, monsieurs?
           (error "Unknown grovel keyword encountered: ~A" type)))
        (when export
          (printf "(cl:export '~A)" lispname))))
    (as-c "return 0;")
    (as-c "}")))

(defun c-constants-extract  (filename output-file package)
  (with-open-file (f output-file :direction :output :if-exists :supersede)
    (with-open-file (i filename :direction :input)
      (let* ((headers (read i))
             (definitions (read i)))
        (print-c-source  f headers definitions package)))))

(defclass grovel-constants-file (asdf:cl-source-file)
  ((package :accessor constants-package :initarg :package)
   (do-not-grovel :accessor do-not-grovel
                  :initform nil
                  :initarg :do-not-grovel)))

(define-condition c-compile-failed (compile-failed) ()
  (:report (lambda (c s)
             (format s "~@<C compiler failed when performing ~A on ~A.~@:>"
                     (error-operation c) (error-component c)))))
(define-condition a-dot-out-failed (compile-failed) ()
  (:report (lambda (c s)
             (format s "~@<a.out failed when performing ~A on ~A.~@:>"
                     (error-operation c) (error-component c)))))

(defmethod asdf:perform ((op asdf:compile-op)
                         (component grovel-constants-file))
  ;; we want to generate all our temporary files in the fasl directory
  ;; because that's where we have write permission.  Can't use /tmp;
  ;; it's insecure (these files will later be owned by root)
  (let* ((output-file (car (output-files op component)))
         (filename (component-pathname component))
         (real-output-file
          (if (typep output-file 'logical-pathname)
              (translate-logical-pathname output-file)
              (pathname output-file)))
         (tmp-c-source (merge-pathnames #p"foo.c" real-output-file))
         (tmp-a-dot-out (merge-pathnames #-win32 #p"a.out" #+win32 #p"a.exe"
                                         real-output-file))
         (tmp-constants (merge-pathnames #p"constants.lisp-temp"
                                         real-output-file)))
    (princ (list filename output-file real-output-file
                 tmp-c-source tmp-a-dot-out tmp-constants))
    (terpri)
    (funcall (intern "C-CONSTANTS-EXTRACT" (find-package "SB-GROVEL"))
             filename tmp-c-source (constants-package component))
    (unless (do-not-grovel component)
      (let* ((cc (or (and (string/= (sb-ext:posix-getenv "CC") "")
                          (sb-ext:posix-getenv "CC"))
                     ;; It might be nice to include a CONTINUE or
                     ;; USE-VALUE restart here, but ASDF seems to insist
                     ;; on handling the errors itself.
                     (error "The CC environment variable has not been set in SB-GROVEL. Since this variable should always be set during the SBCL build process, this might indicate an SBCL with a broken contrib installation.")))
             (code (sb-ext:process-exit-code
                    (sb-ext:run-program
                     cc
                     (append
                      (split-cflags (sb-ext:posix-getenv "EXTRA_CFLAGS"))
                      #+(and linux largefile)
                      '("-D_LARGEFILE_SOURCE"
                        "-D_LARGEFILE64_SOURCE"
                        "-D_FILE_OFFSET_BITS=64")
                      #+(and x86-64 darwin inode64)
                      '("-arch" "x86_64"
                        "-mmacosx-version-min=10.5"
                        "-D_DARWIN_USE_64_BIT_INODE")
                      #+(and x86-64 darwin (not inode64))
                      '("-arch" "x86_64"
                        "-mmacosx-version-min=10.4")
                      #+(and x86 darwin)
                      '("-arch" "i386"
                        "-mmacosx-version-min=10.4")
                      #+(and x86-64 sunos) '("-m64")
                      (list "-o"
                            (namestring tmp-a-dot-out)
                            (namestring tmp-c-source)))
                     :search t
                     :input nil
                     :output *trace-output*))))
        (unless (= code 0)
          (case (operation-on-failure op)
            (:warn (warn "~@<C compiler failure when performing ~A on ~A.~@:>"
                         op component))
            (:error
             (error 'c-compile-failed :operation op :component component)))))
      (let ((code (sb-ext:process-exit-code
                   (sb-ext:run-program (namestring tmp-a-dot-out)
                                       (list (namestring tmp-constants))
                                       :search nil
                                       :input nil
                                       :output *trace-output*))))
        (unless (= code 0)
          (case (operation-on-failure op)
            (:warn (warn "~@<a.out failure when performing ~A on ~A.~@:>"
                         op component))
            (:error
             (error 'a-dot-out-failed :operation op :component component))))))
    (multiple-value-bind (output warnings-p failure-p)
        (compile-file tmp-constants :output-file output-file)
      (when warnings-p
        (case (operation-on-warnings op)
          (:warn (warn
                  (formatter "~@<COMPILE-FILE warned while ~
                              performing ~A on ~A.~@:>")
                  op component))
          (:error (error 'compile-warned :component component :operation op))
          (:ignore nil)))
      (when failure-p
        (case (operation-on-failure op)
          (:warn (warn
                  (formatter "~@<COMPILE-FILE failed while ~
                              performing ~A on ~A.~@:>")
                  op component))
          (:error (error 'compile-failed :component component :operation op))
          (:ignore nil)))
      (unless output
        (error 'compile-error :component component :operation op)))))

