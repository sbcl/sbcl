(in-package #:sb-grovel)

(defvar *default-c-stream* nil)

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

(defun word-cast (arg)
  (format nil "CAST_SIGNED(~A)" arg))

#+(and win32 x86-64)
(defun printf-transform-long-long (x)
  (with-output-to-string (str)
    (loop for previous = #\a then char
          for char across x
          do
          (write-char char str)
          (when (and (char= previous #\%)
                     (char= char #\l))
            (write-char #\l str)))))

#-(and win32 x86-64)
(defun printf-transform-long-long (x)
  x)

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

 (printf \"string ~A ~S %ld %ld\" format-arg-1 format-arg-2
         (word-cast printf-arg-1) (word-cast printf-arg-2))"
  (let ((*print-pretty* nil))
    (apply #'format *default-c-stream*
           "    fprintf(out, \"~@?\\n\"~@{, ~A~});~%"
           (printf-transform-long-long
            (c-escape formatter))
           args)))

(defun c-for-enum (lispname elements export)
  (printf "(sb-alien:define-alien-type ~A (sb-alien:enum nil" lispname)
  (dolist (element elements)
    (destructuring-bind (lisp-element-name c-element-name) element
      (printf " (~S %ld)" lisp-element-name (word-cast c-element-name))))
  (printf "))")
  (when export
    (dolist (element elements)
      (destructuring-bind (lisp-element-name c-element-name) element
        (declare (ignore c-element-name))
        (unless (keywordp lisp-element-name)
          (printf "(export '~S)" lisp-element-name))))))

(defun c-for-structure (lispname cstruct)
  (destructuring-bind (cname &rest elements) cstruct
    (printf "(sb-grovel::define-c-struct ~A %ld" lispname
            (word-cast (format nil "sizeof(~A)" cname)))
    (dolist (e elements)
      (destructuring-bind (lisp-type lisp-el-name c-type c-el-name &key distrust-length) e
        (printf " (~A ~A \"~A\"" lisp-el-name lisp-type c-type)
        ;; offset
        (as-c "{" cname "t;")
        (printf "  %lu"
                (format nil "((unsigned long~A)&(t.~A)) - ((unsigned long~A)&(t))"
                        #+(and win32 x86-64) " long" #-(and win32 x86-64) ""
                        c-el-name
                        #+(and win32 x86-64) " long" #-(and win32 x86-64) ""))
        (as-c "}")
        ;; length
        (if distrust-length
            (printf "  0)")
            (progn
              (as-c "{" cname "t;")
              (printf "  %ld)"
                      (word-cast (format nil "sizeof(t.~A)" c-el-name)))
              (as-c "}")))))
    (printf ")")))

(defun print-c-source (stream headers definitions package-name)
  (declare (ignorable definitions package-name))
  (let ((*default-c-stream* stream)
        (*print-right-margin* nil))
    (loop for i in (cons "stdio.h" headers)
          do (format stream "#include <~A>~%" i))
    (as-c "#define SIGNEDP(x) (((x)-1)<0)")
    (as-c "#define SIGNED_(x) (SIGNEDP(x)?\"\":\"un\")")
    ;; KLUDGE: some win32 constants are unsigned even though the
    ;; functions accepting them declare them as signed
    ;; e.g. ioctlsocket and FIONBIO.
    ;; Cast to signed long when possible.
    ;; Other platforms do not seem to be affected by this,
    ;; but we used to cast everything to INT on x86-64, preserve that behaviour.
    #+(and win32 x86-64)
    (as-c "#define CAST_SIGNED(x) ((sizeof(x) == 4)? (long long) (long) (x): (x))")
    #+(and (not win32) x86-64)
    (as-c "#define CAST_SIGNED(x) ((sizeof(x) == 4)? (long) (int) (x): (x))")
    ;; the C compiler on x86 macOS warns that %ld is the wrong format for an int
    ;; even though 'long' and 'int' are both 4 bytes.
    #-(or x86 64-bit)
    (as-c "#define CAST_SIGNED(x) ((int) (x))")
    #+(and (not x86-64) (or x86 64-bit))
    (as-c "#define CAST_SIGNED(x) ((long) (x))")
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
    (printf "(cl:eval-when (:compile-toplevel :execute)")
    (printf "  (cl:defparameter *integer-sizes* (cl:make-hash-table))")
    (dolist (type '("char" "short" "long long" "long" "int"))
      (printf "  (cl:setf (cl:gethash %ld *integer-sizes*) 'sb-alien:~A)" (substitute #\- #\Space type)
              (word-cast (format nil "sizeof(~A)" type))))
    (printf ")")
    (dolist (def definitions)
      (destructuring-bind (type lispname cname &optional doc export) def
        (case type
          ((:integer :errno)
           (as-c "#ifdef" cname)
           (printf "(cl:defconstant ~A %ld~@[ \"~A\"~])" lispname doc
                   (word-cast cname))
           (when (eql type :errno)
             (printf "(cl:setf (get '~A 'errno) t)" lispname))
           (as-c "#else")
           (printf "(sb-int:style-warn \"Couldn't grovel for ~~A (unknown to the C compiler).\" \"~A\")" cname)
           (as-c "#endif"))
          ((:integer-no-check)
           (printf "(cl:defconstant ~A %ld~@[ \"~A\"~])" lispname doc (word-cast cname)))
          (:enum
           (c-for-enum lispname cname export))
          (:type
           (printf "(sb-alien:define-alien-type ~A (sb-alien:%ssigned %ld))" lispname
                   (format nil "SIGNED_(~A)" cname)
                   (word-cast (format nil "(8*sizeof(~A))" cname))))
          (:string
           (printf "(cl:defparameter ~A %s \"~A\"" lispname doc
                   cname))
          (:function
           (printf "(cl:declaim (cl:inline ~A))" lispname)
           (destructuring-bind (f-cname &rest definition) cname
             (printf "(sb-alien:define-alien-routine (\"~A\" ~A)" f-cname lispname)
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

(defun run-c-compiler (sourcefile exefile)
  (let ((cc (or (and (string/= (sb-ext:posix-getenv "CC") "")
                     (sb-ext:posix-getenv "CC"))
                (if (member :sb-building-contrib *features*)
                    (error "~@<The CC environment variable not set during ~
                            SB-GROVEL build.~:@>")
                    (sb-int:style-warn
                     "CC environment variable not set, SB-GROVEL falling back to \"cc\"."))
                "cc")))
    (sb-ext:process-exit-code
     (sb-ext:run-program
      cc
      (append
       (split-cflags (sb-ext:posix-getenv "EXTRA_CFLAGS"))
       #+(and linux largefile)
       '("-D_LARGEFILE_SOURCE" "-D_LARGEFILE64_SOURCE" "-D_FILE_OFFSET_BITS=64")
       #+64-bit-time
       '("-D_TIME_BITS=64")
       #+(and (or x86 ppc sparc) (or linux freebsd)) '("-m32")
       #+(and x86-64 darwin inode64)
       `("-arch" "x86_64" ,(format nil "-mmacosx-version-min=~A"
                                   (or (sb-ext:posix-getenv "SBCL_MACOSX_VERSION_MIN")
                                       "10.6"))
                 "-D_DARWIN_USE_64_BIT_INODE")
       #+(and x86-64 darwin (not inode64))
       '("-arch" "x86_64" "-mmacosx-version-min=10.4")
       #+(and ppc darwin)
       `("-arch" "ppc"
         ,(format nil "-mmacosx-version-min=~A"
                  (or (sb-ext:posix-getenv "SBCL_MACOSX_VERSION_MIN")
                      "10.4")))
       #+(and x86-64 sunos) '("-m64")
       (list "-o" (namestring exefile) (namestring sourcefile)))
      :search t
      :input nil
      :output *trace-output*))))

;;; Extract constants as specified from INPUT, creating file OUTPUT.
;;; Very important: This OUTPUT formal parameter should ** literally ** be
;;; be named OUTPUT, and not OUTPUT-FILE.  WAT ???
;;; Well, OUTPUT-FILE is the name of a class exported from ASDF and we might do
;;; (USE-PACKAGE "ASDF") *after* having loaded our 'defpackage' without ASDF.
;;; A subsequent USE-PACKAGE would cause a symbol conflict. How can there ever
;;; be a subsequent USE-PACKAGE?  Because some _other_ system decides that it
;;; wants to do the following actions:
;;;  - load system "A" using a non-ASDF system loading tool
;;;    that wants to use SB-GROVEL
;;;  - load ASDF, which pushes :ASDF on *FEATURES*
;;;  - use ASDF to load system "B" which claims to need ASDF to use SB-GROVEL
;;;  - which re-loads SB-GROVEL, which loads our DEFPACKAGE
;;;  - which causes the aforementioned failure.
;;; Now you might think that pushing "SB-GROVEL" on to *MODULES* the first time
;;; would fix things, but it doesn't - because we actually _do_ have to reload
;;; this file, in order to pull in the CLOS stuff.  This problem can't be
;;; fixed until the outside worlds' systems either all use ASDF or not.
;;; Which translates to: this won't be fixed correctly, ever.
;;;
;;; TLDR: this stupid restriction on a variable's name makes
;;; something work that didn't used to work. And victory is ours!
#+asdf
(progn
(defun c-constants-extract  (input output package)
  (with-open-file (f output :direction :output :if-exists :supersede)
    (with-open-file (i input :direction :input)
      (let* ((headers (read i))
             (definitions (read i)))
        (print-c-source  f headers definitions package)))))

(defclass grovel-constants-file (cl-source-file)
  ((package :accessor constants-package :initarg :package)
   (do-not-grovel :accessor do-not-grovel
                  :initform nil
                  :initarg :do-not-grovel)))
(defclass asdf::sb-grovel-constants-file (grovel-constants-file) ())

(define-condition c-compile-failed (compile-file-error)
  ((description :initform "C compiler failed")))
(define-condition a-dot-out-failed (compile-file-error)
  ((description :initform "a.out failed")))

(defmethod perform ((op compile-op)
                    (component grovel-constants-file))
  ;; we want to generate all our temporary files in the fasl directory
  ;; because that's where we have write permission.  Can't use /tmp;
  ;; it's insecure (these files will later be owned by root)
  (let* ((output-files (output-files op component))
         (output-file (first output-files))
         (warnings-file (second output-files))
         (filename (component-pathname component))
         (context-format "~/asdf-action::format-action/")
         (context-arguments `((,op . ,component)))
         (condition-arguments `(:context-format ,context-format
                                :context-arguments ,context-arguments))
         (real-output-file
          (if (typep output-file 'logical-pathname)
              (translate-logical-pathname output-file)
              (pathname output-file)))
         (tmp-c-source (merge-pathnames #p"foo.c" real-output-file))
         (tmp-a-dot-out (merge-pathnames #-win32 #p"a.out" #+win32 #p"a.exe"
                                         real-output-file))
         (tmp-constants (merge-pathnames #p"constants.lisp-temp"
                                         real-output-file)))
    (c-constants-extract filename tmp-c-source (constants-package component))
    (unless (do-not-grovel component)
      (let ((code (run-c-compiler tmp-c-source tmp-a-dot-out)))
        (unless (= code 0)
          (apply 'error 'c-compile-failed condition-arguments)))
      (let ((code (sb-ext:process-exit-code
                   (sb-ext:run-program (namestring tmp-a-dot-out)
                                       (list (namestring tmp-constants))
                                       :search nil
                                       :input nil
                                       :output *trace-output*))))
        (unless (= code 0)
          (apply 'error 'a-dot-out-failed condition-arguments)))
    (multiple-value-bind (output warnings-p failure-p)
        (compile-file* tmp-constants :output-file output-file :warnings-file warnings-file)
      (check-lisp-compile-results output warnings-p failure-p context-format context-arguments)))))
) ; end PROGN
