(in-package #:sb-grovel)

(defvar *default-c-stream* nil)

(defun escape-for-string (string)
  (c-escape string))

(defun c-escape (string &optional (dangerous-chars '(#\")) (escape-char #\\))
  "Escape DANGEROUS-CHARS in STRING, with ESCAPE-CHAR."
  (coerce (loop for c across string
	        if (member c dangerous-chars) collect escape-char
  	        collect c)
	  'string))

(defun as-c (&rest args)
  "Pretty-print ARGS into the C source file, separated by #\Space"
  (format *default-c-stream* "~A~{ ~A~}~%" (first args) (rest args)))

(defun printf (formatter &rest args)
  "Emit C code to printf the quoted code, via FORMAT.
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
	   "    printf (\"~@?\\n\"~@{, ~A~});~%"
	   (c-escape formatter)
	   args)))

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
    (as-c "int main() {")
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
	  (:integer
	   (as-c "#ifdef" cname)
	   (printf "(cl:defconstant ~A %d \"~A\")" lispname doc
		   cname)
	   (as-c "#else")
	   (printf "(sb-int:style-warn \"Couldn't grovel for ~A (unknown to the C compiler).\")" cname)
	   (as-c "#endif"))
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
	   ;; FIXME: structure slots should be auto-exportable as well.
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
  ((package :accessor constants-package :initarg :package)))

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
	 (tmp-a-dot-out (merge-pathnames #p"a.out" real-output-file))
	 (tmp-constants (merge-pathnames #p"constants.lisp-temp"
					 real-output-file)))
    (princ (list filename output-file real-output-file
		 tmp-c-source tmp-a-dot-out tmp-constants))
    (terpri)
    (funcall (intern "C-CONSTANTS-EXTRACT" (find-package "SB-GROVEL"))
	     filename tmp-c-source (constants-package component))
    (and		
     (= (run-shell-command "gcc ~A -o ~S ~S"
			   (if (sb-ext:posix-getenv "EXTRA_CFLAGS")
			       (sb-ext:posix-getenv "EXTRA_CFLAGS")
				"")
			   (namestring tmp-a-dot-out)
			   (namestring tmp-c-source)) 0)
     (= (run-shell-command "~A >~A"
			   (namestring tmp-a-dot-out)
			   (namestring tmp-constants)) 0)
     (compile-file tmp-constants :output-file output-file))))
