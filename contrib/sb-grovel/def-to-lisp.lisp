(in-package :SB-GROVEL)
(defvar *export-symbols* nil)

(defun c-for-structure (stream lisp-name c-struct)
  (destructuring-bind (c-name &rest elements) c-struct
    (format stream "printf(\"(sb-grovel::define-c-struct ~A %d)\\n\",sizeof (~A));~%" lisp-name c-name)
    (dolist (e elements)
      (destructuring-bind (lisp-type lisp-el-name c-type c-el-name) e
        (format stream "printf(\"(sb-grovel::define-c-accessor ~A-~A ~A ~A \");~%"
                lisp-name lisp-el-name lisp-name lisp-type)
        ;; offset
        (format stream "{ ~A t;printf(\"%d \",((unsigned long)&(t.~A)) - ((unsigned long)&(t)) ); }~%"
                c-name c-el-name)
        ;; length
        (format stream "{ ~A t;printf(\"%d\",(sizeof t.~A));}~%"
                c-name c-el-name)
        (format stream "printf(\")\\n\");~%")))))

(defun c-for-function (stream lisp-name alien-defn)
  (destructuring-bind (c-name &rest definition) alien-defn
    (let ((*print-right-margin* nil))
      (format stream "printf(\"(cl:declaim (cl:inline ~A))\\n\");~%"
              lisp-name)
      (princ "printf(\"(sb-grovel::define-foreign-routine (" stream)
      (princ "\\\"" stream) (princ c-name stream) (princ "\\\" " stream)
      (princ lisp-name stream)
      (princ " ) " stream)
      (terpri stream)
      (dolist (d definition)
        (write d :length nil
               :right-margin nil :stream stream)
        (princ " " stream))
      (format stream ")\\n\");")
      (terpri stream))))


(defun print-c-source (stream headers definitions package-name)
  (let ((*print-right-margin* nil))
    (format stream "#define SIGNEDP(x) (((x)-1)<0)~%")
    (format stream "#define SIGNED_(x) (SIGNEDP(x)?\"\":\"un\")~%")
    (loop for i in headers
          do (format stream "#include <~A>~%" i))
    (format stream "main() { ~%
printf(\"(in-package ~S)\\\n\");~%" package-name)  
    (format stream "printf(\"(cl:deftype int () '(%ssigned-byte %d))\\\n\",SIGNED_(int),8*sizeof (int));~%")
    (format stream "printf(\"(cl:deftype char () '(unsigned-byte %d))\\\n\",SIGNED_(char),8*sizeof (char));~%")
    (format stream "printf(\"(cl:deftype long () '(unsigned-byte %d))\\\n\",SIGNED_(long),8*sizeof (long));~%")
    (dolist (def definitions)
      (destructuring-bind (type lispname cname &optional doc) def
        (cond ((eq type :integer)
               (format stream
                       "printf(\"(cl:defconstant ~A %d \\\"~A\\\")\\\n\",~A);~%"
                       lispname doc cname))
	      ((eq type :type)
	       (format stream
                       "printf(\"(sb-alien:define-alien-type ~A (sb-alien:%ssigned %d))\\\n\",SIGNED_(~A),8*(sizeof(~A)));~%"
		       lispname cname cname))
              ((eq type :string)
               (format stream
                       "printf(\"(cl:defvar ~A %S \\\"~A\\\")\\\n\",~A);~%"
                     lispname doc cname))
              ((eq type :function)
               (c-for-function stream lispname cname))
              ((eq type :structure)
               (c-for-structure stream lispname cname))
              (t
               (format stream
                       "printf(\";; Non hablo Espagnol, Monsieur~%")))))
    (format stream "exit(0);~%}")))

(defun c-constants-extract  (filename output-file package)
  (with-open-file (f output-file :direction :output)
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
     (= (run-shell-command "gcc -o ~S ~S" (namestring tmp-a-dot-out)
	 (namestring tmp-c-source)) 0)
     (= (run-shell-command "~A >~A"
			   (namestring tmp-a-dot-out)
			   (namestring tmp-constants)) 0)
     (compile-file tmp-constants :output-file output-file))))

