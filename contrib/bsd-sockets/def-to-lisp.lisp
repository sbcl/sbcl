(in-package :BSD-SOCKETS-SYSTEM)
(defvar *export-symbols* nil)

(defun c-for-structure (stream lisp-name c-struct)
  (destructuring-bind (c-name &rest elements) c-struct
    (format stream "printf(\"(define-c-struct ~A %d)\\n\",sizeof (~A));~%" lisp-name c-name)
    (dolist (e elements)
      (destructuring-bind (lisp-type lisp-el-name c-type c-el-name) e
        (format stream "printf(\"(define-c-accessor ~A-~A ~A ~A \");~%"
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
      (format stream "printf(\"(declaim (inline ~A))\\n\");~%"
              lisp-name)
      (princ "printf(\"(def-foreign-routine (" stream)
      (princ "\\\"" stream) (princ c-name stream) (princ "\\\" " stream)
      (princ lisp-name stream)
      (princ " ) " stream)
      (dolist (d definition)
        (write d :length nil
               :right-margin nil :stream stream)
        (princ " " stream))
      (format stream ")\\n\");")
      (terpri stream))))


(defun print-c-source (stream headers definitions package-name)
  ;(format stream "#include \"struct.h\"~%")
  (let ((*print-right-margin* nil))
    (loop for i in headers
          do (format stream "#include <~A>~%" i))
    (format stream "main() { ~%
printf(\"(in-package ~S)\\\n\");~%" package-name)  
    (format stream "printf(\"(defconstant size-of-int %d)\\\n\",sizeof (int));~%")
    (format stream "printf(\"(defconstant size-of-char %d)\\\n\",sizeof (char));~%")
    (format stream "printf(\"(defconstant size-of-long %d)\\\n\",sizeof (long));~%")
    (dolist (def definitions)
      (destructuring-bind (type lispname cname &optional doc) def
        (cond ((eq type :integer)
               (format stream
                       "printf(\"(defconstant ~A %d \\\"~A\\\")\\\n\",~A);~%"
                       lispname doc cname))
              ((eq type :string)
               (format stream
                       "printf(\"(defvar ~A %S \\\"~A\\\")\\\n\",~A);~%"
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
