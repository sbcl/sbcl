;;; -*-  Lisp -*-
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-grovel))
(defpackage #:sb-bsd-sockets-system (:use #:asdf #:sb-grovel #:cl))
(in-package #:sb-bsd-sockets-system)

;;; we also have a shared library with some .o files in it

(defclass unix-dso (module) ())
(defun unix-name (pathname)
  (namestring 
   (typecase pathname
     (logical-pathname (translate-logical-pathname pathname))
     (t pathname))))

(defmethod asdf::input-files ((operation compile-op) (dso unix-dso))
  (mapcar #'component-pathname (module-components dso)))

(defmethod output-files ((operation compile-op) (dso unix-dso))
  (let ((dir (component-pathname dso)))
    (list
     (make-pathname :type "so"
		    :name (car (last (pathname-directory dir)))
		    :directory (butlast (pathname-directory dir))
		    :defaults dir))))


(defmethod perform :after ((operation compile-op) (dso unix-dso))
  (let ((dso-name (unix-name (car (output-files operation dso)))))
    (unless (zerop
	     (run-shell-command
	      #+sunos "gcc -shared -lresolv -lsocket -lnsl -o ~S ~{~S ~}"
	      #-sunos "gcc -shared -o ~S ~{~S ~} "
	      dso-name
	      (mapcar #'unix-name
		      (mapcan (lambda (c)
				(output-files operation c))
			      (module-components dso)))))
      (error 'operation-error :operation operation :component dso))))

;;; if this goes into the standard asdf, it could reasonably be extended
;;; to allow cflags to be set somehow
(defmethod output-files ((op compile-op) (c c-source-file))
  (list 
   (make-pathname :type "o" :defaults
		  (component-pathname c))))
(defmethod perform ((op compile-op) (c c-source-file))
  (unless
      (= 0 (run-shell-command "gcc -fPIC -o ~S -c ~S"
			      (unix-name (car (output-files op c)))
			      (unix-name (component-pathname c))))
    (error 'operation-error :operation op :component c)))

(defmethod perform ((operation load-op) (c c-source-file))
  t)
  
(defmethod perform ((o load-op) (c unix-dso))
  (let ((co (make-instance 'compile-op)))
    (let ((filename (car (output-files co c))))
      #+cmu (ext:load-foreign filename)
      #+sbcl (sb-alien:load-1-foreign filename))))

(defsystem sb-bsd-sockets
    :version "0.58"
    :depends-on (sb-rt sb-grovel)
    :components ((:file "defpackage")
		 (:file "split" :depends-on ("defpackage"))
                 (:file "array-data" :depends-on ("defpackage"))
		 (:unix-dso "alien"
			    :components ((:c-source-file "undefs")
					 (:c-source-file "get-h-errno")))
		 (:file "malloc" :depends-on ("defpackage"))
		 (:file "foreign-glue" :depends-on ("defpackage" "malloc"))
		 (sb-grovel:grovel-constants-file
		  "constants"
		  :package :sockint  :pathname "constants.lisp"
		  :depends-on  ("def-to-lisp" "defpackage" "foreign-glue"))
		 (:file "sockets"
			:depends-on ("constants" "array-data"))
		 
		 (:file "sockopt" :depends-on ("sockets"))
		 (:file "inet" :depends-on ("sockets" "split"  "constants" ))
		 (:file "local" :depends-on ("sockets" "split" "constants" ))
		 (:file "name-service" :depends-on ("sockets" "constants" "alien"))
		 (:file "misc" :depends-on ("sockets" "constants"))

		 (:file "def-to-lisp")
		 (:file "tests" :depends-on ("inet" "sockopt"))

		 (:static-file "NEWS")
		 ;; (:static-file "INSTALL")
		 (:static-file "README")
		 (:static-file "index" :pathname "index.html")
		 (:static-file "doc" :pathname "doc.lisp")
		 (:static-file "TODO")))

(defmethod perform ((o test-op) (c (eql (find-system :sb-bsd-sockets))))
  (or (funcall (intern "DO-TESTS" (find-package "SB-RT")))
      (error "test-op failed")))

