;;; -*-  Lisp -*-
(require :sb-grovel)
(defpackage #:sb-posix-system (:use #:asdf #:cl #:sb-grovel))
(in-package #:sb-posix-system)


;;; we also have a shared library with some .o files in it
;;;
;;; FIXME: we share this with SB-BSD-SOCKETS.  This should either (a)
;;; be part of ASDF itself, or (b) be in a shared file that we can
;;; LOAD at this point.
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
	      "gcc ~A -o ~S ~{~S ~}"
	      (concatenate 'string
			   (sb-ext:posix-getenv "EXTRA_LDFLAGS")
			   " "
			   #+sunos "-shared -lresolv -lsocket -lnsl"
			   #+darwin "-bundle"
			   #-(or darwin sunos) "-shared")
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
      (= 0 (run-shell-command "gcc ~A -o ~S -c ~S"
			      (concatenate
			       'string
			       (sb-ext:posix-getenv "EXTRA_CFLAGS")
			       " "
			       "-fPIC")
			      (unix-name (car (output-files op c)))
			      (unix-name (component-pathname c))))
    (error 'operation-error :operation op :component c)))

(defmethod perform ((operation load-op) (c c-source-file))
  t)
  
(defmethod perform ((o load-op) (c unix-dso))
  (let ((co (make-instance 'compile-op)))
    (let ((filename (car (output-files co c))))
      #+cmu (ext:load-foreign filename)
      #+sbcl (sb-alien:load-shared-object filename))))

(defsystem sb-posix
    :depends-on (sb-grovel)
    #+sb-building-contrib :pathname
    #+sb-building-contrib "SYS:CONTRIB;SB-POSIX;"
    :components ((:file "defpackage")
		 (:file "designator" :depends-on ("defpackage"))
                 (:unix-dso "alien"
			    :components ((:c-source-file "stat-macros")
					 (:c-source-file "waitpid-macros")))
		 (:file "macros" :depends-on ("designator"))
		 (sb-grovel:grovel-constants-file
		  "constants"
		  :package :sb-posix :depends-on  ("defpackage"))
		 (:file "interface" :depends-on ("constants" "macros" "designator" "alien"))))

(defsystem sb-posix-tests
    :depends-on (sb-rt)
    :components ((:file "posix-tests")))

(defmethod perform :after ((o load-op) (c (eql (find-system :sb-posix))))
  (provide 'sb-posix))

(defmethod perform ((o test-op) (c (eql (find-system :sb-posix))))
  (operate 'load-op 'sb-posix-tests)
  (operate 'test-op 'sb-posix-tests))

(defmethod perform ((o test-op) (c (eql (find-system :sb-posix-tests))))
  (funcall (intern "DO-TESTS" (find-package "SB-RT")))
  (let ((failures (funcall (intern "PENDING-TESTS" "SB-RT")))
	(ignored-failures (loop for sym being the symbols of :sb-posix-tests
			        if (search ".ERROR" (symbol-name sym))
			        collect sym)))
    (cond
      ((null failures)
       t)	     
      ((null (set-difference failures ignored-failures))
       (warn "~@<some POSIX implementations return incorrect error values for ~
              failing calls, but there is legitimate variation between ~
              implementations too.  If you think the errno ~
              from your platform is valid, please contact the sbcl ~
              developers; otherwise, please submit a bug report to your ~
              kernel distributor~@:>")
       t)
      (t
       (error "non-errno tests failed!")))))
