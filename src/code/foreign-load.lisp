;;;; Loading shared object files

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!ALIEN")

(define-unsupported-fun load-foreign
    "Unsupported as of SBCL 0.8.13. See LOAD-SHARED-OBJECT."
  "~S is unsupported as of SBCL 0.8.13. See LOAD-SHARED-OBJECT." 
  (load-foreign))
  
(define-unsupported-fun load-1-foreign
    "Unsupported as of SBCL 0.8.13. Please use LOAD-SHARED-OBJECT."
  "~S is unsupported as of SBCL 0.8.13. Please use LOAD-SHARED-OBJECT."
  (load-1-foreign))

(define-alien-routine dlopen system-area-pointer
  (file c-string) (mode int))

(define-alien-routine dlclose int
  (handle system-area-pointer))

(define-alien-routine dlerror c-string)

(define-alien-routine dlsym system-area-pointer
  (handle system-area-pointer)
  (symbol c-string))

(defvar *runtime-dlhandle*)
(defvar *shared-objects*)

(defstruct shared-object file sap)

(defun dlopen-or-lose (filename)
  (dlerror) ; clear old errors
  (let ((sap (dlopen filename (logior rtld-global rtld-now))))
    (when (zerop (sap-int sap))
      (error "Could not open ~:[runtime~;~:*shared object ~S~]: ~A"
	     filename (dlerror)))
    sap))

(defun load-shared-object (file)
  "Load a shared library/dynamic shared object file/general
dlopenable alien container.

To use LOAD-SHARED-OBJECT, at the Unix command line do this:

 echo 'int summish(int x, int y) { return 1 + x + y; }' > /tmp/ffi-test.c
 make /tmp/ffi-test.o # i.e. cc -c -o /tmp/ffi-test.o /tmp/ffi-test.c
 ld -shared -o /tmp/ffi-test.so /tmp/ffi-test.o

Then in SBCL do this:

 (load-shared-object \"/tmp/ffi-test.so\")
 (define-alien-routine summish int (x int) (y int))

Now running (summish 10 20) should return 31."
  (let* ((real-file (or (unix-namestring file) file))
         (sap (dlopen-or-lose real-file))
	 (obj (make-shared-object :file real-file :sap sap))) 
    (unless (member sap *shared-objects*
		    :test #'sap= :key #'shared-object-sap)
      (setf *shared-objects* (append *shared-objects* (list obj))))
    (pathname real-file)))

(defun try-reopen-shared-object (obj)
  (restart-case 
      (let ((sap (dlopen-or-lose (shared-object-file obj))))
        (setf (shared-object-sap obj) sap)
        obj)
    (skip ()
      :report "Skip this shared object and continue. References to ~
               foreign symbols in this shared object will fail, ~
               causing potential corruption."
      *runtime-dlhandle*)))

;;; Open libraries in *SHARED-OBJECTS* and the runtime. Called during
;;; initialization. 
(defun reopen-shared-objects ()
  ;; Ensure that the runtime is present in the list
  (setf *runtime-dlhandle* (dlopen-or-lose nil)
        *shared-objects* (mapcar #'try-reopen-shared-object *shared-objects*)))

;;; Close all dlopened libraries and clear out sap entries in
;;; *SHARED-OBJECTS*.
(defun close-shared-objects ()
  (dolist (obj (reverse *shared-objects*))
    (dlclose (shared-object-sap obj))
    (setf (shared-object-sap obj) nil))
  (dlclose *runtime-dlhandle*)
  (setf *runtime-dlhandle* nil))

(defun get-dynamic-foreign-symbol-address (symbol)
  (dlerror) ; clear old errors
  (let ((result (sap-int (dlsym *runtime-dlhandle* symbol)))
        (err (dlerror)))
    (if (or (not (zerop result)) (not err))
        result
        (dolist (obj *shared-objects*)
          (setf result (sap-int (dlsym (shared-object-sap obj) symbol))
                err (dlerror))
          (when (or (not (zerop result)) (not err))
            (return result))))))
