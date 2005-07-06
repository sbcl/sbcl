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

;;; Used to serialize modifications to *linkage-info*,
;;; *shared-objects* and the linkage-table proper. Calls thru
;;; linkage-table are unaffected.
(defvar *foreign-lock*
  (sb!thread:make-mutex :name "foreign definition lock"))

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

(define-alien-variable undefined-alien-address unsigned-long)

(defvar *runtime-dlhandle*)
(defvar *shared-objects*)

(defstruct shared-object file sap)

(defun dlopen-or-lose (&optional (obj nil objp))
  (when objp
    (dlclose-or-lose obj))
  (dlerror) ; clear errors
  (let* ((file (and obj (shared-object-file obj)))
         (sap (dlopen file (logior rtld-global rtld-now))))
    (aver (or (not objp) file))
    (when (zerop (sap-int sap))
      (if objp
          (setf (shared-object-sap obj) nil)
          (setf *runtime-dlhandle* nil))
      (error "Error opening ~:[runtime~;shared object ~:*~S~]:~%  ~A."
             file (dlerror)))
    (when objp
      (setf (shared-object-sap obj) sap))
    sap))

(defun dlclose-or-lose (&optional (obj nil objp))
  (dlerror)
  (let (dlerror)
    (cond ((and (not objp) *runtime-dlhandle*)
           (dlclose *runtime-dlhandle*)
           (setf dlerror (dlerror)
                 *runtime-dlhandle* nil))
          ((and objp (shared-object-sap obj))
           (dlclose (shared-object-sap obj))
           (setf dlerror (dlerror)
                 (shared-object-sap obj) nil)))
    (when dlerror
      (cerror "Ignore the error and continue anyway" "dlerror returned an error: ~S" dlerror))))

(defun load-shared-object (file)
  "Load a shared library/dynamic shared object file/general dlopenable
alien container, such as a .so on an ELF platform.

Reloading the same shared object will replace the old definitions; if
a symbol was previously referenced thru the object and is not present
in the reloaded version an error will be signalled. Sameness is
determined using the library filename. Reloading may not work as
expected if user or library-code has called dlopen on FILE.

References to foreign symbols in loaded shared objects do not survive
intact through SB-EXT:SAVE-LISP-AND die on all platforms. See
SB-EXT:SAVE-LISP-AND-DIE for details."
  (sb!thread:with-mutex (*foreign-lock*)
    (let* ((filename (or (unix-namestring file) file))
           (old (find filename *shared-objects* :key #'shared-object-file :test #'equal))
           (obj (or old (make-shared-object :file filename))))
      (dlopen-or-lose obj)
      (setf *shared-objects* (append (remove obj *shared-objects*)
                                     (list obj)))
      #!+linkage-table
      (when (or old (undefined-foreign-symbols-p))
        (update-linkage-table))
      (pathname filename))))

(defun try-reopen-shared-object (obj)
  (declare (type shared-object obj))
  (tagbody :dlopen
     (restart-case
	 (dlopen-or-lose obj)
       (continue ()
	 :report "Skip this shared object and continue."
	 (setf (shared-object-sap obj) nil))
       (retry ()
	 :report "Retry loading this shared object."
	 (go :dlopen))
       (load-other ()
	 :report "Specify an alternate shared object file to load."
	 (setf (shared-object-file obj)
	       (tagbody :query
		  (format *query-io* "~&Enter pathname (evaluated):~%")
		  (force-output *query-io*)
		  (let ((pathname (ignore-errors (pathname (read *query-io*)))))
		    (unless (pathnamep pathname)
		      (format *query-io* "~&Error: invalid pathname.~%")
		      (go :query))
		    (unix-namestring pathname)))))))
  obj)

;;; Open libraries in *SHARED-OBJECTS* and the runtime. Called during
;;; initialization. 
(defun reopen-shared-objects ()
  ;; Ensure that the runtime is open
  (setf *runtime-dlhandle* (dlopen-or-lose)
        *shared-objects* (mapcar #'try-reopen-shared-object *shared-objects*)))

;;; Close all dlopened libraries and clear out sap entries in
;;; *SHARED-OBJECTS*.
(defun close-shared-objects ()
  (mapc #'dlclose-or-lose (reverse *shared-objects*))
  (dlclose-or-lose))

(defun find-dynamic-foreign-symbol-address (symbol)
  (dlerror)				; clear old errors
  (unless *runtime-dlhandle*
    (bug "Cannot resolve foreign symbol: lost *runtime-dlhandle*"))
  ;; On real ELF & dlsym platforms the EXTERN-ALIEN-NAME is a no-op,
  ;; but on platforms where dlsym is simulated we use the mangled name.
  (let* ((extern (extern-alien-name symbol))
	 (result (sap-int (dlsym *runtime-dlhandle* extern)))
	 (err (dlerror)))
    (if (or (not (zerop result)) (not err))
	result
	(dolist (obj *shared-objects*)
	  (let ((sap (shared-object-sap obj)))
	    (when sap
	      (setf result (sap-int (dlsym sap extern))
		    err (dlerror))
	      (when (or (not (zerop result)) (not err))
		(return result))))))))

(let ((symbols (make-hash-table :test #'equal))
      (undefineds (make-hash-table :test #'equal)))
  (defun ensure-dynamic-foreign-symbol-address (symbol &optional datap)
    "Returns the address of the foreign symbol as an integer. On linkage-table
ports if the symbols isn't found a special guard address is returned instead,
accesses to which will result in an UNDEFINED-ALIEN-ERROR. On other ports an
error is immediately signalled if the symbol isn't found. The returned address
is never in the linkage-table."
    (declare (ignorable datap))
    (let ((addr (find-dynamic-foreign-symbol-address symbol)))
      (cond  #!-linkage-table
	     ((not addr)
	      (error 'undefined-alien-error :name symbol))
	     #!+linkage-table
	     ((not addr)
	      (style-warn "Undefined alien: ~S" symbol)
	      (setf (gethash symbol undefineds) t)
	      (remhash symbol symbols)
	      (if datap
		  undefined-alien-address
		  (foreign-symbol-address "undefined_alien_function")))
             (addr
	      (setf (gethash symbol symbols) t)
	      (remhash symbol undefineds)
              addr))))
  (defun undefined-foreign-symbols-p ()
    (plusp (hash-table-count undefineds)))
  (defun dynamic-foreign-symbols-p ()
    (plusp (hash-table-count symbols)))
  (defun list-dynamic-foreign-symbols ()
    (loop for symbol being each hash-key in symbols
	 collect symbol)))

