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

(defvar *runtime-dlhandle*)
(defvar *shared-objects*)

(defstruct shared-object file sap)

(defun dlopen-or-lose (&optional (obj nil objp))
  (when objp
    (dlclose-or-lose obj))
  (dlerror) ; clear errors
  (let* ((file (when obj (shared-object-file obj)))
         (sap (dlopen file (logior rtld-global rtld-now))))
    (aver (or (not objp) file))
    (when (zerop (sap-int sap))
      (if objp
          (setf (shared-object-sap obj) *runtime-dlhandle*)
          (setf *runtime-dlhandle* nil))
      (error "Could not open ~:[runtime~;~:*shared object ~S~]: ~A"
             obj (dlerror)))
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
      (cerror dlerror))))

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
      (when old
        (update-linkage-table))
      (pathname filename))))

(defun try-reopen-shared-object (obj)
  (with-simple-restart (skip "~@<Skip this shared object and continue. ~
                              References to foreign symbols in this ~
                              shared object will fail with undefined ~
                              consequences.~:>")
    (dlopen-or-lose obj)
    obj))

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
