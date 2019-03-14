;;;; Loading shared object files, Unix specifics

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-ALIEN")

(define-alien-routine dlopen system-area-pointer
  (file c-string) (mode int))

(define-alien-routine dlclose int
  (handle system-area-pointer))

(define-alien-routine dlerror c-string)

(define-alien-routine
    #-openbsd dlsym
    #+openbsd ("os_dlsym" dlsym)
    system-area-pointer
  (handle system-area-pointer)
  (symbol c-string))

(defun dlopen-or-lose (&optional (obj nil objp))
  (when objp
    (dlclose-or-lose obj))
  (dlerror) ; clear errors
  (let* ((namestring (and obj (shared-object-namestring obj)))
         (sap (dlopen namestring (logior rtld-global rtld-now))))
    (when (zerop (sap-int sap))
      (if objp
          (setf (shared-object-handle obj) nil)
          (setf *runtime-dlhandle* nil))
      (error "Error opening ~:[runtime~;shared object ~:*~S~]:~%  ~A."
             namestring (dlerror)))
    (when objp
      (setf (shared-object-handle obj) sap))
    sap))

(defun dlclose-or-lose (&optional (obj nil objp))
  (dlerror)
  (let (dlerror)
    (cond ((and (not objp) *runtime-dlhandle*)
           ;; CLH/NS: if we're on sufficiently old darwin we can't close
           ;; *runtime-dlhandle* for some reason, so don't.
           #-darwin
           (dlclose *runtime-dlhandle*)
           (setf dlerror (dlerror)
                 *runtime-dlhandle* nil))
          ((and objp (shared-object-handle obj))
           (dlclose (shared-object-handle obj))
           (setf dlerror (dlerror)
                 (shared-object-handle obj) nil)))
    (when dlerror
      (cerror "Ignore the error and continue as if closing succeeded."
              "dlerror() returned an error while trying to close ~
               ~:[runtime~;shared object ~:*~S~]: ~S"
              (when obj (shared-object-namestring obj))
              dlerror))))

(defun find-dynamic-foreign-symbol-address (symbol)
  (dlerror)                             ; clear old errors
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
          (let ((sap (shared-object-handle obj)))
            (when sap
              (setf result (sap-int (dlsym sap extern))
                    err (dlerror))
              (when (or (not (zerop result)) (not err))
                (return result))))))))


