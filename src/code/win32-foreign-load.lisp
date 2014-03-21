;;;; Loading shared object files, Win32 specifics

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!ALIEN")

(define-alien-type hinstance signed)

(define-alien-routine ("LoadLibraryA" loadlibrary) hinstance
  (file c-string))

(define-alien-routine ("FreeLibrary" freelibrary) int
  (handle hinstance))

(define-alien-routine ("GetProcAddress" getprocaddress) system-area-pointer
  (handle hinstance)
  (symbol c-string))

(define-alien-routine ("SetStdHandle" set-std-handle)
   void
 (id int)
 (handle int))

(define-alien-routine ("GetStdHandle" get-std-handle)
  int
 (id int))

(define-alien-routine ("GetModuleHandleW" get-module-handle)
    hinstance
  (name (c-string :external-format :ucs-2)))

(defvar *reset-stdio-on-dlopen* t)

(defconstant +stdio-handle+ -10)

(defun loadlibrary-without-stdio (namestring)
  (flet ((loadlibrary (namestring)
           (loadlibrary namestring)))
   (if *reset-stdio-on-dlopen*
       (let ((stdio (get-std-handle +stdio-handle+)))
         (unwind-protect
              (progn
                (set-std-handle +stdio-handle+ -1)
                (loadlibrary namestring))
           (set-std-handle +stdio-handle+ stdio)))
       (loadlibrary namestring))))

(defun dlopen-or-lose (&optional obj)
  (if obj
      (let* ((namestring (shared-object-namestring obj))
             (handle (loadlibrary-without-stdio namestring)))
        (aver namestring)
        (when (zerop handle)
          (setf (shared-object-handle obj) nil)
          (error "Error opening shared object ~S:~% ~A"
                 namestring (sb!win32:format-system-message (sb!win32:get-last-error))))
        (setf (shared-object-handle obj) handle)
        handle)
      (extern-alien "runtime_module_handle" hinstance)))

(defun dlclose-or-lose (&optional (obj nil objp))
  (when (and objp (shared-object-handle obj))
    (unless (freelibrary (shared-object-handle obj))
      (cerror "Ignore the error and continue as if closing succeeded."
              "FreeLibrary() caused an error while trying to close ~
               shared object ~S:~% ~A"
              (shared-object-namestring obj)
              (sb!win32:format-system-message (sb!win32:get-last-error))))
    (setf (shared-object-handle obj) nil)))

(defun find-dynamic-foreign-symbol-address (symbol)
  ;; On real ELF & dlsym platforms the EXTERN-ALIEN-NAME is a no-op,
  ;; but on platforms where dlsym is simulated we use the mangled name.
  ;; Win32 is a special case. It needs EXTERN-ALIEN-NAME to mangle the
  ;; name for static linkage, but also needs unmangled symbols for
  ;; GetProcAddress(). So we coerce to base-string instead.
  ;; Oh, and we assume that all runtime symbols are static-linked.
  ;; No *runtime-dlhandle* for us.
  ;; Also, GetProcAddress doesn't call SetLastError(0) on success,
  ;; and GetLastError() doesn't either. For now, we assume that
  ;; GetProcAddress() won't return NULL on success.
  (let* ((extern (coerce symbol 'base-string))
         (result nil))
    (dolist (handle
              (cons *runtime-dlhandle*
                    (mapcar #'shared-object-handle *shared-objects*)))
      (when handle
        (setf result (sap-int (getprocaddress handle extern)))
        (when (not (zerop result))
          (return result))))))

(defun runtime-exported-symbols ()
  ;; TODO: reimplement for x86-64. Not so hard.
  (let* ((image-base (extern-alien "runtime_module_handle" system-area-pointer))
         (pe-base (sap+ image-base (sap-ref-32 image-base 60)))
         (export-directory (sap+ pe-base (- #!+x86 248 #!+x86-64 264 (* 16 8))))
         (export-data (sap+ image-base (sap-ref-32 export-directory 0)))
         (n-functions (sap-ref-32 export-data 20))
         (n-names (sap-ref-32 export-data 24))
         (functions-sap (sap+ image-base (sap-ref-32 export-data 28)))
         (names-sap (sap+ image-base (sap-ref-32 export-data 32))))
    (loop repeat (min n-functions n-names)
          for offset from 0 by #.sb!vm::n-word-bytes
          collect
       (cons
         (sap-int (sap+ image-base (sap-ref-32 functions-sap offset)))
         (sap-int (sap+ image-base (sap-ref-32 names-sap offset)))))))
