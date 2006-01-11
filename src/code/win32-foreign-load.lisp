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

(define-alien-type hinstance long)

(define-alien-routine ("LoadLibraryA@4" loadlibrary) hinstance
  (file c-string))

(define-alien-routine ("FreeLibrary@4" freelibrary) int
  (handle hinstance))

(define-alien-routine ("GetProcAddress@8" getprocaddress) system-area-pointer
  (handle hinstance)
  (symbol c-string))

(define-alien-routine ("GetLastError@0" getlasterror) unsigned-int)

(defvar *shared-objects*)

(defstruct shared-object file handle)

(defun dlopen-or-lose (obj)
  (let* ((file (shared-object-file obj))
         (handle (loadlibrary file)))
    (aver file)
    (when (zerop handle)
      (setf (shared-object-sap obj) nil)
      (error "Error opening shared object ~S:~%  ~A."
             file (getlasterror)))
    (setf (shared-object-handle obj) handle)
    handle))

(defun dlclose-or-lose (&optional (obj nil objp))
  (let (dlerror)
    (cond ((and objp (shared-object-handle obj))
           (setf dlerror (if (freelibrary (shared-object-handle obj))
                             nil
                             (getlasterror))
                 (shared-object-handle obj) nil)))
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
intact through SB-EXT:SAVE-LISP-AND-DIE on all platforms. See
SB-EXT:SAVE-LISP-AND-DIE for details."
  (sb!thread:with-mutex (*foreign-lock*)
    (let* ((filename (or (unix-namestring file) file))
           (old (find filename *shared-objects* :key #'shared-object-file :test #'equal))
           (obj (or old (make-shared-object :file filename))))
      (unless old
        (dlopen-or-lose obj))
      (setf *shared-objects* (append (remove obj *shared-objects*)
                                     (list obj)))
      (pathname filename))))

(defun try-reopen-shared-object (obj)
  (declare (type shared-object obj))
  (tagbody :dlopen
     (restart-case
         (dlopen-or-lose obj)
       (continue ()
         :report "Skip this shared object and continue."
         (setf (shared-object-handle obj) nil))
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
;;; Note that, so long as we don't have linkage-table, this is braindead.
(defun reopen-shared-objects ()
  (setf *shared-objects* (mapcar #'try-reopen-shared-object *shared-objects*)))

;;; Close all dlopened libraries and clear out sap entries in
;;; *SHARED-OBJECTS*.
(defun close-shared-objects ()
  (mapc #'dlclose-or-lose (reverse *shared-objects*)))

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
    (dolist (obj *shared-objects*)
      (let ((handle (shared-object-handle obj)))
        (when handle
          (setf result (sap-int (getprocaddress handle extern)))
          (when (not (zerop result))
            (return result)))))))

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
      (cond  ((not addr)
              (error 'undefined-alien-error :name symbol))
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

