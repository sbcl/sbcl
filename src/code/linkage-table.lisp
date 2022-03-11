;;;; Linkage table specifics

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;;; Linkage table itself is a mmapped memory area in C-land, which is
;;;; initialized by INIT-LINKAGE-TABLE once all shared objects have
;;;; been reopened, based on the information stored in *LINKAGE-INFO*.
;;;;
;;;; For data entries the linkage table holds the real address
;;;; of the foreign symbol, and for code the entries are jumps
;;;; to the real addresses.

(in-package "SB-IMPL")

(define-alien-routine arch-write-linkage-table-entry void
  (index int) (real-address unsigned) (datap int))

(define-load-time-global *linkage-info*
    ;; CDR of the cons is the list of undefineds
    (list (make-hash-table :test 'equal :synchronized t)))
(declaim (type (cons hash-table) *linkage-info*))

(define-alien-variable undefined-alien-address unsigned)

(macrolet ((dlsym-wrapper (&optional warn)
             ;; Produce two values: an indicator of whether the foreign symbol was
             ;; found; and the address as an integer if found, or a guard address
             ;; which when accessed will result in an UNDEFINED-ALIEN-ERROR.
             `(let ((addr (find-dynamic-foreign-symbol-address name)))
                (cond (addr
                       (values t addr))
                      (t
                       (when ,warn
                         ;; If we can report the actual name when an undefined
                         ;; alien is called don't warn.
                         #-(or arm arm64 x86-64)
                         (style-warn 'sb-kernel:undefined-alien-style-warning
                                     :symbol name))
                       (values
                        nil
                        (if datap
                            undefined-alien-address
                            (or
                             (sb-fasl:get-asm-routine 'sb-vm::undefined-alien-tramp)
                             (find-foreign-symbol-address "undefined_alien_function")
                             (bug "unreachable")))))))))

;;; Add a foreign linkage entry if none exists, return the address
;;; in the linkage table.
(defun ensure-foreign-symbol-linkage (name datap)
  (let* ((key (if datap (list name) name))
         (info *linkage-info*)
         (ht (car info)))
    (or (awhen (with-system-mutex ((hash-table-lock ht))
                 (or (gethash key ht)
                     (let* ((index (hash-table-count ht))
                            (capacity (floor (- sb-vm:linkage-table-space-end
                                                sb-vm:linkage-table-space-start)
                                             sb-vm:linkage-table-entry-size)))
                       (when (< index capacity)
                         (multiple-value-bind (defined real-address) (dlsym-wrapper t)
                           (unless defined (push key (cdr info)))
                           (arch-write-linkage-table-entry index real-address
                                                           (if datap 1 0))
                           (logically-readonlyize name)
                           (setf (gethash key ht) index))))))
               (sb-vm::linkage-table-entry-address it))
        (error "Linkage-table full (~D entries): cannot link ~S."
               (hash-table-count ht) name))))

;;; Update the linkage-table. Called during initialization after all
;;; shared libraries have been reopened, and after a previously loaded
;;; shared object is reloaded.
;;;
;;; FIXME: Should figure out how to write only those entries that need
;;; updating.
;;; The problem is that when unloading a library, lacking any way to know which
;;; symbols came from it, we have to try to find every symbol again.
;;; If the shared-object-handle in which each symbol was originally found were
;;; stored in linkage-info, we could know which will become undefined on unload.
;;; The only "problem" is my lack of motivation to change this further.
(defun update-linkage-table (full-scan)
  ;; This symbol is of course itself a prelinked symbol.
  (let* ((n-prelinked (extern-alien "lisp_linkage_table_n_prelinked" int))
         (info *linkage-info*)
         (ht (car info))
         ;; for computing anew the list of undefined symbols
         (notdef))
    (flet ((recheck (key index)
             (let* ((datap (listp key))
                    (name (if datap (car key) key)))
               ;; Symbols required for Lisp startup
               ;; will not be re-pointed to a different address ever.
               ;; Nor will those referenced by ELF core.
               (when (>= index n-prelinked)
                 (multiple-value-bind (defined real-address) (dlsym-wrapper)
                   (unless defined (push key notdef))
                   (arch-write-linkage-table-entry index real-address
                                                   (if datap 1 0)))))))
    (with-system-mutex ((hash-table-lock ht))
      (if full-scan
          ;; Look up everything; this is for image restart or library unload.
          (dohash ((key index) ht)
            (recheck key index))
          ;; Look up only the currently undefined foreign symbols
          (dolist (key (cdr info))
            (recheck key (the (not null) (gethash key ht)))))
      (setf (cdr info) notdef)))))
)
