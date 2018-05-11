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

(in-package "SB!IMPL")

(define-alien-routine arch-write-linkage-table-entry void
  (table-address unsigned) (real-address unsigned) (datap int))

(define-load-time-global *linkage-info*
    (make-hash-table :test 'equal :synchronized t))

;;; Add a foreign linkage entry if none exists, return the address
;;; in the linkage table.
(defun ensure-foreign-symbol-linkage (name datap)
  (let ((key (if datap (list name) name))
        (table-base sb!vm:linkage-table-space-start)
        (ht *linkage-info*))
    (or (awhen (with-locked-system-table (ht)
                 (or (gethash key ht)
                     (let* ((table-offset
                             (* (hash-table-count ht) sb!vm:linkage-table-entry-size))
                            (table-address (+ table-base table-offset)))
                       (when (<= (+ table-address sb!vm:linkage-table-entry-size)
                                 sb!vm:linkage-table-space-end)
                         (let ((real-address
                                (ensure-dynamic-foreign-symbol-address name datap)))
                           (aver real-address)
                           (arch-write-linkage-table-entry
                            table-address real-address (if datap 1 0))
                           (logically-readonlyize name)
                           (setf (gethash key ht) table-offset))))))
               (+ table-base it))
        (error "Linkage-table full (~D entries): cannot link ~S."
               (hash-table-count ht) name))))

;;; Update the linkage-table. Called during initialization after all
;;; shared libraries have been reopened, and after a previously loaded
;;; shared object is reloaded.
;;;
;;; FIXME: Should figure out how to write only those entries that need
;;; updating.
(defun update-linkage-table ()
  ;; This symbol is of course itself a prelinked symbol.
  (let ((n-prelinked (extern-alien "lisp_linkage_table_n_prelinked" int)))
    (dohash ((key table-offset) *linkage-info* :locked t)
      (let* ((datap (listp key))
             (table-address (+ table-offset sb!vm:linkage-table-space-start))
             (name (if datap (car key) key))
             (index (floor (- table-address sb!vm:linkage-table-space-start)
                           sb!vm:linkage-table-entry-size)))
        ;; Partial fix to the above: Symbols required for Lisp startup
        ;; will not be re-pointed to a different address ever.
        ;; Nor will those referenced by ELF core.
        (when (>= index n-prelinked)
          (let ((real-address (ensure-dynamic-foreign-symbol-address name datap)))
            (aver real-address)
            (arch-write-linkage-table-entry table-address real-address
                                            (if datap 1 0))))))))
