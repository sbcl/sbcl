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

(define-alien-routine arch-write-linkage-table-jmp void
  (table-address system-area-pointer)
  (real-address system-area-pointer))

(define-alien-routine arch-write-linkage-table-ref void
  (table-address system-area-pointer)
  (real-address system-area-pointer))

(define-load-time-global *linkage-info*
    (make-hash-table :test 'equal :synchronized t))

(defun write-linkage-table-entry (table-address real-address datap)
  (/show0 "write-linkage-table-entry")
  (let ((reloc (int-sap table-address))
        (target (int-sap real-address)))
    (if datap
        (arch-write-linkage-table-ref reloc target)
        (arch-write-linkage-table-jmp reloc target))))

;;; Add a foreign linkage entry if none exists, return the address
;;; in the linkage table.
(defun ensure-foreign-symbol-linkage (name datap)
  (let ((key (if datap (list name) name))
        (ht *linkage-info*))
    (or (with-locked-system-table (ht)
          (or (gethash key ht)
              (let* ((real-address
                      (ensure-dynamic-foreign-symbol-address name datap))
                     (table-base sb!vm:linkage-table-space-start)
                     (table-address
                      (+ (* (hash-table-count ht) sb!vm:linkage-table-entry-size)
                         table-base)))
                (aver real-address)
                (when (< table-address sb!vm:linkage-table-space-end)
                  (write-linkage-table-entry table-address real-address datap)
                  (let ((str (logically-readonlyize name)))
                    (setf (gethash (if datap (list str) str) ht)
                          table-address))))))
        (error "Linkage-table full (~D entries): cannot link ~S."
               (hash-table-count ht) name))))

;;; Update the linkage-table. Called during initialization after all
;;; shared libraries have been reopened, and after a previously loaded
;;; shared object is reloaded.
;;;
;;; FIXME: Should figure out how to write only those entries that need
;;; updating.
(defun update-linkage-table ()
  (dohash ((key table-address) *linkage-info* :locked t)
    (let* ((datap (listp key))
           (name (if datap (car key) key)))
      ;; Absent a fix for the issue noted above at "Should figure out ...",
      ;; never ever re-touch malloc or free if already linked.
      (unless (or #!+sb-dynamic-core (member name '("malloc" "free") :test 'string=))
        (let ((real-address (ensure-dynamic-foreign-symbol-address name datap)))
          (aver (and table-address real-address))
          (write-linkage-table-entry table-address real-address datap))))))
