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

(defvar *linkage-info* (make-hash-table :test 'equal :synchronized t))

(defun write-linkage-table-entry (table-address real-address datap)
  (/show0 "write-linkage-table-entry")
  (let ((reloc (int-sap table-address))
        (target (int-sap real-address)))
    (if datap
        (arch-write-linkage-table-ref reloc target)
        (arch-write-linkage-table-jmp reloc target))))

;;; Add the linkage information about a foreign symbol in the
;;; persistent table, and write the linkage-table entry.
(defun link-foreign-symbol (name datap)
  (/show0 "link-foreign-symbol")
  (let ((table-address (+ (* (hash-table-count *linkage-info*)
                             sb!vm:linkage-table-entry-size)
                          sb!vm:linkage-table-space-start))
        (real-address (ensure-dynamic-foreign-symbol-address name datap)))
    (aver real-address)
    (unless (< table-address sb!vm:linkage-table-space-end)
      (error "Linkage-table full (~D entries): cannot link ~S."
             (hash-table-count *linkage-info*)
             name))
    (write-linkage-table-entry table-address real-address datap)
    (setf (gethash (cons name datap) *linkage-info*) table-address)))

;;; Add a foreign linkage entry if none exists, return the address
;;; in the linkage table.
(defun ensure-foreign-symbol-linkage (name datap)
  (/show0 "ensure-foreign-symbol-linkage")
  (with-locked-system-table (*linkage-info*)
    (or (gethash (cons name datap) *linkage-info*)
        (link-foreign-symbol name datap))))

;;; Update the linkage-table. Called during initialization after all
;;; shared libraries have been reopened, and after a previously loaded
;;; shared object is reloaded.
;;;
;;; FIXME: Should figure out how to write only those entries that need
;;; updating.
(defun update-linkage-table ()
  (dohash ((name-and-datap table-address) *linkage-info* :locked t)
    (let* ((name (car name-and-datap))
           (datap (cdr name-and-datap))
           (real-address
            (ensure-dynamic-foreign-symbol-address name datap)))
      (aver (and table-address real-address))
      (write-linkage-table-entry table-address
                                 real-address
                                 datap))))
