;;;; trace tables (from codegen.lisp in CMU CL sources)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

(defun trace-table-entry (state)
  (declare (special *trace-table-info*))
  (let ((label (gen-label)))
    (emit-label label)
    (push (cons label state) *trace-table-info*))
  (values))

(def!constant tt-bits-per-state 3)
(def!constant tt-bytes-per-entry 2)
(def!constant tt-bits-per-entry (* tt-bytes-per-entry sb!vm:n-byte-bits))
(def!constant tt-bits-per-offset (- tt-bits-per-entry tt-bits-per-state))
(def!constant tt-max-offset (1- (ash 1 tt-bits-per-offset)))

(deftype tt-state ()
  `(unsigned-byte ,tt-bits-per-state))
(deftype tt-entry ()
  `(unsigned-byte ,tt-bits-per-entry))
(deftype tt-offset ()
  `(unsigned-byte ,tt-bits-per-offset))

;;; Convert the list of (LABEL . STATE) entries into an ivector.
(declaim (ftype (function (list) (simple-array tt-entry 1)) pack-trace-table))
(defun pack-trace-table (entries)
  (declare (list entries))
  (declare (ignore entries))
  ;; (This was interesting under the old CMU CL generational garbage
  ;; collector (GENGC) but is trivial under the GC implementations
  ;; used in SBCL.)
  (make-array 0 :element-type 'tt-entry))
