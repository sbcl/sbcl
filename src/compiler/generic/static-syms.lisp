;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;;; routines for dealing with static symbols

;;; Quoting the value of +STATIC-SYMBOLS+ makes STATIC-SYMBOL-P usable in
;;; cold-init right away, before named constants have been patched in.
;;; Otherwise there would be sensitivity to the order in which you're
;;; allowed to call this.
(defun static-symbol-p (symbol)
  (if symbol (position symbol '#.+static-symbols+) t))

;;; There are no static fdefns with linkage-space, but this predicate tests the nature
;;; of the function, not the fdefn, e.g. could it be called from an assembly routine.
;;; Also static-ness imparts changed behavior in LTN-ANALYZE-KNOWN-CALL and FUN-LVAR-TN.
;;; Clearly it's asking about the function, but rev 34ef6951 claimed that static-fun-offset
;;; was the wrong name for what is essentially the basis of the test so if that's wrong,
;;; I don't know what else to name it.
(defun sb-c::static-fdefn-p (name)
  (if (position name +static-fdefns+) t nil))
#-linkage-space
(progn
;;; Return the byte offset from NIL to the start of the static fdefn object
;;; for the function NAME.
(defun static-fdefn-offset (name)
  (awhen (position name +static-fdefns+)
    (+ (* it (pad-data-block fdefn-size))
       lflist-tail-value-nil-offset
       ;; figure a header, layout (for #-compact-instance-header), and 1 data word
       (pad-data-block (+ instance-data-start 2)) ; sizeof SB-LOCKLESS:+TAIL+
       (- other-pointer-lowtag instance-pointer-lowtag))))
;;; Return the (byte) offset from NIL to the raw-addr slot of the
;;; fdefn object for the static function NAME.
(defun static-fun-offset (name)
  (+ (static-fdefn-offset name)
     (- other-pointer-lowtag)
     (* fdefn-raw-addr-slot n-word-bytes))))
