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
;;;; These functions get recompiled in warm build

#-sb-xc
(defun static-symbol-p (symbol)
  (if symbol (position symbol +static-symbols+) t))

#+sb-xc ; this less-than-ideal replica of the above achieves 2 things:
;; 1. causes STATIC-SYMBOL-P to be usable in cold-init right away,
;;    before named constants have been patched in. Otherwise there would
;;    be sensitivity to the order in which you're allowed to call this.
;; 2. NOTINLINE avoids producing an xperfecthash entry (I don't like that
;;    adding/removing a static symbol cuased the oracle files to change)
;; This function gets recompiled in warm build using the #-sb-xc above.
(defun static-symbol-p (symbol)
  (declare (notinline position))
  (if symbol (position symbol '#.+static-symbols+) t))

(defconstant-eqx +all-static-fdefns+
    #.(concatenate 'vector +c-callable-fdefns+ +static-fdefns+) #'equalp)

(locally
#+sb-xc (declare (notinline position))
;;; There are no static fdefns with linkage-space, but this predicate tests the nature
;;; of the function, not the fdefn, e.g. could it be called from an assembly routine.
;;; Also static-ness imparts changed behavior in LTN-ANALYZE-KNOWN-CALL and FUN-LVAR-TN.
;;; Clearly it's asking about the function, but rev 34ef6951 claimed that static-fun-offset
;;; was the wrong name for what is essentially the basis of the test so if that's wrong,
;;; I don't know what else to name it.
(defun sb-c::static-fdefn-p (name)
  (if (position name +all-static-fdefns+) t nil))
;;; Return the (byte) offset from NIL to the start of the fdefn object
;;; for the static function NAME.
#-linkage-space
(progn
(defun static-fdefn-offset (name)
  (let ((static-fun-index (position name +all-static-fdefns+)))
    (and static-fun-index
         (+ (* (length +static-symbols+) (pad-data-block symbol-size))
            (pad-data-block (1- symbol-size))
            ;; sizeof SB-LOCKLESS:+TAIL+ is calculated as 1 user data slot,
            ;; round-to-odd, add the header word.
            (* (1+ (logior (1+ instance-data-start) 1)) n-word-bytes)
            (- list-pointer-lowtag)
            (* static-fun-index (pad-data-block fdefn-size))
            other-pointer-lowtag))))
;;; Return the (byte) offset from NIL to the raw-addr slot of the
;;; fdefn object for the static function NAME.
(defun static-fun-offset (name)
  (+ (static-fdefn-offset name)
     (- other-pointer-lowtag)
     (* fdefn-raw-addr-slot n-word-bytes)))))
