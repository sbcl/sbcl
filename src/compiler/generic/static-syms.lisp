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

(defun static-symbol-p (symbol)
  #+sb-xc (declare (notinline position))
  (if symbol (position symbol +static-symbols+) t))

(defconstant-eqx +all-static-fdefns+
    #.(concatenate 'vector +c-callable-fdefns+ +static-fdefns+) #'equalp)

;;; Return the (byte) offset from NIL to the start of the fdefn object
;;; for the static function NAME.
(defun static-fdefn-offset (name)
  #+sb-xc (declare (notinline position))
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
