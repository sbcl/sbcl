;;;; detecting and handling exhaustion of memory (stack or heap)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;; A native address on a 4-byte boundary can be thought of (and
;;; passed around in Lisp code as) a FIXNUM. This function converts
;;; from a byte address represented as an unsigned integer to such
;;; a FIXNUM.
;;;
;;; FIXME: There should be some better place for this definition to
;;; go. (Or a redundant definition might already exist. Especially
;;; since this is essentially just a type pun, so there might be some
;;; VOP or something which'd do it for us.)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun native-address-encoded-as-fixnum (native-address)
    (declare (type unsigned-byte native-address))
    (aver (zerop (logand native-address 3)))
    (let* (;; naive encoding
	   (first-try (ash native-address -2))
	   ;; final encoding
	   (second-try 
	    (if (<= first-try sb!vm:*target-most-positive-fixnum*)
		;; looks good
		first-try
		;; When the naive encoding fails to make a FIXNUM
		;; because the sign is wrong, subtracting *T-M-P-F*
		;; should fix it. 
		(- first-try sb!vm:*target-most-positive-fixnum*))))
      (aver (<= second-try sb!vm:*target-most-positive-fixnum*))
      second-try)))

;;; a FIXNUM, to be interpreted as a native pointer, which serves
;;; as a boundary to catch stack overflow
;;;
;;; When stack overflow is detected, this is to be bound to a new
;;; value (allowing some more space for error handling) around the
;;; call to ERROR.
;;;
;;; FIXME: Maybe (probably?) this should be in SB!VM. And maybe the
;;; size of the buffer zone should be set in src/compiler/cpu/parms.lisp
;;; instead of constantly 1Mb for all CPU architectures?
(defvar *stack-exhaustion*
  ;; (initialized in cold init)
  )
(defun !exhaust-cold-init ()
  (setf *stack-exhaustion*
	#.(native-address-encoded-as-fixnum
	   #!+stack-grows-downward (+ sb!vm:control-stack-start (expt 2 20))
	   #!+stack-grows-upward (- sb!vm:control-stack-end (expt 2 20)))))
  
;;; FIXME: Even though this is only called when (> SAFETY (MAX SPEED SPACE))
;;; it's still annoyingly wasteful for it to be a full function call.
;;; It should probably be a VOP calling an assembly routine or something
;;; like that.
(defun %detect-stack-exhaustion ()
  ;; FIXME: Check the stack pointer against *STACK-EXHAUSTION*, and if
  ;; out of range signal an error (in a context where *S-E* has been
  ;; rebound to give some space to let error handling code do its
  ;; thing without new exhaustion problems).
  (values))
