(in-package "SB-VM")

; normally assem-scheduler-p is t, and nil if debugging the assembler
; but apparently we don't trust any of the instruction definitions.
(defconstant sb-assem:assem-scheduler-p nil)
(defconstant sb-assem:+inst-alignment-bytes+ 4)
(defconstant sb-assem:+assem-max-locations+ 68) ; see number-location

(defconstant +backend-fasl-file-implementation+ :hppa)
(defconstant +backend-page-bytes+ 4096)

;;;; Machine Architecture parameters:
(eval-when (:compile-toplevel :load-toplevel :execute)

;;; number of bits per word where a word holds one lisp descriptor
(defconstant n-word-bits 32)

;;; the natural width of a machine word (as seen in e.g. register width,
;;; address space)
(defconstant n-machine-word-bits 32)

(defconstant float-sign-shift 31)

(defconstant single-float-bias 126)
(defconstant-eqx single-float-exponent-byte (byte 8 23) #'equalp)
(defconstant-eqx single-float-significand-byte (byte 23 0) #'equalp)
(defconstant single-float-normal-exponent-min 1)
(defconstant single-float-normal-exponent-max 254)
(defconstant single-float-hidden-bit (ash 1 23))

(defconstant double-float-bias 1022)
(defconstant-eqx double-float-exponent-byte (byte 11 20) #'equalp)
(defconstant-eqx double-float-significand-byte (byte 20 0) #'equalp)
(defconstant double-float-normal-exponent-min 1)
(defconstant double-float-normal-exponent-max #x7FE)
(defconstant double-float-hidden-bit (ash 1 20))

(defconstant single-float-digits
  (+ (byte-size single-float-significand-byte) 1))

(defconstant double-float-digits
  (+ (byte-size double-float-significand-byte) n-word-bits 1))

(defconstant float-inexact-trap-bit (ash 1 0))
(defconstant float-underflow-trap-bit (ash 1 1))
(defconstant float-overflow-trap-bit (ash 1 2))
(defconstant float-divide-by-zero-trap-bit (ash 1 3))
(defconstant float-invalid-trap-bit (ash 1 4))

(defconstant float-round-to-nearest 0)
(defconstant float-round-to-zero 1)
(defconstant float-round-to-positive 2)
(defconstant float-round-to-negative 3)

(defconstant-eqx float-rounding-mode (byte 2 7) #'equalp)
(defconstant-eqx float-sticky-bits (byte 5 27) #'equalp)
(defconstant-eqx float-traps-byte (byte 5 0) #'equalp)
(defconstant-eqx float-exceptions-byte (byte 5 27) #'equalp)
(defconstant-eqx float-condition-bit (ash 1 26) #'equalp)
(defconstant float-fast-bit 0)                   ; No fast mode on HPPA.



;;;; Description of the target address space.

;;; Where to put the different spaces.
;;;
(defconstant linkage-table-space-start #x1000)
(defconstant linkage-table-space-end   linkage-table-space-start) ; 0 size

(defconstant read-only-space-start #x4b000000)
(defconstant read-only-space-end   #x4dff0000)

(defconstant static-space-start    #x4e000000)
(defconstant static-space-end      #x4fff0000)

(defparameter dynamic-0-space-start  #x50000000)
(defparameter dynamic-0-space-end    #x5fff0000)

); eval-when

;;; When doing external branching on hppa (e.g. inst ble)
;;; we must know which space we want to jump into (text, code)

;; The space-register holding the lisp heap.
(defconstant lisp-heap-space 5)

;; The space-register holding the C text heap.
(defconstant c-text-space 4)


;;;; Other random constants.

(defenum ()
  atomic-flag
  interrupted-flag)

(defenum (:start 8)
  halt-trap
  pending-interrupt-trap
  cerror-trap
  breakpoint-trap
  fun-end-breakpoint-trap
  single-step-breakpoint-trap
  single-step-around-trap
  single-step-before-trap
  single-step-after-trap
  error-trap)

;;;; Static symbols.

;;; These symbols are loaded into static space directly after NIL so
;;; that the system can compute their address by adding a constant
;;; amount to NIL.
;;;
;;; The fdefn objects for the static functions are loaded into static
;;; space directly after the static symbols.  That way, the raw-addr
;;; can be loaded directly out of them by indirecting relative to NIL.
(defconstant-eqx +static-symbols+
 `#(,@+common-static-symbols+)
  #'equalp)

(defconstant-eqx +static-fdefns+
  #(length
    two-arg-+
    two-arg--
    two-arg-*
    two-arg-/
    two-arg-<
    two-arg->
    two-arg-=
    two-arg-<=
    two-arg->=
    two-arg-/=
    eql
    %negate
    two-arg-and
    two-arg-ior
    two-arg-xor
    two-arg-gcd
    two-arg-lcm)
  #'equalp)

