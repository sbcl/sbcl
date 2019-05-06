;;;; utility functions and macros needed by the back end to generate
;;;; code

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;; Make a fixnum out of NUM. (I.e. shift by two bits if it will fit.)
(defun fixnumize (num)
  (if (fixnump num)
      (ash num n-fixnum-tag-bits)
      (error "~W is too big for a fixnum." num)))

(declaim (inline tn-byte-offset))
(defun tn-byte-offset (tn)
  (ash (tn-offset tn) word-shift))

;;; Determining whether a constant offset fits in an addressing mode.
#+(or x86 x86-64)
(defun foldable-constant-offset-p (element-size lowtag data-offset offset)
  (if (< element-size n-byte-bits)
      nil
      (multiple-value-bind (min max)
          (displacement-bounds lowtag element-size data-offset)
        (<= min offset max))))


;;;; routines for dealing with static symbols

(defun static-symbol-p (symbol)
  (or (null symbol)
      (and (find symbol +static-symbols+) t)))

;;; the byte offset of the static symbol SYMBOL
(defun static-symbol-offset (symbol)
  (if symbol
      (let ((posn (position symbol +static-symbols+)))
        (unless posn (error "~S is not a static symbol." symbol))
        (+ (* posn (pad-data-block symbol-size))
           (pad-data-block (1- symbol-size))
           other-pointer-lowtag
           (- list-pointer-lowtag)))
      0))

(defconstant-eqx +all-static-fdefns+
    #.(concatenate 'vector +c-callable-fdefns+ +static-fdefns+) #'equalp)

;;; Return the (byte) offset from NIL to the start of the fdefn object
;;; for the static function NAME.
(defun static-fdefn-offset (name)
  (let ((static-fun-index (position name +all-static-fdefns+)))
    (and static-fun-index
         (+ (* (length +static-symbols+) (pad-data-block symbol-size))
            (pad-data-block (1- symbol-size))
            (- list-pointer-lowtag)
            (* static-fun-index (pad-data-block fdefn-size))
            other-pointer-lowtag))))

;;; Return absolute address of the 'fun' slot in static fdefn NAME.
(defun static-fdefn-fun-addr (name)
  (+ nil-value
     (static-fdefn-offset name)
     (- other-pointer-lowtag)
     (ash fdefn-fun-slot word-shift)))

;;; Return the (byte) offset from NIL to the raw-addr slot of the
;;; fdefn object for the static function NAME.
(defun static-fun-offset (name)
  (+ (static-fdefn-offset name)
     (- other-pointer-lowtag)
     (* fdefn-raw-addr-slot n-word-bytes)))

;;;; interfaces to IR2 conversion

;;; Return a wired TN describing the N'th full call argument passing
;;; location.
(defun standard-arg-location (n)
  (declare (type unsigned-byte n))
  (if (< n register-arg-count)
      (make-wired-tn *backend-t-primitive-type* descriptor-reg-sc-number
                     (nth n *register-arg-offsets*))
      (make-wired-tn *backend-t-primitive-type* control-stack-sc-number n)))

;;; Same as above but marks stack locations as :arg-pass
(defun standard-call-arg-location (n)
  (declare (type unsigned-byte n))
  (if (< n register-arg-count)
      (make-wired-tn *backend-t-primitive-type* descriptor-reg-sc-number
                     (nth n *register-arg-offsets*))
      (let ((tn
              (make-wired-tn *backend-t-primitive-type* control-stack-sc-number n)))
        (setf (tn-kind tn) :arg-pass)
        tn)))

(defun standard-arg-location-sc (n)
  (declare (type unsigned-byte n))
  (if (< n register-arg-count)
      (make-sc+offset descriptor-reg-sc-number
                      (nth n *register-arg-offsets*))
      (make-sc+offset control-stack-sc-number n)))

;;; Make a TN to hold the number-stack frame pointer.  This is allocated
;;; once per component, and is component-live.
(defun make-nfp-tn ()
  #+c-stack-is-control-stack
  (make-restricted-tn *fixnum-primitive-type* ignore-me-sc-number)
  #-c-stack-is-control-stack
  (component-live-tn
   (make-wired-tn *fixnum-primitive-type* immediate-arg-scn nfp-offset)))

;;; Make an environment-live stack TN for saving the SP for NLX entry.
(defun make-nlx-sp-tn (env)
  (physenv-live-tn
   (make-representation-tn *fixnum-primitive-type* any-reg-sc-number)
   env))

#-x86-64
(defun make-stack-pointer-tn (&optional nargs)
  (declare (ignore nargs))
  (make-normal-tn *fixnum-primitive-type*))

(defun make-number-stack-pointer-tn ()
  #+c-stack-is-control-stack
  (make-restricted-tn *fixnum-primitive-type* ignore-me-sc-number)
  #-c-stack-is-control-stack
  (make-normal-tn *fixnum-primitive-type*))

;;; Return a list of TNs that can be used to represent an unknown-values
;;; continuation within a function.
(defun make-unknown-values-locations (&optional unused-count)
  (declare (ignorable unused-count))
  (list (make-stack-pointer-tn)
        (cond #+x86-64 ;; needs support from receive-unknown-values
              (unused-count
               (sb-c::make-unused-tn))
              (t
               (make-normal-tn *fixnum-primitive-type*)))))

;;; This function is called by the ENTRY-ANALYZE phase, allowing
;;; VM-dependent initialization of the IR2-COMPONENT structure. We
;;; push placeholder entries in the CONSTANTS to leave room for
;;; additional noise in the code object header.
(defun select-component-format (component)
  (declare (type component component))
  (dotimes (i code-constants-offset)
    (vector-push-extend nil
                        (ir2-component-constants (component-info component))))
  (values))

(defun error-call (vop error-code &rest values)
  "Cause an error.  ERROR-CODE is the error to cause."
  (emit-error-break vop error-trap (error-number-or-lose error-code) values))

(defun cerror-call (vop error-code &rest values)
  "Cause a continuable error.  ERROR-CODE is the error to cause."
  (emit-error-break vop cerror-trap (error-number-or-lose error-code) values))

#+sb-safepoint
(define-vop (insert-safepoint)
  (:policy :fast-safe)
  (:translate sb-kernel::gc-safepoint)
  (:generator 0
    (emit-safepoint)))

(defun other-pointer-tn-ref-p (tn-ref)
  (and (sc-is (tn-ref-tn tn-ref) descriptor-reg)
       (tn-ref-type tn-ref)
       (not (types-equal-or-intersect
             (tn-ref-type tn-ref)
             (specifier-type '(or fixnum
                               #+64-bit single-float
                               function
                               list
                               instance
                               character))))))
