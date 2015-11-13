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

(in-package "SB!VM")

;;; Make a fixnum out of NUM. (I.e. shift by two bits if it will fit.)
(defun fixnumize (num)
  (if (fixnump num)
      (ash num n-fixnum-tag-bits)
      (error "~W is too big for a fixnum." num)))

;;; Determining whether a constant offset fits in an addressing mode.
#!+(or x86 x86-64)
(defun foldable-constant-offset-p (element-size lowtag data-offset offset)
  (if (< element-size n-byte-bits)
      nil
      (multiple-value-bind (min max)
          (sb!impl::displacement-bounds lowtag element-size data-offset)
        (<= min offset max))))


;;;; routines for dealing with static symbols

(defun static-symbol-p (symbol)
  (or (null symbol)
      (and (member symbol *static-symbols*) t)))

;;; the byte offset of the static symbol SYMBOL
(defun static-symbol-offset (symbol)
  (if symbol
      (let ((posn (position symbol *static-symbols*)))
        (unless posn (error "~S is not a static symbol." symbol))
        (+ (* posn (pad-data-block symbol-size))
           (pad-data-block (1- symbol-size))
           other-pointer-lowtag
           (- list-pointer-lowtag)))
      0))

;;; Given a byte offset, OFFSET, return the appropriate static symbol.
(defun offset-static-symbol (offset)
  (if (zerop offset)
      nil
      (multiple-value-bind (n rem)
          (truncate (+ offset list-pointer-lowtag (- other-pointer-lowtag)
                       (- (pad-data-block (1- symbol-size))))
                    (pad-data-block symbol-size))
        (unless (and (zerop rem) (<= 0 n (1- (length *static-symbols*))))
          (error "The byte offset ~W is not valid." offset))
        (elt *static-symbols* n))))

;;; Return the (byte) offset from NIL to the start of the fdefn object
;;; for the static function NAME.
(defun static-fdefn-offset (name)
  (let ((static-syms (length *static-symbols*))
        (static-fun-index (position name *static-funs*)))
    (unless static-fun-index
      (error "~S isn't a static function." name))
    (+ (* static-syms (pad-data-block symbol-size))
       (pad-data-block (1- symbol-size))
       (- list-pointer-lowtag)
       (* static-fun-index (pad-data-block fdefn-size))
       other-pointer-lowtag)))

;;; Return the (byte) offset from NIL to the raw-addr slot of the
;;; fdefn object for the static function NAME.
(defun static-fun-offset (name)
  (+ (static-fdefn-offset name)
     (- other-pointer-lowtag)
     (* fdefn-raw-addr-slot n-word-bytes)))

;;; Various error-code generating helpers
(defvar *adjustable-vectors* nil)

(defmacro with-adjustable-vector ((var) &rest body)
  `(let ((,var (or (pop *adjustable-vectors*)
                   (make-array 16
                               :element-type '(unsigned-byte 8)
                               :fill-pointer 0
                               :adjustable t))))
     ;; Don't declare the length - if it gets adjusted and pushed back
     ;; onto the freelist, it's anyone's guess whether it was expanded.
     ;; This code was wrong for >12 years, so nobody must have needed
     ;; more than 16 elements. Maybe we should make it nonadjustable?
     (declare (type (vector (unsigned-byte 8)) ,var))
     (setf (fill-pointer ,var) 0)
     (unwind-protect
         (progn
           ,@body)
       (push ,var *adjustable-vectors*))))

;;;; interfaces to IR2 conversion

;;; Return a wired TN describing the N'th full call argument passing
;;; location.
(defun standard-arg-location (n)
  (declare (type unsigned-byte n))
  (if (< n register-arg-count)
      (make-wired-tn *backend-t-primitive-type* descriptor-reg-sc-number
                     (nth n *register-arg-offsets*))
      (make-wired-tn *backend-t-primitive-type* control-stack-sc-number n)))

(defun standard-arg-location-sc (n)
  (declare (type unsigned-byte n))
  (if (< n register-arg-count)
      (make-sc-offset descriptor-reg-sc-number
                      (nth n *register-arg-offsets*))
      (make-sc-offset control-stack-sc-number n)))

;;; Make a TN to hold the number-stack frame pointer.  This is allocated
;;; once per component, and is component-live.
(defun make-nfp-tn ()
  #!+c-stack-is-control-stack
  (make-restricted-tn *fixnum-primitive-type* ignore-me-sc-number)
  #!-c-stack-is-control-stack
  (component-live-tn
   (make-wired-tn *fixnum-primitive-type* immediate-arg-scn nfp-offset)))

;;; Make an environment-live stack TN for saving the SP for NLX entry.
(defun make-nlx-sp-tn (env)
  (physenv-live-tn
   (make-representation-tn *fixnum-primitive-type* any-reg-sc-number)
   env))

(defun make-stack-pointer-tn ()
  (make-normal-tn *fixnum-primitive-type*))

(defun make-number-stack-pointer-tn ()
  #!+c-stack-is-control-stack
  (make-restricted-tn *fixnum-primitive-type* ignore-me-sc-number)
  #!-c-stack-is-control-stack
  (make-normal-tn *fixnum-primitive-type*))

;;; Return a list of TNs that can be used to represent an unknown-values
;;; continuation within a function.
(defun make-unknown-values-locations ()
  (list (make-stack-pointer-tn)
        (make-normal-tn *fixnum-primitive-type*)))

;;; This function is called by the ENTRY-ANALYZE phase, allowing
;;; VM-dependent initialization of the IR2-COMPONENT structure. We
;;; push placeholder entries in the CONSTANTS to leave room for
;;; additional noise in the code object header.
(defun select-component-format (component)
  (declare (type component component))
  ;; The 1+ here is because for the x86 the first constant is a
  ;; pointer to a list of fixups, or NIL if the code object has none.
  ;; (The fixups are needed at GC copy time because the X86 code isn't
  ;; relocatable.)
  ;;
  ;; KLUDGE: It'd be cleaner to have the fixups entry be a named
  ;; element of the CODE (aka component) primitive object. However,
  ;; it's currently a large, tricky, error-prone chore to change
  ;; the layout of any primitive object, so for the foreseeable future
  ;; we'll just live with this ugliness. -- WHN 2002-01-02
  (dotimes (i (+ code-constants-offset #!+x86 1))
    (vector-push-extend nil
                        (ir2-component-constants (component-info component))))
  (values))


