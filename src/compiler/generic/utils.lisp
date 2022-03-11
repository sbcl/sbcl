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

;;; the address of the linkage table entry for table index I.
(defun linkage-table-entry-address (i)
  (ecase linkage-table-growth-direction
    (:up   (+ (* i linkage-table-entry-size) linkage-table-space-start))
    (:down (- linkage-table-space-end (* (1+ i) linkage-table-entry-size)))))

(defun linkage-table-index-from-address (addr)
  (ecase linkage-table-growth-direction
    (:up
     (floor (- addr linkage-table-space-start) linkage-table-entry-size))
    (:down
     (1- (floor (- linkage-table-space-end addr) linkage-table-space-end)))))

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
  (environment-live-tn
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
(defun make-unknown-values-locations (&optional unused-count unused-sp)
  (declare (ignorable unused-count unused-sp))
  (list (cond #+(or arm64 x86-64)
              ;; needs support from receive-unknown-values, push-values, %more-arg-values, values-list,
              ;; nlx-entry-multiple
              (unused-sp
               (sb-c::make-unused-tn))
              (t
               (make-stack-pointer-tn)))
        (cond #+(or arm64 x86-64)
              (unused-count
               (sb-c::make-unused-tn))
              (t
               (make-normal-tn *fixnum-primitive-type*)))))

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

;;; Does the TN definitely hold *any* of the 4 pointer types
(defun pointer-tn-ref-p (tn-ref)
  (and (sc-is (tn-ref-tn tn-ref) descriptor-reg)
       (tn-ref-type tn-ref)
       (not (types-equal-or-intersect
             (tn-ref-type tn-ref)
             (specifier-type '(or fixnum
                               #+64-bit single-float
                               character))))))

;;; Does the TN definitely hold any of the 3 non-list pointer types
(defun headered-object-pointer-tn-ref-p (tn-ref)
  (and (pointer-tn-ref-p tn-ref)
       (not (types-equal-or-intersect (tn-ref-type tn-ref)
                                      (specifier-type 'list)))))

;;; Does the TN definitely hold an OTHER pointer
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

(defun not-nil-tn-ref-p (tn-ref)
  (and (tn-ref-type tn-ref)
       (not (types-equal-or-intersect (tn-ref-type tn-ref)
                                      (specifier-type '(eql nil))))))

(defun stack-consed-p (object)
  (let ((write (sb-c::tn-writes object))) ; list of write refs
    (when (or (not write) ; grrrr, the only write is from a LOAD tn
                          ; and we don't know the corresponding normal TN?
              (tn-ref-next write)) ; can't determine if > 1 write
      (return-from stack-consed-p nil))
    (let ((vop (tn-ref-vop write)))
      (when (not vop) ; wat?
        (return-from stack-consed-p nil))
      (when (eq (vop-name vop) 'allocate-vector-on-stack)
        (return-from stack-consed-p t))
      (when (and (eq (vop-name vop) 'fixed-alloc)
                 (fifth (vop-codegen-info vop))) ; STACK-ALLOCATE-P
        (return-from stack-consed-p t))
      ;; Should we try to detect a stack-consed LIST also?
      ;; I don't think that will work.
      ;; (And is there anything else interesting to try?)
      (unless (member (vop-name vop) '(splat-word splat-small splat-any))
        (return-from stack-consed-p nil))
      (let* ((splat-input (vop-args vop))
             (splat-input-source
              (tn-ref-vop (sb-c::tn-writes (tn-ref-tn splat-input)))))
        ;; How in the heck can there NOT be a vop??? Well, sometimes there isn't.
        (when (and splat-input-source
                   (eq (vop-name splat-input-source)
                       'allocate-vector-on-stack))
          (return-from stack-consed-p t)))))
  nil)

;;; Just gathering some data to see where we can improve
(define-load-time-global *store-barriers-potentially-emitted* 0)
(define-load-time-global *store-barriers-emitted* 0)

(defun require-gc-store-barrier-p (object value-tn-ref value-tn)
  (incf *store-barriers-potentially-emitted*)
  ;; If OBJECT is stack-allocated, elide the barrier
  (when (stack-consed-p object)
    (return-from require-gc-store-barrier-p nil))
  (flet ((potential-heap-pointer-p (tn tn-ref)
           (when (sc-is tn any-reg) ; must be fixnum
             (return-from potential-heap-pointer-p nil))
           ;; If stack-allocated, elide the barrier
           (when (stack-consed-p tn)
             (return-from potential-heap-pointer-p nil))
           ;; If immediate non-pointer, elide the barrier
           (when (sc-is tn immediate)
             (let ((value (tn-value tn)))
               (when (sb-xc:typep value '(or character sb-xc:fixnum boolean
                                             #+64-bit single-float))
                 (return-from potential-heap-pointer-p nil))))
           (when (sb-c::unbound-marker-tn-p tn)
             (return-from potential-heap-pointer-p nil))
           ;; And elide for things like (OR FIXNUM NULL)
           (let ((type (tn-ref-type tn-ref)))
             (when (csubtypep type (specifier-type '(or character sb-xc:fixnum boolean
                                                        #+64-bit single-float)))
               (return-from potential-heap-pointer-p nil)))
           t))
    (cond (value-tn
           (unless (eq (tn-ref-tn value-tn-ref) value-tn)
             (aver (eq (tn-ref-load-tn value-tn-ref) value-tn)))
           (unless (potential-heap-pointer-p value-tn value-tn-ref)
             (return-from require-gc-store-barrier-p nil)))
          (value-tn-ref ; a list of refs linked through TN-REF-ACROSS
           ;; (presumably from INSTANCE-SET-MULTIPLE)
           (let ((any-pointer
                  (do ((ref value-tn-ref (tn-ref-across ref)))
                      ((null ref))
                    (when (potential-heap-pointer-p (tn-ref-tn ref) ref)
                      (return t)))))
             (unless any-pointer
               (return-from require-gc-store-barrier-p nil))))))
  (incf *store-barriers-emitted*)
  t)

(defun vop-nth-arg (n vop)
  (let ((ref (vop-args vop)))
    (dotimes (i n ref) (setq ref (tn-ref-across ref)))))

(defun length-field-shift (widetag)
  (if (= widetag instance-widetag)
      instance-length-shift
      n-widetag-bits))

(defconstant array-rank-mask 255)
;;; Rank is encoded as a (UNSIGNED-BYTE 8) minus one.
;;; Initialization of simple rank 1 array header words is completely unaffected-
;;; they store 0 for the rank, which is the correct encoding for 1.
;;; The encoding 1 means 2, encoding 2 means 3, and so on.
;;; Decoding is just an addition and bitwise AND.
(defun encode-array-rank (rank)
  (declare (type (unsigned-byte 8) rank))
  (logand (1- rank) array-rank-mask))

(defun compute-object-header (nwords widetag)
  (let ((array-header-p
         (or (= widetag simple-array-widetag)
             (>= widetag complex-base-string-widetag))))
    (logior (if array-header-p
                (let ((rank (- nwords array-dimensions-offset)))
                  (ash (encode-array-rank rank) array-rank-position))
                (case widetag
                  (#.fdefn-widetag 0)
                  (t (ash (1- nwords) (length-field-shift widetag)))))
            widetag)))

(defmacro id-bits-offset ()
  (let ((slot (get-dsd-index layout sb-kernel::id-word0)))
    (ash (+ sb-vm:instance-slots-offset slot) sb-vm:word-shift)))
