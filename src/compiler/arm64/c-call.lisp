;;;; VOPs and other machine-specific support routines for call-out to C

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(defconstant +number-stack-alignment-mask+ (1- (* n-word-bytes 2)))

(defconstant +max-register-args+ 8)

(defstruct (arg-state
            (:copier nil)
            (:predicate nil)
            (:constructor make-arg-state ()))
  (num-register-args 0)
  (fp-registers 0)
  (stack-frame-size 0))

(defstruct (result-state (:copier nil) (:predicate nil)
                         (:constructor make-result-state ()))
  (num-results 0))

(defun result-reg-offset (slot)
  (ecase slot
    (0 nl0-offset)
    (1 nl1-offset)))

(defun register-args-offset (index)
  (elt '#.(list nl0-offset nl1-offset nl2-offset nl3-offset nl4-offset nl5-offset nl6-offset
                      nl7-offset)
       index))

(define-vop (move-word-arg-stack)
  (:args (x :scs (signed-reg unsigned-reg single-reg))
         (fp :scs (any-reg)))
  (:info size offset)
  (:generator 0
    (let ((addr (@ fp (load-store-offset offset))))
      (ecase size
        (1
         (inst strb x addr))
        (2
         (inst strh x addr))
        (4
         (inst str (if (sc-is x single-reg)
                       x
                       (32-bit-reg x))
               addr))))))

(defun move-to-stack-location (value size offset prim-type sc node block nsp)
  (let ((temp-tn (sb-c:make-representation-tn
                  (primitive-type-or-lose prim-type)
                  sc)))
    (sb-c::emit-move node
                     block
                     (sb-c::lvar-tn node block value)
                     temp-tn)
    (sb-c::vop move-word-arg-stack node block temp-tn nsp size offset)))

(defun int-arg (state prim-type reg-sc stack-sc &optional (size 8))
  (let ((reg-args (arg-state-num-register-args state)))
    (cond ((< reg-args +max-register-args+)
           (setf (arg-state-num-register-args state) (1+ reg-args))
           (make-wired-tn* prim-type reg-sc (register-args-offset reg-args)))
          (t
           (let ((frame-size (align-up (arg-state-stack-frame-size state) size)))
             (setf (arg-state-stack-frame-size state) (+ frame-size size))
             (cond #+darwin
                   ((/= size n-word-bytes)
                    (lambda (value node block nsp)
                      (move-to-stack-location value size frame-size
                                              prim-type reg-sc node block nsp)))
                   (t
                    (make-wired-tn* prim-type stack-sc (truncate frame-size size)))))))))

(defun float-arg (state prim-type reg-sc stack-sc &optional (size 8))
  (let ((reg-args (arg-state-fp-registers state)))
    (cond ((< reg-args +max-register-args+)
           (setf (arg-state-fp-registers state) (1+ reg-args))
           (make-wired-tn* prim-type reg-sc reg-args))
          (t
           (let ((frame-size (align-up (arg-state-stack-frame-size state) size)))
             (setf (arg-state-stack-frame-size state) (+ frame-size size))
             (cond #+darwin
                   ((/= size n-word-bytes)
                    (lambda (value node block nsp)
                      (move-to-stack-location value size frame-size
                                              prim-type reg-sc node block nsp)))
                   (t
                    (make-wired-tn* prim-type stack-sc (truncate frame-size size)))))))))

(define-alien-type-method (integer :arg-tn) (type state)
 (let ((size #+darwin (truncate (alien-type-bits type) n-byte-bits)
             #-darwin n-word-bytes))
   (if (alien-integer-type-signed type)
       (int-arg state 'signed-byte-64 signed-reg-sc-number signed-stack-sc-number
                size)
       (int-arg state 'unsigned-byte-64 unsigned-reg-sc-number unsigned-stack-sc-number
                size))))

(define-alien-type-method (system-area-pointer :arg-tn) (type state)
  (declare (ignore type))
  (int-arg state 'system-area-pointer sap-reg-sc-number sap-stack-sc-number))

(define-alien-type-method (single-float :arg-tn) (type state)
  (declare (ignore type))
  (float-arg state 'single-float single-reg-sc-number single-stack-sc-number #+darwin 4))

(define-alien-type-method (double-float :arg-tn) (type state)
  (declare (ignore type))
  (float-arg state 'double-float double-reg-sc-number double-stack-sc-number))
;;;

(defknown sign-extend ((signed-byte 64) t) fixnum
    (foldable flushable movable))

(defoptimizer (sign-extend derive-type) ((x size))
  (when (sb-c:constant-lvar-p size)
    (specifier-type `(signed-byte ,(sb-c:lvar-value size)))))

(define-vop (sign-extend)
  (:translate sign-extend)
  (:policy :fast-safe)
  (:args (val :scs (signed-reg)))
  (:arg-types signed-num (:constant fixnum))
  (:info size)
  (:results (res :scs (signed-reg)))
  (:result-types fixnum)
  (:generator 1
    (check-type size (member 8 16 32))
    (inst sbfm res val 0 (1- size))))

#-sb-xc-host
(defun sign-extend (x size)
  (declare (type (signed-byte 64) x))
  (ecase size
    (8 (sign-extend x size))
    (16 (sign-extend x size))
    (32 (sign-extend x size))))

(define-alien-type-method (integer :naturalize-gen) (type alien)
  (if (<= (alien-type-bits type) 32)
      (if (alien-integer-type-signed type)
          `(sign-extend ,alien ,(alien-type-bits type))
          `(logand ,alien ,(1- (ash 1 (alien-type-bits type)))))
      alien))

(define-alien-type-method (integer :result-tn) (type state)
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (multiple-value-bind (ptype reg-sc)
        (if (alien-integer-type-signed type)
            (values 'signed-byte-64 signed-reg-sc-number)
            (values 'unsigned-byte-64 unsigned-reg-sc-number))
      (make-wired-tn* ptype reg-sc
                        (result-reg-offset num-results)))))

(define-alien-type-method (system-area-pointer :result-tn) (type state)
  (declare (ignore type state))
  (make-wired-tn* 'system-area-pointer sap-reg-sc-number (result-reg-offset 0)))


(define-alien-type-method (single-float :result-tn) (type state)
  (declare (ignore type state))
  (make-wired-tn* 'single-float single-reg-sc-number 0))

(define-alien-type-method (double-float :result-tn) (type state)
  (declare (ignore type state))
  (make-wired-tn* 'double-float double-reg-sc-number 0))

(define-alien-type-method (values :result-tn) (type state)
  (let ((values (alien-values-type-values type)))
    (when (> (length values) 2)
      (error "Too many result values from c-call."))
    (mapcar (lambda (type)
              (invoke-alien-type-method :result-tn type state))
            values)))

;;;; Struct Return-by-Value Support for ARM64 (AAPCS64)

;;; Check if a record type is a Homogeneous Floating-point Aggregate (HFA)
;;; An HFA is a struct with 1-4 floating-point members of the same type.
;;; Members can be scalar floats, arrays of floats, or nested HFA structs.
(defun hfa-member-info (alien-type)
  "Return (values base-type count) for a potential HFA member, or NIL if not HFA-compatible.
   BASE-TYPE is 'single-float or 'double-float, COUNT is the number of elements."
  (cond
    ;; Single-float scalar
    ((sb-alien::alien-single-float-type-p alien-type)
     (values 'single-float 1))
    ;; Double-float scalar
    ((sb-alien::alien-double-float-type-p alien-type)
     (values 'double-float 1))
    ;; Array type - check if element type is float
    ((sb-alien::alien-array-type-p alien-type)
     (let ((element-type (sb-alien::alien-array-type-element-type alien-type))
           (dims (sb-alien::alien-array-type-dimensions alien-type)))
       ;; Only 1-D arrays for HFA
       (when (and (= (length dims) 1)
                  (integerp (first dims)))
         (let ((len (first dims)))
           (cond
             ((sb-alien::alien-single-float-type-p element-type)
              (values 'single-float len))
             ((sb-alien::alien-double-float-type-p element-type)
              (values 'double-float len))
             ;; Could also be an array of HFA structs
             ((sb-alien::alien-record-type-p element-type)
              (multiple-value-bind (nested-base nested-count)
                  (hfa-base-type element-type)
                (when nested-base
                  (values nested-base (* len nested-count)))))
             (t nil))))))
    ;; Nested record - recursively check HFA
    ((sb-alien::alien-record-type-p alien-type)
     (hfa-base-type alien-type))
    ;; Non-float field
    (t nil)))

(defun hfa-base-type (record-type)
  "Check if record is an HFA. Returns (values base-type member-count) where
   base-type is 'single-float or 'double-float, or NIL if not an HFA."
  (let ((fields (sb-alien::alien-record-type-fields record-type))
        (base-type nil)
        (count 0))
    (dolist (field fields)
      (let ((field-type (sb-alien::alien-record-field-type field)))
        (multiple-value-bind (member-base member-count)
            (hfa-member-info field-type)
          (cond
            ;; Not HFA-compatible member
            ((null member-base)
             (return-from hfa-base-type nil))
            ;; Compatible with existing base type (or first member)
            ((or (null base-type) (eq base-type member-base))
             (setf base-type member-base)
             (incf count member-count))
            ;; Mixed float types - not an HFA
            (t (return-from hfa-base-type nil))))))
    ;; HFA must have 1-4 members
    (when (and base-type (<= 1 count 4))
      (values base-type count))))

;;; Main classification function for ARM64
(defun classify-struct-arm64 (record-type)
  "Classify struct for ARM64 AAPCS return."
  (let* ((bits (sb-alien::alien-type-bits record-type))
         (byte-size (ceiling bits 8))
         (alignment (sb-alien::alien-type-alignment record-type)))
    (multiple-value-bind (hfa-type hfa-count) (hfa-base-type record-type)
      (cond
        ;; HFA: return in floating-point registers
        (hfa-type
         (sb-alien::make-struct-classification
          :register-slots (make-list hfa-count :initial-element
                                 (if (eq hfa-type 'single-float) :sse-single :sse-double))
          :size byte-size
          :alignment alignment
          :memory-p nil))
        ;; Small non-HFA: return in x0 (and x1 if 9-16 bytes)
        ((<= byte-size 16)
         (sb-alien::make-struct-classification
          :register-slots (make-list (max 1 (ceiling byte-size 8)) :initial-element :integer)
          :size byte-size
          :alignment alignment
          :memory-p nil))
        ;; Large struct: use x8 indirect result
        (t
         (sb-alien::make-struct-classification
          :register-slots '(:memory)
          :size byte-size
          :alignment alignment
          :memory-p t))))))

;;; Result TN generation for record types on ARM64
;;; Called from src/code/c-call.lisp
(defun record-result-tn-arm64 (type state)
  "Handle struct return values on ARM64."
  (let ((classification (classify-struct-arm64 type)))
    (if (sb-alien::struct-classification-memory-p classification)
        ;; Large struct: return via hidden pointer in x8
        ;; The caller allocates space and passes pointer in x8
        (progn
          (setf (result-state-num-results state) 1)
          (make-wired-tn* 'system-area-pointer sap-reg-sc-number (result-reg-offset 0)))
        ;; Small struct: return in registers
        (let ((result-tns nil)
              (int-results 0)
              (fp-results 0))
          (dolist (class (sb-alien::struct-classification-register-slots classification))
            (case class
              (:integer
               (push (make-wired-tn* 'unsigned-byte-64
                                     unsigned-reg-sc-number
                                     (result-reg-offset int-results))
                     result-tns)
               (incf int-results))
              (:sse-single
               (push (make-wired-tn* 'single-float
                                     single-reg-sc-number
                                     fp-results)
                     result-tns)
               (incf fp-results))
              (:sse-double
               (push (make-wired-tn* 'double-float
                                     double-reg-sc-number
                                     fp-results)
                     result-tns)
               (incf fp-results))
              (t))) ; skip unknown
          (setf (result-state-num-results state) (+ int-results fp-results))
          (nreverse result-tns)))))

;;; VOPs for struct argument passing on ARM64
;;; These VOPs load register slots from a struct SAP into target registers

(define-vop (load-struct-int-arg)
  (:args (sap :scs (sap-reg)))
  (:info offset)
  (:results (target :scs (unsigned-reg signed-reg)))
  (:generator 5
    (inst ldr target (@ sap offset))))

(define-vop (load-struct-single-arg)
  (:args (sap :scs (sap-reg)))
  (:info offset)
  (:results (target :scs (single-reg)))
  (:generator 5
    (inst ldr target (@ sap offset))))

(define-vop (load-struct-double-arg)
  (:args (sap :scs (sap-reg)))
  (:info offset)
  (:results (target :scs (double-reg)))
  (:generator 5
    (inst ldr target (@ sap offset))))

;;; VOPs for storing struct result registers to memory
;;; These VOPs store result register values back to memory for struct-by-value returns

(define-vop (store-struct-int-result)
  (:args (value :scs (unsigned-reg signed-reg))
         (sap :scs (sap-reg)))
  (:info offset)
  (:generator 5
    (inst str value (@ sap offset))))

(define-vop (store-struct-single-result)
  (:args (value :scs (single-reg))
         (sap :scs (sap-reg)))
  (:info offset)
  (:generator 5
    (inst str value (@ sap offset))))

(define-vop (store-struct-double-result)
  (:args (value :scs (double-reg))
         (sap :scs (sap-reg)))
  (:info offset)
  (:generator 5
    (inst str value (@ sap offset))))

;;; Arg TN generation for record types on ARM64
;;; Called from src/code/c-call.lisp
(defun record-arg-tn-arm64 (type state)
  "Handle struct arguments on ARM64.
   For large structs (>16 bytes), returns a SAP TN for pointer passing.
   For small structs, returns a function that emits load VOPs."
  (let ((classification (classify-struct-arm64 type)))
    (if (sb-alien::struct-classification-memory-p classification)
        ;; Large struct: pass by pointer
        (int-arg state 'system-area-pointer sap-reg-sc-number sap-stack-sc-number)
        ;; Small struct: allocate target TNs and return a function to load into them
        (let ((arg-tns nil)
              (offsets nil)
              (offset 0))
          (dolist (class (sb-alien::struct-classification-register-slots classification))
            (case class
              (:integer
               (push (int-arg state 'unsigned-byte-64
                              unsigned-reg-sc-number
                              unsigned-stack-sc-number)
                     arg-tns)
               (push (cons offset :integer) offsets)
               (incf offset 8))
              (:sse-single
               (push (float-arg state 'single-float
                                single-reg-sc-number
                                single-stack-sc-number #+darwin 4)
                     arg-tns)
               (push (cons offset :sse-single) offsets)
               (incf offset 4))
              (:sse-double
               (push (float-arg state 'double-float
                                double-reg-sc-number
                                double-stack-sc-number)
                     arg-tns)
               (push (cons offset :sse-double) offsets)
               (incf offset 8))
              (t))) ; skip unknown
          (setf arg-tns (nreverse arg-tns))
          (setf offsets (nreverse offsets))
          ;; Return a function that emits the load VOPs
          (lambda (arg call block nsp)
            (declare (ignore nsp))
            (let ((sap-tn (sb-c::lvar-tn call block arg)))
              (loop for target-tn in arg-tns
                    for (off . class) in offsets
                    do (ecase class
                         (:integer
                          (sb-c::emit-and-insert-vop
                           call block
                           (sb-c::template-or-lose 'load-struct-int-arg)
                           (sb-c::reference-tn sap-tn nil)
                           (sb-c::reference-tn target-tn t)
                           nil
                           (list off)))
                         (:sse-single
                          (sb-c::emit-and-insert-vop
                           call block
                           (sb-c::template-or-lose 'load-struct-single-arg)
                           (sb-c::reference-tn sap-tn nil)
                           (sb-c::reference-tn target-tn t)
                           nil
                           (list off)))
                         (:sse-double
                          (sb-c::emit-and-insert-vop
                           call block
                           (sb-c::template-or-lose 'load-struct-double-arg)
                           (sb-c::reference-tn sap-tn nil)
                           (sb-c::reference-tn target-tn t)
                           nil
                           (list off)))))))))))

(defun make-call-out-tns (type)
  (let ((arg-state (make-arg-state))
        (result-type (alien-fun-type-result-type type)))
    (collect ((arg-tns))
      (let (#+darwin (variadic (sb-alien::alien-fun-type-varargs type)))
        (loop for i from 0
              for arg-type in (alien-fun-type-arg-types type)
              do
              #+darwin
              (when (eql i variadic)
                (setf (arg-state-num-register-args arg-state) +max-register-args+
                      (arg-state-fp-registers arg-state) +max-register-args+))
              (arg-tns (invoke-alien-type-method :arg-tn arg-type arg-state))))
      ;; Check if result is a large struct that needs hidden pointer
      (let* ((stack-frame-size (arg-state-stack-frame-size arg-state))
             ;; For large struct returns, we don't allocate stack space here
             ;; The IR1 transform allocates heap memory and passes it as first arg
             ;; We just return a flag indicating this is a large struct return
             (large-struct-return-p
               (when (sb-alien::alien-record-type-p result-type)
                 (let ((classification (classify-struct-arm64 result-type)))
                   (sb-alien::struct-classification-memory-p classification)))))
        (values (make-normal-tn *fixnum-primitive-type*)
                stack-frame-size
                (arg-tns)
                (invoke-alien-type-method :result-tn result-type (make-result-state))
                ;; 5th value: T if large struct return (sret pointer passed as first arg)
                large-struct-return-p)))))

;;; VOP to set up x8 with the hidden struct return pointer
;;; On ARM64, large structs (>16 bytes) are returned via a hidden pointer in x8
;;; The caller allocates memory and passes the address in x8
(define-vop (set-struct-return-pointer)
  (:args (sap :scs (sap-reg) :target x8))
  (:temporary (:sc sap-reg :offset 8) x8)  ; x8 is the indirect result register
  (:generator 1
    (move x8 sap)))

(define-vop (foreign-symbol-sap)
  (:translate foreign-symbol-sap)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (load-foreign-symbol res foreign-symbol)))

(define-vop (foreign-symbol-dataref-sap)
  (:translate foreign-symbol-dataref-sap)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (load-foreign-symbol res foreign-symbol :dataref t)))

#+(or sb-safepoint nonstop-foreign-call)
(defconstant thread-saved-csp-slot -1)

(defconstant-eqx +destroyed-c-registers+
  (loop for i from 0 to 18 collect i)
  #'equal)

(defun emit-c-call (vop nfp-save temp temp2 cfunc function)
  (let ((cur-nfp (current-nfp-tn vop)))
    (when cur-nfp
      (store-stack-tn nfp-save cur-nfp))
    (assemble ()
      #+sb-thread
      (progn
        (inst add temp csp-tn (* 2 n-word-bytes))
        ;; Build a new frame to stash a pointer to the current code object
        ;; for the GC to see.
        (inst adr temp2 return)
        (inst stp cfp-tn temp2 (@ csp-tn))
        (storew-pair csp-tn thread-control-frame-pointer-slot temp thread-control-stack-pointer-slot thread-tn)
        ;; OK to run GC without stopping this thread from this point
        ;; on.
        #+(or sb-safepoint nonstop-foreign-call)
        (storew csp-tn thread-tn thread-saved-csp-slot)
        (cond ((stringp function)
               (invoke-foreign-routine function cfunc))
              (t
               (sc-case function
                 (sap-reg (move cfunc function))
                 (sap-stack
                  (load-stack-offset cfunc cur-nfp function)))
               (inst blr cfunc)))
        ;; Blank all boxed registers that potentially contain Lisp
        ;; pointers, not just volatile ones, since GC could
        ;; potentially observe stale pointers otherwise.
        (loop for reg in (set-difference descriptor-regs
                                         ;; any-reg temporaries contain no pointers
                                         (list #-immobile-space r9-offset ;; invoke-foreign-routine doesn't touch it
                                               r10-offset lexenv-offset))
              do (inst mov
                       (make-random-tn (sc-or-lose 'descriptor-reg) reg)
                       0))
        ;; No longer OK to run GC except at safepoints.
        #+(or sb-safepoint nonstop-foreign-call)
        (storew zr-tn thread-tn thread-saved-csp-slot))
      (storew zr-tn thread-tn thread-control-stack-pointer-slot)
      return
      #-sb-thread
      (progn
        temp2
        (if (stringp function)
            (load-foreign-symbol cfunc function)
            (sc-case function
              (sap-reg (move cfunc function))
              (sap-stack
              (load-stack-offset cfunc cur-nfp function))))
        (invoke-foreign-routine "call_into_c" temp))
      (when cur-nfp
        (load-stack-tn cur-nfp nfp-save)))))

(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  (defun destroyed-c-registers ()
    (let ((gprs +destroyed-c-registers+)
          (vars))
      (append
       (loop for gpr in gprs
             collect `(:temporary (:sc any-reg :offset ,gpr :from :eval :to :result)
                                  ,(car (push (gensym) vars))))
       (loop for float to 31
             collect `(:temporary (:sc single-reg :offset ,float :from :eval :to :result)
                                  ,(car (push (gensym) vars))))
       `((:ignore ,@vars))))))

(define-vop (call-out)
  (:args (function :scs (sap-reg sap-stack))
         (args :more t))
  (:results (results :more t))
  (:ignore args results lr)
  (:temporary (:sc non-descriptor-reg :offset lr-offset) lr)
  (:temporary (:sc any-reg :offset r9-offset
               :from (:argument 0) :to (:result 0)) cfunc)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:temporary (:sc any-reg :offset r10-offset) temp)
  (:temporary (:sc any-reg :offset lexenv-offset) temp2)
  (:vop-var vop)
  (:generator 0
    (emit-c-call vop nfp-save temp temp2 cfunc function))
  .
  #. (destroyed-c-registers))

;;; Manually load the fixup instead of using foreign-symbol-sap,
;;; because it wants to go to r9, which is not compatible with sap-reg.
(define-vop (call-out-named call-out)
  (:args (args :more t))
  (:info function variadic)
  (:ignore args results variadic lr))

(define-vop (alloc-number-stack-space)
  (:info amount)
  (:result-types system-area-pointer)
  (:results (result :scs (sap-reg any-reg)))
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount +number-stack-alignment-mask+)
                             +number-stack-alignment-mask+)))
        (inst sub nsp-tn nsp-tn (add-sub-immediate delta))
        (inst mov-sp result nsp-tn)))))

(define-vop (dealloc-number-stack-space)
  (:info amount)
  (:policy :fast-safe)
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount +number-stack-alignment-mask+)
                             +number-stack-alignment-mask+)))
        (inst add nsp-tn nsp-tn (add-sub-immediate delta))))))

;;; Callback
#-sb-xc-host
(defun alien-callback-accessor-form (type sap offset)
  `(deref (sap-alien (sap+ ,sap ,offset) (* ,type))))

#-sb-xc-host
(defun alien-callback-assembler-wrapper (index result-type argument-types)
  (flet ((make-tn (offset &optional (sc-name 'any-reg))
           (make-random-tn (sc-or-lose sc-name) offset)))
    ;; Calculate frame size: sum of all argument sizes
    (let* ((segment (make-segment))
           ;; Current byte offset in the argument frame
           (frame-offset 0)
           ;; How many bytes have been read from the stack argument area
           (stack-argument-bytes 0)
           (r0-tn (make-tn 0))
           (r1-tn (make-tn 1))
           (r2-tn (make-tn 2))
           (r3-tn (make-tn 3))
           (temp-tn (make-tn 9))
           (nsp-save-tn (make-tn 10))
           (gprs (loop for i below 8
                       collect (make-tn i)))
           (fp-registers 0)
           ;; Calculate frame size from argument types (word-aligned)
           (frame-size (loop for type in argument-types
                             sum (ceiling (sb-alien::alien-type-word-aligned-bits type)
                                          n-byte-bits))))
      (setf frame-size (logandc2 (+ frame-size +number-stack-alignment-mask+)
                                 +number-stack-alignment-mask+))
      (assemble (segment 'nil)
        (inst mov-sp nsp-save-tn nsp-tn)
        (inst str lr-tn (@ nsp-tn -16 :pre-index))
        ;; Make room on the stack for arguments.
        (when (plusp frame-size)
          (inst sub nsp-tn nsp-tn frame-size))
        ;; Copy arguments
        (dolist (type argument-types)
          (let ((target-tn (@ nsp-tn frame-offset))
                (size #+darwin (truncate (sb-alien::alien-type-bits type) n-byte-bits)
                      #-darwin n-word-bytes))
            (cond ((or (alien-integer-type-p type)
                       (alien-pointer-type-p type)
                       (alien-type-= #.(parse-alien-type 'system-area-pointer nil)
                                     type))
                   (let ((gpr (pop gprs)))
                     (cond (gpr
                            (inst str gpr target-tn))
                           (t
                            (setf stack-argument-bytes
                                  (align-up stack-argument-bytes size))
                            (let ((addr (@ nsp-save-tn stack-argument-bytes)))
                              (cond #+darwin
                                    ((/= size 8)
                                     (let ((signed (and (alien-integer-type-p type)
                                                        (alien-integer-type-signed type))))
                                       (ecase size
                                         (1
                                          (if signed
                                              (inst ldrsb temp-tn addr)
                                              (inst ldrb temp-tn addr)))
                                         (2
                                          (if signed
                                              (inst ldrsh temp-tn addr)
                                              (inst ldrh temp-tn addr)))
                                         (4
                                          (if signed
                                              (inst ldrsw (32-bit-reg temp-tn) addr)
                                              (inst ldr (32-bit-reg temp-tn) addr))))))
                                    (t
                                     (inst ldr temp-tn addr)))
                              (inst str temp-tn target-tn))
                            (incf stack-argument-bytes size))))
                   (incf frame-offset n-word-bytes))
                  ((alien-float-type-p type)
                   (cond ((< fp-registers 8)
                          (inst str (make-tn fp-registers
                                             (if (alien-single-float-type-p type)
                                                 'single-reg
                                                 'double-reg))
                                target-tn))
                         (t
                          (setf stack-argument-bytes
                                  (align-up stack-argument-bytes size))
                          (case size
                            #+darwin
                            (4
                             (let ((reg (32-bit-reg temp-tn)))
                              (inst ldr reg (@ nsp-save-tn stack-argument-bytes))
                              (inst str reg target-tn)))
                            (t
                             (inst ldr temp-tn (@ nsp-save-tn stack-argument-bytes))
                             (inst str temp-tn target-tn)))
                          (incf stack-argument-bytes size)))
                   (incf fp-registers)
                   (incf frame-offset (if (alien-single-float-type-p type) 4 8)))
                  ;; Handle struct-by-value arguments
                  ((sb-alien::alien-record-type-p type)
                   (let* ((struct-bytes (ceiling (sb-alien::alien-type-bits type) n-byte-bits))
                          (struct-bytes-aligned (ceiling (sb-alien::alien-type-word-aligned-bits type)
                                                         n-byte-bits))
                          (classification (classify-struct-arm64 type))
                          ;; Use r11 as additional temp for struct pointer
                          (ptr-tn (make-tn 11)))
                     (cond
                       ;; Large struct (>16 bytes): passed by pointer in register
                       ((sb-alien::struct-classification-memory-p classification)
                        ;; The struct pointer is in a GPR; copy struct data to frame
                        (let ((gpr (pop gprs)))
                          (cond (gpr
                                 ;; Move pointer from argument register to ptr-tn
                                 (inst mov ptr-tn gpr))
                                (t
                                 ;; Pointer is on stack
                                 (setf stack-argument-bytes (align-up stack-argument-bytes 8))
                                 (inst ldr ptr-tn (@ nsp-save-tn stack-argument-bytes))
                                 (incf stack-argument-bytes 8)))
                          ;; Copy struct data from pointer to frame
                          ;; Use temp-tn (r9) for copying, ptr-tn (r11) has source address
                          (loop for off from 0 below struct-bytes by 8
                                for remaining = (- struct-bytes off)
                                do (cond ((>= remaining 8)
                                          (inst ldr temp-tn (@ ptr-tn off))
                                          (inst str temp-tn (@ nsp-tn (+ frame-offset off))))
                                         ((>= remaining 4)
                                          (inst ldr (32-bit-reg temp-tn) (@ ptr-tn off))
                                          (inst str (32-bit-reg temp-tn) (@ nsp-tn (+ frame-offset off))))
                                         (t
                                          ;; Copy remaining bytes one by one
                                          (loop for b from 0 below remaining
                                                do (inst ldrb (32-bit-reg temp-tn) (@ ptr-tn (+ off b)))
                                                   (inst strb (32-bit-reg temp-tn) (@ nsp-tn (+ frame-offset off b)))))))))
                       ;; HFA: passed in floating-point registers
                       ((multiple-value-bind (hfa-type hfa-count) (hfa-base-type type)
                          (when hfa-type
                            (let ((fp-size (if (eq hfa-type 'single-float) 4 8)))
                              (dotimes (i hfa-count)
                                (when (< fp-registers 8)
                                  (inst str (make-tn fp-registers
                                                     (if (eq hfa-type 'single-float)
                                                         'single-reg
                                                         'double-reg))
                                        (@ nsp-tn (+ frame-offset (* i fp-size))))
                                  (incf fp-registers))))
                            t)))
                       ;; Small non-HFA struct (<=16 bytes): passed in GPRs
                       (t
                        (let ((num-regs (ceiling struct-bytes 8)))
                          (dotimes (i num-regs)
                            (let ((gpr (pop gprs)))
                              (when gpr
                                (inst str gpr (@ nsp-tn (+ frame-offset (* i 8))))))))))
                     ;; Use word-aligned size for frame offset to match Lisp side
                     (incf frame-offset struct-bytes-aligned)))
                  (t
                   (bug "Unknown alien type: ~S" type)))))
        ;; arg0 to ENTER-ALIEN-CALLBACK (trampoline index)
        (inst mov r0-tn (fixnumize index))
        ;; arg1 to ENTER-ALIEN-CALLBACK (pointer to argument vector)
        (inst mov-sp r1-tn nsp-tn)
        ;; add room on stack for return value
        (inst sub nsp-tn nsp-tn (* n-word-bytes 2))
        ;; arg2 to ENTER-ALIEN-CALLBACK (pointer to return value)
        (inst mov-sp r2-tn nsp-tn)

        ;; Call
        (load-immediate-word r3-tn (foreign-symbol-address "callback_wrapper_trampoline"))
        (inst blr r3-tn)

        ;; Result now on top of stack, put it in the right register
        (cond
          ((or (alien-integer-type-p result-type)
               (alien-pointer-type-p result-type)
               (alien-type-= #.(parse-alien-type 'system-area-pointer nil)
                             result-type))
           (loadw r0-tn nsp-tn))
          ((alien-float-type-p result-type)
           (loadw (make-tn fp-registers
                              (if (alien-single-float-type-p result-type)
                                  'single-reg
                                  'double-reg))
                 nsp-tn))
          ((alien-void-type-p result-type))
          (t
           (error "Unrecognized alien type: ~A" result-type)))
        (inst add nsp-tn nsp-tn (+ frame-size (* n-word-bytes 2)))
        (inst ldr lr-tn (@ nsp-tn 16 :post-index))
        (inst ret))
      (finalize-segment segment)
      ;; Now that the segment is done, convert it to a static
      ;; vector we can point foreign code to.
      (let* ((buffer (sb-assem:segment-buffer segment))
             (vector #-darwin-jit
                     (make-static-vector (length buffer)
                                         :element-type '(unsigned-byte 8)
                                         :initial-contents buffer)
                     #+darwin-jit
                     (make-static-code-vector (length buffer)
                                              buffer))
             (sap (vector-sap vector)))
        (alien-funcall
         (extern-alien "os_flush_icache"
                       (function void
                                 system-area-pointer
                                 unsigned-long))
         sap (length buffer))
        vector))))
