;;;; the VOPs and other necessary machine specific support
;;;; routines for call-out to C

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;; The MOVE-ARG vop is going to store args on the stack for
;; call-out. These tn's will be used for that. move-arg is normally
;; used for things going down the stack but C wants to have args
;; indexed in the positive direction.

(defstruct (arg-state (:copier nil))
  (register-args 0)
  (xmm-args 0)
  (stack-frame-size 0))
(declaim (freeze-type arg-state))

(defconstant max-int-args #.(length *c-call-register-arg-offsets*))
(defconstant max-xmm-args #+win32 4 #-win32 8)

(defun int-arg (state prim-type reg-sc stack-sc)
  (let ((reg-args (max (arg-state-register-args state)
                       #+win32 (arg-state-xmm-args state))))
    (cond ((< reg-args max-int-args)
           (setf (arg-state-register-args state) (1+ reg-args))
           (make-wired-tn* prim-type reg-sc
                             (nth reg-args *c-call-register-arg-offsets*)))
          (t
           (let ((frame-size (arg-state-stack-frame-size state)))
             (setf (arg-state-stack-frame-size state) (1+ frame-size))
             (make-wired-tn* prim-type stack-sc frame-size))))))

(define-alien-type-method (integer :arg-tn) (type state)
  (if (alien-integer-type-signed type)
      (int-arg state 'signed-byte-64 signed-reg-sc-number signed-stack-sc-number)
      (int-arg state 'unsigned-byte-64 unsigned-reg-sc-number unsigned-stack-sc-number)))

(define-alien-type-method (system-area-pointer :arg-tn) (type state)
  (declare (ignore type))
  (int-arg state 'system-area-pointer sap-reg-sc-number sap-stack-sc-number))

(defun float-arg (state prim-type reg-sc stack-sc)
  (let ((xmm-args (max (arg-state-xmm-args state)
                        #+win32 (arg-state-register-args state))))
    (cond ((< xmm-args max-xmm-args)
           (setf (arg-state-xmm-args state) (1+ xmm-args))
           (make-wired-tn* prim-type reg-sc
                             (nth xmm-args *float-regs*)))
          (t
           (let ((frame-size (arg-state-stack-frame-size state)))
             (setf (arg-state-stack-frame-size state) (1+ frame-size))
             (make-wired-tn* prim-type stack-sc frame-size))))))

(define-alien-type-method (double-float :arg-tn) (type state)
  (declare (ignore type))
  (float-arg state 'double-float double-reg-sc-number double-stack-sc-number))

(define-alien-type-method (single-float :arg-tn) (type state)
  (declare (ignore type))
  (float-arg state 'single-float single-reg-sc-number single-stack-sc-number))

(defstruct (result-state (:copier nil))
  (num-results 0))
(declaim (freeze-type result-state))

(defun result-reg-offset (slot)
  (ecase slot
    (0 rax-offset)
    (1 rdx-offset)))

(define-alien-type-method (integer :result-tn) (type state)
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (multiple-value-bind (ptype reg-sc)
        (if (alien-integer-type-signed type)
            (values 'signed-byte-64 signed-reg-sc-number)
            (values 'unsigned-byte-64 unsigned-reg-sc-number))
      (make-wired-tn* ptype reg-sc (result-reg-offset num-results)))))

(define-alien-type-method (integer :naturalize-gen) (type alien)
  (if (<= (alien-type-bits type) 32)
      (if (alien-integer-type-signed type)
          `(sign-extend ,alien ,(alien-type-bits type))
          `(logand ,alien ,(1- (ash 1 (alien-type-bits type)))))
      alien))

(define-alien-type-method (system-area-pointer :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (make-wired-tn* 'system-area-pointer sap-reg-sc-number
                      (result-reg-offset num-results))))

(define-alien-type-method (double-float :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (make-wired-tn* 'double-float double-reg-sc-number num-results)))

(define-alien-type-method (single-float :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (make-wired-tn* 'single-float single-reg-sc-number num-results)))

(define-alien-type-method (values :result-tn) (type state)
  (let ((values (alien-values-type-values type)))
    (when (> (length values) 2)
      (error "Too many result values from c-call."))
    (mapcar (lambda (type)
              (invoke-alien-type-method :result-tn type state))
            values)))

;;;; Struct Return-by-Value Support for x86-64 (System V AMD64 ABI)

;;; Classify a single field for x86-64 ABI
(defun classify-field-x86-64 (type)
  "Classify a single field type for x86-64 ABI.
   Returns :INTEGER, :SSE, or :MEMORY."
  (cond
    ((sb-alien::alien-integer-type-p type) :integer)
    ((sb-alien::alien-pointer-type-p type) :integer)
    ((typep type 'sb-alien::alien-system-area-pointer-type) :integer)
    ((sb-alien::alien-single-float-type-p type) :sse)
    ((sb-alien::alien-double-float-type-p type) :sse)
    ((sb-alien::alien-record-type-p type)
     ;; Recursively classify nested struct - if it's MEMORY, propagate that
     (let ((nested (classify-struct-x86-64 type)))
       (if (sb-alien::struct-classification-memory-p nested)
           :memory
           ;; For nested structs that fit in registers, treat as INTEGER
           ;; since the struct's fields have been merged
           :integer)))
    (t :memory)))

;;; Merge two classes within an eightbyte per ABI rules
(defun merge-classes (class1 class2)
  "Merge two classes within an eightbyte per ABI rules.
   INTEGER dominates SSE; MEMORY dominates everything."
  (cond
    ((eq class1 class2) class1)
    ((eq class1 :no-class) class2)
    ((eq class2 :no-class) class1)
    ((or (eq class1 :memory) (eq class2 :memory)) :memory)
    ((or (eq class1 :integer) (eq class2 :integer)) :integer)
    (t :sse)))

;;; Main classification function for x86-64
(defun classify-struct-x86-64 (record-type)
  "Classify struct for x86-64 System V ABI return.
   Returns STRUCT-CLASSIFICATION."
  (let* ((bits (sb-alien::alien-type-bits record-type))
         (byte-size (ceiling bits 8))
         (alignment (sb-alien::alien-type-alignment record-type)))
    ;; Rule: Structs > 16 bytes always use memory (hidden pointer)
    (when (> byte-size 16)
      (return-from classify-struct-x86-64
        (sb-alien::make-struct-classification
         :register-slots '(:memory)
         :size byte-size
         :alignment alignment
         :memory-p t)))

    ;; Classify each eightbyte
    (let* ((num-eightbytes (max 1 (ceiling byte-size 8)))
           (eightbytes (make-list num-eightbytes :initial-element :no-class)))
      ;; Iterate through fields and classify
      (dolist (field (sb-alien::alien-record-type-fields record-type))
        (let* ((field-offset-bits (sb-alien::alien-record-field-offset field))
               (field-type (sb-alien::alien-record-field-type field))
               (field-offset-bytes (floor field-offset-bits 8))
               (eightbyte-index (floor field-offset-bytes 8))
               (field-class (classify-field-x86-64 field-type)))
          (when (< eightbyte-index num-eightbytes)
            (setf (nth eightbyte-index eightbytes)
                  (merge-classes (nth eightbyte-index eightbytes)
                                 field-class)))))

      ;; Post-merge cleanup per ABI: if second eightbyte is MEMORY, first must be too
      (when (and (> num-eightbytes 1)
                 (eq (second eightbytes) :memory))
        (setf (first eightbytes) :memory))

      ;; Convert remaining :no-class to :integer (padding bytes are treated as integer)
      (setf eightbytes
            (mapcar (lambda (c) (if (eq c :no-class) :integer c)) eightbytes))

      (sb-alien::make-struct-classification
       :register-slots eightbytes
       :size byte-size
       :alignment alignment
       :memory-p (member :memory eightbytes)))))

;;; Result TN generation for record types on x86-64
;;; Called from src/code/c-call.lisp
(defun record-result-tn-x86-64 (type state)
  "Handle struct return values on x86-64."
  (let ((classification (classify-struct-x86-64 type)))
    (if (sb-alien::struct-classification-memory-p classification)
        ;; Large struct: return via hidden pointer
        ;; Caller passes pointer in RDI, callee returns it in RAX
        (progn
          (setf (result-state-num-results state) 1)
          (make-wired-tn* 'system-area-pointer sap-reg-sc-number rax-offset))
        ;; Small struct: return in registers
        (let ((result-tns nil)
              (int-results 0)
              (sse-results 0))
          (dolist (class (sb-alien::struct-classification-register-slots classification))
            (case class
              (:integer
               (push (make-wired-tn* 'unsigned-byte-64
                                     unsigned-reg-sc-number
                                     (result-reg-offset int-results))
                     result-tns)
               (incf int-results))
              (:sse
               (push (make-wired-tn* 'double-float
                                     double-reg-sc-number
                                     sse-results)
                     result-tns)
               (incf sse-results))
              (t))) ; :no-class - skip
          (setf (result-state-num-results state) (+ int-results sse-results))
          (nreverse result-tns)))))

;;; VOPs for struct argument passing
;;; These VOPs load eightbytes from a struct SAP into target registers

(define-vop (load-struct-int-arg)
  (:args (sap :scs (sap-reg)))
  (:info offset)
  (:results (target :scs (unsigned-reg signed-reg)))
  (:generator 5
    (inst mov :qword target (ea offset sap))))

(define-vop (load-struct-sse-arg)
  (:args (sap :scs (sap-reg)))
  (:info offset)
  (:results (target :scs (double-reg single-reg)))
  (:generator 5
    (inst movsd target (ea offset sap))))

;;; VOPs for storing struct result registers to memory
;;; These VOPs store result register values back to memory for struct-by-value returns

(define-vop (store-struct-int-result)
  (:args (value :scs (unsigned-reg signed-reg))
         (sap :scs (sap-reg)))
  (:info offset)
  (:generator 5
    (inst mov :qword (ea offset sap) value)))

(define-vop (store-struct-sse-result)
  (:args (value :scs (double-reg single-reg))
         (sap :scs (sap-reg)))
  (:info offset)
  (:generator 5
    (inst movsd (ea offset sap) value)))

;;; Arg TN generation for record types on x86-64
;;; Called from src/code/c-call.lisp
(defun record-arg-tn-x86-64 (type state)
  "Handle struct arguments on x86-64.
   For large structs (>16 bytes), returns a SAP TN for pointer passing.
   For small structs, returns a function that emits load VOPs."
  (let ((classification (classify-struct-x86-64 type)))
    (if (sb-alien::struct-classification-memory-p classification)
        ;; Large struct: pass by hidden pointer (SAP)
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
               (push (cons offset :integer) offsets))
              (:sse
               (push (float-arg state 'double-float
                                double-reg-sc-number
                                double-stack-sc-number)
                     arg-tns)
               (push (cons offset :sse) offsets))
              (t)) ; :no-class - skip
            (incf offset 8))
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
                           nil  ; insert at end
                           (list off)))
                         (:sse
                          (sb-c::emit-and-insert-vop
                           call block
                           (sb-c::template-or-lose 'load-struct-sse-arg)
                           (sb-c::reference-tn sap-tn nil)
                           (sb-c::reference-tn target-tn t)
                           nil
                           (list off)))))))))))

;;; VOP to set up RDI with the hidden struct return pointer
;;; On x86-64, large structs (>16 bytes) are returned via a hidden pointer in RDI
;;; The caller allocates memory and passes the address in RDI (first arg register)
(define-vop (set-struct-return-pointer)
  (:args (sap :scs (sap-reg) :target rdi))
  (:temporary (:sc sap-reg :offset rdi-offset) rdi)  ; RDI is the first arg register
  (:generator 1
    (move rdi sap)))

(defun make-call-out-tns (type)
  (let ((arg-state (make-arg-state))
        (result-type (alien-fun-type-result-type type)))
    (collect ((arg-tns))
      (dolist (arg-type (alien-fun-type-arg-types type))
        (arg-tns (invoke-alien-type-method :arg-tn arg-type arg-state)))
      ;; Check if result is a large struct that needs hidden pointer
      (let* ((stack-frame-size (* (arg-state-stack-frame-size arg-state) n-word-bytes))
             ;; For large struct returns, we don't allocate stack space here
             ;; The IR1 transform allocates heap memory and passes it as first arg
             ;; We just return a flag indicating this is a large struct return
             (large-struct-return-p
               (when (sb-alien::alien-record-type-p result-type)
                 (let ((classification (classify-struct-x86-64 result-type)))
                   (sb-alien::struct-classification-memory-p classification)))))
        (values (make-wired-tn* 'positive-fixnum any-reg-sc-number rsp-offset)
                stack-frame-size
                (arg-tns)
                (invoke-alien-type-method :result-tn result-type (make-result-state))
                ;; 5th value: T if large struct return (sret pointer passed as first arg)
                large-struct-return-p)))))


(deftransform %alien-funcall ((function type &rest args) * * :node node)
  (aver (sb-c:constant-lvar-p type))
  (let* ((type (sb-c:lvar-value type))
         (env (sb-c::node-lexenv node))
         (arg-types (alien-fun-type-arg-types type))
         (result-type (alien-fun-type-result-type type))
         ;; Large struct returns have a hidden first arg (sret pointer) added by IR1
         (large-struct-return-p
           (multiple-value-bind (in-registers-p register-slots size)
               (sb-alien::struct-return-info result-type)
             (declare (ignore register-slots))
             (and size (not in-registers-p)))))
    (aver (= (length arg-types)
             (- (length args) (if large-struct-return-p 1 0))))
    (if (or (some #'(lambda (type)
                      (and (alien-integer-type-p type)
                           (> (sb-alien::alien-integer-type-bits type) 64)))
                  arg-types)
            (and (alien-integer-type-p result-type)
                 (> (sb-alien::alien-integer-type-bits result-type) 64)))
        (collect ((new-args) (lambda-vars) (new-arg-types))
          (dolist (type arg-types)
            (let ((arg (gensym)))
              (lambda-vars arg)
              (cond ((and (alien-integer-type-p type)
                          (> (sb-alien::alien-integer-type-bits type) 64))
                     ;; CLH: FIXME! This should really be
                     ;; #xffffffffffffffff. nyef says: "Passing
                     ;; 128-bit integers to ALIEN functions on x86-64
                     ;; believed to be broken."
                     (new-args `(logand ,arg #xffffffff))
                     (new-args `(ash ,arg -64))
                     (new-arg-types (parse-alien-type '(unsigned 64) env))
                     (if (alien-integer-type-signed type)
                         (new-arg-types (parse-alien-type '(signed 64) env))
                         (new-arg-types (parse-alien-type '(unsigned 64) env))))
                    (t
                     (new-args arg)
                     (new-arg-types type)))))
          (cond ((and (alien-integer-type-p result-type)
                      (> (sb-alien::alien-integer-type-bits result-type) 64))
                 (let ((new-result-type
                        (let ((sb-alien::*values-type-okay* t))
                          (parse-alien-type
                           (if (alien-integer-type-signed result-type)
                               '(values (unsigned 64) (signed 64))
                               '(values (unsigned 64) (unsigned 64)))
                           env))))
                   `(lambda (function type ,@(lambda-vars))
                      (declare (ignore type))
                      (multiple-value-bind (low high)
                          (%alien-funcall function
                                          ',(make-alien-fun-type
                                             :arg-types (new-arg-types)
                                             :result-type new-result-type)
                                          ,@(new-args))
                        (logior low (ash high 64))))))
                (t
                 `(lambda (function type ,@(lambda-vars))
                    (declare (ignore type))
                    (%alien-funcall function
                                    ',(make-alien-fun-type
                                       :arg-types (new-arg-types)
                                       :result-type result-type)
                                    ,@(new-args))))))
        (sb-c::give-up-ir1-transform))))

;;; The ABI is vague about how signed sub-word integer return values
;;; are handled, but since gcc versions >=4.3 no longer do sign
;;; extension in the callee, we need to do it in the caller.  FIXME:
;;; If the value to be extended is known to already be of the target
;;; type at compile time, we can (and should) elide the extension.
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
    (inst movsx `(,(ecase size (8 :byte) (16 :word) (32 :dword)) :qword) res val)))

#-sb-xc-host
(defun sign-extend (x size)
  (declare (type (signed-byte 64) x))
  (ecase size
    (8 (sign-extend x size))
    (16 (sign-extend x size))
    (32 (sign-extend x size))))

;;; There is a troublesome assumption about alien code linkage entries, namely that you
;;; can reference entry + 8 to extract the actual address of the C function.
;;; This is not ideal, for two distinct reasons:
;;;
;;; (1) The linkage entry should contain instructions for GC yieldpoint cooperation,
;;; removing such instructions from call out sites. (You have to inform the GC that
;;; a thread is leaving managed code and entering code that won't execute yieldpoints.)
;;; Clearly this won't work if jumping into the middle of the linkage entry is allowed.
;;;
;;; (2) The CPU has separate I+D caches, and there is a cost to shuttling data between
;;; them. Jumping to an alien linkage entries as they are puts the whole entry into the I
;;; cache (presumably) when the second word should instead be in the D cache.
;;; To optimally structure the entries, all JMPs should precede all data words, like so:
;;;   jmp [RIP+disp]
;;;   jmp [RIP+disp]
;;;   ...
;;;   data ...
;;; And were such change made, it would cease to be valid to jump to an entry + 8.
(define-vop (foreign-symbol-sap)
  (:translate foreign-symbol-sap)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:vop-var vop)
  (:generator 2
    #-immobile-space (inst lea res (ea (make-fixup foreign-symbol :foreign) null-tn))
    #+immobile-space
    (cond ((code-immobile-p vop)
           (inst lea res (rip-relative-ea (make-fixup foreign-symbol :foreign))))
          (t
           (inst mov res (make-fixup foreign-symbol :foreign))
           (inst add res (static-constant-ea alien-linkage-table))))))

(define-vop (foreign-symbol-dataref-sap)
  (:translate foreign-symbol-dataref-sap)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:vop-var vop)
  (:generator 2
    #-immobile-space (inst mov res (ea (make-fixup foreign-symbol :foreign-dataref) null-tn))
    #+immobile-space
    (cond ((code-immobile-p vop)
           (inst mov res (rip-relative-ea (make-fixup foreign-symbol :foreign-dataref))))
          (t
           (inst mov res (static-constant-ea alien-linkage-table))
           (inst mov res (ea (make-fixup foreign-symbol :foreign-dataref) res))))))

#+(or sb-safepoint nonstop-foreign-call)
(defconstant thread-saved-csp-offset -1)

(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  (defun destroyed-c-registers ()
    ;; Safepoints do not save interrupt contexts to be scanned during
    ;; GCing, it only looks at the stack, so if a register isn't
    ;; spilled it won't be visible to the GC.
    #+(or sb-safepoint nonstop-foreign-call)
    '((:save-p t))
    #-(or sb-safepoint nonstop-foreign-call)
    (let ((gprs (list '#:rcx '#:rdx #-win32 '#:rsi #-win32 '#:rdi
                      '#:r8 '#:r9 '#:r10 '#:r11))
          (vars))
      (append
       (loop for gpr in gprs
             for offset = (symbol-value (intern (concatenate 'string (symbol-name gpr) "-OFFSET") "SB-VM"))
             collect `(:temporary (:sc any-reg :offset ,offset :from :eval :to :result)
                                  ,(car (push gpr vars))))
       (loop for float to 15
             for varname = (format nil "FLOAT~D" float)
             collect `(:temporary (:sc single-reg :offset ,float :from :eval :to :result)
                                  ,(car (push (make-symbol varname) vars))))
       `((:ignore ,@vars))))))

(define-vop (call-out)
  (:args (function :scs (sap-reg)
                   :target rbx)
         (args :more t))
  (:results (results :more t))
  ;; RBX is used to first load the address, allowing the debugger to
  ;; determine which alien was accessed in case it's undefined.
  (:temporary (:sc sap-reg :offset rbx-offset :from (:argument 0)) rbx)
  (:temporary (:sc unsigned-reg :offset rax-offset :to :result) rax)
  #+sb-safepoint
  (:temporary (:sc unsigned-stack :from :eval :to :result) pc-save)
  #+win32
  (:temporary (:sc unsigned-reg :offset r15-offset :from :eval :to :result) r15)
  (:ignore results)
  (:vop-var vop)
  (:generator 0
    (move rbx function)
    (emit-c-call vop rax rbx args
                 sb-alien::*alien-fun-type-varargs-default*
                 #+sb-safepoint pc-save
                 #+win32 rbx))
  #+win32 (:ignore r15)
  . #.(destroyed-c-registers))

;;; Calls to C can generally be made without loading a register
;;; with the function. We receive the function name as an info argument.
(define-vop (call-out-named)
  (:args (args :more t))
  (:results (results :more t))
  (:info c-symbol varargsp)
  (:temporary (:sc unsigned-reg :offset rax-offset :to :result) rax)
  #+sb-safepoint
  (:temporary (:sc unsigned-stack :from :eval :to :result) pc-save)
  #+win32
  (:temporary (:sc unsigned-reg :offset r15-offset :from :eval :to :result) r15)
  #+win32
  (:ignore r15)
  (:temporary (:sc unsigned-reg :offset rbx-offset :from :eval :to :result) rbx)
  (:ignore results)
  (:vop-var vop)
  (:generator 0
    (progn rbx)
    (emit-c-call vop rax c-symbol args varargsp
                 #+sb-safepoint pc-save
                 #+win32 rbx))
  . #.(destroyed-c-registers))

;;; Remember when changing this to check that these work:
;;; - disassembly, undefined alien, and conversion to ELF core
(defun emit-c-call (vop rax fun args varargsp #+sb-safepoint pc-save #+win32 rbx)
  (declare (ignorable varargsp))
  ;; Current PC - don't rely on function to keep it in a form that
  ;; GC understands
  #+sb-safepoint
  (let ((label (gen-label)))
    (inst lea rax (rip-relative-ea label))
    (emit-label label)
    (move pc-save rax))
  (when (sb-c:msan-unpoison sb-c:*compilation*)
    (inst mov rax (thread-slot-ea thread-msan-param-tls-slot))
    ;; Unpoison parameters
    (do ((n 0 (+ n n-word-bytes))
         (arg args (tn-ref-across arg)))
        ((null arg))
      ;; KLUDGE: assume all parameters are 8 bytes or less
      (inst mov :qword (ea n rax) 0)))
  #-win32
  ;; ABI: AL contains amount of arguments passed in XMM registers
  ;; for vararg calls.
  (when varargsp
    (move-immediate rax
                    (loop for tn-ref = args then (tn-ref-across tn-ref)
                          while tn-ref
                          count (eq (sb-name (sc-sb (tn-sc (tn-ref-tn tn-ref))))
                                    'float-registers))))

  ;; Store SP in thread struct, unless the enclosing block says not to

  #+(or sb-safepoint nonstop-foreign-call)
  (when (and #+sb-safepoint
             (policy (sb-c::vop-node vop) (/= sb-c:insert-safepoints 0)))
    (inst mov (thread-slot-ea thread-saved-csp-offset) rsp-tn))

  #+win32 (inst sub rsp-tn #x20)       ;MS_ABI: shadow zone

  ;; Immobile code uses "CALL rel32" to reach the linkage table entry,
  ;; but movable code computes the linkage entry address into RBX first.
  ;; N.B.: if you change how the call is emitted, you will also have to adjust
  ;; the UNDEFINED-ALIEN-TRAMP lisp asm routine to recognize the various shapes
  ;; this instruction sequence can take.
  #-win32
  (pseudo-atomic (:elide-if (not (call-out-pseudo-atomic-p vop)))
    (inst call
          #-immobile-space ; always call via RBX
          (cond ((stringp fun) (inst lea rbx-tn (ea (make-fixup fun :foreign) null-tn)) rbx-tn)
                (t fun))
          #+immobile-space ; sometimes call via RBX
          (if (stringp fun)
              (cond ((code-immobile-p vop) (make-fixup fun :foreign))
                    (t
                     (inst mov rbx-tn (make-fixup fun :foreign))
                     (inst add rbx-tn (static-constant-ea alien-linkage-table))
                     rbx-tn))
              ;; Emit a 3-byte NOP so the undefined-alien routine reads a well-defined byte
              ;; on error. In practice, decoding never seemed to go wrong, but looked fishy
              ;; due to the possibility of any random bytes preceding the call.
              (dolist (b '(#x0f #x1f #x00) fun) (inst byte b)))))

  ;; On win64, calls go through a thunk defined in set_up_win64_seh_data().
  #+win32
  (progn
    (cond ((tn-p fun) (move rbx fun)) ; wasn't this already done by the VOP ?
          ;; Compute address of entrypoint in the alien linkage table into RBX
          ((code-immobile-p vop)
           (inst lea rbx (rip-relative-ea (make-fixup fun :foreign))))
          (t
           #-immobile-space (inst lea rbx (ea (make-fixup fun :foreign) null-tn))
           #+immobile-space
           (progn (inst mov rbx (make-fixup fun :foreign))
                  (inst add rbx (static-constant-ea alien-linkage-table)))))
    (invoke-asm-routine 'call 'seh-trampoline vop))

  ;; For the undefined alien error
  (note-this-location vop :internal-error)
  #+win32 (inst add rsp-tn #x20)       ;MS_ABI: remove shadow space

  ;; Zero the saved CSP, unless this code shouldn't ever stop for GC
  #+sb-safepoint
  (when (policy (sb-c::vop-node vop) (/= sb-c:insert-safepoints 0))
    (inst xor (thread-slot-ea thread-saved-csp-offset) rsp-tn))
  #+nonstop-foreign-call
  (inst mov :qword (thread-slot-ea thread-saved-csp-offset) 0))

(define-vop (alloc-number-stack-space)
  (:info amount)
  (:results (result :scs (sap-reg any-reg)))
  (:result-types system-area-pointer)
  (:generator 0
    (aver (location= result rsp-tn))
    (unless (zerop amount)
      (let ((delta (align-up amount 8)))
        (inst sub rsp-tn delta)))
    ;; C stack must be 16 byte aligned
    (inst and rsp-tn -16)
    (move result rsp-tn)))

(macrolet ((alien-stack-ptr ()
             #+sb-thread `(thread-slot-ea ,(symbol-thread-slot '*alien-stack-pointer*))
             #-sb-thread '(static-symbol-value-ea '*alien-stack-pointer*)))
  (define-vop (alloc-alien-stack-space)
    (:info amount)
    (:results (result :scs (sap-reg any-reg)))
    (:result-types system-area-pointer)
    (:generator 0
      (aver (not (location= result rsp-tn)))
      (unless (zerop amount)
        (let ((delta (align-up amount 8)))
          (inst sub :qword (alien-stack-ptr) delta)))
      (inst mov result (alien-stack-ptr)))))

;;; Callbacks

#-sb-xc-host
(defun alien-callback-accessor-form (type sp offset)
  `(deref (sap-alien (sap+ ,sp ,offset) (* ,type))))

#-sb-xc-host
(defun alien-callback-assembler-wrapper (index result-type argument-types)
  (labels ((make-tn-maker (sc-name)
             (lambda (offset)
               (make-random-tn (sc-or-lose sc-name) offset))))
    (let* ((segment (make-segment))
           (rax rax-tn)
           #+win32 (rcx rcx-tn)
           #-(and win32 sb-thread) (rdi rdi-tn)
           #-(and win32 sb-thread) (rsi rsi-tn)
           (rdx rdx-tn)
           (rbp rbp-tn)
           (rsp rsp-tn)
           #+(and win32 sb-thread) (r8 r8-tn)
           (xmm0 float0-tn)
           ([rsp] (ea rsp))
           ;; How many arguments have been copied
           (arg-count 0)
           ;; How many arguments have been copied from the stack
           (stack-argument-count #-win32 0 #+win32 4)
           (gprs (mapcar (make-tn-maker 'any-reg) *c-call-register-arg-offsets*))
           (fprs (mapcar (make-tn-maker 'double-reg)
                         ;; Only 8 first XMM registers are used for
                         ;; passing arguments
                         (subseq *float-regs* 0 #-win32 8 #+win32 4))))
      (assemble (segment 'nil)
        ;; Make room on the stack for arguments.
        (when argument-types
          (inst sub rsp (* n-word-bytes (length argument-types))))
        ;; Copy arguments from registers to stack
        (dolist (type argument-types)
          (let ((integerp (not (alien-float-type-p type)))
                ;; A TN pointing to the stack location where the
                ;; current argument should be stored for the purposes
                ;; of ENTER-ALIEN-CALLBACK.
                (target-tn (ea (* arg-count n-word-bytes) rsp))
                ;; A TN pointing to the stack location that contains
                ;; the next argument passed on the stack.
                (stack-arg-tn (ea (* (+ 1 (length argument-types) stack-argument-count)
                                     n-word-bytes) rsp)))
            (incf arg-count)
            (cond (integerp
                   (let ((gpr (pop gprs)))
                     #+win32 (pop fprs)
                     ;; Argument not in register, copy it from the old
                     ;; stack location to a temporary register.
                     (unless gpr
                       (incf stack-argument-count)
                       (setf gpr rax)
                       (inst mov gpr stack-arg-tn))
                     ;; Copy from either argument register or temporary
                     ;; register to target.
                     (inst mov target-tn gpr)))
                  ((or (alien-single-float-type-p type)
                       (alien-double-float-type-p type))
                   (let ((fpr (pop fprs)))
                     #+win32 (pop gprs)
                     (cond (fpr
                            ;; Copy from float register to target location.
                            (inst movq target-tn fpr))
                           (t
                            ;; Not in float register. Copy from stack to
                            ;; temporary (general purpose) register, and
                            ;; from there to the target location.
                            (incf stack-argument-count)
                            (inst mov rax stack-arg-tn)
                            (inst mov target-tn rax)))))
                  (t
                   (bug "Unknown alien floating point type: ~S" type)))))

        (macrolet
            ((call-wrapper ()
               ;; Technically this fixup should have an optional arg of
               ;;  (- (ASH SYMBOL-VALUE-SLOT WORD-SHIFT) OTHER-POINTER-LOWTAG)
               ;; but as the fixup is hand-crafted anyway, it doesn't matter.
               `(inst call (rip-relative-ea
                      (make-fixup 'callback-wrapper-trampoline
                                  :immobile-symbol))))) ; arbitraryish flavor
        #-sb-thread
        (progn
          ;; arg0 to ENTER-ALIEN-CALLBACK (trampoline index)
          (inst mov rdx (fixnumize index))
          ;; arg1 to ENTER-ALIEN-CALLBACK (pointer to argument vector)
          (inst mov rdi rsp)
          ;; add room on stack for return value
          (inst sub rsp (if (evenp arg-count)
                            (* n-word-bytes 2)
                            n-word-bytes))
          ;; arg2 to ENTER-ALIEN-CALLBACK (pointer to return value)
          (inst mov rsi rsp)

          ;; Make new frame
          (inst push rbp)
          (inst mov  rbp rsp)

          ;; Call
          (call-wrapper)

          ;; Back! Restore frame
          (inst leave))

        #+sb-thread
        (progn
          ;; arg0 to ENTER-ALIEN-CALLBACK (trampoline index)
          (inst mov #-win32 rdi #+win32 rcx (fixnumize index))
          ;; arg1 to ENTER-ALIEN-CALLBACK (pointer to argument vector)
          (inst mov #-win32 rsi #+win32 rdx rsp)
          ;; add room on stack for return value
          (inst sub rsp (if (evenp arg-count)
                            (* n-word-bytes 2)
                            n-word-bytes))
          ;; arg2 to ENTER-ALIEN-CALLBACK (pointer to return value)
          (inst mov #-win32 rdx #+win32 r8 rsp)
          ;; Make new frame
          (inst push rbp)
          (inst mov  rbp rsp)
          #+win32 (inst sub rsp #x20)
          #+win32 (inst and rsp #x-20)
          ;; Call
          (call-wrapper)

          ;; Back! Restore frame
          (inst leave)))

        ;; Result now on top of stack, put it in the right register
        (cond
          ((or (alien-integer-type-p result-type)
               (alien-pointer-type-p result-type)
               (alien-type-= #.(parse-alien-type 'system-area-pointer nil)
                             result-type))
           (inst mov rax [rsp]))
          ((or (alien-single-float-type-p result-type)
               (alien-double-float-type-p result-type))
           (inst movq xmm0 [rsp]))
          ((alien-void-type-p result-type))
          (t
           (error "Unrecognized alien type: ~A" result-type)))

        ;; Pop the arguments and the return value from the stack to get
        ;; the return address at top of stack.

        (inst add rsp (* (+ arg-count
                            ;; Plus the return value and make sure it's aligned
                            (if (evenp arg-count)
                                2
                                1))
                         n-word-bytes))
        ;; Return
        (inst ret))
      (finalize-segment segment)
      ;; Now that the segment is done, convert it to a static
      ;; vector we can point foreign code to.
      (let* ((buffer (sb-assem:segment-buffer segment))
             (result (make-static-vector (length buffer)
                                         :element-type '(unsigned-byte 8)
                                         :initial-contents buffer)))
        ;; This is an ad-hoc substitute for the general fixup logic, due to
        ;; absence of a code component. Even the machine-dependent part is not
        ;; useful since it wants to call CODE-INSTRUCTIONS.
        (let* ((notes (sb-assem::segment-fixup-notes segment))
               (note (car notes)))
          (when note
            (aver (eq (fixup-note-kind note) :rel32))
            ;; +4 is because RIP-relative EA is relative to following instruction
            (let* ((pc (sap+ (vector-sap result) (+ (fixup-note-position note) 4)))
                   (fixup (fixup-note-fixup note))
                   (ea (+ nil-value (ea-disp (static-symbol-value-ea (fixup-name fixup)))))
                   (disp (sap- (int-sap ea) pc)))
              (setf (signed-sap-ref-32  (vector-sap result) (fixup-note-position note))
                    disp))))
        result))))
