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

(in-package "SB!VM")

;; The MOVE-ARG vop is going to store args on the stack for
;; call-out. These tn's will be used for that. move-arg is normally
;; used for things going down the stack but C wants to have args
;; indexed in the positive direction.

(defun my-make-wired-tn (prim-type-name sc-name offset)
  (make-wired-tn (primitive-type-or-lose prim-type-name)
                 (sc-number-or-lose sc-name)
                 offset))

(defstruct (arg-state (:copier nil))
  (stack-frame-size 0))

(define-alien-type-method (integer :arg-tn) (type state)
  (let ((stack-frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (1+ stack-frame-size))
    (multiple-value-bind (ptype stack-sc)
        (if (alien-integer-type-signed type)
            (values 'signed-byte-32 'signed-stack)
            (values 'unsigned-byte-32 'unsigned-stack))
      (my-make-wired-tn ptype stack-sc stack-frame-size))))

(define-alien-type-method (system-area-pointer :arg-tn) (type state)
  (declare (ignore type))
  (let ((stack-frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (1+ stack-frame-size))
    (my-make-wired-tn 'system-area-pointer
                      'sap-stack
                      stack-frame-size)))

#!+long-float
(define-alien-type-method (long-float :arg-tn) (type state)
  (declare (ignore type))
  (let ((stack-frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (+ stack-frame-size 3))
    (my-make-wired-tn 'long-float 'long-stack stack-frame-size)))

(define-alien-type-method (double-float :arg-tn) (type state)
  (declare (ignore type))
  (let ((stack-frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (+ stack-frame-size 2))
    (my-make-wired-tn 'double-float 'double-stack stack-frame-size)))

(define-alien-type-method (single-float :arg-tn) (type state)
  (declare (ignore type))
  (let ((stack-frame-size (arg-state-stack-frame-size state)))
    (setf (arg-state-stack-frame-size state) (1+ stack-frame-size))
    (my-make-wired-tn 'single-float 'single-stack stack-frame-size)))

(defstruct (result-state (:copier nil))
  (num-results 0))

(defun result-reg-offset (slot)
  (ecase slot
    (0 eax-offset)
    (1 edx-offset)))

(define-alien-type-method (integer :result-tn) (type state)
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (multiple-value-bind (ptype reg-sc)
        (if (alien-integer-type-signed type)
            (values 'signed-byte-32 'signed-reg)
            (values 'unsigned-byte-32 'unsigned-reg))
      (my-make-wired-tn ptype reg-sc (result-reg-offset num-results)))))

(define-alien-type-method (integer :naturalize-gen) (type alien)
  (if (<= (alien-type-bits type) 16)
      (if (alien-integer-type-signed type)
          `(sign-extend ,alien ,(alien-type-bits type))
          `(logand ,alien ,(1- (ash 1 (alien-type-bits type)))))
      alien))

(define-alien-type-method (system-area-pointer :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (my-make-wired-tn 'system-area-pointer 'sap-reg
                      (result-reg-offset num-results))))

#!+long-float
(define-alien-type-method (long-float :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (my-make-wired-tn 'long-float 'long-reg (* num-results 2))))

(define-alien-type-method (double-float :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (my-make-wired-tn 'double-float 'double-reg (* num-results 2))))

(define-alien-type-method (single-float :result-tn) (type state)
  (declare (ignore type))
  (let ((num-results (result-state-num-results state)))
    (setf (result-state-num-results state) (1+ num-results))
    (my-make-wired-tn 'single-float 'single-reg (* num-results 2))))

(define-alien-type-method (values :result-tn) (type state)
  (let ((values (alien-values-type-values type)))
    (when (> (length values) 2)
      (error "Too many result values from c-call."))
    (mapcar (lambda (type)
              (invoke-alien-type-method :result-tn type state))
            values)))

(!def-vm-support-routine make-call-out-tns (type)
  (let ((arg-state (make-arg-state)))
    (collect ((arg-tns))
      (dolist (arg-type (alien-fun-type-arg-types type))
        (arg-tns (invoke-alien-type-method :arg-tn arg-type arg-state)))
      (values (my-make-wired-tn 'positive-fixnum 'any-reg esp-offset)
              (* (arg-state-stack-frame-size arg-state) n-word-bytes)
              (arg-tns)
              (invoke-alien-type-method :result-tn
                                        (alien-fun-type-result-type type)
                                        (make-result-state))))))


(deftransform %alien-funcall ((function type &rest args) * * :node node)
  (aver (sb!c::constant-lvar-p type))
  (let* ((type (sb!c::lvar-value type))
         (env (sb!c::node-lexenv node))
         (arg-types (alien-fun-type-arg-types type))
         (result-type (alien-fun-type-result-type type)))
    (aver (= (length arg-types) (length args)))
    (if (or (some #'(lambda (type)
                      (and (alien-integer-type-p type)
                           (> (sb!alien::alien-integer-type-bits type) 32)))
                  arg-types)
            (and (alien-integer-type-p result-type)
                 (> (sb!alien::alien-integer-type-bits result-type) 32)))
        (collect ((new-args) (lambda-vars) (new-arg-types))
          (dolist (type arg-types)
            (let ((arg (gensym)))
              (lambda-vars arg)
              (cond ((and (alien-integer-type-p type)
                          (> (sb!alien::alien-integer-type-bits type) 32))
                     (new-args `(logand ,arg #xffffffff))
                     (new-args `(ash ,arg -32))
                     (new-arg-types (parse-alien-type '(unsigned 32) env))
                     (if (alien-integer-type-signed type)
                         (new-arg-types (parse-alien-type '(signed 32) env))
                         (new-arg-types (parse-alien-type '(unsigned 32) env))))
                    (t
                     (new-args arg)
                     (new-arg-types type)))))
          (cond ((and (alien-integer-type-p result-type)
                      (> (sb!alien::alien-integer-type-bits result-type) 32))
                 (let ((new-result-type
                        (let ((sb!alien::*values-type-okay* t))
                          (parse-alien-type
                           (if (alien-integer-type-signed result-type)
                               '(values (unsigned 32) (signed 32))
                               '(values (unsigned 32) (unsigned 32)))
                           env))))
                   `(lambda (function type ,@(lambda-vars))
                      (declare (ignore type))
                      (multiple-value-bind (low high)
                          (%alien-funcall function
                                          ',(make-alien-fun-type
                                             :arg-types (new-arg-types)
                                             :result-type new-result-type)
                                          ,@(new-args))
                        (logior low (ash high 32))))))
                (t
                 `(lambda (function type ,@(lambda-vars))
                    (declare (ignore type))
                    (%alien-funcall function
                                    ',(make-alien-fun-type
                                       :arg-types (new-arg-types)
                                       :result-type result-type)
                                    ,@(new-args))))))
        (sb!c::give-up-ir1-transform))))

;;; The ABI is vague about how signed sub-word integer return values
;;; are handled, but since gcc versions >=4.3 no longer do sign
;;; extension in the callee, we need to do it in the caller.  FIXME:
;;; If the value to be extended is known to already be of the target
;;; type at compile time, we can (and should) elide the extension.
(defknown sign-extend ((signed-byte 32) t) fixnum
    (foldable flushable movable))

(define-vop (sign-extend)
  (:translate sign-extend)
  (:policy :fast-safe)
  ;; Need to wire this to EAX since in x86 some dword registers don't
  ;; have a matching word or byte register.
  (:args (val :scs (signed-reg) :target eax))
  (:temporary (:sc signed-reg :offset eax-offset :from :eval :to :result) eax)
  (:arg-types signed-num (:constant fixnum))
  (:info size)
  (:results (res :scs (signed-reg)))
  (:result-types fixnum)
  (:ignore eax)
  (:generator 1
   (inst movsx res
         (make-random-tn :kind :normal
                         :sc (sc-or-lose (ecase size
                                           (8 'byte-reg)
                                           (16 'word-reg)))
                         :offset (tn-offset val)))))

#-sb-xc-host
(defun sign-extend (x size)
  (declare (type (signed-byte 32) x))
  (ecase size
    (8 (sign-extend x size))
    (16 (sign-extend x size))))

#+sb-xc-host
(defun sign-extend (x size)
  (if (logbitp (1- size) x)
      (dpb x (byte size 0) -1)
      x))

(define-vop (foreign-symbol-sap)
  (:translate foreign-symbol-sap)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
   (inst lea res (make-fixup foreign-symbol :foreign))))

#!+linkage-table
(define-vop (foreign-symbol-dataref-sap)
  (:translate foreign-symbol-dataref-sap)
  (:policy :fast-safe)
  (:args)
  (:arg-types (:constant simple-string))
  (:info foreign-symbol)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
   (inst mov res (make-fixup foreign-symbol :foreign-dataref))))

(define-vop (call-out)
  (:args (function :scs (sap-reg))
         (args :more t))
  (:results (results :more t))
  (:temporary (:sc unsigned-reg :offset eax-offset
                   :from :eval :to :result) eax)
  (:temporary (:sc unsigned-reg :offset ecx-offset
                   :from :eval :to :result) ecx)
  (:temporary (:sc unsigned-reg :offset edx-offset
                   :from :eval :to :result) edx)
  (:node-var node)
  (:vop-var vop)
  (:save-p t)
  (:ignore args ecx edx)
  (:generator 0
    ;; FIXME & OAOOM: This is brittle and error-prone to maintain two
    ;; instances of the same logic, on in arch-assem.S, and one in
    ;; c-call.lisp. If you modify this, modify that one too...
    (cond ((policy node (> space speed))
           (move eax function)
           (inst call (make-fixup "call_into_c" :foreign)))
          (t
           ;; Setup the NPX for C; all the FP registers need to be
           ;; empty; pop them all.
           (dotimes (i 8)
             (inst fstp fr0-tn))

           ;; Clear out DF: Darwin, Windows, and Solaris at least require
           ;; this, and it should not hurt others either.
           (inst cld)

           (inst call function)
           ;; To give the debugger a clue. FIXME: not really internal-error?
           (note-this-location vop :internal-error)

           ;; Restore the NPX for lisp; ensure no regs are empty
           (dotimes (i 7)
             (inst fldz))

           (if (and results
                    (location= (tn-ref-tn results) fr0-tn))
               ;; The return result is in fr0.
               (inst fxch fr7-tn)       ; move the result back to fr0
               (inst fldz))             ; insure no regs are empty
           ))))

;;; While SBCL uses the FPU in 53-bit mode, most C libraries assume that
;;; the FPU is in 64-bit mode. So we change the FPU mode to 64-bit with
;;; the SET-FPU-WORD-FOR-C VOP before calling out to C and set it back
;;; to 53-bit mode after coming back using the SET-FPU-WORD-FOR-LISP VOP.
(define-vop (set-fpu-word-for-c)
  (:node-var node)
  (:generator 0
    (when (policy node (= sb!c::float-accuracy 3))
      (inst sub esp-tn 4)
      (inst fnstcw (make-ea :word :base esp-tn))
      (inst wait)
      (inst or (make-ea :word :base esp-tn) #x300)
      (inst fldcw (make-ea :word :base esp-tn))
      (inst wait))))

(define-vop (set-fpu-word-for-lisp)
  (:node-var node)
  (:generator 0
    (when (policy node (= sb!c::float-accuracy 3))
      (inst fnstcw (make-ea :word :base esp-tn))
      (inst wait)
      (inst and (make-ea :word :base esp-tn) #xfeff)
      (inst fldcw (make-ea :word :base esp-tn))
      (inst wait)
      (inst add esp-tn 4))))

(define-vop (alloc-number-stack-space)
  (:info amount)
  (:results (result :scs (sap-reg any-reg)))
  (:result-types system-area-pointer)
  (:generator 0
    (aver (location= result esp-tn))
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 3) 3)))
        (inst sub esp-tn delta)))
    (align-stack-pointer esp-tn)
    (move result esp-tn)))

(define-vop (alloc-alien-stack-space)
  (:info amount)
  #!+sb-thread (:temporary (:sc unsigned-reg) temp)
  (:results (result :scs (sap-reg any-reg)))
  (:result-types system-area-pointer)
  #!+sb-thread
  (:generator 0
    (aver (not (location= result esp-tn)))
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 3) 3)))
        (inst mov temp
              (make-ea-for-symbol-tls-index *alien-stack*))
        (inst sub (make-ea :dword :base temp) delta :fs)))
    (load-tl-symbol-value result *alien-stack*))
  #!-sb-thread
  (:generator 0
    (aver (not (location= result esp-tn)))
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 3) 3)))
        (inst sub (make-ea-for-symbol-value *alien-stack*)
              delta)))
    (load-symbol-value result *alien-stack*)))

(define-vop (dealloc-alien-stack-space)
  (:info amount)
  #!+sb-thread (:temporary (:sc unsigned-reg) temp)
  #!+sb-thread
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 3) 3)))
        (inst mov temp
              (make-ea-for-symbol-tls-index *alien-stack*))
        (inst add (make-ea :dword :base temp) delta :fs))))
  #!-sb-thread
  (:generator 0
    (unless (zerop amount)
      (let ((delta (logandc2 (+ amount 3) 3)))
        (inst add (make-ea-for-symbol-value *alien-stack*)
              delta)))))

;;; not strictly part of the c-call convention, but needed for the
;;; WITH-PINNED-OBJECTS macro used for "locking down" lisp objects so
;;; that GC won't move them while foreign functions go to work.
(define-vop (touch-object)
  (:translate touch-object)
  (:args (object))
  (:ignore object)
  (:policy :fast-safe)
  (:arg-types t)
  (:generator 0))

#-sb-xc-host
(defun alien-callback-accessor-form (type sp offset)
  `(deref (sap-alien (sap+ ,sp ,offset) (* ,type))))

#-sb-xc-host
(defun alien-callback-assembler-wrapper (index return-type arg-types)
  "Cons up a piece of code which calls call-callback with INDEX and a
pointer to the arguments."
  (declare (ignore arg-types))
  (let* ((segment (make-segment))
         (eax eax-tn)
         (edx edx-tn)
         (ebp ebp-tn)
         (esp esp-tn)
         ([ebp-8] (make-ea :dword :base ebp :disp -8))
         ([ebp-4] (make-ea :dword :base ebp :disp -4)))
    (assemble (segment)
              (inst push ebp)                       ; save old frame pointer
              (inst mov  ebp esp)                   ; establish new frame
              (inst mov  eax esp)                   ;
              (inst sub  eax 8)                     ; place for result
              (inst push eax)                       ; arg2
              (inst add  eax 16)                    ; arguments
              (inst push eax)                       ; arg1
              (inst push (ash index 2))             ; arg0

              ;; Indirect the access to ENTER-ALIEN-CALLBACK through
              ;; the symbol-value slot of SB-ALIEN::*ENTER-ALIEN-CALLBACK*
              ;; to ensure it'll work even if the GC moves ENTER-ALIEN-CALLBACK.
              ;; Skip any SB-THREAD TLS magic, since we don't expecte anyone
              ;; to rebind the variable. -- JES, 2006-01-01
              (load-symbol-value eax sb!alien::*enter-alien-callback*)
              (inst push eax) ; function
              (inst mov  eax (foreign-symbol-address "funcall3"))
              (inst call eax)
              ;; now put the result into the right register
              (cond
                ((and (alien-integer-type-p return-type)
                      (eql (alien-type-bits return-type) 64))
                 (inst mov eax [ebp-8])
                 (inst mov edx [ebp-4]))
                ((or (alien-integer-type-p return-type)
                     (alien-pointer-type-p return-type)
                     (alien-type-= #.(parse-alien-type 'system-area-pointer nil)
                                   return-type))
                 (inst mov eax [ebp-8]))
                ((alien-single-float-type-p return-type)
                 (inst fld  [ebp-8]))
                ((alien-double-float-type-p return-type)
                 (inst fldd [ebp-8]))
                ((alien-void-type-p return-type))
                (t
                 (error "unrecognized alien type: ~A" return-type)))
              (inst mov esp ebp)                   ; discard frame
              (inst pop ebp)                       ; restore frame pointer
              (inst ret))
    (finalize-segment segment)
    ;; Now that the segment is done, convert it to a static
    ;; vector we can point foreign code to.
    (let ((buffer (sb!assem::segment-buffer segment)))
      (make-static-vector (length buffer)
                          :element-type '(unsigned-byte 8)
                          :initial-contents buffer))))
