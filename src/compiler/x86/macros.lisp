;;;; a bunch of handy macros for the x86

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;; We can load/store into fp registers through the top of stack
;;; %st(0) (fr0 here). Loads imply a push to an empty register which
;;; then changes all the reg numbers. These macros help manage that.

;;; Use this when we don't have to load anything. It preserves old tos
;;; value, but probably destroys tn with operation.
(defmacro with-tn@fp-top((tn) &body body)
  `(progn
    (unless (zerop (tn-offset ,tn))
      (inst fxch ,tn))
    ,@body
    (unless (zerop (tn-offset ,tn))
      (inst fxch ,tn))))

;;; Use this to prepare for load of new value from memory. This
;;; changes the register numbering so the next instruction had better
;;; be a FP load from memory; a register load from another register
;;; will probably be loading the wrong register!
(defmacro with-empty-tn@fp-top((tn) &body body)
  `(progn
     (inst fstp ,tn)
     ,@body
     (unless (zerop (tn-offset ,tn))
       (inst fxch ,tn))))                ; save into new dest and restore st(0)

;;;; instruction-like macros

(defmacro move (dst src)
  "Move SRC into DST unless they are location=."
  (once-only ((n-dst dst)
              (n-src src))
    `(unless (location= ,n-dst ,n-src)
       (inst mov ,n-dst ,n-src))))

(defmacro align-stack-pointer (tn)
  ;; 16 byte alignment.
  `(inst and ,tn -16))

(defmacro object-slot-ea (ptr slot lowtag &optional (size :dword))
  `(make-ea ,size :base ,ptr :disp (- (* ,slot n-word-bytes) ,lowtag)))

(defmacro loadw (value ptr &optional (slot 0) (lowtag 0))
  `(inst mov ,value (object-slot-ea ,ptr ,slot ,lowtag)))

(defmacro storew (value ptr &optional (slot 0) (lowtag 0))
  (once-only ((value value))
    `(inst mov (object-slot-ea ,ptr ,slot ,lowtag) ,value)))

(defmacro pushw (ptr &optional (slot 0) (lowtag 0))
  `(inst push (object-slot-ea ,ptr ,slot ,lowtag)))

(defmacro popw (ptr &optional (slot 0) (lowtag 0))
  `(inst pop (object-slot-ea ,ptr ,slot ,lowtag)))

(defmacro make-ea-for-vector-data (object &key (size :dword) (offset 0)
                                   index (scale (ash (width-bits size) -3)))
  `(make-ea ,size :base ,object :index ,index :scale ,scale
            :disp (- (+ (* vector-data-offset n-word-bytes)
                        (* ,offset ,scale))
                     other-pointer-lowtag)))

;;;; macros to generate useful values

(defmacro load-symbol (reg symbol)
  `(inst mov ,reg (+ nil-value (static-symbol-offset ,symbol))))

(defmacro make-ea-for-symbol-value (symbol &optional (width :dword))
  (declare (type symbol symbol))
  `(make-ea ,width
    :disp (+ nil-value
           (static-symbol-offset ',symbol)
           (ash symbol-value-slot word-shift)
           (- other-pointer-lowtag))))

(defmacro load-symbol-value (reg symbol)
  `(inst mov ,reg (make-ea-for-symbol-value ,symbol)))

(defmacro store-symbol-value (reg symbol)
  `(inst mov (make-ea-for-symbol-value ,symbol) ,reg))

#+sb-thread
(progn
(defmacro tls-index-of (symbol)
  `(object-slot-ea ,symbol ,sb-vm:symbol-tls-index-slot ,other-pointer-lowtag))
(defmacro make-ea-for-symbol-tls-index (symbol)
  (declare (type symbol symbol))
  `(make-ea :dword
    :disp (+ nil-value
           (static-symbol-offset ',symbol)
           (ash symbol-tls-index-slot word-shift)
           (- other-pointer-lowtag))))

(defmacro load-tl-symbol-value (reg symbol)
  `(with-tls-ea (EA :base ,reg
                    :disp-type :index
                    :disp (make-ea-for-symbol-tls-index ,symbol))
     (inst mov ,reg (make-ea :dword :base ,reg) :maybe-fs)))

(defmacro store-tl-symbol-value (reg symbol temp)
  `(with-tls-ea (EA :base ,temp
                    :disp-type :index
                    :disp (make-ea-for-symbol-tls-index ,symbol))
     (inst mov EA ,reg :maybe-fs))))

#-sb-thread
(progn
(defmacro load-tl-symbol-value (reg symbol) `(load-symbol-value ,reg ,symbol))
(defmacro store-tl-symbol-value (reg symbol temp)
  (declare (ignore temp))
  `(store-symbol-value ,reg ,symbol)))

(defmacro load-binding-stack-pointer (reg)
  #+sb-thread
  `(with-tls-ea (EA :base ,reg
                    :disp-type :constant
                    :disp (* 4 thread-binding-stack-pointer-slot))
     (inst mov ,reg EA :maybe-fs))
  #-sb-thread
  `(load-symbol-value ,reg *binding-stack-pointer*))

(defmacro store-binding-stack-pointer (reg)
  #+sb-thread
  `(progn
     #+win32
     (progn
       (inst push eax-tn)
       (inst push ,reg)
       (with-tls-ea (EA :base eax-tn
                        :disp-type :constant
                        :disp (* 4 thread-binding-stack-pointer-slot))
         (inst pop EA))
       (inst pop eax-tn))
     #-win32
     (with-tls-ea (EA :disp-type :constant
                      :disp (* 4 thread-binding-stack-pointer-slot))
       (inst mov EA ,reg :maybe-fs)))
  #-sb-thread
  `(store-symbol-value ,reg *binding-stack-pointer*))

(defmacro load-type (target source &optional (offset 0))
  "Loads the type bits of a pointer into target independent of
   byte-ordering issues."
  `(inst mov ,target (make-ea :byte :base ,source :disp ,offset)))

;;;; error code
(defun emit-error-break (vop kind code values)
  (assemble ()
    (inst break)
    ;; The return PC points here; note the location for the debugger.
    (when vop
      (note-this-location vop :internal-error))
    (emit-internal-error kind code values)))

(defun generate-error-code (vop error-code &rest values)
  "Generate-Error-Code Error-code Value*
  Emit code for an error with the specified Error-Code and context Values."
  (assemble (:elsewhere)
    (let ((start-lab (gen-label)))
      (emit-label start-lab)
      (emit-error-break vop error-trap (error-number-or-lose error-code) values)
      start-lab)))


;;;; PSEUDO-ATOMIC

;;; This is used to wrap operations which leave untagged memory lying
;;; around.  It's an operation which the AOP weenies would describe as
;;; having "cross-cutting concerns", meaning it appears all over the
;;; place and there's no logical single place to attach documentation.
;;; grep (mostly in src/runtime) is your friend

;;; Unsafely clear pa flags so that the image can properly lose in a
;;; pa section.
#+sb-thread
(defmacro %clear-pseudo-atomic ()
  #+win32
  `(progn)
  #-win32
  '(inst mov (make-ea :dword :disp (* 4 thread-pseudo-atomic-bits-slot)) 0 :fs))

#+sb-safepoint
(defun emit-safepoint ()
  (inst test eax-tn (make-ea :dword :disp
                             (- static-space-start gc-safepoint-trap-offset))))

(defmacro pseudo-atomic ((&key elide-if) &rest forms)
  #+sb-safepoint
  `(progn ,@forms (unless ,elide-if (emit-safepoint)))
  #-sb-safepoint
  (with-unique-names (label pa-bits-ea)
    `(let ((,label (gen-label))
           (,pa-bits-ea
            #+sb-thread (make-ea :dword :disp (* 4 thread-pseudo-atomic-bits-slot))
            #-sb-thread (make-ea-for-symbol-value *pseudo-atomic-bits* :dword)))
       (unless ,elide-if
         (inst mov ,pa-bits-ea ebp-tn #+sb-thread :fs))
       ,@forms
       (unless ,elide-if
         (inst xor ,pa-bits-ea ebp-tn #+sb-thread :fs)
         (inst jmp :z ,label)
         ;; if PAI was set, interrupts were disabled at the same time
         ;; using the process signal mask.
         (inst break pending-interrupt-trap)
         (emit-label ,label)))))

;;;; indexed references

(defmacro define-full-compare-and-swap
    (name type offset lowtag scs el-type &optional translate)
  `(progn
     (define-vop (,name)
         ,@(when translate `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg) :to :eval)
              (index :scs (any-reg immediate unsigned-reg) :to :result)
              (old-value :scs ,scs :target eax)
              (new-value :scs ,scs))
       (:arg-types ,type tagged-num ,el-type ,el-type)
       (:temporary (:sc descriptor-reg :offset eax-offset
                        :from (:argument 2) :to :result :target value)  eax)
       (:results (value :scs ,scs))
       (:result-types ,el-type)
       (:generator 5
         (move eax old-value)
         (let ((ea (sc-case index
                     (immediate
                      (make-ea :dword :base object
                               :disp (- (* (+ ,offset (tn-value index))
                                           n-word-bytes)
                                        ,lowtag)))
                     (unsigned-reg
                      (make-ea :dword :base object :index index :scale 4
                               :disp (- (* ,offset n-word-bytes)
                                        ,lowtag)))
                     (t
                      (make-ea :dword :base object :index index
                               :disp (- (* ,offset n-word-bytes)
                                        ,lowtag))))))
           (inst cmpxchg ea new-value :lock))
         (move value eax)))))

(defmacro define-full-reffer (name type offset lowtag scs el-type &optional translate)
  `(progn
     (define-vop (,name)
       ,@(when translate
           `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
              (index :scs (any-reg immediate unsigned-reg)))
       (:arg-types ,type tagged-num)
       (:results (value :scs ,scs))
       (:result-types ,el-type)
       (:generator 3                    ; pw was 5
         (sc-case index
           (immediate
            (inst mov value (make-ea :dword :base object
                                     :disp (- (* (+ ,offset (tn-value index))
                                                 n-word-bytes)
                                              ,lowtag))))
           (unsigned-reg
            (inst mov value (make-ea :dword :base object :index index :scale 4
                                     :disp (- (* ,offset n-word-bytes)
                                              ,lowtag))))
           (t
            (inst mov value (make-ea :dword :base object :index index
                                     :disp (- (* ,offset n-word-bytes)
                                              ,lowtag)))))))))

(defmacro define-full-reffer+addend (name type offset lowtag scs el-type &optional translate)
  `(progn
     (define-vop (,name)
       ,@(when translate
           `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
              (index :scs (any-reg immediate unsigned-reg)))
       (:arg-types ,type tagged-num
                   (:constant (constant-displacement ,lowtag n-word-bytes ,offset)))
       (:info addend)
       (:results (value :scs ,scs))
       (:result-types ,el-type)
       (:generator 3                    ; pw was 5
         (sc-case index
           (immediate
            (inst mov value (make-ea :dword :base object
                                     :disp (- (* (+ ,offset
                                                    (tn-value index)
                                                    addend)
                                                 n-word-bytes)
                                              ,lowtag))))
           (unsigned-reg
            (inst mov value (make-ea :dword :base object :index index :scale 4
                                     :disp (- (* (+ ,offset addend)
                                                 n-word-bytes)
                                              ,lowtag))))
           (t
            (inst mov value (make-ea :dword :base object :index index
                                     :disp (- (* (+ ,offset addend)
                                                 n-word-bytes)
                                              ,lowtag)))))))))

(defmacro define-full-setter (name type offset lowtag scs el-type &optional translate)
    `(define-vop (,name)
       ,@(when translate
           `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
              (index :scs (any-reg immediate))
              (value :scs ,scs))
       (:arg-types ,type tagged-num ,el-type)
       (:generator 4                    ; was 5
         (sc-case index
           (immediate
            (inst mov (make-ea :dword :base object
                               :disp (- (* (+ ,offset (tn-value index))
                                           n-word-bytes)
                                        ,lowtag))
                  value))
           (t
            (inst mov (make-ea :dword :base object :index index
                               :disp (- (* ,offset n-word-bytes) ,lowtag))
                  value))))))

;;; Helper to hide the fact that thread access on Windows needs one more
;;; instruction, needs the FS prefix in that instruction _instead_ of
;;; the actual load/store, and partially hide the resulting need for a
;;; temporary TN when the non-windows might have have dereferenced an EA
;;; without a TN as a base.

(defmacro with-tls-ea ((ea-var &key base
                                    base-already-live-p
                                    (disp-type :constant)
                                    (disp 0))
                       &body body)
  "Execute BODY with various magic.  BODY is expected to emit instructions.

   In the body, EA-VAR will be an alias for an EA which BODY can use to
   perform a thread-local load or store.

   Within the body, :MAYBE-FS will be replaced with :FS or NIL,
   depending on the target, and needs to be included in any instruction
   performing an access through the EA.

   DISP-TYPE must be :INDEX, or :CONSTANT, and DISP must be an EA/TN,
   or an expression returning an integer, respectively.

   BASE must be a temporary TN, except in the following situation: BASE
   will be unused when DISP-TYPE is constant, BASE-ALREADY-LIVE-P is
   true, _and_ we're on POSIX.  This is an intentional optimization, and
   the caller needs to take care to ignore the TN in this case, or can
   omit this parameter.

   BASE-ALREADY-LIVE-P means that at run-time, the BASE register already
   holds an offset that we should add to instead of overwriting it.
   The value of the BASE register is undefined following the macro invocation."
  (check-type base-already-live-p boolean)
  (check-type disp-type (member :index :constant))
  #-(and win32 sb-thread)
  (let ((body (subst :fs :maybe-fs body)))
    (ecase disp-type
      (:constant
       `(progn
          ,@(subst (if base-already-live-p
                       ;; use BASE and DISP
                       `(make-ea :dword :base ,base :disp ,disp)
                       ;; BASE not live and not needed, just use DISP
                       `(make-ea :dword :disp ,disp))
                   ea-var
                   body)))
      (:index
       ;; need to use BASE in any case; and DISP is an EA
       `(progn
          (inst ,(if base-already-live-p 'add 'mov) ,base ,disp)
          ,@(subst `(make-ea :dword :base ,base)
                   ea-var
                   body)))))
  #+(and win32 sb-thread)
  ;; goes through a temporary register to add the thread address into it
  (multiple-value-bind (constant-disp ea-disp)
      (ecase disp-type
        (:constant (values disp nil))
        (:index    (values 0 disp)))
    `(progn
       ,@(when ea-disp
           `((inst ,(if base-already-live-p 'add 'mov) ,base ,ea-disp)))
       (inst ,(if (or base-already-live-p ea-disp) 'add 'mov)
             ,base
             (make-ea :dword :disp +win32-tib-arbitrary-field-offset+)
             :fs)
       ,@(subst `(make-ea :dword :base ,base :disp ,constant-disp)
                ea-var
                (subst nil :maybe-fs body)))))

;;; Emit the most compact form of the test immediate instruction,
;;; using an 8 bit test when the immediate is only 8 bits and the
;;; value is one of the four low registers (eax, ebx, ecx, edx) or the
;;; control stack.
(defun emit-optimized-test-inst (x y)
  (typecase y
    ((unsigned-byte 7)
     (let ((offset (tn-offset x)))
       (cond ((and (sc-is x any-reg descriptor-reg)
                   (or (= offset eax-offset) (= offset ebx-offset)
                       (= offset ecx-offset) (= offset edx-offset)))
              (inst test (make-random-tn (sc-or-lose 'byte-reg) offset)
                    y))
             ((sc-is x control-stack)
              (inst test (make-ea :byte :base ebp-tn
                                  :disp (frame-byte-offset offset))
                    y))
             (t
              (inst test x y)))))
    (t
     (inst test x y))))
