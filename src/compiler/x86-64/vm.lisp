;;;; miscellaneous VM definition noise for the x86-64

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;;; register specs

(macrolet ((defreg (name offset size)
             (declare (ignore size))
             `(eval-when (:compile-toplevel :load-toplevel :execute)
                    ;; EVAL-WHEN is necessary because stuff like #.EAX-OFFSET
                    ;; (in the same file) depends on compile-time evaluation
                    ;; of the DEFCONSTANT. -- AL 20010224
                (defconstant ,(symbolicate name "-OFFSET") ,offset)))
           (defregset (name &rest regs)
             ;; FIXME: this would be DEFCONSTANT-EQX were it not
             ;; for all the style-warnings about earmuffs on a constant.
             `(defglobal ,name
                  (list ,@(mapcar (lambda (name)
                                    (symbolicate name "-OFFSET"))
                                  regs))))
           ;; Define general-purpose regs in a more concise way, as we seem
           ;; to (redundantly) want each register's offset for dword and qword
           ;; even though the value of the constant is the same.
           ;; We don't need constants for the byte- or word-sized offsets.
           (define-gprs (want-offsets offsets-list names array)
             `(progn
                (defconstant-eqx ,names ,array #'equalp)
                ;; We need the constants evaluable because of the DEFGLOBAL
                ;; which is needed because of forms such as #.*qword-regs*
                (eval-when (:compile-toplevel :load-toplevel :execute)
                  ,@(when want-offsets
                      (let ((i -1))
                        (map 'list
                             (lambda (x)
                               `(defconstant ,(symbolicate x "-OFFSET") ,(incf i)))
                             array))))
                (defglobal ,offsets-list
                    (remove-if (lambda (x)
                                 (member x `(,r11-offset ; temp reg
                                             ,r13-offset ; thread base
                                             ,rsp-offset
                                             ,rbp-offset)))
                               (loop for i below 16 collect i))))))

  (define-gprs t *qword-regs* +qword-register-names+
    #("RAX" "RCX" "RDX" "RBX" "RSP" "RBP" "RSI" "RDI"
      "R8"  "R9"  "R10" "R11" "R12" "R13" "R14" "R15"))
  (define-gprs t *dword-regs* +dword-register-names+
    #("EAX" "ECX" "EDX"  "EBX"  "ESP"  "EBP"  "ESI"  "EDI"
      "R8D" "R9D" "R10D" "R11D" "R12D" "R13D" "R14D" "R15D"))
  (define-gprs nil *word-regs* +word-register-names+
    #("AX"  "CX"  "DX"   "BX"   "SP"   "BP"   "SI"   "DI"
      "R8W" "R9W" "R10W" "R11W" "R12W" "R13W" "R14W" "R15W"))
  ;; High-byte ("h") registers are not generally used on AMD64,
  ;; since they can't be encoded in an instruction that has a REX-prefix,
  ;; but we can sometimes use them.
  (define-gprs nil *byte-regs* +byte-register-names+
    #("AL"  "CL"  "DL"   "BL"   "SPL"  "BPL"  "SIL"  "DIL"
      "R8B" "R9B" "R10B" "R11B" "R12B" "R13B" "R14B" "R15B"
      "AH" "CH" "DH" "BH"))

  ;; floating point registers
  (defreg float0 0 :float)
  (defreg float1 1 :float)
  (defreg float2 2 :float)
  (defreg float3 3 :float)
  (defreg float4 4 :float)
  (defreg float5 5 :float)
  (defreg float6 6 :float)
  (defreg float7 7 :float)
  (defreg float8 8 :float)
  (defreg float9 9 :float)
  (defreg float10 10 :float)
  (defreg float11 11 :float)
  (defreg float12 12 :float)
  (defreg float13 13 :float)
  (defreg float14 14 :float)
  (defreg float15 15 :float)
  (defregset *float-regs* float0 float1 float2 float3 float4 float5 float6 float7
             float8 float9 float10 float11 float12 float13 float14 float15)

  ;; registers used to pass arguments
  ;;
  ;; the number of arguments/return values passed in registers
  (defconstant  register-arg-count 3)
  ;; names and offsets for registers used to pass arguments
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defparameter *register-arg-names* '(rdx rdi rsi)))
  (defregset    *register-arg-offsets* rdx rdi rsi)
  #-win32
  (defregset    *c-call-register-arg-offsets* rdi rsi rdx rcx r8 r9)
  #+win32
  (defregset    *c-call-register-arg-offsets* rcx rdx r8 r9))

;;;; SB definitions

(!define-storage-bases
(define-storage-base registers :finite :size 16)

(define-storage-base float-registers :finite :size 16)

;;; Start from 2, for the old RBP (aka OCFP) and return address
(define-storage-base stack :unbounded :size 2 :size-increment 1)
(define-storage-base constant :non-packed)
(define-storage-base immediate-constant :non-packed)
(define-storage-base noise :unbounded :size 2)
)

;;;; SC definitions

(eval-when (:compile-toplevel :execute)
(defparameter *storage-class-defs* '(
  ;; non-immediate constants in the constant pool
  (constant constant)

  (fp-single-zero immediate-constant)
  (fp-double-zero immediate-constant)
  (fp-complex-single-zero immediate-constant)
  (fp-complex-double-zero immediate-constant)

  (fp-single-immediate immediate-constant)
  (fp-double-immediate immediate-constant)
  (fp-complex-single-immediate immediate-constant)
  (fp-complex-double-immediate immediate-constant)

  #+sb-simd-pack (int-sse-immediate immediate-constant)
  #+sb-simd-pack (double-sse-immediate immediate-constant)
  #+sb-simd-pack (single-sse-immediate immediate-constant)
  #+sb-simd-pack-256 (int-avx2-immediate immediate-constant)
  #+sb-simd-pack-256 (double-avx2-immediate immediate-constant)
  #+sb-simd-pack-256 (single-avx2-immediate immediate-constant)
  (immediate immediate-constant)

  ;;
  ;; the stacks
  ;;

  ;; the control stack
  (control-stack stack)                 ; may be pointers, scanned by GC

  ;; the non-descriptor stacks
  (signed-stack stack)                  ; (signed-byte 64)
  (unsigned-stack stack)                ; (unsigned-byte 64)
  (character-stack stack)               ; non-descriptor characters.
  (sap-stack stack)                     ; System area pointers.
  (single-stack stack)                  ; single-floats
  (double-stack stack)
  (complex-single-stack stack)  ; complex-single-floats
  (complex-double-stack stack :element-size 2)  ; complex-double-floats
  #+sb-simd-pack
  (int-sse-stack stack :element-size 2)
  #+sb-simd-pack
  (double-sse-stack stack :element-size 2)
  #+sb-simd-pack
  (single-sse-stack stack :element-size 2)
  #+sb-simd-pack-256
  (int-avx2-stack stack :element-size 4)
  #+sb-simd-pack-256
  (double-avx2-stack stack :element-size 4)
  #+sb-simd-pack-256
  (single-avx2-stack stack :element-size 4)

  ;;
  ;; magic SCs
  ;;

  (ignore-me noise)

  ;;
  ;; things that can go in the integer registers
  ;;

  ;; On the X86, we don't have to distinguish between descriptor and
  ;; non-descriptor registers, because of the conservative GC.
  ;; Therefore, we use different scs only to distinguish between
  ;; descriptor and non-descriptor values and to specify size.

  ;; immediate descriptor objects. Don't have to be seen by GC, but nothing
  ;; bad will happen if they are. (fixnums, characters, header values, etc).
  (any-reg registers
           :locations #.*qword-regs*
           :constant-scs (immediate)
           :save-p t
           :alternate-scs (control-stack))

  ;; pointer descriptor objects -- must be seen by GC
  (descriptor-reg registers
                  :locations #.*qword-regs*
                  :constant-scs (constant immediate)
                  :save-p t
                  :alternate-scs (control-stack))

  ;; non-descriptor characters
  (character-reg registers
                 :locations #.*qword-regs*
                 :constant-scs (immediate)
                 :save-p t
                 :alternate-scs (character-stack))

  ;; non-descriptor SAPs (arbitrary pointers into address space)
  (sap-reg registers
           :locations #.*qword-regs*
           :constant-scs (immediate)
           :save-p t
           :alternate-scs (sap-stack))

  ;; non-descriptor (signed or unsigned) numbers
  (signed-reg registers
              :locations #.*qword-regs*
              :constant-scs (immediate)
              :save-p t
              :alternate-scs (signed-stack))
  (unsigned-reg registers
                :locations #.*qword-regs*
                :constant-scs (immediate)
                :save-p t
                :alternate-scs (unsigned-stack))

  ;; non-descriptor SINGLE-FLOATs
  (single-reg float-registers
              :locations #.*float-regs*
              :constant-scs (fp-single-zero fp-single-immediate)
              :save-p t
              :alternate-scs (single-stack))

  ;; non-descriptor DOUBLE-FLOATs
  (double-reg float-registers
              :locations #.*float-regs*
              :constant-scs (fp-double-zero fp-double-immediate)
              :save-p t
              :alternate-scs (double-stack))

  (complex-single-reg float-registers
                      :locations #.*float-regs*
                      :constant-scs (fp-complex-single-zero fp-complex-single-immediate)
                      :save-p t
                      :alternate-scs (complex-single-stack))

  (complex-double-reg float-registers
                      :locations #.*float-regs*
                      :constant-scs (fp-complex-double-zero fp-complex-double-immediate)
                      :save-p t
                      :alternate-scs (complex-double-stack))

  ;; temporary only
  #+sb-simd-pack
  (sse-reg float-registers
           :locations #.*float-regs*)
  ;; regular values
  #+sb-simd-pack
  (int-sse-reg float-registers
               :locations #.*float-regs*
               :constant-scs (int-sse-immediate)
               :save-p t
               :alternate-scs (int-sse-stack))
  #+sb-simd-pack
  (double-sse-reg float-registers
                  :locations #.*float-regs*
                  :constant-scs (double-sse-immediate)
                  :save-p t
                  :alternate-scs (double-sse-stack))
  #+sb-simd-pack
  (single-sse-reg float-registers
                  :locations #.*float-regs*
                  :constant-scs (single-sse-immediate)
                  :save-p t
                  :alternate-scs (single-sse-stack))
  #+avx2
  (avx2-reg float-registers
   :locations #.*float-regs*)
  #+sb-simd-pack-256
  (int-avx2-reg float-registers
               :locations #.*float-regs*
               :constant-scs (int-avx2-immediate)
               :save-p t
               :alternate-scs (int-avx2-stack))
  #+sb-simd-pack-256
  (double-avx2-reg float-registers
                  :locations #.*float-regs*
                  :constant-scs (double-avx2-immediate)
                  :save-p t
                  :alternate-scs (double-avx2-stack))
  #+sb-simd-pack-256
  (single-avx2-reg float-registers
                  :locations #.*float-regs*
                  :constant-scs (single-avx2-immediate)
                  :save-p t
                  :alternate-scs (single-avx2-stack))

  (catch-block stack :element-size catch-block-size)
  (unwind-block stack :element-size unwind-block-size)))

(defparameter *qword-sc-names*
  '(any-reg descriptor-reg sap-reg signed-reg unsigned-reg control-stack
    signed-stack unsigned-stack sap-stack single-stack
    character-reg character-stack constant))
;;; added by jrd. I guess the right thing to do is to treat floats
;;; as a separate size...
;;;
;;; These are used to (at least) determine operand size.
(defparameter *float-sc-names* '(single-reg))
(defparameter *double-sc-names* '(double-reg double-stack))
(defparameter *complex-sc-names* '(complex-single-reg complex-single-stack
                                   complex-double-reg complex-double-stack))
#+sb-simd-pack
(defparameter *oword-sc-names* '(sse-reg int-sse-reg single-sse-reg double-sse-reg
                                 int-sse-stack single-sse-stack double-sse-stack))
#+sb-simd-pack-256
(defparameter *hword-sc-names* '(avx2-reg int-avx2-reg single-avx2-reg double-avx2-reg
                                   int-avx2-stack single-avx2-stack double-avx2-stack))
) ; EVAL-WHEN
(!define-storage-classes
  . #.(mapcar (lambda (class-spec)
                (let ((size
                        (case (car class-spec)
                          #+sb-simd-pack
                          (#.*oword-sc-names*   :oword)
                          #+sb-simd-pack-256
                          (#.*hword-sc-names*   :hword)
                          (#.*qword-sc-names*   :qword)
                          (#.*float-sc-names*   :float)
                          (#.*double-sc-names*  :double)
                          (#.*complex-sc-names* :complex))))
                  (append class-spec (if size (list :operand-size size)))))
              *storage-class-defs*))

;;;; miscellaneous TNs for the various registers

(macrolet ((def-gpr-tns (sc-name name-array &aux (i -1))
             `(progn
                ,@(map 'list
                       (lambda (reg-name)
                         `(define-load-time-global ,(symbolicate reg-name "-TN")
                              (make-random-tn :kind :normal
                                              :sc (sc-or-lose ',sc-name)
                                              :offset ,(incf i))))
                       (symbol-value name-array))))
           (def-fpr-tns (sc-name &rest reg-names)
             (collect ((forms))
               (dolist (reg-name reg-names `(progn ,@(forms)))
                 (let ((tn-name (symbolicate reg-name "-TN"))
                       (offset-name (symbolicate reg-name "-OFFSET")))
                   (forms `(define-load-time-global ,tn-name
                               (make-random-tn :kind :normal
                                               :sc (sc-or-lose ',sc-name)
                                               :offset ,offset-name))))))))
  (def-gpr-tns unsigned-reg +qword-register-names+)
  ;; RIP is not an addressable register, but this global var acts as
  ;; a moniker for it in an effective address so that the EA structure
  ;; does not need to accept a symbol (such as :RIP) for the base reg.
  ;; Because there is no :OFFSET, unanticipated use will be caught.
  (define-load-time-global rip-tn
      (make-random-tn :kind :normal :sc (sc-or-lose 'unsigned-reg)))
  (def-fpr-tns single-reg
      float0 float1 float2 float3 float4 float5 float6 float7
      float8 float9 float10 float11 float12 float13 float14 float15))

;;; Return true if THING is a general-purpose register TN.
(defun gpr-tn-p (thing)
  (and (tn-p thing)
       (eq (sb-name (sc-sb (tn-sc thing))) 'registers)))
;;; Return true if THING is an XMM register TN.
(defun xmm-tn-p (thing)
  (and (tn-p thing)
       (eq (sb-name (sc-sb (tn-sc thing))) 'float-registers)))
;;; Return true if THING is on the stack (in whatever storage class).
(defun stack-tn-p (thing)
  (and (tn-p thing)
       (eq (sb-name (sc-sb (tn-sc thing))) 'stack)))

;; A register that's never used by the code generator, and can therefore
;; be used as an assembly temporary in cases where a VOP :TEMPORARY can't
;; be used.
(define-symbol-macro temp-reg-tn r11-tn)

;;; TNs for registers used to pass arguments
;;; This can't be a DEFCONSTANT-EQX, for a similar reason to above, but worse.
;;; Among the problems, RECEIVE-UNKNOWN-VALUES uses (FIRST *REGISTER-ARG-TNS*)
;;; and so the compiler knows that the object is constant and wants to dump it
;;; as such; it has no name, so it's not even reasonable to expect it to
;;; use the corresponding object in RDX-TN.
(define-load-time-global *register-arg-tns*
  (mapcar (lambda (register-arg-name)
            (symbol-value (symbolicate register-arg-name "-TN")))
          *register-arg-names*))

;; r13 is preferable to r12 because 12 is an alias of 4 in ModRegRM, which
;; implies use of a SIB byte with no index register for fixed displacement.
(define-symbol-macro thread-base-tn r13-tn)

;;; If value can be represented as an immediate constant, then return
;;; the appropriate SC number, otherwise return NIL.
(defun immediate-constant-sc (value)
  (typecase value
    ((or (integer #.sb-xc:most-negative-fixnum #.sb-xc:most-positive-fixnum)
         character)
     immediate-sc-number)
    (symbol ; Symbols in static and immobile space are immediate
     (when (or ;; With #+immobile-symbols, all symbols are in immobile-space.
               ;; And the cross-compiler always uses immobile-space if enabled.
               #+(or immobile-symbols (and immobile-space sb-xc-host)) t

               ;; Otherwise, if #-immobile-symbols, and the symbol was present
               ;; in the initial core image as indicated by the symbol header, then
               ;; it's in immobile-space. There is a way in which the bit can be wrong,
               ;; but it's highly unlikely - the symbol would have to be uninterned from
               ;; the loading SBCL, reallocated in dynamic space and re-interned into its
               ;; initial package. All without breaking anything. Hence, unlikely.
               ;; Also note that if compiling to memory, the symbol's current address
               ;; is used to determine whether it's immediate.
               #+(and (not sb-xc-host) immobile-space (not immobile-symbols))
               (or (logbitp +initial-core-symbol-bit+ (get-header-data value))
                   (and (sb-c::core-object-p sb-c::*compile-object*)
                        (immobile-space-obj-p value)))

               (static-symbol-p value))
       immediate-sc-number))
    #+immobile-space
    (layout
       immediate-sc-number)
    (single-float
       (if (eql value $0f0) fp-single-zero-sc-number fp-single-immediate-sc-number))
    (double-float
       (if (eql value $0d0) fp-double-zero-sc-number fp-double-immediate-sc-number))
    ((complex single-float)
       (if (eql value (complex $0f0 $0f0))
            fp-complex-single-zero-sc-number
            fp-complex-single-immediate-sc-number))
    ((complex double-float)
       (if (eql value (complex $0d0 $0d0))
            fp-complex-double-zero-sc-number
            fp-complex-double-immediate-sc-number))
    #+(and sb-simd-pack (not sb-xc-host))
    ((simd-pack double-float) double-sse-immediate-sc-number)
    #+(and sb-simd-pack (not sb-xc-host))
    ((simd-pack single-float) single-sse-immediate-sc-number)
    #+(and sb-simd-pack (not sb-xc-host))
    (simd-pack int-sse-immediate-sc-number)
    #+(and sb-simd-pack-256 (not sb-xc-host))
    ((simd-pack-256 double-float) double-avx2-immediate-sc-number)
    #+(and sb-simd-pack-256 (not sb-xc-host))
    ((simd-pack-256 single-float) single-avx2-immediate-sc-number)
    #+(and sb-simd-pack-256 (not sb-xc-host))
    (simd-pack-256 int-avx2-immediate-sc-number)))

(defun boxed-immediate-sc-p (sc)
  (eql sc immediate-sc-number))

(defun encode-value-if-immediate (tn &optional (tag t))
  (if (sc-is tn immediate)
      (let ((val (tn-value tn)))
        (etypecase val
          (integer  (if tag (fixnumize val) val))
          (symbol   (if (static-symbol-p val)
                        (+ nil-value (static-symbol-offset val))
                        (make-fixup val :immobile-symbol)))
          #+immobile-space
          (layout
           (make-fixup val :layout))
          (character (if tag
                         (logior (ash (char-code val) n-widetag-bits)
                                 character-widetag)
                         (char-code val)))))
      tn))

;;;; miscellaneous function call parameters

;;; Offsets of special stack frame locations relative to RBP.
;;;
;;; Consider the standard prologue PUSH RBP; MOV RBP, RSP: the return
;;; address is at RBP+8, the old control stack frame pointer is at
;;; RBP, the magic 3rd slot is at RBP-8. Then come the locals from
;;; RBP-16 on.
(defconstant return-pc-save-offset 0)
(defconstant ocfp-save-offset 1)
;;; Let SP be the stack pointer before CALLing, and FP is the frame
;;; pointer after the standard prologue. SP +
;;; FRAME-WORD-OFFSET(SP->FP-OFFSET + I) = FP + FRAME-WORD-OFFSET(I).
(defconstant sp->fp-offset 2)

(declaim (inline frame-word-offset))
(defun frame-word-offset (index)
  (- (1- index)))

(declaim (inline frame-byte-offset))
(defun frame-byte-offset (index)
  (* (frame-word-offset index) n-word-bytes))

;;; This is used by the debugger.
(defconstant single-value-return-byte-offset 3)

;;; This function is called by debug output routines that want a pretty name
;;; for a TN's location. It returns a thing that can be printed with PRINC.
(defun location-print-name (tn)
  (declare (type tn tn))
  (let* ((sc (tn-sc tn))
         (sb (sb-name (sc-sb sc)))
         (offset (tn-offset tn)))
    (ecase sb
      (registers (reg-name (tn-reg tn)))
      (float-registers (format nil "FLOAT~D" offset))
      (stack (format nil "S~D" offset))
      (constant (format nil "Const~D" offset))
      (immediate-constant "Immed")
      (noise (symbol-name (sc-name sc))))))

(defconstant nargs-offset rcx-offset)
(defconstant cfp-offset rbp-offset) ; pfw - needed by stuff in /code

(defun combination-implementation-style (node)
  (declare (type sb-c::combination node))
  (flet ((valid-funtype (args result)
           (sb-c::valid-fun-use node
                                (sb-c::specifier-type
                                 `(function ,args ,result)))))
    (case (sb-c::combination-fun-source-name node)
      (logtest
       (cond
         ((or (valid-funtype '(fixnum fixnum) '*)
              ;; todo: nothing prevents this from testing an unsigned word against
              ;; a signed word, except for the mess of VOPs it would demand
              (valid-funtype '((signed-byte 64) (signed-byte 64)) '*)
              (valid-funtype '((unsigned-byte 64) (unsigned-byte 64)) '*))
          (values :maybe nil))
         (t
          (values :default nil))))
      (logbitp
       (if (or
            (valid-funtype '((mod 64) word) '*)
            (valid-funtype '((mod 64) signed-word) '*))
           (values :transform '(lambda (index integer) (%logbitp index integer)))
           (values :default nil)))
      (t
       (values :default nil)))))

(defparameter *register-names* +qword-register-names+)
