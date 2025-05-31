;;;; miscellaneous VM definition noise for the x86

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

(defconstant-eqx +byte-register-names+
    #("AL" "AH" "CL" "CH" "DL" "DH" "BL" "BH")
  #'equalp)
(defconstant-eqx +word-register-names+
    #("AX" NIL "CX" NIL "DX" NIL "BX" NIL "SP" NIL "BP" NIL "SI" NIL "DI" NIL)
  #'equalp)
(defconstant-eqx +dword-register-names+
    #("EAX" NIL "ECX" NIL "EDX" NIL "EBX" NIL "ESP" NIL "EBP" NIL "ESI" NIL "EDI" NIL)
  #'equalp)

(macrolet ((defreg (name offset size)
             (declare (ignore size))
             `(eval-when (:compile-toplevel :load-toplevel :execute)
                    ;; EVAL-WHEN is necessary because stuff like #.EAX-OFFSET
                    ;; (in the same file) depends on compile-time evaluation
                    ;; of the DEFCONSTANT. -- AL 20010224
                (defconstant ,(symbolicate name "-OFFSET") ,offset))))

  ;; byte registers
  ;;
  ;; Note: the encoding here is different than that used by the chip.
  ;; We use this encoding so that the compiler thinks that AX (and
  ;; EAX) overlap AL and AH instead of AL and CL.
  (defreg al 0 :byte)
  (defreg ah 1 :byte)
  (defreg cl 2 :byte)
  (defreg ch 3 :byte)
  (defreg dl 4 :byte)
  (defreg dh 5 :byte)
  (defreg bl 6 :byte)
  (defreg bh 7 :byte)
  (defregset *byte-regs* al ah cl ch dl dh bl bh)

  ;; word registers
  (defreg ax 0 :word)
  (defreg cx 2 :word)
  (defreg dx 4 :word)
  (defreg bx 6 :word)
  (defreg sp 8 :word)
  (defreg bp 10 :word)
  (defreg si 12 :word)
  (defreg di 14 :word)
  (defregset *word-regs* ax cx dx bx si di)

  ;; double word registers
  (defreg eax 0 :dword)
  (defreg ecx 2 :dword)
  (defreg edx 4 :dword)
  (defreg ebx 6 :dword)
  (defreg esp 8 :dword)
  (defreg ebp 10 :dword)
  (defreg esi 12 :dword)
  (defreg edi 14 :dword)
  (defregset *dword-regs* eax ecx edx ebx esi edi)

  ;; floating point registers
  (defreg fr0 0 :float)
  (defreg fr1 1 :float)
  (defreg fr2 2 :float)
  (defreg fr3 3 :float)
  (defreg fr4 4 :float)
  (defreg fr5 5 :float)
  (defreg fr6 6 :float)
  (defreg fr7 7 :float)
  (defregset *float-regs* fr0 fr1 fr2 fr3 fr4 fr5 fr6 fr7)

  ;; registers used to pass arguments
  ;;
  ;; the number of arguments/return values passed in registers
  (defconstant  register-arg-count 3)
  ;; names and offsets for registers used to pass arguments
  (defconstant-eqx register-arg-names '(edx edi esi) #'equal)
  (defregset    *register-arg-offsets* edx edi esi))

;;;; SB definitions

(!define-storage-bases
;;; Despite the fact that there are only 8 different registers, we consider
;;; them 16 in order to describe the overlap of byte registers. The only
;;; thing we need to represent is what registers overlap. Therefore, we
;;; consider bytes to take one unit, and words or dwords to take two. We
;;; don't need to tell the difference between words and dwords, because
;;; you can't put two words in a dword register.
(define-storage-base registers :finite :size 16)

;;; jrd changed this from size 1 to size 8. It doesn't seem to make much
;;; sense to use the 387's idea of a stack; 8 separate registers is easier
;;; to deal with.
;;; the old way:
;;;   (define-storage-base float-registers :finite :size 1)
;;; the new way:
(define-storage-base float-registers :finite :size 8)

(define-storage-base stack :unbounded :size 3 :size-increment 1)
(define-storage-base constant :non-packed)
(define-storage-base immediate-constant :non-packed))

;;;; SC definitions

(eval-when (:compile-toplevel :execute)
(defparameter *storage-class-defs* '(

  ;; non-immediate constants in the constant pool
  (constant constant)

  ;; some FP constants can be generated in the i387 silicon
  (fp-constant immediate-constant)
  (fp-single-immediate immediate-constant)
  (fp-double-immediate immediate-constant)
  (immediate immediate-constant)

  ;;
  ;; the stacks
  ;;

  ;; the control stack
  (control-stack stack)                 ; may be pointers, scanned by GC

  ;; the non-descriptor stacks
  (signed-stack stack)                  ; (signed-byte 32)
  (unsigned-stack stack)                ; (unsigned-byte 32)
  (character-stack stack)               ; non-descriptor characters.
  (sap-stack stack)                     ; System area pointers.
  (single-stack stack)                  ; single-floats
  (double-stack stack :element-size 2)  ; double-floats.
  #+long-float
  (long-stack stack :element-size 3)    ; long-floats.
  (complex-single-stack stack :element-size 2)  ; complex-single-floats
  (complex-double-stack stack :element-size 4)  ; complex-double-floats
  #+long-float
  (complex-long-stack stack :element-size 6)    ; complex-long-floats

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
           :locations #.*dword-regs*
           :element-size 2
;          :reserve-locations (#.eax-offset)
           :constant-scs (immediate)
           :save-p t
           :alternate-scs (control-stack))

  ;; pointer descriptor objects -- must be seen by GC
  (descriptor-reg registers
                  :locations #.*dword-regs*
                  :element-size 2
;                 :reserve-locations (#.eax-offset)
                  :constant-scs (constant immediate)
                  :save-p t
                  :alternate-scs (control-stack))

  ;; non-descriptor characters
  (character-reg registers
                 :locations #-sb-unicode #.*byte-regs*
                            #+sb-unicode #.*dword-regs*
                 #+sb-unicode #+sb-unicode
                 :element-size 2
                 #-sb-unicode #-sb-unicode
                 :reserve-locations (#.ah-offset #.al-offset)
                 :constant-scs (immediate)
                 :save-p t
                 :alternate-scs (character-stack))

  ;; non-descriptor SAPs (arbitrary pointers into address space)
  (sap-reg registers
           :locations #.*dword-regs*
           :element-size 2
;          :reserve-locations (#.eax-offset)
           :constant-scs (immediate)
           :save-p t
           :alternate-scs (sap-stack))

  ;; non-descriptor (signed or unsigned) numbers
  (signed-reg registers
              :locations #.*dword-regs*
              :element-size 2
;             :reserve-locations (#.eax-offset)
              :constant-scs (immediate)
              :save-p t
              :alternate-scs (signed-stack))
  (unsigned-reg registers
                :locations #.*dword-regs*
                :element-size 2
;               :reserve-locations (#.eax-offset)
                :constant-scs (immediate)
                :save-p t
                :alternate-scs (unsigned-stack))

  ;; miscellaneous objects that must not be seen by GC. Used only as
  ;; temporaries.
  (word-reg registers
            :locations #.*word-regs*
            :element-size 2
;           :reserve-locations (#.ax-offset)
            )
  (byte-reg registers
            :locations #.*byte-regs*
;           :reserve-locations (#.al-offset #.ah-offset)
            )

  ;; that can go in the floating point registers

  ;; non-descriptor SINGLE-FLOATs
  (single-reg float-registers
              :locations (0 1 2 3 4 5 6 7)
              :constant-scs (fp-constant fp-single-immediate)
              :save-p t
              :alternate-scs (single-stack))

  ;; non-descriptor DOUBLE-FLOATs
  (double-reg float-registers
              :locations (0 1 2 3 4 5 6 7)
              :constant-scs (fp-constant fp-double-immediate)
              :save-p t
              :alternate-scs (double-stack))

  ;; non-descriptor LONG-FLOATs
  #+long-float
  (long-reg float-registers
            :locations (0 1 2 3 4 5 6 7)
            :constant-scs (fp-constant)
            :save-p t
            :alternate-scs (long-stack))

  (complex-single-reg float-registers
                      :locations (0 2 4 6)
                      :element-size 2
                      :constant-scs ()
                      :save-p t
                      :alternate-scs (complex-single-stack))

  (complex-double-reg float-registers
                      :locations (0 2 4 6)
                      :element-size 2
                      :constant-scs ()
                      :save-p t
                      :alternate-scs (complex-double-stack))

  #+long-float
  (complex-long-reg float-registers
                    :locations (0 2 4 6)
                    :element-size 2
                    :constant-scs ()
                    :save-p t
                    :alternate-scs (complex-long-stack))

  (catch-block stack :element-size catch-block-size)
  (unwind-block stack :element-size unwind-block-size)))

(defparameter *byte-sc-names*
  '(#-sb-unicode character-reg byte-reg #-sb-unicode character-stack))
(defparameter *word-sc-names* '(word-reg))
(defparameter *dword-sc-names*
  '(any-reg descriptor-reg sap-reg signed-reg unsigned-reg control-stack
    signed-stack unsigned-stack sap-stack single-stack
    #+sb-unicode character-reg #+sb-unicode character-stack constant))
;;; added by jrd. I guess the right thing to do is to treat floats
;;; as a separate size...
;;;
;;; These are used to (at least) determine operand size.
(defparameter *float-sc-names* '(single-reg))
(defparameter *double-sc-names* '(double-reg double-stack))
) ; EVAL-WHEN
(!define-storage-classes
  . #.(mapcar (lambda (class-spec)
                (let ((size
                       (case (car class-spec)
                         (#.*dword-sc-names*   :dword)
                         (#.*word-sc-names*    :word)
                         (#.*byte-sc-names*    :byte)
                         (#.*float-sc-names*   :float)
                         (#.*double-sc-names*  :double))))
                  (append class-spec (if size (list :operand-size size)))))
              *storage-class-defs*))

;;;; miscellaneous TNs for the various registers

(macrolet ((def-misc-reg-tns (sc-name &rest reg-names)
             (collect ((forms))
               (dolist (reg-name reg-names `(progn ,@(forms)))
                 (let ((tn-name (symbolicate reg-name "-TN"))
                       (offset-name (symbolicate reg-name "-OFFSET")))
                   (forms `(defglobal ,tn-name
                             (make-random-tn (sc-or-lose ',sc-name) ,offset-name))))))))

  (def-misc-reg-tns unsigned-reg eax ebx ecx edx ebp esp edi esi)
  (def-misc-reg-tns word-reg ax bx cx dx bp sp di si)
  (def-misc-reg-tns byte-reg al ah bl bh cl ch dl dh)
  (def-misc-reg-tns single-reg fr0 fr1 fr2 fr3 fr4 fr5 fr6 fr7))

;;; TNs for registers used to pass arguments
(define-load-time-global *register-arg-tns*
  (mapcar (lambda (register-arg-name)
            (symbol-value (symbolicate register-arg-name "-TN")))
          register-arg-names))

;;; FIXME: doesn't seem to be used in SBCL
#|
;;; added by pw
(defparameter fp-constant-tn
  (make-random-tn (sc-or-lose 'fp-constant) 31))          ; Offset doesn't get used.
|#

;;; If value can be represented as an immediate constant, then return
;;; the appropriate SC number, otherwise return NIL.
(defun immediate-constant-sc (value)
  (typecase value
    ((or (integer #.most-negative-fixnum #.most-positive-fixnum)
         character)
     immediate-sc-number)
    (symbol
     (when (static-symbol-p value)
       immediate-sc-number))
    (single-float
       (case value
         ((0f0 1f0) fp-constant-sc-number)
         (t fp-single-immediate-sc-number)))
    (double-float
       (case value
         ((0d0 1d0) fp-constant-sc-number)
         (t fp-double-immediate-sc-number)))
    #+long-float
    (long-float
       (when (or (eql value 0l0) (eql value 1l0)
                 (eql value pi)
                 (eql value (log 10l0 2l0))
                 (eql value (log 2.718281828459045235360287471352662L0 2l0))
                 (eql value (log 2l0 10l0))
                 (eql value (log 2l0 2.718281828459045235360287471352662L0)))
         fp-constant-sc-number))
    (structure-object
     (when (eq value sb-lockless:+tail+)
       immediate-sc-number))))

(defun boxed-immediate-sc-p (sc)
  (eql sc immediate-sc-number))

;; For an immediate TN, return its value encoded for use as a literal.
;; For any other TN, return the TN.  Only works for FIXNUMs,
;; STATIC-SYMBOLs, and CHARACTERS (FLOATs and SAPs are handled
;; elsewhere).
(defun encode-value-if-immediate (tn)
  (if (sc-is tn immediate)
      (let ((val (tn-value tn)))
        (etypecase val
          (integer (fixnumize val))
          (symbol (+ nil-value (static-symbol-offset val)))
          (character (logior (ash (char-code val) n-widetag-bits)
                             character-widetag))
          (structure-object
           (if (eq val sb-lockless:+tail+)
               (+ static-space-start lockfree-list-tail-value-offset)
               (bug "immediate structure-object ~S" val)))))
      tn))

;;;; miscellaneous function call parameters

;;; Offsets of special stack frame locations relative to EBP.
;;;
;;; Consider the standard prologue PUSH EBP; MOV EBP, ESP: the return
;;; address is at EBP+4, the old control stack frame pointer is at
;;; EBP, the magic 3rd slot is at EBP-4. Then come the locals from
;;; EBP-8 on.
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

;;; FIXME: This is a bad comment (changed since when?) and there are others
;;; like it in this file. It'd be nice to clarify them. Failing that deleting
;;; them or flagging them with KLUDGE might be better than nothing.
;;;
;;; names of these things seem to have changed. these aliases by jrd

(defconstant nargs-offset ecx-offset)
(defconstant cfp-offset ebp-offset)    ; pfw - needed by stuff in /code
                                        ; related to signal context stuff

;;; This is used by the debugger.
(defconstant single-value-return-byte-offset 0)

;;; This function is called by debug output routines that want a pretty name
;;; for a TN's location. It returns a thing that can be printed with PRINC.
(defun location-print-name (tn)
  (declare (type tn tn))
  (let* ((sc (tn-sc tn))
         (sb (sb-name (sc-sb sc)))
         (offset (tn-offset tn)))
    (ecase sb
      (registers
       (let ((name-vec (case (sb-c:sc-operand-size sc)
                         (:byte  +byte-register-names+)
                         (:word  +word-register-names+)
                         (:dword +dword-register-names+))))
         (or (and name-vec
                  (< -1 offset (length name-vec))
                  (svref name-vec offset))
             ;; FIXME: Shouldn't this be an ERROR?
             (format nil "<unknown reg: off=~W, sc=~A>" offset (sc-name sc)))))
      (float-registers (format nil "FR~D" offset))
      (stack (format nil "S~D" offset))
      (constant (format nil "Const~D" offset))
      (immediate-constant "Immed")
      (noise (symbol-name (sc-name sc))))))
