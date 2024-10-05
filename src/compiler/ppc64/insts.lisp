;;;; the instruction set definition for the PPC

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-PPC64-ASM")

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Imports from this package into SB-VM
  (import '(reg-tn-encoding) "SB-VM")
  ;; Imports from SB-VM into this package
  (import '(;; SBs and SCs
            sb-vm::zero sb-vm::immediate-constant
            sb-vm::registers sb-vm::float-registers
            ;; TNs and offsets
            sb-vm::code-tn
            #-64-bit
            sb-vm::zero-tn
            sb-vm::code-tn-lowtag
            sb-vm::code-offset
            sb-vm::lip-tn
            sb-vm::null-offset)))

;;;; Constants, types, conversion functions, some disassembler stuff.

;;; This constant is referenced only in files in the ppc64 subdirectory,
;;; but ppc32 uses this file as well.
(defconstant code-tn-lowtag #+ppc64 0 #-ppc64 other-pointer-lowtag)

(defun reg-tn-encoding (tn)
  (declare (type tn tn))
  (sc-case tn
    ;; zero is not a storage class for the 64-bit vm
    #-64-bit (zero sb-vm::zero-offset)
    (null null-offset)
    (t
     (if (eq (sb-name (sc-sb (tn-sc tn))) 'registers)
         (tn-offset tn)
         (error "~S isn't a register." tn)))))

(defun reg-or-0 (tn)
  (cond ((eql tn 0) 0)
        ((eql (tn-offset tn) 0) (error "Can't encode RA=r0"))
        (t (reg-tn-encoding tn))))

(defun fp-reg-tn-encoding (tn)
  (declare (type tn tn))
  (unless (eq (sb-name (sc-sb (tn-sc tn))) 'float-registers)
    (error "~S isn't a floating-point register." tn))
  (tn-offset tn))

(defvar *disassem-use-lisp-reg-names* t)

(defun location-number (loc)
  (etypecase loc
    (null)
    (number)
    (label)
    (fixup)
    (tn
     (ecase (sb-name (sc-sb (tn-sc loc)))
       (immediate-constant
        ;; Can happen if $ZERO or $NULL are passed in.
        nil)
       (registers
        (unless (zerop (tn-offset loc))
          (tn-offset loc)))
       (float-registers
        (+ (tn-offset loc) 32))))
    (symbol
     (ecase loc
       (:memory 0)
       (:ccr 64)
       (:xer 65)
       (:lr 66)
       (:ctr 67)
       (:fpscr 68)))))

(defparameter reg-symbols
  (map 'vector
       #'(lambda (name)
           (cond ((null name) nil)
                 (t (make-symbol (concatenate 'string "$" name)))))
       sb-vm::*register-names*))

(define-arg-type reg
  :printer
  (lambda (value stream dstate)
    (let ((regname (aref reg-symbols value)))
      (cond (stream
             (princ regname stream)
             (maybe-note-associated-storage-ref value 'registers regname dstate)
             (maybe-add-notes value dstate))
            (t
             (operand regname dstate))))))

(defparameter float-reg-symbols
  #.(coerce
     (loop for n from 0 to 31 collect (make-symbol (format nil "$F~d" n)))
     'vector))

(define-arg-type fp-reg
  :printer #'(lambda (value stream dstate)
               (let ((regname (aref float-reg-symbols value)))
                 (cond (stream
                        (princ regname stream)
                        (maybe-note-associated-storage-ref
                         value 'float-registers regname dstate))
                       (t
                        (operand regname dstate))))))

(define-arg-type ds-field
  :prefilter (lambda (dstate value)
               (declare (ignore dstate))
               (* value 4)))

(defconstant-eqx bo-kind-names
    #(:bo-dnzf :bo-dnzfp :bo-dzf :bo-dzfp :bo-f :bo-fp nil nil
      :bo-dnzt :bo-dnztp :bo-dzt :bo-dztp :bo-t :bo-tp nil nil
      :bo-dnz :bo-dnzp :bo-dz :bo-dzp :bo-u nil nil nil
      nil nil nil nil nil nil nil nil)
    #'equalp)

(define-arg-type bo-field
  :printer #'(lambda (value stream dstate)
               (let ((thing (svref bo-kind-names value)))
                 (if stream
                     (princ thing stream)
                     (operand thing dstate)))))

(define-compiler-macro valid-bo-encoding (&whole form enc)
  (declare (notinline valid-bo-encoding))
  (if (keywordp enc) (valid-bo-encoding enc) form))
(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
(defun valid-bo-encoding (enc)
  (or (if (integerp enc)
        (and (= enc (logand #x1f enc))
             (not (null (svref bo-kind-names enc)))
             enc)
        (and enc (position enc bo-kind-names)))
      (error "Invalid BO field spec: ~s" enc)))
)

(defconstant-eqx cr-bit-names #(:lt :gt :eq :so) #'equalp)
(defconstant-eqx cr-bit-inverse-names #(:ge :le :ne :ns) #'equalp)

(defconstant-eqx cr-field-names #(:cr0 :cr1 :cr2 :cr3 :cr4 :cr5 :cr6 :cr7)
  #'equalp)

(defun valid-cr-bit-encoding (enc &optional error-p)
  (or (if (integerp enc)
        (and (= enc (logand 3 enc))
             enc))
      (position enc cr-bit-names)
      (if error-p (error "Invalid condition bit specifier : ~s" enc))))

(defun valid-cr-field-encoding (enc)
  (let* ((field (if (integerp enc)
                    (and (= enc (logand #x7 enc)) enc)
                    (position enc cr-field-names))))
    (if field
      (ash field 2)
      (error "Invalid condition register field specifier : ~s" enc))))

(defun valid-bi-encoding (enc)
  (or
   (if (atom enc)
     (if (integerp enc)
       (and (= enc (logand 31 enc)) enc)
       (position enc cr-bit-names))
     (+ (valid-cr-field-encoding (car enc))
        (valid-cr-bit-encoding (cadr enc))))
   (error "Invalid BI field spec : ~s" enc)))

(define-arg-type bi-field
  :printer #'(lambda (value stream dstate)
               (declare (type (unsigned-byte 5) value))
               (let* ((bitname (svref cr-bit-names (logand 3 value)))
                      (crfield (ash value -2)))
                 (declare (type (unsigned-byte 3) crfield))
                 (let ((operand
                         (if (= crfield 0)
                             bitname
                             (list (svref cr-field-names crfield) bitname))))
                   (if stream
                       (princ operand stream)
                       (operand operand dstate))))))

(define-arg-type crf
  :printer #'(lambda (value stream dstate)
               (declare (type (unsigned-byte 3) value))
               (let ((operand (svref cr-field-names value)))
                 (if stream
                     (princ operand stream)
                     (operand operand dstate)))))

(define-arg-type relative-label
  :sign-extend t
  :use-label #'(lambda (value dstate)
                 (declare (type (signed-byte 24) value))
                 (+ (ash value 2) (dstate-cur-addr dstate))))

(defconstant-eqx trap-values-alist
  '((:t . 31) (:lt . 16) (:le . 20) (:eq . 4) (:lng . 6)
    (:ge . 12) (:ne . 24) (:ng . 20) (:llt . 2) (:f . 0)
    (:lle . 6) (:lge . 5) (:lgt . 1) (:lnl . 5))
  #'equal)


(defun valid-tcond-encoding (enc)
  (or (and (if (integerp enc) (= (logand 31 enc) enc)) enc)
      (cdr (assoc enc trap-values-alist))
      (error "Unknown trap condition: ~s" enc)))

(define-arg-type to-field
  :sign-extend nil
  :printer #'(lambda (value stream dstate)
               (declare (type fixnum value))
               (let ((operand (or (car (rassoc value trap-values-alist))
                                  value)))
                 (if stream
                     (princ operand stream)
                     (operand operand dstate)))))

(defun emit-conditional-branch (segment bo bi target &optional aa-p lk-p)
  (declare (type boolean aa-p lk-p))
  (let* ((bo (valid-bo-encoding bo))
         (bi (valid-bi-encoding bi))
         (aa-bit (if aa-p 1 0))
         (lk-bit (if lk-p 1 0)))
    (if aa-p                            ; Not bloody likely, bwth.
      (emit-b-form-inst segment 16 bo bi target aa-bit lk-bit)
      ;; the target may be >32k away, in which case we have to invert the
      ;; test and do an absolute branch
      (emit-chooser
       ;; We emit either 4 or 8 bytes, so I think we declare this as
       ;; preserving 4 byte alignment.  If this gives us no joy, we can
       ;; stick a nop in the long branch and then we will be
       ;; preserving 8 byte alignment
       segment 8 2 ; 2^2 is 4 byte alignment.  I think
       #'(lambda (segment chooser posn magic-value)
           (declare (ignore chooser))
           (let ((delta (ash (- (label-position target posn magic-value) posn)
                             -2)))
             (when (typep delta '(signed-byte 14))
               (emit-back-patch segment 4
                                #'(lambda (segment posn)
                                    (emit-b-form-inst
                                     segment 16 bo bi
                                     (ash (- (label-position target) posn) -2)
                                     aa-bit lk-bit)))
               t)))
       #'(lambda (segment posn)
           (declare (ignore posn))
           (let ((bo (logxor 8 bo))) ;; invert the test
             (emit-b-form-inst segment 16 bo bi
                               2 ; skip over next instruction
                               0 0)
             (emit-back-patch segment 4
                              #'(lambda (segment posn)
                                  (declare (ignore posn))
                                  (emit-i-form-branch segment target lk-p)))))
       ))))



; non-absolute I-form: B, BL.
(defun emit-i-form-branch (segment target &optional lk-p)
  (let* ((lk-bit (if lk-p 1 0)))
    (etypecase target
      (fixup
       (note-fixup segment :b target)
       (emit-i-form-inst segment 18 0 0 lk-bit))
      (label
       (emit-back-patch segment 4
                        #'(lambda (segment posn)
                            (emit-i-form-inst
                             segment
                             18
                             (ash (- (label-position target) posn) -2)
                             0
                             lk-bit)))))))

(defconstant-eqx +spr-numbers-alist+ '((:xer 1) (:lr 8) (:ctr 9)) #'equal)

(define-arg-type spr
  :printer #'(lambda (value stream dstate)
               (declare (type (unsigned-byte 10) value))
               (let* ((name (car (rassoc value +spr-numbers-alist+)))
                      (operand (or name value)))
                 (if stream
                     (princ operand stream)
                     (operand operand dstate)))))

#-sb-xc-host ; no definition of MAYBE-NOTE-ASSEMBLER-ROUTINE
(defparameter jump-printer
    #'(lambda (value stream dstate)
        (let ((addr (ash value 2)))
          (maybe-note-assembler-routine addr t dstate)
          (if stream
              (write addr :base 16 :radix t :stream stream)
              (operand addr dstate)))))



;;;; dissassem:define-instruction-formats

(defmacro ppc-byte (startbit &optional (endbit startbit))
  (unless (and (typep startbit '(unsigned-byte 32))
               (typep endbit '(unsigned-byte 32))
               (>= endbit startbit))
    (error "Bad bits."))
  ``(byte ,(1+ ,(- endbit startbit)) ,(- 31 ,endbit)))

(defglobal *ppc-field-specs-alist*
    `((aa :field ,(ppc-byte 30))
      (ba :field ,(ppc-byte 11 15) :type 'bi-field)
      (bb :field ,(ppc-byte 16 20) :type 'bi-field)
      (bd :field ,(ppc-byte 16 29) :type 'relative-label)
      (bf :field ,(ppc-byte 6 8) :type 'crf)
      (bfa :field ,(ppc-byte 11 13) :type 'crf)
      (bi :field ,(ppc-byte 11 15) :type 'bi-field)
      (bo :field ,(ppc-byte 6 10) :type 'bo-field)
      (bt :field ,(ppc-byte 6 10) :type 'bi-field)
      (d :field ,(ppc-byte 16 31) :sign-extend t)
      (ds :field ,(ppc-byte 16 29) :sign-extend t :type 'ds-field)
      ;; I don't think the 2 leftover bits of the scaled D field
      ;; (bits that would otherwise be part of the immediate value
      ;; were it not for their use an an opcode extension) have
      ;; a name in the processor manual. I'm naming them SUBOP.
      (ds-form-subop :field ,(ppc-byte 30 31))
      (flm :field ,(ppc-byte 7 14) :sign-extend nil)
      (fra :field ,(ppc-byte 11 15) :type 'fp-reg)
      (frb :field ,(ppc-byte 16 20) :type 'fp-reg)
      (frc :field ,(ppc-byte 21 25) :type 'fp-reg)
      (frs :field ,(ppc-byte 6 10) :type 'fp-reg)
      (frt :field ,(ppc-byte 6 10) :type 'fp-reg)
      (fxm :field ,(ppc-byte 12 19) :sign-extend nil)
      (l :field ,(ppc-byte 10) :sign-extend nil)
      (li :field ,(ppc-byte 6 29) :sign-extend t :type 'relative-label)
      (li-abs :field ,(ppc-byte 6 29) :sign-extend t :printer jump-printer)
      (lk :field ,(ppc-byte 31))
      (mb :field ,(ppc-byte 21 25) :sign-extend nil)
      (me :field ,(ppc-byte 26 30) :sign-extend nil)
      (nb :field ,(ppc-byte 16 20) :sign-extend nil)
      (oe :field ,(ppc-byte 21))
      (ra :field ,(ppc-byte 11 15) :type 'reg)
      (rb :field ,(ppc-byte 16 20) :type 'reg)
      (rc :field ,(ppc-byte 31))
      (rs :field ,(ppc-byte 6 10) :type 'reg)
      (rt :field ,(ppc-byte 6 10) :type 'reg)
      (sh :field ,(ppc-byte 16 20) :sign-extend nil)
      (si :field ,(ppc-byte 16 31) :sign-extend t)
      (spr :field ,(ppc-byte 11 20) :type 'spr)
      (to :field ,(ppc-byte 6 10) :type 'to-field)
      (u :field ,(ppc-byte 16 19) :sign-extend nil)
      (ui :field ,(ppc-byte 16 31) :sign-extend nil)
      (xo21-30 :field ,(ppc-byte 21 30) :sign-extend nil)
      (xo22-30 :field ,(ppc-byte 22 30) :sign-extend nil)
      (xo26-30 :field ,(ppc-byte 26 30) :sign-extend nil)

      ;; Apart from abbreviations documented in the ISA manual,
      ;; I can not figure out the naming convention at all.
      ;; So I'm making up my own WITH COMMENTS starting now.

      ;; 6-bit mask begin/end
      (mask6 :field ,(ppc-byte 21 26) :prefilter #'decode-mask6)
      ;; 6-bit shift amount
      (sh6 :fields (list ,(ppc-byte 16 20) ,(ppc-byte 30)) :prefilter #'unsplit-sh)
      (md-form-subop :field ,(ppc-byte 27 29))
      (mds-form-subop :field ,(ppc-byte 27 30))
      (xs-form-subop :field ,(ppc-byte 21 29))
      ))

(define-instruction-format (instr 32)
  (op :field (byte 6 26))
  (other :field (byte 26 0)))

(define-instruction-format (sc 32 :default-printer '(:name :tab rest))
  (op :field (byte 6 26))
  (rest :field (byte 26 0) :value 2))



(defmacro def-ppc-iformat ((name &optional default-printer) &rest specs)
  (flet ((specname-field (specname)
           (or (assoc specname *ppc-field-specs-alist*)
               (error "Unknown ppc instruction field spec ~s" specname))))
    (labels ((spec-field (spec)
               (if (atom spec)
                   (specname-field spec)
                   (cons (car spec)
                         (cdr (specname-field (cadr spec)))))))
      (collect ((field (list '(op :field (byte 6 26)))))
               (dolist (spec specs)
                 (field (spec-field spec)))
               `(define-instruction-format (,name 32 ,@(if default-printer `(:default-printer ,default-printer)))
                 ,@(field))))))

(def-ppc-iformat (i '(:name :tab li))
  li aa lk)

(def-ppc-iformat (i-abs '(:name :tab li-abs))
  li-abs aa lk)

(def-ppc-iformat (b '(:name :tab bo "," bi "," bd))
  bo bi bd aa lk)

(def-ppc-iformat (d '(:name :tab rt "," d "(" ra ")"))
  rt ra d)

(def-ppc-iformat (ds '(:name :tab rt "," ; D scaled
                       #+ppc64 (:using #'ds-annotate ds)
                       #+ppc ds
                       "(" ra ")"))
  rt ra ds (subop ds-form-subop))

(def-ppc-iformat (d-si '(:name :tab rt "," ra "," si )) ; D with signed immediate
  rt ra si)

(def-ppc-iformat (d-rs '(:name :tab rs "," d "(" ra ")"))
  rs ra d)

(def-ppc-iformat (d-rs-ui '(:name :tab ra "," rs "," ui))
  rs ra ui)

(def-ppc-iformat (d-crf-si)
  bf l ra si)

(def-ppc-iformat (d-crf-ui)
  bf l ra ui)

(def-ppc-iformat (d-to '(:name :tab to "," ra "," si))
  to ra rb si)

(def-ppc-iformat (d-frt '(:name :tab frt "," d "(" ra ")"))
  frt ra d)

(def-ppc-iformat (d-frs '(:name :tab frs "," d "(" ra ")"))
  frs ra d)



;;; There are around ... oh, 28 or so ... variants on the "X" format.
;;;  Some of them are only used by one instruction; some are used by dozens.
;;;  Some aren't used by instructions that we generate ...

(def-ppc-iformat (x '(:name :tab rt "," ra "," rb))
  rt ra rb (xo xo21-30))

(def-ppc-iformat (x-1 '(:name :tab rt "," ra "," nb))
  rt ra nb (xo xo21-30))

(def-ppc-iformat (x-4 '(:name :tab rt))
  rt (xo xo21-30))

(def-ppc-iformat (x-5 '(:name :tab rs "," ra "," rb))
  rs ra rb (xo xo21-30) rc)

(def-ppc-iformat (x-8 '(:name :tab rs "," ra "," nb))
  rs ra nb (xo xo21-30))

(def-ppc-iformat (x-9 '(:name :tab ra "," rs "," sh))
  rs ra sh (xo xo21-30) rc)

(def-ppc-iformat (x-10 '(:name :tab ra "," rs))
  rs ra (xo xo21-30) rc)

(def-ppc-iformat (x-14 '(:name :tab bf "," l "," ra "," rb))
  bf l ra rb (xo xo21-30))

(def-ppc-iformat (x-15 '(:name :tab bf "," l "," fra "," frb))
  bf l fra frb (xo xo21-30))

(def-ppc-iformat (x-18 '(:name :tab bf))
  bf (xo xo21-30))

(def-ppc-iformat (x-19 '(:name :tab to "," ra "," rb))
  to ra rb (xo xo21-30))

(def-ppc-iformat (x-20 '(:name :tab frt "," ra "," rb))
  frt ra rb (xo xo21-30))

(def-ppc-iformat (x-21 '(:name :tab frt "," rb))
  frt rb (xo xo21-30) rc)

(def-ppc-iformat (x-22 '(:name :tab frt))
  frt (xo xo21-30) rc)

(def-ppc-iformat (x-23 '(:name :tab frs "," ra "," rb))
  frs ra rb (xo xo21-30))

(def-ppc-iformat (x-24 '(:name :tab bt))
  bt (xo xo21-30) rc)

(def-ppc-iformat (x-25 '(:name :tab ra "," rb))
  ra rb (xo xo21-30))

(def-ppc-iformat (x-26 '(:name :tab rb))
  rb (xo xo21-30))

(def-ppc-iformat (x-27 '(:name))
  (xo xo21-30))


;;;;

(def-ppc-iformat (xl '(:name :tab bt "," ba "," bb))
  bt ba bb (xo xo21-30))

(def-ppc-iformat (xl-bo-bi '(:name :tab bo "," bi))
  bo bi (xo xo21-30) lk)

(def-ppc-iformat (xl-cr '(:name :tab bf "," bfa))
  bf bfa (xo xo21-30))

(def-ppc-iformat (xl-xo '(:name))
  (xo xo21-30))


;;;;

(def-ppc-iformat (xfx)
  rt spr (xo xo21-30))

(def-ppc-iformat (xfx-fxm '(:name :tab fxm "," rs))
  rs fxm (xo xo21-30))

(def-ppc-iformat (xfl '(:name :tab flm "," frb))
  flm frb (xo xo21-30) rc)


;;;

(def-ppc-iformat (xo '(:name :tab rt "," ra "," rb))
  rt ra rb oe (xo xo22-30) rc)

(def-ppc-iformat (xo-oe '(:name :tab rt "," ra "," rb))
  rt ra rb (xo xo22-30) rc)

(def-ppc-iformat (xo-a '(:name :tab rt "," ra))
  rt ra oe (xo xo22-30) rc)


;;;

(def-ppc-iformat (a '(:name :tab frt "," fra "," frb "," frc))
  frt fra frb frc (xo xo26-30) rc)

(def-ppc-iformat (a-tab '(:name :tab frt "," fra "," frb))
  frt fra frb (xo xo26-30) rc)

(def-ppc-iformat (a-tac '(:name :tab frt "," fra "," frc))
  frt fra frc (xo xo26-30) rc)

(def-ppc-iformat (a-tbc '(:name :tab frt "," frb "," frc))
  frt frb frc (xo xo26-30) rc)


(def-ppc-iformat (m '(:name :tab ra "," rs "," rb "," mb "," me))
  rs ra rb mb me rc)

(def-ppc-iformat (m-sh '(:name :tab ra "," rs "," sh "," mb "," me))
  rs ra sh mb me rc)

(define-instruction-format (xinstr 32 :default-printer '(:name :tab data))
  (op-to-a :field (byte 16 16))
  (data :field (byte 16 0) :reader xinstr-data))



;;;; Primitive emitters.


(define-bitfield-emitter emit-word 32 (byte 32 0))
(define-bitfield-emitter emit-dword 64 (byte 64 0))

(define-bitfield-emitter emit-i-form-inst 32
  (byte 6 26) (byte 24 2) (byte 1 1) (byte 1 0))

(define-bitfield-emitter emit-b-form-inst 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 14 2) (byte 1 1) (byte 1 0))

(define-bitfield-emitter emit-sc-form-inst 32
  (byte 6 26) (byte 26 0))

(define-bitfield-emitter emit-d-form-inst 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 16 0))

(define-bitfield-emitter emit-ds-form-inst 32 ; D scaled form
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 14 2) (byte 2 0))

; Also used for XL-form.  What's the difference ?
(define-bitfield-emitter emit-x-form-inst 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 5 11) (byte 10 1) (byte 1 0))

(define-bitfield-emitter emit-xfx-form-inst 32
  (byte 6 26) (byte 5 21) (byte 10 11) (byte 10 1) (byte 1 0))

(define-bitfield-emitter emit-xfl-form-inst 32
  (byte 6 26) (byte 10  16) (byte 5 11) (byte 10 1) (byte 1 0))

(define-bitfield-emitter emit-xo-form-inst 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 5 11) (byte 1 10) (byte 9 1) (byte 1 0))

(define-bitfield-emitter emit-a-form-inst 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 5 11) (byte 5 6) (byte 5 1) (byte 1 0))



(defun patchable-emit-d-form (segment opcode rt ra si)
  (cond ((and (label-p si) (= ra code-offset))
         ;; Assume that the displacement is short enough to
         ;; be 1 instruction and we never need to use a chooser.
         (emit-back-patch
          segment 4
          (lambda (segment posn)
            (declare (ignore posn))
            (emit-d-form-inst segment opcode rt ra
                              (+ (component-header-length)
                                 (segment-header-skew segment)
                                 (label-position si)
                                 (- sb-vm::code-tn-lowtag))))))
        (t
         (when (typep si 'fixup)
           (note-fixup segment :l si)
           (setq si 0))
         (emit-d-form-inst segment opcode rt ra si))))

(defun patchable-emit-ds-form (segment opcode rt ra si subop)
  (cond ((and (label-p si) (= ra code-offset))
         ;; Assume that the displacement is short enough to
         ;; be 1 instruction and we never need to use a chooser.
         (emit-back-patch
          segment 4
          (lambda (segment posn)
            (declare (ignore posn))
            ;; call recursively to get the error check and scaling
            (patchable-emit-ds-form
             segment opcode rt ra
             (+ (component-header-length)
                (segment-header-skew segment)
                (label-position si))
             subop))))
        (t
         (when (fixup-p si) ; assume the fixup will be valid
           (aver (eq (fixup-flavor si) :linkage-cell))
           (note-fixup segment :addis+ld si)
           (setq si 0))
         (if (= (mod si 4) 0)
             (emit-ds-form-inst segment opcode rt ra (ash si -2) subop)
             (error "Displacement should be a multiple of 4")))))

(eval-when (:compile-toplevel :execute)
(defun classify-dependencies (deplist)
  (collect ((reads) (writes))
    (dolist (dep deplist)
      (ecase (car dep)
        (reads (reads dep))
        (writes (writes dep))))
    (values (reads) (writes)))))

;; Load-store instructions can not access register 0 as RA.
;; In the non-updating opcodes, 0 encodes a literal 0.
;; In the updating opcodes, "If RA=0 or RA=RT, the instruction form is invalid."
(macrolet ((define-d-instruction (name op &key (cost 2) other-dependencies pinned)
             (multiple-value-bind (other-reads other-writes) (classify-dependencies other-dependencies)
               `(define-instruction ,name (segment rt ra si)
                 (:declare (type (or (signed-byte 16) label) si))
                 (:printer d ((op ,op)))
                 (:delay ,cost)
                 (:cost ,cost)
                 ,@(when pinned '(:pinned))
                 (:dependencies (reads ra) (reads :memory) ,@other-reads
                                (writes rt) ,@other-writes)
                 (:emitter
                  (patchable-emit-d-form segment ,op (reg-tn-encoding rt) (reg-or-0 ra) si)))))
           (define-ds-instruction (name op subop)
             `(define-instruction ,name (segment rt ra si)
                (:declare (type (or (signed-byte 16) label) si))
                (:printer ds ((op ,op) (subop ,subop)))
                (:emitter
                 (patchable-emit-ds-form segment ,op (reg-tn-encoding rt)
                                         (reg-or-0 ra) si ,subop))))
           ;; This is really stupid. We use a diffent macro because use a different printer
           ;; because we use a different instruction format.
           ;; Why not just print loads and stores using the SAME format???
           ;; We're basically causing a turtles-all-the-way-down problem.
           (define-d-rs-instruction (name op &key (cost 1) other-dependencies pinned)
             (multiple-value-bind (other-reads other-writes) (classify-dependencies other-dependencies)
               `(define-instruction ,name (segment rs ra si)
                  (:declare (type (signed-byte 16) si))
                  (:printer d-rs ((op ,op)))
                  (:delay ,cost)
                  (:cost ,cost)
                  ,@(when pinned '(:pinned))
                  (:dependencies (reads rs) (reads ra) ,@other-reads
                                 (writes :memory :partially t) ,@other-writes)
                  (:emitter
                   (emit-d-form-inst segment ,op (reg-tn-encoding rs) (reg-or-0 ra) si)))))
           (define-x-instruction (name op xo &key (cost 2) other-dependencies)
             (multiple-value-bind (other-reads other-writes) (classify-dependencies other-dependencies)
               `(define-instruction ,name (segment rt ra rb)
                 (:printer x ((op ,op) (xo ,xo)))
                 (:delay ,cost)
                 (:cost ,cost)
                 (:dependencies (reads ra) (reads rb) (reads :memory) ,@other-reads
                                (writes rt) ,@other-writes)
                 (:emitter
                  (emit-x-form-inst segment ,op (reg-tn-encoding rt)
                                    (reg-or-0 ra) (reg-tn-encoding rb) ,xo 0)))))
           ;; Bit 31 is the EH field for the store conditional forms, or the RC field
           ;; for ALU operations. It's arbitrary which name we give it.
           (define-x-5-st-instruction (name op xo bit31 &key (cost 1) other-dependencies)
             (multiple-value-bind (other-reads other-writes) (classify-dependencies other-dependencies)
               `(define-instruction ,name (segment rs ra rb)
                  (:printer x-5 ((op ,op) (xo ,xo) (rc ,bit31)))
                  (:delay ,cost)
                  (:cost ,cost)
                  (:dependencies (reads ra) (reads rb) (reads rs) ,@other-reads
                                 (writes :memory :partially t) ,@other-writes)
                  (:emitter
                   (emit-x-form-inst segment ,op (reg-tn-encoding rs)
                                     (reg-or-0 ra) (reg-tn-encoding rb) ,xo ,bit31)))))
           ;; As above - this is stupid.  Different macro, different printer, different format.
           ;; But they should be the same.
           (define-d-frt-instruction (name op &key (cost 3) other-dependencies)
             (multiple-value-bind (other-reads other-writes) (classify-dependencies other-dependencies)
               `(define-instruction ,name (segment frt ra si)
                 (:declare (type (or (signed-byte 16) label) si))
                 (:printer d-frt ((op ,op)))
                 (:delay ,cost)
                 (:cost ,cost)
                 (:dependencies (reads ra) (reads :memory) ,@other-reads
                                (writes frt) ,@other-writes)
                 (:emitter
                  (emit-d-form-inst segment ,op (fp-reg-tn-encoding frt) (reg-or-0 ra) si)))))
           (define-d-frs-instruction (name op &key (cost 1) other-dependencies)
             (multiple-value-bind (other-reads other-writes) (classify-dependencies other-dependencies)
               `(define-instruction ,name (segment frs ra si)
                 (:declare (type (signed-byte 16) si))
                 (:printer d-frs ((op ,op)))
                 (:delay ,cost)
                 (:cost ,cost)
                 (:dependencies (reads frs) (reads ra) ,@other-reads
                                (writes :memory :partially t) ,@other-writes)
                 (:emitter
                  (emit-d-form-inst segment ,op
                                    (fp-reg-tn-encoding frs) (reg-or-0 ra) si)))))
           ;; Again, more horribleness: essentially identical formats.
           ;; And these names are gobbledygook - if "X-5" means that it takes 5 operands
           ;; after the opcode, then what are "-20-" and "-23-" supposed to mean?
           ;; Clearly they don't take 20 and 23 operands respectively.
           (define-x-20-instruction (name op xo &key (cost 2) other-dependencies)
             (multiple-value-bind (other-reads other-writes) (classify-dependencies other-dependencies)
               `(define-instruction ,name (segment frt ra rb)
                  (:printer x-20 ((op ,op) (xo ,xo)))
                  (:delay ,cost)
                  (:cost ,cost)
                  (:dependencies (reads ra) (reads rb) ,@other-reads
                                 (writes frt) ,@other-writes)
                  (:emitter
                   (emit-x-form-inst segment ,op (fp-reg-tn-encoding frt)
                                     (reg-or-0 ra) (reg-tn-encoding rb) ,xo 0)))))
           (define-x-23-st-instruction (name op xo &key (cost 1) other-dependencies)
             (multiple-value-bind (other-reads other-writes) (classify-dependencies other-dependencies)
               `(define-instruction ,name (segment frs ra rb)
                  (:printer x-23 ((op ,op) (xo ,xo)))
                  (:delay ,cost)
                  (:cost ,cost)
                  (:dependencies (reads ra) (reads rb) (reads frs) ,@other-reads
                                 (writes :memory :partially t) ,@other-writes)
                  (:emitter
                   (emit-x-form-inst segment ,op (fp-reg-tn-encoding frs)
                                     (reg-or-0 ra) (reg-tn-encoding rb) ,xo 0))))))

  ;;; Loads
  ;; Byte
  (define-d-instruction      lbz   34)
  (define-d-instruction      lbzu  35 :other-dependencies ((writes ra)))
  (define-x-instruction      lbzx  31 87)
  (define-x-instruction      lbzux 31 119 :other-dependencies ((writes ra)))
  ;; Halfword zero-extend    ing
  (define-d-instruction      lhz   40)
  (define-d-instruction      lhzu  41 :other-dependencies ((writes ra)))
  (define-x-instruction      lhzx  31 279)
  (define-x-instruction      lhzux 31 311 :other-dependencies ((writes ra)))
  ;; Halfword sign-extend    ing
  (define-d-instruction      lha   42)
  (define-d-instruction      lhau  43 :other-dependencies ((writes ra)))
  (define-x-instruction      lhax  31 343)
  (define-x-instruction      lhaux 31 375 :other-dependencies ((writes ra)))
  ;; Word zero-extending)
  (define-instruction lwz (segment rt ra si)
    (:declare (type (or (signed-byte 16) label fixup) si))
    (:printer d ((op 32)))
    (:delay 2)
    (:cost 2)
    (:dependencies (reads ra) (writes rt) (reads :memory))
    (:emitter
     (patchable-emit-d-form segment 32 (reg-tn-encoding rt)
                            (reg-or-0 ra) si)))
  (define-d-instruction      lwzu  33 :other-dependencies ((writes ra)))
  (define-x-instruction      lwzx  31 23)
  (define-x-instruction      lwzux 31 55 :other-dependencies ((writes ra)))
  ;; Sign-extending
  (define-ds-instruction     lwa   58 #b10)
  (define-x-instruction      lwax  31 341)
  (define-x-instruction      lwaux 31 373 :other-dependencies ((writes ra)))
  ;; Doubleword
  ;;(define-ds-instruction     ld    58 #b00)
  (define-instruction ld (segment rt ra si)
    (:declare (type (or (signed-byte 16) label fixup) si))
    (:printer ds ((op 58) (subop 0)))
    (:emitter
     (patchable-emit-ds-form segment 58 (reg-tn-encoding rt) (reg-or-0 ra) si 0)))
  (define-ds-instruction     ldu   58 #b01)
  (define-x-instruction      ldx   31 21)
  (define-x-instruction      ldux  31 53)
  ;; Quadword
  ;; (not done yet)

  ;;; Stores
  ;; Byte
  (define-d-rs-instruction   stb   38)
  (define-d-rs-instruction   stbu  39 :other-dependencies ((writes ra)))
  (define-x-5-st-instruction stbx  31 215 0)
  (define-x-5-st-instruction stbux 31 247 0 :other-dependencies ((writes ra)))
  ;; Halfword
  (define-d-rs-instruction   sth   44)
  (define-d-rs-instruction   sthu  45 :other-dependencies ((writes ra)))
  (define-x-5-st-instruction sthx  31 407 0)
  (define-x-5-st-instruction sthux 31 439 0 :other-dependencies ((writes ra)))
  ;; Word
  (define-d-rs-instruction   stw   36)
  (define-d-rs-instruction   stwu  37 :other-dependencies ((writes ra)))
  (define-x-5-st-instruction stwx  31 151 0)
  (define-x-5-st-instruction stwux 31 183 0 :other-dependencies ((writes ra)))
  ;; Doubleword
  (define-ds-instruction     std   62 #b00)
  (define-ds-instruction     stdu  62 #b01) ; :other-dependencies ((writes ra))
  (define-x-5-st-instruction stdx  31 149 0)
  (define-x-5-st-instruction stdux 31 181 0 :other-dependencies ((writes ra)))
  ;; Quadword
  ;; (not done yet)

  ;; Byte-reversed
  (define-x-instruction      lhbrx  31 790)
  (define-x-instruction      lwbrx  31 534)
  (define-x-instruction      ldbrx  31 532)
  (define-x-5-st-instruction sthbrx 31 918 0)
  (define-x-5-st-instruction stwbrx 31 662 0)
  (define-x-5-st-instruction stdbrx 31 660 0)

  ;; Multiword
  (define-d-instruction      lmw 46 :pinned t)
  (define-d-rs-instruction   stmw 47 :pinned t)

  ;; Load and reserve, store conditional
  (define-x-instruction      lbarx 31 52)
  (define-x-instruction      lharx 31 116)
  (define-x-instruction      lwarx 31 20)
  (define-x-instruction      ldarx 31 84)
  (define-x-5-st-instruction stbcx. 31 694 1 :other-dependencies ((writes :ccr)))
  (define-x-5-st-instruction sthcx. 31 726 1 :other-dependencies ((writes :ccr)))
  (define-x-5-st-instruction stwcx. 31 150 1 :other-dependencies ((writes :ccr)))
  (define-x-5-st-instruction stdcx. 31 214 1 :other-dependencies ((writes :ccr)))

  ;; Floating-point load/store
  (define-d-frt-instruction   lfs    48)
  (define-d-frt-instruction   lfsu   49 :other-dependencies ((writes ra)))
  (define-d-frt-instruction   lfd    50)
  (define-d-frt-instruction   lfdu   51 :other-dependencies ((writes ra)))
  (define-x-20-instruction    lfsx   31 535)
  (define-x-20-instruction    lfsux  31 567 :other-dependencies ((writes ra)))
  (define-x-20-instruction    lfdx   31 599)
  (define-x-20-instruction    lfdux  31 631 :other-dependencies ((writes ra)))

  (define-d-frs-instruction   stfs   52)
  (define-d-frs-instruction   stfsu  53 :other-dependencies ((writes ra)))
  (define-d-frs-instruction   stfd   54)
  (define-d-frs-instruction   stfdu  55 :other-dependencies ((writes ra)))
  (define-x-23-st-instruction stfsx  31 663)
  (define-x-23-st-instruction stfsux 31 695 :other-dependencies ((writes ra)))
  (define-x-23-st-instruction stfdx  31 727)
  (define-x-23-st-instruction stfdux 31 759 :other-dependencies ((writes ra)))
  ;; Todo: lfiwax lfiwzx stfiwx
  ;;       lfdp stfdp lfdpx stfdpx
  )

(macrolet ((define-xo-instruction
               (name op xo oe-p rc-p always-reads-xer always-writes-xer cost)
               `(define-instruction ,name (segment rt ra rb)
                 (:printer xo ((op ,op ) (xo ,xo) (oe ,(if oe-p 1 0)) (rc ,(if rc-p 1 0))))
                 (:dependencies (reads ra) (reads rb) ,@(if always-reads-xer '((reads :xer)))
                  (writes rt) ,@(if rc-p '((writes :ccr))) ,@(if (or oe-p always-writes-xer) '((writes :xer))) )
                 (:cost ,cost)
                 (:delay ,cost)
                 (:emitter
                  (emit-xo-form-inst segment ,op
                   (reg-tn-encoding rt)
                   (reg-tn-encoding ra)
                   (reg-tn-encoding rb)
                   ,(if oe-p 1 0)
                   ,xo
                   ,(if rc-p 1 0)))))
           (define-xo-oe-instruction
               (name op xo rc-p always-reads-xer always-writes-xer cost)
               `(define-instruction ,name (segment rt ra rb)
                 (:printer xo-oe ((op ,op) (xo ,xo) (rc ,(if rc-p 1 0))))
                 (:dependencies (reads ra) (reads rb) ,@(if always-reads-xer '((reads :xer)))
                  (writes rt) ,@(if rc-p '((writes :ccr))) ,@(if always-writes-xer '((writes :xer))))
                 (:cost ,cost)
                 (:delay ,cost)
                 (:emitter
                  (emit-xo-form-inst segment ,op
                   (reg-tn-encoding rt)
                   (reg-tn-encoding ra)
                   (reg-tn-encoding rb)
                   0
                   ,xo
                   (if ,rc-p 1 0)))))
           (define-4-xo-instructions
               (base op xo &key always-reads-xer always-writes-xer (cost 1))
               `(progn
                 (define-xo-instruction ,base ,op ,xo nil nil ,always-reads-xer ,always-writes-xer ,cost)
                 (define-xo-instruction ,(symbolicate base ".") ,op ,xo nil t ,always-reads-xer ,always-writes-xer ,cost)
                 (define-xo-instruction ,(symbolicate base "O") ,op ,xo t nil ,always-reads-xer ,always-writes-xer ,cost)
                 (define-xo-instruction ,(symbolicate base "O.") ,op ,xo t t ,always-reads-xer ,always-writes-xer ,cost)))

           (define-2-xo-oe-instructions (base op xo &key always-reads-xer always-writes-xer (cost 1))
               `(progn
                 (define-xo-oe-instruction ,base ,op ,xo nil ,always-reads-xer ,always-writes-xer ,cost)
                 (define-xo-oe-instruction ,(symbolicate base ".") ,op ,xo t ,always-reads-xer ,always-writes-xer ,cost)))

           (define-xo-a-instruction (name op xo oe-p rc-p always-reads-xer always-writes-xer cost)
               `(define-instruction ,name (segment rt ra)
                 (:printer xo-a ((op ,op) (xo ,xo) (rc ,(if rc-p 1 0)) (oe ,(if oe-p 1 0))))
                 (:dependencies (reads ra) ,@(if always-reads-xer '((reads :xer)))
                  (writes rt) ,@(if rc-p '((writes :ccr))) ,@(if always-writes-xer '((writes :xer))) )
                 (:cost ,cost)
                 (:delay ,cost)
                 (:emitter
                  (emit-xo-form-inst segment ,op
                   (reg-tn-encoding rt)
                   (reg-tn-encoding ra)
                   0
                   (if ,oe-p 1 0)
                   ,xo
                   (if ,rc-p 1 0)))))

           (define-4-xo-a-instructions (base op xo &key always-reads-xer always-writes-xer (cost 1))
               `(progn
                 (define-xo-a-instruction ,base ,op ,xo nil nil ,always-reads-xer ,always-writes-xer ,cost)
                 (define-xo-a-instruction ,(symbolicate base ".") ,op ,xo nil t ,always-reads-xer ,always-writes-xer ,cost)
                 (define-xo-a-instruction ,(symbolicate base "O")  ,op ,xo t nil ,always-reads-xer ,always-writes-xer ,cost)
                 (define-xo-a-instruction ,(symbolicate base "O.") ,op ,xo t t ,always-reads-xer ,always-writes-xer ,cost)))

           (define-x-5-instruction (name op xo rc-p &key (cost 1) other-dependencies)
               (multiple-value-bind (other-reads other-writes) (classify-dependencies other-dependencies)
                 `(define-instruction ,name (segment ra rs rb)
                   (:printer x-5 ((op ,op) (xo ,xo) (rc ,(if rc-p 1 0))))
                   (:delay ,cost)
                   (:cost ,cost)
                   (:dependencies (reads rb) (reads rs) ,@other-reads
                    (writes ra) ,@other-writes)
                   (:emitter
                    (emit-x-form-inst segment ,op
                     (reg-tn-encoding rs)
                     (reg-tn-encoding ra)
                     (reg-tn-encoding rb)
                     ,xo
                     ,(if rc-p 1 0))))))

           (define-x-10-instruction (name op xo rc-p &key (cost 1) other-dependencies)
               (multiple-value-bind (other-reads other-writes) (classify-dependencies other-dependencies)
                 `(define-instruction ,name (segment ra rs)
                   (:printer x-10 ((op ,op) (xo ,xo) (rc ,(if rc-p 1 0))))
                   (:delay ,cost)
                   (:cost ,cost)
                   (:dependencies (reads rs) ,@other-reads
                    (writes ra) ,@other-writes)
                   (:emitter
                    (emit-x-form-inst segment ,op
                     (reg-tn-encoding rs)
                     (reg-tn-encoding ra)
                     0
                     ,xo
                     ,(if rc-p 1 0))))))

           (define-2-x-5-instructions (name op xo &key (cost 1) other-dependencies)
               `(progn
                 (define-x-5-instruction ,name ,op ,xo nil :cost ,cost :other-dependencies ,other-dependencies)
                 (define-x-5-instruction ,(symbolicate name ".") ,op ,xo t :cost ,cost
                                         :other-dependencies ,other-dependencies)))

           (define-2-x-10-instructions (name op xo &key (cost 1) other-dependencies)
               `(progn
                 (define-x-10-instruction ,name ,op ,xo nil :cost ,cost :other-dependencies ,other-dependencies)
                 (define-x-10-instruction ,(symbolicate name ".") ,op ,xo t :cost ,cost
                                          :other-dependencies ,other-dependencies)))


           (define-x-21-instruction (name op xo rc-p &key (cost 4) other-dependencies)
               (multiple-value-bind (other-reads other-writes) (classify-dependencies other-dependencies)
                 `(define-instruction ,name (segment frt frb)
                   ;; (:printer x-21 ((op ,op) (xo ,xo) (rc ,(if rc-p 1 0))))
                   (:cost ,cost)
                   (:delay ,cost)
                   (:dependencies (reads frb) ,@other-reads
                    (writes frt) ,@other-writes)
                   (:emitter
                    (emit-x-form-inst segment ,op
                     (fp-reg-tn-encoding frt)
                     0
                     (fp-reg-tn-encoding frb)
                     ,xo
                     ,(if rc-p 1 0))))))

           (define-2-x-21-instructions (name op xo &key (cost 4) other-dependencies)
               `(progn
                 (define-x-21-instruction ,name ,op ,xo nil :cost ,cost :other-dependencies ,other-dependencies)
                 (define-x-21-instruction ,(symbolicate name ".") ,op ,xo t :cost ,cost
                                          :other-dependencies ,other-dependencies)))


           (define-d-si-instruction (name op &key (fixup nil) (cost 1) other-dependencies)
             (binding* (((other-reads other-writes) (classify-dependencies other-dependencies))
                        (allow-r0 (not (member name '(addi addis)))))
                 `(define-instruction ,name (segment rt ra si)
                   (:declare (type (or ,@(when fixup '(fixup))
                                       (unsigned-byte 16) (signed-byte 16))
                                   si))
                   (:printer d-si ((op ,op)))
                   (:delay ,cost)
                   (:cost ,cost)
                   (:dependencies (reads ra) ,@other-reads
                    (writes rt) ,@other-writes)
                   (:emitter
                    ;; To compute the address of a jump-table label in the unboxed code data,
                    ;; we add LIP := CODE-TN + imm. The 'imm' must fit in a (signed-byte 16)
                    ;; because we're only using the ADDI instruction, not also ADDIS.
                    ;; It might be better if CODE-TN pointed to CODE-INSTRUCTIONS rather than
                    ;; the base of the code so that boxed constants could be referenced
                    ;; using negative offsets and unboxed constants with positive.
                    ;; This probably works already, with perhaps a minor alteration
                    ;; to plausible_base_register(), maybe not even that.
                    ;; As long as LIP is greater than CODE-TN, we should be OK.
                    ;; And of course it would be best if we never moved on-stack code at all.
                    ,@(when (eq fixup :l)
                        `((when (and (typep si 'fixup) (label-p (fixup-offset si)))
                            (return-from ,name
                             (emit-back-patch
                              segment 4
                              (lambda (segment posn)
                                (declare (ignore posn))
                                (let ((offset-from-code-tn
                                       (+ (component-header-length)
                                          (- (segment-header-skew segment))
                                          (- code-tn-lowtag)
                                          (label-position (fixup-offset si)))))
                                  (unless (typep offset-from-code-tn '(signed-byte 16))
                                    (bug "Jump table offset overflow"))
                                  (emit-d-form-inst
                                   segment ,op (reg-tn-encoding rt)
                                   ,(if allow-r0 '(reg-tn-encoding ra) '(reg-or-0 ra))
                                   offset-from-code-tn))))))
                          (when (and (typep si 'fixup) (eq (fixup-flavor si) :linkage-cell))
                            (note-fixup segment :addis+ld si)
                            (setq si 0))))
                    (when (typep si 'fixup)
                      (ecase ,fixup
                        ((:ha :l) (note-fixup segment ,fixup si)))
                      (setq si (fixup-offset si)))
                    (emit-d-form-inst segment ,op (reg-tn-encoding rt)
                                      ,(if allow-r0 '(reg-tn-encoding ra) '(reg-or-0 ra))
                                      si)))))

           (define-d-rs-ui-instruction (name op &key (cost 1) other-dependencies)
               (multiple-value-bind (other-reads other-writes) (classify-dependencies other-dependencies)
                 `(define-instruction ,name (segment ra rs ui)
                   (:declare (type (unsigned-byte 16) ui))
                   (:printer d-rs-ui ((op ,op)))
                   (:cost ,cost)
                   (:delay ,cost)
                   (:dependencies (reads rs) ,@other-reads
                    (writes ra) ,@other-writes)
                   (:emitter
                    (emit-d-form-inst segment ,op (reg-tn-encoding rs) (reg-tn-encoding ra) ui)))))

           (define-a-instruction (name op xo rc &key (cost 1) other-dependencies)
               `(define-instruction ,name (segment frt fra frb frc)
                 (:printer a ((op ,op) (xo ,xo) (rc ,rc)))
                 (:cost ,cost)
                 (:delay ,cost)
                 (:dependencies (writes frt) (reads fra) (reads frb) (reads frc) ,@other-dependencies)
                 (:emitter
                  (progn frc) ; unused sometimes. Why?
                  (emit-a-form-inst segment
                   ,op
                   (fp-reg-tn-encoding frt)
                   (fp-reg-tn-encoding fra)
                   (fp-reg-tn-encoding frb)
                   (fp-reg-tn-encoding frb)
                   ,xo
                   ,rc))))

           (define-2-a-instructions (name op xo &key (cost 1) other-dependencies)
               `(progn
                 (define-a-instruction ,name ,op ,xo 0 :cost ,cost :other-dependencies ,other-dependencies)
                 (define-a-instruction ,(symbolicate name ".")
                     ,op ,xo 1  :cost ,cost :other-dependencies ,other-dependencies)))

           (define-a-tab-instruction (name op xo rc &key (cost 1) other-dependencies)
               (multiple-value-bind (other-reads other-writes) (classify-dependencies other-dependencies)
                 `(define-instruction ,name (segment frt fra frb)
                   (:printer a-tab ((op ,op) (xo ,xo) (rc ,rc)))
                   (:cost ,cost)
                   (:delay 1)
                   (:dependencies (reads fra) (reads frb) ,@other-reads
                    (writes frt) ,@other-writes)
                   (:emitter
                    (emit-a-form-inst segment
                     ,op
                     (fp-reg-tn-encoding frt)
                     (fp-reg-tn-encoding fra)
                     (fp-reg-tn-encoding frb)
                     0
                     ,xo
                     ,rc)))))

           (define-2-a-tab-instructions (name op xo &key (cost 1) other-dependencies)
               `(progn
                 (define-a-tab-instruction ,name ,op ,xo 0 :cost ,cost :other-dependencies ,other-dependencies)
                 (define-a-tab-instruction ,(symbolicate name ".")
                     ,op ,xo 1  :cost ,cost :other-dependencies ,other-dependencies)))

           (define-a-tac-instruction (name op xo rc &key (cost 1) other-dependencies)
               (multiple-value-bind (other-reads other-writes) (classify-dependencies other-dependencies)
                 `(define-instruction ,name (segment frt fra frb)
                   (:printer a-tac ((op ,op) (xo ,xo) (rc ,rc)))
                   (:cost ,cost)
                   (:delay 1)
                   (:dependencies (reads fra) (reads frb) ,@other-reads
                    (writes frt) ,@other-writes)
                   (:emitter
                    (emit-a-form-inst segment
                     ,op
                     (fp-reg-tn-encoding frt)
                     (fp-reg-tn-encoding fra)
                     0
                     (fp-reg-tn-encoding frb)
                     ,xo
                     ,rc)))))

           (define-2-a-tac-instructions (name op xo &key (cost 1) other-dependencies)
               `(progn
                 (define-a-tac-instruction ,name ,op ,xo 0 :cost ,cost :other-dependencies ,other-dependencies)
                 (define-a-tac-instruction ,(symbolicate name ".")
                     ,op ,xo 1  :cost ,cost :other-dependencies ,other-dependencies)))

           (define-crbit-instruction (name op xo)
               `(define-instruction ,name (segment dbit abit bbit)
                 (:printer xl ((op ,op ) (xo ,xo)))
                 (:delay 1)
                 (:cost 1)
                 (:dependencies (reads :ccr) (writes :ccr))
                 (:emitter (emit-x-form-inst segment 19
                            (valid-bi-encoding dbit)
                            (valid-bi-encoding abit)
                            (valid-bi-encoding bbit)
                            ,xo
                            0)))))

   ;;; The instructions, in numerical order
  #+64-bit
  (define-instruction unimp (segment data)
    (:declare (type (signed-byte 16) data))
    (:printer xinstr ((op-to-a #.(logior (ash 2 10) (ash 1 5) null-offset)))
              :default :control #'unimp-control)
    :pinned
    (:delay 0)
    (:emitter
     #.(assert (> nil-value (1- (expt 2 16))))
     ;; TDI LGT,$NULL,data
     (emit-d-form-inst segment 2 1 null-offset data)))
  #-64-bit
  (define-instruction unimp (segment data)
    (:declare (type (signed-byte 16) data))
    (:printer xinstr ((op-to-a #.(logior (ash 3 10) (ash 6 5) 0)))
              :default :control #'unimp-control)
    :pinned
    (:delay 0)
    (:emitter (emit-d-form-inst segment 3 6 0 data)))

  (define-instruction tdi (segment tcond ra si)
    (:printer d-to ((op 2)))
    (:delay 0)
    :pinned
    (:emitter (emit-d-form-inst segment 2 (valid-tcond-encoding tcond) (reg-tn-encoding ra) si)))
  (define-instruction twi (segment tcond ra si)
    (:printer d-to ((op 3)))
    (:delay 0)
    :pinned
    (:emitter (emit-d-form-inst segment 3 (valid-tcond-encoding tcond) (reg-tn-encoding ra) si)))

  (define-instruction isel (segment rt ra rb bc)
    (:emitter
     (emit-a-form-inst segment 31
                       (reg-tn-encoding rt)
                       (reg-tn-encoding ra)
                       (reg-tn-encoding rb)
                       ;; BC encoding is the same as BI encoding, I think.
                       (valid-bi-encoding bc) 15 0)))

  (define-d-si-instruction mulli 7 :cost 5)
  (define-d-si-instruction subfic 8)

  (macrolet ((def (mnemonic op L fmt imm-arg-field)
               `(define-instruction ,mnemonic (segment crf ra &optional (imm nil imm-p))
                  (:printer ,fmt ((op ,op) (l ,L)) '(:name :tab bf "," ra "," ,imm-arg-field))
                  (:dependencies (if imm-p (reads ra) (reads crf)) (writes :ccr))
                  (:delay 1)
                  (:emitter
                   (unless imm-p
                     (setq imm ra ra crf crf :cr0))
                   (emit-d-form-inst segment
                                     ,op
                                     (logior (valid-cr-field-encoding crf) ,L)
                                     (reg-tn-encoding ra)
                                     (ldb (byte 16 0) imm))))))
    (def cmpwi  11 0 d-crf-si si)
    (def cmpdi  11 1 d-crf-si si)
    (def cmplwi 10 0 d-crf-ui ui)
    (def cmpldi 10 1 d-crf-ui ui))

  (define-d-si-instruction addic 12 :other-dependencies ((writes :xer)))
  (define-d-si-instruction addic. 13 :other-dependencies ((writes :xer) (writes :ccr)))

  (define-d-si-instruction addi 14 :fixup :l)
  (define-d-si-instruction addis 15 :fixup :ha)

  ;; There's no real support here for branch options that decrement
  ;; and test the CTR :
  ;; (a) the instruction scheduler doesn't know that anything's happening
  ;;    to the CTR
  ;; (b) Lisp may have to assume that the CTR always has a lisp
  ;;    object/locative in it.

  (define-instruction bc (segment bo bi target)
    (:declare (type label target))
    (:printer b ((op 16) (aa 0) (lk 0)))
    (:attributes branch)
    (:delay 0)
    (:dependencies (reads :ccr))
    (:emitter
     (emit-conditional-branch segment bo bi target)))

  (define-instruction bcl (segment bo bi target)
    (:declare (type label target))
    (:printer b ((op 16) (aa 0) (lk 1)))
    (:attributes branch)
    (:delay 0)
    (:dependencies (reads :ccr))
    (:emitter
     (emit-conditional-branch segment bo bi target nil t)))

  (define-instruction bca (segment bo bi target)
    (:declare (type label target))
    (:printer b ((op 16) (aa 1) (lk 0)))
    (:attributes branch)
    (:delay 0)
    (:dependencies (reads :ccr))
    (:emitter
     (emit-conditional-branch segment bo bi target t)))

  (define-instruction bcla (segment bo bi target)
    (:declare (type label target))
    (:printer b ((op 16) (aa 1) (lk 1)))
    (:attributes branch)
    (:delay 0)
    (:dependencies (reads :ccr))
    (:emitter
     (emit-conditional-branch segment bo bi target t t)))

;;; There may (or may not) be a good reason to use this in preference
;;; to "b[la] target".  I can't think of a -bad- reason ...

  (define-instruction bu (segment target)
    (:declare (type label target))
    (:printer b ((op 16) (bo (valid-bo-encoding :bo-u)) (bi 0) (aa 0) (lk 0))
              '(:name :tab bd))
    (:attributes branch)
    (:delay 0)
    (:emitter
     (emit-conditional-branch segment (valid-bo-encoding :bo-u) 0 target nil nil)))


  (define-instruction bt (segment bi  target)
    (:printer b ((op 16) (bo (valid-bo-encoding :bo-t)) (aa 0) (lk 0))
              '(:name :tab bi "," bd))
    (:attributes branch)
    (:delay 0)
    (:emitter
     (emit-conditional-branch segment (valid-bo-encoding :bo-t) bi target nil nil)))

  (define-instruction bf (segment bi  target)
    (:printer b ((op 16) (bo (valid-bo-encoding :bo-f)) (aa 0) (lk 0))
              '(:name :tab bi "," bd))
    (:attributes branch)
    (:delay 0)
    (:emitter
     (emit-conditional-branch segment (valid-bo-encoding :bo-f) bi target nil nil)))

  (define-instruction b? (segment cr-field-name cr-name  &optional (target nil target-p))
    (:attributes branch)
    (:delay 0)
    (:emitter
     (unless target-p
       (setq target cr-name cr-name cr-field-name cr-field-name :cr0))
     (let*  ((+cond (position cr-name cr-bit-names))
             (-cond (position cr-name cr-bit-inverse-names))
             (b0 (if +cond :bo-t
                     (if -cond
                         :bo-f
                         (error "Unknown branch condition ~s" cr-name))))
             (cr-form (list cr-field-name (if +cond cr-name (svref cr-bit-names -cond)))))
       (emit-conditional-branch segment b0 cr-form target))))

  (define-instruction sc (segment)
    (:printer sc ((op 17)))
    (:attributes branch)
    (:delay 0)
    :pinned
    (:emitter (emit-sc-form-inst segment 17 2)))

  (define-instruction b (segment target)
    (:printer i ((op 18) (aa 0) (lk 0)))
    (:attributes branch)
    (:delay 0)
    (:emitter
     (emit-i-form-branch segment target nil)))

  (define-instruction ba (segment target)
    (:printer i-abs ((op 18) (aa 1) (lk 0)))
    (:attributes branch)
    (:delay 0)
    (:emitter
     (when (typep target 'fixup)
       (note-fixup segment :ba target)
       (setq target 0))
     (emit-i-form-inst segment 18 (ash target -2) 1 0)))


  (define-instruction bl (segment target)
    (:printer i ((op 18) (aa 0) (lk 1)))
    (:attributes branch)
    (:delay 0)
    (:emitter
     (emit-i-form-branch segment target t)))

  (define-instruction bla (segment target)
    (:printer i-abs ((op 18) (aa 1) (lk 1)))
    (:attributes branch)
    (:delay 0)
    (:emitter
     (when (typep target 'fixup)
       (note-fixup segment :ba target)
       (setq target 0))
     (emit-i-form-inst segment 18 (ash target -2) 1 1)))

  (define-instruction blr (segment)
    (:printer xl-bo-bi ((op 19) (xo 16) (bo (valid-bo-encoding :bo-u))(bi 0) (lk 0))  '(:name))
    (:attributes branch)
    (:delay 0)
    (:dependencies (reads :ccr) (reads :ctr))
    (:emitter
     (emit-x-form-inst segment 19 (valid-bo-encoding :bo-u) 0 0 16 0)))

  (define-instruction bclr (segment bo bi)
    (:printer xl-bo-bi ((op 19) (xo 16)))
    (:attributes branch)
    (:delay 0)
    (:dependencies (reads :ccr) (reads :lr))
    (:emitter
     (emit-x-form-inst segment 19 (valid-bo-encoding bo) (valid-bi-encoding bi) 0 16 0)))

  (define-instruction bclrl (segment bo bi)
    (:printer xl-bo-bi ((op 19) (xo 16) (lk 1)))
    (:attributes branch)
    (:delay 0)
    (:dependencies (reads :ccr) (reads :lr))
    (:emitter
     (emit-x-form-inst segment 19 (valid-bo-encoding bo)
                       (valid-bi-encoding bi) 0 16 1)))

  (define-crbit-instruction crnor 19 33)
  (define-crbit-instruction crandc 19 129)
  (define-instruction isync (segment)
    (:printer xl-xo ((op 19) (xo 150)))
    (:delay 1)
    :pinned
    (:emitter (emit-x-form-inst segment 19 0 0 0 150 0)))

  (define-crbit-instruction crxor 19 193)
  (define-crbit-instruction crnand 19 225)
  (define-crbit-instruction crand 19 257)
  (define-crbit-instruction creqv 19 289)
  (define-crbit-instruction crorc 19 417)
  (define-crbit-instruction cror 19 449)

  (define-instruction bcctr (segment bo bi)
    (:printer xl-bo-bi ((op 19) (xo 528)))
    (:attributes branch)
    (:delay 0)
    (:dependencies (reads :ccr) (reads :ctr))
    (:emitter
     (emit-x-form-inst segment 19 (valid-bo-encoding bo) (valid-bi-encoding bi) 0 528 0)))

  (define-instruction bcctrl (segment bo bi)
    (:printer xl-bo-bi ((op 19) (xo 528) (lk 1)))
    (:attributes branch)
    (:delay 0)
    (:dependencies (reads :ccr) (reads :ctr) (writes :lr))
    (:emitter
     (emit-x-form-inst segment 19 (valid-bo-encoding bo) (valid-bi-encoding bi) 0 528 1)))

  (define-instruction bctr (segment)
    (:printer xl-bo-bi ((op 19) (xo 528) (bo (valid-bo-encoding :bo-u)) (bi 0) (lk 0))  '(:name))
    (:attributes branch)
    (:delay 0)
    (:dependencies (reads :ccr) (reads :ctr))
    (:emitter
     (emit-x-form-inst segment 19 (valid-bo-encoding :bo-u) 0 0  528 0)))

  (define-instruction bctrl (segment)
    (:printer xl-bo-bi ((op 19) (xo 528) (bo (valid-bo-encoding :bo-u)) (bi 0) (lk 1))  '(:name))
    (:attributes branch)
    (:delay 0)
    (:dependencies (reads :ccr) (reads :ctr))
    (:emitter
     (emit-x-form-inst segment 19 (valid-bo-encoding :bo-u) 0 0  528 1)))

  (macrolet ((def (name op rc)
               `(define-instruction ,name (segment ra rs sh mb me)
                  (:printer m-sh ((op ,op) (rc ,rc)))
                  (:dependencies (reads rs) (writes ra))
                  (:delay 1)
                  (:emitter
                   (emit-a-form-inst segment ,op (reg-tn-encoding rs) (reg-tn-encoding ra)
                                     sh mb me ,rc)))))
    (def rlwinm  21 0) ; Rotate Left Word Immediate then AND with Mask
    (def rlwinm. 21 1)
    (def rlwimi  20 0) ; Rotate Left Word Immediate then Mask Insert
    (def rlwimi. 20 1))
  (macrolet ((def (name op rc)
               `(define-instruction ,name (segment ra rs rb mb me)
                  (:printer m ((op ,op) (rc ,rc) (rb nil :type 'reg)))
                  (:delay 1)
                  (:dependencies (reads rs) (writes ra) (reads rb))
                  (:emitter
                   (emit-a-form-inst segment ,op (reg-tn-encoding rs) (reg-tn-encoding ra)
                                     (reg-tn-encoding rb) mb me ,rc)))))
    (def rlwnm  23 0) ; Rotate Left Word then AND with Mask
    (def rlwnm. 23 1))

  (define-bitfield-emitter emit-md-form-inst 32
    (byte 6 26) (byte 5 21) (byte 5 16) (byte 5 11) (byte 6 5) (byte 3 2) (byte 1 1) (byte 1 0))
  (def-ppc-iformat (md-form '(:name :tab ra "," rs "," sh "," mask))
    rs ra (sh sh6) (mask mask6) (subop md-form-subop) rc)
  (defun unsplit-sh (dstate sh0-4 sh5)
    (declare (ignore dstate))
    (logior (ash sh5 5) sh0-4))
  (defun encode-mask6 (m)
    (logior (ash (ldb (byte 5 0) m) 1) (ldb (byte 1 5) m)))
  (defun decode-mask6 (dstate value)
    (declare (ignore dstate))
    (logior (ash (logand value 1) 5) (ash value -1)))

  (macrolet ((def (mnemonic op Rc)
               `(define-instruction ,mnemonic (segment ra rs sh m)
                  (:declare (type (integer 0 63) sh) (type (or (integer 0 63) fixup) m))
                  (:printer md-form ((op 30) (subop ,op) (rc ,rc)))
                  (:emitter
                   (when (and (fixup-p m) (eq (fixup-flavor m) :card-table-index-mask))
                     (note-fixup segment :rldic-m m)
                     (setq m 0))
                   (emit-md-form-inst segment 30
                                      (reg-tn-encoding rs) (reg-tn-encoding ra)
                                      (ldb (byte 5 0) sh)
                                      (encode-mask6 m)
                                      ,op (ldb (byte 1 5) sh) ,rc)))))
    (def rldicl  0 0) ; Rotate Left Doubleword Immediate then Clear Left
    (def rldicl. 0 1)
    (def rldicr  1 0) ; Rotate Left Doubleword Immediate then Clear Right
    (def rldicr. 1 1)
    (def rldic   2 0) ; Rotate Left Doubleword Immediate then Clear
    (def rldic.  2 1))

  (define-bitfield-emitter emit-mds-form-inst 32
    (byte 6 26) (byte 5 21) (byte 5 16) (byte 5 11) (byte 6 5) (byte 4 1) (byte 1 0))
  (def-ppc-iformat (mds-form '(:name :tab ra "," rs "," rb "," mask))
    rs ra rb (mask mask6) (subop mds-form-subop) rc)
  (macrolet ((def (mnemonic op Rc)
               `(define-instruction ,mnemonic (segment ra rs rb m)
                  (:printer mds-form ((op 30) (subop ,op) (rc ,rc)))
                  (:emitter
                   (emit-mds-form-inst segment 30 (reg-tn-encoding rs)
                                       (reg-tn-encoding ra) (reg-tn-encoding rb)
                                       (encode-mask6 m) ,op ,rc)))))
    (def rldcl  8 0) ; Rotate Left Doubleword then Clear Left
    (def rldcl. 8 1))

  (define-d-rs-ui-instruction ori 24)

  (define-instruction nop (segment)
    (:printer d-rs-ui ((op 24) (rs 0) (ra 0) (ui 0)) '(:name))
    (:cost 1)
    (:delay 1)
    (:emitter
     (emit-d-form-inst segment 24 0 0 0)))

  (define-d-rs-ui-instruction oris 25)
  (define-d-rs-ui-instruction xori 26)
  (define-d-rs-ui-instruction xoris 27)
  (define-d-rs-ui-instruction andi. 28 :other-dependencies ((writes :ccr)))
  (define-d-rs-ui-instruction andis. 29 :other-dependencies ((writes :ccr)))

  (macrolet ((def (mnemonic L XO)
               `(define-instruction ,mnemonic (segment crf ra  &optional (rb nil rb-p))
                  (:printer x-14 ((op 31) (xo ,xo) (l ,L)) '(:name :tab bf "," ra "," rb))
                  (:delay 1)
                  (:dependencies (reads ra) (if rb-p (reads rb) (reads crf)) (reads :xer) (writes :ccr))
                  (:emitter
                   (unless rb-p
                     (setq rb ra ra crf crf :cr0))
                   (emit-x-form-inst segment
                                     31
                                     (logior (valid-cr-field-encoding crf) ,L)
                                     (reg-tn-encoding ra)
                                     (reg-tn-encoding rb)
                                     ,xo
                                     0)))))
    (def cmpw 0 0)
    (def cmpd 1 0)
    (def cmplw 0 32)
    (def cmpld 1 32))

  (define-instruction tw (segment tcond ra rb)
    (:printer x-19 ((op 31) (xo 4)))
    (:attributes branch)
    (:delay 0)
    :pinned
    (:emitter (emit-x-form-inst segment 31 (valid-tcond-encoding tcond) (reg-tn-encoding ra) (reg-tn-encoding rb) 4 0)))
  (define-instruction td (segment tcond ra rb)
    (:printer x-19 ((op 31) (xo 68)))
    (:attributes branch)
    (:delay 0)
    :pinned
    (:emitter (emit-x-form-inst segment 31 (valid-tcond-encoding tcond) (reg-tn-encoding ra) (reg-tn-encoding rb) 68 0)))

  (define-4-xo-instructions subfc 31 8 :always-writes-xer t)
  (define-4-xo-instructions addc 31 10 :always-writes-xer t)
  (define-2-xo-oe-instructions mulhdu 31 9 :cost 5)
  (define-2-xo-oe-instructions mulhwu 31 11 :cost 5)

  (define-instruction mfcr (segment rd)
    (:printer x-4 ((op 31) (xo 19)))
    (:delay 1)
    (:dependencies (reads :ccr) (writes rd))
    (:emitter (emit-x-form-inst segment 31 (reg-tn-encoding rd) 0 0 19 0)))

  ;; This arrangement (or lack thereof) is an utter mess.
  ;; Should it be alphabetical by mnemonic, or grouped by the lower bits
  ;; of the subcode, or by the instruction format, or by the page number
  ;; in the architecture reference manual, or random, or all of the above?
  ;; This instruction set file is actually less understandable than x86-64
  ;; given the propensity to name fields (such as 'xo') so cryptically.

  (define-2-x-5-instructions slw 31 24)
  (define-2-x-5-instructions sld 31 27) ; (logical) shift left doubleword
  (define-2-x-5-instructions srd 31 539) ; (logical) shift right doubleword
  (define-2-x-5-instructions srad 31 794) ; shift right algebraic doubleword
  (define-2-x-10-instructions cntlzw 31 26)
  (define-2-x-10-instructions cntlzd 31 58)
  (define-x-10-instruction popcntd 31 506 nil)
  (define-2-x-5-instructions and 31 28)

  (define-4-xo-instructions subf 31 40)
                                        ; dcbst
  (define-2-x-5-instructions andc 31 60)
  (define-2-xo-oe-instructions mulhd 31 73 :cost 5)
  (define-2-xo-oe-instructions mulhw 31 75 :cost 5)

  (define-4-xo-a-instructions neg 31 104)
  (define-2-x-5-instructions nor 31 124)
  (define-4-xo-instructions subfe 31 136 :always-reads-xer t :always-writes-xer t)

  (define-instruction-macro sube (rt ra rb)
    `(inst subfe ,rt ,rb ,ra))

  (define-instruction-macro sube. (rt ra rb)
    `(inst subfe. ,rt ,rb ,ra))

  (define-instruction-macro subeo (rt ra rb)
    `(inst subfeo ,rt ,rb ,ra))

  (define-instruction-macro subeo. (rt ra rb)
    `(inst subfeo ,rt ,rb ,ra))

  (define-4-xo-instructions adde 31 138 :always-reads-xer t :always-writes-xer t)

  (define-instruction mtcrf (segment mask rt)
    (:printer xfx-fxm ((op 31) (xo 144)))
    (:delay 1)
    (:dependencies (reads rt) (writes :ccr))
    (:emitter (emit-xfx-form-inst segment 31 (reg-tn-encoding rt) (ash mask 1) 144 0)))

  (define-4-xo-a-instructions subfze 31 200 :always-reads-xer t :always-writes-xer t)
  (define-4-xo-a-instructions addze 31 202 :always-reads-xer t :always-writes-xer t)
  (define-4-xo-a-instructions subfme 31 232 :always-reads-xer t :always-writes-xer t)
  (define-4-xo-a-instructions addme 31 234 :always-reads-xer t :always-writes-xer t)
  (define-4-xo-instructions mulld 31 233 :cost 5)
  (define-4-xo-instructions mullw 31 235 :cost 5)
  (define-4-xo-instructions add 31 266)
  (define-2-x-5-instructions eqv 31 284)
  (define-2-x-5-instructions xor 31 316)

  ;; This instruction existed on processor model 601; it is the same as 'mfspr'
  ;; with a spr encoding that is now an illegal one.
  (define-instruction mfmq (segment rt)
    (:printer xfx ((op 31) (xo 339) (spr 0)) '(:name :tab rt))
    (:delay 1)
    (:dependencies (reads :xer) (writes rt))
    (:emitter (emit-xfx-form-inst segment 31 (reg-tn-encoding rt) (ash 0 5) 339 0)))

  (define-instruction mfxer (segment rt)
    (:printer xfx ((op 31) (xo 339) (spr 1)) '(:name :tab rt))
    (:delay 1)
    (:dependencies (reads :xer) (writes rt))
    (:emitter (emit-xfx-form-inst segment 31 (reg-tn-encoding rt) (ash 1 5) 339 0)))

  (define-instruction mflr (segment rt)
    (:printer xfx ((op 31) (xo 339) (spr 8)) '(:name :tab rt))
    (:delay 1)
    (:dependencies (reads :lr) (writes rt))
    (:emitter (emit-xfx-form-inst segment 31 (reg-tn-encoding rt) (ash 8 5) 339 0)))

  (define-instruction mfctr (segment rt)
    (:printer xfx ((op 31) (xo 339) (spr 9)) '(:name :tab rt))
    (:delay 1)
    (:dependencies (reads rt) (reads :ctr))
    (:emitter (emit-xfx-form-inst segment 31 (reg-tn-encoding rt) (ash 9 5) 339 0)))

  (define-2-x-5-instructions orc 31 412)

  (define-instruction or (segment ra rs rb)
    (:printer x-5 ((op 31) (xo 444) (rc 0)) '((:cond
                                                ((rs :same-as rb) 'mr)
                                                (t :name))
                                              :tab
                                              ra "," rs
                                              (:unless (:same-as rs) "," rb)))
    (:delay 1)
    (:cost 1)
    (:dependencies (reads rb) (reads rs) (writes ra))
    (:emitter
     (emit-x-form-inst segment
                       31
                       (reg-tn-encoding rs)
                       (reg-tn-encoding ra)
                       (reg-tn-encoding rb)
                       444
                       0)))

  (define-instruction or. (segment ra rs rb)
    (:printer x-5 ((op 31) (xo 444) (rc 1)) '((:cond
                                                ((rs :same-as rb) 'mr.)
                                                (t :name))
                                              :tab
                                              ra "," rs
                                              (:unless (:same-as rs) "," rb)))
    (:delay 1)
    (:cost 1)
    (:dependencies (reads rb) (reads rs) (writes ra) (writes :ccr))
    (:emitter
     (emit-x-form-inst segment
                       31
                       (reg-tn-encoding rs)
                       (reg-tn-encoding ra)
                       (reg-tn-encoding rb)
                       444
                       1)))

  (define-instruction-macro mr (ra rs)
    `(inst or ,ra ,rs ,rs))

  (define-instruction-macro mr. (ra rs)
    `(inst or. ,ra ,rs ,rs))

  (define-4-xo-instructions div    31 331 :cost 36)
  (define-4-xo-instructions divw   31 491 :cost 36)
  (define-4-xo-instructions divwu  31 459 :cost 36)
  (define-4-xo-instructions divwe  31 427 :cost 36)
  (define-4-xo-instructions divweu 31 395 :cost 36)

  (define-4-xo-instructions divd   31 489 :cost 36)
  (define-4-xo-instructions divdu  31 457 :cost 36)
  (define-4-xo-instructions divde  31 425 :cost 36)
  (define-4-xo-instructions divdeu 31 393 :cost 36)

  (define-instruction mtmq (segment rt)
    (:printer xfx ((op 31) (xo 467) (spr (ash 0 5))) '(:name :tab rt))
    (:delay 1)
    (:dependencies (reads rt) (writes :xer))
    (:emitter (emit-xfx-form-inst segment 31 (reg-tn-encoding rt) (ash 0 5) 467 0)))

  (define-instruction mtxer (segment rt)
    (:printer xfx ((op 31) (xo 467) (spr (ash 1 5))) '(:name :tab rt))
    (:delay 1)
    (:dependencies (reads rt) (writes :xer))
    (:emitter (emit-xfx-form-inst segment 31 (reg-tn-encoding rt) (ash 1 5) 467 0)))

  (define-instruction mtlr (segment rt)
    (:printer xfx ((op 31) (xo 467) (spr (ash 8 5))) '(:name :tab rt))
    (:delay 1)
    (:dependencies (reads rt) (writes :lr))
    (:emitter (emit-xfx-form-inst segment 31 (reg-tn-encoding rt) (ash 8 5) 467 0)))

  (define-instruction mtctr (segment rt)
    (:printer xfx ((op 31) (xo 467) (spr (ash 9 5))) '(:name :tab rt))
    (:delay 1)
    (:dependencies (reads rt) (writes :ctr))
    (:emitter (emit-xfx-form-inst segment 31 (reg-tn-encoding rt) (ash 9 5) 467 0)))


  (define-2-x-5-instructions nand 31 476)

  (define-instruction mcrxr (segment crf)
    (:printer x-18 ((op 31) (xo 512)))
    (:delay 1)
    (:dependencies (reads :xer) (writes :ccr) (writes :xer))
    (:emitter (emit-x-form-inst segment 31 (valid-cr-field-encoding crf) 0 0 512 0)))

  (define-instruction lswx (segment rs ra rb)
    (:printer x ((op 31) (xo 533) (rc 0)))
    (:delay 1)
    :pinned
    (:cost 8)
    (:emitter
     (emit-x-form-inst segment 31 (reg-tn-encoding rs) (reg-or-0 ra) (reg-tn-encoding rb) 533 0)))

  (define-2-x-5-instructions srw 31 536)

  (define-instruction lswi (segment rt ra rb)
    (:printer x-1 ((op 31) (xo 597) (rc 0)))
    :pinned
    (:delay 8)
    (:cost 8)
    (:emitter (emit-x-form-inst segment 31 (reg-tn-encoding rt) (reg-or-0 ra) rb 597 0)))

  (define-instruction sync (segment)
    (:printer x-27 ((op 31) (xo 598)))
    (:delay 1)
    :pinned
    (:emitter (emit-x-form-inst segment 31 0 0 0 598 0)))

  (define-instruction stswx (segment rs ra rb)
    (:printer x-5 ((op 31) (xo 661)))
    :pinned
    (:cost 8)
    (:delay 1)
    (:emitter
     (emit-x-form-inst segment 31 (reg-tn-encoding rs) (reg-or-0 ra) (reg-tn-encoding rb) 661 0)))

  (define-instruction stswi (segment rs ra nb)
    (:printer x-8 ((op 31) (xo 725)))
    :pinned
    (:delay 1)
    (:emitter
     (emit-x-form-inst segment 31 (reg-tn-encoding rs) (reg-or-0 ra) nb 725 0)))

  (define-2-x-5-instructions sraw 31 792)

  (macrolet ((def-sraw (mnemonic xo rc)
               `(define-instruction ,mnemonic (segment ra rs sh)
                  (:printer x-9 ((op 31) (xo ,xo) (rc ,rc)))
                  (:cost 1)
                  (:delay 1)
                  (:dependencies (reads rs) (writes ra))
                  (:emitter
                   (emit-x-form-inst segment 31
                                     (reg-tn-encoding rs)
                                     (reg-tn-encoding ra)
                                     sh ,xo ,rc)))))
    (def-sraw srawi  824 0)
    (def-sraw srawi. 824 1))

  (define-bitfield-emitter emit-xs-form-inst 32
    (byte 6 26) (byte 5 21) (byte 5 16) (byte 5 11) (byte 9 2) (byte 1 1) (byte 1 0))
  (def-ppc-iformat (xs-form '(:name :tab ra "," rs "," sh))
    rs ra (sh sh6) (subop xs-form-subop) rc)
  (macrolet ((def-srad (mnemonic xo rc)
               `(define-instruction ,mnemonic (segment ra rs sh)
                  (:printer xs-form ((op 31) (subop ,xo) (rc ,rc)))
                  (:cost 1)
                  (:delay 1)
                  (:dependencies (reads rs) (writes ra))
                  (:emitter
                   (emit-xs-form-inst segment 31
                                      (reg-tn-encoding rs)
                                      (reg-tn-encoding ra)
                                      (ldb (byte 5 0) sh) ,xo
                                      (ldb (byte 1 5) sh) ,rc)))))
    (def-srad sradi  413 0)
    (def-srad sradi. 413 1))

  (define-instruction eieio (segment)
    (:printer x-27 ((op 31) (xo 854)))
    :pinned
    (:delay 1)
    (:emitter (emit-x-form-inst segment 31 0 0 0 854 0)))

  (define-2-x-10-instructions extsb 31 954)
  (define-2-x-10-instructions extsh 31 922)
                                        ; Whew.

  (define-2-a-tab-instructions fdivs 59 18 :cost 17)
  (define-2-a-tab-instructions fsubs 59 20)
  (define-2-a-tab-instructions fadds 59 21)
  (define-2-a-tac-instructions fmuls 59 25)
  (define-2-a-instructions fmsubs 59 28 :cost 4)
  (define-2-a-instructions fmadds 59 29 :cost 4)
  (define-2-a-instructions fnmsubs 59 30 :cost 4)
  (define-2-a-instructions fnmadds 59 31 :cost 4)

  (define-instruction fcmpu (segment crfd fra frb)
    (:printer x-15 ((op 63) (xo 0)))
    (:dependencies (reads fra) (reads frb) (reads :fpscr)
                   (writes :fpscr) (writes :ccr))
    (:cost 4)
    (:delay 4)
    (:emitter (emit-x-form-inst segment
                                63
                                (valid-cr-field-encoding crfd)
                                (fp-reg-tn-encoding fra)
                                (fp-reg-tn-encoding frb)
                                0
                                0)))


  (define-2-x-21-instructions frsp 63 12)
  (define-2-x-21-instructions fctiw 63 14)
  (define-2-x-21-instructions fctiwz 63 15)
  ;; floating convert with round double-precision to signed-doubleword
  (define-2-x-21-instructions fctid 63 814)
  ;; floating convert with truncate double-precision to signed-doubleword
  (define-2-x-21-instructions fctidz 63 815)
  ;; floating convert with round signed-doubleword to double-precision
  (define-2-x-21-instructions fcfid 63 846)
  ;; floating convert with round unsigned-doubleword to double-precision
  (define-2-x-21-instructions fcfidu 63 974)
  ;; floating convert with round signed-doubleword to single-precision
  (define-2-x-21-instructions fcfids 59 846)
  ;; floating convert with round unsigned-doubleword to single-precision
  (define-2-x-21-instructions fcfidus 59 974)

  (define-2-a-tab-instructions fdiv 63 18 :cost 31)
  (define-2-a-tab-instructions fsub 63 20)
  (define-2-a-tab-instructions fadd 63 21)
  (define-2-a-tac-instructions fmul 63 25 :cost 5)
  (define-2-a-instructions fmsub 63 28 :cost 5)
  (define-2-a-instructions fmadd 63 29 :cost 5)
  (define-2-a-instructions fnmsub 63 30 :cost 5)
  (define-2-a-instructions fnmadd 63 31 :cost 5)

  (define-instruction fcmpo (segment crfd fra frb)
    (:printer x-15 ((op 63) (xo 32)))
    (:dependencies (reads fra) (reads frb) (reads :fpscr)
                   (writes :fpscr) (writes :ccr))
    (:cost 4)
    (:delay 1)
    (:emitter (emit-x-form-inst segment
                                63
                                (valid-cr-field-encoding crfd)
                                (fp-reg-tn-encoding fra)
                                (fp-reg-tn-encoding frb)
                                32
                              0)))

  (define-2-x-21-instructions fneg 63 40)

  (define-2-x-21-instructions fmr 63 72)
  (define-2-x-21-instructions fnabs 63 136)
  (define-2-x-21-instructions fabs 63 264)

  (define-instruction mffs (segment frd)
  (:printer x-22 ((op 63)  (xo 583) (rc 0)))
  (:delay 1)
  (:dependencies (reads :fpscr) (writes frd))
  (:emitter (emit-x-form-inst segment
                          63
                          (fp-reg-tn-encoding frd)
                          0
                          0
                          583
                          0)))

  (define-instruction mffs. (segment frd)
  (:printer x-22 ((op 63)  (xo 583) (rc 1)))
  (:delay 1)
  (:dependencies (reads :fpscr) (writes frd) (writes :ccr))
  (:emitter (emit-x-form-inst segment
                          63
                          (fp-reg-tn-encoding frd)
                          0
                          0
                          583
                          1)))

  (define-instruction mtfsf (segment mask rb)
  (:printer xfl ((op 63) (xo 711) (rc 0)))
  (:dependencies (reads rb) (writes :fpscr))
  (:delay 1)
  (:emitter (emit-xfl-form-inst segment 63  (ash mask 1) (fp-reg-tn-encoding rb) 711 0)))

  (define-instruction mtfsf. (segment mask rb)
  (:printer xfl ((op 63) (xo 711) (rc 1)))
  (:delay 1)
  (:dependencies (reads rb) (writes :ccr) (writes :fpscr))
  (:emitter (emit-xfl-form-inst segment 63  (ash mask 1) (fp-reg-tn-encoding rb) 711 1)))




;;; Here in the future, macros are our friends.

  (define-instruction-macro subis (rt ra simm)
    `(inst addis ,rt ,ra (- ,simm)))

  (define-instruction-macro sub (rt rb ra)
    `(inst subf ,rt ,ra ,rb))
  (define-instruction-macro sub. (rt rb ra)
    `(inst subf. ,rt ,ra ,rb))
  (define-instruction-macro subo (rt rb ra)
    `(inst subfo ,rt ,ra ,rb))
  (define-instruction-macro subo. (rt rb ra)
    `(inst subfo. ,rt ,ra ,rb))


  (define-instruction-macro subic (rt ra simm)
    `(inst addic ,rt ,ra (- ,simm)))


  (define-instruction-macro subic. (rt ra simm)
    `(inst addic. ,rt ,ra (- ,simm)))



  (define-instruction-macro subc (rt rb ra)
    `(inst subfc ,rt ,ra ,rb))
  (define-instruction-macro subc. (rt rb ra)
    `(inst subfc. ,rt ,ra ,rb))
  (define-instruction-macro subco (rt rb ra)
    `(inst subfco ,rt ,ra ,rb))
  (define-instruction-macro subco. (rt rb ra)
    `(inst subfco. ,rt ,ra ,rb))

  (define-instruction-macro subi (rt ra simm)
    `(inst addi ,rt ,ra (- ,simm)))

  (define-instruction-macro li (rt val) `(inst addi ,rt 0 ,val))
  (define-instruction-macro lis (rt val) `(inst addis ,rt 0 ,val))

  (define-instruction-macro not (ra rs)
    `(inst nor ,ra ,rs ,rs))

  (define-instruction-macro not. (ra rs)
    `(inst nor. ,ra ,rs ,rs))

;; These can be temporarily commented out to find inadvertent
;; use of 32-bit rlwinm and related macro instructions in 64-bit code.
(progn
  (define-instruction-macro extlwi (ra rs n b)
    `(inst rlwinm ,ra ,rs ,b 0 (1- ,n)))

  (define-instruction-macro extlwi. (ra rs n b)
    `(inst rlwinm. ,ra ,rs ,b 0 (1- ,n)))

  (define-instruction-macro extrwi (ra rs n b)
    `(inst rlwinm ,ra ,rs (mod (+ ,b ,n) 32) (- 32 ,n) 31))

  (define-instruction-macro extrwi. (ra rs n b)
    `(inst rlwinm. ,ra ,rs (mod (+ ,b ,n) 32) (- 32 ,n) 31))

  (define-instruction-macro srwi (ra rs n) ; N < 32
    `(let ((.n. (the (integer 0 31) ,n)))
       (inst rlwinm ,ra ,rs (logand #b11111 (- 32 .n.)) .n. 31))) ; n=0 -> rotate 0

  (define-instruction-macro srwi. (ra rs n)
    `(let ((.n. (the (integer 0 31) ,n)))
       (inst rlwinm. ,ra ,rs (logand #b11111 (- 32 .n.)) .n. 31))) ; n=0 -> rotate 0

  (define-instruction-macro clrlwi (ra rs n)
    `(inst rlwinm ,ra ,rs 0 ,n 31))

  (define-instruction-macro clrlwi. (ra rs n)
    `(inst rlwinm. ,ra ,rs 0 ,n 31))

  (define-instruction-macro clrrwi (ra rs n)
    `(inst rlwinm ,ra ,rs 0 0 (- 31 ,n)))

  (define-instruction-macro clrrwi. (ra rs n)
    `(inst rlwinm. ,ra ,rs 0 0 (- 31 ,n)))

  (define-instruction-macro inslw (ra rs n b)
    `(inst rlwimi ,ra ,rs (- 32 ,b) ,b (+ ,b (1- ,n))))

  (define-instruction-macro inslw. (ra rs n b)
    `(inst rlwimi. ,ra ,rs (- 32 ,b) ,b (+ ,b (1- ,n))))

  (define-instruction-macro rotlw (ra rs rb)
    `(inst rlwnm ,ra ,rs ,rb 0 31))

  (define-instruction-macro rotlw. (ra rs rb)
    `(inst rlwnm. ,ra ,rs ,rb 0 31))

  (define-instruction-macro rotlwi (ra rs n)
    `(inst rlwinm ,ra ,rs ,n 0 31))

  (define-instruction-macro rotrwi (ra rs n)
    `(inst rlwinm ,ra ,rs (- 32 ,n) 0 31))

  (define-instruction-macro slwi (ra rs n) ; N < 32
    `(inst rlwinm ,ra ,rs ,n 0 (- 31 ,n)))

  (define-instruction-macro slwi. (ra rs n)
    `(inst rlwinm. ,ra ,rs ,n 0 (- 31 ,n)))

  (define-instruction-macro rotldi (ra rs n) `(inst rldicl ,ra ,rs ,n 0))
  (define-instruction-macro rotrdi (ra rs n) `(inst rldicl ,ra ,rs (- 64 ,n) 0))
  (define-instruction-macro rotld (ra rs rb) `(inst rldcl ,ra ,rs ,rb 0))

) ; end PROGN

  ;; found on page 104 of version 3.0B of the ISA
  ;; and/or appendix C on page 797
  (define-instruction-macro sldi (ra rs n) ; N < 64
    `(inst rldicr ,ra ,rs ,n (- 63 ,n)))

  (define-instruction-macro sldi. (ra rs n)
    `(inst rldicr. ,ra ,rs ,n (- 63 ,n)))

  (define-instruction-macro srdi (ra rs n) ; N < 64
    `(let ((.n. (the (integer 0 63) ,n)))
       (inst rldicl ,ra ,rs (logand #b111111 (- 64 .n.)) .n.))) ; n=0 -> rotate 0

  (define-instruction-macro srdi. (ra rs n)
    `(let ((.n. (the (integer 0 63) ,n)))
       (inst rldicl. ,ra ,rs (logand #b111111 (- 64 .n.)) .n.))) ; n=0 -> rotate 0

  (define-instruction-macro clrrdi (ra rs n) ; N < 64
    `(inst rldicr ,ra ,rs 0 (- 63 ,n)))
  (define-instruction-macro clrrdi. (ra rs n) ; N < 64
    `(inst rldicr. ,ra ,rs 0 (- 63 ,n)))
  )

#|
(macrolet
  ((define-conditional-branches (name bo-name)
     (let* ((bo-enc (valid-bo-encoding bo-name)))
       `(progn
          (define-instruction-macro ,(symbolicate name "A") (bi target)
            ``(inst bca ,,,bo-enc ,,bi ,,target))
          (define-instruction-macro ,(symbolicate name "L") (bi target)
            ``(inst bcl ,,,bo-enc ,,bi ,,target))
          (define-instruction-macro ,(symbolicate name "LA") (bi target)
            ``(inst bcla ,,,bo-enc ,,bi ,,target))
          (define-instruction-macro ,(symbolicate name "CTR") (bi target)
            ``(inst bcctr ,,,bo-enc ,,bi ,,target))
          (define-instruction-macro ,(symbolicate name "CTRL") (bi target)
            ``(inst bcctrl ,,,bo-enc ,,bi ,,target))
          (define-instruction-macro ,(symbolicate name "LR") (bi target)
            ``(inst bclr ,,,bo-enc ,,bi ,,target))
          (define-instruction-macro ,(symbolicate name "LRL") (bi target)
            ``(inst bclrl ,,,bo-enc ,,bi ,,target))))))
  (define-conditional-branches bt :bo-t)
  (define-conditional-branches bf :bo-f))
|#

(macrolet
  ((define-positive-conditional-branches (name cr-bit-name)
     `(progn
        (define-instruction-macro ,name (crf &optional (target nil target-p))
          (unless target-p
            (setq target crf crf :cr0))
          `(inst bt `(,,crf ,,,cr-bit-name) ,target))
#|
        (define-instruction-macro ,(symbolicate name "A") (target &optional (cr-field :cr0))
          ``(inst bta (,,cr-field ,,,cr-bit-name) ,,target))
        (define-instruction-macro ,(symbolicate name "L") (target &optional (cr-field :cr0))
          ``(inst btl (,,cr-field ,,,cr-bit-name) ,,target))
        (define-instruction-macro ,(symbolicate name "LA") (target &optional (cr-field :cr0))
          ``(inst btla (,,cr-field ,,,cr-bit-name) ,,target))
        (define-instruction-macro ,(symbolicate name "CTR") (target &optional (cr-field :cr0))
          ``(inst btctr (,,cr-field ,,,cr-bit-name) ,,target))
        (define-instruction-macro ,(symbolicate name "CTRL") (target &optional (cr-field :cr0))
          ``(inst btctrl (,,cr-field ,,,cr-bit-name) ,,target))
        (define-instruction-macro ,(symbolicate name "LR") (target &optional (cr-field :cr0))
          ``(inst btlr (,,cr-field ,,,cr-bit-name) ,,target))
        (define-instruction-macro ,(symbolicate name "LRL") (target &optional (cr-field :cr0))
          ``(inst btlrl (,,cr-field ,,,cr-bit-name) ,,target))
|#
        )))
  (define-positive-conditional-branches beq :eq)
  (define-positive-conditional-branches blt :lt)
  (define-positive-conditional-branches bgt :gt)
  (define-positive-conditional-branches bso :so)
  (define-positive-conditional-branches bun :so))


(macrolet
  ((define-negative-conditional-branches (name cr-bit-name)
     `(progn
        (define-instruction-macro ,name (crf &optional (target nil target-p))
          (unless target-p
            (setq target crf crf :cr0))
          `(inst bf `(,,crf ,,,cr-bit-name) ,target))
#|
        (define-instruction-macro ,(symbolicate name "A") (target &optional (cr-field :cr0))
          ``(inst bfa (,,cr-field ,,,cr-bit-name) ,,target))
        (define-instruction-macro ,(symbolicate name "L") (target &optional (cr-field :cr0))
          ``(inst bfl (,,cr-field ,,,cr-bit-name) ,,target))
        (define-instruction-macro ,(symbolicate name "LA") (target &optional (cr-field :cr0))
          ``(inst bfla (,,cr-field ,,,cr-bit-name) ,,target))
        (define-instruction-macro ,(symbolicate name "CTR") (target &optional (cr-field :cr0))
          ``(inst bfctr (,,cr-field ,,,cr-bit-name) ,,target))
        (define-instruction-macro ,(symbolicate name "CTRL") (target &optional (cr-field :cr0))
          ``(inst bfctrl (,,cr-field ,,,cr-bit-name) ,,target))
        (define-instruction-macro ,(symbolicate name "LR") (target &optional (cr-field :cr0))
          ``(inst bflr (,,cr-field ,,,cr-bit-name) ,,target))
        (define-instruction-macro ,(symbolicate name "LRL") (target &optional (cr-field :cr0))
          ``(inst bflrl (,,cr-field ,,,cr-bit-name) ,,target))
|#
)))
  (define-negative-conditional-branches bne :eq)
  (define-negative-conditional-branches bnl :lt)
  (define-negative-conditional-branches bge :lt)
  (define-negative-conditional-branches bng :gt)
  (define-negative-conditional-branches ble :gt)
  (define-negative-conditional-branches bns :so)
  (define-negative-conditional-branches bnu :so))



(define-instruction-macro j (func-tn offset)
  `(progn
    (inst addi lip-tn ,func-tn ,offset)
    (inst mtctr lip-tn)
    (inst bctr)))


#|
(define-instruction-macro bua (target)
  `(inst bca :bo-u 0 ,target))

(define-instruction-macro bul (target)
  `(inst bcl :bo-u 0 ,target))

(define-instruction-macro bula (target)
  `(inst bcla :bo-u 0 ,target))
|#

(define-instruction-macro blrl ()
  `(inst bclrl :bo-u 0))


;;; Some more macros

#-64-bit
(defun %lr (reg value)
  (etypecase value
    ((signed-byte 16)
     (inst li reg value))
    ((unsigned-byte 16)
     (inst ori reg zero-tn value))
    ((or (signed-byte 32) (unsigned-byte 32))
     (let* ((high-half (ldb (byte 16 16) value))
            (low-half (ldb (byte 16 0) value)))
       (declare (type (unsigned-byte 16) high-half low-half))
       (cond ((and (logbitp 15 low-half) (= high-half #xffff))
              (inst li reg (dpb low-half (byte 16 0) -1)))
             ((and (not (logbitp 15 low-half)) (zerop high-half))
              (inst li reg low-half))
             (t
              (inst lis reg (if (logbitp 15 high-half)
                                (dpb high-half (byte 16 0) -1)
                              high-half))
              (unless (zerop low-half)
                (inst ori reg reg low-half))))))
    (fixup
     (inst lis reg value)
     (inst addi reg reg value))))

;;; The 64-bit code for %LR is simple - if one 'li' instruction
;;; isn't adequate, then load the value from memory.
;;; There are plenty of tricks that we're missing though, such as some large
;;; positive constants being modularly equivalent to small negative ones,
;;; and any constant for which a single 'addis' would suffice.
#+64-bit
(defun %lr (reg value)
  (etypecase value
    (integer
     (etypecase value
       ((signed-byte 16)
        (inst li reg value))
       ((unsigned-byte 16)
        (inst lhz reg code-tn (register-inline-constant value :halfword)))
       ((signed-byte 32)
        (if (not (logtest value #xFFFF))
            (inst lis reg (ash value -16))
            (inst lwa reg code-tn (register-inline-constant value :word))))
       ((unsigned-byte 32)
        (inst lwz reg code-tn (register-inline-constant value :word)))
       ((or (signed-byte 64) (unsigned-byte 64))
        (inst ld reg code-tn (register-inline-constant value :dword)))))
    (fixup
     (inst lis reg value)
     (inst addi reg reg value))))

(define-instruction-macro lr (reg value)
  `(%lr ,reg ,value))



;;;; Instructions for dumping data and header objects.

(define-instruction word (segment word)
  (:declare (type (or (unsigned-byte 32) (signed-byte 32) fixup) word))
  :pinned
  (:delay 0)
  (:emitter
   (etypecase word
     #-64-bit
     (fixup
      (note-fixup segment :absolute word)
      (emit-word segment 0))
     (integer
      (emit-word segment word)))))
(define-instruction dword (segment dword)
  (:declare (type (or (unsigned-byte 32) (signed-byte 32) fixup) dword)) ; what is this I can't even
  :pinned
  (:delay 0)
  (:emitter
   (etypecase dword
     #+64-bit
     (fixup
      (note-fixup segment :absolute dword)
      (emit-dword segment 0))
     (integer
      (emit-dword segment dword)))))

(define-instruction byte (segment byte)
  (:declare (type (or (unsigned-byte 8) (signed-byte 8)) byte))
  :pinned
  (:delay 0)
  (:emitter
   (emit-byte segment byte)))

(defun emit-header-data (segment type)
  (emit-back-patch
   segment n-word-bytes
   #'(lambda (segment posn)
       (#-64-bit emit-word #+64-bit emit-dword
        segment (logior type (ash (+ posn (component-header-length))
                                  (- n-widetag-bits word-shift)))))))

(define-instruction simple-fun-header-word (segment)
  :pinned
  (:delay 0)
  (:emitter
   (emit-header-data segment simple-fun-widetag)))

(define-instruction lra-header-word (segment)
  :pinned
  (:delay 0)
  (:emitter
   (emit-header-data segment return-pc-widetag)))


;;;; Instructions for converting between code objects, functions, and lras.
(defun emit-compute-inst (segment vop dst src label temp calc)
  (emit-chooser
   ;; We emit either 12 or 4 bytes, so we maintain 8 byte alignments.
   segment 12 3
   #'(lambda (segment chooser posn delta-if-after)
       (declare (ignore chooser))
       (let ((delta (funcall calc label posn delta-if-after)))
         (when (<= (- (ash 1 15)) delta (1- (ash 1 15)))
           (emit-back-patch segment 4
                            #'(lambda (segment posn)
                                (assemble (segment vop)
                                          (inst addi dst src
                                                (funcall calc label posn 0)))))
           t)))
   #'(lambda (segment posn)
       (let ((delta (funcall calc label posn 0)))
         (assemble (segment vop)
                   (inst lis temp (ldb (byte 16 16) delta))
                   (inst ori temp temp (ldb (byte 16 0) delta))
                   (inst add dst src temp))))))

;; code = lip - header - label-offset + code-tn-lowtag
(define-instruction compute-code-from-lip (segment dst src label temp)
  (:declare (type tn dst src temp) (type label label))
  (:attributes variable-length)
  (:dependencies (reads src) (writes dst) (writes temp))
  (:delay 0)
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop dst src label temp
                      #'(lambda (label posn delta-if-after)
                          (- code-tn-lowtag
                             (label-position label posn delta-if-after)
                             (component-header-length))))))

;; code = lra - other-pointer-tag - header - label-offset + code-tn-lowtag
(define-instruction compute-code-from-lra (segment dst src label temp)
  (:declare (type tn dst src temp) (type label label))
  (:attributes variable-length)
  (:dependencies (reads src) (writes dst) (writes temp))
  (:delay 0)
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop dst src label temp
                      #'(lambda (label posn delta-if-after)
                          (- code-tn-lowtag
                             (label-position label posn delta-if-after)
                             (component-header-length)
                             other-pointer-lowtag)))))

;; lra = code - code-tn-lowtag + header + label-offset + other-pointer-tag
(define-instruction compute-lra-from-code (segment dst src label temp)
  (:declare (type tn dst src temp) (type label label))
  (:attributes variable-length)
  (:dependencies (reads src) (writes dst) (writes temp))
  (:delay 0)
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop dst src label temp
                      #'(lambda (label posn delta-if-after)
                          (+ (label-position label posn delta-if-after)
                             (component-header-length)
                             other-pointer-lowtag
                             (- code-tn-lowtag))))))

;;; Unboxed constant support

(defun sb-vm:canonicalize-inline-constant (constant)
  (destructuring-bind (value size) constant
    (cons size value)))

(defun sb-vm:inline-constant-value (constant)
  (declare (ignore constant))
  (let ((label (gen-label)))
    (values label label)))

(defun size-nbyte (size)
  (ecase size
    (:byte     1)
    (:halfword 2)
    (:word     4)
    (:dword    8)
    (:qword   16)))

(defun align-of (constant)
  (case (car constant)
    (:jump-table n-word-bytes)
    (t (size-nbyte (car constant)))))

(defun sort-inline-constants (constants)
  (stable-sort constants #'> :key (lambda (x) (align-of (car x)))))

(sb-assem::%def-inst-encoder
 '.layout-id
 (lambda (segment layout)
   (sb-c:note-fixup segment :layout-id (sb-c:make-fixup layout :layout-id))
   (sb-assem::%emit-skip segment 4)))

(defun emit-inline-constant (section constant label)
  (let ((size (align-of constant)))
    (emit section
          `(.align ,(integer-length (1- size)))
          label
          (cond ((eq (car constant) :jump-table)
                 `(.lispword ,@(coerce (cdr constant) 'list)))
                ((typep (cdr constant) '(cons (eql :layout-id)))
                 `(.layout-id ,(cddr constant)))
                (t
                 (let* ((val (cdr constant))
                        (bytes (loop repeat size
                                     collect (prog1 (ldb (byte 8 0) val)
                                               (setf val (ash val -8))))))
                   #+big-endian (setq bytes (nreverse bytes))
                   `(.byte ,@bytes)))))))

(defun sb-vm:fixup-code-object (code offset value kind flavor)
  (declare (type index offset))
  (unless (zerop (rem offset sb-assem:+inst-alignment-bytes+))
    (error "Unaligned instruction?  offset=#x~X." offset))
  (let ((sap (code-instructions code)))
    (ecase kind
      (:absolute
       ;; There is an implicit addend currently stored in the fixup location.
       (incf (sap-ref-word sap offset) value))
      (:layout-id
       (aver (zerop (sap-ref-32 sap offset)))
       (setf (signed-sap-ref-32 sap offset) (the layout-id value)))
      (:rldic-m ; This is the M (mask) immediate operand to RLDIC{L,R} which
       ;; appears in (byte 6 5) of the instruction. See EMIT-MD-FORM-INST.
       (setf (ldb (byte 6 5) (sap-ref-32 sap offset)) (encode-mask6 (- 64 value))))
      (:addis+ld
       ;; load from linkage table
       (binding* (((q r) (floor (ash value word-shift) 65536))
                  (table-bias (- (ash (ash 1 (+ 19 3)) -16))))
         (when (>= r 32768) ; LD instruction sign-extends, so this is actually negative
           (incf q))
         (setf (sap-ref-32 sap (- offset 4)) ; addis instruction
               (logior (sap-ref-32 sap (- offset 4)) (ldb (byte 16 0) (+ table-bias q)))
               (sap-ref-32 sap offset) ; ld instruction
               (logior (sap-ref-32 sap offset) r))))
      (:b
       (error "Can't deal with CALL fixups, yet."))
      (:ba
       (setf (ldb (byte 24 2) (sap-ref-32 sap offset)) (ash value -2)))
      (:ha
       (let* ((h (ldb (byte 16 16) value))
              (l (ldb (byte 16 0) value)))
          ; Compensate for possible sign-extension when the low half
          ; is added to the high.  We could avoid this by ORI-ing
          ; the low half in 32-bit absolute loads, but it'd be
          ; nice to be able to do:
          ;  lis rX,foo@ha
          ;  lwz rY,foo@l(rX)
          ; and lwz/stw and friends all use a signed 16-bit offset.
          (setf (ldb (byte 16 0) (sap-ref-32 sap offset))
                 (if (logbitp 15 l) (ldb (byte 16 0) (1+ h)) h))))
      (:l
       (when (eq flavor :assembly-routine)
         (decf value nil-value))
       (setf (ldb (byte 16 0) (sap-ref-32 sap offset))
             (ldb (byte 16 0) value)))))
  nil)

(define-instruction store-coverage-mark (segment mark-index temp)
  (:emitter
   ;; No backpatch is needed to compute the offset into the code header
   ;; because COMPONENT-HEADER-LENGTH is known at this point.
   (let ((offset (+ (component-header-length)
                    ;; skip over jump table word and entries
                    (* (1+ (component-n-jump-table-entries))
                       n-word-bytes)
                    mark-index
                    (- code-tn-lowtag))))
     (inst* segment 'stb sb-vm::null-tn sb-vm::code-tn
            (etypecase offset
              ((unsigned-byte 15) offset)
              ((unsigned-byte 31)
               ;; This is redundant with the logic in %LR, but unfortunately
               ;; %LR does not take a SEGMENT argument.
               ;; We could probably do this whole sequence in just 2 instructions-
               ;; an "ADDIS LIP, CODE, something" and a STB using LIP as the base.
               (inst* segment 'lis temp (ldb (byte 15 16) offset))
               (inst* segment 'ori temp (ldb (byte 16 16) offset))
               temp))))))
