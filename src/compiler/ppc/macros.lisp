;;;; a bunch of handy macros for the PPC

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;; Instruction-like macros.

(defmacro move (dst src)
  "Move SRC into DST unless they are location=."
  (once-only ((n-dst dst)
              (n-src src))
    `(unless (location= ,n-dst ,n-src)
       (inst mr ,n-dst ,n-src))))

(defmacro load-asm-rtn-addr (reg name)
  ;; asm code resides in static space and we can reference it relative to NIL.
  ;; The :L fixup subtracts the address of NIL for you.
  `(inst addi ,reg null-tn (make-fixup ,name :assembly-routine)))

(macrolet
    ((def (op inst shift)
       `(defmacro ,op (object base &optional (offset 0) (lowtag 0))
          `(inst ,',inst ,object ,base (- (ash ,offset ,,shift) ,lowtag)))))
  (def loadw lwz word-shift)
  (def storew stw word-shift))

(defmacro load-symbol (reg symbol)
  `(inst addi ,reg null-tn (static-symbol-offset ,symbol)))

(defmacro load-symbol-value (reg symbol)
  `(inst lwz ,reg null-tn (+ (static-symbol-offset ',symbol)
                             (ash symbol-value-slot word-shift)
                             (- other-pointer-lowtag))))
(defmacro store-symbol-value (reg symbol)
  `(inst stw ,reg null-tn (+ (static-symbol-offset ',symbol)
                             (ash symbol-value-slot word-shift)
                             (- other-pointer-lowtag))))

;; FIXME: These are only good for static-symbols, so why not
;; statically-allocate the static-symbol TLS slot indices at
;; cross-compile time so we can just use a fixed offset within the
;; TLS block instead of mucking about with the extra memory access
;; (and temp register, for stores)?
#+sb-thread
(progn
(defmacro load-tl-symbol-value (reg symbol)
  `(progn
     (inst lwz ,reg null-tn (+ (static-symbol-offset ',symbol)
                               (ash symbol-tls-index-slot word-shift)
                               (- other-pointer-lowtag)))
     (inst lwzx ,reg thread-base-tn ,reg)))
(defmacro store-tl-symbol-value (reg symbol temp)
  `(progn
     (inst lwz ,temp null-tn (+ (static-symbol-offset ',symbol)
                                (ash symbol-tls-index-slot word-shift)
                                (- other-pointer-lowtag)))
     (inst stwx ,reg thread-base-tn ,temp))))
#-sb-thread
(progn
(defmacro load-tl-symbol-value (reg symbol)
  `(load-symbol-value ,reg ,symbol))
(defmacro store-tl-symbol-value (reg symbol temp)
  (declare (ignore temp))
  `(store-symbol-value ,reg ,symbol)))

(defmacro load-type (target source &optional (offset 0))
  "Loads the type bits of a pointer into target independent of
  byte-ordering issues."
  (once-only ((n-target target)
              (n-source source)
              (n-offset offset))
    (ecase *backend-byte-order*
      (:little-endian
       `(inst lbz ,n-target ,n-source ,n-offset))
      (:big-endian
       `(inst lbz ,n-target ,n-source (+ ,n-offset (1- n-word-bytes)))))))

;;; Macros to handle the fact that we cannot use the machine native call and
;;; return instructions.

(defmacro lisp-jump (function lip)
  "Jump to the lisp function FUNCTION.  LIP is an interior-reg temporary."
  `(progn
    ;; something is deeply bogus.  look at this
    ;; (loadw ,lip ,function function-code-offset function-pointer-type)
    (inst addi ,lip ,function (- (* n-word-bytes simple-fun-insts-offset) fun-pointer-lowtag))
    (inst mtctr ,lip)
    (inst bctr)))

(defmacro lisp-return (return-pc lip &key (offset 0))
  "Return to RETURN-PC."
  `(progn
     (inst addi ,lip ,return-pc (- (* (1+ ,offset) n-word-bytes) other-pointer-lowtag))
     (inst mtlr ,lip)
     (inst blr)))

(defmacro emit-return-pc (label)
  "Emit a return-pc header word.  LABEL is the label to use for this return-pc."
  `(progn
     (emit-alignment n-lowtag-bits)
     (emit-label ,label)
     (inst lra-header-word)))



;;;; Stack TN's

;;; Move a stack TN to a register and vice-versa.
(defmacro load-stack-tn (reg stack)
  `(let ((reg ,reg)
         (stack ,stack))
     (let ((offset (tn-offset stack)))
       (sc-case stack
         ((control-stack)
          (loadw reg cfp-tn offset))))))
(defmacro store-stack-tn (stack reg)
  `(let ((stack ,stack)
         (reg ,reg))
     (let ((offset (tn-offset stack)))
       (sc-case stack
         ((control-stack)
          (storew reg cfp-tn offset))))))

(defmacro maybe-load-stack-tn (reg reg-or-stack)
  "Move the TN Reg-Or-Stack into Reg if it isn't already there."
  (once-only ((n-reg reg)
              (n-stack reg-or-stack))
    `(sc-case ,n-reg
       ((any-reg descriptor-reg)
        (sc-case ,n-stack
          ((any-reg descriptor-reg)
           (move ,n-reg ,n-stack))
          ((control-stack)
           (loadw ,n-reg cfp-tn (tn-offset ,n-stack))))))))

(defmacro with-fixed-allocation ((result-tn flag-tn temp-tn type-code size
                                            &key (lowtag other-pointer-lowtag)
                                                 stack-allocate-p)
                                 &body body)
  "Do stuff to allocate an other-pointer object of fixed Size with a single
  word header having the specified Type-Code.  The result is placed in
  Result-TN, and Temp-TN is a non-descriptor temp (which may be randomly used
  by the body.)  The body is placed inside the PSEUDO-ATOMIC, and presumably
  initializes the object."
  (once-only ((result-tn result-tn) (temp-tn temp-tn) (flag-tn flag-tn)
              (type-code type-code) (size size) (lowtag lowtag))
    `(pseudo-atomic (,flag-tn :sync ,type-code)
       (if ,stack-allocate-p
           (progn
             (align-csp ,temp-tn)
             (inst ori ,result-tn csp-tn ,lowtag)
             (inst addi csp-tn csp-tn (pad-data-block ,size)))
         (allocation nil
                     (* (pad-data-block ,size)
                        #+bignum-assertions (if (eql ,type-code bignum-widetag) 2 1))
                     ,lowtag ,result-tn
                     :temp-tn ,temp-tn
                     :flag-tn ,flag-tn))
       (inst lr ,temp-tn (compute-object-header ,size ,type-code))
       (storew ,temp-tn ,result-tn 0 ,lowtag)
       ,@body)))

;;;; Error Code
(defun emit-error-break (vop kind code values)
  (assemble ()
    (when vop
      (note-this-location vop :internal-error))
    (emit-internal-error kind code values
                         :trap-emitter (lambda (tramp-number)
                                         (inst unimp tramp-number)))
    (emit-alignment word-shift)))

(defun generate-error-code (vop error-code &rest values)
  "Generate-Error-Code Error-code Value*
  Emit code for an error with the specified Error-Code and context Values."
  (assemble (:elsewhere)
    (let ((start-lab (gen-label)))
      (emit-label start-lab)
      (emit-error-break vop error-trap (error-number-or-lose error-code) values)
      start-lab)))

;;;; PSEUDO-ATOMIC

;;; handy macro for making sequences look atomic with respect to GC
(defmacro pseudo-atomic ((flag-tn &key elide-if (sync #+sb-thread t)) &body forms)
  `(progn
     (unless ,elide-if
       (without-scheduling ()
         (inst stb null-tn thread-base-tn (* n-word-bytes thread-pseudo-atomic-bits-slot))))
     ,@forms
     (unless ,elide-if
       (when ,sync
         ;; sync is generally needed so that stores aren't delayed past the end of P-A,
         ;; which is important for GC-correctness.
         ;; Given that, I'm unsure why it's ever not needed.
         (inst sync))
       (without-scheduling ()
         (inst stb thread-base-tn thread-base-tn (* n-word-bytes thread-pseudo-atomic-bits-slot))
         ;; Now test to see if the pseudo-atomic interrupted bit is set.
         (inst lhz ,flag-tn thread-base-tn (+ 2 (* n-word-bytes thread-pseudo-atomic-bits-slot)))
         (inst twi :ne ,flag-tn 0)))))
