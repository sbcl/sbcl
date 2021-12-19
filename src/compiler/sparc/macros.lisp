;;;; various useful macros for generating Sparc code

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
       (inst move ,n-dst ,n-src))))

(macrolet
    ((def (op inst shift)
       `(defmacro ,op (object base &optional (offset 0) (lowtag 0))
          `(inst ,',inst ,object ,base (- (ash ,offset ,,shift) ,lowtag)))))
  (def loadw ld word-shift)
  (def storew st word-shift))

(defmacro load-symbol (reg symbol)
  `(inst add ,reg null-tn (static-symbol-offset ,symbol)))

(macrolet
    ((frob (slot)
       (let ((loader (intern (concatenate 'simple-string
                                          "LOAD-SYMBOL-"
                                          (string slot))))
             (storer (intern (concatenate 'simple-string
                                          "STORE-SYMBOL-"
                                          (string slot))))
             (offset (intern (concatenate 'simple-string
                                          "SYMBOL-"
                                          (string slot)
                                          "-SLOT")
                             (find-package "SB-VM"))))
         `(progn
            (defmacro ,loader (reg symbol)
              `(inst ld ,reg null-tn
                     (+ (static-symbol-offset ',symbol)
                        (ash ,',offset word-shift)
                        (- other-pointer-lowtag))))
            (defmacro ,storer (reg symbol)
              `(inst st ,reg null-tn
                     (+ (static-symbol-offset ',symbol)
                        (ash ,',offset word-shift)
                        (- other-pointer-lowtag))))))))
  (frob value)
  (frob function))

(defmacro load-type (target source &optional (offset 0))
  "Loads the type bits of a pointer into target independent of
  byte-ordering issues."
  (once-only ((n-target target)
              (n-source source)
              (n-offset offset))
    (ecase *backend-byte-order*
      (:little-endian
       `(inst ldub ,n-target ,n-source ,n-offset))
      (:big-endian
       `(inst ldub ,n-target ,n-source (+ ,n-offset (1- n-word-bytes)))))))

;;; Macros to handle the fact that we cannot use the machine native call and
;;; return instructions.

(defmacro lisp-jump (fun)
  "Jump to the lisp function FUNCTION.  LIP is an interior-reg temporary."
  `(progn
     (inst j ,fun
           (- (ash simple-fun-insts-offset word-shift) fun-pointer-lowtag))
     (move code-tn ,fun)))

(defmacro lisp-return (return-pc &key (offset 0) (frob-code t))
  "Return to RETURN-PC."
  `(progn
     (inst j ,return-pc
           (- (* (1+ ,offset) n-word-bytes) other-pointer-lowtag))
     ,(if frob-code
          `(move code-tn ,return-pc)
          '(inst nop))))

(defmacro emit-return-pc (label)
  "Emit a return-pc header word.  LABEL is the label to use for this return-pc."
  `(progn
     (emit-alignment n-lowtag-bits)
     (emit-label ,label)
     (inst lra-header-word)))



;;;; stack TN's

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



;;;; Storage allocation:

;;;; Allocation macro
;;;;
;;;; This macro does the appropriate stuff to allocate space.
;;;;
;;;; The allocated space is stored in RESULT-TN with the lowtag LOWTAG
;;;; applied.  The amount of space to be allocated is SIZE bytes (which
;;;; must be a multiple of the lisp object size).
(defun allocation (type size lowtag result-tn &key stack-p temp-tn)
  (declare (ignorable type))
  #+gencgc
  ;; A temp register is needed to do inline allocation.  TEMP-TN, in
  ;; this case, can be any register, since it holds a double-word
  ;; aligned address (essentially a fixnum).
  (assert temp-tn)
  ;; We assume we're in a pseudo-atomic so the pseudo-atomic bit is
  ;; set.
  (cond
     (stack-p
      ;; Stack allocation
      ;;
      ;; The control stack grows up, so round up CSP to a
      ;; multiple of 8 (lispobj size).  Use that as the
      ;; allocation pointer.  Then add SIZE bytes to the
      ;; allocation and set CSP to that, so we have the desired
      ;; space.

      ;; Make sure the temp-tn is a non-descriptor register!
      (aver (and temp-tn (sc-is temp-tn non-descriptor-reg)))

      ;; temp-tn is csp-tn rounded up to a multiple of 8 (lispobj size)
      (align-csp temp-tn)
      ;; For the benefit of future historians, this is how CMUCL does the
      ;; align-csp (I think their version is branch free only because
      ;; they simply don't worry about zeroing the pad word):
      #+nil (inst add ,temp-tn csp-tn sb-vm:lowtag-mask)
      #+nil (inst andn ,temp-tn sb-vm:lowtag-mask)

      ;; Set the result to csp-tn, with appropriate lowtag
      (inst or result-tn csp-tn lowtag)

      ;; Allocate the desired space on the stack.
      ;;
      ;; FIXME: Can't allocate on stack if SIZE is too large.
      ;; Need to rearrange this code.
      (inst add csp-tn size))

     #-gencgc
     ;; Normal allocation to the heap -- cheneygc version.
     ;;
     ;; On cheneygc, the alloc-tn currently has the pseudo-atomic bit.
     ;; If the lowtag also has a 1 bit in the same position, we're all set.
     ;;
     ;; See comment in PSEUDO-ATOMIC-FLAG.
     ((logbitp (1- n-lowtag-bits) lowtag)
      (inst or result-tn alloc-tn lowtag)
      (inst add alloc-tn size))
     ;;
     ;; Otherwise, we need to zap out the lowtag from alloc-tn, and then
     ;; or in the lowtag.
     #-gencgc
     (t
      (inst andn result-tn alloc-tn lowtag-mask)
      (inst or result-tn lowtag)
      (inst add alloc-tn size))

     ;; Normal allocation to the heap -- gencgc version.
     ;;
     ;; No need to worry about lowtag bits matching up here, since
     ;; alloc-tn is just a "pseudo-atomic-bit-tn" now and we don't read
     ;; it.
     #+gencgc
     (t
      (loadw result-tn null-tn 0 (- nil-value mixed-region)) ; free_pointer
      (loadw temp-tn null-tn 1 (- nil-value mixed-region))   ; end_addr

      (without-scheduling ()
        (let ((done (gen-label))
              (full-alloc (gen-label)))
          ;; See if we can do an inline allocation.  The updated
          ;; free pointer should not point past the end of the
          ;; current region.  If it does, a full alloc needs to be
          ;; done.
          (inst add result-tn size)

          ;; result-tn points to the new end of region.  Did we go
          ;; past the actual end of the region?  If so, we need a
          ;; full alloc.
          (inst cmp result-tn temp-tn)
          (if (member :sparc-v9 *backend-subfeatures*)
              (inst b :gtu full-alloc :pn)
              (inst b :gtu full-alloc))
          ;; New algorithm: no taken branches in the fast case, utilize
          ;; the branch delay slot to write back the free-pointer
          ;; (on overflow restore it the trap handler to a good value),
          ;; and fold the lowtag addition into the size subtraction.
          (storew result-tn null-tn 0 (- nil-value mixed-region))
          ;; Compute the base pointer and add lowtag.
          (cond ((integerp size)
                 (inst sub result-tn (- size lowtag)))
                (t
                 (inst sub result-tn size)
                 (inst or result-tn lowtag)))
          (emit-label done)
          (assemble (:elsewhere)
            (without-scheduling ()
              ;; Encode the RESULT-TN, SIZE, and TYPE in an OR instruction
              ;; which is never executed.
              (inst or (make-random-tn :sc (sc-or-lose 'unsigned-reg)
                                       :kind :normal
                                       :offset (if (eq type 'list) 1 0))
                    result-tn size)
              (emit-label FULL-ALLOC)
              ;; Trap into the C allocator
              (inst unimp allocation-trap)
              (inst b done)
              (inst or result-tn lowtag))))))))

(defmacro with-fixed-allocation ((result-tn temp-tn type-code size)
                                 &body body)
  "Do stuff to allocate an other-pointer object of fixed Size with a single
  word header having the specified Type-Code.  The result is placed in
  Result-TN, and Temp-TN is a non-descriptor temp (which may be randomly used
  by the body.)  The body is placed inside the PSEUDO-ATOMIC, and presumably
  initializes the object."
  (unless body
    (bug "empty &body in WITH-FIXED-ALLOCATION"))
  (once-only ((result-tn result-tn) (temp-tn temp-tn)
              (type-code type-code) (size size))
    `(pseudo-atomic ()
       (allocation nil (pad-data-block ,size) other-pointer-lowtag ,result-tn
                   :temp-tn ,temp-tn)
       (inst li ,temp-tn (compute-object-header ,size ,type-code))
       (storew ,temp-tn ,result-tn 0 other-pointer-lowtag)
       ,@body)))

(defun align-csp (temp)
  (let ((aligned (gen-label)))
    ;; FIXME: why use a TEMP?  Why not just ZERO-TN?
    (inst andcc temp csp-tn lowtag-mask)
    (if (member :sparc-v9 *backend-subfeatures*)
        (inst b :eq aligned :pt)
        (inst b :eq aligned))
    (storew zero-tn csp-tn 0) ; sneaky use of delay slot
    (inst add csp-tn csp-tn n-word-bytes)
    (emit-label aligned)))

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
      (apply #'error-call vop error-code values)
      start-lab)))

;;; a handy macro for making sequences look atomic
(defmacro pseudo-atomic ((&optional) &rest forms)
  (let ()
    `(progn
       ;; Set the pseudo-atomic flag.
       (without-scheduling ()
         (inst or alloc-tn 4))
       ,@forms
       ;; Reset the pseudo-atomic flag.
       (without-scheduling ()
        ;; Remove the pseudo-atomic flag.
        (inst andn alloc-tn 4)
        ;; Check to see if pseudo-atomic interrupted flag is set (bit 0 = 1).
        (inst andcc zero-tn alloc-tn 3)
        ;; The C code needs to process this correctly and fixup alloc-tn.
        (inst t :ne pseudo-atomic-trap)))))
