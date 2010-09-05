;;;; a bunch of handy macros for the PPC

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;; Instruction-like macros.

(defmacro move (dst src)
  "Move SRC into DST unless they are location=."
  (once-only ((n-dst dst)
              (n-src src))
    `(unless (location= ,n-dst ,n-src)
       (inst mr ,n-dst ,n-src))))

(macrolet
    ((def (op inst shift)
       `(defmacro ,op (object base &optional (offset 0) (lowtag 0))
          `(inst ,',inst ,object ,base (- (ash ,offset ,,shift) ,lowtag)))))
  (def loadw lwz word-shift)
  (def storew stw word-shift))

(defmacro load-symbol (reg symbol)
  `(inst addi ,reg null-tn (static-symbol-offset ,symbol)))

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
                             (find-package "SB!VM"))))
         `(progn
            (defmacro ,loader (reg symbol)
              `(inst lwz ,reg null-tn
                     (+ (static-symbol-offset ',symbol)
                        (ash ,',offset word-shift)
                        (- other-pointer-lowtag))))
            (defmacro ,storer (reg symbol)
              `(inst stw ,reg null-tn
                     (+ (static-symbol-offset ',symbol)
                        (ash ,',offset word-shift)
                        (- other-pointer-lowtag))))))))
  (frob value)
  (frob function)

  ;; FIXME: These are only good for static-symbols, so why not
  ;; statically-allocate the static-symbol TLS slot indices at
  ;; cross-compile time so we can just use a fixed offset within the
  ;; TLS block instead of mucking about with the extra memory access
  ;; (and temp register, for stores)?
  #!+sb-thread
  (defmacro load-tl-symbol-value (reg symbol)
    `(progn
       (inst lwz ,reg null-tn
             (+ (static-symbol-offset ',symbol)
                (ash symbol-tls-index-slot word-shift)
                (- other-pointer-lowtag)))
       (inst lwzx ,reg thread-base-tn ,reg)))
  #!-sb-thread
  (defmacro load-tl-symbol-value (reg symbol)
    `(load-symbol-value ,reg ,symbol))

  #!+sb-thread
  (defmacro store-tl-symbol-value (reg symbol temp)
    `(progn
       (inst lwz ,temp null-tn
             (+ (static-symbol-offset ',symbol)
                (ash symbol-tls-index-slot word-shift)
                (- other-pointer-lowtag)))
       (inst stwx ,reg thread-base-tn ,temp)))
  #!-sb-thread
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
    (inst addi ,lip ,function (- (* n-word-bytes simple-fun-code-offset) fun-pointer-lowtag))
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


;;;; Storage allocation:

;;; This is the main mechanism for allocating memory in the lisp heap.
;;;
;;; The allocated space is stored in RESULT-TN with the lowtag LOWTAG
;;; applied.  The amount of space to be allocated is SIZE bytes (which
;;; must be a multiple of the lisp object size).
;;;
;;; On other platforms (Non-PPC), if STACK-P is given, then allocation
;;; occurs on the control stack (for dynamic-extent).  In this case,
;;; you MUST also specify NODE, so that the appropriate compiler
;;; policy can be used, and TEMP-TN, which is needed for work-space.
;;; TEMP-TN MUST be a non-descriptor reg. FIXME: This is not yet
;;; implemented on PPC. We should implement this and replace the
;;; inline stack-based allocation that presently occurs in the
;;; VOPs. The stack-p argument is ignored on PPC.
;;;
;;; If generational GC is enabled, you MUST supply a value for TEMP-TN
;;; because a temp register is needed to do inline allocation.
;;; TEMP-TN, in this case, can be any register, since it holds a
;;; double-word aligned address (essentially a fixnum).
(defmacro allocation (result-tn size lowtag &key stack-p node temp-tn flag-tn)
  ;; We assume we're in a pseudo-atomic so the pseudo-atomic bit is
  ;; set.  If the lowtag also has a 1 bit in the same position, we're all
  ;; set.  Otherwise, we need to zap out the lowtag from alloc-tn, and
  ;; then or in the lowtag.
  ;; Normal allocation to the heap.
  (declare (ignore stack-p node)
           #!-gencgc
           (ignore temp-tn flag-tn))
    #!-gencgc
    (let ((alloc-size (gensym)))
      `(let ((,alloc-size ,size))
         (if (logbitp (1- n-lowtag-bits) ,lowtag)
             (progn
               (inst ori ,result-tn alloc-tn ,lowtag))
             (progn
               (inst clrrwi ,result-tn alloc-tn n-lowtag-bits)
               (inst ori ,result-tn ,result-tn ,lowtag)))
         (if (numberp ,alloc-size)
             (inst addi alloc-tn alloc-tn ,alloc-size)
             (inst add alloc-tn alloc-tn ,alloc-size))))
    #!+gencgc
    `(progn
       ;; Make temp-tn be the size
       (cond ((numberp ,size)
              (inst lr ,temp-tn ,size))
             (t
              (move ,temp-tn ,size)))

       #!-sb-thread
       (inst lr ,flag-tn (make-fixup "boxed_region" :foreign))
       #!-sb-thread
       (inst lwz ,result-tn ,flag-tn 0)
       #!+sb-thread
       (inst lwz ,result-tn thread-base-tn (* thread-alloc-region-slot
                                              n-word-bytes))

       ;; we can optimize this to only use one fixup here, once we get
       ;; it working
       ;; (inst lr ,flag-tn (make-fixup "boxed_region" :foreign 4))
       ;; (inst lwz ,flag-tn ,flag-tn 0)
       #!-sb-thread
       (inst lwz ,flag-tn ,flag-tn 4)
       #!+sb-thread
       (inst lwz ,flag-tn thread-base-tn (* (1+ thread-alloc-region-slot)
                                            n-word-bytes))

       (without-scheduling ()
         ;; CAUTION: The C code depends on the exact order of
         ;; instructions here.  In particular, immediately before the
         ;; TW instruction must be an ADD or ADDI instruction, so it
         ;; can figure out the size of the desired allocation and
         ;; storing the new base pointer back to the allocation region
         ;; must take two instructions (one on threaded targets).

         ;; Now make result-tn point at the end of the object, to
         ;; figure out if we overflowed the current region.
         (inst add ,result-tn ,result-tn ,temp-tn)
         ;; result-tn points to the new end of the region.  Did we go past
         ;; the actual end of the region?  If so, we need a full alloc.
         ;; The C code depends on this exact form of instruction.  If
         ;; either changes, you have to change the other appropriately!
         (inst tw :lge ,result-tn ,flag-tn)

         ;; The C code depends on this instruction sequence taking up
         ;; #!-sb-thread three #!+sb-thread one machine instruction.
         ;; The lr of a fixup counts as two instructions.
         #!-sb-thread
         (inst lr ,flag-tn (make-fixup "boxed_region" :foreign))
         #!-sb-thread
         (inst stw ,result-tn ,flag-tn 0)
         #!+sb-thread
         (inst stw ,result-tn thread-base-tn (* thread-alloc-region-slot
                                                n-word-bytes)))

       ;; Should the allocation trap above have fired, the runtime
       ;; arranges for execution to resume here, just after where we
       ;; would have updated the free pointer in the alloc region.

       ;; At this point, result-tn points at the end of the object.
       ;; Adjust to point to the beginning.
       (inst sub ,result-tn ,result-tn ,temp-tn)
       ;; Set the lowtag appropriately
       (inst ori ,result-tn ,result-tn ,lowtag)))

(defmacro with-fixed-allocation ((result-tn flag-tn temp-tn type-code size
                                            &key (lowtag other-pointer-lowtag))
                                 &body body)
  "Do stuff to allocate an other-pointer object of fixed Size with a single
  word header having the specified Type-Code.  The result is placed in
  Result-TN, and Temp-TN is a non-descriptor temp (which may be randomly used
  by the body.)  The body is placed inside the PSEUDO-ATOMIC, and presumably
  initializes the object."
  (once-only ((result-tn result-tn) (temp-tn temp-tn) (flag-tn flag-tn)
              (type-code type-code) (size size) (lowtag lowtag))
    `(pseudo-atomic (,flag-tn)
       (allocation ,result-tn (pad-data-block ,size) ,lowtag
                   :temp-tn ,temp-tn
                   :flag-tn ,flag-tn)
       (when ,type-code
         (inst lr ,temp-tn (logior (ash (1- ,size) n-widetag-bits) ,type-code))
         (storew ,temp-tn ,result-tn 0 ,lowtag))
       ,@body)))

(defun align-csp (temp)
  ;; is used for stack allocation of dynamic-extent objects
  (let ((aligned (gen-label)))
    (inst andi. temp csp-tn lowtag-mask)
    (inst beq aligned)
    (inst addi csp-tn csp-tn n-word-bytes)
    (storew zero-tn csp-tn -1)
    (emit-label aligned)))


;;;; Error Code
(defun emit-error-break (vop kind code values)
  (assemble ()
    (when vop
      (note-this-location vop :internal-error))
    (inst unimp kind)
    (with-adjustable-vector (vector)
      (write-var-integer code vector)
      (dolist (tn values)
        (write-var-integer (make-sc-offset (sc-number (tn-sc tn))
                                           (or (tn-offset tn) 0))
                           vector))
      (inst byte (length vector))
      (dotimes (i (length vector))
        (inst byte (aref vector i)))
      (emit-alignment word-shift))))

(defun error-call (vop error-code &rest values)
  #!+sb-doc
  "Cause an error.  ERROR-CODE is the error to cause."
  (emit-error-break vop error-trap (error-number-or-lose error-code) values))

(defun generate-error-code (vop error-code &rest values)
  #!+sb-doc
  "Generate-Error-Code Error-code Value*
  Emit code for an error with the specified Error-Code and context Values."
  (assemble (*elsewhere*)
    (let ((start-lab (gen-label)))
      (emit-label start-lab)
      (emit-error-break vop error-trap (error-number-or-lose error-code) values)
      start-lab)))

;;;; PSEUDO-ATOMIC

;;; handy macro for making sequences look atomic
;;;
;;; FLAG-TN must be wired to NL3. If a deferred interrupt happens
;;; while we have the low bits of ALLOC-TN set, we add a "large"
;;; constant to FLAG-TN. On exit, we add FLAG-TN to ALLOC-TN which (a)
;;; aligns ALLOC-TN again and (b) makes ALLOC-TN go negative. We then
;;; trap if ALLOC-TN's negative (handling the deferred interrupt) and
;;; using FLAG-TN - minus the large constant - to correct ALLOC-TN.
(defmacro pseudo-atomic ((flag-tn) &body forms)
  `(progn
     (without-scheduling ()
       ;; Extra debugging stuff:
       #+debug
       (progn
         (inst andi. ,flag-tn alloc-tn lowtag-mask)
         (inst twi :ne ,flag-tn 0))
       (inst ori alloc-tn alloc-tn pseudo-atomic-flag))
     ,@forms
     (without-scheduling ()
       (inst subi alloc-tn alloc-tn pseudo-atomic-flag)
       ;; Now test to see if the pseudo-atomic interrupted bit is set.
       (inst andi. ,flag-tn alloc-tn pseudo-atomic-interrupted-flag)
       (inst twi :ne ,flag-tn 0))
     #+debug
     (progn
       (inst andi. ,flag-tn alloc-tn lowtag-mask)
       (inst twi :ne ,flag-tn 0))))

(def!macro with-pinned-objects ((&rest objects) &body body)
  "Arrange with the garbage collector that the pages occupied by
OBJECTS will not be moved in memory for the duration of BODY.
Useful for e.g. foreign calls where another thread may trigger
garbage collection.  This is currently implemented by disabling GC"
  #!-gencgc
  (declare (ignore objects))            ; should we eval these for side-effect?
  #!-gencgc
  `(without-gcing
    ,@body)
  #!+gencgc
  `(let ((*pinned-objects* (list* ,@objects *pinned-objects*)))
     ,@body))
