;;;; various useful macros for generating Sparc code

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
                             (find-package "SB!VM"))))
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
  #!+sb-doc
  "Loads the type bits of a pointer into target independent of
  byte-ordering issues."
  (once-only ((n-target target)
              (n-source source)
              (n-offset offset))
    ;; FIXME: although I don't understand entirely, I'm going to do
    ;; what whn does in x86/macros.lisp -- Christophe
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
           (- (ash simple-fun-code-offset word-shift) fun-pointer-lowtag))
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
    `(pseudo-atomic (:extra (pad-data-block ,size))
       (inst or ,result-tn alloc-tn other-pointer-lowtag)
       (inst li ,temp-tn (logior (ash (1- ,size) n-widetag-bits) ,type-code))
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun emit-error-break (vop kind code values)
    (let ((vector (gensym)))
      `((let ((vop ,vop))
          (when vop
            (note-this-location vop :internal-error)))
        (inst unimp ,kind)
        (with-adjustable-vector (,vector)
          (write-var-integer (error-number-or-lose ',code) ,vector)
          ,@(mapcar #'(lambda (tn)
                        `(let ((tn ,tn))
                           (write-var-integer (make-sc-offset (sc-number
                                                               (tn-sc tn))
                                                              (tn-offset tn))
                                              ,vector)))
                    values)
          (inst byte (length ,vector))
          (dotimes (i (length ,vector))
            (inst byte (aref ,vector i))))
        (emit-alignment word-shift)))))

(defmacro error-call (vop error-code &rest values)
  "Cause an error.  ERROR-CODE is the error to cause."
  (cons 'progn
        (emit-error-break vop error-trap error-code values)))


(defmacro cerror-call (vop label error-code &rest values)
  "Cause a continuable error.  If the error is continued, execution resumes at
  LABEL."
  `(progn
     (inst b ,label)
     ,@(emit-error-break vop cerror-trap error-code values)))

(defmacro generate-error-code (vop error-code &rest values)
  "Generate-Error-Code Error-code Value*
  Emit code for an error with the specified Error-Code and context Values."
  `(assemble (*elsewhere*)
     (let ((start-lab (gen-label)))
       (emit-label start-lab)
       (error-call ,vop ,error-code ,@values)
       start-lab)))

(defmacro generate-cerror-code (vop error-code &rest values)
  "Generate-CError-Code Error-code Value*
  Emit code for a continuable error with the specified Error-Code and
  context Values.  If the error is continued, execution resumes after
  the GENERATE-CERROR-CODE form."
  (with-unique-names (continue error)
    `(let ((,continue (gen-label)))
       (emit-label ,continue)
       (assemble (*elsewhere*)
         (let ((,error (gen-label)))
           (emit-label ,error)
           (cerror-call ,vop ,continue ,error-code ,@values)
           ,error)))))

;;; a handy macro for making sequences look atomic
(defmacro pseudo-atomic ((&key (extra 0)) &rest forms)
  (let ((n-extra (gensym)))
    `(let ((,n-extra ,extra))
       ;; Set the pseudo-atomic flag.
       (without-scheduling ()
         (inst add alloc-tn 4))
       ,@forms
       ;; Reset the pseudo-atomic flag.
       (without-scheduling ()
         #+nil (inst taddcctv alloc-tn (- ,n-extra 4))
        ;; Remove the pseudo-atomic flag.
        (inst add alloc-tn (- ,n-extra 4))
        ;; Check to see if pseudo-atomic interrupted flag is set (bit 0 = 1).
        (inst andcc zero-tn alloc-tn 3)
        ;; The C code needs to process this correctly and fixup alloc-tn.
        (inst t :ne pseudo-atomic-trap)))))


(def!macro with-pinned-objects ((&rest objects) &body body)
  "Arrange with the garbage collector that the pages occupied by
OBJECTS will not be moved in memory for the duration of BODY.
Useful for e.g. foreign calls where another thread may trigger
garbage collection.  This is currently implemented by disabling GC"
  (declare (ignore objects))            ;should we eval these for side-effect?
  `(without-gcing
    ,@body))
