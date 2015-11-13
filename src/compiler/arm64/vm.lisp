;;;; miscellaneous VM definition noise for the ARM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")


;;;; register specs

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *register-names* (make-array 32 :initial-element nil)))

(macrolet ((defreg (name offset)
             (let ((offset-sym (symbolicate name "-OFFSET")))
               `(eval-when (:compile-toplevel :load-toplevel :execute)
                  (def!constant ,offset-sym ,offset)
                  (setf (svref *register-names* ,offset-sym) ,(symbol-name name)))))

           (defregset (name &rest regs)
             `(eval-when (:compile-toplevel :load-toplevel :execute)
                (defparameter ,name
                  (list ,@(mapcar #'(lambda (name)
                                      (symbolicate name "-OFFSET")) regs))))))

  (defreg nl0 0)
  (defreg nl1 1)
  (defreg nl2 2)
  (defreg nl3 3)
  (defreg nl4 4)
  (defreg nl5 5)
  (defreg nl6 6)
  (defreg nl7 7)
  (defreg nl8 8)
  (defreg nl9 9)

  (defreg r0 10)
  (defreg r1 11)
  (defreg r2 12)
  (defreg r3 13)
  (defreg r4 14)
  (defreg r5 15)
  (defreg r6 16)
  (defreg r7 17)
  (defreg r8 18)
  (defreg r9 19)

  #!+sb-thread
  (defreg thread 20)
  #!-sb-thread
  (defreg r10 20)

  (defreg lexenv 21)

  (defreg nargs 22)
  (defreg nfp 23)
  (defreg ocfp 24)
  (defreg cfp 25)
  (defreg csp 26)
  (defreg tmp 27)
  (defreg null 28)
  (defreg code 29)
  (defreg lr 30)
  (defreg nsp 31)
  (defreg zr 31)

  (defregset system-regs
      null cfp nsp lr code)

  (defregset descriptor-regs
      r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 #!-sb-thread r10 lexenv)

  (defregset non-descriptor-regs
      nl0 nl1 nl2 nl3 nl4 nl5 nl6 nl7 nl8 nl9 nargs nfp ocfp)

  ;; registers used to pass arguments
  ;;
  ;; the number of arguments/return values passed in registers
  (def!constant  register-arg-count 4)
  ;; names and offsets for registers used to pass arguments
  (defregset *register-arg-offsets*  r0 r1 r2 r3)
  (defparameter *register-arg-names* '(r0 r1 r2 r3)))


;;;; SB and SC definition:

(define-storage-base registers :finite :size 32)
(define-storage-base control-stack :unbounded :size 2 :size-increment 1)
(define-storage-base non-descriptor-stack :unbounded :size 0)
(define-storage-base constant :non-packed)
(define-storage-base immediate-constant :non-packed)
(define-storage-base float-registers :finite :size 32)

;;;
;;; Handy macro so we don't have to keep changing all the numbers whenever
;;; we insert a new storage class.
;;;
(defmacro define-storage-classes (&rest classes)
  (do ((forms (list 'progn)
              (let* ((class (car classes))
                     (sc-name (car class))
                     (constant-name (intern (concatenate 'simple-string
                                                         (string sc-name)
                                                         "-SC-NUMBER"))))
                (list* `(define-storage-class ,sc-name ,index
                          ,@(cdr class))
                       `(def!constant ,constant-name ,index)
                       forms)))
       (index 0 (1+ index))
       (classes classes (cdr classes)))
      ((null classes)
       (nreverse forms))))

(define-storage-classes

    ;; Non-immediate contstants in the constant pool
    (constant constant)

    ;; NULL is in a register.
    (null immediate-constant)

  ;; Anything else that can be an immediate.
  (immediate immediate-constant)


  ;; **** The stacks.

  ;; The control stack.  (Scanned by GC)
  (control-stack control-stack)

  ;; We put ANY-REG and DESCRIPTOR-REG early so that their SC-NUMBER
  ;; is small and therefore the error trap information is smaller.
  ;; Moving them up here from their previous place down below saves
  ;; ~250K in core file size.  --njf, 2006-01-27

  ;; Immediate descriptor objects.  Don't have to be seen by GC, but nothing
  ;; bad will happen if they are.  (fixnums, characters, header values, etc).
  (any-reg
   registers
   :locations #.(append non-descriptor-regs descriptor-regs)
   :constant-scs (immediate)
   :save-p t
   :alternate-scs (control-stack))

  ;; Pointer descriptor objects.  Must be seen by GC.
  (descriptor-reg registers
                  :locations #.descriptor-regs
                  :constant-scs (constant null immediate)
                  :save-p t
                  :alternate-scs (control-stack))

  (32-bit-reg registers
              :locations #.(loop for i below 32 collect i))

  ;; The non-descriptor stacks.
  (signed-stack non-descriptor-stack)    ; (signed-byte 64)
  (unsigned-stack non-descriptor-stack)  ; (unsigned-byte 64)
  (character-stack non-descriptor-stack) ; non-descriptor characters.
  (sap-stack non-descriptor-stack)       ; System area pointers.
  (single-stack non-descriptor-stack)    ; single-floats
  (double-stack non-descriptor-stack) ; double floats.
  (complex-single-stack non-descriptor-stack)
  (complex-double-stack non-descriptor-stack :element-size 2 :alignment 2)

  ;; **** Things that can go in the integer registers.

  ;; Non-Descriptor characters
  (character-reg registers
                 :locations #.non-descriptor-regs
                 :constant-scs (immediate)
                 :save-p t
                 :alternate-scs (character-stack))

  ;; Non-Descriptor SAP's (arbitrary pointers into address space)
  (sap-reg registers
           :locations #.non-descriptor-regs
           :constant-scs (immediate)
           :save-p t
           :alternate-scs (sap-stack))

  ;; Non-Descriptor (signed or unsigned) numbers.
  (signed-reg registers
              :locations #.non-descriptor-regs
              :constant-scs (immediate)
              :save-p t
              :alternate-scs (signed-stack))
  (unsigned-reg registers
                :locations #.non-descriptor-regs
                :constant-scs (immediate)
                :save-p t
                :alternate-scs (unsigned-stack))

  ;; Random objects that must not be seen by GC.  Used only as temporaries.
  (non-descriptor-reg registers
                      :locations #.non-descriptor-regs)

  ;; Pointers to the interior of objects.  Used only as a temporary.
  (interior-reg registers
                :locations (#.lr-offset))

  ;; **** Things that can go in the floating point registers.

  ;; Non-Descriptor single-floats.
  (single-reg float-registers
              :locations #.(loop for i below 32 collect i)
              :constant-scs ()
              :save-p t
              :alternate-scs (single-stack))

  ;; Non-Descriptor double-floats.
  (double-reg float-registers
              :locations #.(loop for i below 32 collect i)
              :constant-scs ()
              :save-p t
              :alternate-scs (double-stack))

  (complex-single-reg float-registers
                      :locations #.(loop for i below 32 collect i)
                      :constant-scs ()
                      :save-p t
                      :alternate-scs (complex-single-stack))

  (complex-double-reg float-registers
                      :locations #.(loop for i below 32 collect i)
                      :constant-scs ()
                      :save-p t
                      :alternate-scs (complex-double-stack))

  ;; A catch or unwind block.
  (catch-block control-stack :element-size catch-block-size))

;;;; Make some random tns for important registers.

(macrolet ((defregtn (name sc)
               (let ((offset-sym (symbolicate name "-OFFSET"))
                     (tn-sym (symbolicate name "-TN")))
                 `(defparameter ,tn-sym
                   (make-random-tn :kind :normal
                    :sc (sc-or-lose ',sc)
                    :offset ,offset-sym)))))

  (defregtn null descriptor-reg)
  (defregtn code descriptor-reg)
  (defregtn tmp any-reg)

  (defregtn nargs any-reg)
  (defregtn ocfp any-reg)
  (defregtn nsp any-reg)
  (defregtn zr any-reg)
  (defregtn cfp any-reg)
  (defregtn csp any-reg)
  (defregtn lr interior-reg)
  #!+sb-thread
  (defregtn thread interior-reg))

;;; If VALUE can be represented as an immediate constant, then return the
;;; appropriate SC number, otherwise return NIL.
(defun immediate-constant-sc (value)
  (typecase value
    (null
     (sc-number-or-lose 'null))
    ((or (integer #.sb!xc:most-negative-fixnum #.sb!xc:most-positive-fixnum)
         character)
     (sc-number-or-lose 'immediate))
    (symbol
     (if (static-symbol-p value)
         (sc-number-or-lose 'immediate)
         nil))))

(defun boxed-immediate-sc-p (sc)
  (or (eql sc (sc-number-or-lose 'null))
      (eql sc (sc-number-or-lose 'immediate))))

;;;; function call parameters

;;; the SC numbers for register and stack arguments/return values
(def!constant immediate-arg-scn (sc-number-or-lose 'any-reg))
(def!constant control-stack-arg-scn (sc-number-or-lose 'control-stack))

;;; offsets of special stack frame locations
(def!constant ocfp-save-offset 0)
(def!constant lra-save-offset 1)
(def!constant nfp-save-offset 2)

;;; This is used by the debugger.
;;; < nyef> Ah, right. So, SINGLE-VALUE-RETURN-BYTE-OFFSET doesn't apply to x86oids or ARM.
(def!constant single-value-return-byte-offset 0)


;;; A list of TN's describing the register arguments.
;;;
(defparameter *register-arg-tns*
  (mapcar #'(lambda (n)
              (make-random-tn :kind :normal
                              :sc (sc-or-lose 'descriptor-reg)
                              :offset n))
          *register-arg-offsets*))

;;; This function is called by debug output routines that want a pretty name
;;; for a TN's location.  It returns a thing that can be printed with PRINC.
(defun location-print-name (tn)
  (declare (type tn tn))
  (let ((sb (sb-name (sc-sb (tn-sc tn))))
        (offset (tn-offset tn)))
    (ecase sb
      (registers (format nil "~:[~;W~]~A"
                         (sc-is tn 32-bit-reg)
                         (svref *register-names* offset)))
      (control-stack (format nil "CS~D" offset))
      (non-descriptor-stack (format nil "NS~D" offset))
      (constant (format nil "Const~D" offset))
      (immediate-constant "Immed")
      (float-registers
       (format nil "~A~D"
               (sc-case tn
                 (single-reg "S")
                 ((double-reg complex-single-reg) "D")
                 (complex-double-reg "Q"))
               offset)))))

(defun combination-implementation-style (node)
  (flet ((valid-funtype (args result)
           (sb!c::valid-fun-use node
                                (sb!c::specifier-type
                                 `(function ,args ,result)))))
    (case (sb!c::combination-fun-source-name node)
      (logtest
       (cond
         ((valid-funtype '(fixnum fixnum) '*)
          (values :maybe nil))
         ((valid-funtype '(signed-word signed-word) '*)
          (values :maybe nil))
         ((valid-funtype '(word word) '*)
          (values :maybe nil))
         (t (values :default nil))))
      (logbitp
       (cond
         ((or (valid-funtype '((constant-arg (integer 0 #.(1- n-fixnum-bits))) fixnum) '*)
              (valid-funtype '((constant-arg (integer 0 #.(1- n-word-bits))) signed-word) '*)
              (valid-funtype '((constant-arg (integer 0 #.(1- n-word-bits))) word) '*))
          (values :transform '(lambda (index integer)
                               (%logbitp integer index))))
         (t (values :default nil))))
      (%ldb
       (flet ((validp (type width)
                (and (valid-funtype `((constant-arg (mod ,width))
                                      (constant-arg (mod ,width))
                                      ,type)
                                    'unsigned-byte)
                     (destructuring-bind (size posn integer)
                         (sb!c::basic-combination-args node)
                       (declare (ignore integer))
                       (and (plusp (sb!c::lvar-value posn))
                            (<= (+ (sb!c::lvar-value size)
                                   (sb!c::lvar-value posn))
                                width))))))
         (if (or (validp 'fixnum n-fixnum-bits)
                 (validp '(signed-byte 64) 64)
                 (validp '(unsigned-byte 64) 64))
             (values :transform '(lambda (size posn integer)
                                  (%%ldb integer size posn)))
             (values :default nil))))
      (%dpb
       (flet ((validp (type width)
                (and (valid-funtype `(,type
                                      (constant-arg (mod ,width))
                                      (constant-arg (mod ,width))
                                      ,type)
                                    'integer)
                     (destructuring-bind (newbyte size posn integer)
                         (sb!c::basic-combination-args node)
                       (declare (ignore integer newbyte))
                       (and (plusp (sb!c::lvar-value posn))
                            (<= (+ (sb!c::lvar-value size)
                                   (sb!c::lvar-value posn))
                                width))))))
         (if (or (validp 'fixnum n-fixnum-bits)
                 (validp '(signed-byte 64) 64)
                 (validp '(unsigned-byte 64) 64))
             (values :transform '(lambda (newbyte size posn integer)
                                  (%%dpb newbyte size posn integer)))
             (values :default nil))))
      (t (values :default nil)))))

(defun primitive-type-indirect-cell-type (ptype)
  (declare (ignore ptype))
  nil)

(defun 32-bit-reg (tn)
  (make-random-tn :kind :normal
                  :sc (sc-or-lose '32-bit-reg)
                  :offset (tn-offset tn)))
