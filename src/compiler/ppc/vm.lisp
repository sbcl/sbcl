;;;; miscellaneous VM definition noise for the PPC

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;; NUMBER-STACK-DISPLACEMENT
;;;
;;; The number of bytes reserved above the number stack pointer.  These
;;; slots are required by architecture, mostly (?) to make C backtrace
;;; work. This must be a power of 2 - see BYTES-REQUIRED-FOR-NUMBER-STACK.
;;;
(def!constant number-stack-displacement
  (* #!-darwin 2
     #!+darwin 8
     n-word-bytes))

;;;; Define the registers

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

  (defreg zero 0)
  (defreg nsp 1)
  (defreg rtoc 2)                         ; May be "NULL" someday.
  (defreg nl0 3)
  (defreg nl1 4)
  (defreg nl2 5)
  (defreg nl3 6)
  (defreg nl4 7)
  (defreg nl5 8)
  (defreg nl6 9)
  (defreg fdefn 10)                     ; was nl7
  (defreg nargs 11)
  ;; FIXME: some kind of comment here would be nice.
  ;;
  ;; FIXME II: this also reveals the need to autogenerate lispregs.h
  #!+darwin  (defreg cfunc 12)
  #!-darwin  (defreg nfp 12)
  #!+darwin  (defreg nfp 13)
  #!-darwin  (defreg cfunc 13)
  (defreg bsp 14)
  (defreg cfp 15)
  (defreg csp 16)
  (defreg alloc 17)
  (defreg null 18)
  (defreg code 19)
  (defreg cname 20)
  (defreg lexenv 21)
  (defreg ocfp 22)
  (defreg lra 23)
  (defreg a0 24)
  (defreg a1 25)
  (defreg a2 26)
  (defreg a3 27)
  (defreg l0 28)
  (defreg l1 29)
  (defreg #!-sb-thread l2 #!+sb-thread thread 30)
  (defreg lip 31)

  (defregset non-descriptor-regs
      nl0 nl1 nl2 nl3 nl4 nl5 nl6 #+nil nl7 cfunc nargs nfp)

  (defregset descriptor-regs
      fdefn a0 a1 a2 a3  ocfp lra cname lexenv l0 l1 #!-sb-thread l2 )


 (defregset *register-arg-offsets*  a0 a1 a2 a3)
 (defparameter register-arg-names '(a0 a1 a2 a3)))



;;;; SB and SC definition:

(define-storage-base registers :finite :size 32)
(define-storage-base float-registers :finite :size 32)
(define-storage-base control-stack :unbounded :size 8)
(define-storage-base non-descriptor-stack :unbounded :size 0)
(define-storage-base constant :non-packed)
(define-storage-base immediate-constant :non-packed)

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

  ;; ZERO and NULL are in registers.
  (zero immediate-constant)
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
   :constant-scs (zero immediate)
   :save-p t
   :alternate-scs (control-stack))

  ;; Pointer descriptor objects.  Must be seen by GC.
  (descriptor-reg registers
   :locations #.descriptor-regs
   :constant-scs (constant null immediate)
   :save-p t
   :alternate-scs (control-stack))

  ;; The non-descriptor stacks.
  (signed-stack non-descriptor-stack) ; (signed-byte 32)
  (unsigned-stack non-descriptor-stack) ; (unsigned-byte 32)
  (character-stack non-descriptor-stack) ; non-descriptor characters.
  (sap-stack non-descriptor-stack) ; System area pointers.
  (single-stack non-descriptor-stack) ; single-floats
  (double-stack non-descriptor-stack
                :element-size 2 :alignment 2) ; double floats.
  (complex-single-stack non-descriptor-stack :element-size 2)
  (complex-double-stack non-descriptor-stack :element-size 4 :alignment 2)


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
   :constant-scs (zero immediate)
   :save-p t
   :alternate-scs (signed-stack))
  (unsigned-reg registers
   :locations #.non-descriptor-regs
   :constant-scs (zero immediate)
   :save-p t
   :alternate-scs (unsigned-stack))

  ;; Random objects that must not be seen by GC.  Used only as temporaries.
  (non-descriptor-reg registers
   :locations #.non-descriptor-regs)

  ;; Pointers to the interior of objects.  Used only as a temporary.
  (interior-reg registers
   :locations (#.lip-offset))


  ;; **** Things that can go in the floating point registers.

  ;; Non-Descriptor single-floats.
  (single-reg float-registers
   :locations #.(loop for i from 0 to 31 collect i)
   ;; ### Note: We really should have every location listed, but then we
   ;; would have to make load-tns work with element-sizes other than 1.
   :constant-scs ()
   :save-p t
   :alternate-scs (single-stack))

  ;; Non-Descriptor double-floats.
  (double-reg float-registers
   :locations #.(loop for i from 0 to 31 collect i)
   ;; ### Note: load-tns don't work with an element-size other than 1.
   ;; :element-size 2 :alignment 2
   :constant-scs ()
   :save-p t
   :alternate-scs (double-stack))

  (complex-single-reg float-registers
   :locations #.(loop for i from 0 to 30 by 2 collect i)
   :element-size 2
   :constant-scs ()
   :save-p t
   :alternate-scs (complex-single-stack))

  (complex-double-reg float-registers
   :locations #.(loop for i from 0 to 30 by 2 collect i)
   :element-size 2
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

  (defregtn zero any-reg)
  (defregtn lip interior-reg)
  (defregtn null descriptor-reg)
  (defregtn code descriptor-reg)
  (defregtn alloc any-reg)
  (defregtn lra descriptor-reg)
  (defregtn lexenv descriptor-reg)

  (defregtn nargs any-reg)
  (defregtn bsp any-reg)
  (defregtn csp any-reg)
  (defregtn cfp any-reg)
  (defregtn ocfp any-reg)
  (defregtn nsp any-reg))

;;; If VALUE can be represented as an immediate constant, then return the
;;; appropriate SC number, otherwise return NIL.
(defun immediate-constant-sc (value)
  (typecase value
    ((integer 0 0)
     (sc-number-or-lose 'zero))
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
  (or (eql sc (sc-number-or-lose 'zero))
      (eql sc (sc-number-or-lose 'null))
      (eql sc (sc-number-or-lose 'immediate))))

;;; A predicate to see if a character can be used as an inline
;;; constant (the immediate field in the instruction used is sixteen
;;; bits wide, which is not the same as any defined subtype of
;;; CHARACTER).
(defun inlinable-character-constant-p (char)
  (and (characterp char)
       (< (char-code char) #x10000)))

;;;; function call parameters

;;; the SC numbers for register and stack arguments/return values
(def!constant immediate-arg-scn (sc-number-or-lose 'any-reg))
(def!constant control-stack-arg-scn (sc-number-or-lose 'control-stack))

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; offsets of special stack frame locations
(def!constant ocfp-save-offset 0)
(def!constant lra-save-offset 1)
(def!constant nfp-save-offset 2)

;;; the number of arguments/return values passed in registers
(def!constant register-arg-count 4)

;;; names to use for the argument registers


) ; EVAL-WHEN


;;; A list of TN's describing the register arguments.
;;;
(defparameter *register-arg-tns*
  (mapcar #'(lambda (n)
              (make-random-tn :kind :normal
                              :sc (sc-or-lose 'descriptor-reg)
                              :offset n))
          *register-arg-offsets*))

#!+sb-thread
(defparameter thread-base-tn
  (make-random-tn :kind :normal :sc (sc-or-lose 'unsigned-reg)
                  :offset thread-offset))

(export 'single-value-return-byte-offset)

;;; This is used by the debugger.
(def!constant single-value-return-byte-offset 8)

;;; This function is called by debug output routines that want a pretty name
;;; for a TN's location.  It returns a thing that can be printed with PRINC.
(defun location-print-name (tn)
  (declare (type tn tn))
  (let ((sb (sb-name (sc-sb (tn-sc tn))))
        (offset (tn-offset tn)))
    (ecase sb
      (registers (or (svref *register-names* offset)
                     (format nil "R~D" offset)))
      (float-registers (format nil "F~D" offset))
      (control-stack (format nil "CS~D" offset))
      (non-descriptor-stack (format nil "NS~D" offset))
      (constant (format nil "Const~D" offset))
      (immediate-constant "Immed"))))

(defun combination-implementation-style (node)
  (declare (type sb!c::combination node))
  (flet ((valid-funtype (args result)
           (sb!c::valid-fun-use node
                                (sb!c::specifier-type
                                 `(function ,args ,result)))))
    (case (sb!c::combination-fun-source-name node)
      (logtest
       (cond
         ((or (valid-funtype '(fixnum fixnum) '*)
              (valid-funtype '((signed-byte 32) (signed-byte 32)) '*)
              (valid-funtype '((unsigned-byte 32) (unsigned-byte 32)) '*))
          (values :maybe nil))
         (t (values :default nil))))
      (logbitp
       (cond
         ((or (valid-funtype '((constant-arg (integer 0 29)) fixnum) '*)
              (valid-funtype '((constant-arg (integer 0 31)) (signed-byte 32)) '*)
              (valid-funtype '((constant-arg (integer 0 31)) (unsigned-byte 32)) '*))
          (values :transform '(lambda (index integer)
                               (%logbitp integer index))))
         (t (values :default nil))))
      ;; FIXME: can handle MIN and MAX here
      (%ldb
       (flet ((validp (type width)
                (and (valid-funtype `((constant-arg (integer 1 29))
                                      (constant-arg (mod ,width))
                                      ,type)
                                    'fixnum)
                     (destructuring-bind (size posn integer)
                         (sb!c::basic-combination-args node)
                       (declare (ignore integer))
                       (<= (+ (sb!c::lvar-value size)
                              (sb!c::lvar-value posn))
                           width)))))
         (if (or (validp 'fixnum 29)
                 (validp '(signed-byte 32) 32)
                 (validp '(unsigned-byte 32) 32))
             (values :transform '(lambda (size posn integer)
                                  (%%ldb integer size posn)))
             (values :default nil))))
      (t (values :default nil)))))

(defun primitive-type-indirect-cell-type (ptype)
  (declare (ignore ptype))
  nil)
