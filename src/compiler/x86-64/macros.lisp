;;;; a bunch of handy macros for the x86

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; instruction-like macros

(defmacro move (dst src)
  #!+sb-doc
  "Move SRC into DST unless they are location=."
  (once-only ((n-dst dst)
              (n-src src))
    `(unless (location= ,n-dst ,n-src)
       (inst mov ,n-dst ,n-src))))

(defmacro make-ea-for-object-slot (ptr slot lowtag)
  `(make-ea :qword :base ,ptr :disp (- (* ,slot n-word-bytes) ,lowtag)))
(defmacro make-ea-for-object-slot-half (ptr slot lowtag)
  `(make-ea :dword :base ,ptr :disp (- (* ,slot n-word-bytes) ,lowtag)))

(defmacro loadw (value ptr &optional (slot 0) (lowtag 0))
  `(inst mov ,value (make-ea-for-object-slot ,ptr ,slot ,lowtag)))

(defmacro storew (value ptr &optional (slot 0) (lowtag 0))
  (once-only ((value value))
    `(cond ((and (integerp ,value)
                 (not (typep ,value '(signed-byte 32))))
            (multiple-value-bind (lo hi) (dwords-for-quad ,value)
              (inst mov (make-ea-for-object-slot-half
                         ,ptr ,slot ,lowtag) lo)
              (inst mov (make-ea-for-object-slot-half
                         ,ptr (+ ,slot 1/2) ,lowtag) hi)))
           (t
            (inst mov (make-ea-for-object-slot ,ptr ,slot ,lowtag) ,value)))))

(defmacro pushw (ptr &optional (slot 0) (lowtag 0))
  `(inst push (make-ea-for-object-slot ,ptr ,slot ,lowtag)))

(defmacro popw (ptr &optional (slot 0) (lowtag 0))
  `(inst pop (make-ea-for-object-slot ,ptr ,slot ,lowtag)))

;;;; macros to generate useful values

(defmacro load-symbol (reg symbol)
  `(inst mov ,reg (+ nil-value (static-symbol-offset ,symbol))))

(defmacro make-ea-for-symbol-value (symbol)
  `(make-ea :qword
    :disp (+ nil-value
           (static-symbol-offset ',symbol)
           (ash symbol-value-slot word-shift)
           (- other-pointer-lowtag))))

(defmacro load-symbol-value (reg symbol)
  `(inst mov ,reg (make-ea-for-symbol-value ,symbol)))

(defmacro store-symbol-value (reg symbol)
  `(inst mov (make-ea-for-symbol-value ,symbol) ,reg))

#!+sb-thread
(defmacro make-ea-for-symbol-tls-index (symbol)
  `(make-ea :qword
    :disp (+ nil-value
           (static-symbol-offset ',symbol)
           (ash symbol-tls-index-slot word-shift)
           (- other-pointer-lowtag))))

#!+sb-thread
(defmacro load-tl-symbol-value (reg symbol)
  `(progn
    (inst mov ,reg (make-ea-for-symbol-tls-index ,symbol))
    (inst mov ,reg (make-ea :qword :base thread-base-tn :scale 1 :index ,reg))))
#!-sb-thread
(defmacro load-tl-symbol-value (reg symbol) `(load-symbol-value ,reg ,symbol))

#!+sb-thread
(defmacro store-tl-symbol-value (reg symbol temp)
  `(progn
    (inst mov ,temp (make-ea-for-symbol-tls-index ,symbol))
    (inst mov (make-ea :qword :base thread-base-tn :scale 1 :index ,temp) ,reg)))
#!-sb-thread
(defmacro store-tl-symbol-value (reg symbol temp)
  (declare (ignore temp))
  `(store-symbol-value ,reg ,symbol))

(defmacro load-type (target source &optional (offset 0))
  #!+sb-doc
  "Loads the type bits of a pointer into target independent of
   byte-ordering issues."
  (once-only ((n-target target)
              (n-source source)
              (n-offset offset))
    (ecase *backend-byte-order*
      (:little-endian
       `(inst mov ,n-target
              (make-ea :byte :base ,n-source :disp ,n-offset)))
      (:big-endian
       `(inst mov ,n-target
              (make-ea :byte :base ,n-source :disp (+ ,n-offset 4)))))))

;;;; allocation helpers

;;; All allocation is done by calls to assembler routines that
;;; eventually invoke the C alloc() function.

;;; Emit code to allocate an object with a size in bytes given by
;;; Size. The size may be an integer of a TN. If Inline is a VOP
;;; node-var then it is used to make an appropriate speed vs size
;;; decision.

(defun allocation-dynamic-extent (alloc-tn size)
  (inst sub rsp-tn size)
  ;; see comment in x86/macros.lisp implementation of this
  (inst and rsp-tn #.(lognot lowtag-mask))
  (aver (not (location= alloc-tn rsp-tn)))
  (inst mov alloc-tn rsp-tn)
  (values))

;;; This macro should only be used inside a pseudo-atomic section,
;;; which should also cover subsequent initialization of the
;;; object.
(defun allocation-tramp (alloc-tn size &optional ignored)
  (declare (ignore ignored))
  (inst push size)
  (inst lea r13-tn (make-ea :qword
                            :disp (make-fixup "alloc_tramp" :foreign)))
  (inst call r13-tn)
  (inst pop alloc-tn)
  (values))

(defun allocation (alloc-tn size &optional ignored dynamic-extent)
  (declare (ignore ignored))
  (when dynamic-extent
    (allocation-dynamic-extent alloc-tn size)
    (return-from allocation (values)))
  (let ((NOT-INLINE (gen-label))
        (DONE (gen-label))
        ;; Yuck.
        (in-elsewhere (eq *elsewhere* sb!assem::**current-segment**))
        ;; thread->alloc_region.free_pointer
        (free-pointer
         #!+sb-thread
         (make-ea :qword
                  :base thread-base-tn :scale 1
                  :disp (* n-word-bytes thread-alloc-region-slot))
         #!-sb-thread
         (make-ea :qword
                  :scale 1 :disp
                  (make-fixup (extern-alien-name "boxed_region") :foreign)))
        ;; thread->alloc_region.end_addr
        (end-addr
         #!+sb-thread
         (make-ea :qword
                  :base thread-base-tn :scale 1
                  :disp (* n-word-bytes (1+ thread-alloc-region-slot)))
         #!-sb-thread
         (make-ea :qword
                  :scale 1 :disp
                  (make-fixup (extern-alien-name "boxed_region") :foreign 8))))
    (cond (in-elsewhere
           (allocation-tramp alloc-tn size))
          (t
           (unless (and (tn-p size) (location= alloc-tn size))
             (inst mov alloc-tn size))
           (inst add alloc-tn free-pointer)
           (inst cmp end-addr alloc-tn)
           (inst jmp :be NOT-INLINE)
           (inst xchg free-pointer alloc-tn)
           (emit-label DONE)
           (assemble (*elsewhere*)
             (emit-label NOT-INLINE)
             (cond ((numberp size)
                    (allocation-tramp alloc-tn size))
                   (t
                    (inst sub alloc-tn free-pointer)
                    (allocation-tramp alloc-tn alloc-tn)))
             (inst jmp DONE))
           (values)))))

#+nil
(defun allocation (alloc-tn size &optional ignored)
  (declare (ignore ignored))
  (inst push size)
  (inst lea r13-tn (make-ea :qword
                            :disp (make-fixup "alloc_tramp" :foreign)))
  (inst call r13-tn)
  (inst pop alloc-tn)
  (values))

;;; Allocate an other-pointer object of fixed SIZE with a single word
;;; header having the specified WIDETAG value. The result is placed in
;;; RESULT-TN.
(defmacro with-fixed-allocation ((result-tn widetag size &optional inline)
                                 &body forms)
  (unless forms
    (bug "empty &body in WITH-FIXED-ALLOCATION"))
  (once-only ((result-tn result-tn) (size size))
    `(pseudo-atomic
      (allocation ,result-tn (pad-data-block ,size) ,inline)
      (storew (logior (ash (1- ,size) n-widetag-bits) ,widetag)
              ,result-tn)
      (inst lea ,result-tn
            (make-ea :qword :base ,result-tn :disp other-pointer-lowtag))
      ,@forms)))

;;;; error code
(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  (defun emit-error-break (vop kind code values)
    (let ((vector (gensym)))
      `((inst int 3)                            ; i386 breakpoint instruction
        ;; The return PC points here; note the location for the debugger.
        (let ((vop ,vop))
          (when vop
                (note-this-location vop :internal-error)))
        (inst byte ,kind)                       ; eg trap_Xyyy
        (with-adjustable-vector (,vector)       ; interr arguments
          (write-var-integer (error-number-or-lose ',code) ,vector)
          ,@(mapcar (lambda (tn)
                      `(let ((tn ,tn))
                         ;; classic CMU CL comment:
                         ;;   zzzzz jrd here. tn-offset is zero for constant
                         ;;   tns.
                         (write-var-integer (make-sc-offset (sc-number
                                                             (tn-sc tn))
                                                            (or (tn-offset tn)
                                                                0))
                                            ,vector)))
                    values)
          (inst byte (length ,vector))
          (dotimes (i (length ,vector))
            (inst byte (aref ,vector i))))))))

(defmacro error-call (vop error-code &rest values)
  #!+sb-doc
  "Cause an error. ERROR-CODE is the error to cause."
  (cons 'progn
        (emit-error-break vop error-trap error-code values)))

(defmacro generate-error-code (vop error-code &rest values)
  #!+sb-doc
  "Generate-Error-Code Error-code Value*
  Emit code for an error with the specified Error-Code and context Values."
  `(assemble (*elsewhere*)
     (let ((start-lab (gen-label)))
       (emit-label start-lab)
       (error-call ,vop ,error-code ,@values)
       start-lab)))


;;;; PSEUDO-ATOMIC

;;; This is used to wrap operations which leave untagged memory lying
;;; around.  It's an operation which the AOP weenies would describe as
;;; having "cross-cutting concerns", meaning it appears all over the
;;; place and there's no logical single place to attach documentation.
;;; grep (mostly in src/runtime) is your friend

;;; FIXME: *PSEUDO-ATOMIC-FOO* could be made into *PSEUDO-ATOMIC-BITS*,
;;; set with a single operation and cleared with SHR *PSEUDO-ATOMIC-BITS*,-2;
;;; the ATOMIC bit is bit 0, the INTERRUPTED bit is bit 1, and you check
;;; the C flag after the shift to see whether you were interrupted.

;;; FIXME: THIS NAME IS BACKWARDS!
(defmacro maybe-pseudo-atomic (really-p &body body)
  `(if ,really-p
       (progn ,@body)
       (pseudo-atomic ,@body)))

#!+sb-thread
(defmacro pseudo-atomic (&rest forms)
  (with-unique-names (label)
    `(let ((,label (gen-label)))
      (inst mov (make-ea :byte
                 :base thread-base-tn
                 :disp (* 8 thread-pseudo-atomic-interrupted-slot)) 0)
      (inst mov (make-ea :byte
                 :base thread-base-tn
                 :disp (* 8 thread-pseudo-atomic-atomic-slot))
            (fixnumize 1))
      ,@forms
      (inst mov (make-ea :byte
                 :base thread-base-tn
                 :disp (* 8 thread-pseudo-atomic-atomic-slot)) 0)
      (inst cmp (make-ea :byte
                 :base thread-base-tn
                 :disp (* 8 thread-pseudo-atomic-interrupted-slot)) 0)
      (inst jmp :eq ,label)
      ;; if PAI was set, interrupts were disabled at the same
      ;; time using the process signal mask.
      (inst break pending-interrupt-trap)
      (emit-label ,label))))


#!-sb-thread
(defmacro pseudo-atomic (&rest forms)
  (with-unique-names (label)
    `(let ((,label (gen-label)))
      ;; FIXME: The MAKE-EA noise should become a MACROLET macro or
      ;; something. (perhaps SVLB, for static variable low byte)
      (inst mov (make-ea :byte :disp (+ nil-value
                                        (static-symbol-offset
                                         '*pseudo-atomic-interrupted*)
                                        (ash symbol-value-slot word-shift)
                                        ;; FIXME: Use mask, not minus, to
                                        ;; take out type bits.
                                        (- other-pointer-lowtag)))
       0)
      (inst mov (make-ea :byte :disp (+ nil-value
                                        (static-symbol-offset
                                         '*pseudo-atomic-atomic*)
                                        (ash symbol-value-slot word-shift)
                                        (- other-pointer-lowtag)))
       (fixnumize 1))
      ,@forms
      (inst mov (make-ea :byte :disp (+ nil-value
                                        (static-symbol-offset
                                         '*pseudo-atomic-atomic*)
                                        (ash symbol-value-slot word-shift)
                                        (- other-pointer-lowtag)))
       0)
      ;; KLUDGE: Is there any requirement for interrupts to be
      ;; handled in order? It seems as though an interrupt coming
      ;; in at this point will be executed before any pending interrupts.
      ;; Or do incoming interrupts check to see whether any interrupts
      ;; are pending? I wish I could find the documentation for
      ;; pseudo-atomics.. -- WHN 19991130
      (inst cmp (make-ea :byte
                 :disp (+ nil-value
                          (static-symbol-offset
                           '*pseudo-atomic-interrupted*)
                          (ash symbol-value-slot word-shift)
                          (- other-pointer-lowtag)))
       0)
      (inst jmp :eq ,label)
      ;; if PAI was set, interrupts were disabled at the same time
      ;; using the process signal mask.
      (inst break pending-interrupt-trap)
      (emit-label ,label))))



;;;; indexed references

(defmacro define-full-reffer (name type offset lowtag scs el-type &optional translate)
  `(progn
     (define-vop (,name)
       ,@(when translate
           `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
              (index :scs (any-reg)))
       (:arg-types ,type tagged-num)
       (:results (value :scs ,scs))
       (:result-types ,el-type)
       (:generator 3                    ; pw was 5
         (inst mov value (make-ea :qword :base object :index index
                                  :disp (- (* ,offset n-word-bytes)
                                           ,lowtag)))))
     (define-vop (,(symbolicate name "-C"))
       ,@(when translate
           `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg)))
       (:info index)
       (:arg-types ,type
                   (:constant (load/store-index ,n-word-bytes ,(eval lowtag)
                                                ,(eval offset))))
       (:results (value :scs ,scs))
       (:result-types ,el-type)
       (:generator 2                    ; pw was 5
         (inst mov value (make-ea :qword :base object
                                  :disp (- (* (+ ,offset index) n-word-bytes)
                                           ,lowtag)))))))

(defmacro define-full-setter (name type offset lowtag scs el-type &optional translate)
  `(progn
     (define-vop (,name)
       ,@(when translate
           `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
              (index :scs (any-reg))
              (value :scs ,scs :target result))
       (:arg-types ,type tagged-num ,el-type)
       (:results (result :scs ,scs))
       (:result-types ,el-type)
       (:generator 4                    ; was 5
         (inst mov (make-ea :qword :base object :index index
                            :disp (- (* ,offset n-word-bytes) ,lowtag))
               value)
         (move result value)))
     (define-vop (,(symbolicate name "-C"))
       ,@(when translate
           `((:translate ,translate)))
       (:policy :fast-safe)
       (:args (object :scs (descriptor-reg))
              (value :scs ,scs :target result))
       (:info index)
       (:arg-types ,type
                   (:constant (load/store-index ,n-word-bytes ,(eval lowtag)
                                                ,(eval offset)))
                   ,el-type)
       (:results (result :scs ,scs))
       (:result-types ,el-type)
       (:generator 3                    ; was 5
         (inst mov (make-ea :qword :base object
                            :disp (- (* (+ ,offset index) n-word-bytes)
                                     ,lowtag))
               value)
         (move result value)))))

;;; helper for alien stuff.
(defmacro with-pinned-objects ((&rest objects) &body body)
  "Arrange with the garbage collector that the pages occupied by
OBJECTS will not be moved in memory for the duration of BODY.
Useful for e.g. foreign calls where another thread may trigger
garbage collection"
  `(multiple-value-prog1
       (progn
         ,@(loop for p in objects
                 collect `(push-word-on-c-stack
                           (int-sap (sb!kernel:get-lisp-obj-address ,p))))
         ,@body)
     ;; If the body returned normally, we should restore the stack pointer
     ;; for the benefit of any following code in the same function.  If
     ;; there's a non-local exit in the body, sp is garbage anyway and
     ;; will get set appropriately from {a, the} frame pointer before it's
     ;; next needed
     (pop-words-from-c-stack ,(length objects))))
