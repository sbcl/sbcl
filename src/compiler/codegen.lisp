;;;; the implementation-independent parts of the code generator. We use
;;;; functions and information provided by the VM definition to convert
;;;; IR2 into assembly code. After emitting code, we finish the
;;;; assembly and then do the post-assembly phase.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;;; utilities used during code generation

;;; KLUDGE: the assembler can not emit backpatches comprising jump tables without
;;; knowing the boxed code header length. But there is no compiler IR2 metaobject,
;;; for SB-FASL:*ASSEMBLER-ROUTINES*. We have to return a fixed answer for that.
(defun asm-routines-boxed-header-nwords ()
  (align-up (+ sb-vm:code-constants-offset
               #+x86-64 1) ; KLUDGE: make room for 1 boxed constant
            2))
;;; the number of bytes used by the code object header
(defun component-header-length ()
  (cond #+sb-xc-host ((not (boundp '*component-being-compiled*))
                      (* sb-vm:n-word-bytes (asm-routines-boxed-header-nwords)))
        (t
         (let* ((2comp (component-info *component-being-compiled*))
                (constants (ir2-component-constants 2comp)))
           (ash (align-up (length constants) code-boxed-words-align)
                sb-vm:word-shift)))))

(defun component-n-jump-table-entries (&optional (component *component-being-compiled*))
  (ir2-component-n-jump-table-entries (component-info component)))

;;; the size of the NAME'd SB in the currently compiled component.
;;; This is useful mainly for finding the size for allocating stack
;;; frames.
(defun sb-allocated-size (name)
  (finite-sb-current-size (sb-or-lose name)))

;;; the TN that is used to hold the number stack frame-pointer in
;;; VOP's function, or NIL if no number stack frame was allocated
#-c-stack-is-control-stack
(defun current-nfp-tn (vop)
  (unless (zerop (sb-allocated-size 'non-descriptor-stack))
    (let ((block (ir2-block-block (vop-block vop))))
      (when (ir2-environment-number-stack-p
             (environment-info
              (block-environment block)))
        (ir2-component-nfp (component-info (block-component block)))))))

;;; the TN that is used to hold the number stack frame-pointer in the
;;; function designated by 2ENV, or NIL if no number stack frame was
;;; allocated
#-c-stack-is-control-stack
(defun callee-nfp-tn (2env)
  (unless (zerop (sb-allocated-size 'non-descriptor-stack))
    (when (ir2-environment-number-stack-p 2env)
      (ir2-component-nfp (component-info *component-being-compiled*)))))

;;; the TN used for passing the return PC in a local call to the function
;;; designated by 2ENV
(defun callee-return-pc-tn (2env)
  (ir2-environment-return-pc-pass 2env))

;;;; Fixups

;;; a fixup of some kind
(defstruct (fixup
            (:constructor make-fixup (name flavor &optional offset))
            (:copier nil))
  ;; the name and flavor of the fixup. The assembler makes no
  ;; assumptions about the contents of these fields; their semantics
  ;; are imposed by the dumper.
  (name nil :read-only t)
  ;; FIXME: "-flavor" and "-kind" are completedly devoid of meaning.
  ;; They former should probably be "fixup-referent-type" or "fixup-source"
  ;; to indicate that it denotes a namespace for NAME, and latter should be
  ;; "fixup-how" as it conveys a manner in which to modify encoded bytes.
  (flavor nil :read-only t)
  ;; OFFSET is an optional offset from whatever external label this
  ;; fixup refers to. Or in the case of the :CODE-OBJECT flavor of
  ;; fixups on the :X86 architecture, NAME is always NIL, so this
  ;; fixup doesn't refer to an external label, and OFFSET is an offset
  ;; from the beginning of the current code block.
  ;; A LABEL can also be used for ppc or ppc64 in which case the value
  ;; of the fixup will be the displacement to the label from CODE-TN.
  (offset 0 :type (or sb-vm:signed-word label)
            :read-only t))

;;; A FIXUP-NOTE tells you where the assembly patch is to be performed
(defstruct (fixup-note
            (:constructor make-fixup-note (kind fixup position))
            (:copier nil))
  ;; KIND is architecture-dependent (see the various 'vm' files)
  (kind nil :type symbol)
  (fixup (missing-arg) :type fixup)
  (position 0 :type fixnum))
(declaim (freeze-type fixup fixup-note))

;;; Record a FIXUP of KIND occurring at the current position in SEGMENT
(defun note-fixup (segment kind fixup)
  (emit-back-patch
   segment 0
   (lambda (segment posn)
     (push (make-fixup-note kind fixup
                            (- posn (segment-header-skew segment)))
           (sb-assem::segment-fixup-notes segment)))))

;;;; noise to emit an instruction trace

(defun trace-instruction (section vop inst args state
                          &aux (*standard-output* *compiler-trace-output*))
  (macrolet ((prev-section () `(car state))
             (prev-vop () `(cdr state)))
    (unless (eq (prev-section) section)
      (format t "in the ~A section:~%" section)
      (setf (prev-section) section))
    (unless (eq (prev-vop) vop)
      (when vop
        (format t "~%VOP ")
        (if (vop-p vop)
            (print-vop vop)
            (format *compiler-trace-output* "~S~%" vop)))
      (terpri)
      (setf (prev-vop) vop))
    (case inst
      (:label
       (format t "~A:~%" args))
      (:align
       (format t "~0,8T.align~0,8T~A~%" args))
      (t
       (format t "~0,8T~A~@[~0,8T~{~A~^, ~}~]~%" inst args))))
  (values))

;;;; GENERATE-CODE and support routines

;;; standard defaults for slots of SEGMENT objects
(defun default-segment-run-scheduler ()
  (policy (lambda-bind
           (block-home-lambda
            (block-next (component-head *component-being-compiled*))))
          (or (> speed compilation-speed) (> space compilation-speed))))

;;; Some platforms support unboxed constants immediately following the boxed
;;; code header. Such platform must implement supporting 4 functions:
;;; * CANONICALIZE-INLINE-CONSTANT: converts a constant descriptor (list) into
;;;    a canonical description, to be used as a key in an EQUAL hash table
;;;    and to guide the generation of the constant itself.
;;; * INLINE-CONSTANT-VALUE: given a canonical constant descriptor, computes
;;;    two values:
;;;     1. A label that will be used to emit the constant (usually a
;;;         sb-assem:label)
;;;     2. A value that will be returned to code generators referring to
;;;         the constant (on x86oids, an EA object)
;;; * SORT-INLINE-CONSTANTS: Receives a vector of unique constants;
;;;    the car of each entry is the constant descriptor, and the cdr the
;;;    corresponding label. Destructively returns a vector of constants
;;;    sorted in emission order. It could actually perform arbitrary
;;;    modifications to the vector, e.g. to fuse constants of different
;;;    size.
;;; * EMIT-INLINE-CONSTANT: receives a constant descriptor and its associated
;;;    label. Emits the constant.
;;;
;;; Implementing this feature lets VOP generators use sb-c:register-inline-constant
;;; to get handles (as returned by sb-vm:inline-constant-value) from constant
;;; descriptors.
;;;
#+(or arm64 mips ppc ppc64 x86 x86-64)
(defun register-inline-constant (&rest constant-descriptor)
  ;; N.B.: Please do not think yourself so clever as to declare DYNAMIC-EXTENT on
  ;; CONSTANT-DESCRIPTOR. Giving the list indefinite extent allows backends to simply
  ;; return it as the key to the hash-table in the most trivial possible implementation.
  ;; The cost of listifying a &REST arg is unnoticeable here.
  (let ((asmstream *asmstream*)
        (constant (sb-vm:canonicalize-inline-constant constant-descriptor)))
    (values ; kill the second value
     (ensure-gethash
      constant
      (asmstream-constant-table asmstream)
      (multiple-value-bind (label value) (sb-vm:inline-constant-value constant)
        (vector-push-extend (cons constant label)
                            (asmstream-constant-vector asmstream))
        value)))))
#-(or arm64 mips ppc ppc64 x86 x86-64)
(progn (defun sb-vm:sort-inline-constants (constants) constants)
       (defun sb-vm:emit-inline-constant (&rest args)
         (error "EMIT-INLINE-CONSTANT called with ~S" args)))
;;; Emit the subset of inline constants which represent jump tables
;;; and remove those constants so that they don't get emitted again.
(defun emit-jump-tables (ir2-component)
  ;; Other backends will probably need relative jump tables instead
  ;; of absolute tables because of the problem of needing to load
  ;; the LIP register prior to loading an arbitrary PC.
  (let* ((asmstream *asmstream*)
         (constants (asmstream-constant-vector asmstream))
         (section (asmstream-data-section asmstream))
         (nwords 0))
    (collect ((jump-tables) (other))
      (dovector (constant constants)
        ;; Constant is ((category . data) . label)
        (cond ((eq (caar constant) :jump-table)
               (incf nwords (length (cdar constant)))
               (jump-tables constant))
              (t
               (other constant))))
      ;; Preface the unboxed data with a count of jump-table words,
      ;; including the count itself as 1 word.
      ;; On average this will add another padding word half of the time
      ;; depending on the number of boxed constants. We could reduce space
      ;; by storing one bit in the header indicating whether or not there
      ;; is a jump table. I don't think that's worth the trouble.
      (emit section `(.lispword ,(1+ nwords)))
      (when (plusp nwords)
        (setf (ir2-component-n-jump-table-entries ir2-component) nwords)
        (dolist (constant (jump-tables))
          (sb-vm:emit-inline-constant section (car constant) (cdr constant)))
        (let ((nremaining (length (other))))
          (adjust-array constants nremaining
                        :fill-pointer nremaining
                        :initial-contents (other)))))))
;;; Return T if and only if there were any constants emitted.
(defun emit-inline-constants ()
  (let* ((asmstream *asmstream*)
         (constants (asmstream-constant-vector asmstream))
         (section (asmstream-data-section asmstream)))
    (when (plusp (length constants))
      (dovector (constant (sb-vm:sort-inline-constants constants) t)
        (sb-vm:emit-inline-constant section (car constant) (cdr constant))))))

;; Collect "static" count of number of times each vop is employed.
;; (as opposed to "dynamic" - how many times its code is hit at runtime)
(defglobal *static-vop-usage-counts* nil)
(defparameter *do-instcombine-pass* t)

(defun generate-code (component &aux (ir2-component (component-info component)))
  (declare (type ir2-component ir2-component))
  (when *compiler-trace-output*
    (let ((*print-pretty* nil)) ; force 1 line
      (format *compiler-trace-output* "~|~%assembly code for ~S~2%" component)))
  (let* ((prev-env nil)
         (sb-vm::*adjustable-vectors* nil)
         ;; The first function's alignment word is zero-filled, but subsequent
         ;; ones can use a NOP which helps the disassembler not lose sync.
         (filler-pattern 0)
         (asmstream (make-asmstream))
         (*asmstream* asmstream))
    (declare (special sb-vm::*adjustable-vectors*))

    (emit (asmstream-elsewhere-section asmstream)
          (asmstream-elsewhere-label asmstream))

    (do-ir2-blocks (block component)
      (let ((1block (ir2-block-block block)))
        (when (and (eq (block-info 1block) block)
                   (block-start 1block))
          (assemble (:code 'nil) ; bind **CURRENT-VOP** to nil
            ;; Align first emitted block of each loop: x86 and x86-64 both
            ;; like 16 byte alignment, however, since x86 aligns code objects
            ;; on 8 byte boundaries we cannot guarantee proper loop alignment
            ;; there (yet.)  Only x86-64 does something with ALIGNP, but
            ;; it may be useful in the future.
            ;; FIXME: see comment in ASSEMBLE-SECTIONS - we *can* enforce larger
            ;; alignment than the size of a cons cell.
            (let ((alignp (let ((cloop (block-loop 1block)))
                            (when (and cloop
                                       (loop-tail cloop)
                                       (not (loop-info cloop)))
                              ;; Mark the loop as aligned by saving the IR1 block aligned.
                              (setf (loop-info cloop) 1block)
                              filler-pattern))))
              (setf filler-pattern :long-nop)
              (emit-block-header (block-label 1block)
                                 (ir2-block-%trampoline-label block)
                                 (ir2-block-dropped-thru-to block)
                                 alignp)))
          (let ((env (block-environment 1block)))
            (unless (eq env prev-env)
              (let ((lab (gen-label "environment elsewhere start")))
                (setf (ir2-environment-elsewhere-start (environment-info env))
                      lab)
                (emit (asmstream-elsewhere-section asmstream) lab))
              (setq prev-env env)))))
      (do ((vop (ir2-block-start-vop block) (vop-next vop)))
          ((null vop))
        (let ((gen (vop-info-generator-function (vop-info vop))))
          (awhen *static-vop-usage-counts*
            (let ((name (vop-name vop)))
              (incf (gethash name it 0))))
          (assemble (:code vop)
            (cond ((not gen)
                   (format t
                           "missing generator for ~S~%"
                           (template-name (vop-info vop))))
                  #+arm64
                  ((and (vop-next vop)
                        (eq (vop-name vop)
                            (vop-name (vop-next vop)))
                        (memq (vop-name vop) '(move move-operand sb-vm::move-arg))
                        (sb-vm::load-store-two-words vop (vop-next vop)))
                   (setf vop (vop-next vop)))
                  (t
                   (funcall gen vop)))))))

    (when *do-instcombine-pass*
      #+(or arm64 x86-64)
      (sb-assem::combine-instructions (asmstream-code-section asmstream)))

    (emit (asmstream-data-section asmstream)
          (sb-assem::asmstream-data-origin-label asmstream))
    ;; Jump tables precede the coverage mark bytes to simplify locating
    ;; them in trans_code().
    (emit-jump-tables ir2-component)
    (let ((coverage-map (ir2-component-coverage-map ir2-component)))
      (unless (zerop (length coverage-map))
        ;; Nothing depends on the length of the constant vector at this
        ;; phase (codegen has not made use of component-header-length),
        ;; so extending can be done with impunity.
        #+arm64
        (vector-push-extend (list :coverage-marks (length coverage-map))
                            (ir2-component-constants ir2-component))
        (vector-push-extend
         (make-constant (cons 'coverage-map
                              (map-into coverage-map #'car coverage-map)))
         (ir2-component-constants ir2-component))
        ;; Allocate space in the data section for coverage marks.
        #-arm64
        (emit (asmstream-data-section asmstream)
              `(.skip ,(length coverage-map) #-(or x86 x86-64) #xff))))

    (emit-inline-constants)

    (let* ((n-boxed (length (ir2-component-constants ir2-component)))
           ;; Skew is either 0 or N-WORD-BYTES depending on whether the boxed
           ;; header length is even or odd
           (skew (if (and (= code-boxed-words-align 1) (oddp n-boxed))
                     sb-vm:n-word-bytes
                     0)))
      (multiple-value-bind (segment text-length fixup-notes fun-table)
          (assemble-sections
           asmstream
           (ir2-component-entries ir2-component)
           (make-segment (default-segment-run-scheduler) skew))
        (values segment text-length fun-table
                (asmstream-elsewhere-label asmstream) fixup-notes
                (sb-assem::get-allocation-points asmstream))))))

(defun label-elsewhere-p (label-or-posn kind)
  (let ((elsewhere (label-position *elsewhere-label*))
        (label (etypecase label-or-posn
                 (label
                  (label-position label-or-posn))
                 (index
                  label-or-posn))))
    (if (memq kind '(:single-value-return
                     :unknown-return
                     :known-return))
        ;; We're interested in what precedes the return, not after
        (< elsewhere label)
        (<= elsewhere label))))
