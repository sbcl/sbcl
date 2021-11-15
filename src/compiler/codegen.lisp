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

;;; the number of bytes used by the code object header

(macrolet ((header-length-in-bytes (comp)
             `(let* ((2comp (component-info ,comp))
                     (constants (ir2-component-constants 2comp)))
                (ash (align-up (length constants) code-boxed-words-align)
                     sb-vm:word-shift))))
  #-sb-xc-host
  (defun component-header-length (&optional (component *component-being-compiled*))
    (header-length-in-bytes component))
  ;; KLUDGE: the assembler can not emit backpatches comprising jump tables without
  ;; knowing the boxed code header length. But there is no compiler IR2 metaobject,
  ;; for SB-FASL:*ASSEMBLER-ROUTINES*. We have to return a fixed answer for that.
  #+sb-xc-host
  (defun component-header-length ()
    (if (boundp '*component-being-compiled*)
        (header-length-in-bytes *component-being-compiled*)
        (* sb-vm:n-word-bytes (align-up sb-vm:code-constants-offset 2)))))

;;; the size of the NAME'd SB in the currently compiled component.
;;; This is useful mainly for finding the size for allocating stack
;;; frames.
(defun sb-allocated-size (name)
  (finite-sb-current-size (sb-or-lose name)))

;;; the TN that is used to hold the number stack frame-pointer in
;;; VOP's function, or NIL if no number stack frame was allocated
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
(defun callee-nfp-tn (2env)
  (unless (zerop (sb-allocated-size 'non-descriptor-stack))
    (when (ir2-environment-number-stack-p 2env)
      (ir2-component-nfp (component-info *component-being-compiled*)))))

;;; the TN used for passing the return PC in a local call to the function
;;; designated by 2ENV
(defun callee-return-pc-tn (2env)
  (ir2-environment-return-pc-pass 2env))

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
(defun emit-jump-tables ()
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

;;; If a constant is already loaded into a register use that register.
(defun optimize-constant-loads (component)
  (let* ((register-sb (sb-or-lose 'sb-vm::registers))
         (loaded-constants
           (make-array (sb-size register-sb)
                       :initial-element nil)))
    (do-ir2-blocks (block component)
      (fill loaded-constants nil)
      (do ((vop (ir2-block-start-vop block) (vop-next vop)))
          ((null vop))
        (labels ((register-p (tn)
                   (and (tn-p tn)
                        (not (eq (tn-kind tn) :unused))
                        (eq (sc-sb (tn-sc tn)) register-sb)))
                 (constant-eql-p (a b)
                   (or (eq a b)
                       (and (eq (sc-name (tn-sc a)) 'constant)
                            (eq (tn-sc a) (tn-sc b))
                            (eql (tn-offset a) (tn-offset b)))))
                 (remove-constant (tn)
                   (when (register-p tn)
                     (setf (svref loaded-constants (tn-offset tn)) nil)))
                 (remove-written-tns ()
                   (cond ((memq (vop-info-save-p (vop-info vop))
                                '(t :force-to-stack))
                          (fill loaded-constants nil))
                         (t
                          (do ((ref (vop-results vop) (tn-ref-across ref)))
                              ((null ref))
                            (remove-constant (tn-ref-tn ref))
                            (remove-constant (tn-ref-load-tn ref)))
                          (do ((ref (vop-temps vop) (tn-ref-across ref)))
                              ((null ref))
                            (remove-constant (tn-ref-tn ref)))
                          (do ((ref (vop-args vop) (tn-ref-across ref)))
                              ((null ref))
                            (remove-constant (tn-ref-load-tn ref))))))
                 (compatible-scs-p (a b)
                   (or (eql a b)
                       (and (eq (sc-name a) 'sb-vm::control-stack)
                            (eq (sc-name b) 'sb-vm::descriptor-reg))
                       (and (eq (sc-name b) 'sb-vm::control-stack)
                            (eq (sc-name a) 'sb-vm::descriptor-reg))))
                 (find-constant-tn (constant sc)
                   (loop for (saved-constant . tn) across loaded-constants
                         when (and saved-constant
                                   (constant-eql-p saved-constant constant)
                                   (compatible-scs-p (tn-sc tn) sc))
                         return tn)))
          (case (vop-name vop)
            ((move sb-vm::move-arg)
             (let* ((args (vop-args vop))
                    (x (tn-ref-tn args))
                    (y (tn-ref-tn (vop-results vop)))
                    constant)
               (cond ((or (eq (sc-name (tn-sc x)) 'null)
                          (not (eq (tn-kind x) :constant)))
                      (remove-written-tns))
                     ((setf constant (find-constant-tn x (tn-sc y)))
                      (when (register-p y)
                        (setf (svref loaded-constants (tn-offset y))
                              (cons x y)))
                      ;; XOR is more compact on x86oids and many
                      ;; RISCs have a zero register
                      (unless (and (constant-p (tn-leaf x))
                                   (eql (tn-value x) 0)
                                   (register-p y))
                        (setf (tn-ref-tn args) constant)
                        (setf (tn-ref-load-tn args) nil)))
                     ((register-p y)
                      (setf (svref loaded-constants (tn-offset y))
                            (cons x y)))
                     (t
                      (remove-written-tns)))))
            (t
             (remove-written-tns))))))))

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
         ;; The first function's alignment word is zero-filled, but subsequent
         ;; ones can use a NOP which helps the disassembler not lose sync.
         (filler-pattern 0)
         (asmstream (make-asmstream))
         (coverage-map)
         (*asmstream* asmstream))

    (declare (ignorable coverage-map))
    (emit (asmstream-elsewhere-section asmstream)
          (asmstream-elsewhere-label asmstream))

    #-(or x86 x86-64)
    (let ((path->index (make-hash-table :test 'equal)))
      ;; Pre-scan for MARK-COVERED vops and collect the set of
      ;; distinct source paths that occur. Delay adding the coverage map to
      ;; the boxed constant vector until all vop generators have run, because
      ;; they can use EMIT-CONSTANT to add more constants,
      ;; while the coverage map has to be the very last entry in the vector.
      (do-ir2-blocks (block component)
        (do ((vop (ir2-block-start-vop block) (vop-next vop)))
            ((null vop))
          (when (eq (vop-name vop) 'mark-covered)
            (setf (vop-codegen-info vop)
                  (list (ensure-gethash (car (vop-codegen-info vop))
                                        path->index
                                        (hash-table-count path->index)))))))
      (when (plusp (hash-table-count path->index))
        (setf coverage-map (make-array (hash-table-count path->index)))
        (maphash (lambda (path index) (setf (aref coverage-map index) path))
                 path->index)))

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
    (emit-jump-tables)
    ;; Todo: can we implement the flow-based aspect of coverage mark compression
    ;; in IR2 instead of waiting until assembly generation?
    #+(or x86 x86-64) (coverage-mark-lowering-pass component asmstream)
    ;; The #+(or x86 x86-64) case has _already_ output the mark bytes into
    ;; the data section in lowering pass, so we don't do that here.
    #-(or x86 x86-64)
    (when coverage-map
      #+arm64
      (vector-push-extend (make-constant (make-array (length coverage-map)
                                                     :element-type '(unsigned-byte 8)
                                                     :initial-element #xFF))
                          (ir2-component-constants ir2-component))
      (vector-push-extend (make-constant (cons 'coverage-map coverage-map))
                          (ir2-component-constants ir2-component))
      ;; The mark vop can store the low byte from either ZERO-TN or NULLL-TN
      ;; to avoid loading a constant. Either one won't match #xff.
      #-arm64
      (emit (asmstream-data-section asmstream)
            `(.skip ,(length coverage-map) #xff)))

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
           (mapcar #'entry-info-offset (ir2-component-entries ir2-component))
           (make-segment :header-skew skew
                         :run-scheduler (default-segment-run-scheduler)))
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

;;; Translate .COVERAGE-MARK pseudo-op into machine assembly language,
;;; combining any number of consecutive operations with no intervening
;;; control flow into a single operation.
;;; FIXME: this pass runs even if no coverage instrumentation was generated.
(defun coverage-mark-lowering-pass (component asmstream)
  (declare (ignorable component asmstream))
  #+(or x86-64 x86)
  (let ((label (gen-label))
        ;; vector of lists of original source paths covered
        (src-paths (make-array 10 :fill-pointer 0))
        (previous-mark))
    (do ((statement (stmt-next (section-start (asmstream-code-section asmstream)))
                    (stmt-next statement)))
        ((null statement))
      (dolist (item (ensure-list (stmt-labels statement)))
        (when (label-usedp item) ; control can transfer to here
          (setq previous-mark nil)))
      (let ((mnemonic (stmt-mnemonic statement)))
        (typecase mnemonic
         (function ; this can do anything, who knows
          (setq previous-mark nil))
         (string) ; a comment can be ignored
         (t
          (cond ((branch-opcode-p mnemonic) ; control flow kills mark combining
                 (setq previous-mark nil))
                ((eq mnemonic '.coverage-mark)
                 (let ((path (car (stmt-operands statement))))
                   (cond ((not previous-mark) ; record a new path
                          (let ((mark-index
                                 (vector-push-extend (list path) src-paths)))
                            ;; have the backend lower it into a real instruction
                            (replace-coverage-instruction statement label mark-index))
                            (setq previous-mark statement))
                           (t ; record that the already-emitted mark pertains
                            ;; to an additional source path
                            (push path (elt src-paths (1- (fill-pointer src-paths))))
                            ;; Clobber this statement. Do not try to remove it and
                            ;; combine its labels into the previous statement.
                            ;; Doing that could move a label into an earlier IR2 block
                            ;; which crashes when dumping locations.
                            (setf (stmt-mnemonic statement) nil
                                  (stmt-operands statement) nil))))))))))

    ;; Allocate space in the data section for coverage marks
    (let ((mark-index (length src-paths)))
      (when (plusp mark-index)
        (setf (label-usedp label) t)
        (let ((v (ir2-component-constants (component-info component))))
          ;; Nothing depends on the length of the constant vector at this
          ;; phase (codegen has not made use of component-header-length),
          ;; so extending can be done with impunity.
          (vector-push-extend
           (make-constant (cons 'coverage-map
                                (coerce src-paths 'simple-vector)))
           v))
        (emit (asmstream-data-section asmstream) label `(.skip ,mark-index))))))
