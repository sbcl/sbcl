;;;; scheduling assembler

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-ASSEM")

;;;; assembly control parameters

;;; Only the scheduling assembler cares about this constant,
;;; but it has to be defined. If ASSEM-SCHEDULER-P is true and it hasn't
;;; been assigned, we'll get an error. It's OK if ASSEM-SCHEDULER-P is NIL
;;; and the constant _has_ already been defined. (This is weird but harmless)
(defconstant +assem-max-locations+
  (if (boundp '+assem-max-locations+)
      (symbol-value '+assem-max-locations+)
      0))

;;;; Constants.

;;; ASSEMBLY-UNIT-BITS -- the number of bits in the minimum assembly
;;; unit, (also referred to as a ``byte''). Hopefully, different
;;; instruction sets won't require changing this.
(defconstant assembly-unit-bits 8)
(defconstant assembly-unit-mask (1- (ash 1 assembly-unit-bits)))

(deftype assembly-unit ()
  `(unsigned-byte ,assembly-unit-bits))

;;; Some functions which accept assembly units can meaningfully accept
;;; signed values with the same number of bits and silently munge them
;;; into appropriate unsigned values. (This is handy behavior e.g.
;;; when assembling branch instructions on the X86.)
(deftype possibly-signed-assembly-unit ()
  `(or assembly-unit
       (signed-byte ,assembly-unit-bits)))

;;; the maximum alignment we can guarantee given the object format. If
;;; the loader only loads objects 8-byte aligned, we can't do any
;;; better than that ourselves.
(defconstant max-alignment 5)

(deftype alignment ()
  `(integer 0 ,max-alignment))


;;;; the SEGMENT structure

;;; This structure holds the state of the assembler.
(defstruct (segment (:copier nil))
  ;; This is a vector where instructions are written.
  ;; It used to be an adjustable array, but we now do the array size
  ;; management manually for performance reasons (as of 2006-05-13 hairy
  ;; array operations are rather slow compared to simple ones).
  (buffer (make-array 100 :element-type 'assembly-unit)
          :type (simple-array assembly-unit 1))
  (encoder-state)
  ;; whether or not to run the scheduler. Note: if the instruction
  ;; definitions were not compiled with the scheduler turned on, this
  ;; has no effect.
  (run-scheduler nil)
  ;; what position does this correspond to? Initially, positions and
  ;; indexes are the same, but after we start collapsing choosers,
  ;; positions can change while indexes stay the same.
  (current-posn 0 :type index)
  (%current-index 0 :type index)
  ;; a list of all the annotations that have been output to this segment
  (annotations nil :type list)
  ;; the subset of annotations which are of type ALIGNMENT-NOTE
  (alignment-annotations)
  ;; a pointer to the last cons cell in the annotations list. This is
  ;; so we can quickly add things to the end of the annotations list.
  (last-annotation nil :type list)
  ;; the number of bits of alignment at the last time we synchronized
  (alignment max-alignment :type alignment)
  ;; number of bytes to subtract from all finalized positions such that
  ;; position 0 corresponds to CODE-INSTRUCTIONS of the code component
  ;; being assembled.
  (header-skew 0 :type (member 0 #.sb-vm:n-word-bytes))
  ;; the position the last time we synchronized
  (sync-posn 0 :type index)
  ;; a label at position 0
  (origin (gen-label) :read-only t)
  ;; The posn and index everything ends at. This is not maintained
  ;; while the data is being generated, but is filled in after.
  ;; Basically, we copy CURRENT-POSN and CURRENT-INDEX so that we can
  ;; trash them while processing choosers and back-patches.
  (final-posn 0 :type index)
  (final-index 0 :type index)
  ;; *** State used by the scheduler during instruction queueing.
  ;;
  ;; a list of postits. These are accumulated between instructions.
  (postits nil :type list)
  ;; ``Number'' for last instruction queued. Used only to supply insts
  ;; with unique sset-element-number's.
  (inst-number 0 :type index)
  ;; SIMPLE-VECTORs mapping locations to the instruction that reads them and
  ;; instructions that write them
  (readers (make-array +assem-max-locations+ :initial-element nil)
           :type simple-vector)
  (writers (make-array +assem-max-locations+ :initial-element nil)
           :type simple-vector)
  ;; The number of additional cycles before the next control transfer,
  ;; or NIL if a control transfer hasn't been queued. When a delayed
  ;; branch is queued, this slot is set to the delay count.
  (branch-countdown nil :type (or null (and fixnum unsigned-byte)))
  ;; *** These two slots are used both by the queuing noise and the
  ;; scheduling noise.
  ;;
  ;; All the instructions that are pending and don't have any
  ;; unresolved dependents. We don't list branches here even if they
  ;; would otherwise qualify. They are listed above.
  (emittable-insts-sset (make-sset) :type sset)
  ;; list of queued branches. We handle these specially, because they
  ;; have to be emitted at a specific place (e.g. one slot before the
  ;; end of the block).
  (queued-branches nil :type list)
  ;; *** state used by the scheduler during instruction scheduling
  ;;
  ;; the instructions who would have had a read dependent removed if
  ;; it were not for a delay slot. This is a list of lists. Each
  ;; element in the top level list corresponds to yet another cycle of
  ;; delay. Each element in the second level lists is a dotted pair,
  ;; holding the dependency instruction and the dependent to remove.
  (delayed nil :type list)
  ;; The emittable insts again, except this time as a list sorted by depth.
  (emittable-insts-queue nil :type list)
  (fixup-notes)
  ;; Whether or not to collect dynamic statistics. This is just the same as
  ;; *COLLECT-DYNAMIC-STATISTICS* but is faster to reference.
  #+sb-dyncount
  (collect-dynamic-statistics nil))
(declaim (freeze-type segment))
(defprinter (segment :identity t))

(declaim (inline segment-current-index))
(defun segment-current-index (segment)
  (segment-%current-index segment))

(defun (setf segment-current-index) (new-value segment)
  (declare (type index new-value)
           (type segment segment))
  ;; FIXME: It would be lovely to enforce this, but first FILL-IN will
  ;; need to be convinced to stop rolling SEGMENT-CURRENT-INDEX
  ;; backwards.
  ;;
  ;; Enforce an observed regularity which makes it easier to think
  ;; about what's going on in the (legacy) code: The segment never
  ;; shrinks. -- WHN the reverse engineer
  #+nil (aver (>= new-value (segment-current-index segment)))
  (let* ((buffer (segment-buffer segment))
         (new-buffer-size (length buffer)))
    (declare (type (simple-array (unsigned-byte 8)) buffer)
             (type index new-buffer-size))
    ;; Make sure the array is big enough.
    (when (<= new-buffer-size new-value)
      (do ()
          ((> new-buffer-size new-value))
        ;; When we have to increase the size of the array, we want to
        ;; roughly double the vector length: that way growing the array
        ;; to size N conses only O(N) bytes in total.
        (setf new-buffer-size (* 2 new-buffer-size)))
      (let ((new-buffer (make-array new-buffer-size
                                    :element-type '(unsigned-byte 8))))
        (replace new-buffer buffer)
        (setf (segment-buffer segment) new-buffer)))
    ;; Now that the array has the intended next free byte, we can point to it.
    (setf (segment-%current-index segment) new-value)))

;;; Various functions (like BACK-PATCH-FUN or CHOOSER-WORST-CASE-FUN)
;;; aren't cleanly parameterized, but instead use
;;; SEGMENT-CURRENT-INDEX and/or SEGMENT-CURRENT-POSN as global
;;; variables. So code which calls such functions needs to modify
;;; SEGMENT-CURRENT-INDEX and SEGMENT-CURRENT-POSN. This is left over
;;; from the old new-assem.lisp C-style code, and so all the
;;; destruction happens to be done after other uses of these slots are
;;; done and things basically work. However, (1) it's fundamentally
;;; nasty, and (2) at least one thing doesn't work right: OpenMCL
;;; properly points out that SUBSEQ's indices aren't supposed to
;;; exceed its logical LENGTH, i.e. its FILL-POINTER, i.e.
;;; SEGMENT-CURRENT-INDEX.
;;;
;;; As a quick fix involving minimal modification of legacy code,
;;; we do such sets of SEGMENT-CURRENT-INDEX and SEGMENT-CURRENT-POSN
;;; using this macro, which restores 'em afterwards.
;;;
;;; FIXME: It'd probably be better to cleanly parameterize things like
;;; BACK-PATCH-FUN so we can avoid this nastiness altogether.
(defmacro with-modified-segment-index-and-posn ((segment index posn)
                                                &body body)
  (with-unique-names (n-segment old-index old-posn)
    `(let* ((,n-segment ,segment)
            (,old-index (segment-current-index ,n-segment))
            (,old-posn (segment-current-posn ,n-segment)))
       (unwind-protect
           (progn
             (setf (segment-current-index ,n-segment) ,index
                   (segment-current-posn ,n-segment) ,posn)
             ,@body)
         (setf (segment-current-index ,n-segment) ,old-index
               (segment-current-posn ,n-segment) ,old-posn)))))

;;;; structures/types used by the scheduler

(!def-boolean-attribute instruction
  ;; This attribute is set if the scheduler can freely flush this
  ;; instruction if it thinks it is not needed. Examples are NOP and
  ;; instructions that have no side effect not described by the
  ;; writes.
  flushable
  ;; This attribute is set when an instruction can cause a control
  ;; transfer. For test instructions, the delay is used to determine
  ;; how many instructions follow the branch.
  branch
  ;; This attribute indicates that this ``instruction'' can be
  ;; variable length, and therefore had better never be used in a
  ;; branch delay slot.
  variable-length)

(defstruct (instruction
            (:include sset-element)
            (:conc-name inst-)
            (:constructor make-instruction (number emitter attributes delay))
            (:copier nil))
  ;; The function to envoke to actually emit this instruction. Gets called
  ;; with the segment as its one argument.
  (emitter (missing-arg) :type (or null function))
  ;; The attributes of this instruction.
  (attributes (instruction-attributes) :type sb-c:attributes)
  ;; Number of instructions or cycles of delay before additional
  ;; instructions can read our writes.
  (delay 0 :type (and fixnum unsigned-byte))
  ;; the maximum number of instructions in the longest dependency
  ;; chain from this instruction to one of the independent
  ;; instructions. This is used as a heuristic at to which
  ;; instructions should be scheduled first.
  (depth nil :type (or null (and fixnum unsigned-byte)))
  ;; Note: When trying remember which of the next four is which, note
  ;; that the ``read'' or ``write'' always refers to the dependent
  ;; (second) instruction.
  ;;
  ;; instructions whose writes this instruction tries to read
  (read-dependencies (make-sset) :type sset :read-only t)
  ;; instructions whose writes or reads are overwritten by this instruction
  (write-dependencies (make-sset) :type sset :read-only t)
  ;; instructions which write what we read or write
  (write-dependents (make-sset) :type sset :read-only t)
  ;; instructions which read what we write
  (read-dependents (make-sset) :type sset :read-only t))
(declaim (freeze-type instruction))
#+sb-show-assem (defvar *inst-ids* (make-hash-table :test 'eq))
#+sb-show-assem (defvar *next-inst-id* 0)
(defmethod print-object ((inst instruction) stream)
  (print-unreadable-object (inst stream :type t :identity t)
    #+sb-show-assem
    (princ (ensure-gethash inst *inst-ids* (incf *next-inst-id*))
           stream)
    (format stream
            #+sb-show-assem " emitter=~S" #-sb-show-assem "emitter=~S"
            (let ((emitter (inst-emitter inst)))
              (if emitter
                  (multiple-value-bind (lambda lexenv-p name)
                      (function-lambda-expression emitter)
                    (declare (ignore lambda lexenv-p))
                    name)
                  '<flushed>)))
    (when (inst-depth inst)
      (format stream ", depth=~W" (inst-depth inst)))))

#+sb-show-assem
(defun reset-inst-ids ()
  (clrhash *inst-ids*)
  (setf *next-inst-id* 0))
;;;

;;; Instructions are streamed into a section before (optionally combining
;;; sections and) assembling into a SEGMENT.
(defstruct (stmt (:constructor make-stmt (labels vop mnemonic operands)))
  (labels)
  (vop)
  (mnemonic)
  (operands)
  (plist nil) ; put anything you want here for later passes such as instcombine
  (prev nil)
  (next nil))
(declaim (freeze-type stmt))
(defmethod print-object ((stmt stmt) stream)
  (print-unreadable-object (stmt stream :type t :identity t)
    (awhen (stmt-labels stmt)
      (princ it stream)
      (write-char #\space stream))
    (princ (stmt-mnemonic stmt) stream)))

;;; A section is just a doubly-linked list of statements with a head and
;;; tail pointer to allow insertion anywhere,
;;; and a dummy head node to avoid special-casing an empty section.
(defun make-section ()
  (let ((first (make-stmt nil nil :ignore nil)))
    (cons first first)))
(defun section-start (section) (car section))
(defmacro section-tail (section) `(cdr ,section))

(defstruct asmstream
  (data-section (make-section) :read-only t)
  (code-section (make-section) :read-only t)
  (elsewhere-section (make-section) :read-only t)
  (data-origin-label (gen-label "data start") :read-only t)
  (text-origin-label (gen-label "text start") :read-only t)
  (elsewhere-label (gen-label "elsewhere start") :read-only t)
  (inter-function-padding :normal :type (member :normal :nop))
  ;; for collecting unique "unboxed constants" prior to placing them
  ;; into the data section
  (constant-table (make-hash-table :test #'equal) :read-only t)
  (constant-vector (make-array 16 :adjustable t :fill-pointer 0) :read-only t)
  ;; for deterministic allocation profiler (or possibly other tooling)
  ;; that wants to monkey patch the instructions at runtime.
  (alloc-points)
  ;; for shrinking the size of the code fixups, we can choose to emit at most one call
  ;; from a dynamic space code component to a given assembly routine. The call goes
  ;; through an extra indirection in the component.
  ;; This table is stored as an alist of (NAME . LABEL).
  (indirection-table)
  ;; tracking where we last wrote an instruction so that SB-C::TRACE-INSTRUCTION
  ;; can print "in the {x} section" whenever it changes.
  (tracing-state (list nil nil) :read-only t)) ; segment and vop
(declaim (freeze-type asmstream))

(defvar *asmstream*)
(declaim (type asmstream *asmstream*))

(defun get-allocation-points (asmstream)
  ;; Convert the label positions to a packed integer
  ;; Utilize PACK-CODE-FIXUP-LOCS to perform compression.
  (awhen (mapcar 'label-posn (asmstream-alloc-points asmstream))
    (sb-c:pack-code-fixup-locs it)))

;;; Insert STMT after PREDECESSOR.
(defun insert-stmt (stmt predecessor)
  (let ((successor (stmt-next predecessor)))
    (setf (stmt-next predecessor) stmt
          (stmt-prev stmt) predecessor
          (stmt-next stmt) successor)
    (when successor
      (stmt-prev successor) stmt))
  stmt)

(defun delete-stmt (stmt)
  (let ((prev (stmt-prev stmt))
        (next (stmt-next stmt)))
    (aver prev)
    ;; KLUDGE: we're not passing around the section, but instcombine only
    ;; runs on the code section, so check whether we're deleting the last
    ;; statement in that section, and fix the last pointer if so.
    (let ((section (asmstream-code-section *asmstream*)))
      (when (eq (section-tail section) stmt)
        (setf (section-tail section) prev)))
    (setf (stmt-next prev) next)
    (when next
      (setf (stmt-prev next) prev))))

(defun add-stmt-labels (statement more-labels)
  (let ((list (nconc (ensure-list (stmt-labels statement))
                     (ensure-list more-labels))))
    (setf (stmt-labels statement)
          (if (singleton-p list) (car list) list))))

;;; This is used only to keep track of which vops emit which insts.
(defvar *current-vop*)

;;; Return the final statement emitted.
(defun emit (section &rest things)
  ;; each element of THINGS can be:
  ;; - a label
  ;; - a list (mnemonic . operands) for a machine instruction or assembler directive
  ;; - a function to emit a postit
  (let ((last (section-tail section))
        (vop (if (boundp '*current-vop*) *current-vop*)))
    (dolist (thing things (setf (section-tail section) last))
      (if (label-p thing) ; Accumulate multiple labels until the next instruction
          (if (stmt-mnemonic last)
              (setq last (insert-stmt (make-stmt thing vop nil nil) last))
              (let ((old (stmt-labels last)) (new (list thing)))
                (setf (stmt-labels last)
                      (if (label-p old) (cons old new) (nconc old new)))))
          (multiple-value-bind (mnemonic operands)
              (if (consp thing) (values (car thing) (cdr thing)) thing)
            (unless (member mnemonic '(.align .byte .skip))
              ;; This automatically gets the .QWORD pseudo-op which we use on x86-64
              ;; to create jump tables, but it's sort of unfortunate that the mnemonic
              ;; is specific to that backend. It should probably be .LISPWORD instead.
              ;; Anyway, the good news is that jump tables flag all the labels as used.
              (dolist (operand operands)
                (if (label-p operand)
                    (setf (label-usedp operand) t)
                    ;; backend decides what labels are used
                    (%mark-used-labels operand))))
            (if (stmt-mnemonic last)
                (setq last (insert-stmt (make-stmt nil vop mnemonic operands) last))
                (setf (stmt-vop last) (or (stmt-vop last) vop)
                      (stmt-mnemonic last) mnemonic
                      (stmt-operands last) operands)))))))

#-(or x86-64 x86)
(defun %mark-used-labels (operand) ; default implementation
  (declare (ignore operand)))

;;; This holds either the current section (if writing symbolic assembly)
;;; or current segment (if machine-encoding). Use ASSEMBLE to change it.
(defvar *current-destination*)

;;; This formerly had some absolutely bizarre behavior regarding a manually-specified
;;; list of labels. It was so confusing that I couldn't figure out either what it was
;;; designed to do, or what it did do, because they certainly weren't the same thing.
;;; My best guess is that if you needed labels which were not directly at the "spine"
;;; of the ASSEMBLE form, but nested within, you could force it to call GEN-LABEL
;;; for you, which is no different from calling GEN-LABEL _outside_ of the ASSEMBLE.
;;; (Which most people do anyway if they need to)
;;; But in anything more than a straightfoward usage, the expansion could be wrong.
;;; For example this fragment calls EMIT-LABEL on the same label instance twice:
#|
 (sb-cltl2:macroexpand-all
  '(assemble ()
     B
     (assemble (nil nil :labels (foo))
       (inst pop rax-tn)
       B
       (assemble ()
         B
         (wat)))))
=>
(LET* ((B (GEN-LABEL)))
  (SYMBOL-MACROLET ()
    (EMIT-LABEL B)
    (LET* ((B (GEN-LABEL)) (FOO (GEN-LABEL)))
      (SYMBOL-MACROLET ((SB-ASSEM::..INHERITED-LABELS.. (B)))
        (INST* 'POP RAX-TN)
        (EMIT-LABEL B)
        (LET* ()
          (SYMBOL-MACROLET ((SB-ASSEM::..INHERITED-LABELS.. NIL))
            (EMIT-LABEL B)
            (WAT)))))))
|#

(defmacro assemble ((&optional dest vop) &body body &environment env)
  "Execute BODY (as a progn) with DEST as the current section or segment."
  (flet ((label-name-p (thing) (typep thing '(and symbol (not null)))))
    (let ((inherited (multiple-value-bind (expansion expanded)
                         (#+sb-xc-host cl:macroexpand-1
                          #-sb-xc-host %macroexpand-1 '..inherited-labels.. env)
                       (if expanded expansion)))
          (new-labels (sort (copy-list (remove-if-not #'label-name-p body)) #'string<)))
      ;; Compare for dups using STRING=. Two reasons to use that rather than EQ:
      ;; (1) the assembler input is generally string-like - consider that instruction
      ;;     mnemonics are looked up by string even though written as symbols.
      ;; (2) the above SORT could yield an unpredictable result across build hosts
      (unless (= (length (remove-duplicates new-labels :test #'string=))
                 (length new-labels))
        (error "Repeated labels in ASSEMBLE body"))
      (awhen (intersection inherited new-labels)
        (style-warn "Shadowed asm labels ~S should be renamed not to conflict" it))
      `(let* (,@(when dest
                  `((*current-destination*
                     ,(case dest
                        (:code '(asmstream-code-section *asmstream*))
                        (:elsewhere '(asmstream-elsewhere-section *asmstream*))
                        (t dest)))))
              ,@(when vop `((*current-vop* ,vop)))
              ,@(mapcar (lambda (name) `(,name (gen-label)))
                        new-labels))
         (symbol-macrolet ((..inherited-labels.. ,(append inherited new-labels)))
           ,@(mapcar (lambda (form)
                       (if (label-name-p form)
                           `(emit-label ,form)
                           form))
                     body))))))

(defun assembling-to-elsewhere-p ()
  (eq *current-destination* (asmstream-elsewhere-section *asmstream*)))

;;;; the scheduler itself

(defmacro without-scheduling (() &body body)
  "Execute BODY (as a PROGN) without scheduling any of the instructions
   generated inside it. This is not protected by UNWIND-PROTECT, so
   DO NOT use THROW or RETURN-FROM to escape from it."
  `(let ((section. *current-destination*))
     ;; This is similar to bracketing the code with ".set noreorder"
     ;; and ".set reorder" in the MIPS assembler, except that we could
     ;; theoretically allow nesting (so only restore ".set reorder"
     ;; after the outermost), except that we don't allow it.
     (emit section. '(.begin-without-scheduling))
     ,@body
     (emit section. '(.end-without-scheduling))))

(defmacro note-dependencies ((segment inst) &body body)
  (once-only ((segment segment) (inst inst))
    `(macrolet ((reads (loc) `(note-read-dependency ,',segment ,',inst ,loc))
                (writes (loc &rest keys)
                  `(note-write-dependency ,',segment ,',inst ,loc ,@keys)))
       ,@body)))

#.(unless assem-scheduler-p
    '(defmacro schedule-pending-instructions (segment)
      (declare (ignore segment))))

#.(when assem-scheduler-p
'(progn
(defun note-read-dependency (segment inst read)
  (multiple-value-bind (loc-num size)
      (sb-c:location-number read)
    #+sb-show-assem (format *trace-output*
                             "~&~S reads ~S[~W for ~W]~%"
                             inst read loc-num size)
    (when loc-num
      ;; Iterate over all the locations for this TN.
      (do ((index loc-num (1+ index))
           (end-loc (+ loc-num (or size 1))))
          ((>= index end-loc))
        (declare (type (mod 2048) index end-loc))
        (let ((writers (svref (segment-writers segment) index)))
          (when writers
            ;; The inst that wrote the value we want to read must have
            ;; completed.
            (let ((writer (car writers)))
              (sset-adjoin writer (inst-read-dependencies inst))
              (sset-adjoin inst (inst-read-dependents writer))
              (sset-delete writer (segment-emittable-insts-sset segment))
              ;; And it must have been completed *after* all other
              ;; writes to that location. Actually, that isn't quite
              ;; true. Each of the earlier writes could be done
              ;; either before this last write, or after the read, but
              ;; we have no way of representing that.
              (dolist (other-writer (cdr writers))
                (sset-adjoin other-writer (inst-write-dependencies writer))
                (sset-adjoin writer (inst-write-dependents other-writer))
                (sset-delete other-writer
                             (segment-emittable-insts-sset segment))))
            ;; And we don't need to remember about earlier writes any
            ;; more. Shortening the writers list means that we won't
            ;; bother generating as many explicit arcs in the graph.
            (setf (cdr writers) nil)))
        (push inst (svref (segment-readers segment) index)))))
  (values))

(defun note-write-dependency (segment inst write &key partially)
  (multiple-value-bind (loc-num size)
      (sb-c:location-number write)
    #+sb-show-assem (format *trace-output*
                             "~&~S writes ~S[~W for ~W]~%"
                             inst write loc-num size)
    (when loc-num
      ;; Iterate over all the locations for this TN.
      (do ((index loc-num (1+ index))
           (end-loc (+ loc-num (or size 1))))
          ((>= index end-loc))
        (declare (type (mod 2048) index end-loc))
        ;; All previous reads of this location must have completed.
        (dolist (prev-inst (svref (segment-readers segment) index))
          (unless (eq prev-inst inst)
            (sset-adjoin prev-inst (inst-write-dependencies inst))
            (sset-adjoin inst (inst-write-dependents prev-inst))
            (sset-delete prev-inst (segment-emittable-insts-sset segment))))
        (when partially
          ;; All previous writes to the location must have completed.
          (dolist (prev-inst (svref (segment-writers segment) index))
            (sset-adjoin prev-inst (inst-write-dependencies inst))
            (sset-adjoin inst (inst-write-dependents prev-inst))
            (sset-delete prev-inst (segment-emittable-insts-sset segment)))
          ;; And we can forget about remembering them, because
          ;; depending on us is as good as depending on them.
          (setf (svref (segment-writers segment) index) nil))
        (push inst (svref (segment-writers segment) index)))))
  (values))

;;; This routine is called by due to uses of the INST macro when the
;;; scheduler is turned on. The change to the dependency graph has
;;; already been computed, so we just have to check to see whether the
;;; basic block is terminated.
(defun queue-inst (segment inst)
  #+sb-show-assem (format *trace-output* "~&queuing ~S~%" inst)
  #+sb-show-assem (format *trace-output*
                           "  reads ~S~%  writes ~S~%"
                           (collect ((reads))
                             (do-sset-elements (read
                                                (inst-read-dependencies inst))
                                (reads read))
                             (reads))
                           (collect ((writes))
                             (do-sset-elements (write
                                                (inst-write-dependencies inst))
                                (writes write))
                             (writes)))
  (aver (segment-run-scheduler segment))
  (let ((countdown (segment-branch-countdown segment)))
    (when countdown
      (decf countdown)
      (aver (not (instruction-attributep (inst-attributes inst)
                                         variable-length))))
    (cond ((instruction-attributep (inst-attributes inst) branch)
           (unless countdown
             (setf countdown (inst-delay inst)))
           (push (cons countdown inst)
                 (segment-queued-branches segment)))
          (t
           (sset-adjoin inst (segment-emittable-insts-sset segment))))
    (when countdown
      (setf (segment-branch-countdown segment) countdown)
      (when (zerop countdown)
        (schedule-pending-instructions segment))))
  (values))

;;; Emit all the pending instructions, and reset any state. This is
;;; called whenever we hit a label (i.e. an entry point of some kind)
;;; and when the user turns the scheduler off (otherwise, the queued
;;; instructions would sit there until the scheduler was turned back
;;; on, and emitted in the wrong place).
(defun schedule-pending-instructions (segment)
  (aver (segment-run-scheduler segment))

  ;; Quick blow-out if nothing to do.
  (when (and (sset-empty (segment-emittable-insts-sset segment))
             (null (segment-queued-branches segment)))
    (return-from schedule-pending-instructions
                 (values)))

  #+sb-show-assem (format *trace-output*
                           "~&scheduling pending instructions..~%")

  ;; Note that any values live at the end of the block have to be
  ;; computed last.
  (let ((emittable-insts (segment-emittable-insts-sset segment))
        (writers (segment-writers segment)))
    (dotimes (index (length writers))
      (let* ((writer (svref writers index))
             (inst (car writer))
             (overwritten (cdr writer)))
        (when writer
          (when overwritten
            (let ((write-dependencies (inst-write-dependencies inst)))
              (dolist (other-inst overwritten)
                (sset-adjoin inst (inst-write-dependents other-inst))
                (sset-adjoin other-inst write-dependencies)
                (sset-delete other-inst emittable-insts))))
          ;; If the value is live at the end of the block, we can't flush it.
          (setf (instruction-attributep (inst-attributes inst) flushable)
                nil)))))

  ;; Grovel through the entire graph in the forward direction finding
  ;; all the leaf instructions.
  (labels ((grovel-inst (inst)
             (let ((max 0))
               (do-sset-elements (dep (inst-write-dependencies inst))
                 (let ((dep-depth (or (inst-depth dep) (grovel-inst dep))))
                   (when (> dep-depth max)
                     (setf max dep-depth))))
               (do-sset-elements (dep (inst-read-dependencies inst))
                 (let ((dep-depth
                        (+ (or (inst-depth dep) (grovel-inst dep))
                           (inst-delay dep))))
                   (when (> dep-depth max)
                     (setf max dep-depth))))
               (cond ((and (sset-empty (inst-read-dependents inst))
                           (instruction-attributep (inst-attributes inst)
                                                   flushable))
                      #+sb-show-assem (format *trace-output*
                                               "flushing ~S~%"
                                               inst)
                      (setf (inst-emitter inst) nil)
                      (setf (inst-depth inst) max))
                     (t
                      (setf (inst-depth inst) max))))))
    (let ((emittable-insts nil)
          (delayed nil))
      (do-sset-elements (inst (segment-emittable-insts-sset segment))
        (grovel-inst inst)
        (if (zerop (inst-delay inst))
            (push inst emittable-insts)
            (setf delayed
                  (add-to-nth-list delayed inst (1- (inst-delay inst))))))
      (setf (segment-emittable-insts-queue segment)
            (sort emittable-insts #'> :key #'inst-depth))
      (setf (segment-delayed segment) delayed))
    (dolist (branch (segment-queued-branches segment))
      (grovel-inst (cdr branch))))
  #+sb-show-assem (format *trace-output*
                           "queued branches: ~S~%"
                           (segment-queued-branches segment))
  #+sb-show-assem (format *trace-output*
                           "initially emittable: ~S~%"
                           (segment-emittable-insts-queue segment))
  #+sb-show-assem (format *trace-output*
                           "initially delayed: ~S~%"
                           (segment-delayed segment))

  ;; Accumulate the results in reverse order. Well, actually, this
  ;; list will be in forward order, because we are generating the
  ;; reverse order in reverse.
  (let ((results nil))

    ;; Schedule all the branches in their exact locations.
    (let ((insts-from-end (segment-branch-countdown segment)))
      (dolist (branch (segment-queued-branches segment))
        (let ((inst (cdr branch)))
          (dotimes (i (- (car branch) insts-from-end))
            ;; Each time through this loop we need to emit another
            ;; instruction. First, we check to see whether there is
            ;; any instruction that must be emitted before (i.e. must
            ;; come after) the branch inst. If so, emit it. Otherwise,
            ;; just pick one of the emittable insts. If there is
            ;; nothing to do, then emit a nop. ### Note: despite the
            ;; fact that this is a loop, it really won't work for
            ;; repetitions other than zero and one. For example, if
            ;; the branch has two dependents and one of them dpends on
            ;; the other, then the stuff that grabs a dependent could
            ;; easily grab the wrong one. But I don't feel like fixing
            ;; this because it doesn't matter for any of the
            ;; architectures we are using or plan on using.
            (flet ((maybe-schedule-dependent (dependents)
                     (do-sset-elements (inst dependents)
                       ;; If do-sset-elements enters the body, then there is a
                       ;; dependent. Emit it.
                       (note-resolved-dependencies segment inst)
                       ;; Remove it from the emittable insts.
                       (setf (segment-emittable-insts-queue segment)
                             (delete inst
                                     (segment-emittable-insts-queue segment)
                                     :test #'eq))
                       ;; And if it was delayed, removed it from the delayed
                       ;; list. This can happen if there is a load in a
                       ;; branch delay slot.
                       (block scan-delayed
                         (do ((delayed (segment-delayed segment)
                                       (cdr delayed)))
                             ((null delayed))
                           (do ((prev nil cons)
                                (cons (car delayed) (cdr cons)))
                               ((null cons))
                             (when (eq (car cons) inst)
                               (if prev
                                   (setf (cdr prev) (cdr cons))
                                   (setf (car delayed) (cdr cons)))
                               (return-from scan-delayed nil)))))
                       ;; And return it.
                       (return inst))))
              (let ((fill (or (maybe-schedule-dependent
                               (inst-read-dependents inst))
                              (maybe-schedule-dependent
                               (inst-write-dependents inst))
                              (schedule-one-inst segment t)
                              :nop)))
                #+sb-show-assem (format *trace-output*
                                         "filling branch delay slot with ~S~%"
                                         fill)
                (push fill results)))
            (advance-one-inst segment)
            (incf insts-from-end))
          (note-resolved-dependencies segment inst)
          (push inst results)
          #+sb-show-assem (format *trace-output* "emitting ~S~%" inst)
          (advance-one-inst segment))))

    ;; Keep scheduling stuff until we run out.
    (loop
      (let ((inst (schedule-one-inst segment nil)))
        (unless inst
          (return))
        (push inst results)
        (advance-one-inst segment)))

    ;; Now call the emitters, but turn the scheduler off for the duration.
    (setf (segment-run-scheduler segment) nil)
    (assemble (segment)
     (dolist (inst results)
       (if (eq inst :nop)
           (sb-c:emit-nop segment)
           (funcall (inst-emitter inst) segment))))
    (setf (segment-run-scheduler segment) t))

  ;; Clear out any residue left over.
  (setf (segment-inst-number segment) 0)
  (setf (segment-queued-branches segment) nil)
  (setf (segment-branch-countdown segment) nil)
  (setf (segment-emittable-insts-sset segment) (make-sset))
  (fill (segment-readers segment) nil)
  (fill (segment-writers segment) nil)

  ;; That's all, folks.
  (values))

;;; a utility for maintaining the segment-delayed list. We cdr down
;;; list n times (extending it if necessary) and then push thing on
;;; into the car of that cons cell.
(defun add-to-nth-list (list thing n)
  (do ((cell (or list (setf list (list nil)))
             (or (cdr cell) (setf (cdr cell) (list nil))))
       (i n (1- i)))
      ((zerop i)
       (push thing (car cell))
       list)))

;;; Find the next instruction to schedule and return it after updating
;;; any dependency information. If we can't do anything useful right
;;; now, but there is more work to be done, return :NOP to indicate
;;; that a nop must be emitted. If we are all done, return NIL.
(defun schedule-one-inst (segment delay-slot-p)
  (do ((prev nil remaining)
       (remaining (segment-emittable-insts-queue segment) (cdr remaining)))
      ((null remaining))
    (let ((inst (car remaining)))
      (unless (and delay-slot-p
                   (instruction-attributep (inst-attributes inst)
                                           variable-length))
        ;; We've got us a live one here. Go for it.
        #+sb-show-assem (format *trace-output* "emitting ~S~%" inst)
        ;; Delete it from the list of insts.
        (if prev
            (setf (cdr prev) (cdr remaining))
            (setf (segment-emittable-insts-queue segment)
                  (cdr remaining)))
        ;; Note that this inst has been emitted.
        (note-resolved-dependencies segment inst)
        ;; And return.
        (return-from schedule-one-inst
                     ;; Are we wanting to flush this instruction?
                     (if (inst-emitter inst)
                         ;; Nope, it's still a go. So return it.
                         inst
                         ;; Yes, so pick a new one. We have to start
                         ;; over, because note-resolved-dependencies
                         ;; might have changed the emittable-insts-queue.
                         (schedule-one-inst segment delay-slot-p))))))
  ;; Nothing to do, so make something up.
  (cond ((segment-delayed segment)
         ;; No emittable instructions, but we have more work to do. Emit
         ;; a NOP to fill in a delay slot.
         #+sb-show-assem (format *trace-output* "emitting a NOP~%")
         :nop)
        (t
         ;; All done.
         nil)))

;;; This function is called whenever an instruction has been
;;; scheduled, and we want to know what possibilities that opens up.
;;; So look at all the instructions that this one depends on, and
;;; remove this instruction from their dependents list. If we were the
;;; last dependent, then that dependency can be emitted now.
(defun note-resolved-dependencies (segment inst)
  (aver (sset-empty (inst-read-dependents inst)))
  (aver (sset-empty (inst-write-dependents inst)))
  (do-sset-elements (dep (inst-write-dependencies inst))
    ;; These are the instructions who have to be completed before our
    ;; write fires. Doesn't matter how far before, just before.
    (let ((dependents (inst-write-dependents dep)))
      (sset-delete inst dependents)
      (when (and (sset-empty dependents)
                 (sset-empty (inst-read-dependents dep)))
        (insert-emittable-inst segment dep))))
  (do-sset-elements (dep (inst-read-dependencies inst))
    ;; These are the instructions who write values we read. If there
    ;; is no delay, then just remove us from the dependent list.
    ;; Otherwise, record the fact that in n cycles, we should be
    ;; removed.
    (if (zerop (inst-delay dep))
        (let ((dependents (inst-read-dependents dep)))
          (sset-delete inst dependents)
          (when (and (sset-empty dependents)
                     (sset-empty (inst-write-dependents dep)))
            (insert-emittable-inst segment dep)))
        (setf (segment-delayed segment)
              (add-to-nth-list (segment-delayed segment)
                               (cons dep inst)
                               (inst-delay dep)))))
  (values))

;;; Process the next entry in segment-delayed. This is called whenever
;;; anyone emits an instruction.
(defun advance-one-inst (segment)
  (let ((delayed-stuff (pop (segment-delayed segment))))
    (dolist (stuff delayed-stuff)
      (if (consp stuff)
          (let* ((dependency (car stuff))
                 (dependent (cdr stuff))
                 (dependents (inst-read-dependents dependency)))
            (sset-delete dependent dependents)
            (when (and (sset-empty dependents)
                       (sset-empty (inst-write-dependents dependency)))
              (insert-emittable-inst segment dependency)))
          (insert-emittable-inst segment stuff)))))

;;; Note that inst is emittable by sticking it in the
;;; SEGMENT-EMITTABLE-INSTS-QUEUE list. We keep the emittable-insts
;;; sorted with the largest ``depths'' first. Except that if INST is a
;;; branch, don't bother. It will be handled correctly by the branch
;;; emitting code in SCHEDULE-PENDING-INSTRUCTIONS.
(defun insert-emittable-inst (segment inst)
  (unless (instruction-attributep (inst-attributes inst) branch)
    #+sb-show-assem (format *trace-output* "now emittable: ~S~%" inst)
    (do ((my-depth (inst-depth inst))
         (remaining (segment-emittable-insts-queue segment) (cdr remaining))
         (prev nil remaining))
        ((or (null remaining) (> my-depth (inst-depth (car remaining))))
         (if prev
             (setf (cdr prev) (cons inst remaining))
             (setf (segment-emittable-insts-queue segment)
                   (cons inst remaining))))))
  (values))
)) ; end PROGN

;;;; structure used during output emission

;;; a constraint on how the output stream must be aligned
(defstruct (alignment-note (:include annotation)
                            (:conc-name alignment-)
                            (:predicate alignment-p)
                            (:constructor make-alignment (bits size pattern))
                            (:copier nil))
  ;; the minimum number of low-order bits that must be zero
  (bits 0 :type alignment)
  ;; the amount of filler we are assuming this alignment op will take
  (size 0 :type (integer 0 #.(1- (ash 1 max-alignment))))
  ;; the byte used as filling or :LONG-NOP, indicating to call EMIT-LONG-NOP
  ;; to emit a filling pattern
  (pattern 0 :type (or possibly-signed-assembly-unit
                       (member :long-nop))))

;;; a reference to someplace that needs to be back-patched when
;;; we actually know what label positions, etc. are
(defstruct (back-patch (:include annotation)
                        (:constructor make-back-patch (size fun))
                        (:copier nil))
  ;; the area affected by this back-patch
  (size 0 :type index :read-only t)
  ;; the function to use to generate the real data
  (fun nil :type function :read-only t))

;;; This is similar to a BACK-PATCH, but also an indication that the
;;; amount of stuff output depends on label positions, etc.
;;; BACK-PATCHes can't change their mind about how much stuff to emit,
;;; but CHOOSERs can.
(defstruct (chooser (:include annotation)
                     (:constructor make-chooser
                                   (size alignment maybe-shrink worst-case-fun))
                     (:copier nil))
  ;; the worst case size for this chooser. There is this much space
  ;; allocated in the output buffer.
  (size 0 :type index :read-only t)
  ;; the worst case alignment this chooser is guaranteed to preserve
  ;; (Q: why can't we guarantee to preserve nothing, thus simplifying the API?)
  (alignment 0 :type alignment :read-only t)
  ;; the function to call to determine if we can use a shorter
  ;; sequence. It returns NIL if nothing shorter can be used, or emits
  ;; that sequence and returns T.
  (maybe-shrink nil :type function :read-only t)
  ;; the function to call to generate the worst case sequence. This is
  ;; used when nothing else can be condensed.
  (worst-case-fun nil :type function :read-only t))

;;; This is used internally when we figure out a chooser or alignment
;;; doesn't really need as much space as we initially gave it.
(defstruct (filler (:include annotation)
                    (:constructor make-filler (bytes))
                    (:copier nil))
  ;; the number of bytes of filler here
  (bytes 0 :type index))
(declaim (freeze-type annotation))

;;;; output functions

;;; interface: Emit the supplied BYTE to SEGMENT, growing SEGMENT if
;;; necessary.
(defun emit-byte (segment byte)
  (declare (type segment segment))
  (declare (type possibly-signed-assembly-unit byte))
  (let ((old-index (segment-current-index segment)))
    (incf (segment-current-index segment))
    (setf (aref (segment-buffer segment) old-index)
          (logand byte assembly-unit-mask)))
  (incf (segment-current-posn segment))
  (values))

;;; internal: Output AMOUNT bytes to SEGMENT, either copies of
;;; PATTERN (if that is an integer), or by calling EMIT-LONG-NOP
;;; (if PATTERN is :LONG-NOP).
(defun %emit-skip (segment amount &optional (pattern 0))
  (declare (type segment segment)
           (type index amount))
  (etypecase pattern
    (integer
     (dotimes (i amount)
       (emit-byte segment pattern)))
    ;; EMIT-LONG-NOP does not exist for most backends.
    ;; Better to get an ECASE error than undefined-function.
    #+x86-64
    ((eql :long-nop)
     (sb-vm:emit-long-nop segment amount)))
  (values))

;;; This is used to handle the common parts of annotation emission. We
;;; just assign the POSN and INDEX of NOTE and tack it on to the end
;;; of SEGMENT's annotations list.
(defun emit-annotation (segment note)
  (declare (type segment segment)
           (type annotation note))
  (when (annotation-posn note)
    (error "attempt to emit ~S a second time" note))
  (setf (annotation-posn note) (segment-current-posn segment))
  (setf (annotation-index note) (segment-current-index segment))
  (let ((last (segment-last-annotation segment))
        (new (list note)))
    (setf (segment-last-annotation segment)
          (if last
              (setf (cdr last) new)
              (setf (segment-annotations segment) new))))
  (values))

;;; Note that the instruction stream has to be back-patched when label
;;; positions are finally known. SIZE bytes are reserved in SEGMENT,
;;; and function will be called with two arguments: the segment and
;;; the position. The function should look at the position and the
;;; position of any labels it wants to and emit the correct sequence.
;;; (And it better be the same size as SIZE). SIZE can be zero, which
;;; is useful if you just want to find out where things ended up.
(defun emit-back-patch (segment size function)
  (emit-annotation segment (make-back-patch size function))
  (%emit-skip segment size))

;;; Note that the instruction stream here depends on the actual
;;; positions of various labels, so can't be output until label
;;; positions are known. Space is made in SEGMENT for at most SIZE
;;; bytes. When all output has been generated, the MAYBE-SHRINK
;;; functions for all choosers are called with three arguments: the
;;; segment, the position, and a magic value. The MAYBE-SHRINK
;;; decides if it can use a shorter sequence, and if so, emits that
;;; sequence to the segment and returns T. If it can't do better than
;;; the worst case, it should return NIL (without emitting anything).
;;; When calling LABEL-POSITION, it should pass it the position and
;;; the magic-value it was passed so that LABEL-POSITION can return
;;; the correct result. If the chooser never decides to use a shorter
;;; sequence, the WORST-CASE-FUN will be called, just like a
;;; BACK-PATCH. (See EMIT-BACK-PATCH.)
(defun emit-chooser (segment size alignment maybe-shrink worst-case-fun)
  (declare (type segment segment) (type index size) (type alignment alignment)
           (type function maybe-shrink worst-case-fun))
  (let ((chooser (make-chooser size alignment maybe-shrink worst-case-fun)))
    (emit-annotation segment chooser)
    (%emit-skip segment size)
    (adjust-alignment-after-chooser segment chooser)))

;;; Return T if there is an alignment annotation anywhere in between
;;; the FROM and TO annotations.
(defun any-alignment-between-p (segment from to)
  (let ((from (annotation-index from))
        (to (annotation-index to)))
    (dolist (x (segment-alignment-annotations segment))
      (when (<= from (annotation-index x) to)
        (return t)))))

;;; This is called in EMIT-CHOOSER and COMPRESS-SEGMENT in order to
;;; recompute the current alignment information in light of this
;;; chooser. If the alignment guaranteed by the chooser is less than
;;; the segment's current alignment, we have to adjust the segment's
;;; notion of the current alignment.
;;;
;;; The hard part is recomputing the sync posn, because it's not just
;;; the chooser's posn. Consider a chooser that emits either one or
;;; three words. It preserves 8-byte (3 bit) alignments, because the
;;; difference between the two choices is 8 bytes.
(defun adjust-alignment-after-chooser (segment chooser)
  (declare (type segment segment) (type chooser chooser))
  (let ((alignment (chooser-alignment chooser))
        (seg-alignment (segment-alignment segment)))
    (when (< alignment seg-alignment)
      ;; The chooser might change the alignment of the output. So we
      ;; have to figure out what the worst case alignment could be.
      (setf (segment-alignment segment) alignment)
      (let* ((posn (chooser-posn chooser))
             (sync-posn (segment-sync-posn segment))
             (offset (- posn sync-posn))
             (delta (logand offset (1- (ash 1 alignment)))))
        (setf (segment-sync-posn segment) (- posn delta)))))
  (values))

;;; This is used internally whenever a chooser or alignment decides it
;;; doesn't need as much space as it originally thought.
;;; This function used to extend an existing filler instead of creating
;;; a new one when the previous segment annotation was a filler. Now
;;; this is only done if the previous filler is immediately adjacent
;;; to the new one in the segment, too. To see why this restriction is
;;; necessary, consider a jump followed by an alignment made of
;;; multi-byte NOPs when both are shrunk: The shortened alignment is
;;; reemitted at its original _start_ position but the joined filler
;;; would extend over this position and instead leave a subsequence of
;;; the segment up to the alignment's original _end_ position visible.
(defun %emit-filler (segment n-bytes)
  (declare (type index n-bytes))
  (let ((last (segment-last-annotation segment)))
    (cond ((and last
                (filler-p (car last))
                (= (+ (filler-index (car last))
                      (filler-bytes (car last)))
                   (segment-current-index segment)))
           (incf (filler-bytes (car last)) n-bytes))
          (t
           (emit-annotation segment (make-filler n-bytes)))))
  (incf (segment-current-index segment) n-bytes)
  (values))

;;; EMIT-LABEL (the interface) basically just expands into this,
;;; supplying the SEGMENT and VOP.
(defun %emit-label (segment vop label)
  (declare (ignore vop))
  (when (segment-run-scheduler segment)
    (schedule-pending-instructions segment))
  (let ((postits (segment-postits segment)))
    (setf (segment-postits segment) nil)
    (dolist (postit postits)
      (emit-back-patch segment 0 postit)))
  (emit-annotation segment label))

;;; Called by the EMIT-ALIGNMENT macro to emit an alignment note. We check to
;;; see if we can guarantee the alignment restriction by just outputting a
;;; fixed number of bytes. If so, we do so. Otherwise, we create and emit an
;;; alignment note.
(defun %emit-alignment (segment vop bits &optional (pattern 0))
  (declare (ignore vop))
  (when (segment-run-scheduler segment)
    (schedule-pending-instructions segment))
  (let ((alignment (segment-alignment segment))
        (offset (- (segment-current-posn segment)
                   (segment-sync-posn segment))))
    (cond ((> bits alignment)
           ;; We need more bits of alignment. Emit an alignment note.
           ;; The ALIGNMENT many least significant bits of (- OFFSET)
           ;; give the amount of bytes to skip to get back in sync with
           ;; ALIGNMENT, and one-bits to the left of that up to position
           ;; BITS provide the remaining amount.
           (let ((size (deposit-field (- offset)
                                      (byte 0 alignment)
                                      (1- (ash 1 bits)))))
             (aver (> size 0))
             (let ((thing (make-alignment bits size pattern)))
               (emit-annotation segment thing)
               (push thing (segment-alignment-annotations segment)))
             (%emit-skip segment size pattern))
           (setf (segment-alignment segment) bits)
           (setf (segment-sync-posn segment) (segment-current-posn segment)))
          (t
           ;; The last alignment was more restrictive than this one.
           ;; So we can just figure out how much noise to emit
           ;; assuming the last alignment was met.
           (let* ((mask (1- (ash 1 bits)))
                  (new-offset (logand (+ offset mask) (lognot mask))))
             (%emit-skip segment (- new-offset offset) pattern))
           ;; But we emit an alignment with size=0 so we can verify
           ;; that everything works.
           (emit-annotation segment (make-alignment bits 0 pattern)))))
  (values))

;;; This is used to find how ``aligned'' different offsets are.
;;; Returns the number of low-order 0 bits, up to MAX-ALIGNMENT.
(defun find-alignment (offset)
  (dotimes (i max-alignment max-alignment)
    (when (logbitp i offset)
      (return i))))

;;; Emit a postit. The function will be called as a back-patch with
;;; the position the following instruction is finally emitted. Postits
;;; do not interfere at all with scheduling.
(defun %emit-postit (segment function)
  (push function (segment-postits segment))
  (values))

;;;; output compression/position assignment stuff

;;; Grovel though all the annotations looking for choosers. When we
;;; find a chooser, invoke the maybe-shrink function. If it returns T,
;;; it output some other byte sequence.
(defun compress-output (segment)
  (dotimes (i 5) ; it better not take more than one or two passes.
    (let ((delta 0))
      (setf (segment-alignment segment) max-alignment)
      (setf (segment-sync-posn segment) 0)
      (do* ((prev nil)
            (remaining (segment-annotations segment) next)
            (next (cdr remaining) (cdr remaining)))
           ((null remaining))
        (let* ((note (car remaining))
               (posn (annotation-posn note)))
          (unless (zerop delta)
            (decf posn delta)
            (setf (annotation-posn note) posn))
          (cond
           ((chooser-p note)
            (with-modified-segment-index-and-posn (segment (chooser-index note)
                                                           posn)
              (setf (segment-last-annotation segment) prev)
              (cond
               ((funcall (chooser-maybe-shrink note) segment note posn delta)
                ;; It emitted some replacement.
                (let ((new-size (- (segment-current-index segment)
                                   (chooser-index note)))
                      (old-size (chooser-size note)))
                  (when (> new-size old-size)
                    (error "~S emitted ~W bytes, but claimed its max was ~W."
                           note new-size old-size))
                  (let ((additional-delta (- old-size new-size)))
                    (when (< (find-alignment additional-delta)
                             (chooser-alignment note))
                      (error "~S shrunk by ~W bytes, but claimed that it ~
                              preserves ~W bits of alignment."
                             note additional-delta (chooser-alignment note)))
                    (incf delta additional-delta)
                    (%emit-filler segment additional-delta))
                  (setf prev (segment-last-annotation segment))
                  (if prev
                      (setf (cdr prev) (cdr remaining))
                      (setf (segment-annotations segment)
                            (cdr remaining)))))
               (t
                ;; The chooser passed on shrinking. Make sure it didn't
                ;; emit anything.
                (unless (= (segment-current-index segment)
                           (chooser-index note))
                  (error "Chooser ~S passed, but not before emitting ~W bytes."
                         note
                         (- (segment-current-index segment)
                            (chooser-index note))))
                ;; Act like we just emitted this chooser.
                (let ((size (chooser-size note)))
                  (incf (segment-current-index segment) size)
                  (incf (segment-current-posn segment) size))
                ;; Adjust the alignment accordingly.
                (adjust-alignment-after-chooser segment note)
                ;; And keep this chooser for next time around.
                (setf prev remaining)))))
           ((alignment-p note)
            (unless (zerop (alignment-size note))
              ;; Re-emit the alignment, letting it collapse if we know
              ;; anything more about the alignment guarantees of the
              ;; segment.
              (let ((index (alignment-index note)))
                (with-modified-segment-index-and-posn (segment index posn)
                  (setf (segment-last-annotation segment) prev)
                  (%emit-alignment segment nil (alignment-bits note)
                                   (alignment-pattern note))
                  (let* ((new-index (segment-current-index segment))
                         (size (- new-index index))
                         (old-size (alignment-size note))
                         (additional-delta (- old-size size)))
                    (when (minusp additional-delta)
                      (error "Alignment ~S needs more space now?  It was ~W, ~
                              and is ~W now."
                             note old-size size))
                    (when (plusp additional-delta)
                      (%emit-filler segment additional-delta)
                      (incf delta additional-delta)))
                  (setf prev (segment-last-annotation segment))
                  (if prev
                      (setf (cdr prev) (cdr remaining))
                      (setf (segment-annotations segment)
                            (cdr remaining)))))))
           (t
            (setf prev remaining)))))
      (when (zerop delta)
        (return))
      (decf (segment-final-posn segment) delta)))
  (values))

;;; We have run all the choosers we can, so now we have to figure out
;;; exactly how much space each alignment note needs.
(defun finalize-positions (segment)
  (let ((delta 0))
    (do* ((prev nil)
          (remaining (segment-annotations segment) next)
          (next (cdr remaining) (cdr remaining)))
         ((null remaining))
      (let* ((note (car remaining))
             (posn (- (annotation-posn note) delta)))
        (cond
         ((alignment-p note)
          (let* ((bits (alignment-bits note))
                 (mask (1- (ash 1 bits)))
                 (new-posn (logand (+ posn mask) (lognot mask)))
                 (size (- new-posn posn))
                 (old-size (alignment-size note))
                 (additional-delta (- old-size size)))
            (aver (<= 0 size old-size))
            (unless (zerop additional-delta)
              (setf (segment-last-annotation segment) prev)
              (incf delta additional-delta)
              (with-modified-segment-index-and-posn (segment
                                                     (alignment-index note)
                                                     posn)
                (when (eql (alignment-pattern note) :long-nop)
                  ;; We need to re-emit the alignment because a shorter
                  ;; multi-byte NOP pattern is most of the time not a
                  ;; prefix of a longer one.
                  (%emit-skip segment size (alignment-pattern note)))
                (%emit-filler segment additional-delta)
                (setf prev (segment-last-annotation segment))
                (if prev
                    (setf (cdr prev) next)
                    (setf (segment-annotations segment) next))))))
         (t
          (setf (annotation-posn note) posn)
          (setf prev remaining)
          (setf next (cdr remaining))))))
    (unless (zerop delta)
      (decf (segment-final-posn segment) delta)))
  (values))

;;; Grovel over segment, filling in any backpatches. If any choosers
;;; are left over, we need to emit their worst case variant.
(defun process-back-patches (segment)
  (do* ((prev nil)
        (remaining (segment-annotations segment) next)
        (next (cdr remaining) (cdr remaining)))
      ((null remaining))
    (let ((note (car remaining)))
      (flet ((fill-in (function old-size)
               (let ((index (annotation-index note))
                     (posn (annotation-posn note)))
                 (with-modified-segment-index-and-posn (segment index posn)
                   (setf (segment-last-annotation segment) prev)
                   (funcall function segment posn)
                   (let ((new-size (- (segment-current-index segment) index)))
                     (unless (= new-size old-size)
                       (error "~S emitted ~W bytes, but claimed it was ~W."
                              note new-size old-size)))
                   (let ((tail (segment-last-annotation segment)))
                     (if tail
                         (setf (cdr tail) next)
                         (setf (segment-annotations segment) next)))
                   (setf next (cdr prev))))))
        (cond ((back-patch-p note)
               (fill-in (back-patch-fun note)
                        (back-patch-size note)))
              ((chooser-p note)
               (fill-in (chooser-worst-case-fun note)
                        (chooser-size note)))
              (t
               (setf prev remaining)))))))

;;; Replace the SEGMENT-BUFFER of SEGMENT with a vector that contains
;;; only the valid content of the original buffer, that is, the parts
;;; not covered by fillers. Set FINAL-INDEX of SEGMENT to the length
;;; of the new vector and return this length.
(defun compact-segment-buffer (segment)
  (decf (segment-final-posn segment) (segment-header-skew segment))
  (let ((buffer (segment-buffer segment))
        (new-buffer (make-array (segment-final-posn segment)
                                :element-type 'assembly-unit))
        (i0 (segment-header-skew segment)) ; input index
        (index 0)) ; output index
    (declare (type (simple-array assembly-unit 1) buffer)
             (type index index))
    (flet ((frob (i0 i1)
             (when (< i0 i1)
               (replace new-buffer buffer :start1 index :start2 i0 :end2 i1)
               (incf index (- i1 i0)))))
      (dolist (note (segment-annotations segment))
        (when (filler-p note)
          (let ((i1 (filler-index note)))
            (frob i0 i1)
            (setf i0 (+ i1 (filler-bytes note))))))
      (frob i0 (segment-final-index segment)))
    (aver (= index (segment-final-posn segment)))
    (setf (segment-buffer segment) new-buffer)
    (setf (segment-final-index segment) (segment-final-posn segment))
    new-buffer))


;;;; interface to the rest of the compiler

;;; Map of opcode symbol to function that emits it into the byte stream
;;; (or with the schedulding assembler, into the queue)
;;; Key is a symbol. Value is either #<function> or (#<function>),
;;; the latter if function wants to receive prefix arguments.
(defglobal *inst-encoder* (make-hash-table)) ; keys are symbols

;;; Return T only if STATEMENT has a label which is potentially a branch
;;; target, and not merely labeled to store a location of interest.
(defun labeled-statement-p (statement &aux (labels (stmt-labels statement)))
  (if (listp labels) (some #'label-usedp labels) (label-usedp labels)))

#-x86-64
(progn
  (defun extract-prefix-keywords (x) x)
  (defun decode-prefix (args) args))

(defun dump-symbolic-asm (section stream &aux last-vop all-labels (n 0))
  (format stream "~2&Assembler input:~%")
  (do ((statement (stmt-next (section-start section)) (stmt-next statement))
       (*print-pretty* nil))
      ((null statement))
    (incf n)
    (binding* ((vop (stmt-vop statement) :exit-if-null))
      (unless (eq vop last-vop)
        (format stream "## ~A~%" (sb-c::vop-name vop)))
      (setq last-vop vop))
    (let ((op (stmt-mnemonic statement))
          (eol-comment ""))
      (awhen (stmt-labels statement)
        (let ((list (ensure-list it))
              (usedp (labeled-statement-p statement)))
          (setq all-labels (append list all-labels)
                eol-comment (format nil " # ~{~@[~A~^, ~]~}"
                                    (mapcar #'label-comment list)))
          (format stream "~A~{~A: ~}~A~%"
                  (if usedp "" "# ")
                  list
                  (if usedp "" " (notused)"))))
      (if (functionp op)
          (format stream "# postit ~S~A~%" op eol-comment)
          (format stream "    ~:@(~A~) ~{~A~^, ~}~A~%"
                  op
                  (if (consp (gethash op *inst-encoder*))
                      (decode-prefix (stmt-operands statement))
                      (stmt-operands statement))
                  eol-comment))))
  (let ((*print-length* nil)
        (*print-pretty* t)
        (*print-right-margin* 80))
    (format stream "# Unused labels:~%~A~%"
            ;; it comes out in approximately numeric order when reversed
            (nreverse (remove-if #'label-usedp all-labels))))
  (format stream "# end of input~%")
  n)

;;; Append SECOND to the end of FIRST, make SECOND empty, and return FIRST.
(defun append-sections (first second)
  (let ((last-stmt (section-tail first)))
    (let ((head (section-start second)))
      (aver (eq (stmt-mnemonic head) :ignore))
      (when (stmt-next head)
        (setf (stmt-next last-stmt) (stmt-next head)
              (stmt-prev (stmt-next head)) last-stmt)
        (setf last-stmt (section-tail second)))
      (setf (stmt-next head) nil
            (section-tail second) head))
    (setf (section-tail first) last-stmt))
  first)

;;; Combine INPUTS into one assembly stream and assemble into SEGMENT
(defun %assemble (segment section)
  (let ((*current-vop* nil)
        (in-without-scheduling)
        (was-scheduling))
    ;; HEADER-SKEW is 1 word (in bytes) if the boxed code header word count is odd.
    ;; The purpose is to trick the assembler into performing alignment such that
    ;; word 1 is the first doubleword-aligned word. It would not work, for example,
    ;; to merely inform the assembler that it has less than MAX-ALIGNMENT bits
    ;; of alignment when starting out, because then there would be no way for it
    ;; to recover to MAX-ALIGNMENT bits as required for simple-funs. So we tell it
    ;; that it starts out with MAX-ALIGNMENT, but it's not word index 0 that has
    ;; that alignment. There is probably a way to do this using the SYNC-POSN slot,
    ;; but I couldn't figure that out. A more logical solution would be to emit
    ;; _all_ boxed words as .SKIP directives. Then it's not so much a "trick" as
    ;; a complete, albeit wasteful, representation of the code object.
    ;; A little thinking shows that we only need to fake at most 1 boxed word.
    (when (plusp (segment-header-skew segment))
      (%emit-skip segment (segment-header-skew segment)))
    (%emit-label segment nil (segment-origin segment))
    #+sb-dyncount
    (setf (segment-collect-dynamic-statistics segment) *collect-dynamic-statistics*)
    (when (and sb-c::*compiler-trace-output*
               (memq :symbolic-asm sb-c::*compile-trace-targets*))
      (dump-symbolic-asm section sb-c::*compiler-trace-output*))
    (do ((statement (stmt-next (section-start section)) (stmt-next statement)))
        ((null statement))
      (awhen (stmt-vop statement) (setq *current-vop* it))
      (dolist (label (ensure-list (stmt-labels statement)))
        (%emit-label segment *current-vop* label))
      (let ((mnemonic (stmt-mnemonic statement))
            (operands (stmt-operands statement)))
        (if (functionp mnemonic)
            (%emit-postit segment mnemonic)
            (case mnemonic
              (.begin-without-scheduling
               (aver (not in-without-scheduling))
               (setq in-without-scheduling t
                     was-scheduling (segment-run-scheduler segment))
               (when was-scheduling
                 (schedule-pending-instructions segment)
                 (setf (segment-run-scheduler segment) nil)))
              (.end-without-scheduling
               (aver in-without-scheduling)
               (setf (segment-run-scheduler segment) was-scheduling
                     in-without-scheduling nil
                     was-scheduling nil))
               ((nil)) ; ignore
               (t
                (let ((encoder (gethash mnemonic *inst-encoder*)))
                  (cond (encoder
                         (instruction-hooks segment)
                         (apply (the function (if (listp encoder) (car encoder) encoder))
                                segment
                                (perform-operand-lowering operands)))
                        (t
                         (bug "No encoder for ~S" mnemonic))))))))))
  (finalize-segment segment))

;;; The interface to %ASSEMBLE
(defun assemble-sections (asmstream entries segment)
  (let* ((n-entries (length entries))
         (trailer-len (* (+ n-entries 1) 4))
         (end-text (gen-label))
         (combined
           (append-sections
            (append-sections (asmstream-data-section asmstream)
                             (asmstream-code-section asmstream))
            (let ((section (asmstream-elsewhere-section asmstream)))
              (emit section
                    end-text
                    `(.align 2)
                    `(.skip ,trailer-len)
                    `(.align ,sb-vm:n-lowtag-bits))
              section)))
         (fun-offsets)
         (octets (segment-buffer (%assemble segment combined)))
         (index (length octets)))
    ;; N-ENTRIES is packed into a uint16 with 5 other bits
    (declare (type (unsigned-byte 11) n-entries))

    ;; Total size of the code object must be multiple of 2 lispwords
    (aver (not (logtest (+ (segment-header-skew segment) index)
                        sb-vm:lowtag-mask)))
    (sb-sys:with-pinned-objects (octets)
      (let ((sap (sb-sys:vector-sap octets)))
        ;; TODO: as the comment in GENERATE-CODE suggests, we would like to align
        ;; code objects to 16 bytes for 32-bit x86. That's simple - all we have to do
        ;; is ensure that each code blob is a multiple of 16 bytes in size.
        ;; Since code can only go on pages reserved for code, there will be no smaller
        ;; object on the same page to cause misalignment.
        ;; Extra padding can be inserted before the trailing simple-fun table.
        (let ((padding (if (eql n-entries 0)
                           0
                           (- index trailer-len (label-position end-text)))))
          (unless (and (typep trailer-len '(unsigned-byte 16))
                       (typep n-entries '(unsigned-byte 12))
                       ;; Padding must be representable in 4 bits at assembly time,
                       ;; but CODE-HEADER/TRAILER-ADJUST can increase the padding.
                       (typep padding '(unsigned-byte 4)))
            (bug "Oversized code component?"))
          (setf (sap-ref-16 sap (- index 2)) trailer-len)
          (setf (sap-ref-16 sap (- index 4)) (logior (ash n-entries 5) padding)))
        (decf index trailer-len)
        ;; Iteration over label positions occurs from numerically highest
        ;; to lowest, which is right because the 0th indexed simple-fun
        ;; has the lowest entry offset, and is the last one written
        ;; into the trailer table. The trailer table is indexed backwards:
        ;;  ... simple-fun-2 | simple-fun-1 | simple-fun-0
        ;; And because we use PUSH, we collect them in forward order
        ;; which is the correct thing to return.
        (dolist (entry entries)
          (let ((val (label-position (sb-c::entry-info-offset entry))))
            (push val fun-offsets)
            (setf (sap-ref-32 sap index) val)
            (incf index 4)))))
    (aver (= index (- (length octets) 4)))
    (values segment
            (label-position end-text)
            (segment-fixup-notes segment)
            fun-offsets)))

;;; Most backends do not convert register TNs into a different type of
;;; internal object prior to handing the operands off to the emitter.
;;; x86-64 does have a different representation, which makes some of
;;; the emitter logic easier to understand.
#-x86-64
(defun perform-operand-lowering (operands) operands)

(defun trace-inst (section mnemonic operands)
  (when sb-c::*compiler-trace-output*
    (let* ((asmstream *asmstream*)
           (section-name
            (if (eq section (asmstream-code-section asmstream))
                :regular
                :elsewhere)))
      (sb-c::trace-instruction section-name *current-vop* mnemonic operands
                               (asmstream-tracing-state asmstream)))))

(defmacro inst (&whole whole mnemonic &rest args)
  "Emit the specified instruction to the current segment."
  (let* ((sym (find-symbol (string mnemonic) *backend-instruction-set-package*))
         (definedp (nth-value 1 (gethash sym *inst-encoder*))))
    (cond ((not definedp)
           ;; INST* can not execute random forms, so MNEMONIC must be a literal to be
           ;; recognized as a macro instruction. It's basically a lisp macro that can
           ;; coexist with other identically-named lisp macros or functions.
           ;; For example, arm64 has {ASR, LSR} as DEFUNs and macro instructions.
           ;; By using an unusual convention of a symbol with #\: in its name,
           ;; FIND-SYMBOL reliably tests whether a macro is defined without further
           ;; using FBOUNDP or MACRO-FUNCTION.
           (let ((macro (find-symbol (format nil "M:~A" mnemonic)
                                     *backend-instruction-set-package*)))
             (when macro
               (return-from inst `(,macro ,@args))))
           (warn "Undefined instruction: ~s in~% ~s" mnemonic whole)
           `(error "Undefined instruction: ~s in~% ~s" ',mnemonic ',whole))
          (t
           `(inst* ',sym ,@args)))))

;;; Place INST in the current assembly section (or sometimes SEGMENT)
;;; based on *CURRENT-DESTINATION*. The latter occurs in two scenarios:
;;;
;;; (1) FINALIZE-SEGMENT invokes choosers which emit instructions.
;;;     (see e.g. any of the EMIT-COMPUTE-INST definitions).
;;;     Rather than forcing choosers to use a different API - perhaps
;;;     named ENCODE-INST - they can just use the same old INST macro.
;;; (2) ALIEN-CALLBACK-ASSEMBLER-WRAPPER uses the assembler backend
;;;     but not its front-end. This could be changed, but it's not wrong.
;;; As such, we must detect that we are emitting directly to machine code.
;;;
(defun inst* (mnemonic &rest operands)
  (let ((dest
         (etypecase mnemonic
           (symbol
            ;; If called by a vop, the first argument is a mnemonic.
            *current-destination*)
           (segment
            ;; If called by an instruction encoder to encode other instructions,
            ;; the first argument is a SEGMENT. This facilitates a different kind of
            ;; macro-instruction, one which decides only at encoding time what other
            ;; instructions to emit. Similar results could be achievedy by factoring
            ;; out other emitters into callable functions, though the INST macro
            ;; tends to be a more convenient interface.
            (prog1 mnemonic (setq mnemonic (the symbol (pop operands)))))))
        (action (gethash mnemonic *inst-encoder*)))
    (unless action ; try canonicalizing again
      (setq mnemonic (find-symbol (string mnemonic)
                                  *backend-instruction-set-package*)
            action (gethash mnemonic *inst-encoder*))
      (aver action))
    (when (listp action) (setq operands (extract-prefix-keywords operands)))
    (typecase dest
      (cons ; streaming in to the assembler
       (trace-inst dest mnemonic operands)
       (emit dest (cons mnemonic operands)))
      (segment ; streaming out of the assembler
       (instruction-hooks dest)
       (apply (the function (if (listp action) (car action) action))
              dest (perform-operand-lowering operands))))))

(defun emit-label (label)
  "Emit LABEL at this location in the current section."
  (let ((s *current-destination*))
    (trace-inst s :label label)
    (emit s label)))

(defun emit-postit (function)
  (let ((s *current-destination*))
    (emit s (the function function))))

(defun emit-alignment (bits &optional (pattern 0))
  "Emit an alignment restriction to the current segment."
  (let ((s *current-destination*))
    (trace-inst s :align bits)
    (emit s `(.align ,bits ,pattern))))

(defun label-position (label &optional if-after delta)
  "Return the current position for LABEL. Chooser maybe-shrink functions
   should supply IF-AFTER and DELTA in order to ensure correct results."
  (declare #-sb-xc-host (values (or null index)))
  (let ((posn (label-posn label)))
    (if (and if-after (> posn if-after))
        (- posn delta)
        posn)))

(defun finalize-segment (segment)
  "Do any final processing of SEGMENT and return the total number of bytes
   covered by this segment."
  (when (segment-run-scheduler segment)
    (schedule-pending-instructions segment))
  (setf (segment-run-scheduler segment) nil)
  (let ((postits (segment-postits segment)))
    (setf (segment-postits segment) nil)
    (dolist (postit postits)
      (emit-back-patch segment 0 postit)))
  (setf (segment-final-index segment) (segment-current-index segment))
  (setf (segment-final-posn segment) (segment-current-posn segment))
  (compress-output segment)
  (finalize-positions segment)
  (process-back-patches segment)
  (let ((skew (segment-header-skew segment)))
    (when (plusp skew)
      (dolist (note (segment-annotations segment))
        (when (label-p note)
          (decf (label-index note) skew)
          (decf (label-posn note) skew)))))
  (compact-segment-buffer segment)
  segment)

;;; Return the contents of SEGMENT as a vector. We assume SEGMENT has
;;; been finalized so that we can simply return its buffer.
(defun segment-contents-as-vector (segment)
  (declare (type segment segment))
  (aver (= (segment-final-index segment) (segment-final-posn segment)))
  (segment-buffer segment))

;;; Write the code accumulated in SEGMENT to STREAM, and return the
;;; number of bytes written. We assume that SEGMENT has been finalized.
(defun write-segment-contents (segment stream)
  (declare (type segment segment))
  (let ((v (segment-contents-as-vector segment)))
    (declare (type (simple-array assembly-unit 1) v))
    (length (write-sequence v stream))))


;;;; interface to the instruction set definition

;;; Define a function named NAME that merges its arguments into a
;;; single integer and then emits the bytes of that integer in the
;;; correct order based on the endianness of the target-backend.
(defmacro define-bitfield-emitter (name total-bits &rest byte-specs)
  (collect ((arg-names) (arg-types))
    (let* ((total-bits (eval total-bits))
           (overall-mask (ash -1 total-bits))
           (num-bytes (multiple-value-bind (quo rem)
                          (truncate total-bits assembly-unit-bits)
                        (unless (zerop rem)
                          (error "~W isn't an even multiple of ~W."
                                 total-bits assembly-unit-bits))
                        quo))
           (bytes (make-array num-bytes :initial-element nil))
           (segment-arg (gensym "SEGMENT-")))
      (dolist (byte-spec-expr byte-specs)
        (let* ((byte-spec (eval byte-spec-expr))
               (byte-size (byte-size byte-spec))
               (byte-posn (byte-position byte-spec))
               (arg (gensym (format nil "~:@(ARG-FOR-~S-~)" byte-spec-expr))))
          (when (ldb-test (byte byte-size byte-posn) overall-mask)
            (error "The byte spec ~S either overlaps another byte spec, or ~
                    extends past the end."
                   byte-spec-expr))
          (setf (ldb byte-spec overall-mask) -1)
          (arg-names arg)
          (arg-types `(type (integer ,(ash -1 (1- byte-size))
                                     ,(1- (ash 1 byte-size)))
                            ,arg))
          (multiple-value-bind (start-byte offset)
              (floor byte-posn assembly-unit-bits)
            (let ((end-byte (floor (1- (+ byte-posn byte-size))
                                   assembly-unit-bits)))
              (flet ((maybe-ash (expr offset)
                       (if (zerop offset)
                           expr
                           `(ash ,expr ,offset))))
                (declare (inline maybe-ash))
                (cond ((zerop byte-size))
                      ((= start-byte end-byte)
                       (push (maybe-ash `(ldb (byte ,byte-size 0) ,arg)
                                        offset)
                             (svref bytes start-byte)))
                      (t
                       (push (maybe-ash
                              `(ldb (byte ,(- assembly-unit-bits offset) 0)
                                    ,arg)
                              offset)
                             (svref bytes start-byte))
                       (do ((index (1+ start-byte) (1+ index)))
                           ((>= index end-byte))
                         (push
                          `(ldb (byte ,assembly-unit-bits
                                      ,(- (* assembly-unit-bits
                                             (- index start-byte))
                                          offset))
                                ,arg)
                          (svref bytes index)))
                       (let ((len (rem (+ byte-size offset)
                                       assembly-unit-bits)))
                         (push
                          `(ldb (byte ,(if (zerop len)
                                           assembly-unit-bits
                                           len)
                                      ,(- (* assembly-unit-bits
                                             (- end-byte start-byte))
                                          offset))
                                ,arg)
                          (svref bytes end-byte))))))))))
      (unless (= overall-mask -1)
        (error "There are holes."))
      (let ((forms nil))
        (dotimes (i num-bytes)
          (let ((pieces (svref bytes i)))
            (aver pieces)
            (push `(emit-byte ,segment-arg
                              ,(if (cdr pieces)
                                   `(logior ,@pieces)
                                   (car pieces)))
                  forms)))
        `(defun ,name (,segment-arg ,@(arg-names))
           (declare (type segment ,segment-arg) ,@(arg-types))
           ,@(ecase sb-c:*backend-byte-order*
               (:little-endian (nreverse forms))
               (:big-endian forms)))))))

(defun %def-inst-encoder (symbol thing &optional accept-prefixes)
  (let ((function
         (or thing ; load-time effect passes the definition
             ;; (where compile-time doesn't).
             ;; Otherwise, take what we already had so that a compile-time
             ;; effect doesn't clobber an already-working hash-table entry
             ;; if re-evaluating a define-instruction form.
             (car (ensure-list (gethash symbol *inst-encoder*))))))
    (setf (gethash symbol *inst-encoder*)
          (if accept-prefixes (cons function t) function))))

(defmacro define-instruction (name lambda-list &rest options)
  (binding* ((fun-name (intern (symbol-name name) *backend-instruction-set-package*))
             (segment-name (car lambda-list))
             (vop-name nil)
             (emitter nil)
             (decls nil)
             (attributes nil)
             (cost nil)
             (dependencies nil)
             (delay nil)
             (pinned nil)
             (pdefs nil))
    (declare (ignorable pinned))
    (dolist (option-spec options)
      (multiple-value-bind (option args)
          (if (consp option-spec)
              (values (car option-spec) (cdr option-spec))
              (values option-spec nil))
        (case option
          (:emitter
           (when emitter
             (error "You can only specify :EMITTER once per instruction."))
           (setf emitter args))
          (:declare
           (setf decls (append decls args)))
          (:attributes
           (setf attributes (append attributes args)))
          (:cost
           (setf cost (first args)))
          (:dependencies
           (setf dependencies (append dependencies args)))
          (:delay
           (when delay
             (error "You can only specify :DELAY once per instruction."))
           (setf delay args))
          (:pinned
           (setf pinned t))
          (:vop-var
           (if vop-name
               (error "You can only specify :VOP-VAR once per instruction.")
               (setf vop-name (car args))))
          (:printer
           (let* ((inst-args (second args))
                  (names (mapcar #'car inst-args)))
             (when (> (length names) (length (remove-duplicates names)))
               (error "Duplicate operand names in ~S~%" args)))
           (destructuring-bind (name operands . options) args
             (push ``(,',name (,,@(mapcar (lambda (x) ``(,',(car x) ,,@(cdr x)))
                                          operands))
                              ,,@options) pdefs)))
          (t
           (error "unknown option: ~S" option)))))
    (when emitter
      (unless cost (setf cost 1))
      #+sb-dyncount
      (push `(when (segment-collect-dynamic-statistics ,segment-name)
               (let* ((info (sb-c:ir2-component-dyncount-info
                             (sb-c:component-info
                              sb-c:*component-being-compiled*)))
                      (costs (sb-c:dyncount-info-costs info))
                      (block-number (sb-c:block-number
                                     (sb-c:ir2-block-block
                                      (sb-c:vop-block ,vop-name)))))
                 (incf (aref costs block-number) ,cost)))
            emitter)
      (when assem-scheduler-p
        (if pinned
            (setf emitter
                  `((when (segment-run-scheduler ,segment-name)
                      (schedule-pending-instructions ,segment-name))
                    ,@emitter))
            (let ((flet-name (make-symbol (concatenate 'string "ENCODE-"
                                                       (string fun-name))))
                  (inst-name '#:inst))
              (setf emitter `((flet ((,flet-name (,segment-name)
                                       ,@emitter))
                                (if (segment-run-scheduler ,segment-name)
                                    (let ((,inst-name
                                            (make-instruction
                                             (incf (segment-inst-number
                                                    ,segment-name))
                                             #',flet-name
                                             (instruction-attributes
                                              ,@attributes)
                                             (progn ,@delay))))
                                      ,@(when dependencies
                                          `((note-dependencies
                                                (,segment-name ,inst-name)
                                              ,@dependencies)))
                                      (queue-inst ,segment-name ,inst-name))
                                    (,flet-name ,segment-name)))))))))
    `(progn
       #-sb-xc-host ; The disassembler is not used on the host.
       (setf (get ',fun-name 'sb-disassem::instruction-flavors)
             (list ,@pdefs))
       ,@(when emitter
           (let* ((operands (cdr lambda-list))
                  (accept-prefixes (eq (car operands) '&prefix)))
             (when accept-prefixes (setf operands (cdr operands)))
             `((eval-when (:compile-toplevel)
                 (%def-inst-encoder ',fun-name nil ',accept-prefixes))
               (%def-inst-encoder
                ',fun-name
                (named-lambda ,(string fun-name) (,segment-name ,@operands)
                  (declare ,@decls)
                  (let ,(and vop-name `((,vop-name *current-vop*)))
                    (block ,fun-name ,@emitter)))
                ',accept-prefixes))))
       ',fun-name)))

(defun instruction-hooks (segment)
  (let ((postits (segment-postits segment)))
    (setf (segment-postits segment) nil)
    (dolist (postit postits)
      (emit-back-patch segment 0 postit))))

;;; This was convenient as merely a shorthand for DEFMACRO, but it can't be now
;;; because for example, "NOT" on the Alpha is a macro, conflicting with CL:NOT.
;;; git revision 8d21b520d9 seems to imply that using MAKE-MACRO-LAMBDA to create
;;; anonymous macros somehow failed.
(defmacro define-instruction-macro (name lambda-list &body body)
  (let ((macro (package-symbolicate
                *backend-instruction-set-package* "M:" name)))
    `(progn
       ;; Ensure that it does not exist as a directly encodable instruction
       (remhash ',(find-symbol (string name) *backend-instruction-set-package*)
                *inst-encoder*)
       (defmacro ,macro ,lambda-list ,@body))))

(%def-inst-encoder '.align
                   (lambda (segment bits &optional (pattern 0))
                     (%emit-alignment segment *current-vop* bits pattern)))
(%def-inst-encoder '.byte
                   (lambda (segment &rest bytes)
                     (dolist (byte bytes) (emit-byte segment byte))))
(%def-inst-encoder '.skip
                    (lambda (segment n-bytes &optional (pattern 0))
                      (%emit-skip segment n-bytes pattern)))
(%def-inst-encoder
 '.lispword
 (lambda (segment &rest vals)
   (flet ((emit-bytes (segment val)
            #+little-endian
            (loop for i below sb-vm:n-word-bits by 8
                  do (emit-byte segment (ldb (byte 8 i) val)))
            #+big-endian
            (loop for i from (- sb-vm:n-word-bits 8) downto 0 by 8
                  do (emit-byte segment (ldb (byte 8 i) val)))))
     (dolist (val vals)
       (cond ((label-p val)
              ;; note a fixup prior to writing the backpatch so that the fixup's
              ;; position is the location counter at the patch point
              ;; (i.e. prior to skipping N-WORD-BYTES bytes)
              ;; This fixup is *not* recorded in code->fixups. Instead, trans_code()
              ;; will fixup a counted initial subsequence of unboxed words.
              ;; Q: why are fixup notes a "compiler" abstractions?
              ;; They seem pretty assembler-related to me.
              (sb-c:note-fixup segment :absolute (sb-c:make-fixup nil :code-object 0))
              (emit-back-patch
               segment
               sb-vm:n-word-bytes
               (let ((val val)) ; capture the current label
                 (lambda (segment posn)
                   (declare (ignore posn)) ; don't care where the fixup itself is
                   (emit-bytes segment
                               (+ (sb-c:component-header-length)
                                  (- (segment-header-skew segment))
                                  (- sb-vm:other-pointer-lowtag)
                                  (label-position val)))))))
             (t
              (emit-bytes segment val)))))))

;;;; Peephole pass

(defmacro defpattern (name (opcodes1 opcodes2) lambda-list &body body)
  `(%defpattern ,name ',opcodes1 ',opcodes2 (lambda ,lambda-list ,@body)))

(defparameter *show-peephole-transforms-p* nil)
(defglobal *asm-pattern-matchers* nil)
(defglobal *asm-pattern-matchers-invoked* (make-array 20 :initial-element 0))
(defun %defpattern (name opcodes1 opcodes2 applicator)
  (let ((entry (find name *asm-pattern-matchers* :test #'string= :key #'fifth)))
    (if entry
        (setf (first entry) opcodes1
              (second entry) opcodes2
              (third entry) applicator)
        (let* ((index (length *asm-pattern-matchers*))
               (entry (list opcodes1 opcodes2  applicator index name)))
          (push entry *asm-pattern-matchers*)))))

(defun combine-instructions (section)
  ;; Triply nested loop:
  ;;   - repeatedly scan until no further changes
  ;;   -   looking for a pattern that starts at each instruction
  ;;   -      for all possible rules that match on opcodes
  ;; This is a greedy algorithm, it could do the wrong thing
  ;; by applying rules that make other rules impossible to apply,
  ;; but that seems unlikely.
  (loop
    (let* ((any-changes)
           (stmt nil)
           (next (section-start section)))
      (loop
        (setq stmt next next (stmt-next stmt))
        (unless next (return))
        ;; All the patterns examine exactly 2 instructions for now.
        ;; It should be possible to create a pattern that matches a sequence
        ;; of 3 or more instruction or has intervening random stuff
        ;; (e.g. "MOV reg, ea" + ? + "CMP reg, val") provided that the "?"
        ;; does not interact with instructions around it.
        (unless (labeled-statement-p next)
          (let ((op (stmt-mnemonic stmt))
                (next-op (stmt-mnemonic next)))
            ;; Look for a rule that can be applied
            (dolist (rule *asm-pattern-matchers*)
              (destructuring-bind (opcodes1 opcodes2 . action) rule
                ;; Don't return from the innermost loop until finding a rule that accepts
                ;; the statement. If the match on opcodes alone is deemed a hit, but the
                ;; rule fails, we would not try other rules that could have applied.
                (when (and (member op opcodes1) (member next-op opcodes2))
                  (let ((new-next (funcall (car action) stmt next)))
                    (when new-next
                      (incf (aref *asm-pattern-matchers-invoked* (cadr action)))
                      (when *show-peephole-transforms-p*
                        (format t "~&applied ~a~%" (caddr action)))
                      ;; The rule returns any non-deleted statement. It could be any
                      ;; line of the pattern. Skip backwards to ensure that we see
                      ;; patterns with next's predecessor as the first instruction.
                      (setq next (stmt-prev new-next)
                            any-changes t)
                      (return)))))))))
      (unless any-changes (return))))
  #+x86-64
  ;; Build the label -> stmt map
  (let ((label->stmt (make-hash-table)))
    (do ((stmt (section-start section) (stmt-next stmt)))
        ((null stmt))
      (dolist (label (ensure-list (stmt-labels stmt)))
        (aver (not (gethash label label->stmt)))
        (setf (gethash label label->stmt) stmt)))
    (perform-jump-to-jump-elimination (section-start section) label->stmt)))

;; Remove macros that users should not invoke
(push '("SB-ASSEM" define-instruction define-instruction-macro)
      *!removable-symbols*)
