;;;; target-only code that knows how to load compiled code directly
;;;; into core
;;;;
;;;; FIXME: The filename here is confusing because "core" here means
;;;; "main memory", while elsewhere in the system it connotes a
;;;; ".core" file dumping the contents of main memory.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;; Map of code-component -> list of PC offsets at which allocations occur.
;;; This table is needed in order to enable allocation profiling.
(define-load-time-global *allocation-patch-points*
  (make-hash-table :test 'eq :weakness :key :synchronized t))

#-x86-64
(progn
(defun sb-vm::statically-link-code-obj (code fixups)
  (declare (ignore code fixups))))

#+immobile-code
(progn
  ;; Use FDEFINITION because it strips encapsulations - whether that's
  ;; the right behavior for it or not is a separate concern.
  ;; If somebody tries (TRACE LENGTH) for example, it should not cause
  ;; compilations to fail on account of LENGTH becoming a closure.
  (defun sb-vm::function-raw-address (name &aux (fun (fdefinition name)))
    (cond ((not (immobile-space-obj-p fun))
           (error "Can't statically link to ~S: code is movable" name))
          ((neq (%fun-pointer-widetag fun) sb-vm:simple-fun-widetag)
           (error "Can't statically link to ~S: non-simple function" name))
          (t
           (let ((addr (get-lisp-obj-address fun)))
             (sap-ref-word (int-sap addr)
                           (- (ash sb-vm:simple-fun-self-slot sb-vm:word-shift)
                              sb-vm:fun-pointer-lowtag))))))

  ;; Return the address to which to jump when calling FDEFN,
  ;; which is either an fdefn or the name of an fdefn.
  ;; FIXME: Shouldn't this go in x86-64-vm ?
  (defun sb-vm::fdefn-entry-address (fdefn)
    (let ((fdefn (if (fdefn-p fdefn) fdefn (find-or-create-fdefn fdefn))))
      (+ (get-lisp-obj-address fdefn)
         (- 2 sb-vm:other-pointer-lowtag)))))

;;; Point FUN's 'self' slot to FUN.
;;; FUN must be pinned when calling this.
(declaim (inline assign-simple-fun-self))
(defun assign-simple-fun-self (fun)
  (let ((self ;; x86 backends store the address of the entrypoint in 'self'
          #+(or x86 x86-64 arm64)
          (%make-lisp-obj
           (truly-the word (+ (get-lisp-obj-address fun)
                              (ash sb-vm:simple-fun-insts-offset sb-vm:word-shift)
                              (- sb-vm:fun-pointer-lowtag))))
          ;; non-x86 backends store the function itself (what else?) in 'self'
          #-(or x86 x86-64 arm64) fun))
    #-darwin-jit
    (setf (sb-vm::%simple-fun-self fun) self)
    #+darwin-jit
    (sb-vm::jit-patch (+ (get-lisp-obj-address fun)
                         (- sb-vm:fun-pointer-lowtag)
                         (* sb-vm:simple-fun-self-slot sb-vm:n-word-bytes))
               (get-lisp-obj-address self))))

(flet ((fixup (code-obj offset name kind flavor preserved-lists statically-link-p)
         (declare (ignorable statically-link-p))
         ;; NAME depends on the kind and flavor of fixup.
         ;; PRESERVED-LISTS is a vector of lists of locations (by kind)
         ;; at which fixup must be re-applied after code movement.
         ;; CODE-OBJ must already be pinned in order to legally call this.
         ;; One call site that reaches here is below at MAKE-CORE-COMPONENT
         ;; and the other is LOAD-CODE, both of which pin the code.
         (ecase
             (sb-vm:fixup-code-object
                 code-obj offset
                 (ecase flavor
                   ((:assembly-routine :assembly-routine* :asm-routine-nil-offset)
                    (- (or (get-asm-routine name (eq flavor :assembly-routine*))
                           (error "undefined assembler routine: ~S" name))
                       (if (eq flavor :asm-routine-nil-offset) sb-vm:nil-value 0)))
                   (:foreign (foreign-symbol-address name))
                   (:foreign-dataref (foreign-symbol-address name t))
                   (:code-object (get-lisp-obj-address code-obj))
                   #+sb-thread (:symbol-tls-index (ensure-symbol-tls-index name))
                   (:layout (get-lisp-obj-address
                             (wrapper-friend (if (symbolp name) (find-layout name) name))))
                   (:layout-id (layout-id name))
                   #+gencgc (:gc-barrier (extern-alien "gc_card_table_nbits" int))
                   (:immobile-symbol (get-lisp-obj-address name))
                   ;; It is legal to take the address of symbol-value only if the
                   ;; value is known to be an immobile object
                   ;; (whose address we don't want to wire in).
                   (:symbol-value (get-lisp-obj-address (symbol-global-value name)))
                   #+immobile-code
                   (:named-call
                    (prog1 (sb-vm::fdefn-entry-address name) ; creates if didn't exist
                      (when statically-link-p
                        (push (cons offset (find-fdefn name)) (elt preserved-lists 0)))))
                   #+immobile-code (:static-call (sb-vm::function-raw-address name)))
                 kind flavor)
           ((nil)) ; don't need to save it in the code-fixups, otherwise do
           (:relative (push offset (elt preserved-lists 1)))
           (:absolute (push offset (elt preserved-lists 2)))
           (:immediate (push offset (elt preserved-lists 3)))
           (:absolute64 (push offset (elt preserved-lists 4)))))

       (finish-fixups (code-obj preserved-lists)
         (declare (ignorable code-obj preserved-lists))
         ;; PRESERVED-LISTS are somewhat backend-dependent, but essentially
         ;; you get to store three lists that might as well have been named
         ;; Larry, Moe, and Curly.
         (let ((rel-fixups (elt preserved-lists 1))
               (abs-fixups (elt preserved-lists 2))
               (gc-barrier-fixups (elt preserved-lists 3))
               (abs64-fixups (elt preserved-lists 4)))
           ;; the fixup list packer only preserves at most 3 lists.
           ;; And it's not clear that this is the best way to represent them.
           (aver (not abs64-fixups))
           (when (or abs-fixups rel-fixups gc-barrier-fixups)
             (setf (sb-vm::%code-fixups code-obj)
                   (sb-c:pack-code-fixup-locs abs-fixups rel-fixups gc-barrier-fixups))))

         ;; Assign all SIMPLE-FUN-SELF slots
         (dotimes (i (code-n-entries code-obj))
           (let ((fun (%code-entry-point code-obj i)))
             (assign-simple-fun-self fun)
             ;; And maybe store the layout in the high half of the header
             #+(and compact-instance-header x86-64)
             (setf (sap-ref-32 (int-sap (get-lisp-obj-address fun))
                               (- 4 sb-vm:fun-pointer-lowtag))
                   (truly-the (unsigned-byte 32)
                     (get-lisp-obj-address (wrapper-friend #.(find-layout 'function)))))))
         ;; And finally, make the memory range executable
         #-(or x86 x86-64) (sb-vm:sanctify-for-execution code-obj)
         ;; Return fixups amenable to static linking
         (aref preserved-lists 0)))

  (defun apply-fasl-fixups (fop-stack code-obj n-fixups &aux (top (svref fop-stack 0)))
    (dx-let ((preserved (make-array 5 :initial-element nil)))
      (macrolet ((pop-fop-stack () `(prog1 (svref fop-stack top) (decf top))))
        (binding* ((alloc-points (pop-fop-stack) :exit-if-null))
          (setf (gethash code-obj *allocation-patch-points*) alloc-points))
        (dotimes (i n-fixups (setf (svref fop-stack 0) top))
          (multiple-value-bind (offset kind flavor)
              (sb-fasl::!unpack-fixup-info (pop-fop-stack))
            (fixup code-obj offset (pop-fop-stack) kind flavor preserved nil))))
      (finish-fixups code-obj preserved)))

  (defun apply-core-fixups (fixup-notes code-obj)
    (declare (list fixup-notes))
    (dx-let ((preserved (make-array 5 :initial-element nil)))
      (dolist (note fixup-notes)
        (let ((fixup (fixup-note-fixup note))
              (offset (fixup-note-position note)))
          (fixup code-obj offset
                 (fixup-name fixup)
                 (fixup-note-kind note)
                 (fixup-flavor fixup)
                 preserved t)))
      (finish-fixups code-obj preserved))))

;;; Return a behaviorally identical copy of CODE.
(defun copy-code-object (code)
  ;; Must have one simple-fun
  (aver (= (code-n-entries code) 1))
  ;; Disallow relative instruction operands.
  ;; (This restriction could be removed by actually performing fixups)
  ;; x86-64 absolute fixups are OK since they will only point to static objects.
  #+x86-64
  (aver (not (nth-value
              1 (sb-c:unpack-code-fixup-locs (sb-vm::%code-fixups code)))))
  (let* ((nbytes (code-object-size code))
         (boxed (code-header-words code)) ; word count
         (unboxed (- nbytes (ash boxed sb-vm:word-shift))) ; byte count
         (copy (allocate-code-object
                :dynamic (code-n-named-calls code) boxed unboxed)))
    (with-pinned-objects (code copy)
      #-darwin-jit
      (%byte-blt (code-instructions code) 0 (code-instructions copy) 0 unboxed)
      #+darwin-jit
      (sb-vm::jit-memcpy (code-instructions copy) (code-instructions code) unboxed)
      ;; copy boxed constants so that the fixup step (if needed) sees the 'fixups'
      ;; slot from the new object.
      (loop for i from 2 below boxed
            do (setf (code-header-ref copy i) (code-header-ref code i)))
      ;; x86 needs to fixup instructions that reference code constants,
      ;; and the jmp to TAIL-CALL-VARIABLE
      #+x86 (alien-funcall (extern-alien "gencgc_apply_code_fixups" (function void unsigned unsigned))
                           (- (get-lisp-obj-address code) sb-vm:other-pointer-lowtag)
                           (- (get-lisp-obj-address copy) sb-vm:other-pointer-lowtag))
      (assign-simple-fun-self (%code-entry-point copy 0)))
    copy))

;;; Note the existence of FUNCTION.
(defun note-fun (info function object)
  (declare (type function function)
           (type core-object object))
  (let ((patch-table (core-object-patch-table object)))
    (dolist (patch (gethash info patch-table))
      (setf (code-header-ref (car patch) (the index (cdr patch))) function))
    (remhash info patch-table))
  (setf (gethash info (core-object-entry-table object)) function)
  (values))

;;; Stick a reference to the function FUN in CODE-OBJECT at index I. If the
;;; function hasn't been compiled yet, make a note in the patch table.
(defun reference-core-fun (code-obj i fun object)
  (declare (type core-object object) (type functional fun)
           (type index i))
  (let* ((info (leaf-info fun))
         (found (gethash info (core-object-entry-table object))))
    ;; A  core component should not have cross-component references.
    ;; If it could, then the entries would have placed into one component.
    (aver found)
    (if found
        (setf (code-header-ref code-obj i) found)
        (push (cons code-obj i)
              (gethash info (core-object-patch-table object)))))
  (values))

;;; Dump a component to core. We pass in the assembler fixups, code
;;; vector and node info.

;;; Note that it is critical that the new code object not be movable after
;;; copying in unboxed bytes and prior to fixing up those bytes.
;;; Why: suppose the object has a jump table initially filled with addends of
;;; 0x1000, 0x1100, 0x1200 representing the label offsets. If GC moves the object,
;;; it adds the amount of movement to those labels. If it moves by, say +0x5000,
;;; then the addends get mangled into 0x6000, 0x6100, 0x6200.
;;; When we then add those addends to the virtual address of the code to
;;; perform fixup application, the resulting addresses are all wrong.
;
;;; While GC might be made to use a heuristic to decide whether the label offsets
;;; had been fixed up at all, it would be fragile nonetheless, because an offset
;;; could theoretically resemble a valid address on machines where code resides
;;; at low addresses in an object whose size is large. e.g. for an object which
;;; spans the range 0x10000..0x30000 in memory, and the word 0x11000 in the jump
;;; table, does that word represent an already-fixed-up label offset of 0x01000,
;;; or an un-fixed-up value which needs to become 0x21000 ? It's ambiguous.
;;; We could add 1 bit to the code object signifying that fixups had beeen applied
;;; for the first time. But that complication is not needed, as long as we keep the
;;; code pinned. That suffices because prior to copying in anything, all bytes
;;; are 0, so the jump table count is 0.
;;; Similar considerations pertain to x86[-64] fixups within the machine code.

(defun assign-code-serialno (code-obj)
  (let* ((serialno (ldb (byte (byte-size sb-vm::code-serialno-byte) 0)
                        (atomic-incf *code-serialno*)))
         (insts (code-instructions code-obj))
         (jumptable-word (sap-ref-word insts 0)))
    (aver (zerop (ash jumptable-word -14)))
    (setf (sb-vm::sap-ref-word-jit insts 0) ; insert serialno
          (logior (ash serialno (byte-position sb-vm::code-serialno-byte))
                  jumptable-word))))

(defun make-core-component (component segment length fixup-notes alloc-points object)
  (declare (type component component)
           (type segment segment)
           (type index length)
           (list fixup-notes)
           (type core-object object))
  (let* ((debug-info (debug-info-for-component component))
         (2comp (component-info component))
         (constants (ir2-component-constants 2comp))
         (nboxed (align-up (length constants) sb-c::code-boxed-words-align))
         (n-named-calls
          ;; Pre-scan for fdefinitions to ensure their existence.
          ;; Doing so guarantees that storing them into the boxed header now
          ;; can't create any old->young pointer, which is important since gencgc
          ;; does not deal with untagged pointers when looking for old->young.
          (do ((count 0)
               (index (+ sb-vm:code-constants-offset
                         (* (length (ir2-component-entries 2comp))
                            sb-vm:code-slots-per-simple-fun))
                      (1+ index)))
              ((>= index (length constants)) count)
            (let* ((const (aref constants index))
                   (kind (if (listp const) (car const) const)))
              (case kind
                ((member :named-call :fdefinition)
                 (setf (second const) (find-or-create-fdefn (second const)))
                 (when (eq kind :named-call) (incf count)))))))
         (code-obj (allocate-code-object (component-mem-space component)
                                         n-named-calls nboxed length))
         (named-call-fixups
      ;; The following operations need the code pinned:
      ;; 1. copying into code-instructions (a SAP)
      ;; 2. apply-core-fixups and sanctify-for-execution
      ;; A very specific store order is necessary to allow using uninitialized memory
      ;; pages for code. Storing of the debug-info slot does not need the code pinned,
      ;; but that store must occur between steps 1 and 2.
          (with-pinned-objects (code-obj)
           (let ((bytes (the (simple-array assembly-unit 1)
                             (segment-contents-as-vector segment))))
             ;; Note that this does not have to take care to ensure atomicity
             ;; of the store to the final word of unboxed data. Even if BYTE-BLT were
             ;; interrupted in between the store of any individual byte, this code
             ;; is GC-safe because we no longer need to know where simple-funs are embedded
             ;; within the object to trace pointers. We *do* need to know where the funs
             ;; are when transporting the object, but it's currently pinned.
             #-darwin-jit
             (%byte-blt bytes 0 (code-instructions code-obj) 0 (length bytes))
             #+darwin-jit
             (with-pinned-objects (bytes)
               (sb-vm::jit-memcpy (code-instructions code-obj) (vector-sap bytes) (length bytes)))

             ;; Serial# shares a word with the jump-table word count,
             ;; so we can't assign serial# until after all raw bytes are copied in.
             (assign-code-serialno code-obj))
           ;; Enforce that the final unboxed data word is published to memory
           ;; before the debug-info is set.
           (sb-thread:barrier (:write))
           ;; Until debug-info is assigned, it is illegal to create a simple-fun pointer
           ;; into this object, because the C code assumes that the fun table is in an
           ;; invalid/incomplete state (i.e. can't be read) until the code has debug-info.
           ;; That is, C code can't deal with an interior code pointer until the fun-table
           ;; is valid. This store must occur prior to calling %CODE-ENTRY-POINT, and
           ;; applying fixups calls %CODE-ENTRY-POINT, so we have to do this before that.
            (setf (%code-debug-info code-obj) debug-info)
            (apply-core-fixups fixup-notes code-obj))))

    (when alloc-points
      #+(and x86-64 sb-thread)
      (if (= (extern-alien "alloc_profiling" int) 0) ; record the object for later
          (setf (gethash code-obj *allocation-patch-points*) alloc-points)
          (funcall 'sb-aprof::patch-code code-obj alloc-points)))

      ;; Don't need code pinned now
      ;; (It will implicitly be pinned on the conservatively scavenged backends)
    (let* ((entries (ir2-component-entries 2comp))
           (fun-index (length entries)))
      (dolist (entry-info entries)
        (let ((fun (%code-entry-point code-obj (decf fun-index)))
              (w (+ sb-vm:code-constants-offset
                    (* sb-vm:code-slots-per-simple-fun fun-index))))
          (aver (functionp fun)) ; in case %CODE-ENTRY-POINT returns NIL
          (setf (code-header-ref code-obj (+ w sb-vm:simple-fun-name-slot))
                (entry-info-name entry-info)
                (code-header-ref code-obj (+ w sb-vm:simple-fun-arglist-slot))
                (entry-info-arguments entry-info)
                (code-header-ref code-obj (+ w sb-vm:simple-fun-source-slot))
                (entry-info-form/doc entry-info)
                (code-header-ref code-obj (+ w sb-vm:simple-fun-info-slot))
                (entry-info-type/xref entry-info))
          (note-fun entry-info fun object))))

    (push debug-info (core-object-debug-info object))

    (do ((index (+ sb-vm:code-constants-offset
                   (* (length (ir2-component-entries 2comp))
                      sb-vm:code-slots-per-simple-fun))
                (1+ index)))
        ((>= index (length constants)))
      (let* ((const (aref constants index))
             (kind (if (listp const) (car const) const)))
        (case kind
          (:entry
           (reference-core-fun code-obj index (cadr const) object))
          ((nil))
          (t
           (let ((referent
                  (etypecase kind
                   ((member :named-call :fdefinition) (cadr const))
                   ((eql :known-fun)
                    (%coerce-name-to-fun (cadr const)))
                   (constant
                    (constant-value const)))))
             (if (eq kind :named-call)
                 (set-code-fdefn code-obj index referent)
                 (setf (code-header-ref code-obj index) referent)))))))
    (when named-call-fixups
      (sb-vm::statically-link-code-obj code-obj named-call-fixups))
    (when sb-fasl::*show-new-code*
      (let ((*print-pretty* nil))
        (format t "~&New code(~Db,core): ~A~%" (code-object-size code-obj) code-obj)))
    code-obj))

(defun set-code-fdefn (code index fdefn)
  #+untagged-fdefns
  (with-pinned-objects (fdefn)
    (setf (code-header-ref code index)
          (%make-lisp-obj (logandc2 (get-lisp-obj-address fdefn)
                                    sb-vm:lowtag-mask))))
  #-untagged-fdefns
  (setf (code-header-ref code index) fdefn))

;;; Backpatch all the DEBUG-INFOs dumped so far with the specified
;;; SOURCE-INFO list. We also check that there are no outstanding
;;; forward references to functions.
(defun fix-core-source-info (info object &optional function)
  (declare (type core-object object))
  (declare (type (or null function) function))
  (aver (zerop (hash-table-count (core-object-patch-table object))))
  (let ((source (debug-source-for-info info :function function)))
    (dolist (info (core-object-debug-info object))
      (setf (debug-info-source info) source)))
  (setf (core-object-debug-info object) nil)
  (values))
