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

(in-package "SB!C")

;;; Map of code-component -> list of PC offsets at which allocations occur.
;;; This table is needed in order to enable allocation profiling.
(define-load-time-global *allocation-point-fixups*
  (make-hash-table :test 'eq :weakness :key :synchronized t))

#!-x86-64
(defun convert-alloc-point-fixups (dummy1 dummy2)
  (declare (ignore dummy1 dummy2)))

(flet ((fixup (code-obj offset sym kind flavor layout-finder preserved-lists)
         ;; PRESERVED-LISTS is a vector of lists of locations (by kind)
         ;; at which fixup must be re-applied after code movement.
         ;; CODE-OBJ must already be pinned in order to legally call this.
         ;; One call site that reaches here is below at MAKE-CORE-COMPONENT
         ;; and the other is LOAD-CODE, both of which pin the code.
         (when (sb!vm:fixup-code-object
                 code-obj offset
                 (ecase flavor
                   ((:assembly-routine :assembly-routine*)
                    (or (get-asm-routine sym (eq flavor :assembly-routine*))
                        (error "undefined assembler routine: ~S" sym)))
                   (:foreign (foreign-symbol-address sym))
                   (:foreign-dataref (foreign-symbol-address sym t))
                   (:code-object (get-lisp-obj-address code-obj))
                   (:symbol-tls-index (ensure-symbol-tls-index sym))
                   (:layout (get-lisp-obj-address (funcall layout-finder sym)))
                   (:immobile-object (get-lisp-obj-address sym))
                   #!+immobile-code (:named-call (sb!vm::fdefn-entry-address sym))
                   #!+immobile-code (:static-call (sb!vm::function-raw-address sym)))
                 kind flavor)
           (ecase kind
             (:relative (push offset (elt preserved-lists 0)))
             (:absolute (push offset (elt preserved-lists 1)))))
         ;; These won't exist except for x86-64, but it doesn't matter.
         (when (member sym '(sb!vm::enable-alloc-counter
                             sb!vm::enable-sized-alloc-counter))
           (push offset (elt preserved-lists 2))))

       (finish-fixups (code-obj preserved-lists)
         (declare (ignorable code-obj preserved-lists))
         #!+(or immobile-space x86)
         (let ((rel-fixups (elt preserved-lists 0))
               (abs-fixups (elt preserved-lists 1)))
           (when (or abs-fixups rel-fixups)
             (setf (sb!vm::%code-fixups code-obj)
                   (sb!c::pack-code-fixup-locs abs-fixups rel-fixups))))
         (awhen (elt preserved-lists 2)
           (setf (gethash code-obj *allocation-point-fixups*)
                 (convert-alloc-point-fixups code-obj it)))
         ;; Assign all SIMPLE-FUN-SELF slots
         (dotimes (i (code-n-entries code-obj))
           (let ((fun (%code-entry-point code-obj i)))
             (setf (%simple-fun-self fun)
                   ;; x86 backends store the address of the entrypoint in 'self'
                   #!+(or x86 x86-64)
                   (%make-lisp-obj
                    (truly-the word (+ (get-lisp-obj-address fun)
                                       (ash sb!vm:simple-fun-code-offset sb!vm:word-shift)
                                       (- sb!vm:fun-pointer-lowtag))))
                   ;; non-x86 backends store the function itself (what else?) in 'self'
                   #!-(or x86 x86-64) fun)
             ;; And maybe store the layout in the high half of the header
             #!+(and compact-instance-header x86-64)
             (setf (sap-ref-32 (int-sap (get-lisp-obj-address fun))
                               (- 4 sb!vm:fun-pointer-lowtag))
                   (truly-the (unsigned-byte 32)
                     (get-lisp-obj-address #.(find-layout 'function))))))
         ;; And finally, make the memory range executable
         #!-(or x86 x86-64)
         (sb!vm:sanctify-for-execution code-obj)))

  (defun apply-fasl-fixups (fop-stack code-obj &aux (top (svref fop-stack 0)))
    (dx-let ((preserved (vector nil nil nil)))
      (macrolet ((pop-fop-stack () `(prog1 (svref fop-stack top) (decf top))))
        (dotimes (i (pop-fop-stack) (setf (svref fop-stack 0) top))
          (multiple-value-bind (offset kind flavor)
              (sb!fasl::!unpack-fixup-info (pop-fop-stack))
            (fixup code-obj offset (pop-fop-stack) kind flavor
                   #'find-layout preserved))))
      (finish-fixups code-obj preserved)))

  (defun apply-core-fixups (fixup-notes code-obj)
    (declare (list fixup-notes))
    (dx-let ((preserved (vector nil nil nil)))
      (dolist (note fixup-notes)
        (let ((fixup (fixup-note-fixup note))
              (offset (fixup-note-position note)))
          (fixup code-obj offset
                 (fixup-name fixup)
                 (fixup-note-kind note)
                 (fixup-flavor fixup)
               ;; Compiling to memory creates layout fixups with the name being
               ;; an instance of LAYOUT, not a symbol. Those probably should be
               ;; :IMMOBILE-OBJECT fixups. But since they're not, inform the
               ;; fixupper not to call find-layout on them.
                 #'identity
                 preserved)))
      (finish-fixups code-obj preserved))))

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
    (if found
        (setf (code-header-ref code-obj i) found)
        (push (cons code-obj i)
              (gethash info (core-object-patch-table object)))))
  (values))

;;; Dump a component to core. We pass in the assembler fixups, code
;;; vector and node info.
(defun make-core-component (component segment length fixup-notes object)
  (declare (type component component)
           (type segment segment)
           (type index length)
           (list fixup-notes)
           (type core-object object))
  (let ((debug-info (debug-info-for-component component)))
    (let* ((2comp (component-info component))
           (constants (ir2-component-constants 2comp))
           (nboxed (align-up (length constants) sb!c::code-boxed-words-align))
           (code-obj (allocate-code-object
                      (component-mem-space component) nboxed length)))

      ;; The following operations need the code pinned:
      ;; 1. copying into code-instructions (a SAP)
      ;; 2. apply-core-fixups and sanctify-for-execution
      (with-pinned-objects (code-obj)
        (let ((bytes (the (simple-array assembly-unit 1)
                          (segment-contents-as-vector segment))))
          ;; By design, until the last 2 unboxed bytes of CODE-OBJ contain a
          ;; nonzero value, GC will not see any simple-funs therein.
          (%byte-blt bytes 0 (code-instructions code-obj) 0 (length bytes)))
        (apply-core-fixups fixup-notes code-obj))

      ;; Don't need code pinned now
      (let* ((entries (ir2-component-entries 2comp))
             (fun-index (length entries)))
        (dolist (entry-info entries)
          (let ((fun (%code-entry-point code-obj (decf fun-index))))
            (setf (%simple-fun-name fun) (entry-info-name entry-info))
            (setf (%simple-fun-arglist fun) (entry-info-arguments entry-info))
            (setf (%simple-fun-type fun) (entry-info-type entry-info))
            ;; We can't make a decision to record the source here
            ;; because the policy is not available in a simple way.
            ;; (It would demand groveling through the component to find
            ;; the lambda-bind corresponding to this entry)
            (setf (%simple-fun-info fun) (entry-info-info entry-info))
            (note-fun entry-info fun object))))

      (push debug-info (core-object-debug-info object))
      (setf (%code-debug-info code-obj) debug-info)

      (do ((index sb!vm:code-constants-offset (1+ index)))
          ((>= index (length constants)))
        (let ((const (aref constants index)))
            (etypecase const
              (null)
              (constant
               (setf (code-header-ref code-obj index)
                     (constant-value const)))
              (list
               (ecase (car const)
                 (:entry
                  (reference-core-fun code-obj index (cdr const) object))
                 (:fdefinition
                  (setf (code-header-ref code-obj index)
                        (find-or-create-fdefn (cdr const))))
                 (:known-fun
                  (setf (code-header-ref code-obj index)
                        (%coerce-name-to-fun (cdr const)))))))))))
  (values))

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
