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

(declaim (ftype (sfunction (boolean fixnum fixnum) code-component) allocate-code-object))
(defun allocate-code-object (immobile-p boxed unboxed)
  (declare (ignorable immobile-p))
  #!+gencgc
  (without-gcing
      (cond #!+immobile-code
            (immobile-p (sb!vm::allocate-immobile-code boxed unboxed))
            (t (%make-lisp-obj
                (alien-funcall (extern-alien "alloc_code_object"
                                             (function unsigned unsigned unsigned))
                               boxed unboxed)))))
  #!-gencgc
  (%primitive allocate-code-object boxed unboxed))

;;; Assign all simple-fun offsets in CODE, then set N-ENTRIES so that GC sees
;;; a nonzero number of functions only after their addresses are assigned.
;;; Otherwise GC would see their offsets as all 0 and bad things would happen.
;;; Each element of OFFSETS is a byte number beyond (CODE-INSTRUCTIONS CODE).
(defun set-code-entrypoints (code offsets)
  (let* ((nfuns (the (unsigned-byte 14) (length offsets)))
         (fun-index nfuns))
    (dolist (offset offsets)
      (declare (type index offset))
      (unless (zerop (logand offset sb!vm:lowtag-mask))
        (bug "unaligned function object, offset = #X~X" offset))
      (decf fun-index)
      (let ((header-data (get-header-data code)))
        (if (> fun-index 0)
            (let* ((n-header-words (logand header-data sb!vm:short-header-max-words))
                   (index (+ (- sb!vm:other-pointer-lowtag)
                             (ash n-header-words sb!vm:word-shift)
                             (ash (1- fun-index) 2))))
              (aver (eql (sap-ref-32 (int-sap (get-lisp-obj-address code)) index) 0))
              (setf (sap-ref-32 (int-sap (get-lisp-obj-address code)) index) offset))
            ;; Special case for the first simple-fun:
            ;; The value range of 'offset' and 'nfuns' is the same
            ;; regardless of word size.
            ;; It's as if it's a positive 32-bit fixnum (29 significant bits).
            ;; 16 bits is enough for the offset because it only needs to
            ;; skip over the unboxed constants.
            #!-64-bit
            (let ((newval (logior (ash (the (mod #x8000) offset) 14) nfuns)))
              (aver (eql (sb!vm::%code-n-entries code) 0))
              (setf (sb!vm::%code-n-entries code) newval))
            #!+64-bit
            (let ((newval (logior (ash (the (mod #x8000) offset) 16) nfuns)))
              (aver (eql (ldb (byte 32 24) header-data) 0))
              (set-header-data code (dpb newval (byte 32 24) header-data)))))
      ;; COMPUTE-FUN is ok even if code-obj is not pinned (which it is)
      (let ((fun (truly-the function (%primitive sb!c:compute-fun code offset))))
        #!+(and compact-instance-header x86-64)
        (setf (sap-ref-32 (int-sap (get-lisp-obj-address fun))
                          (- 4 sb!vm:fun-pointer-lowtag))
              (truly-the (unsigned-byte 32)
                         (get-lisp-obj-address #.(find-layout 'function))))
        (setf (%simple-fun-self fun)
              ;; x86 backends store the address of the entrypoint in 'self'
              #!+(or x86 x86-64)
              (%make-lisp-obj
               (truly-the word (+ (get-lisp-obj-address fun)
                                  (ash sb!vm:simple-fun-code-offset sb!vm:word-shift)
                                  (- sb!vm:fun-pointer-lowtag))))
              ;; non-x86 backends store the function itself (what else?) in 'self'
              #!-(or x86 x86-64)
              (setf (%simple-fun-self fun) fun))))))

(flet ((fixup (code-obj offset sym kind flavor layout-finder)
         ;; CODE-OBJ must already be pinned in order to legally call this.
         ;; One call site that reaches here is below at MAKE-CORE-COMPONENT
         ;; and the other is LOAD-CODE, both of which pin the code.
         (sb!vm:fixup-code-object
          code-obj offset
          (ecase flavor
            (:assembly-routine (or (get-asm-routine sym)
                                   (error "undefined assembler routine: ~S" sym)))
            (:foreign (foreign-symbol-address sym))
            (:foreign-dataref (foreign-symbol-address sym t))
            (:code-object (get-lisp-obj-address code-obj))
            (:symbol-tls-index (ensure-symbol-tls-index sym))
            (:layout (get-lisp-obj-address (funcall layout-finder sym)))
            (:immobile-object (get-lisp-obj-address sym))
            #!+immobile-code (:named-call (sb!vm::fdefn-entry-address sym))
            #!+immobile-code (:static-call (sb!vm::function-raw-address sym)))
          kind flavor))
       (finish-fixups (code-obj preserved-fixups)
         (declare (ignorable code-obj preserved-fixups))
         #!+(or immobile-space x86)
         (when preserved-fixups
           (setf (sb!vm::%code-fixups code-obj)
                 (sb!c::pack-code-fixup-locs preserved-fixups)))
         #!-(or x86 x86-64)
         (sb!vm:sanctify-for-execution code-obj)))

  (defun apply-fasl-fixups (fop-stack code-obj &aux (top (svref fop-stack 0))
                                                    (preserved-fixups nil))
    (macrolet ((pop-fop-stack () `(prog1 (svref fop-stack top) (decf top))))
      (dotimes (i (pop-fop-stack) (setf (svref fop-stack 0) top))
        (multiple-value-bind (offset kind flavor)
            (sb!fasl::!unpack-fixup-info (pop-fop-stack))
          (when (fixup code-obj offset (pop-fop-stack) kind flavor #'find-layout)
            (push offset preserved-fixups)))))
    (finish-fixups code-obj preserved-fixups))

  (defun apply-core-fixups (fixup-notes code-obj &aux (preserved-fixups nil))
    (declare (list fixup-notes))
    (dolist (note fixup-notes)
      (let ((fixup (fixup-note-fixup note))
            (offset (fixup-note-position note)))
        (when (fixup code-obj offset
                     (fixup-name fixup)
                     (fixup-note-kind note)
                     (fixup-flavor fixup)
               ;; Compiling to memory creates layout fixups with the name being
               ;; an instance of LAYOUT, not a symbol. Those probably should be
               ;; :IMMOBILE-OBJECT fixups. But since they're not, inform the
               ;; fixupper not to call find-layout on them.
                     #'identity)
          (push offset preserved-fixups))))
      (finish-fixups code-obj preserved-fixups)))

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
    ;; See also the remark in LOAD-CODE about the order of installing
    ;; simple-funs and setting the 'nfuns' value.
    (let* ((2comp (component-info component))
           (constants (ir2-component-constants 2comp))
           (box-num (- (length constants) sb!vm:code-constants-offset))
           (code-obj (allocate-code-object
                      (or #!+immobile-code (eq *compile-to-memory-space* :immobile))
                      box-num length)))
      (declare (type index box-num length))

      ;; The following operations need the code pinned:
      ;; 1. copying into code-instructions (a SAP)
      ;; 2. apply-core-fixups and sanctify-for-execution
      ;; 3. set-code-entrypointa
      (with-pinned-objects (code-obj)
         (copy-byte-vector-to-system-area
          (the (simple-array assembly-unit 1) (segment-contents-as-vector segment))
          (code-instructions code-obj))

        (apply-core-fixups fixup-notes code-obj)

        (set-code-entrypoints
         code-obj (mapcar (lambda (entry-info)
                            (label-position (entry-info-offset entry-info)))
                          (ir2-component-entries 2comp))))

      ;; Don't need code pinned now
      (let* ((entries (ir2-component-entries 2comp))
             (fun-index (length entries)))
        (dolist (entry-info entries)
          (let ((fun (%code-entry-point code-obj (decf fun-index))))
            (setf (%simple-fun-name fun) (entry-info-name entry-info))
            (setf (%simple-fun-arglist fun) (entry-info-arguments entry-info))
            (setf (%simple-fun-type fun) (entry-info-type entry-info))
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
  (declare (type core-object object)
           (type (or null function) function))
  (aver (zerop (hash-table-count (core-object-patch-table object))))
  (let ((source (debug-source-for-info info :function function)))
    (dolist (info (core-object-debug-info object))
      (setf (debug-info-source info) source)))
  (setf (core-object-debug-info object) nil)
  (values))
