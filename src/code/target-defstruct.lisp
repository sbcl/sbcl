;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

(/show0 "target-defstruct.lisp 12")

;;;; structure frobbing primitives

;;; This allocator is used by the expansion of MAKE-LOAD-FORM-SAVING-SLOTS
;;; when given a STRUCTURE-OBJECT.
(defun allocate-struct (type)
  (let* ((layout (classoid-layout (the structure-classoid (find-classoid type))))
         (structure (%make-instance (layout-length layout))))
    (setf (%instance-layout structure) layout)
    (dolist (dsd (dd-slots (layout-dd layout)) structure)
      (when (eq (dsd-raw-type dsd) 't)
        (setf (%instance-ref structure (dsd-index dsd)) (make-unbound-marker))))))

;;; Return the value from the INDEXth slot of INSTANCE. This is SETFable.
;;; This is used right away in warm compile by MAKE-LOAD-FORM-SAVING-SLOTS,
;;; so without it already defined, you can't define it, because you can't dump
;;; debug info structures. Were it not for that, this would go in 'stubs'.
(defun %instance-ref (instance index)
  (%instance-ref instance index))

(defun set-layout-inherits (layout inherits structurep this-id)
  #-metaspace (setf (layout-inherits layout) inherits)
  #+metaspace (setf (wrapper-inherits (layout-friend layout)) inherits)
  ;;; If structurep, and *only* if, store all the inherited layout IDs.
  ;;; It looks enticing to try to always store "something", but that goes wrong,
  ;;; because only structure-object layouts are growable, and only structure-object
  ;;; can store the self-ID in the proper index.
  ;;; If the depthoid is -1, the self-ID has to go in index 0.
  ;;; Standard-object layouts are not growable. The inherited layouts are known
  ;;; only at class finalization time, at which point we've already made the layout.
  ;;; Hence, the required indirection to the simple-vector of inherits.
  (with-pinned-objects (layout)
    ;; The layout-id vector is an array of int32_t starting at the ID-WORD0 slot.
    ;; We could use (SETF %RAW-INSTANCE-REF/WORD) for 32-bit architectures,
    ;; but on 64-bit we have to use SAP-REF, so may as well be consistent here
    ;; and use a SAP either way.
    (let ((sap (sap+ (int-sap (get-lisp-obj-address layout))
                     (- (ash (+ sb-vm:instance-slots-offset (get-dsd-index layout id-word0))
                             sb-vm:word-shift)
                        sb-vm:instance-pointer-lowtag))))
      (cond (structurep
             (loop for i from 0 by 4
                   for j from 2 below (length inherits) ; skip T and STRUCTURE-OBJECT
                   do (setf (signed-sap-ref-32 sap i) (layout-id (svref inherits j)))
                   finally (setf (signed-sap-ref-32 sap i) this-id)))
            ((not (eql this-id 0))
             (setf (signed-sap-ref-32 sap 0) this-id))))))

;;; Normally IR2 converted, definition needed for interpreted structure
;;; constructors only.
#+(or sb-eval sb-fasteval)
(defun %make-structure-instance (dd slot-specs &rest slot-values)
  (let ((instance (%make-instance (dd-length dd))) ; length = sans header word
        (value-index 0))
    (declare (index value-index))
    (setf (%instance-layout instance) (dd-layout-or-lose dd))
    (dolist (spec slot-specs instance)
      (destructuring-bind (kind raw-type . index) spec
        (if (eq kind :unbound)
            (setf (%instance-ref instance index)
                  (sb-sys:%primitive make-unbound-marker))
            (macrolet ((make-case ()
                           `(ecase raw-type
                              ((t)
                               (setf (%instance-ref instance index) value))
                              ,@(map 'list
                                 (lambda (rsd)
                                   `(,(raw-slot-data-raw-type rsd)
                                      (setf (,(raw-slot-data-accessor-name rsd)
                                              instance index)
                                            value)))
                                 *raw-slot-data*))))
                (let ((value (fast-&rest-nth value-index slot-values)))
                  (incf value-index)
                  (make-case))))))))


;;;; target-only parts of the DEFSTRUCT top level code

;;; A list of hooks designating functions of one argument, the
;;; classoid, to be called when a defstruct is evaluated.
(!define-load-time-global *defstruct-hooks* nil)

;;; the part of %DEFSTRUCT which makes sense only on the target SBCL
;;;
(defmacro set-layout-equalp-impl (layout newval)
  `(setf #-metaspace (%instance-ref ,layout
                                    (get-dsd-index layout equalp-impl))
         #+metaspace (%instance-ref (layout-friend ,layout)
                                    (get-dsd-index wrapper equalp-impl))
        ,newval))

(defun assign-equalp-impl (type-name function)
  (set-layout-equalp-impl (find-layout type-name) function))

(defun %target-defstruct (dd equalp)
  (declare (type defstruct-description dd))

  (when (dd-doc dd)
    (setf (documentation (dd-name dd) 'structure)
          (dd-doc dd)))

  (let ((classoid (find-classoid (dd-name dd))))
    (let ((layout (classoid-layout classoid)))
      (set-layout-equalp-impl
          layout
          (cond ((compiled-function-p equalp) equalp)
                ((eql (layout-bitmap layout) +layout-all-tagged+)
                 #'sb-impl::instance-equalp)
                (t
                  ;; Make a vector of EQUALP slots comparators, indexed by
                  ;; (- word-index INSTANCE-DATA-START).
                  ;; The initial element of NIL means "do not compare".
                  ;; Ignored words (comparator = NIL) fall into two categories:
                  ;; - pseudo-ignored, which get compared by their
                  ;;   predecessor word, as for complex-double-float,
                  ;; - internal padding words which are truly ignored.
                  ;; Other words are compared as tagged if the comparator is 0,
                  ;; or as untagged if the comparator is a type-specific function.
                  (let ((comparators
                     ;; If data-start is 1, subtract 1 because we don't need
                     ;; a comparator for the LAYOUT slot.
                     (make-array (- (dd-length dd) sb-vm:instance-data-start)
                                 :initial-element nil)))
                    (dolist (slot (dd-slots dd) comparators)
                      (setf (aref comparators
                                  (- (dsd-index slot) sb-vm:instance-data-start))
                            (let ((rsd (dsd-raw-slot-data slot)))
                              (if (not rsd)
                                  0 ; means recurse using EQUALP
                                  (raw-slot-data-comparator rsd)))))
                    (lambda (a b)
                      (sb-impl::instance-equalp* comparators a b)))))))

    (dolist (fun *defstruct-hooks*)
      (funcall fun classoid)))

  (dd-name dd))

;;; Copy any old kind of structure.
(defun copy-structure (structure)
  "Return a copy of STRUCTURE with the same (EQL) slot values."
  (declare (type structure-object structure))
  (let ((layout (%instance-layout structure)))
    (when (layout-invalid layout)
      (error "attempt to copy an obsolete structure:~%  ~S" structure))
    ;; Previously this had to used LAYOUT-LENGTH in the allocation,
    ;; to avoid copying random bits from the stack to the heap if you had a
    ;; padding word in a stack-allocated instance. This is no longer an issue.
    ;; %INSTANCE-LENGTH returns the number of words that are logically in the
    ;; instance, with no padding. Using %INSTANCE-LENGTH allows potentially
    ;; interesting nonstandard things like variable-length structures.
    (let* ((len (%instance-length structure))
           (res (%make-instance len)))
      (declare (type index len))
      (let ((bitmap (layout-bitmap layout)))
        ;; Don't assume that %INSTANCE-REF can access the layout.
        (setf (%instance-layout res) (%instance-layout structure))
        ;; On backends which don't segregate descriptor vs. non-descriptor
        ;; registers, we could speed up this code in an obvious way.
        (macrolet ((copy-loop (tagged-p &optional step)
                     `(do ((i sb-vm:instance-data-start (1+ i)))
                          ((>= i len))
                        (declare (index i))
                        (if ,tagged-p
                            (setf (%instance-ref res i)
                                  (%instance-ref structure i))
                            (setf (%raw-instance-ref/word res i)
                                  (%raw-instance-ref/word structure i)))
                        ,step)))
          (cond ((eql bitmap +layout-all-tagged+) (copy-loop t))
                ;; The fixnum case uses fixnum operations for ODDP and ASH.
                ((fixnump bitmap) ; shift and mask is faster than logbitp
                 (copy-loop (oddp (truly-the fixnum bitmap))
                            (setq bitmap (ash bitmap -1))))
                (t ; bignum - use LOGBITP to avoid consing more bignums
                 (copy-loop (logbitp i bitmap))))))
      res)))
;;; Like above, but copy all slots (including the LAYOUT) as though boxed.
;;; If the structure might contain raw slots and the GC is precise,
;;; this won't ever be called.
(defun %copy-instance (to from)
  (declare (structure-object to from) (optimize (safety 0)))
  (setf (%instance-layout to) (%instance-layout from))
  (dotimes (i (%instance-length to) to)
    (setf (%instance-ref to i) (%instance-ref from i))))
;;; Like %COPY-INSTANCE, but layout was already assigned.
;;; Similarly, will not be called if raw slots and precise GC.
(defun %copy-instance-slots (to from)
  (declare (structure-object to from) (optimize (safety 0)))
  (loop for i from sb-vm:instance-data-start below (%instance-length to)
        do (setf (%instance-ref to i) (%instance-ref from i)))
  to)

;;; default PRINT-OBJECT method

;;; Printing formerly called the auto-generated accessor functions,
;;; but reading the slots more primitively confers several advantages:
;;; - it works even if the user clobbered an accessor
;;; - it works if the slot fails a type-check and the reader was SAFE-P,
;;    i.e. was required to perform a check. This is a feature, not a bug.
(macrolet ((access (dsd)
             `(let ((i (dsd-index ,dsd)))
                (acond ((dsd-raw-slot-data ,dsd)
                        (funcall (raw-slot-data-accessor-fun it) structure i))
                       (t
                        (%instance-ref structure i))))))

(defun %default-structure-pretty-print (structure stream name dd)
  (pprint-logical-block (stream nil :prefix "#S(" :suffix ")")
    (write name :stream stream) ; escaped or not, according to printer controls
    (let ((remaining-slots (dd-slots dd)))
      (when remaining-slots
        (write-char #\space stream)
                 ;; CMU CL had (PPRINT-INDENT :BLOCK 2 STREAM) here,
                 ;; but I can't see why. -- WHN 20000205
        (pprint-newline :linear stream)
        (loop (pprint-pop)
              (let ((slot (pop remaining-slots)))
                (output-symbol (dsd-name slot) *keyword-package* stream)
                (write-char #\space stream)
                (pprint-newline :miser stream)
                (output-object (access slot) stream)
                (when (null remaining-slots)
                  (return))
                (write-char #\space stream)
                (pprint-newline :linear stream)))))))

(defun %default-structure-ugly-print (structure stream name dd)
  (descend-into (stream)
    (write-string "#S(" stream)
    (write name :stream stream)
    (do ((index 0 (1+ index))
         (limit (or (and (not *print-readably*) *print-length*)
                    most-positive-fixnum))
         (remaining-slots (dd-slots dd) (cdr remaining-slots)))
        ((or (null remaining-slots) (>= index limit))
         (write-string (if remaining-slots " ...)" ")") stream))
      (declare (type index index))
      (write-char #\space stream)
      (let ((slot (first remaining-slots)))
        (output-symbol (dsd-name slot) *keyword-package* stream)
        (write-char #\space stream)
        (output-object (access slot) stream)))))
) ; end MACROLET

(defun default-structure-print (structure stream depth)
  (declare (ignore depth))
  (if (funcallable-instance-p structure)
      (print-unreadable-object (structure stream :identity t :type t))
      (let* ((layout (%instance-layout structure))
             (dd (layout-info layout))
             (name (layout-classoid-name layout)))
        (cond ((not dd)
               ;; FIXME? this branch may be unnecessary as a consequence
               ;; of change f02bee325920166b69070e4735a8a3f295f8edfd which
               ;; stopped the badness is a stronger way. It should be the case
               ;; that absence of a DD can't happen unless the classoid is absent.
               ;; KLUDGE: during warm build, we may see a CONDITION or STANDARD-OBJECT
               ;; here, and we need a way to get a handle on it (for SB-VM:HEXDUMP at least).
               ;; I'm not sure why a  condition-report function isn't called in the former
               ;; case. Using the default structure printer won't solve anything, because
               ;; the layout of a condition does not describe any slot of interest.
               (pprint-logical-block (stream nil :prefix "#<" :suffix ">")
                 (prin1 name stream)
                 (write-char #\space stream)
                 (write-string "(no LAYOUT-INFO) " stream)
                 (write (get-lisp-obj-address structure) :base 16 :radix t :stream stream)))
              ((not (dd-slots dd))
               ;; the structure type doesn't count as a component for *PRINT-LEVEL*
               ;; processing. We can likewise elide the logical block processing,
               ;; since all we have to print is the type name. -- CSR, 2004-10-05
               (write-string "#S(" stream)
               (prin1 name stream)
               (write-char #\) stream))
              (t
               (funcall (if *print-pretty*
                            #'%default-structure-pretty-print
                            #'%default-structure-ugly-print)
                        structure stream name dd))))))

(defmethod print-object ((x structure-object) stream)
  (default-structure-print x stream *current-level-in-print*))

;;; Used internally, but it would be nice to provide something
;;; like this for users as well.
(defmacro define-structure-slot-addressor (name &key structure slot (byte-offset 0))
  (let* ((dd (find-defstruct-description structure t))
         (slotd (or (and dd (find slot (dd-slots dd) :key #'dsd-name))
                    (error "Slot ~S not found in ~S." slot structure)))
         (index (dsd-index slotd)))
    `(progn
       (declaim (inline ,name))
       (defun ,name (instance)
         (declare (type ,structure instance) (optimize speed))
         (truly-the
          word
          (+ (get-lisp-obj-address instance)
             ,byte-offset
             ,(+ (- sb-vm:instance-pointer-lowtag)
                 (* (+ sb-vm:instance-slots-offset index)
                    sb-vm:n-word-bytes))))))))

(/show0 "target-defstruct.lisp end of file")
