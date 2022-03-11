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

;;; For lack of any better to place to write up some detail surrounding
;;; layout creation for structure types, I'm putting here.
;;; When you issue a DEFSTRUCT at the REPL, there are *three* instances
;;; of LAYOUT makde for the new structure.
;;; 1) The first is one associated with a temporary instance of
;;; structure-classoid used in parsing the DEFSTRUCT form so that
;;; we don't signal an UNKNOWN-TYPE condition for something like:
;;;   (defstruct chain (next nil :type (or null chain)).
;;; The temporary classoid is garbage immediately after parsing
;;; and is never installed.
;;; 2) The next is the actual LAYOUT that ends up being registered.
;;; 3) The third is a layout created when setting the "compiler layout"
;;; which contains copies of the length/depthoid/inherits etc
;;; that we compare against the installed one to make sure they match.
;;; The third one also gets thrown away.
(define-load-time-global *layout-id-generator* (cons 0 nil))
(declaim (type (cons fixnum) *layout-id-generator*))
;;; NB: for #+metaspace this returns a WRAPPER, not a LAYOUT.
(defun make-layout (clos-hash classoid
                    &key (depthoid -1) (length 0) (flags 0)
                         (inherits #())
                         (info nil)
                         (bitmap (if info (dd-bitmap info) 0))
                         (invalid :uninitialized)
                    &aux (id (or (atomic-pop (cdr *layout-id-generator*))
                                 (atomic-incf (car *layout-id-generator*)))))
  (unless (typep id '(and layout-id (not (eql 0))))
    (error "Layout ID limit reached"))
  (let* ((fixed-words (type-dd-length sb-vm:layout))
         (extra-id-words ; count of additional words needed to store ancestors
          (if (logtest flags +structure-layout-flag+)
              (calculate-extra-id-words depthoid)
              0))
         (bitmap-words (ceiling (1+ (integer-length bitmap)) sb-vm:n-word-bits))
         (nwords (+ fixed-words extra-id-words bitmap-words))
         (layout
          (truly-the sb-vm:layout
                     #-(or metaspace immobile-space) (%make-instance nwords)
                     #+metaspace (sb-vm::allocate-metaspace-chunk (1+ nwords))
                     #+(and immobile-space (not metaspace))
                     (sb-vm::alloc-immobile-fixedobj
                      (1+ nwords)
                      (logior (ash nwords sb-vm:instance-length-shift)
                              sb-vm:instance-widetag))))
         (wrapper #-metaspace layout))
    (%set-instance-layout layout
          (wrapper-friend #.(find-layout #+metaspace 'sb-vm:layout
                                         #-metaspace 'wrapper)))
    #+metaspace
    (setf wrapper (%make-wrapper :clos-hash clos-hash :classoid classoid
                                 :%info info :invalid invalid :friend layout)
          (layout-friend layout) wrapper)
    (setf (layout-flags layout) #+64-bit (pack-layout-flags depthoid length flags)
                                #-64-bit flags)
    (setf (layout-clos-hash layout) clos-hash
          (wrapper-classoid wrapper) classoid
          (wrapper-invalid wrapper) invalid)
    #-64-bit (setf (wrapper-depthoid wrapper) depthoid
                   (wrapper-length wrapper) length)
    #-metaspace (setf (wrapper-%info wrapper) info ; already set if #+metaspace
                      (wrapper-slot-table wrapper) #(1 nil))
    (set-layout-inherits wrapper inherits (logtest flags +structure-layout-flag+) id)
    (let ((bitmap-base (+ fixed-words extra-id-words)))
      (dotimes (i bitmap-words)
        (%raw-instance-set/word layout (+ bitmap-base i)
              (ldb (byte sb-vm:n-word-bits (* i sb-vm:n-word-bits)) bitmap))))
    ;; It's not terribly important that we recycle layout IDs, but I have some other
    ;; changes planned that warrant a finalizer per layout.
    ;; FIXME: structure IDs should never be recycled because code blobs referencing
    ;; th ID do not reference the layout, and so the layout could be GCd allowing
    ;; reuse of an old ID for a new type.
    (unless (built-in-classoid-p classoid)
      (let ((layout-addr (- (get-lisp-obj-address layout) sb-vm:instance-pointer-lowtag)))
        (declare (ignorable layout-addr))
        (finalize wrapper
                  (lambda ()
                    #+metaspace (sb-vm::unallocate-metaspace-chunk layout-addr)
                    (atomic-push id (cdr *layout-id-generator*)))
                :dont-save t)))
    ;; Rather than add and delete this line of debugging which I've done so many times,
    ;; let's instead keep it but commented out.
    #+nil
    (alien-funcall (extern-alien "printf" (function void system-area-pointer unsigned unsigned
                                                    unsigned system-area-pointer))
                   (vector-sap #.(format nil "New wrapper ID=%d %p %p '%s'~%"))
                   id (get-lisp-obj-address layout) (get-lisp-obj-address wrapper)
                   (vector-sap (string (classoid-name classoid))))
    wrapper))

#+metaspace
(defun make-temporary-wrapper (clos-hash classoid inherits)
  (let* ((layout (%make-temporary-layout #.(find-layout t) clos-hash))
         (wrapper (%make-wrapper :clos-hash clos-hash :classoid classoid
                                 :inherits inherits :invalid nil
                                 :friend layout)))
    (setf (layout-friend layout) wrapper)
    wrapper))

;;; This allocator is used by the expansion of MAKE-LOAD-FORM-SAVING-SLOTS
;;; when given a STRUCTURE-OBJECT.
(defun allocate-struct (type)
  (let* ((wrapper (classoid-wrapper (the structure-classoid (find-classoid type))))
         (structure (%make-instance (wrapper-length wrapper))))
    (setf (%instance-wrapper structure) wrapper)
    (dolist (dsd (dd-slots (wrapper-dd wrapper)) structure)
      (when (eq (dsd-raw-type dsd) 't)
        (%instance-set structure (dsd-index dsd) (make-unbound-marker))))))

;;; Return the value from the INDEXth slot of INSTANCE. This is SETFable.
;;; This is used right away in warm compile by MAKE-LOAD-FORM-SAVING-SLOTS,
;;; so without it already defined, you can't define it, because you can't dump
;;; debug info structures. Were it not for that, this would go in 'stubs'.
(defun %instance-ref (instance index)
  (%instance-ref instance index))
(defun (setf %instance-ref) (newval instance index)
  (%instance-set instance index newval)
  newval)
#.`(progn
     ,@(map 'list
            (lambda (rsd)
              (let* ((reader (sb-kernel::raw-slot-data-reader-name rsd))
                     (writer (sb-kernel::raw-slot-data-writer-name rsd))
                     (type (sb-kernel::raw-slot-data-raw-type rsd)))
                `(progn
                   (defun ,reader (instance index) (,reader instance index))
                   ;; create a well-behaving SETF-compatible slot setter
                   (defun (setf ,reader) (newval instance index)
                     (declare (,type newval))
                     (,writer instance index newval)
                     newval)
                   ;; .. and a non-SETF-compatible one
                   (defun ,writer (instance index newval)
                     (declare (,type newval))
                     (,writer instance index newval)
                     (values)))))
            sb-kernel::*raw-slot-data*))

(macrolet ((id-bits-sap ()
             `(sap+ (int-sap (get-lisp-obj-address layout))
                    ,(- (sb-vm::id-bits-offset) sb-vm:instance-pointer-lowtag))))
(defun layout-id (layout)
  ;; If a structure type at depthoid >= 2, then fetch the INDEXth id
  ;; where INDEX is depthoid - 2. Otherwise fetch the 0th id.
  ;; There are a few non-structure types at positive depthoid; those do not store
  ;; their ancestors in the vector; they only store self-id at index 0.
  ;; This isn't performance-critical. If it were, then we should store self-ID
  ;; at a fixed index. Using it for type-based dispatch remains a possibility.
  (let* ((layout (cond #+metaspace ((typep layout 'wrapper) (wrapper-friend layout))
                       (t layout)))
         (depth (- (sb-vm::layout-depthoid layout) 2))
         (index (if (or (< depth 0) (not (logtest (layout-flags layout)
                                                  +structure-layout-flag+)))
                    0 depth)))
    (truly-the layout-id
              #-64-bit (%raw-instance-ref/signed-word
                        layout (+ (get-dsd-index sb-vm:layout id-word0) index))
              #+64-bit ; use SAP-ref for lack of half-sized slots
              (with-pinned-objects (layout)
                (signed-sap-ref-32 (id-bits-sap) (ash index 2))))))

(defun set-layout-inherits (wrapper inherits structurep this-id
                            &aux (layout (wrapper-friend wrapper)))
  (setf (wrapper-inherits wrapper) inherits)
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
    (let ((sap (id-bits-sap)))
      (cond (structurep
             (loop for i from 0 by 4
                   for j from 2 below (length inherits) ; skip T and STRUCTURE-OBJECT
                   do (setf (signed-sap-ref-32 sap i) (layout-id (svref inherits j)))
                   finally (setf (signed-sap-ref-32 sap i) this-id)))
            ((not (eql this-id 0))
             (setf (signed-sap-ref-32 sap 0) this-id))))))
) ; end MACROLET

;;; Normally IR2 converted, definition needed for interpreted structure
;;; constructors only.
;;; [Hmm, do we correctly type-check in the call to the constructor?
;;; Because this sure as heck doesn't check anything]
#+(or sb-eval sb-fasteval)
(defun %make-structure-instance (dd slot-specs &rest slot-values)
  (let ((instance (%make-instance (dd-length dd))) ; length = sans header word
        (value-index 0))
    (declare (index value-index))
    (setf (%instance-wrapper instance) (dd-layout-or-lose dd))
    (dolist (spec slot-specs instance)
      (destructuring-bind (kind raw-type . index) spec
        (if (eq kind :unbound)
            (%instance-set instance index (make-unbound-marker))
            (macrolet ((make-case ()
                           `(ecase raw-type
                              ((t) (%instance-set instance index value))
                              ,@(map 'list
                                 (lambda (rsd)
                                   `(,(raw-slot-data-raw-type rsd)
                                     (,(raw-slot-data-writer-name rsd)
                                      instance index value)))
                                 *raw-slot-data*))))
                (let ((value (fast-&rest-nth value-index slot-values)))
                  (incf value-index)
                  (make-case))))))))


;;;; target-only parts of the DEFSTRUCT top level code

;;; A list of hooks designating functions of one argument, the
;;; classoid, to be called when a defstruct is evaluated.
(define-load-time-global *defstruct-hooks* nil)

;;; the part of %DEFSTRUCT which makes sense only on the target SBCL
;;;
(defmacro set-wrapper-equalp-impl (wrapper newval)
  `(%instance-set ,wrapper (get-dsd-index wrapper equalp-impl) ,newval))

(defun assign-equalp-impl (type-name function)
  (set-wrapper-equalp-impl (find-layout type-name) function))

(defun %target-defstruct (dd equalp)
  (declare (type defstruct-description dd))

  (when (dd-doc dd)
    (setf (documentation (dd-name dd) 'structure)
          (dd-doc dd)))

  (let ((classoid (find-classoid (dd-name dd))))
    (let ((layout (classoid-wrapper classoid)))
      (set-wrapper-equalp-impl
          layout
          (cond ((compiled-function-p equalp) equalp)
                ((eql (dd-bitmap dd) +layout-all-tagged+) #'sb-impl::instance-equalp)
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

    (when *type-system-initialized*
      (dolist (fun *defstruct-hooks*)
        (funcall fun classoid))))

  (dd-name dd))

;;; Copy any old kind of structure.
(defun copy-structure (structure)
  "Return a copy of STRUCTURE with the same (EQL) slot values."
  (declare (type structure-object structure))
  (let ((wrapper (%instance-wrapper structure)))
    (when (wrapper-invalid wrapper)
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
      (let ((bitmap (dd-bitmap (wrapper-dd wrapper))))
        ;; Don't assume that %INSTANCE-REF can access the layout.
        (%set-instance-layout res (%instance-layout structure))
        ;; On backends which don't segregate descriptor vs. non-descriptor
        ;; registers, we could speed up this code in an obvious way.
        (macrolet ((copy-loop (tagged-p &optional step)
                     `(do ((i sb-vm:instance-data-start (1+ i)))
                          ((>= i len))
                        (declare (index i))
                        (if ,tagged-p
                            (%instance-set res i (%instance-ref structure i))
                            (%raw-instance-set/word res i
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
  (%set-instance-layout to (%instance-layout from))
  (dotimes (i (%instance-length to) to)
    (%instance-set to i (%instance-ref from i))))
;;; Like %COPY-INSTANCE, but layout was already assigned.
;;; Similarly, will not be called if raw slots and precise GC.
(defun %copy-instance-slots (to from)
  (declare (structure-object to from) (optimize (safety 0)))
  (loop for i from sb-vm:instance-data-start below (%instance-length to)
        do (%instance-set to i (%instance-ref from i)))
  to)
(defun (setf %instance-wrapper) (newval x)
  (%set-instance-layout x (wrapper-friend newval))
  newval)
(defun (setf %fun-wrapper) (newval x)
  (%set-fun-layout x (wrapper-friend newval))
  newval)

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
      (let* ((wrapper (%instance-wrapper structure))
             (dd (wrapper-info wrapper))
             (name (wrapper-classoid-name wrapper)))
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

#+metaspace
(defmethod print-object ((self sb-vm:layout) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (write (layout-id self) :stream stream)))

(defun id-to-layout (id)
  (maphash (lambda (k v)
             (declare (ignore k))
             (when (eql (layout-id v) id)
               (return-from id-to-layout (wrapper-friend v))))
           (classoid-subclasses (find-classoid 't))))
(export 'id-to-layout)

(defun summarize-layouts ()
  (flet ((flag-bits (x) (logand (layout-flags (wrapper-friend x))
                                layout-flags-mask)))
     (let ((prev -1))
       (dolist (layout (sort (loop for v being each hash-value
                                of (classoid-subclasses (find-classoid 't))
                                collect v)
                             #'> :key #'flag-bits))
         (let ((flags (flag-bits layout)))
           (unless (= flags prev)
             (format t "Layout flags = #b~10,'0b~%" flags)
             (setq prev flags)))
         (format t "  ~a~%" layout)))))

(/show0 "target-defstruct.lisp end of file")
