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

#+permgen
(defun allocate-permgen-layout (nwords)
  (flet ((thunk ()
           (let ((freeptr sb-vm:*permgen-space-free-pointer*))
             (setf sb-vm:*permgen-space-free-pointer*
                   ;; round-to-odd, add the header word
                   (sap+ freeptr (ash (1+ (logior nwords 1)) sb-vm:word-shift)))
             (aver (<= (sap-int sb-vm:*permgen-space-free-pointer*)
                       (+ sb-vm:permgen-space-start sb-vm:permgen-space-size)))
             (setf (sap-ref-word freeptr 0)
                   (logior (ash nwords sb-vm:instance-length-shift)
                           sb-vm:instance-widetag))
             (%make-lisp-obj (sap-int (sap+ freeptr sb-vm:instance-pointer-lowtag))))))
    (if (sb-thread::mutex-p sb-vm::*allocator-mutex*)
        (with-system-mutex (sb-vm::*allocator-mutex* :without-gcing t) (thunk))
        (thunk))))

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
(defun make-layout (clos-hash classoid
                    &key (depthoid -1) (length 0) (flags 0)
                         (inherits #())
                         (info nil)
                         (bitmap (if info (dd-bitmap info) 0))
                         (invalid :uninitialized))
  (let* ((fixed-words (type-dd-length layout))
         (extra-id-words ; count of additional words needed to store ancestors
          (if (logtest flags +structure-layout-flag+)
              (calculate-extra-id-words depthoid)
              0))
         (bitmap-words (ceiling (1+ (integer-length bitmap)) sb-vm:n-word-bits))
         (nwords (+ fixed-words extra-id-words bitmap-words))
         (layout
          (truly-the layout
                     #+compact-instance-header
                     (progn
                       #+permgen (allocate-permgen-layout nwords)
                       #+immobile-space
                       (sb-vm::alloc-immobile-fixedobj
                        (1+ nwords)
                        (logior (ash nwords sb-vm:instance-length-shift)
                                sb-vm:instance-widetag)))
                     #-compact-instance-header
                     (%make-instance/mixed nwords))))
    (%set-instance-layout layout #.(find-layout 'layout))
    (setf (layout-flags layout) #+64-bit (pack-layout-flags depthoid length flags)
                                #-64-bit flags)
    (setf (layout-clos-hash layout) clos-hash
          (layout-classoid layout) classoid
          (layout-invalid layout) invalid)
    #-64-bit (setf (layout-depthoid layout) depthoid
                   (layout-length layout) length)
    (setf (layout-%info layout) info
          (layout-slot-table layout) #(1 nil))
    ;; All layouts initially have an ID of 0. The ID is assigned on demand
    ;; for the layouts that actually get registered, and only those.
    ;; We do not need to or want to recycle IDs, because:
    ;; - assembly code can use layout-ID fixups without reference to the layout.
    ;;   So GCing a layout should *not* allow reuse of its ID because we have no
    ;;   way of knowing where it was wired in.
    ;; - only subtypes of STRUCTURE-OBJECT need an ID for TYPEP, and structures
    ;;   are not redefinable, so you'd have to define 2^32 different structure
    ;;   types to exhaust the space of IDS.
    (set-layout-inherits layout inherits (logtest flags +structure-layout-flag+) 0)
    (let ((bitmap-base (+ fixed-words extra-id-words)))
      (dotimes (i bitmap-words)
        (%raw-instance-set/word layout (+ bitmap-base i)
              (ldb (byte sb-vm:n-word-bits (* i sb-vm:n-word-bits)) bitmap))))
    (aver (= (%layout-bitmap layout) bitmap)) ; verify it reads back the same
    layout))

;;; Extract the bitmap from 1 or more words of bits that have the same format
;;; as a BIGNUM - least-significant word first, native bit order within each word,
;;; all but the last are unsigned, and the last is signed.
(defun %layout-bitmap (layout &aux (nwords (bitmap-nwords layout))
                                   (start (bitmap-start layout)))
  (declare (type layout layout))
  (if (and (= nwords 1) (fixnump (%raw-instance-ref/signed-word layout start)))
      (the fixnum (%raw-instance-ref/signed-word layout start))
      (do* ((res (sb-bignum:%allocate-bignum nwords))
            (i 0 (1+ i))
            (j start (1+ j)))
           ((= i nwords)
            ;; not sure if this NORMALIZE call is needed, but it can't hurt
            (sb-bignum::%normalize-bignum res nwords))
       (setf (sb-bignum:%bignum-ref res i) (%raw-instance-ref/word layout j)))))

(defun layout-bitmap (layout)
  (declare (type layout layout))
  ;; Whenever we call LAYOUT-BITMAP on a structure-object subtype,
  ;; it's supposed to have the INFO slot populated, linking it to a defstruct-description.
  (acond ((layout-info layout) (dd-bitmap it))
         ;; Instances lacking DD-INFO are CLOS objects, which can't generally have
         ;; raw slots, except that funcallable-instances may have 2 raw slots -
         ;; the trampoline and the layout. The trampoline can have a tag, depending
         ;; on the platform, and the layout is tagged but a special case.
         ;; In any event, the bitmap is always 1 word, and there are no "extra ID"
         ;; words preceding it.
         (t
          (aver (not (logtest +structure-layout-flag+ (layout-flags layout))))
          (the fixnum
               (%raw-instance-ref/signed-word layout (type-dd-length layout))))))

;;; This allocator is used by the expansion of MAKE-LOAD-FORM-SAVING-SLOTS
;;; when given a STRUCTURE-OBJECT.
(defun allocate-struct (type)
  (let* ((layout (classoid-layout (the structure-classoid (find-classoid type))))
         (dd (layout-dd layout))
         (structure (let ((len (layout-length layout)))
                      (if (dd-has-raw-slot-p dd)
                          (%make-instance/mixed len)
                          (%make-instance len)))))
    (%set-instance-layout structure layout)
    (dolist (dsd (dd-slots dd) structure)
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

(macrolet ((define-raw-slot-accessors ()
             `(progn
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
                       sb-kernel::*raw-slot-data*))))
  (define-raw-slot-accessors))

(define-load-time-global *layout-id-generator* (cons 0  nil))
(declaim (type (cons fixnum) *layout-id-generator*))
(define-load-time-global *layout-id-mutex* (sb-thread:make-mutex :name "LAYOUT-ID"))
(macrolet ((id-bits-sap ()
             `(sap+ (int-sap (get-lisp-obj-address layout))
                    ,(- (sb-vm::id-bits-offset) sb-vm:instance-pointer-lowtag)))
           (access-it ()
             #-64-bit `(%raw-instance-ref/signed-word
                        layout (+ (get-dsd-index layout id-word0) index))
             ;; use SAP-ref, for lack of half-sized slots
             #+64-bit `(signed-sap-ref-32 (id-bits-sap) (ash index 2))))
(defun layout-id (layout &optional (assign t))
  ;; If a structure type at depthoid >= 2, then fetch the INDEXth id
  ;; where INDEX is depthoid - 2. Otherwise fetch the 0th id.
  ;; There are a few non-structure types at positive depthoid; those do not store
  ;; their ancestors in the vector; they only store self-id at index 0.
  ;; This isn't performance-critical. If it were, then we should store self-ID
  ;; at a fixed index. Using it for type-based dispatch remains a possibility.
  (let* ((depth (- (sb-vm::layout-depthoid layout) 2))
         (index (if (or (< depth 0) (not (logtest (layout-flags layout)
                                                  +structure-layout-flag+)))
                    0 depth))
         (id (with-pinned-objects (layout)
               (access-it))))
    (truly-the
     (or null layout-id)
     (cond ((not (zerop id)) id)
           (assign
            (aver (logior +structure-layout-flag+ (layout-flags layout)))
            (with-system-mutex (*layout-id-mutex*)
              (let ((id (truly-the layout-id (access-it)))) ; double-check
                (if (zerop id)
                    (with-pinned-objects (layout)
                      (setf (access-it)
                            ;; doesn't really need ATOMIC- any moren
                            (atomic-incf (car *layout-id-generator*))))
                    id))))))))

(defun set-layout-inherits (layout inherits structurep this-id)
  (setf (layout-inherits layout) inherits)
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
  (let ((instance (let ((len (dd-length dd))) ; # of words excluding the header
                    (if (dd-has-raw-slot-p dd)
                        (%make-instance/mixed len)
                        (%make-instance len))))
        (value-index 0))
    (declare (index value-index))
    (%set-instance-layout instance (dd-layout-or-lose dd))
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
(defmacro set-layout-equalp-impl (layout newval)
  `(%instance-set ,layout (get-dsd-index layout equalp-impl) ,newval))

(defun assign-equalp-impl (type-name function)
  (set-layout-equalp-impl (find-layout type-name) function))

;;; This variable is just a somewhat hokey way to pass additional
;;; arguments to the defstruct hook (which renders the structure definition
;;; into a CLOS class) without having to figure out some means of stashing
;;; functions in the DD or DD for the structure.
(defvar *struct-accesss-fragments* nil)
(define-load-time-global *struct-accesss-fragments-delayed* nil)

(defun !bootstrap-defstruct-hook (classoid)
  ;; I hate this, but do whatever it takes...
  ;; A better approach might be to write the correct data
  ;; into the LAYOUT-SLOT-TABLE now.
  ;; (I think that's where the code fragments end up)
  (unless (member (classoid-name classoid) '(pathname condition)) ; KLUDGE
    (push (cons (classoid-name classoid) *struct-accesss-fragments*)
          *struct-accesss-fragments-delayed*)))

(defun %target-defstruct (dd equalp &rest accessors)
  (declare (type defstruct-description dd))

  (when (dd-doc dd)
    (setf (documentation (dd-name dd) 'structure) (dd-doc dd)))

  (let ((classoid (find-classoid (dd-name dd))))
    (let ((layout (classoid-layout classoid)))
      (set-layout-equalp-impl
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

    (let ((*struct-accesss-fragments* accessors))
      (dolist (fun *defstruct-hooks*)
        (funcall fun classoid))))

  (dd-name dd))
(defun !target-defstruct-altmetaclass (dd &rest accessors)
  (declare (type defstruct-description dd))
  (let ((classoid (find-classoid (dd-name dd)))
        (*struct-accesss-fragments* accessors))
    (dolist (fun *defstruct-hooks*)
      (funcall fun classoid)))
  t)

;;; Copy any old kind of structure.

;; The finicky backends disallow loading and storing raw words
;; using %INSTANCE-REF. The robust backends allow it.
;; MIPS is probably robust, but I don't care to try it.
(macrolet ((fast-loop (result)
             `(do ((i sb-vm:instance-data-start (1+ i)))
                  ((>= i len) ,result)
                (declare (index i))
                (%instance-set ,result i (%instance-ref structure i)))))

(defun sys-copy-struct (structure)
  (declare (sb-c::tlab :system))
  #-system-tlabs (copy-structure structure)
  #+system-tlabs ; KISS by allocating to sys-mixed-tlab in all cases
  (let* ((len (%instance-length structure))
         (copy (%make-instance/mixed len)))
    (%set-instance-layout copy (%instance-layout structure))
    (fast-loop copy)))

(defun copy-structure (structure)
  "Return a copy of STRUCTURE with the same (EQL) slot values."
  (declare (type structure-object structure))
  ;; %INSTANCE-LENGTH returns the number of words that are logically in the
  ;; instance excluding any padding and/or stable-hash slot.
  ;; Variable-length structure types are allowed.
  (let* ((layout (%instance-layout structure))
         (len (%instance-length structure)))
      #+(or x86 x86-64) ; Two allocators, but same loop either way
      (let ((res (%new-instance* layout len)))
        (fast-loop res))
      #-(or x86 x86-64) ; Different loops
      (if (logtest (layout-flags layout) sb-vm::+strictly-boxed-flag+)
          (let ((res (%new-instance layout len)))
            (fast-loop res))
          (let ((res (%make-instance/mixed len)))
            (%set-instance-layout res layout)
            ;; DO-LAYOUT-BITMAP does not visit the LAYOUT itself
            ;; (if that occupies a whole slot vs. being in the header)
            (do-layout-bitmap (i taggedp layout len)
              (if taggedp
                  (%instance-set res i (%instance-ref structure i))
                  (%raw-instance-set/word
                   res i (%raw-instance-ref/word structure i))))
            res)))))

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
(defun (setf %instance-layout) (newval x)
  (%set-instance-layout x newval)
  newval)
(defun (setf %fun-layout) (newval x)
  (%set-fun-layout x newval)
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

(defun id-to-layout (id)
  (maphash (lambda (k v)
             (declare (ignore k))
             (when (eql (layout-id v) id)
               (return-from id-to-layout v)))
           (classoid-subclasses (find-classoid 't))))
(export 'id-to-layout)

(defun summarize-layouts ()
  (flet ((flag-bits (x) (logand (layout-flags x) layout-flags-mask)))
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

(defconstant fast-slot-table-fixed-cells 3)

;;; Optimizations to speed up slot-value on structure-object
;;;  1. try to generate fewer collisions in the symbol -> index map
;;;  2. use the fastrem-32 algorithm to compute FLOOR
;;;  3. specialized function which doesn't check slot-unbound or whether the
;;;     slot has :CLASS allocation (which it can't)
;;; If "flexible" defstructs (multiple inheritance, standard-object ancestors) are ever
;;; brought to life, these might be inadmissible. Probably we would not store the
;;; fast map in such situations.
(defun best-slot-map-parameters (names)
  (let* ((raw-hashes (mapcar (lambda (x) (symbol-hash (the symbol x))) names))
         (n-names (length names))
         (n-cells (logior n-names 1)) ; round to odd if not already
         ;; remember the best N-COLLISIONS N-CELLS MASK C
         (best))
    (declare (type (unsigned-byte 32) n-cells))
    ;; Unlike with package symbol tables, a slot table has a fixed set of symbols.
    ;; Therefore we can try to produce the best hashing for that particular set.
    (loop ; over increasing value of N-CELLS
     (multiple-value-bind (mask c) (sb-impl::optimized-symtbl-remainder-params n-cells)
       (loop for shift from 0 to 31
             do
             (let* ((indices
                     (mapcar (lambda (hash)
                               (let ((masked-hash (logand (ash hash (- shift)) mask)))
                                 (sb-vm::fastrem-32 masked-hash c n-cells)))
                             raw-hashes))
                    (badness (- n-names (length (remove-duplicates indices)))))
               (when (= badness 0)
                 (return-from best-slot-map-parameters
                   (list 0 n-cells shift mask c)))
               (when (or (not best) (< badness (car best)))
                 (setf best
                       (list badness n-cells shift mask c))))))
     (when (> n-cells (* 2 n-names))
       (return best))
     (incf n-cells 2))))

;; The result is a vector:
;;  #(SHIFT MASK C symbol .. symbol ... index .. index ...)
;; The length of the vector implies the divisor.
(defun make-struct-slot-map (dd)
  (destructuring-bind (n-cells shift mask c)
      (cdr (best-slot-map-parameters
            (mapcar 'dsd-name (dd-slots dd))))
    (let ((map (make-array (+ (* n-cells 2) fast-slot-table-fixed-cells)
                           :initial-element 0)))
      (setf (aref map 0) shift
            (aref map 1) mask
            (aref map 2) c)
      (fill map nil :start (+ fast-slot-table-fixed-cells n-cells))
      (dolist (dsd (dd-slots dd) map)
          (binding*
           ((hash (symbol-hash (dsd-name dsd)))
            (masked-hash (logand (ash hash (- shift)) mask))
            (bin (truly-the index
                            (+ (sb-vm::fastrem-32 masked-hash c n-cells)
                               fast-slot-table-fixed-cells)))
            (dsd-bits (dsd-bits dsd))
            (name (dsd-name dsd))
            ((key value)
             (cond ((eql (svref map bin) 0) ; empty, just store the name and dsd-index
                    (values name dsd-bits))
                   ;; A bin with a collision is upgraded to a vector of the two entries
                   ((symbolp (svref map bin))
                    (values (vector (svref map bin) name)
                            (vector (svref map (+ bin n-cells)) dsd-bits)))
                   ;; Multiple collisions
                   (t
                    (values (concatenate 'vector (svref map bin) (list name))
                            (concatenate 'vector
                                         (svref map (+ bin n-cells))
                                         (list dsd-bits)))))))
           (setf (svref map bin) key
                 (svref map (+ bin n-cells)) value))))))


(/show0 "target-defstruct.lisp end of file")
