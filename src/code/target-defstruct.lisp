;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

(/show0 "target-defstruct.lisp 12")

;;;; structure frobbing primitives

;;; Allocate a new instance with LENGTH data slots.
(defun %make-instance (length)
  (declare (type index length))
  (%make-instance length))

;;; Given an instance, return its length.
(defun %instance-length (instance)
  (declare (type instance instance))
  (%instance-length instance))

;;; Return the value from the INDEXth slot of INSTANCE. This is SETFable.
(defun %instance-ref (instance index)
  (%instance-ref instance index))

;;; Set the INDEXth slot of INSTANCE to NEW-VALUE.
(defun %instance-set (instance index new-value)
  (setf (%instance-ref instance index) new-value))

;;; Normally IR2 converted, definition needed for interpreted structure
;;; constructors only.
#!+(or sb-eval sb-fasteval)
(defun %make-structure-instance (dd slot-specs &rest slot-values)
  (let ((instance (%make-instance (dd-instance-length dd))))
    (setf (%instance-layout instance) (dd-layout-or-lose dd))
    (mapc (lambda (spec value)
            (destructuring-bind (raw-type . index) (cdr spec)
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
                (make-case))))
          slot-specs slot-values)
    instance))

(defun %raw-instance-ref/word (instance index)
  (declare (type index index))
  (%raw-instance-ref/word instance index))
(defun %raw-instance-set/word (instance index new-value)
  (declare (type index index)
           (type sb!vm:word new-value))
  (%raw-instance-set/word instance index new-value))

(defun %raw-instance-ref/single (instance index)
  (declare (type index index))
  (%raw-instance-ref/single instance index))
(defun %raw-instance-set/single (instance index new-value)
  (declare (type index index)
           (type single-float new-value))
  (%raw-instance-set/single instance index new-value))

(defun %raw-instance-ref/double (instance index)
  (declare (type index index))
  (%raw-instance-ref/double instance index))
(defun %raw-instance-set/double (instance index new-value)
  (declare (type index index)
           (type double-float new-value))
  (%raw-instance-set/double instance index new-value))

(defun %raw-instance-ref/complex-single (instance index)
  (declare (type index index))
  (%raw-instance-ref/complex-single instance index))
(defun %raw-instance-set/complex-single (instance index new-value)
  (declare (type index index)
           (type (complex single-float) new-value))
  (%raw-instance-set/complex-single instance index new-value))

(defun %raw-instance-ref/complex-double (instance index)
  (declare (type index index))
  (%raw-instance-ref/complex-double instance index))
(defun %raw-instance-set/complex-double (instance index new-value)
  (declare (type index index)
           (type (complex double-float) new-value))
  (%raw-instance-set/complex-double instance index new-value))

(defun %instance-layout (instance)
  (%instance-layout instance))

(defun %set-instance-layout (instance new-value)
  (%set-instance-layout instance new-value))

(defun %make-funcallable-instance (len)
  (%make-funcallable-instance len))

(defun funcallable-instance-p (x)
  (funcallable-instance-p x))

(deftype funcallable-instance ()
  `(satisfies funcallable-instance-p))

(defun %funcallable-instance-info (fin i)
  (%funcallable-instance-info fin i))

(defun %set-funcallable-instance-info (fin i new-value)
  (%set-funcallable-instance-info fin i new-value))

(defun funcallable-instance-fun (fin)
  (%funcallable-instance-function fin))

(defun (setf funcallable-instance-fun) (new-value fin)
  (setf (%funcallable-instance-function fin) new-value))

;;;; target-only parts of the DEFSTRUCT top level code

;;; A list of hooks designating functions of one argument, the
;;; classoid, to be called when a defstruct is evaluated.
(!defvar *defstruct-hooks* nil)

;;; the part of %DEFSTRUCT which makes sense only on the target SBCL
;;;
(defun %target-defstruct (dd)
  (declare (type defstruct-description dd))

  (/show0 "entering %TARGET-DEFSTRUCT")

  (when (dd-doc dd)
    (setf (fdocumentation (dd-name dd) 'structure)
          (dd-doc dd)))

  (let* ((classoid (find-classoid (dd-name dd)))
         (layout (classoid-layout classoid)))
    (when (eq (dd-pure dd) t)
      (setf (layout-pure layout) t))
    #!+interleaved-raw-slots
    ;; Make a vector of EQUALP slots comparators, indexed by (- word-index data-start).
    ;; This has to be assigned to something regardless of whether there are
    ;; raw slots just in case someone mutates a layout which had raw
    ;; slots into one which does not - although that would probably crash
    ;; unless no instances exist or all raw slots miraculously contained
    ;; bits which were the equivalent of valid Lisp descriptors.
    ;;
    ;; It's not worth adding a #-interleaved-raw-slots case to this optimization
    ;; because every architecture can be made to use the new approach.
    (setf (layout-equalp-tests layout)
          (if (zerop (layout-untagged-bitmap layout))
              #()
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
                     (make-array (- (dd-length dd) sb!vm:instance-data-start)
                                 :initial-element nil)))
                (dolist (slot (dd-slots dd) comparators)
                  ;; -1 because LAYOUT (slot index 0) has no comparator stored.
                  (setf (aref comparators
                              (- (dsd-index slot) sb!vm:instance-data-start))
                        (let ((rsd (dsd-raw-slot-data slot)))
                          (if (not rsd)
                              0 ; means recurse using EQUALP
                              (raw-slot-data-comparer rsd))))))))

    (dolist (fun *defstruct-hooks*)
      (funcall fun classoid)))

  (/show0 "leaving %TARGET-DEFSTRUCT")
  (values))

;;; Copy any old kind of structure.
(defun copy-structure (structure)
  #!+sb-doc
  "Return a copy of STRUCTURE with the same (EQL) slot values."
  (declare (type structure-object structure))
  (let ((layout (%instance-layout structure)))
    (when (layout-invalid layout)
      (error "attempt to copy an obsolete structure:~%  ~S" structure))
    (let ((res (%make-instance (%instance-length structure)))
          (len (layout-length layout)))
      (declare (type index len))
      #!-interleaved-raw-slots
      (let ((nuntagged (layout-n-untagged-slots layout)))
        ;; Copy ordinary slots including the layout.
        (dotimes (i (- len nuntagged))
          (declare (type index i))
          (setf (%instance-ref res i) (%instance-ref structure i)))
        ;; Copy raw slots.
        (dotimes (i nuntagged)
          (declare (type index i))
          (setf (%raw-instance-ref/word res i)
                (%raw-instance-ref/word structure i))))
      #!+interleaved-raw-slots
      (let ((metadata (layout-untagged-bitmap layout)))
        ;; Don't assume that %INSTANCE-REF can access the layout.
        (setf (%instance-layout res) (%instance-layout structure))
        ;; With interleaved slots, the only difference between %instance-ref
        ;; and %raw-instance-ref/word is the storage class of the VOP operands.
        ;; Since x86(-64) doesn't partition the register set, the bitmap test
        ;; could be skipped if we wanted to copy everything as raw.
        (macrolet ((copy-loop (raw-p &optional step)
                     `(do ((i sb!vm:instance-data-start (1+ i)))
                          ((>= i len))
                        (declare (index i))
                        (if ,raw-p
                            (setf (%raw-instance-ref/word res i)
                                  (%raw-instance-ref/word structure i))
                            (setf (%instance-ref res i)
                                  (%instance-ref structure i)))
                        ,step)))
          (cond ((zerop metadata) ; no untagged slots.
                 (copy-loop nil))
                ;; The fixnum case uses fixnum operations for ODDP and ASH.
                ((fixnump metadata) ; shift and mask is faster than logbitp
                 (copy-loop (oddp (truly-the fixnum metadata))
                            (setq metadata (ash metadata -1))))
                (t ; bignum - use LOGBITP to avoid consing more bignums
                 (copy-loop (logbitp i metadata))))))
      res)))


;; Do an EQUALP comparison on the raw slots (only, not the normal slots) of a
;; structure.
#!-interleaved-raw-slots
(defun raw-instance-slots-equalp (layout x y)
  ;; This implementation sucks, but hopefully EQUALP on raw structures
  ;; won't be a major bottleneck for anyone. It'd be tempting to do
  ;; all this with %RAW-INSTANCE-REF/WORD and bitwise comparisons, but
  ;; that'll fail in some cases. For example -0.0 and 0.0 are EQUALP
  ;; but have different bit patterns. -- JES, 2007-08-21
  (loop for dsd in (dd-slots (layout-info layout))
        for raw-type-index = (dsd-%raw-type dsd)
        always (or (eql raw-type-index -1)
                   (funcall (raw-slot-data-comparer
                             (svref *raw-slot-data* raw-type-index))
                            (dsd-index dsd) x y))))

;;; default PRINT-OBJECT method

;;; Printing formerly called the auto-generated accessor functions,
;;; but reading the slots more primitively confers several advantages:
;;; - it works even if the user clobbered an accessor
;;; - it works if the slot fails a type-check and the reader was SAFE-P,
;;    i.e. was required to perform a check. This is a feature, not a bug.
(macrolet ((access-fn (dsd)
             `(acond ((dsd-raw-slot-data ,dsd)
                      (symbol-function (raw-slot-data-accessor-name it)))
                     (t #'%instance-ref))))

(defun %default-structure-pretty-print (structure stream name dd)
  (pprint-logical-block (stream nil :prefix "#S(" :suffix ")")
    (prin1 name stream)
    (let ((remaining-slots (dd-slots dd)))
      (when remaining-slots
        (write-char #\space stream)
                 ;; CMU CL had (PPRINT-INDENT :BLOCK 2 STREAM) here,
                 ;; but I can't see why. -- WHN 20000205
        (pprint-newline :linear stream)
        (loop (pprint-pop)
              (let ((slot (pop remaining-slots)))
                (write-char #\: stream)
                (output-symbol-name (symbol-name (dsd-name slot)) stream)
                (write-char #\space stream)
                (pprint-newline :miser stream)
                (output-object (funcall (access-fn slot) structure (dsd-index slot))
                               stream)
                (when (null remaining-slots)
                  (return))
                (write-char #\space stream)
                (pprint-newline :linear stream)))))))

(defun %default-structure-ugly-print (structure stream name dd)
  (descend-into (stream)
    (write-string "#S(" stream)
    (prin1 name stream)
    (do ((index 0 (1+ index))
         (limit (or (and (not *print-readably*) *print-length*)
                    most-positive-fixnum))
         (remaining-slots (dd-slots dd) (cdr remaining-slots)))
        ((or (null remaining-slots) (>= index limit))
         (write-string (if remaining-slots " ...)" ")") stream))
      (declare (type index index))
      (write-string " :" stream)
      (let ((slot (first remaining-slots)))
        (output-symbol-name (symbol-name (dsd-name slot)) stream)
        (write-char #\space stream)
        (output-object (funcall (access-fn slot) structure (dsd-index slot))
                       stream)))))
) ; end MACROLET

(defun default-structure-print (structure stream depth)
  (declare (ignore depth))
  (if (funcallable-instance-p structure)
      (print-unreadable-object (structure stream :identity t :type t))
      (let* ((layout (%instance-layout structure))
             (dd (layout-info layout))
             (name (classoid-name (layout-classoid layout))))
        (cond ((not dd)
               ;; FIXME? this branch may be unnecessary as a consequence
               ;; of change f02bee325920166b69070e4735a8a3f295f8edfd which
               ;; stopped the badness is a stronger way. It should be the case
               ;; that absence of a DD can't happen unless the classoid is absent.
               ;; KLUDGE: during PCL build debugging, we can sometimes
               ;; attempt to print out a PCL object (with null LAYOUT-INFO).
               (pprint-logical-block (stream nil :prefix "#<" :suffix ">")
               (prin1 name stream)
               (write-char #\space stream)
               (write-string "(no LAYOUT-INFO)" stream)))
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

(def!method print-object ((x structure-object) stream)
  (default-structure-print x stream *current-level-in-print*))

;; This generates a sexpr that can be recognized as having a particular
;; shape so that the dumping mechanism can decide if it is or is not
;; necessary to run that code through the main compiler - FASL operations
;; can be used in the case that all slots are preserved.
;; In particular, there are no gensyms in the result, so that calling this
;; twice on the same object yields the same list as compared by EQUAL.
(defun structure-obj-slot-saving-forms (struct slot-names slot-names-p)
  (let* ((layout (%instance-layout struct))
         (dd (layout-info layout)))
    (mapcan (lambda (dsd)
              (when (or (not slot-names-p) (memq (dsd-name dsd) slot-names))
                (let ((index (dsd-index dsd))
                      (rsd (dsd-raw-slot-data dsd)))
                  (if (not rsd)
                      `((%instance-ref ,struct ,index)
                        ,(let ((val (%instance-ref struct index)))
                           (if (and (or (listp val) (symbolp val))
                                    (not (member val '(nil t))))
                               (list 'quote val)
                               val)))
                      (let ((accessor (raw-slot-data-accessor-name rsd)))
                        `((,accessor ,struct ,index)
                          ,(funcall accessor struct index)))))))
            (dd-slots dd))))

;; Return T if CREATION-FORM and INIT-FORM would have the identical effect
;; as :SB-JUST-DUMP-IT-NORMALLY for STRUCT. MAKE-LOAD-FORM-SAVING-SLOTS can't
;; merely return the magic token (when possible) because a user application
;; could call MAKE-LOAD-FORM-SAVING-SLOTS to obtain forms that can be evaluated
;; or otherwise examined. So instead we scan the code and detect whether it is
;; identical to what was returned from a trivial use of M-L-F-S-S.
(defun canonical-slot-saving-forms-p (struct creation-form init-form)
  (and (sb!c::canonical-instance-maker-form-p creation-form)
       (typep init-form '(cons (eql setf)))
       (eq (cadr (cadr (cadr creation-form))) (class-name (class-of struct)))
       (= (length (dd-slots (layout-info (%instance-layout struct))))
          (ash (list-length (cdr init-form)) -1))
       (flet ((eq-quoted-p (a b)
                (and (typep a '(cons (eql quote) (cons t null)))
                     (typep b '(cons (eql quote) (cons t null)))
                     (eq (cadr a) (cadr b)))))
         ;; Naively, EQUALP would almost work to compare the slot assignments,
         ;; but we must not get stuck in circular lists, so traverse by hand.
         (loop for (expect-place expect-value)
               on (structure-obj-slot-saving-forms struct nil nil) by #'cddr
               for (actual-place actual-value) on (cdr init-form) by #'cddr
               always (and (equal actual-place expect-place)
                           ;; Use EQL, not EQ. Values come from the identical
                           ;; struct, but reading a raw slot can create new
                           ;; pointers. For QUOTE forms, EQ is ok.
                           (or (eql actual-value expect-value)
                               (eq-quoted-p actual-value expect-value)))))))

(/show0 "target-defstruct.lisp end of file")
