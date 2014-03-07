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
#!+sb-eval
(defun %make-structure-instance (dd slot-specs &rest slot-values)
  (let ((instance (%make-instance (dd-instance-length dd))))
    (setf (%instance-layout instance) (dd-layout-or-lose dd))
    (mapc (lambda (spec value)
            (destructuring-bind (raw-type . index) (cdr spec)
              (macrolet ((make-case ()
                           `(ecase raw-type
                              ((t)
                               (setf (%instance-ref instance index) value))
                              ,@(mapcar
                                 (lambda (rsd)
                                   `(,(raw-slot-data-raw-type rsd)
                                      (setf (,(raw-slot-data-accessor-name rsd)
                                              instance index)
                                            value)))
                                 *raw-slot-data-list*))))
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
(defvar *defstruct-hooks* nil)

;;; the part of %DEFSTRUCT which makes sense only on the target SBCL
;;;
;;; (The "static" in the name is because it needs to be done not only
;;; in ordinary toplevel %DEFSTRUCT, but also in cold init as early as
;;; possible, to simulate static linking of structure functions as
;;; nearly as possible.)
(defun %target-defstruct (dd)
  (declare (type defstruct-description dd))

  (/show0 "entering %TARGET-DEFSTRUCT")

  (when (dd-doc dd)
    (setf (fdocumentation (dd-name dd) 'structure)
          (dd-doc dd)))

  ;; the BOUNDP test here is to get past cold-init.
  (when (boundp '*defstruct-hooks*)
    (dolist (fun *defstruct-hooks*)
      (funcall fun (find-classoid (dd-name dd)))))

  (/show0 "leaving %TARGET-DEFSTRUCT")
  (values))

;;; Copy any old kind of structure.
(defun copy-structure (structure)
  #!+sb-doc
  "Return a copy of STRUCTURE with the same (EQL) slot values."
  (declare (type structure-object structure))
  (let* ((layout (%instance-layout structure))
         (res (%make-instance (%instance-length structure)))
         (len (layout-length layout))
         (nuntagged (layout-n-untagged-slots layout)))

    (declare (type index len))
    (when (layout-invalid layout)
      (error "attempt to copy an obsolete structure:~%  ~S" structure))

    ;; Copy ordinary slots and layout.
    (dotimes (i (- len nuntagged))
      (declare (type index i))
      (setf (%instance-ref res i)
            (%instance-ref structure i)))

    ;; Copy raw slots.
    (dotimes (i nuntagged)
      (declare (type index i))
      (setf (%raw-instance-ref/word res i)
            (%raw-instance-ref/word structure i)))

    res))



;; Do an EQUALP comparison on the raw slots (only, not the normal slots) of a
;; structure.
(defun raw-instance-slots-equalp (layout x y)
  ;; This implementation sucks, but hopefully EQUALP on raw structures
  ;; won't be a major bottleneck for anyone. It'd be tempting to do
  ;; all this with %RAW-INSTANCE-REF/WORD and bitwise comparisons, but
  ;; that'll fail in some cases. For example -0.0 and 0.0 are EQUALP
  ;; but have different bit patterns. -- JES, 2007-08-21
  (loop for dsd in (dd-slots (layout-info layout))
        for raw-type = (dsd-raw-type dsd)
        for rsd = (unless (eql raw-type t)
                    (find raw-type
                          *raw-slot-data-list*
                          :key 'raw-slot-data-raw-type))
        always (or (not rsd)
                   (funcall (raw-slot-data-comparer rsd) (dsd-index dsd) x y))))

;;; default PRINT-OBJECT method

(defun %print-structure-sans-layout-info (name stream)
  ;; KLUDGE: during PCL build debugging, we can sometimes
  ;; attempt to print out a PCL object (with null LAYOUT-INFO).
  (pprint-logical-block (stream nil :prefix "#<" :suffix ">")
    (prin1 name stream)
    (write-char #\space stream)
    (write-string "(no LAYOUT-INFO)" stream)))

(defun %print-structure-sans-slots (name stream)
  ;; the structure type doesn't count as a component for *PRINT-LEVEL*
  ;; processing. We can likewise elide the logical block processing,
  ;; since all we have to print is the type name. -- CSR, 2004-10-05
  (write-string "#S(" stream)
  (prin1 name stream)
  (write-char #\) stream))

(defun %default-structure-pretty-print (structure stream)
  (let* ((layout (%instance-layout structure))
         (name (classoid-name (layout-classoid layout)))
         (dd (layout-info layout)))
    (cond ((not dd)
           (%print-structure-sans-layout-info name stream))
          ((not (dd-slots dd))
           (%print-structure-sans-slots name stream))
          (t
           (pprint-logical-block (stream nil :prefix "#S(" :suffix ")")
             (prin1 name stream)
             (let ((remaining-slots (dd-slots dd)))
               (when remaining-slots
                 (write-char #\space stream)
                 ;; CMU CL had (PPRINT-INDENT :BLOCK 2 STREAM) here,
                 ;; but I can't see why. -- WHN 20000205
                 (pprint-newline :linear stream)
                 (loop
                   (pprint-pop)
                   (let ((slot (pop remaining-slots)))
                     (write-char #\: stream)
                     (output-symbol-name (symbol-name (dsd-name slot)) stream)
                     (write-char #\space stream)
                     (pprint-newline :miser stream)
                     (output-object (funcall (fdefinition (dsd-accessor-name slot))
                                             structure)
                                    stream)
                     (when (null remaining-slots)
                       (return))
                     (write-char #\space stream)
                     (pprint-newline :linear stream))))))))))

(defun %default-structure-ugly-print (structure stream)
  (let* ((layout (%instance-layout structure))
         (name (classoid-name (layout-classoid layout)))
         (dd (layout-info layout)))
    (cond ((not dd)
           (%print-structure-sans-layout-info name stream))
          ((not (dd-slots dd))
           (%print-structure-sans-slots name stream))
          (t
           (descend-into (stream)
             (write-string "#S(" stream)
             (prin1 name stream)
             (do ((index 0 (1+ index))
                  (remaining-slots (dd-slots dd) (cdr remaining-slots)))
                 ((or (null remaining-slots)
                      (and (not *print-readably*)
                           *print-length*
                           (>= index *print-length*)))
                  (if (null remaining-slots)
                      (write-string ")" stream)
                      (write-string " ...)" stream)))
               (declare (type index index))
               (write-string " :" stream)
               (let ((slot (first remaining-slots)))
                 (output-symbol-name (symbol-name (dsd-name slot)) stream)
                 (write-char #\space stream)
                 (output-object
                  (funcall (fdefinition (dsd-accessor-name slot))
                           structure)
                  stream))))))))

(defun default-structure-print (structure stream depth)
  (declare (ignore depth))
  (cond ((funcallable-instance-p structure)
         (print-unreadable-object (structure stream :identity t :type t)))
        (*print-pretty*
         (%default-structure-pretty-print structure stream))
        (t
         (%default-structure-ugly-print structure stream))))

(def!method print-object ((x structure-object) stream)
  (default-structure-print x stream *current-level-in-print*))

(/show0 "target-defstruct.lisp end of file")
