;;;; COERCE and related code

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(macrolet ((def (name result access src-type &optional typep)
             `(defun ,name (object ,@(if typep '(type) ()))
                (declare (type ,(ecase src-type
                                       (:list 'list)
                                       (:vector 'vector)
                                       (:sequence 'sequence)) object))
                (do* ((index 0 (1+ index))
                      (length (length object))
                      (result ,result)
                      (in-object object))
                     ((>= index length) result)
                  (declare (fixnum length index))
                  (declare (type vector result))
                  (setf (,access result index)
                        ,(ecase src-type
                           (:list '(pop in-object))
                           (:vector '(aref in-object index))
                           (:sequence '(elt in-object index))))))))

  (def list-to-vector* (make-sequence type length)
    aref :list t)

  (def vector-to-vector* (make-sequence type length)
    aref :vector t)

  (def sequence-to-vector* (make-sequence type length)
    aref :sequence t))

(defun vector-to-list* (object)
  (declare (type vector object))
  (let ((result (list nil))
        (length (length object)))
    (declare (fixnum length))
    (do ((index 0 (1+ index))
         (splice result (cdr splice)))
        ((>= index length) (cdr result))
      (declare (fixnum index))
      (rplacd splice (list (aref object index))))))

(defvar *offending-datum*); FIXME: Remove after debugging COERCE.

;;; These are used both by the full DEFUN function and by various
;;; optimization transforms in the constant-OUTPUT-TYPE-SPEC case.
;;;
;;; Most of them are INLINE so that they can be optimized when the
;;; argument type is known. It might be better to do this with
;;; DEFTRANSFORMs, though.
(declaim (inline coerce-to-list))
(declaim (inline coerce-to-vector))
(defun coerce-to-fun (object)
  ;; (Unlike the other COERCE-TO-FOOs, this one isn't inline, because
  ;; it's so big and because optimizing away the outer ETYPECASE
  ;; doesn't seem to buy us that much anyway.)
  (etypecase object
    (symbol
     ;; ANSI lets us return ordinary errors (non-TYPE-ERRORs) here.
     (cond ((macro-function object)
            (error "~S names a macro." object))
           ((special-operator-p object)
            (error "~S is a special operator." object))
           (t (fdefinition object))))
    (list
     (case (first object)
       ((setf)
        (fdefinition object))
       ((lambda)
        ;; FIXME: If we go to a compiler-only implementation, this can
        ;; become COMPILE instead of EVAL, which seems nicer to me.
        (eval `(function ,object)))
       ((instance-lambda)
        (deprecation-warning 'instance-lambda 'lambda)
        (eval `(function ,object)))
       (t
        (error 'simple-type-error
               :datum object
               :expected-type '(or symbol
                                   ;; KLUDGE: ANSI wants us to
                                   ;; return a TYPE-ERROR here, and
                                   ;; a TYPE-ERROR is supposed to
                                   ;; describe the expected type,
                                   ;; but it's not obvious how to
                                   ;; describe the coerceable cons
                                   ;; types, so we punt and just say
                                   ;; CONS. -- WHN 20000503
                                   cons)
               :format-control "~S can't be coerced to a function."
               :format-arguments (list object)))))))

(defun coerce-to-list (object)
  (etypecase object
    (vector (vector-to-list* object))))

(defun coerce-to-vector (object output-type-spec)
  (etypecase object
    (list (list-to-vector* object output-type-spec))
    (vector (vector-to-vector* object output-type-spec))))

;;; old working version
(defun coerce (object output-type-spec)
  #!+sb-doc
  "Coerce the Object to an object of type Output-Type-Spec."
  (flet ((coerce-error ()
           (/show0 "entering COERCE-ERROR")
           (error 'simple-type-error
                  :format-control "~S can't be converted to type ~S."
                  :format-arguments (list object output-type-spec)
                  :datum object
                  :expected-type output-type-spec)))
    (let ((type (specifier-type output-type-spec)))
      (cond
        ((%typep object output-type-spec)
         object)
        ((eq type *empty-type*)
         (coerce-error))
        ((csubtypep type (specifier-type 'character))
         (character object))
        ((numberp object)
         (cond
           ((csubtypep type (specifier-type 'single-float))
            (let ((res (%single-float object)))
              (unless (typep res output-type-spec)
                (coerce-error))
              res))
           ((csubtypep type (specifier-type 'double-float))
            (let ((res (%double-float object)))
              (unless (typep res output-type-spec)
                (coerce-error))
              res))
           #!+long-float
           ((csubtypep type (specifier-type 'long-float))
            (let ((res (%long-float object)))
              (unless (typep res output-type-spec)
                (coerce-error))
              res))
           ((csubtypep type (specifier-type 'float))
            (let ((res (%single-float object)))
              (unless (typep res output-type-spec)
                (coerce-error))
              res))
           (t
            (let ((res
                   (cond
                     ((csubtypep type (specifier-type '(complex single-float)))
                      (complex (%single-float (realpart object))
                               (%single-float (imagpart object))))
                     ((csubtypep type (specifier-type '(complex double-float)))
                      (complex (%double-float (realpart object))
                               (%double-float (imagpart object))))
                     #!+long-float
                     ((csubtypep type (specifier-type '(complex long-float)))
                      (complex (%long-float (realpart object))
                               (%long-float (imagpart object))))
                     ((csubtypep type (specifier-type '(complex float)))
                      (complex (%single-float (realpart object))
                               (%single-float (imagpart object))))
                     ((and (typep object 'rational)
                           (csubtypep type (specifier-type '(complex float))))
                      ;; Perhaps somewhat surprisingly, ANSI specifies
                      ;; that (COERCE FOO 'FLOAT) is a SINGLE-FLOAT,
                      ;; not dispatching on
                      ;; *READ-DEFAULT-FLOAT-FORMAT*.  By analogy, we
                      ;; do the same for complex numbers. -- CSR,
                      ;; 2002-08-06
                      (complex (%single-float object)))
                     ((csubtypep type (specifier-type 'complex))
                      (complex object))
                     (t
                      (coerce-error)))))
              ;; If RES has the wrong type, that means that rule of
              ;; canonical representation for complex rationals was
              ;; invoked. According to the Hyperspec, (coerce 7/2
              ;; 'complex) returns 7/2. Thus, if the object was a
              ;; rational, there is no error here.
              (unless (or (typep res output-type-spec)
                          (rationalp object))
                (coerce-error))
              res))))
        ((csubtypep type (specifier-type 'list))
         (if (vectorp object)
             (cond
               ((type= type (specifier-type 'list))
                (vector-to-list* object))
               ((type= type (specifier-type 'null))
                (if (= (length object) 0)
                    'nil
                    (sequence-type-length-mismatch-error type
                                                         (length object))))
               ((cons-type-p type)
                (multiple-value-bind (min exactp)
                    (sb!kernel::cons-type-length-info type)
                  (let ((length (length object)))
                    (if exactp
                        (unless (= length min)
                          (sequence-type-length-mismatch-error type length))
                        (unless (>= length min)
                          (sequence-type-length-mismatch-error type length)))
                    (vector-to-list* object))))
               (t (sequence-type-too-hairy (type-specifier type))))
             (if (sequencep object)
                 (cond
                   ((type= type (specifier-type 'list))
                    (sb!sequence:make-sequence-like
                     nil (length object) :initial-contents object))
                   ((type= type (specifier-type 'null))
                    (if (= (length object) 0)
                        'nil
                        (sequence-type-length-mismatch-error type
                                                             (length object))))
                   ((cons-type-p type)
                    (multiple-value-bind (min exactp)
                        (sb!kernel::cons-type-length-info type)
                      (let ((length (length object)))
                        (if exactp
                            (unless (= length min)
                              (sequence-type-length-mismatch-error type length))
                            (unless (>= length min)
                              (sequence-type-length-mismatch-error type length)))
                        (sb!sequence:make-sequence-like
                         nil length :initial-contents object))))
                   (t (sequence-type-too-hairy (type-specifier type))))
                 (coerce-error))))
        ((csubtypep type (specifier-type 'vector))
         (typecase object
           ;; FOO-TO-VECTOR* go through MAKE-SEQUENCE, so length
           ;; errors are caught there. -- CSR, 2002-10-18
           (list (list-to-vector* object output-type-spec))
           (vector (vector-to-vector* object output-type-spec))
           (sequence (sequence-to-vector* object output-type-spec))
           (t
            (coerce-error))))
        ((and (csubtypep type (specifier-type 'sequence))
              (find-class output-type-spec nil))
         (let ((class (find-class output-type-spec)))
           (sb!sequence:make-sequence-like
            (sb!mop:class-prototype class)
            (length object) :initial-contents object)))
        ((csubtypep type (specifier-type 'function))
         (when (and (legal-fun-name-p object)
                    (not (fboundp object)))
           (error 'simple-type-error
                  :datum object
                  ;; FIXME: SATISFIES FBOUNDP is a kinda bizarre broken
                  ;; type specifier, since the set of values it describes
                  ;; isn't in general constant in time. Maybe we could
                  ;; find a better way of expressing this error? (Maybe
                  ;; with the UNDEFINED-FUNCTION condition?)
                  :expected-type '(satisfies fboundp)
               :format-control "~S isn't fbound."
               :format-arguments (list object)))
         (when (and (symbolp object)
                    (sb!xc:macro-function object))
           (error 'simple-type-error
                  :datum object
                  :expected-type '(not (satisfies sb!xc:macro-function))
                  :format-control "~S is a macro."
                  :format-arguments (list object)))
         (when (and (symbolp object)
                    (special-operator-p object))
           (error 'simple-type-error
                  :datum object
                  :expected-type '(not (satisfies special-operator-p))
                  :format-control "~S is a special operator."
                  :format-arguments (list object)))
         (eval `#',object))
        (t
         (coerce-error))))))

;;; new version, which seems as though it should be better, but which
;;; does not yet work
#+nil
(defun coerce (object output-type-spec)
  #!+sb-doc
  "Coerces the Object to an object of type Output-Type-Spec."
  (flet ((coerce-error ()
           (error 'simple-type-error
                  :format-control "~S can't be converted to type ~S."
                  :format-arguments (list object output-type-spec)))
         (check-result (result)
           #!+high-security (aver (typep result output-type-spec))
           result))
    (let ((type (specifier-type output-type-spec)))
      (cond
        ((%typep object output-type-spec)
         object)
        ((eq type *empty-type*)
         (coerce-error))
        ((csubtypep type (specifier-type 'character))
         (character object))
        ((csubtypep type (specifier-type 'function))
         (coerce-to-fun object))
        ((numberp object)
         (let ((res
                (cond
                  ((csubtypep type (specifier-type 'single-float))
                   (%single-float object))
                  ((csubtypep type (specifier-type 'double-float))
                   (%double-float object))
                  #!+long-float
                  ((csubtypep type (specifier-type 'long-float))
                   (%long-float object))
                  ((csubtypep type (specifier-type 'float))
                   (%single-float object))
                  ((csubtypep type (specifier-type '(complex single-float)))
                   (complex (%single-float (realpart object))
                            (%single-float (imagpart object))))
                  ((csubtypep type (specifier-type '(complex double-float)))
                   (complex (%double-float (realpart object))
                            (%double-float (imagpart object))))
                  #!+long-float
                  ((csubtypep type (specifier-type '(complex long-float)))
                   (complex (%long-float (realpart object))
                            (%long-float (imagpart object))))
                  ((csubtypep type (specifier-type 'complex))
                   (complex object))
                  (t
                   (coerce-error)))))
           ;; If RES has the wrong type, that means that rule of
           ;; canonical representation for complex rationals was
           ;; invoked. According to the ANSI spec, (COERCE 7/2
           ;; 'COMPLEX) returns 7/2. Thus, if the object was a
           ;; rational, there is no error here.
           (unless (or (typep res output-type-spec) (rationalp object))
             (coerce-error))
           res))
        ((csubtypep type (specifier-type 'list))
         (coerce-to-list object))
        ((csubtypep type (specifier-type 'string))
         (check-result (coerce-to-simple-string object)))
        ((csubtypep type (specifier-type 'bit-vector))
         (check-result (coerce-to-bit-vector object)))
        ((csubtypep type (specifier-type 'vector))
         (check-result (coerce-to-vector object output-type-spec)))
        (t
         (coerce-error))))))
