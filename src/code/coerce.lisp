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

(macrolet ((def (name constructor access src-type &optional explicit-check)
             `(defun ,name (object type)
                (declare (type ,src-type object))
                ,@(when explicit-check `((declare (explicit-check))))
                (do* ((index 0 (1+ index))
                      (length (length object))
                      (result ,constructor)
                      (in-object object))
                     ((>= index length) result)
                  (declare (fixnum length index))
                  (declare (type vector result))
                  (setf (,access result index)
                        ,(ecase src-type
                           (list '(pop in-object))
                           (vector '(aref in-object index))
                           (sequence '(elt in-object index))))))))

  (def list-to-vector* (make-sequence type length) aref list t)

  (def vector-to-vector* (make-sequence type length) aref vector t)

  (def sequence-to-vector* (make-sequence type length) aref sequence))

(defun vector-to-list* (object)
  (declare (type vector object))
  (dx-let ((result (list nil)))
    (let ((splice result))
      (do-vector-data (elt object (cdr result))
        (let ((cell (list elt)))
          (setf (cdr splice) cell splice cell))))))

(defun sequence-to-list (sequence)
  (declare (type sequence sequence))
  (dx-let ((result (list nil)))
    (let ((splice result))
      (sb!sequence:dosequence (elt sequence (cdr result))
        (let ((cell (list elt)))
          (setf (cdr splice) cell splice cell))))))

;;; These are used both by the full DEFUN function and by various
;;; optimization transforms in the constant-OUTPUT-TYPE-SPEC case.
;;;
;;; Most of them are INLINE so that they can be optimized when the
;;; argument type is known. It might be better to do this with
;;; DEFTRANSFORMs, though.
(declaim (inline coerce-to-list))
(declaim (inline coerce-to-vector))

(defun coerce-symbol-to-fun (symbol)
  ;; FIXME? I would think to use SYMBOL-FUNCTION here which does not strip off
  ;; encapsulations. But Stas wrote FDEFINITION so ...
  ;; [Also note, we won't encapsulate a macro or special-form, so this
  ;; introspective technique to decide what kind something is works either way]
  (let ((def (fdefinition symbol)))
    (if (macro/special-guard-fun-p def)
        (error (ecase (car (%fun-name def))
                (:macro "~S names a macro.")
                (:special "~S names a special operator."))
               symbol)
        def)))

(defun coerce-to-fun (object)
  ;; (Unlike the other COERCE-TO-FOOs, this one isn't inline, because
  ;; it's so big and because optimizing away the outer ETYPECASE
  ;; doesn't seem to buy us that much anyway.)
  (etypecase object
    (function object)
    (symbol
     (coerce-symbol-to-fun object))
    (list
     (case (first object)
       (setf
        (fdefinition object))
       (lambda
        (eval object))
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
  (seq-dispatch object
                object
                (vector-to-list* object)
                (sequence-to-list object)))

(defun coerce-to-vector (object output-type-spec)
  (etypecase object
    (list (list-to-vector* object output-type-spec))
    (vector (vector-to-vector* object output-type-spec))))

;;; old working version
(defun coerce (object output-type-spec)
  #!+sb-doc
  "Coerce the Object to an object of type Output-Type-Spec."
  (declare (explicit-check))
  (flet ((coerce-error ()
           (error 'simple-type-error
                  :format-control "~S can't be converted to type ~S."
                  :format-arguments (list object output-type-spec)
                  :datum object
                  :expected-type output-type-spec)))
    (let ((type (specifier-type output-type-spec)))
      (cond
        ((%%typep object type)
         object)
        ((eq type *empty-type*)
         (coerce-error))
        ((type= type (specifier-type 'character))
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
                     ((and (typep object 'rational) ; TODO jmoringe unreachable?
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
         (let ((prototype (sb!mop:class-prototype
                           (sb!pcl:ensure-class-finalized
                            (find-class output-type-spec)))))
           (sb!sequence:make-sequence-like
            prototype (length object) :initial-contents object)))
        ((csubtypep type (specifier-type 'function))
         (coerce-to-fun object))
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
        ((%%typep object type)
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
