;;;; This file contains stuff for maintaining a database of special
;;;; information about functions known to the compiler. This includes
;;;; semantic information such as side effects and type inference
;;;; functions as well as transforms and IR2 translators.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

(/show0 "knownfun.lisp 17")

;;;; interfaces to defining macros

;;; an IR1 transform
(defstruct (transform (:copier nil))
  ;; the function type which enables this transform.
  ;;
  ;; (Note that declaring this :TYPE FUN-TYPE probably wouldn't
  ;; work because some function types, like (SPECIFIER-TYPE 'FUNCTION0
  ;; itself, are represented as BUILT-IN-TYPE, and at least as of
  ;; sbcl-0.pre7.54 or so, that's inconsistent with being a
  ;; FUN-TYPE.)
  (type (missing-arg) :type ctype)
  ;; the transformation function. Takes the COMBINATION node and
  ;; returns a lambda expression, or throws out.
  (function (missing-arg) :type function)
  ;; string used in efficiency notes
  (note (missing-arg) :type string)
  ;; T if we should emit a failure note even if SPEED=INHIBIT-WARNINGS.
  (important nil :type (member nil :slightly t)))

(defprinter (transform) type note important)

;;; Grab the FUN-INFO and enter the function, replacing any old
;;; one with the same type and note.
(declaim (ftype (function (t list function &optional (or string null)
                             (member nil :slightly t))
                          *)
                %deftransform))
(defun %deftransform (name type fun &optional note important)
  (let* ((ctype (specifier-type type))
         (note (or note "optimize"))
         (info (fun-info-or-lose name))
         (old (find-if (lambda (x)
                         (and (type= (transform-type x) ctype)
                              (string-equal (transform-note x) note)
                              (eq (transform-important x) important)))
                       (fun-info-transforms info))))
    (cond (old
           (style-warn 'redefinition-with-deftransform
                       :transform old)
           (setf (transform-function old) fun
                 (transform-note old) note))
          (t
           (push (make-transform :type ctype :function fun :note note
                                 :important important)
                 (fun-info-transforms info))))
    name))

;;; Make a FUN-INFO structure with the specified type, attributes
;;; and optimizers.
(declaim (ftype (function (list list attributes t &key
                                (:derive-type (or function null))
                                (:optimizer (or function null))
                                (:destroyed-constant-args (or function null))
                                (:result-arg (or index null))
                                (:overwrite-fndb-silently boolean)
                                (:foldable-call-check (or function null))
                                (:callable-check (or function null))
                                (:call-type-deriver (or function null)))
                          *)
                %defknown))
(defun %defknown (names type attributes location
                  &key derive-type optimizer destroyed-constant-args result-arg
                       overwrite-fndb-silently
                       foldable-call-check
                       callable-check
                       call-type-deriver)
  (let ((ctype (specifier-type type)))
    (dolist (name names)
      (unless overwrite-fndb-silently
        (let ((old-fun-info (info :function :info name)))
          (when old-fun-info
            ;; This is handled as an error because it's generally a bad
            ;; thing to blow away all the old optimization stuff. It's
            ;; also a potential source of sneaky bugs:
            ;;    DEFKNOWN FOO
            ;;    DEFTRANSFORM FOO
            ;;    DEFKNOWN FOO ; possibly hidden inside some macroexpansion
            ;;    ; Now the DEFTRANSFORM doesn't exist in the target Lisp.
            ;; However, it's continuable because it might be useful to do
            ;; it when testing new optimization stuff interactively.
            (cerror "Go ahead, overwrite it."
                    "~@<overwriting old FUN-INFO ~2I~_~S ~I~_for ~S~:>"
                    old-fun-info name))))
      (setf (info :function :type name) ctype)
      (setf (info :function :where-from name) :declared)
      (setf (info :function :kind name) :function)
      (setf (info :function :info name)
            (make-fun-info :attributes attributes
                           :derive-type derive-type
                           :optimizer optimizer
                           :destroyed-constant-args destroyed-constant-args
                           :result-arg result-arg
                           :foldable-call-check foldable-call-check
                           :callable-check callable-check
                           :call-type-deriver call-type-deriver))
      (if location
          (setf (getf (info :source-location :declaration name) 'defknown)
                location)
          (remf (info :source-location :declaration name) 'defknown))))
  names)

;;; Return the FUN-INFO for NAME or die trying.
(declaim (ftype (sfunction (t) fun-info) fun-info-or-lose))
(defun fun-info-or-lose (name)
  (or (info :function :info name) (error "~S is not a known function." name)))

;;;; generic type inference methods

;;; Derive the type to be the type of the xxx'th arg. This can normally
;;; only be done when the result value is that argument.
(defun result-type-first-arg (call)
  (declare (type combination call))
  (let ((lvar (first (combination-args call))))
    (when lvar (lvar-type lvar))))
(defun result-type-last-arg (call)
  (declare (type combination call))
  (let ((lvar (car (last (combination-args call)))))
    (when lvar (lvar-type lvar))))

;;; Derive the result type according to the float contagion rules, but
;;; always return a float. This is used for irrational functions that
;;; preserve realness of their arguments.
(defun result-type-float-contagion (call)
  (declare (type combination call))
  (reduce #'numeric-contagion (combination-args call)
          :key #'lvar-type
          :initial-value (specifier-type 'single-float)))

(defun simplify-list-type (type &key preserve-dimensions)
  ;; Preserve all the list types without dragging
  ;; (cons (eql 10)) stuff in.
  (let ((cons-type (specifier-type 'cons))
        (list-type (specifier-type 'list))
        (null-type (specifier-type 'null)))
    (cond ((and preserve-dimensions
                (csubtypep type cons-type))
           cons-type)
          ((and preserve-dimensions
                (csubtypep type null-type))
           null-type)
          ((csubtypep type list-type)
           list-type))))

;;; Return a closure usable as a derive-type method for accessing the
;;; N'th argument. If arg is a list, result is a list. If arg is a
;;; vector, result is a vector with the same element type.
(defun sequence-result-nth-arg (n &key preserve-dimensions
                                       preserve-vector-type)
  (lambda (call)
    (declare (type combination call))
    (let ((lvar (nth (1- n) (combination-args call))))
      (when lvar
        (let ((type (lvar-type lvar)))
          (cond ((simplify-list-type type
                                     :preserve-dimensions preserve-dimensions))
                ((not (csubtypep type (specifier-type 'vector)))
                 nil)
                (preserve-vector-type
                 type)
                (t
                 (let ((simplified (simplify-vector-type type)))
                   (if (and preserve-dimensions
                            (csubtypep simplified (specifier-type 'simple-array)))
                       (type-intersection (specifier-type
                                           `(simple-array * ,(ctype-array-dimensions type)))
                                          simplified)
                       simplified)))))))))

;;; Derive the type to be the type specifier which is the Nth arg.
(defun result-type-specifier-nth-arg (n)
  (lambda (call)
    (declare (type combination call))
    (let ((lvar (nth (1- n) (combination-args call))))
      (when (and lvar (constant-lvar-p lvar))
        (careful-specifier-type (lvar-value lvar))))))

;;; Derive the type to be the type specifier which is the Nth arg,
;;; with the additional restriptions noted in the CLHS for STRING and
;;; SIMPLE-STRING, defined to specialize on CHARACTER, and for VECTOR
;;; (under the page for MAKE-SEQUENCE).
;;; At present this is used to derive the output type of CONCATENATE,
;;; MAKE-SEQUENCE, and MERGE. Two things seem slightly amiss:
;;; 1. The sequence type actually produced might not be exactly that specified.
;;;    (TYPE-OF (MAKE-SEQUENCE '(AND (NOT SIMPLE-ARRAY) (VECTOR BIT)) 9))
;;;    => (SIMPLE-BIT-VECTOR 9)
;;; 2. Because we *know* that a hairy array won't be produced,
;;;    why does derivation preserve the non-simpleness, if so specified?
(defun creation-result-type-specifier-nth-arg (n)
  (lambda (call)
    (declare (type combination call))
    (let ((lvar (nth (1- n) (combination-args call))))
      (when (and lvar (constant-lvar-p lvar))
        (let* ((specifier (lvar-value lvar))
               (lspecifier (if (atom specifier) (list specifier) specifier)))
          (cond
            ((eq (car lspecifier) 'string)
             (destructuring-bind (string &rest size)
                 lspecifier
               (declare (ignore string))
               (careful-specifier-type
                `(vector character ,@(when size size)))))
            ((eq (car lspecifier) 'simple-string)
             (destructuring-bind (simple-string &rest size)
                 lspecifier
               (declare (ignore simple-string))
               (careful-specifier-type
                `(simple-array character ,@(if size (list size) '((*)))))))
            (t
             (let ((ctype (careful-specifier-type specifier)))
               (cond ((not (array-type-p ctype))
                      ctype)
                     ((unknown-type-p (array-type-element-type ctype))
                      (make-array-type (array-type-dimensions ctype)
                                       :complexp (array-type-complexp ctype)
                                       :element-type *wild-type*
                                       :specialized-element-type *wild-type*))
                     ((eq (array-type-specialized-element-type ctype)
                          *wild-type*)
                      (make-array-type (array-type-dimensions ctype)
                                       :complexp (array-type-complexp ctype)
                                       :element-type *universal-type*
                                       :specialized-element-type *universal-type*))
                     (t
                      ctype))))))))))

(defun remove-non-constants-and-nils (fun)
  (lambda (list)
    (remove-if-not #'lvar-value
                   (remove-if-not #'constant-lvar-p (funcall fun list)))))

;;; FIXME: bad name (first because it uses 1-based indexing; second
;;; because it doesn't get the nth constant arguments)
(defun nth-constant-args (&rest indices)
  (lambda (list)
    (let (result)
      (do ((i 1 (1+ i))
           (list list (cdr list))
           (indices indices))
          ((null indices) (nreverse result))
        (when (= i (car indices))
          (when (constant-lvar-p (car list))
            (push (car list) result))
          (setf indices (cdr indices)))))))

;;; FIXME: a number of the sequence functions not only do not destroy
;;; their argument if it is empty, but also leave it alone if :start
;;; and :end bound a null sequence, or if :count is 0.  This test is a
;;; bit complicated to implement, verging on the impossible, but for
;;; extra points (fill #\1 "abc" :start 0 :end 0) should not cause a
;;; warning.
(defun nth-constant-nonempty-sequence-args (&rest indices)
  (lambda (list)
    (let (result)
      (do ((i 1 (1+ i))
           (list list (cdr list))
           (indices indices))
          ((null indices) (nreverse result))
        (when (= i (car indices))
          (when (constant-lvar-p (car list))
            (let ((value (lvar-value (car list))))
              (unless (or (typep value 'null)
                          (typep value '(vector * 0)))
                (push (car list) result))))
          (setf indices (cdr indices)))))))

(defun read-elt-type-deriver (skip-arg-p element-type-spec no-hang)
  (lambda (call)
    (let* ((element-type (specifier-type element-type-spec))
           (null-type (specifier-type 'null))
           (err-args (if skip-arg-p ; for PEEK-CHAR, skip 'peek-type' + 'stream'
                         (cddr (combination-args call))
                         (cdr (combination-args call)))) ; else just 'stream'
           (eof-error-p (first err-args))
           (eof-value (second err-args))
           (unexceptional-type ; the normally returned thing
            (if (and eof-error-p
                     (types-equal-or-intersect (lvar-type eof-error-p)
                                               null-type))
                ;; (READ-elt stream nil <x>) returns (OR (EQL <x>) elt-type)
                (type-union (if eof-value (lvar-type eof-value) null-type)
                            element-type)
                ;; If eof-error is unsupplied, or was but couldn't be nil
                element-type)))
      (if no-hang
          (type-union unexceptional-type null-type)
          unexceptional-type))))

;;; Return MAX MIN
(defun sequence-lvar-dimensions (lvar)
  (if (not (constant-lvar-p lvar))
      (let ((max 0) (min array-total-size-limit))
        (block nil
          (labels ((max-dim (type)
                     ;; This can deal with just enough hair to handle type STRING,
                     ;; but might be made to use GENERIC-ABSTRACT-TYPE-FUNCTION
                     ;; if we really want to be more clever.
                     (typecase type
                       (union-type
                        (mapc #'max-dim (union-type-types type)))
                       (array-type (if (array-type-complexp type)
                                       (return '*)
                                       (process-dim (array-type-dimensions type))))
                       (t (return '*))))
                   (process-dim (dim)
                     (let ((length (car dim)))
                       (if (and (singleton-p dim)
                                (integerp length))
                           (setf max (max max length)
                                 min (min min length))
                           (return '*)))))
            ;; If type derivation were able to notice that non-simple arrays can
            ;; be mutated (changing the type), we could safely use LVAR-TYPE on
            ;; any vector type. But it doesn't notice.
            ;; We could use LVAR-CONSERVATIVE-TYPE to get a conservative answer.
            ;; However that's probably not an important use, so the above
            ;; logic restricts itself to simple arrays.
            (max-dim (lvar-type lvar))
            (values max min))))
      (let ((value (lvar-value lvar)))
        (and (typep value 'sequence)
             (let ((length (length value)))
               (values length length))))))

(defun position-derive-type (call)
  (let ((dim (sequence-lvar-dimensions (second (combination-args call)))))
    (when (integerp dim)
      (specifier-type `(or (integer 0 (,dim)) null)))))

(defun count-derive-type (call)
  (let ((dim (sequence-lvar-dimensions (second (combination-args call)))))
    (when (integerp dim)
      (specifier-type `(integer 0 ,dim)))))

;;; This used to be done in DEFOPTIMIZER DERIVE-TYPE, but
;;; ASSERT-CALL-TYPE already asserts the ARRAY type, so it gets an extra
;;; assertion that may not get eliminated and requires extra work.
(defun array-call-type-deriver (call trusted)
  (let ((type (lvar-type (combination-fun call)))
        (policy (lexenv-policy (node-lexenv call)))
        (args (combination-args call)))
    (flet ((assert-type (arg type)
             (when (assert-lvar-type arg type policy)
               (unless trusted (reoptimize-lvar arg)))))
      (loop for (type . next) on (fun-type-required type)
            while next
            do (assert-type (pop args) type))
      (assert-type (pop args)
                   (specifier-type `(array * ,(make-list (length args)
                                                         :initial-element '*))))
      (loop for subscript in args
            do (assert-type subscript (fun-type-rest type))))))

(/show0 "knownfun.lisp end of file")
