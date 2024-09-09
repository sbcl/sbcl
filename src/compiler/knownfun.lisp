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

(in-package "SB-C")

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
  ;; returns a lambda expression, or THROWs out.
  (function (missing-arg) :type function)
  ;; T if we should emit a failure note even if SPEED=INHIBIT-WARNINGS.
  (important nil :type (member nil :slightly t))
  ;; A function with NODE as an argument that checks wheteher the
  ;; transform applies in its policy.
  ;; It used to be checked in the FUNCTION body but it would produce
  ;; notes about failed transformation due to types even though it
  ;; wouldn't have been applied with the right types anyway,
  ;; or if another transform could be applied with the right policy.
  (policy nil :type (or null function)))

;;; A transform inserted at the front of fun-info-transforms and stops
;;; other from firing if it has a VOP that can do the job.
(defstruct (vop-transform (:copier nil)
                          (:predicate nil)
                          (:include transform)))

(defun transform-note (transform)
  (or #+sb-xc-host (documentation (transform-function transform) 'function)
      #-sb-xc-host (and (fboundp 'sb-pcl::fun-doc)
                        (funcall 'sb-pcl::fun-doc (transform-function transform)))
      "optimize"))

(defmethod print-object ((x transform) stream)
  (print-unreadable-object (x stream :type t :identity t)
    (princ (type-specifier (transform-type x)) stream)))

;;; Grab the FUN-INFO and enter the function, replacing any old
;;; one with the same type and note.
;;; Argument order is: policy constraint, ftype constraint, consequent.
;;; (think "qualifiers + specializers -> method")
(defun %deftransform (name policy type fun &optional (important :slightly))
  (let* ((ctype (specifier-type type))
         (info (fun-info-or-lose name))
         (transforms (fun-info-transforms info))
         (old (find-if (lambda (transform)
                         (and (if (eq important :vop)
                                  (typep transform 'vop-transform)
                                  (not (typep transform 'vop-transform)))
                              (type= (transform-type transform)
                                     ctype)))
                       transforms)))
    (cond (old
           (style-warn 'redefinition-with-deftransform :transform old)
           (setf (transform-function old) fun
                 (transform-policy old) policy)
           (unless (eq important :vop)
             (setf (transform-important old) important)))
          (t
           ;; Put vop-transform at the front.
           (if (eq important :vop)
               (push (make-vop-transform :type ctype :function fun
                                         :policy policy)
                     (fun-info-transforms info))
               (let ((normal (member-if (lambda (transform)
                                          (not (typep transform 'vop-transform)))
                                        transforms))
                     (transform (make-transform :type ctype :function fun
                                                :important important
                                                :policy policy)))
                 (setf (fun-info-transforms info)
                       (append (ldiff transforms normal) (list* transform normal)))))))
    name))

;;; Make a FUN-INFO structure with the specified type, attributes
;;; and optimizers.
(defun %defknown (names type attributes location
                  &key derive-type optimizer result-arg
                       overwrite-fndb-silently
                       call-type-deriver
                       annotation
                       folder)
  (let* ((ctype (specifier-type type))
         (type-to-store (if (contains-unknown-type-p ctype)
                            ;; unparse it, so SFUNCTION -> FUNCTION
                            (type-specifier ctype)
                            ctype)))
    (dolist (name names)
      (let ((old-fun-info (info :function :info name))
            inherit)
        (block ignore
          (unless overwrite-fndb-silently
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
              (restart-case
                  (cerror "Go ahead, overwrite it."
                          "~@<overwriting old FUN-INFO ~2I~_~S ~I~_for ~S~:>"
                          old-fun-info name)
                (continue ()
                  :report "Inherit templates and optimizers"
                  (setf inherit t))
                (ignore ()
                  (return-from ignore)))))
          (setf (info :function :type name) type-to-store)
          (setf (info :function :where-from name) :declared)
          (setf (info :function :kind name) :function)
          (cond (inherit
                 (when optimizer
                   (setf (fun-info-optimizer old-fun-info) optimizer))
                 (when derive-type
                   (setf (fun-info-derive-type old-fun-info) derive-type))
                 (setf (fun-info-attributes old-fun-info) attributes
                       (fun-info-result-arg old-fun-info) result-arg
                       (fun-info-annotation old-fun-info) annotation
                       (fun-info-call-type-deriver old-fun-info) call-type-deriver
                       (fun-info-folder old-fun-info) folder))
                (t
                 (setf (info :function :info name)
                       (make-fun-info :attributes attributes
                                      :derive-type derive-type
                                      :optimizer optimizer
                                      :result-arg result-arg
                                      :call-type-deriver call-type-deriver
                                      :annotation annotation
                                      :folder folder))))
          (if location
              (setf (getf (info :source-location :declaration name) 'defknown)
                    location)
              (remf (info :source-location :declaration name) 'defknown))))))
  names)


;;; This macro should be the way that all implementation independent
;;; information about functions is made known to the compiler.
;;;
;;; FIXME: The comment above suggests that perhaps some of my added
;;; FTYPE declarations are in poor taste. Should I change my
;;; declarations, or change the comment, or what?
;;;
;;; FIXME: DEFKNOWN is needed only at build-the-system time. Figure
;;; out some way to keep it from appearing in the target system.
;;;
;;; Declare the function NAME to be a known function. We construct a
;;; type specifier for the function by wrapping (FUNCTION ...) around
;;; the ARG-TYPES and RESULT-TYPE. ATTRIBUTES is an unevaluated list
;;; of boolean attributes of the function. See their description in
;;; (!DEF-BOOLEAN-ATTRIBUTE IR1). NAME may also be a list of names, in
;;; which case the same information is given to all the names. The
;;; keywords specify the initial values for various optimizers that
;;; the function might have.
(defmacro defknown (name arg-types result-type &optional (attributes '(any))
                    &body keys)
  #-sb-xc-host
  (when (member 'unsafe attributes)
    (style-warn "Ignoring legacy attribute UNSAFE. Replaced by its inverse: DX-SAFE.")
    (setf attributes (remove 'unsafe attributes)))
  (when (and (intersection attributes '(any call unwind))
             (intersection attributes '(movable)))
    (error "function cannot have both good and bad attributes: ~S" attributes))

  (when (member 'any attributes)
    (setq attributes (union '(unwind) attributes)))
  (when (member 'flushable attributes)
    (pushnew 'unsafely-flushable attributes))
  ;; Needs to be supported by the call VOPs
  #-(or arm64 x86-64)
  (setf attributes (remove 'no-verify-arg-count attributes))
  #-(or arm64 x86-64)
  (setf attributes (remove 'unboxed-return attributes))
  #-(or arm64 x86-64) ;; Needs to be supported by the call VOPs, sb-vm::fixed-call-arg-location
  (setf attributes (remove 'fixed-args attributes))
  (when (or (memq 'fixed-args attributes)
            (memq 'unboxed-return attributes))
    (pushnew 'no-verify-arg-count attributes))

  (multiple-value-bind (type annotation)
      (split-type-info arg-types result-type)
    `(%defknown ',(if (and (consp name)
                           (not (legal-fun-name-p name)))
                      name
                      (list name))
                ',type
                (ir1-attributes ,@attributes)
                (source-location)
                :annotation ,annotation
                ,@keys
                :folder ,(and (memq 'foldable attributes)
                              (not (getf keys :folder))
                              (or
                               (memq 'fixed-args attributes)
                               (memq 'unboxed-return attributes))
                              (let ((args (make-gensym-list (length arg-types))))
                                `(lambda ,args (funcall ',name ,@args)))))))

(defstruct (fun-type-annotation
            (:copier nil))
  positional ;; required and &optional
  rest
  key
  returns)

(defun split-type-info (arg-types result-type)
  (if (eq arg-types '*)
      `(sfunction ,arg-types ,result-type)
      (multiple-value-bind (llks required optional rest keys)
          (parse-lambda-list
           arg-types
           :context :function-type
           :accept (lambda-list-keyword-mask
                    '(&optional &rest &key &allow-other-keys))
           :silent t)
        (let ((i -1)
              positional-annotation
              rest-annotation
              key-annotation
              return-annotation)
          (labels ((annotation-p (x)
                     (typep x '(or (cons (member function function-designator modifying
                                          inhibit-flushing))
                                (member type-specifier proper-sequence proper-list
                                 proper-or-dotted-list proper-or-circular-list))))
                   (strip-annotation (x)
                     (if (consp x)
                         (ecase (car x)
                           ((function function-designator) (car x))
                           ((modifying inhibit-flushing) (cadr x)))
                         (case x
                           (proper-sequence 'sequence)
                           ((proper-list proper-or-dotted-list proper-or-circular-list) 'list)
                           (t x))))
                   (process-positional (type)
                     (incf i)
                     (cond ((annotation-p type)
                            (push (cons i (ensure-list type)) positional-annotation)
                            (strip-annotation type))
                           (t
                            type)))
                   (process-key (pair)
                     (cond ((annotation-p (cadr pair))
                            (destructuring-bind (key value) pair
                              (setf (getf key-annotation key) (ensure-list value))
                              (list key (strip-annotation value))))
                           (t
                            pair)))
                   (process-rest (type)
                     (cond ((annotation-p type)
                            (setf rest-annotation (ensure-list type))
                            (strip-annotation type))
                           (t
                            type)))
                   (process-return (type)
                     (cond ((annotation-p type)
                            (setf return-annotation (ensure-list type))
                            (strip-annotation type))
                           (t
                            type))))
            (let ((required (mapcar #'process-positional required))
                  (optional (mapcar #'process-positional optional))
                  (rest (process-rest (car rest)))
                  (key (mapcar #'process-key keys))
                  (return (process-return result-type)))
              (values
               `(sfunction
                 (,@required
                  ,@(and optional `(&optional ,@optional))
                  ,@(and (ll-kwds-restp llks) `(&rest ,rest))
                  ,@(and (ll-kwds-keyp llks) `(&key ,@key))
                  ,@(and (ll-kwds-allowp llks) '(&allow-other-keys)))
                 ,return)
               (when (or positional-annotation rest-annotation
                         key-annotation return-annotation)
                 `(make-fun-type-annotation :positional ',positional-annotation
                                            :rest ',rest-annotation
                                            :key ',key-annotation
                                            :returns ',return-annotation)))))))))

;;; Return the FUN-INFO for NAME or die trying.
(declaim (ftype (sfunction (t) fun-info) fun-info-or-lose))
(defun fun-info-or-lose (name)
  (or (info :function :info name) (error "~S is not a known function." name)))

;;;; generic type inference methods

(defun maybe-find-free-var (name)
  (let ((found (gethash name (free-vars *ir1-namespace*))))
    (unless (eq found :deprecated)
      found)))

(defun symbol-value-derive-type (node &aux (args (basic-combination-args node))
                                      (lvar (pop args)))
  (unless (and lvar (endp args))
    (return-from symbol-value-derive-type))
  (if (constant-lvar-p lvar)
      (let* ((sym (lvar-value lvar))
             (var (maybe-find-free-var sym))
             (local-type (when var
                           (lexenv-find var type-restrictions :lexenv (node-lexenv node))))
             (global-type (info :variable :type sym)))
        (if local-type
            (type-intersection local-type global-type)
            global-type))
      *universal-type*))

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

(defun result-type-nth-arg (n)
  (lambda (call)
    (let ((lvar (nth n (combination-args call))))
      (when lvar (lvar-type lvar)))))

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
                                       preserve-vector-type
                                       string-designator)
  (lambda (call)
    (declare (type combination call))
    (let ((lvar (nth n (combination-args call))))
      (when lvar
        (let* ((type (lvar-type lvar))
               (result type))
          (unless string-designator
            (let ((list-type (type-intersection type (specifier-type 'list))))
              (unless (eq list-type *empty-type*)
                (setf result
                      (type-union
                       result
                       (simplify-list-type list-type
                                           :preserve-dimensions preserve-dimensions))))))
          (when string-designator
            (when (types-equal-or-intersect type (specifier-type 'character))
              (setf result
                    (type-union
                     (type-difference result (specifier-type 'character))
                     (specifier-type '(simple-string 1)))))
            (let ((symbol-type (type-intersection type (specifier-type 'symbol))))
              (unless (eq symbol-type *empty-type*)
                (setf result
                      (type-union
                       (type-difference result (specifier-type 'symbol))
                       (if (member-type-p symbol-type)
                           (sb-kernel::%type-union (mapcar-member-type-members
                                                    (lambda (s) (ctype-of (symbol-name s)))
                                                    symbol-type))
                           (specifier-type 'simple-string)))))))
          (unless preserve-vector-type
            (let ((vector-type (type-intersection type (specifier-type 'vector))))
              (unless (eq vector-type *empty-type*)
                (let ((simplified (simplify-vector-type vector-type)))
                  (setf result
                        (type-union
                         result
                         (if (and preserve-dimensions
                                  (csubtypep simplified (specifier-type 'simple-array)))
                             (type-intersection (specifier-type
                                                 `(simple-array * ,(ctype-array-dimensions vector-type)))
                                                simplified)
                             simplified)))))))
          result)))))

;;; Derive the type to be the type specifier which is the Nth arg.
(defun result-type-specifier-nth-arg (n)
  (lambda (call)
    (declare (type combination call))
    (let ((lvar (nth n (combination-args call))))
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
    (let ((lvar (nth n (combination-args call))))
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
  (cond ((constant-lvar-p lvar)
         (let ((value (lvar-value lvar)))
           (and (proper-sequence-p value)
                (let ((length (length value)))
                  (values length length)))))
        ((csubtypep (lvar-type lvar) (specifier-type 'cons))
         (values nil 1))
        (t
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
                          (t
                           (cond ((csubtypep type (specifier-type 'cons))
                                  (setf max array-total-size-limit
                                        min (min min 1)))
                                 ((csubtypep type (specifier-type 'null))
                                  (setf min 0))
                                 (t
                                  (return '*))))))
                      (process-dim (dim)
                        (if (typep dim '(cons integer null))
                            (let ((length (car dim)))
                              (setf max (max max length)
                                    min (min min length)))
                            (return '*))))
               ;; If type derivation were able to notice that non-simple arrays can
               ;; be mutated (changing the type), we could safely use LVAR-TYPE on
               ;; any vector type. But it doesn't notice.
               ;; We could use LVAR-CONSERVATIVE-TYPE to get a conservative answer.
               ;; However that's probably not an important use, so the above
               ;; logic restricts itself to simple arrays.
               (max-dim (lvar-type lvar))
               (values max min)))))))

;;; This used to be done in DEFOPTIMIZER DERIVE-TYPE, but
;;; ASSERT-CALL-TYPE already asserts the ARRAY type, so it gets an extra
;;; assertion that may not get eliminated and requires extra work.
(defun array-call-type-deriver (call trusted &optional set row-major-aref)
  (let* ((fun (combination-fun call))
         (type (lvar-fun-type fun))
         (policy (lexenv-policy (node-lexenv call)))
         (args (combination-args call)))
    (when (fun-type-p type)
      (flet ((assert-type (arg type &optional set index)
               (when (cond (index
                            (assert-array-index-lvar-type arg type policy))
                           (t
                            (when set
                              (add-annotation arg
                                              (make-lvar-modified-annotation :caller (lvar-fun-name fun))))
                            (assert-lvar-type arg type policy)))
                 (unless trusted (reoptimize-lvar arg)))))
        (let ((required (fun-type-required type)))
          (when set
            (assert-type (pop args)
                         (pop required)))
          (assert-type (pop args)
                       (if row-major-aref
                           (pop required)
                           (type-intersection
                            (pop required)
                            (let ((rank (length args)))
                              (when (>= rank array-rank-limit)
                                (setf (combination-kind call) :error)
                                (compiler-warn "More subscripts for ~a (~a) than ~a (~a)"
                                               (combination-fun-debug-name call)
                                               rank
                                               'array-rank-limit
                                               array-rank-limit)
                                (return-from array-call-type-deriver))
                              (specifier-type `(array * ,rank)))))
                       set)
          (loop for type in required
                do
                (assert-type (pop args) type nil (or (not (and set row-major-aref))
                                                     args)))
          (loop for type in (fun-type-optional type)
                do (assert-type (pop args) type nil t))
          (loop for subscript in args
                do (assert-type subscript (fun-type-rest type) nil t)))))))

(defun append-call-type-deriver (call trusted)
  (let* ((policy (lexenv-policy (node-lexenv call)))
         (args (combination-args call))
         (list-type (specifier-type 'list)))
    ;; All but the last argument should be proper lists
    (loop for (arg next) on args
          while next
          do
          (add-annotation
           arg
           (make-lvar-proper-sequence-annotation
            :kind 'proper-list))
          (when (and (assert-lvar-type arg list-type policy)
                     (not trusted))
            (reoptimize-lvar arg)))))

(defun nconc-call-type-deriver (call trusted)
  (let* ((policy (lexenv-policy (node-lexenv call)))
         (args (combination-args call))
         (list-type (specifier-type 'list)))
    ;; All but the last argument should be proper lists
    (loop for (arg next) on args
          while next
          do
          (add-annotation
           arg
           (make-lvar-proper-sequence-annotation
            :kind 'proper-or-dotted-list))
          (when (policy policy (> check-constant-modification 0))
            (add-annotation arg
                            (make-lvar-modified-annotation :caller 'nconc)))
          (when (and (assert-lvar-type arg list-type policy)
                     (not trusted))
            (reoptimize-lvar arg)))))

;;; It's either (number) or (real real)
(defun atan-call-type-deriver (call trusted)
  (let* ((policy (lexenv-policy (node-lexenv call)))
         (args (combination-args call)))
    (case (length args)
      (1
       (when (and (assert-lvar-type (car args) (specifier-type 'number) policy)
                  (not trusted))
         (reoptimize-lvar (car args))))
      (2
       (loop for arg in args
             do
             (when (and (assert-lvar-type arg (specifier-type 'real) policy)
                        (not trusted))
               (reoptimize-lvar arg)))))))
