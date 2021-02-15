;;;; implementation of CL:DOCUMENTATION

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is in the public domain and is provided with absolutely no
;;;; warranty. See the COPYING and CREDITS files for more information.

(in-package "SB-PCL")

(defun fun-doc (function)
  (typecase function
    (interpreted-function
     #+sb-fasteval (sb-interpreter:proto-fn-docstring (sb-interpreter:fun-proto-fn function))
     #+sb-eval (sb-eval:interpreted-function-documentation function))
    (generic-function
     (slot-value function '%documentation))
    (t
     (when (closurep function)
       (let ((val (nth-value sb-impl::+closure-doc-index+
                             (closure-extra-values function))))
         (unless (unbound-marker-p val)
           (return-from fun-doc val))))
     (%simple-fun-doc (%fun-fun function)))))

(defun (setf fun-doc) (new-value function)
  (declare (type (or null string) new-value))
  (typecase function
    (interpreted-function
     #+sb-fasteval
     (setf (sb-interpreter:proto-fn-docstring (sb-interpreter:fun-proto-fn function))
           new-value)
     #+sb-eval
     (setf (sb-eval:interpreted-function-documentation function) new-value))
    (generic-function
     (setf (slot-value function '%documentation) new-value))
    (closure
     (set-closure-extra-values
      function nil
      (pack-closure-extra-values
       (nth-value +closure-name-index+ (closure-extra-values function))
       new-value)))
    (simple-fun
     ;; Don't allow PCL CTORs and other random functions through
     ;; because we don't want to affect builtin docstrings.
     (setf (%simple-fun-doc function) new-value)))
  new-value)

;;; (SETF %DOC-INFO) is a thin wrapper on INFO that set or clears
;;; a :DOCUMENTATION info value depending on whether STRING is NIL.
;;; It, and the corresponding reader, are not for use outside this file.
(defun (setf %doc-info) (string name doc-type)
  (declare (type (or null string) string))
  (let ((info-number
         (macrolet ((info-number (class type)
                      (meta-info-number (meta-info class type))))
           (case doc-type
             (variable (info-number :variable :documentation))
             (structure
              (cond ((eq (info :type :kind name) :instance)
                     (info-number :type :documentation))
                    ((info :typed-structure :info name)
                     (info-number :typed-structure :documentation))))
             (type (info-number :type :documentation))))))
    (cond (info-number
           (if string
               (set-info-value name info-number string)
               (clear-info-values name (list info-number))))
          ((eq doc-type 'function)
           ;; FIXME: this silently loses
           ;; * (setf (documentation '(a bad name) 'function) "x") => "x"
           ;; * (documentation '(a bad name) 'function) => NIL
           ;; which is fine because as noted in pcl/documentation.lsp
           ;;   even for supported doc types an implementation is permitted
           ;;   to discard docs at any time
           ;; but should a warning be issued just as for an unknown DOC-TYPE?
           ;;
           ;; And there's additional weirdness if you do, in this order -
           ;;  * (setf (documentation 'foo 'function) "hi")
           ;;  * (defun foo () "hey" 1)
           ;;  * (documentation 'foo 'function) => "hi" ; should be "hey"
           ;; CLHS says regarding DEFUN:
           ;; " Documentation is attached as a documentation string to
           ;;   /name/ (as kind function) and to the /function object/."
           (cond ((not (legal-fun-name-p name)))
                 ((not (equal (sb-c::real-function-name name) name))
                  (setf (random-documentation name 'function) string))
                 (t
                  (setf (fun-doc (fdefinition name)) string))))
          ((typep name '(or symbol cons))
           (setf (random-documentation name doc-type) string)))))

;;; It would be nice not to need this at all, but there's too much spaghetti
;;; and macrology for me to figure out where relying on a method just works.
(defun %doc-info (x doc-type)
  (case doc-type
    (function
     ;; Unused
     (error "FUNCTION doc-type is not supported."))
    (structure
     (typecase x
       (symbol (cond
                 ((eq (info :type :kind x) :instance)
                  (values (info :type :documentation x)))
                 ((info :typed-structure :info x)
                  (values (info :typed-structure :documentation x)))))))
    (type
     (typecase x
       (structure-class (values (info :type :documentation (class-name x))))
       (t (and (typep x 'symbol) (values (info :type :documentation x))))))
    ((t)
     (typecase x
       (function (fun-doc x))
       (structure-class (values (info :type :documentation (class-name x))))
       ((or symbol cons)
        (random-documentation x doc-type))))
    (t
     (when (typep x '(or symbol cons))
       (random-documentation x doc-type)))))

(defun set-function-name-documentation (name documentation)
  (aver name)
  (cond ((not (legal-fun-name-p name))
         nil)
        ((not (equal (sb-c::real-function-name name) name))
         (setf (random-documentation name 'function) documentation))
        (t
         (setf (fun-doc (or (and (symbolp name)
                                 (macro-function name))
                            (fdefinition name)))
               documentation)))
  documentation)

;;; Generic behavior

(defgeneric documentation (object doc-type)
  (:argument-precedence-order doc-type object))

(fmakunbound '(setf documentation))
(defgeneric (setf documentation) (new-value object doc-type)
  (:argument-precedence-order doc-type object new-value))

(defmethod (setf documentation) :around (new-value (x (eql nil)) doc-type)
  (style-warn "Ignoring doc-type ~a for ~a." doc-type nil)
  new-value)

;;; default if DOC-TYPE doesn't match one of the specified types
(defmethod documentation (object doc-type)
  (warn "unsupported DOCUMENTATION: doc-type ~S for object of type ~S"
        doc-type (type-of object))
  nil)

;;; default if DOC-TYPE doesn't match one of the specified types
(defmethod (setf documentation) (new-value object doc-type)
  ;; CMU CL made this an error, but since ANSI says that even for supported
  ;; doc types an implementation is permitted to discard docs at any time
  ;; for any reason, this feels to me more like a warning. -- WHN 19991214
  (warn "discarding unsupported DOCUMENTATION: doc-type ~S for object of type ~S"
        doc-type (type-of object))
  new-value)

;;; Deprecation note

(defun maybe-add-deprecation-note (namespace name documentation)
  (unless (member namespace '(function variable type))
    (return-from maybe-add-deprecation-note documentation))
  (binding* (((state since replacements)
              (deprecated-thing-p namespace name))
             (note (when state
                     (%with-output-to-string (stream)
                       (sb-impl::print-deprecation-message
                        namespace name (first since) (second since)
                        replacements stream)))))
    (cond
      ((and documentation note)
       (concatenate
        'string note #.(format nil "~2%") documentation))
      (documentation)
      (note))))

(defmethod documentation :around ((x t) (doc-type t))
  (let ((namespace (cond
                     ((typep x 'function)
                      'function)
                     ((eq doc-type 'compiler-macro)
                      'function)
                     ((typep x 'class)
                      'type)
                     ((eq doc-type 'structure)
                      'type)
                     (t
                      doc-type)))
        (name (cond
                ((typep x 'function)
                 (%fun-name x))
                ((typep x 'class)
                 (class-name x))
                (t
                 x)))
        (documentation (call-next-method)))
    (maybe-add-deprecation-note namespace name documentation)))

;;; functions, macros, and special forms

(flet ((maybe-function-documentation (name)
         (cond
           ((not (legal-fun-name-p name)))
           ((random-documentation name 'function))
           ;; Nothing under the name, check the function object.
           ((fboundp name)
            (fun-doc (cond
                       ((and (symbolp name) (special-operator-p name))
                        (fdefinition name))
                       ((and (symbolp name) (macro-function name)))
                       ((fdefinition name))))))))

  (defmethod documentation ((x function) (doc-type (eql 't)))
    (fun-doc x))

  (defmethod documentation ((x function) (doc-type (eql 'function)))
    (fun-doc x))

  (defmethod documentation ((x list) (doc-type (eql 'compiler-macro)))
    (awhen (compiler-macro-function x)
      (documentation it t)))

  (defmethod documentation ((x list) (doc-type (eql 'function)))
    (maybe-function-documentation x))

  (defmethod documentation ((x symbol) (doc-type (eql 'function)))
    (maybe-function-documentation x))

  (defmethod documentation ((x symbol) (doc-type (eql 'compiler-macro)))
    (awhen (compiler-macro-function x)
      (documentation it t))))

(defmethod (setf documentation) (new-value (x function) (doc-type (eql 't)))
  (setf (fun-doc x) new-value))

(defmethod (setf documentation) (new-value (x function) (doc-type (eql 'function)))
  (setf (fun-doc x) new-value))

(defmethod (setf documentation) (new-value (x list) (doc-type (eql 'function)))
  (set-function-name-documentation x new-value))

(defmethod (setf documentation) (new-value (x list) (doc-type (eql 'compiler-macro)))
  (awhen (compiler-macro-function x)
    (setf (documentation it t) new-value)))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'function)))
  (set-function-name-documentation x new-value))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'compiler-macro)))
  (awhen (compiler-macro-function x)
    (setf (documentation it t) new-value)))

;;; SETF documentation is attached to the function that performs expansion,
;;; except for short form DEFSETF which is in the globaldb value directly.
(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'setf)))
  (let ((expander (info :setf :expander x)))
    (typecase expander
      ((cons symbol) (setf (second expander) new-value))
      (cons (setf (documentation (cdr expander) 'function) new-value))
      (function (setf (documentation expander 'function) new-value)))))

(defmethod documentation ((x symbol) (doc-type (eql 'setf)))
  (let ((expander (info :setf :expander x)))
    (typecase expander
      ((cons symbol) (second expander))
      (cons (documentation (cdr expander) 'function))
      (function (documentation expander 'function)))))

;;; method combinations
(defmethod documentation ((x method-combination) (doc-type (eql 't)))
  (slot-value x '%documentation))

(defmethod documentation
    ((x method-combination) (doc-type (eql 'method-combination)))
  (slot-value x '%documentation))

(defmethod documentation ((x symbol) (doc-type (eql 'method-combination)))
  (random-documentation x 'method-combination))

(defmethod (setf documentation)
    (new-value (x method-combination) (doc-type (eql 't)))
  (setf (slot-value x '%documentation) new-value))

(defmethod (setf documentation)
    (new-value (x method-combination) (doc-type (eql 'method-combination)))
  (setf (slot-value x '%documentation) new-value))

(defmethod (setf documentation)
    (new-value (x symbol) (doc-type (eql 'method-combination)))
  (setf (random-documentation x 'method-combination) new-value))

;;; methods
(defmethod documentation ((x standard-method) (doc-type (eql 't)))
  (slot-value x '%documentation))

(defmethod (setf documentation)
    (new-value (x standard-method) (doc-type (eql 't)))
  (setf (slot-value x '%documentation) new-value))

;;; types, classes, and structure names

(macrolet
    ((define-type-documentation-methods (specializer get-form set-form)
       `(progn
          (defmethod documentation ((x ,specializer) (doc-type (eql 't)))
            ,get-form)

          (defmethod documentation ((x ,specializer) (doc-type (eql 'type)))
            (documentation x t))

          (defmethod (setf documentation) (new-value
                                           (x ,specializer)
                                           (doc-type (eql 't)))
            ,set-form)

          (defmethod (setf documentation) (new-value
                                           (x ,specializer)
                                           (doc-type (eql 'type)))
            (setf (documentation x 't) new-value))))
     (define-type-documentation-lookup-methods (doc-type)
       `(progn
          (defmethod documentation ((x symbol) (doc-type (eql ',doc-type)))
            (acond
             ((find-class x nil)
              (documentation it t))
             (t
              (%doc-info x ',doc-type))))

          (defmethod (setf documentation) (new-value
                                           (x symbol)
                                           (doc-type (eql ',doc-type)))
            (acond
             ((find-class x nil)
              (setf (documentation it t) new-value))
             (t
              (setf (%doc-info x ',doc-type) new-value)))))))

  (define-type-documentation-methods structure-class
      (%doc-info (class-name x) 'type)
      (setf (%doc-info (class-name x) 'type) new-value))

  (define-type-documentation-methods class
      (slot-value x '%documentation)
      (setf (slot-value x '%documentation) new-value))

  ;; although the CLHS doesn't mention this, it is reasonable to
  ;; assume that parallel treatment of condition-class was intended
  ;; (if condition-class is in fact not implemented as a
  ;; standard-class or structure-class).
  (define-type-documentation-methods condition-class
      (%doc-info (class-name x) 'type)
      (setf (%doc-info (class-name x) 'type) new-value))

  (define-type-documentation-lookup-methods type)
  (define-type-documentation-lookup-methods structure))


;;; variables
(defmethod documentation ((x symbol) (doc-type (eql 'variable)))
  (values (info :variable :documentation x)))

(defmethod (setf documentation) (new-value
                                 (x symbol)
                                 (doc-type (eql 'variable)))
  (setf (%doc-info x 'variable) new-value))

;;; extra-standard methods, for getting at slot documentation
(defmethod documentation ((slotd standard-slot-definition) (doc-type (eql 't)))
  (declare (ignore doc-type))
  (slot-value slotd '%documentation))

(defmethod (setf documentation)
    (new-value (slotd standard-slot-definition) (doc-type (eql 't)))
  (declare (ignore doc-type))
  (setf (slot-value slotd '%documentation) new-value))

;;; Now that we have created the machinery for setting documentation, we can
;;; set the documentation for the machinery for setting documentation.
(setf (documentation 'documentation 'function)
      "Return the documentation string of Doc-Type for X, or NIL if none
exists. System doc-types are VARIABLE, FUNCTION, STRUCTURE, TYPE, SETF, and T.

Function documentation is stored separately for function names and objects:
DEFUN, LAMBDA, &co create function objects with the specified documentation
strings.

 \(SETF (DOCUMENTATION NAME 'FUNCTION) STRING)

sets the documentation string stored under the specified name, and

 \(SETF (DOCUMENTATION FUNC T) STRING)

sets the documentation string stored in the function object.

 \(DOCUMENTATION NAME 'FUNCTION)

returns the documentation stored under the function name if any, and
falls back on the documentation in the function object if necessary.")

(setf (documentation '+slot-unbound+ 'variable)
  "SBCL specific extensions to MOP: if this value is read from an
instance using STANDARD-INSTANCE-ACCESS, the slot is unbound.
Similarly, an :INSTANCE allocated slot can be made unbound by
assigning this to it using (SETF STANDARD-INSTANCE-ACCESS).

Value of +SLOT-UNBOUND+ is unspecified, and should not be relied to be
of any particular type, but it is guaranteed to be suitable for EQ
comparison.")

#.(prog1 `(progn ,@*!documentation-methods*)
    (setq *!documentation-methods* nil))

(dolist (args (prog1 *!docstrings* (makunbound '*!docstrings*)))
  (apply #'(setf documentation) args))
