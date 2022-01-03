(in-package "SB-PCL")

(defmacro defgeneric (fun-name lambda-list &body options)
  (declare (type list lambda-list))
  (check-designator fun-name defgeneric)
  (with-current-source-form (lambda-list)
    (check-gf-lambda-list lambda-list))
  (let ((initargs ())
        (methods ()))
    (flet ((duplicate-option (name)
             (%program-error "The option ~S appears more than once." name))
           (expand-method-definition (qab) ; QAB = qualifiers, arglist, body
             (let* ((arglist-pos (position-if #'listp qab))
                    (arglist (elt qab arglist-pos))
                    (qualifiers (subseq qab 0 arglist-pos))
                    (body (nthcdr (1+ arglist-pos) qab)))
               `(defmethod ,fun-name ,@qualifiers ,arglist ,@body))))
      (macrolet ((initarg (key) `(getf initargs ,key)))
        (dolist (option options)
          (let ((car-option (car option)))
            (case car-option
              (declare
               (dolist (spec (cdr option))
                 (unless (consp spec)
                   (%program-error "~@<Invalid declaration specifier in ~
                                    DEFGENERIC: ~S~:@>"
                                   spec))
                 (when (member (first spec)
                               ;; FIXME: this list is slightly weird.
                               ;; ANSI (on the DEFGENERIC page) in one
                               ;; place allows only OPTIMIZE; in
                               ;; another place gives this list of
                               ;; disallowed declaration specifiers.
                               ;; This seems to be the only place where
                               ;; the FUNCTION declaration is
                               ;; mentioned; TYPE seems to be missing.
                               ;; Very strange.  -- CSR, 2002-10-21
                               '(declaration ftype function
                                 inline notinline special))
                   (%program-error "The declaration specifier ~S is ~
                                    not allowed inside DEFGENERIC."
                                  spec))
                 (if (or (eq 'optimize (first spec))
                         (info :declaration :known (first spec)))
                     (push spec (initarg :declarations))
                     (warn "Ignoring unrecognized declaration in DEFGENERIC: ~S"
                           spec))))
              (:method-combination
               (when (initarg car-option)
                 (duplicate-option car-option))
               (unless (symbolp (cadr option))
                 (%program-error "METHOD-COMBINATION name not a symbol: ~
                                  ~S"
                                (cadr option)))
               (setf (initarg car-option)
                     `',(cdr option)))
              (:argument-precedence-order
               (let* ((required (nth-value 1 (parse-lambda-list lambda-list)))
                      (supplied (cdr option)))
                 (unless (= (length required) (length supplied))
                   (%program-error "argument count discrepancy in ~
                                    :ARGUMENT-PRECEDENCE-ORDER clause."))
                 (when (set-difference required supplied)
                   (%program-error "unequal sets for ~
                                    :ARGUMENT-PRECEDENCE-ORDER clause: ~
                                    ~S and ~S"
                                   required supplied))
                 (setf (initarg car-option)
                       `',(cdr option))))
              ((:documentation :generic-function-class :method-class)
               (unless (proper-list-of-length-p option 2)
                 (error "bad list length for ~S" option))
               (if (initarg car-option)
                   (duplicate-option car-option)
                   (setf (initarg car-option) `',(cadr option))))
              (:method
                  (push (cdr option) methods))
              (t
               ;; ANSI requires that unsupported things must get a
               ;; PROGRAM-ERROR.
               (%program-error "unsupported option ~S" option)))))

        (when (initarg :declarations)
          (setf (initarg :declarations)
                `',(initarg :declarations))))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (compile-or-load-defgeneric ',fun-name))
         (load-defgeneric ',fun-name ',lambda-list
                          (sb-c:source-location) ,@initargs)
         ,@(when methods
             `((set-initial-methods (list ,@(mapcar #'expand-method-definition methods))
                                    (fdefinition ',fun-name))))
         (fdefinition ',fun-name)))))

(define-condition generic-function-lambda-list-error
    (reference-condition simple-program-error)
  ()
  (:default-initargs :references '((:ansi-cl :section (3 4 2)))))

(defun generic-function-lambda-list-error (format-control &rest format-arguments)
  (error 'generic-function-lambda-list-error
         :format-control format-control
         :format-arguments format-arguments))

(defun check-gf-lambda-list (lambda-list)
  (declare (muffle-conditions compiler-note))
  (binding* ((context "a generic function lambda list")
             ((nil nil optional nil keys)
              (multiple-value-call #'check-lambda-list-names
                (parse-lambda-list
                 lambda-list
                 :accept (lambda-list-keyword-mask
                          '(&optional &rest &key &allow-other-keys))
                 :condition-class 'generic-function-lambda-list-error
                 :context context)
                :context context
                :signal-via #'generic-function-lambda-list-error)))
    ;; PARSE-LAMBDA-LIST validates the skeleton, so just check for
    ;; incorrect use of defaults.
    (labels ((lose (kind arg)
               (generic-function-lambda-list-error
                (sb-format:tokens
                 "~@<Invalid ~A argument specifier ~S ~_in ~A ~
                  ~/sb-impl:print-lambda-list/~:>")
                kind arg context lambda-list))
             (verify-optional (spec)
               (when (nth-value 3 (parse-optional-arg-spec spec))
                 (lose '&optional spec)))
             (verify-key (spec)
               (when (nth-value 4 (parse-key-arg-spec spec))
                 (lose '&key spec))))
      ;; no defaults or supplied-p vars allowed for &OPTIONAL or &KEY
      (mapc #'verify-optional optional)
      (mapc #'verify-key keys))))
