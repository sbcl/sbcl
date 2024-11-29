;;;; Type checking of higher order functions.
;;;;
;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

(defun assert-function-designator (caller lvars lvar annotation policy)
  (destructuring-bind (args &optional results &rest options) annotation
    (declare (ignore options))
    (multiple-value-bind (arg-specs result-specs deps)
        (callable-dependant-lvars caller lvars args results)
      (let* ((type (make-fun-type
                    :required
                    (make-list (+ (length arg-specs)
                                  ;; Count ordinary types without annotations
                                  (count-if #'atom args))
                               :initial-element *universal-type*)
                    :returns *wild-type*
                    :designator t))
             (annotation (make-lvar-function-designator-annotation
                          :caller caller
                          :arg-specs arg-specs
                          :result-specs result-specs
                          :deps deps
                          :type type)))
        (when (add-annotation lvar annotation)
          (assert-lvar-type lvar
                            (specifier-type 'function-designator)
                            policy))))))

(defun fun-type-positional-count (fun-type)
  (+ (length (fun-type-required fun-type))
     (length (fun-type-optional fun-type))))

(defun callable-dependant-lvars (caller lvars args results)
  (let ((fun-type (global-ftype caller)))
    (collect ((lvars))
      (let ((arg-position -1)
            (optional-args (nthcdr (fun-type-positional-count fun-type) lvars)))
        (labels ((record-lvar (lvar)
                   (lvars lvar)
                   (incf arg-position))
                 (handle-keys (options)
                   (loop for (key value*) on options by #'cddr
                         for value = (if (or (eq key :key)
                                             (eq key :value))
                                         (if (consp value*)
                                             (let ((value-lvar (getf optional-args (car value*))))
                                               (when value-lvar
                                                 (let ((if (getf optional-args (second value*))))
                                                   (cond (if
                                                          (list (record-lvar value-lvar)
                                                                (record-lvar if) (third value*)))
                                                         ((null (third value*))
                                                          (record-lvar value-lvar))))))
                                             (let ((lvar (getf optional-args value*)))
                                               (and lvar
                                                    (record-lvar lvar))))
                                         value*)
                         when value
                         collect key
                         and
                         collect value))
                 (process-arg (arg)
                   (ecase (car arg)
                     (nth-arg
                      (list
                       (list* (record-lvar (nth (cadr arg) lvars))
                              (handle-keys (cddr arg)))))
                     (rest-args
                      (loop for lvar in optional-args
                            collect
                            (list* (record-lvar lvar)
                                   (handle-keys (cdr arg)))))
                     (or
                      (list
                       (list* 'or (process-args (cdr arg)))))))
                 (process-args (args)
                   (loop for arg in args
                         when (consp arg)
                         nconc
                         (process-arg arg))))
          (values (process-args args)
                  (and (consp results)
                       (car (process-arg results)))
                  (lvars)))))))

(defun map-key-lvars (function args type)
  (when (fun-type-keyp type)
    (let ((key-args (nthcdr (fun-type-positional-count type)
                            args))
          (key-types (fun-type-keywords type))
          seen
          unknown)
      (loop for (key lvar) on key-args by #'cddr
            for key-value = (and (constant-lvar-p key)
                                 (lvar-value key))
            for key-info = (find key-value key-types :key #'key-info-name)
            do (cond (key-info
                      (unless (memq key-value seen)
                        (funcall function key-value lvar)
                        (push key-value seen)))
                     ((eq key-value
                          :allow-other-keys))
                     (t
                      (push key unknown))))
      unknown)))

;;; Turn constant LVARs in keyword arg positions to constants so that
;;; they can be passed to FUN-INFO-CALLABLE-CHECK.
(defun resolve-key-args (args type)
  (if (fun-type-keyp type)
      (let ((non-key (fun-type-positional-count type)))
        (if (> (length args) non-key)
            (let* (key-arguments
                   (unknown (map-key-lvars (lambda (key value)
                                             (push key key-arguments)
                                             (push value key-arguments))
                                           args
                                           type)))
              (values (nconc (subseq args 0 non-key)
                             (nreverse key-arguments))
                      unknown))
            (values args nil)))
      (values args nil)))

;;; The function should accept
;;; (lvar args results &key (unknown-keys boolean) (no-function-conversion boolean) (arg-lvars list-of-lvars))
(defun map-callable-arguments (function combination)
  (let* ((combination-name (lvar-fun-name (combination-fun combination) t))
         (type (global-ftype combination-name))
         (info (info :function :info combination-name))
         (annotation (fun-info-annotation info)))
    (when annotation
      (multiple-value-bind (arg-lvars unknown) (resolve-key-args (combination-args combination) type)
        (flet ((call (lvar annotation)
                 (destructuring-bind (args &optional results . options) annotation
                   (apply function lvar
                          (loop for arg in args
                                if (typep arg '(cons (eql rest-args)))
                                nconc (loop repeat (- (length (combination-args combination))
                                                      (fun-type-positional-count type))
                                            collect arg)
                                else
                                collect arg)
                          results
                          :arg-lvars arg-lvars
                          :unknown-keys unknown
                          options))))
          (loop for (n kind . annotation) in (fun-type-annotation-positional annotation)
                when (memq kind '(function function-designator))
                do
                (let ((arg (nth n arg-lvars)))
                  (when arg
                    (call arg annotation))))
          (loop with keys = (nthcdr (fun-type-positional-count type)
                                    arg-lvars)
                for (key (kind . annotation)) on (fun-type-annotation-key annotation) by #'cddr
                when (memq kind '(function function-designator))
                do
                (let ((lvar (getf keys key)))
                  (when lvar
                    (call lvar annotation)))))))))

;; Handle #'function,  'function and (lambda (x y))
(defun node-fun-type (node &optional defined-here asserted-type)
  (let* ((use node)
         (lvar-type (single-value-type (node-derived-type use)))
         (leaf (if (ref-p use)
                   (ref-leaf use)
                   (return-from node-fun-type
                     (values lvar-type
                             (node-source-form use)))))
         (asserted t)
         (defined-type (and (global-var-p leaf)
                            (eq (global-var-kind leaf) :global-function)
                            (case (leaf-where-from leaf)
                              (:declared
                               (leaf-type leaf))
                              (:defined
                               (cond ((or defined-here
                                          asserted-type
                                          (and (defined-fun-p leaf)
                                               (eq (defined-fun-inlinep leaf) 'notinline))
                                          (fun-lexically-notinline-p (leaf-%source-name leaf)
                                                                     (node-lexenv (lvar-dest (node-lvar node)))))
                                      (setf asserted nil)
                                      lvar-type)
                                     (t
                                      (global-ftype (leaf-%source-name leaf)))))
                              ((:defined-here :declared-verify)
                               (cond ((or (and (defined-fun-p leaf)
                                               (eq (defined-fun-inlinep leaf) 'notinline))
                                          (fun-lexically-notinline-p (leaf-%source-name leaf)
                                                                     (node-lexenv (lvar-dest (node-lvar node)))))
                                      lvar-type)
                                     (t
                                      (global-ftype (leaf-%source-name leaf)))))
                              (t
                               (global-var-defined-type leaf)))))
         (entry-fun (if (and (functional-p leaf)
                             (functional-kind-eq leaf external))
                        (functional-entry-fun leaf)
                        leaf))
         (lvar-type (cond ((and defined-type
                                (neq defined-type *universal-type*))
                           defined-type)
                          ((and (functional-p entry-fun)
                                (fun-type-p (functional-type entry-fun)))
                           (functional-type entry-fun))
                          ((and (not (fun-type-p lvar-type))
                                (lambda-p entry-fun)
                                (functional-kind-eq entry-fun nil)
                                (lambda-tail-set entry-fun))
                           (make-fun-type :wild-args t
                                          :returns
                                          (tail-set-type (lambda-tail-set entry-fun))))
                          ((and asserted-type
                                (not (constant-p leaf)))
                           (setf asserted nil)
                           ;; Don't trust FUNCTION type declarations,
                           ;; they perform no runtime assertions.
                           (specifier-type 'function))
                          (t
                           (setf asserted nil)
                           lvar-type)))
         (fun-name (cond ((or (fun-type-p lvar-type)
                              (functional-p leaf)
                              (and (global-var-p leaf)
                                   (eq (global-var-kind leaf) :global-function)))
                          (cond ((constant-p leaf)
                                 (let ((value (constant-value leaf)))
                                   (etypecase value
                                     #-sb-xc-host
                                     (function
                                      (%fun-name value))
                                     (symbol
                                      value))))
                                ((and (lambda-p leaf)
                                      (functional-kind-eq leaf external))
                                 (leaf-debug-name (lambda-entry-fun leaf)))
                                (t
                                 (leaf-debug-name leaf))))
                         ((constant-p leaf)
                          (constant-value leaf))
                         (t
                          (return-from node-fun-type lvar-type))))
         (type (cond ((fun-type-p lvar-type)
                      lvar-type)
                     ((symbolp fun-name)
                      (if (or (fun-lexically-notinline-p fun-name
                                                         (node-lexenv (lvar-dest (node-lvar node))))
                              (and (or asserted-type
                                       defined-here)
                                   (neq (info :function :where-from fun-name) :declared)))
                          lvar-type
                          (global-ftype fun-name)))
                     ((functional-p leaf)
                      (let ((info (functional-info leaf)))
                        (if info
                            (specifier-type (entry-info-type info))
                            lvar-type)))
                     (t
                      lvar-type))))
    (values type fun-name leaf asserted)))

(defun lvar-fun-type (lvar &optional defined-here asserted-type)
  (let* ((use (principal-lvar-use lvar))
         (lvar-type (lvar-type lvar)))
    (if (ref-p use)
        (multiple-value-bind (type fun-name leaf asserted) (node-fun-type use defined-here asserted-type)
          (let ((int (if (fun-type-p lvar-type)
                         ;; save the cast type
                         (type-intersection type lvar-type)
                         type)))
            (values (if (neq int *empty-type*)
                        int
                        lvar-type)
                    fun-name leaf asserted)))
        (values lvar-type
                (typecase use
                  (node
                   (node-source-form use))
                  (t
                   '.anonymous.))))))

(defun callable-argument-lossage-kind (fun-name leaf soft hard)
  (if (or (not leaf)
          (and (not (memq (leaf-where-from leaf) '(:defined-here :declared-verify)))
               (not (and (functional-p leaf)
                         (or (lambda-p leaf)
                             (functional-kind-eq leaf toplevel-xep))))
               (or (not fun-name)
                   (not (info :function :info fun-name)))))
      soft
      hard))

(defun validate-test-and-test-not (combination)
  (let* ((combination-name (lvar-fun-name (combination-fun combination) t))
         (info (info :function :info combination-name)))
    (when (and info
               (ir1-attributep (fun-info-attributes info) call))
      (let (test
            test-not
            (null-type (specifier-type 'null)))
        (map-key-lvars (lambda (key value)
                         (when (and (not test)
                                    (eq key :test))
                           (setf test value))
                         (when (and (not test-not)
                                    (eq key :test-not))
                           (setf test-not value)))
                       (combination-args combination)
                       (global-ftype combination-name))
        (when (and test
                   test-not
                   (eq (type-intersection null-type (lvar-type test))
                       *empty-type*)
                   (eq (type-intersection null-type (lvar-type test-not))
                       *empty-type*))
          (note-lossage "~s: can't specify both :TEST and :TEST-NOT"
                        combination-name))))))

(defun function-designator-lvar-types (annotation)
  (labels ((arg-type (arg)
             (if (lvar-p arg)
                 (lvar-type arg)
                 (leaf-type arg)))
           (sequence-element-type (type)
             (cond ((array-type-p type)
                    (let ((elt-type (array-type-element-type type)))
                      (if (eq elt-type *wild-type*)
                          *universal-type*
                          elt-type)))
                   ((csubtypep type (specifier-type 'string))
                    (specifier-type 'character))
                   (t
                    *universal-type*))))
    (let ((deps (lvar-function-designator-annotation-deps annotation))
          (arg-specs (lvar-function-designator-annotation-arg-specs annotation))
          (result-specs (lvar-function-designator-annotation-result-specs annotation)))
      (labels ((%process-arg (spec)
                 (destructuring-bind (nth-arg . options) spec
                   (let* ((arg (nth nth-arg deps))
                          (value-nth (getf options :value))
                          (key-nth (getf options :key))
                          (value (and value-nth
                                      (nth (if (consp value-nth)
                                               (car value-nth)
                                               value-nth)
                                           deps)))
                          (key (and key-nth (nth key-nth deps)))
                          (key-return-type (and key
                                                (multiple-value-bind (type name)
                                                    (cond ((global-var-p key)
                                                           (values (global-var-defined-type key)
                                                                   (leaf-%source-name key)))
                                                          ((lvar-p key)
                                                           (lvar-fun-type key)))
                                                  (cond ((eq name 'identity)
                                                         nil)
                                                        ((fun-type-p type)
                                                         (single-value-type (fun-type-returns type)))
                                                        (t
                                                         *universal-type*)))))
                          (type (cond (key-return-type)
                                      ((getf options :sequence)
                                       (sequence-element-type (arg-type arg)))
                                      ((getf options :sequence-type)
                                       (or (let* ((value (cond ((constant-p arg)
                                                                (constant-value arg))
                                                               ((and (lvar-p arg)
                                                                     (constant-lvar-p arg))
                                                                (lvar-value arg))))
                                                  (type (and value
                                                             (careful-specifier-type value))))
                                             (and type
                                                  (sequence-element-type type)))
                                           *universal-type*))
                                      (t
                                       (arg-type arg)))))
                     (cond (value-nth
                            (if (consp value-nth)
                                (let* ((option (nth (second value-nth) deps))
                                       (option-p (third value-nth))
                                       (false (or (not option)
                                                  (csubtypep (lvar-type option) (specifier-type 'null))))
                                       (true (and option
                                                  (csubtypep (lvar-type option) (specifier-type '(not null)))))
                                       (unknown (not (or true false))))
                                  (cond (unknown
                                         (type-union (lvar-type value)
                                                     type))
                                        ((and true option-p)
                                         (lvar-type value))
                                        ((and false (not option-p))
                                         (lvar-type value))
                                        (t
                                         type)))
                                (if (lvar-p value)
                                    (lvar-type value)
                                    *universal-type*)))
                           (t
                            type)))))
               (process-arg (spec)
                 (if (eq (car spec) 'or)
                     (cons 'or (mapcar #'%process-arg (cdr spec)))
                     (%process-arg spec))))
        (values
         (if arg-specs
             (mapcar #'process-arg arg-specs)
             (fun-type-required
              (lvar-function-designator-annotation-type annotation)))
         (if result-specs
             (process-arg result-specs)
             (fun-type-returns (lvar-function-designator-annotation-type annotation))))))))

;;; Return MIN, MAX, whether it contaions &optional/&key/&rest
(defun fun-type-arg-limits (type)
  (if (fun-type-wild-args type)
      (values nil nil)
      (let* ((min (length (fun-type-required type)))
             (max (and (not (or (fun-type-rest type)
                                (fun-type-keyp type)))
                       (+ min
                          (length (fun-type-optional type))))))
        (values min max (or (fun-type-rest type)
                            (fun-type-keyp type)
                            (fun-type-optional type))))))

;;; Should have enough types for N
(defun fun-type-n-arg-types (n type)
  (let (result)
    (flet ((pick (types)
             (loop for type in types
                   do (push type result)
                   while (plusp (decf n)))))
      (pick (fun-type-required type))
      (pick (fun-type-optional type))
      (loop with rest = (or (fun-type-rest type)
                            *universal-type*)
            repeat n
            do (push rest result)))
    (nreverse result)))

(defun report-arg-count-mismatch (fun caller type arg-count
                                  condition-type
                                  &optional lossage-fun name)
  (flet ((lose (format-control &rest format-args)
           (if lossage-fun
               (apply lossage-fun format-control format-args)
               (warn condition-type :format-control format-control
                                    :format-arguments format-args))
           t)
         (callee ()
           (or name
               (nth-value 1 (lvar-fun-type fun))))
         (caller ()
           (or caller
               (loop for annotation in (lvar-annotations fun)
                     when (typep annotation 'lvar-function-designator-annotation)
                     do (setf *compiler-error-context* annotation)
                        (return (lvar-function-designator-annotation-caller annotation))))))
    (multiple-value-bind (min max optional) (fun-type-arg-limits type)
      (or
       (cond
         ((and (not min) (not max))
          nil)
         ((not optional)
          (when (/= arg-count min)
            (lose
             "The function ~S is called~@[ by ~S~] with ~R argument~:P, but wants exactly ~R."
             (callee) (caller)
             arg-count min)))
         ((< arg-count min)
          (lose
           "The function ~S is called~@[ by ~S~] with ~R argument~:P, but wants at least ~R."
           (callee) (caller)
           arg-count min))
         ((not max)
          nil)
         ((> arg-count max)
          (lose
           "The function ~S called~@[ by ~S~] with ~R argument~:P, but wants at most ~R."
           (callee) (caller)
           arg-count max)))
       (let ((positional (fun-type-positional-count type)))
         (when (and (fun-type-keyp type)
                    (> arg-count positional)
                    (oddp (- arg-count positional)))
           (lose
            "The function ~s is called with odd number of keyword arguments."
            (callee))))))))

(defun disable-arg-count-checking (leaf type arg-count)
  (when (lambda-p leaf)
    (let ((once nil))
      ;; TODO: what if all destinations can disable arg count checking.
      (map-refs (lambda (dest)
                  (declare (ignore dest))
                  (when (shiftf once t)
                    (return-from disable-arg-count-checking)))
                leaf))
    (multiple-value-bind (min max) (fun-type-arg-limits type)
      (when (and min
                 (if max
                     (<= min arg-count max)
                     (<= min arg-count)))
        (setf (lambda-lexenv leaf)
              (make-lexenv :default (lambda-lexenv leaf)
                           :policy (augment-policy verify-arg-count 0
                                                   (lexenv-policy (lambda-lexenv leaf)))))))))

;;; This can provide better errors and better handle OR types than a
;;; simple type intersection.
(defun check-function-designator-lvar (lvar annotation)
  (map-all-uses
   (lambda (node)
     (multiple-value-bind (type name leaf) (node-fun-type node)
       (cond
         ((and name
               (valid-function-name-p name)
               (memq (info :function :kind name) '(:macro :special-form)))
          (compiler-warn "~(~a~) ~s where a function is expected"
                         (info :function :kind name) name))
         ((fun-type-p type)
          ;; If the destination is a combination-fun that means the function
          ;; is called here and not passed somewhere else, there's no longer a
          ;; need to check the function type, the arguments to the call will
          ;; do the same job.
          (unless (let* ((dest (lvar-dest lvar)))
                    (and (basic-combination-p dest)
                         (eq (basic-combination-fun dest) lvar)))
            (multiple-value-bind (args results)
                (function-designator-lvar-types annotation)
              (let* ((condition (if (eq node (principal-lvar-use lvar))
                                    (callable-argument-lossage-kind name
                                                                    leaf
                                                                    'simple-style-warning
                                                                    'simple-warning)
                                    'simple-style-warning))
                     (type-condition (case condition
                                       (simple-style-warning
                                        'type-style-warning)
                                       (t
                                        'type-warning)))
                     (caller (lvar-function-designator-annotation-caller annotation))
                     (arg-count (length args)))
                (or (report-arg-count-mismatch lvar caller
                                               type
                                               arg-count
                                               condition
                                               nil
                                               name)
                    (let ((param-types (fun-type-n-arg-types arg-count type)))
                      (unless (and (eq caller 'reduce)
                                   (eql arg-count 2))
                        (disable-arg-count-checking leaf type arg-count))
                      (block nil
                        ;; Need to check each OR seperately, a UNION could
                        ;; intersect with the function parameters
                        (labels ((hide-ors (current-or or-part)
                                   (loop for spec in args
                                         collect (cond ((eq spec current-or)
                                                        or-part)
                                                       ((typep spec '(cons (eql or)))
                                                        (sb-kernel::%type-union (cdr spec)))
                                                       (t
                                                        spec))))
                                 (check (arg param &optional
                                                     current-spec)
                                   (when (eq (type-intersection param arg) *empty-type*)
                                     (warn type-condition
                                           :format-control
                                           "The function ~S is called by ~S with ~S but it accepts ~S."
                                           :format-arguments
                                           (list
                                            name
                                            caller
                                            (mapcar #'type-specifier (hide-ors current-spec arg))
                                            (mapcar #'type-specifier param-types)))
                                     (return t))))
                          (loop for arg-type in args
                                for param-type in param-types
                                if (typep arg-type '(cons (eql or)))
                                do (loop for type in (cdr arg-type)
                                         do (check type param-type arg-type))
                                else do (check arg-type param-type)))))
                    (let ((returns (single-value-type (fun-type-returns type))))
                      (when (and (neq returns *wild-type*)
                                 (neq returns *empty-type*)
                                 (neq results *wild-type*)
                                 (eq (type-intersection returns results) *empty-type*))
                        (warn type-condition
                              :format-control
                              "The function ~S called by ~S returns ~S but ~S is expected"
                              :format-arguments
                              (list
                               name
                               caller
                               (type-specifier returns)
                               (type-specifier results)))))))))))))
   lvar)
  t)

(defun check-function-lvar (lvar annotation)
  (let ((atype (lvar-function-annotation-type annotation)))
    (multiple-value-bind (type name leaf) (lvar-fun-type lvar)
      (when (fun-type-p type)
        (let ((condition (callable-argument-lossage-kind name
                                                         leaf
                                                         'type-style-warning
                                                         'type-warning)))
          (if (eq (lvar-function-annotation-context annotation) :mv-call)
              (let* ((*compiler-error-context* annotation)
                     (max-accepted (nth-value 1 (fun-type-nargs (lvar-fun-type lvar))))
                     (min-args (fun-type-nargs atype)))
                (when (and max-accepted
                           (> min-args max-accepted))
                  (warn condition
                        :format-control
                        "~@<MULTIPLE-VALUE-CALL calls ~a with with at least ~R ~
                              values when it expects at most ~R.~@:>"
                        :format-arguments (list name min-args
                                                max-accepted))))
              (let ((int (type-intersection type atype)))
                (when (or (memq *empty-type* (fun-type-required int))
                          (and (eq (fun-type-returns int) *empty-type*)
                               (neq (fun-type-returns type) *empty-type*)
                               (not (and (eq (fun-type-returns atype) *empty-type*)
                                         (eq (fun-type-returns type) *wild-type*)))))
                  (%compile-time-type-error-warn annotation
                                                 (type-specifier atype)
                                                 (type-specifier type)
                                                 (list name)
                                                 :condition condition)))))))))
