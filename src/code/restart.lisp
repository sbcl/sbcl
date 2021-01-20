;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

(declaim (inline make-restart)) ;; to allow DX-allocation

(defstruct (restart (:constructor make-restart
                        ;; Having TEST-FUNCTION at the end allows
                        ;; to not replicate its default value in RESTART-BIND.
                        (name function
                         &optional report-function
                                   interactive-function
                                   test-function))
                    (:copier nil) (:predicate nil))
  (name (missing-arg) :type symbol :read-only t)
  (function (missing-arg) :type function :read-only t)
  (report-function nil :type (or null function) :read-only t)
  (interactive-function nil :type (or null function) :read-only t)
  (test-function (lambda (cond) (declare (ignore cond)) t) :type function :read-only t)
  ;; the list of conditions which are currently associated to the
  ;; restart. maintained by WITH-CONDITION-RESTARTS in a neither
  ;; thread- nor interrupt-safe way. This should not be a problem
  ;; however, since safe uses of restarts have to assume dynamic
  ;; extent.
  (associated-conditions '() :type list))

(declaim (freeze-type restart))

(defmacro with-condition-restarts
    (condition-form restarts-form &body body)
  "Evaluates the BODY in a dynamic environment where the restarts in the list
   RESTARTS-FORM are associated with the condition returned by CONDITION-FORM.
   This allows FIND-RESTART, etc., to recognize restarts that are not related
   to the error currently being debugged. See also RESTART-CASE."
  (once-only ((condition-form condition-form)
              (restarts restarts-form))
    (with-unique-names (restart)
      ;; FIXME: check the need for interrupt-safety.
      `(unwind-protect
           (progn
             (dolist (,restart ,restarts)
               (push ,condition-form
                     (restart-associated-conditions ,restart)))
             ,@body)
         (dolist (,restart ,restarts)
           (pop (restart-associated-conditions ,restart)))))))

(defmacro restart-bind (bindings &body forms)
  "(RESTART-BIND ({(case-name function {keyword value}*)}*) forms)
   Executes forms in a dynamic context where the given bindings are in
   effect. Users probably want to use RESTART-CASE. A case-name of NIL
   indicates an anonymous restart. When bindings contain the same
   restart name, FIND-RESTART will find the first such binding."
  (flet ((parse-binding (binding)
           (with-current-source-form (binding)
             (unless (>= (length binding) 2)
               (error "ill-formed restart binding: ~S" binding))
             (destructuring-bind (name function
                                       &key interactive-function
                                       test-function
                                       report-function)
                 binding
               (unless (or name report-function)
                 (warn "Unnamed restart does not have a report function: ~
                        ~S" binding))
               `(make-restart ',name ,function
                              ,@(and (or report-function
                                         interactive-function
                                         test-function)
                                     `(,report-function))
                              ,@(and (or interactive-function
                                         test-function)
                                     `(,interactive-function))
                              ,@(and test-function
                                     `(,test-function)))))))
    `(let ((*restart-clusters*
            (cons (list ,@(mapcar #'parse-binding bindings))
                  *restart-clusters*)))
       (declare (truly-dynamic-extent *restart-clusters*))
       (progn
         ,@forms))))

;;; Transform into WITH-SIMPLE-CONDITION-RESTARTS when appropriate.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun munge-restart-case-expression (expression env)
    (let ((exp (%macroexpand expression env)))
      (if (consp exp)
          (let* ((name (car exp))
                 (args (if (eq name 'cerror) (cddr exp) (cdr exp))))
            (if (member name '(signal error cerror warn))
                `(with-simple-condition-restarts
                   ',name
                   ,(and (eq name 'cerror)
                         (second exp))
                   ,(first args)
                   ,@(rest args))
                expression))
          expression))))

(defmacro restart-case (expression &body clauses &environment env)
  "(RESTART-CASE form {(case-name arg-list {keyword value}* body)}*)
   The form is evaluated in a dynamic context where the clauses have
   special meanings as points to which control may be transferred (see
   INVOKE-RESTART).  When clauses contain the same case-name,
   FIND-RESTART will find the first such clause. If form is a call to
   SIGNAL, ERROR, CERROR or WARN (or macroexpands into such) then the
   signalled condition will be associated with the new restarts."
  ;; PARSE-CLAUSE (which uses PARSE-KEYWORDS-AND-BODY) is used to
  ;; parse all clauses into lists of the form
  ;;
  ;;  (NAME TAG KEYWORDS LAMBDA-LIST BODY)
  ;;
  ;; where KEYWORDS are suitable keywords for use in HANDLER-BIND
  ;; bindings. These lists are then passed to
  ;; * MAKE-BINDING which generates bindings for the respective NAME
  ;;   for HANDLER-BIND
  ;; * MAKE-APPLY-AND-RETURN which generates TAGBODY entries executing
  ;;   the respective BODY.
  (let ((block-tag (sb-xc:gensym "BLOCK"))
        (temp-var (gensym)))
    (labels ((parse-keywords-and-body (keywords-and-body)
               (do ((form keywords-and-body (cddr form))
                    (result '())) (nil)
                 (destructuring-bind (&optional key (arg nil argp) &rest rest)
                     form
                   (declare (ignore rest))
                   (setq result
                         (append
                          (cond
                            ((and (eq key :report) argp)
                             (list :report-function
                                   (if (stringp arg)
                                       `#'(lambda (stream)
                                            (write-string ,arg stream))
                                       `#',arg)))
                            ((and (eq key :interactive) argp)
                             (list :interactive-function `#',arg))
                            ((and (eq key :test) argp)
                             (list :test-function `#',arg))
                            (t
                             (return (values result form))))
                          result)))))
             (parse-clause (clause)
               (with-current-source-form (clause)
                 (unless (and (listp clause) (>= (length clause) 2)
                              (listp (second clause)))
                   (error "ill-formed ~S clause, no lambda-list:~%  ~S"
                          'restart-case clause))
                 (destructuring-bind (name lambda-list &body body) clause
                   (multiple-value-bind (keywords body)
                       (parse-keywords-and-body body)
                     (list name (gensym "TAG") keywords lambda-list body)))))
             (make-binding (clause-data)
               (destructuring-bind (name tag keywords lambda-list body) clause-data
                 (declare (ignore body))
                 `(,name
                   (lambda ,(cond ((null lambda-list)
                                   ())
                                  ((and (null (cdr lambda-list))
                                        (not (member (car lambda-list)
                                                     '(&optional &key &aux))))
                                   '(temp))
                                  (t
                                   '(&rest temp)))
                     ,@(when lambda-list `((setq ,temp-var temp)))
                     (locally (declare (optimize (safety 0)))
                       (go ,tag)))
                   ,@keywords)))
             (make-apply-and-return (clause-data)
               (destructuring-bind (name tag keywords lambda-list body) clause-data
                 (declare (ignore name keywords))
                 (multiple-value-bind (body declarations) (parse-body body nil)
                   `(,tag (return-from ,block-tag
                            ,(cond ((null lambda-list)
                                    `(locally ,@declarations ,@body))
                                   ((and (null (cdr lambda-list))
                                         (not (member (car lambda-list)
                                                      '(&optional &key &aux))))
                                    `(funcall (lambda ,lambda-list
                                                ,@declarations
                                                (progn ,@body))
                                              ,temp-var))
                                   (t
                                    `(apply (lambda ,lambda-list
                                              ,@declarations
                                              (progn ,@body))
                                            ,temp-var)))))))))
      (let ((clauses-data (mapcar #'parse-clause clauses)))
        `(block ,block-tag
           (let ((,temp-var nil))
             (declare (ignorable ,temp-var))
             (tagbody
                (restart-bind
                    ,(mapcar #'make-binding clauses-data)
                  (return-from ,block-tag
                    ,(munge-restart-case-expression expression env)))
                ,@(mapcan #'make-apply-and-return clauses-data))))))))

(defmacro with-simple-restart ((restart-name format-string
                                                       &rest format-arguments)
                                         &body forms)
  "(WITH-SIMPLE-RESTART (restart-name format-string format-arguments)
   body)
   If restart-name is not invoked, then all values returned by forms are
   returned. If control is transferred to this restart, it immediately
   returns the values NIL and T."
  (let ((stream (sb-xc:gensym "STREAM")))
   `(restart-case
        ;; If there's just one body form, then don't use PROGN. This allows
        ;; RESTART-CASE to "see" calls to ERROR, etc.
        ,(if (= (length forms) 1) (car forms) `(progn ,@forms))
      (,restart-name ()
        :report (lambda (,stream)
                  (declare (type stream ,stream)
                           ;; No need to optimize FORMAT for error reporting
                           (optimize (speed 1) (space 3)))
                  (format ,stream ,format-string ,@format-arguments))
        (values nil t)))))
