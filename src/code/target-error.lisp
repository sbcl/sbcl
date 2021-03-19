;;;; that part of the condition system which can or should come early
;;;; (mostly macro-related)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

(defun decode-internal-error-args (sap trap-number &optional error-number)
  (let ((error-number (cond (error-number)
                            ((>= trap-number sb-vm:error-trap)
                             (prog1
                                 (- trap-number sb-vm:error-trap)
                               (setf trap-number sb-vm:error-trap)))
                            (t
                             (prog1 (sap-ref-8 sap 0)
                               (setf sap (sap+ sap 1)))))))
    (let ((length (error-length error-number)))
      (declare (type (unsigned-byte 8) length))
      (values error-number
              (loop repeat length with index = 0
                    collect (sb-c:sap-read-var-integerf sap index))
              trap-number))))

(defun muffle-warning-p (warning)
  (declare (special *muffled-warnings*))
  (typep warning *muffled-warnings*))

;;; Each cluster is an alist of the form
;;;
;;;  ((TYPE-TEST1 . HANDLER1) (TYPE-TEST2 . HANDLER2) ...)
;;;
;;; where TYPE-TESTN are functions of one argument which test a given
;;; condition instance for the type required by the corresponding
;;; HANDLERN. HANDLERN are function designators.
;;;
;;; Newly established handlers are added at the beginning of the
;;; list. Elements to the left of the alist take precedence over
;;; elements to the right.
;;;
;;; Lists to which *HANDLER-CLUSTERS* is bound generally have dynamic
;;; extent.

(defmethod print-object ((restart restart) stream)
  (if *print-escape*
      (print-unreadable-object (restart stream :type t :identity t)
        (prin1 (restart-name restart) stream))
      (restart-report restart stream)))

(setf (documentation 'restart-name 'function)
      "Return the name of the given restart object.")

(defun restart-report (restart stream)
  (if (restart-report-function restart)
      (funcall (truly-the function (restart-report-function restart))
               stream)
      (prin1 (or (restart-name restart)
                 restart)
             stream)))

(defvar *restart-test-stack* nil)

;; Call FUNCTION with all restarts in the current dynamic environment,
;; 1) that are associated to CONDITION (when CONDITION is NIL, all
;;    restarts are processed)
;; 2) and for which the restart test returns non-NIL for CONDITION.
;; When CALL-TEST-P is non-NIL, all restarts are processed.
(defun map-restarts (function &optional condition (call-test-p t))
  (declare (function function))
  (let ((stack *restart-test-stack*))
    (dolist (restart-cluster *restart-clusters*)
      (dolist (restart restart-cluster)
        (when (and (or (not condition)
                       (null (restart-associated-conditions restart))
                       (memq condition (restart-associated-conditions restart)))
                   ;; A call to COMPUTE-RESTARTS -- from an error,
                   ;; from user code, whatever -- inside the test
                   ;; function would cause infinite recursion here, so
                   ;; we disable each restart using
                   ;; *restart-test-stack* for the duration of the
                   ;; test call.
                   (not (memq restart stack))
                   (or (not call-test-p)
                       (let ((*restart-test-stack* (cons restart stack)))
                         (declare (truly-dynamic-extent *restart-test-stack*))
                         (funcall (restart-test-function restart) condition))))
          (funcall function restart))))))

(defun compute-restarts (&optional condition)
  "Return a list of all the currently active restarts ordered from most recently
established to less recently established. If CONDITION is specified, then only
restarts associated with CONDITION (or with no condition) will be returned."
  (collect ((result))
    (map-restarts (lambda (restart) (result restart)) condition)
    (result)))

(defun %find-restart (identifier condition &optional (call-test-p t))
  (flet ((eq-restart-p (restart)
           (when (eq identifier restart)
             (return-from %find-restart restart)))
         (named-restart-p (restart)
           (when (eq identifier (restart-name restart))
             (return-from %find-restart restart))))
    ;; KLUDGE: can the compiler infer this dx automatically?
    (declare (truly-dynamic-extent #'eq-restart-p #'named-restart-p))
    (if (typep identifier 'restart)
        ;; The code under #+previous-... below breaks the abstraction
        ;; introduced by MAP-RESTARTS, but is about twice as
        ;; fast as #+equivalent-... . Also, it is a common case due to
        ;;
        ;;    (INVOKE-RESTART RESTART)
        ;; -> (FIND-RESTART-OR-CONTROL-ERROR RESTART)
        ;; -> (FIND-RESTART RESTART)
        ;;
        ;; However, both #+previous-... and #+equivalent-... may be
        ;; wrong altogether because of
        ;; https://bugs.launchpad.net/sbcl/+bug/774410:
        ;; The behavior expected in that report can be achieved by the
        ;; following line (which is, of course, the slowest of all
        ;; possibilities):
        (map-restarts #'eq-restart-p condition call-test-p)

        #+equivalent-to-previous-sbcl-behavior--faster-but-see-bug-774410
        (map-restarts #'eq-restart-p nil nil)

        #+previous-behavior--fastest-but-see-bug-774410
        (and (find-if (lambda (cluster) (find identifier cluster)) *restart-clusters*)
             identifier)

        (map-restarts #'named-restart-p condition call-test-p))))

(defun find-restart (identifier &optional condition)
  "Return the first restart identified by IDENTIFIER. If IDENTIFIER is a symbol,
then the innermost applicable restart with that name is returned. If IDENTIFIER
is a restart, it is returned if it is currently active. Otherwise NIL is
returned. If CONDITION is specified and not NIL, then only restarts associated
with that condition (or with no condition) will be returned."
  ;; Calls MAP-RESTARTS such that restart test functions are
  ;; respected.
  (%find-restart identifier condition))

;;; helper for the various functions which are ANSI-spec'ed to do
;;; something with a restart or signal CONTROL-ERROR if there is none
(defun find-restart-or-control-error (identifier &optional condition (call-test-p t))
  (declare (optimize allow-non-returning-tail-call))
  (or (%find-restart identifier condition call-test-p)
      (error 'simple-control-error
             :format-control "No restart ~S is active~@[ for ~S~]."
             :format-arguments (list identifier condition))))

(defun invoke-restart (restart &rest values)
  "Calls the function associated with the given restart, passing any given
   arguments. If the argument restart is not a restart or a currently active
   non-nil restart name, then a CONTROL-ERROR is signalled."
  (/show "entering INVOKE-RESTART" restart)
  ;; The following code calls MAP-RESTARTS (through
  ;; FIND-RESTART-OR-CONTROL-ERROR -> %FIND-RESTART) such that restart
  ;; test functions are respected when RESTART is a symbol, but not
  ;; when RESTART is a RESTART instance.
  ;;
  ;; Without disabling test functions for the RESTART instance case,
  ;; the following problem would arise:
  ;;
  ;;  (restart-case
  ;;      (handler-bind
  ;;          ((some-condition (lambda (c)
  ;;                             (invoke-restart (find-restart 'foo c)) ; a)
  ;;                             (invoke-restart 'foo)                  ; b)
  ;;                             )))
  ;;        (signal 'some-condition))
  ;;    (foo ()
  ;;     :test (lambda (c) (typep c 'some-condition))))
  ;;
  ;; In case a), INVOKE-RESTART receives the RESTART instance, but
  ;; cannot supply the condition instance needed by the test. In case
  ;; b) INVOKE-RESTART calls FIND-RESTART, but again cannot supply the
  ;; condition instance. As a result, the restart would be impossible
  ;; the invoke.
  (let ((real-restart (find-restart-or-control-error
                       restart nil (symbolp restart))))
    (apply (restart-function real-restart) values)))

(defun interactive-restart-arguments (real-restart)
  (let ((interactive-function (restart-interactive-function real-restart)))
    (if interactive-function
        (funcall interactive-function)
        '())))

(defun invoke-restart-interactively (restart)
  "Calls the function associated with the given restart, prompting for any
   necessary arguments. If the argument restart is not a restart or a
   currently active non-NIL restart name, then a CONTROL-ERROR is signalled."
  ;; For an explanation of the call to FIND-RESTART-OR-CONTROL-ERROR,
  ;; see comment in INVOKE-RESTART.
  (let* ((real-restart (find-restart-or-control-error
                        restart nil (symbolp restart)))
         (args (interactive-restart-arguments real-restart)))
    (apply (restart-function real-restart) args)))

;;; To reduce expansion size of RESTART-CASE
(defun with-simple-condition-restarts (function cerror-arg datum &rest arguments)
  (let ((sb-debug:*stack-top-hint* (or sb-debug:*stack-top-hint*
                                       'with-simple-condition-restarts))
        (condition (apply #'coerce-to-condition datum
                          (case function
                            (warn 'simple-warning)
                            (signal 'simple-condition)
                            (t 'simple-error))
                          function
                          arguments)))
    (with-condition-restarts condition (car *restart-clusters*)
      (if (eq function 'cerror)
          (cerror cerror-arg condition)
          (funcall function condition)))))


(defun assert-error (assertion &rest rest)
  (let* ((rest rest)
         (n-args-and-values (if (fixnump (car rest))
                                (* (pop rest) 2)
                                0))
         (args-and-values (subseq rest 0 n-args-and-values)))
    (destructuring-bind (&optional places datum &rest arguments)
        (subseq rest n-args-and-values)
      (let ((cond (if datum
                      (apply #'coerce-to-condition
                             datum 'simple-error 'error arguments)
                      (make-condition
                       'simple-error
                       :format-control "~@<The assertion ~S failed~:[.~:; ~
                                           with ~:*~{~S = ~S~^, ~}.~]~:@>"
                       :format-arguments (list assertion args-and-values)))))
        (restart-case
            (error cond)
          (continue ()
            :report (lambda (stream)
                      (format stream "Retry assertion")
                      (if places
                          (format stream " with new value~P for ~{~S~^, ~}."
                                  (length places) places)
                          (format stream ".")))
            nil))))))

;;; READ-EVALUATED-FORM is used as the interactive method for restart cases
;;; setup by the Common Lisp "casing" (e.g., CCASE and CTYPECASE) macros
;;; and by CHECK-TYPE.
(defun read-evaluated-form (&optional (prompt-control nil promptp)
                            &rest prompt-args)
  (apply #'format *query-io*
         (if promptp prompt-control "~&Enter a form to be evaluated: ")
         prompt-args)
  (finish-output *query-io*)
  (list (eval (read *query-io*))))

(defun read-evaluated-form-of-type (type &optional (prompt-control nil promptp)
                                    &rest prompt-args)
  (loop (apply #'format *query-io*
               (if promptp prompt-control "~&Enter a form evaluating to a value of type ~a: ")
               (if promptp prompt-args (list type)))
        (finish-output *query-io*)
        (let ((result (eval (read *query-io*))))
          (when (typep result type)
            (return (list result)))
          (format *query-io* "~s is not of type ~s" result type))))

;;; Same as above but returns multiple values
(defun mv-read-evaluated-form (&optional (prompt-control nil promptp)
                               &rest prompt-args)
  (apply #'format *query-io*
         (if promptp prompt-control "~&Enter a form to be evaluated: ")
         prompt-args)
  (finish-output *query-io*)
  (multiple-value-list (eval (read *query-io*))))

(defun check-type-error (place place-value type &optional type-string)
  (let ((condition
         (make-condition
          'simple-type-error
          :datum place-value
          :expected-type type
          :format-control
          "The value of ~S is ~S, which is not ~:[of type ~S~;~:*~A~]."
          :format-arguments (list place place-value type-string type))))
    (restart-case (error condition)
      (store-value (value)
        :report (lambda (stream)
                  (format stream "Supply a new value for ~S." place))
        :interactive read-evaluated-form
        value))))

(defun etypecase-failure (value keys)
  (declare (optimize allow-non-returning-tail-call))
  (error 'case-failure
         :name 'etypecase
         :datum value
         :expected-type `(or ,@keys)
         :possibilities keys))

(defun ecase-failure (value keys)
  (declare (optimize allow-non-returning-tail-call))
  ;; inline definition not seen yet. Can't move this file later
  ;; in build because **<foo>-clusters** are needed early.
  (declare (notinline coerce))
  (when (vectorp keys) (setq keys (coerce keys 'list)))
  (error 'case-failure
         :name 'ecase
         :datum value
         :expected-type `(member ,@keys)
         :possibilities keys))

(defun case-body-error (name keyform keyform-value expected-type keys)
  (restart-case
      (error 'case-failure
             :name name
             :datum keyform-value
             :expected-type expected-type
             :possibilities keys)
    (store-value (value)
      :report (lambda (stream)
                (format stream "Supply a new value for ~S." keyform))
      :interactive read-evaluated-form
      value)))
