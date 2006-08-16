;;;; compiler parts of the single stepper

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;; Local stepping control: STEP binds this to T, and when forms are
;;; being skipped this is bound to NIL down the stack to prevent
;;; signalling of STEP-CONDITIONs.
(defvar *step* nil)

;;; Global stepping control: STEP binds this to T, and when the
;;; restart to continue without stepping is selected this is set to
;;; NIL to prevent the *STEPPER-HOOK* from being called.
(defvar *stepping* nil)

(defun step-form (form source-path pathname)
  (when *step*
    (restart-case
        (signal 'step-form-condition
                :form form
                :source-path source-path
                :pathname pathname)
      (step-continue ()
        (setf *stepping* nil))
      (step-next ()
        nil)
      (step-into ()
        t))))

(defun step-variable (symbol value)
  (when *step*
    (signal 'step-variable-condition :form symbol :result value)))

(defun step-values (form values)
  (when *step*
    (signal 'step-values-condition :form form :result values)))

(defun insert-step-conditions (form)
  `(locally (declare
             (optimize (insert-step-conditions
                        ,(policy *lexenv* insert-step-conditions))))
    ,form))

;;; Flag to control instrumentation function call arguments.
(defvar *step-arguments-p* nil)

(defun known-single-value-fun-p (fun)
  (and (legal-fun-name-p fun)
       (info :function :info fun)
       (let ((type (info :function :type fun)))
         (and (and (fun-type-p type))
              (type-single-value-p (fun-type-returns type))))))

(defun ir1-convert-step (start next result form)
  (let ((form-string (let ((*print-pretty* t)
                           (*print-readably* nil))
                       (prin1-to-string form))))
    (etypecase form
      (symbol
       (let ((ctran (make-ctran))
             (*allow-instrumenting* nil))
         (ir1-convert start ctran nil `(step-variable ,form-string ,form))
         (ir1-convert ctran next result form)))
      (list
       (let* ((*step-arguments-p* (and *allow-instrumenting*
                                       (policy *lexenv* (= insert-step-conditions 3))))
              (step-form `(step-form ,form-string
                                     ',(source-path-original-source *current-path*)
                                     *compile-file-pathname*))
              (fun (car form))
              (values-form `(,fun
                             ,@(if *step-arguments-p*
                                   (mapcar #'insert-step-conditions (cdr form))
                                   (cdr form)))))
         (ir1-convert start next result
                      `(locally (declare (optimize (insert-step-conditions 0)))
                         ,(if *step-arguments-p*
                              `(let ((*step* ,step-form))
                                 ,(if (known-single-value-fun-p fun)
                                      `((lambda (value)
                                          (step-values ,form-string (list value))
                                          value)
                                        ,values-form)
                                      `(multiple-value-call
                                           (lambda (&rest values)
                                             (step-values ,form-string values)
                                             (values-list values))
                                         ,values-form)))
                              `(progn ,step-form ,values-form)))))))))

(defun step-form-p (form)
  #+sb-xc-host (declare (ignore form))
  #-sb-xc-host
  (flet ((step-symbol-p (symbol)
           (not (member (symbol-package symbol)
                        (load-time-value
                         ;; KLUDGE: packages we're not interested in stepping.
                         (mapcar #'find-package '(sb!c sb!int sb!impl sb!kernel sb!pcl)))))))
    (let ((lexenv *lexenv*))
      (and *allow-instrumenting*
           (policy lexenv (>= insert-step-conditions 2))
           (cond ((consp form)
                  (let ((op (car form)))
                    (or (and (consp op) (eq 'lambda (car op)))
                        (and (symbolp op)
                             (not (special-operator-p op))
                             (member (lexenv-find op funs) '(nil functional global-var))
                             (not (eq :macro (info :function :kind op)))
                             (step-symbol-p op)))))
                 ((symbolp form)
                  (and *step-arguments-p*
                       *allow-instrumenting*
                       (policy lexenv (= insert-step-conditions 3))
                       (not (consp (lexenv-find form vars)))
                       (not (constantp form))
                       (step-symbol-p form))))))))
