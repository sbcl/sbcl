;;;; error-handling machinery for MAKE-MACRO-LAMBDA separated from
;;;; that code because the happy path can be handled
;;;; earlier in the bootstrap sequence than DEFINE-CONDITION can be

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

(define-condition defmacro-lambda-list-bind-error (error)
  ((kind :reader defmacro-lambda-list-bind-error-kind
         :initarg :kind)
   (name :reader defmacro-lambda-list-bind-error-name
         :initarg :name
         :initform nil)))

;;; shared logic for REPORTing variants of DEFMACRO-LAMBDA-LIST-BIND-ERROR:
;;; Set up appropriate prettying and indentation on STREAM, print some
;;; boilerplate related to CONDITION (an instance of
;;; DEFMACRO-LAMBDA-LIST-BIND-ERROR), then execute BODY.
(defmacro !printing-defmacro-lambda-list-bind-error ((condition stream)
                                                     &body body)
  `(%printing-defmacro-lambda-list-bind-error ,condition
                                              ,stream
                                              (lambda (,stream)
                                                (declare (type stream ,stream))
                                                ,@body)))
(defun %printing-defmacro-lambda-list-bind-error (condition stream fun)
  (declare (type stream stream) (type function fun))
  (pprint-logical-block (stream nil)
    (format stream
            "error while parsing arguments to ~A~@[ ~S~]:~2I~:@_"
            (defmacro-lambda-list-bind-error-kind condition)
            (defmacro-lambda-list-bind-error-name condition))
    (pprint-logical-block (stream nil)
      (funcall fun stream))))

(define-condition arg-count-error (defmacro-lambda-list-bind-error)
  ((args :reader arg-count-error-args :initarg :args)
   (lambda-list :reader arg-count-error-lambda-list
                :initarg :lambda-list)
   (minimum :reader arg-count-error-minimum :initarg :minimum)
   (maximum :reader arg-count-error-maximum :initarg :maximum))
  (:report
   (lambda (condition stream)
    (!printing-defmacro-lambda-list-bind-error (condition stream)
      (let* ((min (arg-count-error-minimum condition))
             (max (arg-count-error-maximum condition))
             (actual (arg-count-error-args condition))
             (n-actual (if (proper-list-p actual) (length actual) nil)))
       (format stream
               "~A elements in ~2I~_~:S ~
                ~I~_to satisfy lambda list ~2I~_~:S: ~I~_"
               (cond ((and n-actual (< n-actual min)) "too few")
                     ((and n-actual max (> n-actual max)) "too many")
                     (t "invalid number of"))
               actual (arg-count-error-lambda-list condition))
       (format stream
               (cond ((null max) "at least ~W expected")
                     ((= min max) "exactly ~W expected")
                     (t "between ~W and ~W expected"))
               min max)
       (cond ((and (atom actual) actual)
              (format stream ", but got a non-list"))
             ((cdr (last actual))
              (format stream ", but got an improper list"))
             (t
              (format stream ", but got ~d" n-actual))))))))

(define-condition defmacro-lambda-list-broken-key-list-error
                  (defmacro-lambda-list-bind-error)
  ((problem :reader defmacro-lambda-list-broken-key-list-error-problem
            :initarg :problem)
   (info :reader defmacro-lambda-list-broken-key-list-error-info
         :initarg :info))
  (:report (lambda (condition stream)
             (!printing-defmacro-lambda-list-bind-error (condition stream)
               (format stream
                       ;; FIXME: These should probably just be three
                       ;; subclasses of the base class, so that we don't
                       ;; need to maintain the set of tags both here and
                       ;; implicitly wherever this macro is used. (This
                       ;; might get easier once CLOS is initialized in
                       ;; cold init.)
                       (ecase
                           (defmacro-lambda-list-broken-key-list-error-problem
                             condition)
                         (:dotted-list
                          "dotted keyword/value list: ~S")
                         (:odd-length
                          "odd number of elements in keyword/value list: ~S")
                         (:unknown-keyword
                          ;; Todo: print the keyword portion of the actual args
                          ;;  "unknown keyword foo in (:A 1 :B ...);
                          ;;   expected one of ..."
                          "~{unknown keyword: ~S; expected one of ~
                           ~{~S~^, ~}~}"))
                       (defmacro-lambda-list-broken-key-list-error-info
                         condition))))))
