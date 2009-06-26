;;;; FIXME: This should probably move to some separate tests or benchmarks
;;;; directory.

(in-package "SB-PCL")

(declaim (optimize (speed 3) (safety 0) (compilation-speed 0)))

(defvar *tests*)
(setq *tests* nil)

(defvar m (car (generic-function-methods #'shared-initialize)))
(defvar gf #'shared-initialize)
(defvar c (find-class 'standard-class))

(defclass str ()
  ((slot :initform nil :reader str-slot))
  (:metaclass structure-class))

(defvar str (make-instance 'str))

(push (cons "Time unoptimized slot-value. This is case (1) from notes.text. (standard)"
            '(time-slot-value m 'plist 10000))
      *tests*)
(push (cons "Time unoptimized slot-value. This is case (1) from notes.text. (standard)"
            '(time-slot-value m '%generic-function 10000))
      *tests*)
(push (cons "Time unoptimized slot-value. This is case (1) from notes.text. (structure)"
            '(time-slot-value str 'slot 10000))
      *tests*)
(defun time-slot-value (object slot-name n)
  (time (dotimes-fixnum (i n) (slot-value object slot-name))))

(push (cons "Time optimized slot-value outside of a defmethod. Case (2). (standard)"
            '(time-slot-value-function m 10000))
      *tests*)
(defun time-slot-value-function (object n)
  (time (dotimes-fixnum (i n) (slot-value object '%function))))

(push (cons "Time optimized slot-value outside of a defmethod. Case (2). (structure)"
            '(time-slot-value-slot str 10000))
      *tests*)
(defun time-slot-value-slot (object n)
  (time (dotimes-fixnum (i n) (slot-value object 'slot))))

(push (cons "Time one-class dfun."
            '(time-generic-function-methods gf 10000))
      *tests*)
(defun time-generic-function-methods (object n)
  (time (dotimes-fixnum (i n) (generic-function-methods object))))

(push (cons "Time one-index dfun."
            '(time-class-precedence-list c 10000))
      *tests*)
(defun time-class-precedence-list (object n)
  (time (dotimes-fixnum (i n) (class-precedence-list object))))

(push (cons "Time n-n dfun."
            '(time-method-function m 10000))
      *tests*)
(defun time-method-function (object n)
  (time (dotimes-fixnum (i n) (method-function object))))

(push (cons "Time caching dfun."
            '(time-class-slots c 10000))
      *tests*)
(defun time-class-slots (object n)
  (time (dotimes-fixnum (i n) (class-slots object))))

(push (cons "Time typep for classes."
            '(time-typep-standard-object m 10000))
      *tests*)
(defun time-typep-standard-object (object n)
  (time (dotimes-fixnum (i n) (typep object 'standard-object))))

(push (cons "Time default-initargs."
            '(time-default-initargs (find-class 'plist-mixin) 1000))
      *tests*)
(defun time-default-initargs (n)
  (time (dotimes-fixnum (i n) (default-initargs nil nil))))

(push (cons "Time make-instance."
            '(time-make-instance (find-class 'plist-mixin) 1000))
      *tests*)
(defun time-make-instance (class n)
  (time (dotimes-fixnum (i n) (make-instance class))))

(push (cons "Time constant-keys make-instance."
            '(time-constant-keys-make-instance 1000))
      *tests*)

(expanding-make-instance-toplevel
(defun constant-keys-make-instance (n)
  (dotimes-fixnum (i n) (make-instance 'plist-mixin))))

(precompile-random-code-segments)

(defun time-constant-keys-make-instance (n)
  (time (constant-keys-make-instance n)))

(defun expand-all-macros (form)
  (walk-form form nil (lambda (form context env)
                        (if (and (eq context :eval)
                                 (consp form)
                                 (symbolp (car form))
                                 (not (special-form-p (car form)))
                                 (macro-function (car form)))
                            (values (macroexpand form env))
                            form))))

(push (cons "Macroexpand meth-structure-slot-value"
            '(pprint (multiple-value-bind (pgf pm)
                         (prototypes-for-make-method-lambda
                          'meth-structure-slot-value)
                       (expand-defmethod
                        'meth-structure-slot-value pgf pm
                        nil '((object str))
                        '((lambda () (slot-value object 'slot)))
                        nil))))
      *tests*)

(push (cons "Show code for slot-value inside a defmethod for a structure-class. Case (3)."
            '(disassemble (meth-structure-slot-value str)))
      *tests*)
(defmethod meth-structure-slot-value ((object str))
  (lambda () (slot-value object 'slot)))

#|| ; interesting, but long. (produces 100 lines of output)
(push (cons "Macroexpand meth-standard-slot-value"
            '(pprint (expand-all-macros
                     (expand-defmethod-internal 'meth-standard-slot-value
                      nil '((object standard-method))
                      '((lambda () (slot-value object '%function)))
                      nil))))
      *tests*)
(push (cons "Show code for slot-value inside a defmethod for a standard-class. Case (4)."
            '(disassemble (meth-standard-slot-value m)))
      *tests*)
(defmethod meth-standard-slot-value ((object standard-method))
  (lambda () (slot-value object '%function)))
||#

(defun run-tests ()
  (dolist (doc+form (reverse *tests*))
    (format t "~&~%~A~%" (car doc+form))
    (pprint (cdr doc+form))
    (eval (cdr doc+form))))
