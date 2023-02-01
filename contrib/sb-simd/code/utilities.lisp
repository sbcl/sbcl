(in-package #:sb-simd-internals)

;;; Types

(deftype type-specifier ()
  '(or symbol cons))

(deftype non-nil-symbol ()
  '(and symbol (not null)))

(deftype function-name ()
  '(or non-nil-symbol (cons (eql setf) (cons non-nil-symbol null))))

(deftype index ()
  '(integer
    (#.(- (1- array-total-size-limit)))
    (#.(1- array-total-size-limit))))

;;; Functions

(defun index+ (&rest indices)
  (the index (apply #'+ indices)))

(define-compiler-macro index+ (&rest indices)
  `(the index (+ ,@(loop for index in indices collect `(the index ,index)))))

(defun index- (index &rest more-indices)
  (the index (apply #'- index more-indices)))

(define-compiler-macro index- (index &rest more-indices)
  `(the index (- (the index ,index) ,@(loop for index in more-indices collect `(the index ,index)))))

(defun index* (&rest indices)
  (the index (apply #'* indices)))

(define-compiler-macro index* (&rest indices)
  `(the index (* ,@(loop for index in indices collect `(the index ,index)))))

(defun ensure-package (name)
  (or (find-package name)
      (make-package name)))

(defun mksym (package &rest string-designators)
  (intern
   (apply #'concatenate 'string (mapcar #'string string-designators))
   package))

(defun prefixed-symbols (prefix n &optional (package *package*))
  (loop for index below n
        collect
        (mksym package prefix (format nil "~D" index))))

(declaim (notinline touch))
(defun touch (&rest arguments &aux value)
  (declare (ignore arguments))
  (declare (special value))
  (values-list (loop repeat (random 100) collect value)))

(defun required-argument (initarg)
  (error "Required argument: ~S" initarg))

(defun macroexpand-all (form &optional env)
  (let ((sb-walker::*walk-form-expand-macros-p* t))
    (sb-walker:walk-form form env)))

(defun ensure-list (x)
  (if (listp x)
      x
      (list x)))

(defun lambda-expression-p (x)
  (and (listp x)
       (> (list-length x) 1)
       (eq (first x) 'lambda)
       (listp (second x))))

(defun parse-function-name (function-name)
  (typecase function-name
    (non-nil-symbol
     (values function-name nil))
    ((cons (eql setf) (cons non-nil-symbol null))
     (values (second function-name) t))
    (otherwise
     (error "Not a valid function name: ~S" function-name))))

;;; Macros

(defmacro define-inline (name lambda-list &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,lambda-list ,@body)))

(defmacro define-notinline (name lambda-list &body body)
  `(progn
     (declaim (notinline ,name))
     (defun ,name ,lambda-list ,@body)))

(defun integer-type-specifier-inclusive-bounds (type-specifier)
  (assert (= 2 (length type-specifier)))
  (let ((bits (the (integer 1) (second type-specifier))))
    (ecase (first type-specifier)
      (unsigned-byte
       (values 0 (1- (expt 2 bits))))
      (signed-byte
       (values (- (expt 2 (1- bits)))
               (1- (expt 2 (1- bits))))))))
