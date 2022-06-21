(in-package #:sb-simd-internals)

;;; An instruction set is a description for a set of data types and
;;; functions in a particular package.

(defclass instruction-set (printable)
  (;; The instruction set's name.
   (%name
    :type keyword
    :initarg :name
    :initform (required-argument :name)
    :reader instruction-set-name)
   ;; The package that holds the instruction set's symbols.
   (%package
    :type package
    :initarg :package
    :initform (required-argument :package)
    :reader instruction-set-package)
   ;; A thunk, returning whether the instruction set is currently available.
   ;; Such a run time check is needed in the case where an executable is
   ;; created on one machine and run on another machine.  In that case, some
   ;; of the instructions sets available on the former might not be available
   ;; on the latter.
   (%test
    :type function
    :initarg :test :initform (required-argument :test)
    :reader instruction-set-test)
   ;; A list of instruction sets included by this one.
   (%includes
    :type list
    :initarg :includes
    :initform '()
    :reader instruction-set-includes)
   ;; A hash table, mapping from function records of scalar functions to
   ;; lists of function records of SIMD functions.
   (%vectorizer-table
    :type hash-table
    :initform (make-hash-table :test #'eq)
    :reader instruction-set-vectorizer-table)))

(defmethod printable-slot-plist append ((instruction-set instruction-set))
  (list :name (instruction-set-name instruction-set)
        :package (instruction-set-package instruction-set)))

(defun instruction-set-p (x)
  (typep x 'instruction-set))

(defun instruction-set-available-p (instruction-set)
  (funcall (instruction-set-test instruction-set)))

;;; Returns a list containing the name of the supplied instruction set, and
;;; the names of all instruction sets that are directly or indirectly
;;; included by it.
(defun included-instruction-sets (instruction-set)
  (let ((result '()))
    (labels ((scan (instruction-set)
               (with-accessors ((includes instruction-set-includes)) instruction-set
                 (unless (member instruction-set result)
                   (push instruction-set result)
                   (mapcar #'scan includes)))))
      (scan instruction-set)
      (nreverse result))))

(defun register-vectorizer (X-record X.Y-record)
  (assert (scalar-function-record-p X-record))
  (assert (simd-function-record-p X.Y-record))
  (with-accessors ((vectorizer-table instruction-set-vectorizer-table))
      (function-record-instruction-set X.Y-record)
    (pushnew X.Y-record (gethash X-record vectorizer-table '()))))

(defun instruction-set-vectorizers (instruction-set X-record)
  (loop for instruction-set in (included-instruction-sets instruction-set)
        append
        (gethash X-record (instruction-set-vectorizer-table instruction-set) '())))

;;; A hash table, mapping from instruction set names or packages to
;;; instruction sets.
(defparameter *instruction-sets* (make-hash-table :test #'eq))

(defun find-instruction-set (designator &optional (errorp t))
  (or (gethash designator *instruction-sets*)
      (when errorp
        (typecase designator
          (symbol (error "There is no instruction set with the name ~S." designator))
          (package (error "There is not instruction set with the package ~S" designator))
          (otherwise (error "Not a valid instruction set designator: ~S" designator))))))

(defmethod make-load-form ((instruction-set instruction-set) &optional env)
  (declare (ignore env))
  `(find-instruction-set ',(instruction-set-name instruction-set)))

(defmethod shared-initialize :after
    ((instruction-set instruction-set) slot-names &key &allow-other-keys)
  (setf (gethash (instruction-set-name instruction-set) *instruction-sets*)
        instruction-set)
  (setf (gethash (instruction-set-package instruction-set) *instruction-sets*)
        instruction-set)
  instruction-set)

;;; The currently active instruction set.
(defvar *instruction-set*)

;;; Defining Instruction Sets

(defparameter *instruction-set-options*
  '(:include :test :scalars :simd-packs :simd-casts :reinterpret-casts
    :instructions :loads :stores :reffers
    :associatives :reducers :comparisons :unequals :ifs))

(defgeneric decode-record-definition (record-name expr))

(defmacro define-instruction-set (name &body options)
  ;; Ensure that only valid options are supplied.
  (dolist (option options)
    (unless (and (listp option)
                 (member (first option) *instruction-set-options*))
      (error "Not a valid instruction set option:~% ~S" option)))
  (flet ((decode (keyword decoder)
           (loop for (key . exprs) in options
                 when (eq key keyword)
                   append (mapcar decoder exprs)))
         (decode-include (expr)
           `(find-instruction-set ',expr))
         (record-decoder (record-name)
           (lambda (x)
             (decode-record-definition record-name x))))
    ;; The macro expansion of an instruction set is a very large expression
    ;; that is evaluated exactly once, so compiling it would be a waste of
    ;; resources.  Instead, we use SBCL's built-in interpreter.
    `(let ((sb-ext:*evaluator-mode* :interpret))
       (eval
        '(let ((*instruction-set*
                (make-instance 'instruction-set
                 :name ',name
                 :package ,(if (eq name :sb-simd)
                               (find-package "SB-SIMD")
                               (find-package (concatenate 'string "SB-SIMD-" (string name))))
                 :test (lambda () (and ,@(decode :test #'identity)))
                 :includes (list ,@(decode :include #'decode-include)))))
          ,@(decode :scalars (record-decoder 'value-record))
          ,@(decode :simd-packs (record-decoder 'simd-record))
          ,@(decode :instructions (record-decoder 'instruction-record))
          ,@(decode :loads (record-decoder 'load-record))
          ,@(decode :stores (record-decoder 'store-record))
          ,@(decode :reffers (record-decoder 'reffer-record))
          ,@(decode :associatives (record-decoder 'associative-record))
          ,@(decode :reducers (record-decoder 'reducer-record))
          ,@(decode :comparisons (record-decoder 'comparison-record))
          ,@(decode :unequals (record-decoder 'unequal-record))
          ,@(decode :ifs (record-decoder 'if-record))
          ,@(decode :simd-casts (record-decoder 'simd-cast-record))
          ,@(decode :reinterpret-casts (record-decoder 'reinterpret-cast-record)))))))
