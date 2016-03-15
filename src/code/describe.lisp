;;;; the DESCRIBE system

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;; SB-IMPL, not SB!IMPL, since we're built in warm load.
(in-package "SB-IMPL")

;;;; Utils, move elsewhere.

(defun class-name-or-class (class)
  (let ((name (class-name class)))
    (if (eq class (find-class name nil))
        name
        class)))

;;;; the ANSI interface to function names (and to other stuff too)
;;; Note: this function gets called by the compiler (as of 1.0.17.x,
;;; in MAYBE-INLINE-SYNTACTIC-CLOSURE), and so although ANSI says
;;; we're allowed to return NIL here freely, it seems plausible that
;;; small changes to the circumstances under which this function
;;; returns non-NIL might have subtle consequences on the compiler.
;;; So it might be desirable to have the compiler not rely on this
;;; function, eventually.
(defun function-lambda-expression (fun)
  #+sb-doc
  "Return (VALUES DEFINING-LAMBDA-EXPRESSION CLOSURE-P NAME), where
  DEFINING-LAMBDA-EXPRESSION is NIL if unknown, or a suitable argument
  to COMPILE otherwise, CLOSURE-P is non-NIL if the function's definition
  might have been enclosed in some non-null lexical environment, and
  NAME is some name (for debugging only) or NIL if there is no name."
  (declare (type function fun))
  (etypecase fun
    #+sb-eval
    (sb-eval:interpreted-function
     (let ((name (sb-eval:interpreted-function-name fun))
           (lambda-list (sb-eval:interpreted-function-lambda-list fun))
           (declarations (sb-eval:interpreted-function-declarations fun))
           (body (sb-eval:interpreted-function-body fun)))
       (values `(lambda ,lambda-list
                  ,@(when declarations `((declare ,@declarations)))
                  ,@body)
               t name)))
    #+sb-fasteval
    (sb-interpreter:interpreted-function
     (sb-interpreter:fun-lambda-expression fun))
    (function
     (let* ((name (fun-name fun))
            (fun (%simple-fun-self (%fun-fun fun)))
            (code (sb-di::fun-code-header fun))
            (info (sb-kernel:%code-debug-info code))
            (source (if info (sb-c::debug-info-source info))))
       (cond ((and source (sb-c::debug-source-form source)
                   (eq (sb-c::debug-source-function source) fun))
              (values (sb-c::debug-source-form source) nil name))
             ((legal-fun-name-p name)
              (let ((exp (fun-name-inline-expansion name)))
                (values exp (not exp) name)))
             (t
              (values nil t name)))))))

;;; Prints X on a single line, limiting output length by *PRINT-RIGHT-MARGIN*
;;; -- good for printing object parts, etc.
(defun prin1-to-line (x &key (columns 1) (reserve 0))
  (let* ((line (write-to-string x :escape t :readably nil :lines 2 :circle t))
         (p (position #\newline line))
         (limit (truncate (- *print-right-margin* reserve) columns)))
    (flet ((trunc (&optional end)
             (let ((line-end (- limit 2)))
               (with-simple-output-to-string (s)
                 (write-string line s :end (if end
                                               (min end line-end)
                                               line-end))
                 (write-string ".." s)))))
      (cond (p
             (trunc p))
            ((> (length line) limit)
             (trunc))
            (t
             line)))))

(defun describe (object &optional (stream-designator *standard-output*))
  #+sb-doc
  "Print a description of OBJECT to STREAM-DESIGNATOR."
  (let ((stream (out-synonym-of stream-designator))
        (*print-right-margin* (or *print-right-margin* 72))
        (*print-circle* t)
        (*suppress-print-errors*
          (if (subtypep 'serious-condition *suppress-print-errors*)
              *suppress-print-errors*
              'serious-condition)))
    ;; Until sbcl-0.8.0.x, we did
    ;;   (FRESH-LINE STREAM)
    ;;   (PPRINT-LOGICAL-BLOCK (STREAM NIL)
    ;;     ...
    ;; here. However, ANSI's specification of DEFUN DESCRIBE,
    ;;   DESCRIBE exists as an interface primarily to manage argument
    ;;   defaulting (including conversion of arguments T and NIL into
    ;;   stream objects) and to inhibit any return values from
    ;;   DESCRIBE-OBJECT.
    ;; doesn't mention either FRESH-LINEing or PPRINT-LOGICAL-BLOCKing,
    ;; and the example of typical DESCRIBE-OBJECT behavior in ANSI's
    ;; specification of DESCRIBE-OBJECT will work poorly if we do them
    ;; here. (The example method for DESCRIBE-OBJECT does its own
    ;; FRESH-LINEing, which is a physical directive which works poorly
    ;; inside a pretty-printer logical block.)
    (handler-bind ((print-not-readable #'print-unreadably))
      (describe-object object stream))
    ;; We don't TERPRI here either (any more since sbcl-0.8.0.x), because
    ;; again ANSI's specification of DESCRIBE doesn't mention it and
    ;; ANSI's example of DESCRIBE-OBJECT does its own final TERPRI.
    (values)))

;;;; DESCRIBE-OBJECT
;;;;
;;;; Style guide:
;;;;
;;;; * Each interesting class has a primary method of its own.
;;;;
;;;; * Output looks like
;;;;
;;;;    object-self-string
;;;;      [object-type-string]
;;;;
;;;;    Block1:
;;;;      Sublabel1: text
;;;;      Sublabel2: text
;;;;
;;;;    Block2:
;;;;      ...
;;;;
;;;; * The newline policy that gets the whitespace right is for
;;;;   each block to both start and end with a newline.

(defgeneric object-self-string (x))

(defmethod object-self-string (x)
  (prin1-to-line x))

(defmethod object-self-string ((x symbol))
  (let ((*package* (find-package :keyword)))
    (prin1-to-string x)))

(defgeneric object-type-string (x))

(defmethod object-type-string (x)
  (let ((type (class-name-or-class (class-of x))))
    (if (symbolp type)
        (string-downcase type)
        (prin1-to-string type))))

(defmethod object-type-string ((x cons))
  (if (listp (cdr x)) "list" "cons"))

(defmethod object-type-string ((x hash-table))
  "hash-table")

(defmethod object-type-string ((x condition))
  "condition")

(defmethod object-type-string ((x structure-object))
  "structure-object")

(defmethod object-type-string ((x standard-object))
  "standard-object")

(defmethod object-type-string ((x function))
  (typecase x
    (simple-fun "compiled function")
    (closure "compiled closure")
    ((or #+sb-fasteval sb-interpreter:interpreted-function
         #+sb-eval sb-eval:interpreted-function) "interpreted function")
    (generic-function "generic-function")
    (t "funcallable-instance")))

(defmethod object-type-string ((x stream))
  "stream")

(defmethod object-type-string ((x sb-gray:fundamental-stream))
  "gray stream")

(defmethod object-type-string ((x package))
  "package")

(defmethod object-type-string ((x array))
  (cond ((or (stringp x) (bit-vector-p x))
         (format nil "~@[simple-~*~]~A"
                 (typep x 'simple-array)
                 (typecase x
                   (base-string "base-string")
                   (string "string")
                   (t "bit-vector"))))
        (t
         (if (simple-vector-p x)
             "simple-vector"
             (format nil "~@[simple ~*~]~@[specialized ~*~]~:[array~;vector~]"
                     (typep x 'simple-array)
                     (neq t (array-element-type x))
                     (vectorp x))))))

(defmethod object-type-string ((x character))
  (typecase x
    (standard-char "standard-char")
    (base-char "base-char")
    (t "character")))

(defun print-standard-describe-header (x stream)
  (format stream "~&~A~%  [~A]~%"
          (object-self-string x)
          (object-type-string x)))

(defgeneric describe-object (x stream))

;;; Catch-all.

(defmethod describe-object ((x t) s)
  (print-standard-describe-header x s))

(defmethod describe-object ((x cons) s)
  (print-standard-describe-header x s)
  (describe-function x nil s))

(defmethod describe-object ((x function) s)
  (print-standard-describe-header x s)
  (describe-function nil x s))

(defmethod describe-object ((x class) s)
  (print-standard-describe-header x s)
  (describe-class nil x s)
  (describe-instance x s))

(defmethod describe-object ((x sb-pcl::slot-object) s)
  (print-standard-describe-header x s)
  (describe-instance x s))

(defmethod describe-object ((x character) s)
  (print-standard-describe-header x s)
  (format s "~%Char-code: ~S" (char-code x))
  (format s "~%Char-name: ~A" (char-name x)))

(defmethod describe-object ((x array) s)
  (print-standard-describe-header x s)
  (format s "~%Element-type: ~S" (array-element-type x))
  (if (vectorp x)
      (if (array-has-fill-pointer-p x)
          (format s "~%Fill-pointer: ~S~%Size: ~S"
                  (fill-pointer x)
                  (array-total-size x))
          (format s "~%Length: ~S" (length x)))
      (format s "~%Dimensions: ~S" (array-dimensions x)))
  (let ((*print-array* nil))
    (unless (typep x 'simple-array)
      (format s "~%Adjustable: ~A" (if (adjustable-array-p x) "yes" "no"))
      (multiple-value-bind (to offset) (array-displacement x)
        (if (format s "~%Displaced-to: ~A~%Displaced-offset: ~S"
                    (prin1-to-line to)
                    offset)
            (format s "~%Displaced: no"))))
    (when (and (not (array-displacement x)) (array-header-p x))
      (format s "~%Storage vector: ~A"
              (prin1-to-line (array-storage-vector x))))
    (terpri s)))

(defmethod describe-object ((x hash-table) s)
  (print-standard-describe-header x s)
  ;; Don't print things which are already apparent from the printed
  ;; representation -- COUNT, TEST, and WEAKNESS
  (format s "~%Occupancy: ~,1F" (float (/ (hash-table-count x)
                                          (hash-table-size x))))
  (format s "~%Rehash-threshold: ~S" (hash-table-rehash-threshold x))
  (format s "~%Rehash-size: ~S" (hash-table-rehash-size x))
  (format s "~%Size: ~S" (hash-table-size x))
  (format s "~%Synchronized: ~A" (if (hash-table-synchronized-p x) "yes" "no"))
  (terpri s))

(defmethod describe-object ((symbol symbol) stream)
  (print-standard-describe-header symbol stream)

  ;; Describe the value cell.
  (describe-variable symbol stream)

  ;; TODO: We could grovel over all packages looking for and
  ;; reporting other phenomena, e.g. IMPORT and SHADOW, or
  ;; availability in some package even after (SYMBOL-PACKAGE SYMBOL) has
  ;; been set to NIL.
  ;;
  ;; TODO: It might also be nice to describe (find-package symbol)
  ;; if one exists. Maybe not all the exports, etc, but the package
  ;; documentation.
  (describe-function symbol nil stream)
  (describe-class symbol nil stream)

  ;; Type specifier
  (describe-type symbol stream)

  (awhen (sb-c::policy-quality-name-p symbol)
    (pprint-logical-block (stream nil)
      (pprint-newline :mandatory stream)
      (pprint-indent :block 2 stream)
      (format stream "~A names a~:[ dependent~;n~] optimization policy quality:"
              symbol (minusp it))
      (describe-documentation symbol 'optimize stream t))
    (terpri stream))

  ;; Print out properties.
  (let ((plist (symbol-plist symbol)))
    (when plist
      (pprint-logical-block (stream nil)
        (format stream "~%Symbol-plist:")
        (pprint-indent :block 2 stream)
        (sb-pcl::doplist (key value) plist
          (format stream "~@:_~A -> ~A"
                  (prin1-to-line key :columns 2 :reserve 5)
                  (prin1-to-line value :columns 2 :reserve 5))))
      (terpri stream))))

(defmethod describe-object ((package package) stream)
  (print-standard-describe-header package stream)
  (pprint-logical-block (stream nil)
    (describe-documentation package t stream)
    (flet ((humanize (list)
             (sort (mapcar (lambda (x)
                             (if (packagep x)
                                 (package-name x)
                                 x))
                           list)
                   #'string<))
           (out (label list)
             (describe-stuff label list stream :escape nil)))
      (let ((exports nil))
        (do-external-symbols (ext package)
          (push ext exports))
        #+sb-package-locks
        (let ((implemented (humanize (package-implemented-by-list package)))
              (implements (humanize (package-implements-list package)))
              (this (list (package-name package))))
          (when (package-locked-p package)
            (format stream "~@:_Locked."))
          (when (set-difference implemented this :test #'string=)
            (out "Implemented-by-list" implemented))
          (when (set-difference implements this :test #'string=)
            (out "Implements-list" implements)))
        (out "Nicknames" (humanize (package-nicknames package)))
        (out "Use-list" (humanize (package-use-list package)))
        (out "Used-by-list" (humanize (package-used-by-list package)))
        (out "Shadows" (humanize (package-shadowing-symbols package)))
        (out "Exports" (humanize exports))
        (format stream "~@:_~S internal symbols."
                (package-internal-symbol-count package))))
    (terpri stream)))

;;;; Helpers to deal with shared functionality

(defun describe-deprecation (namespace name stream)
  (multiple-value-bind (state since replacements)
      (deprecated-thing-p namespace name)
    (when state
      (destructuring-bind (software version) since
        (format stream "~@:_In ~A deprecation since ~@[~A ~]version ~A.~
                        ~@[ ~/sb-impl::print-deprecation-replacements/~]"
                state software version replacements)))))

(defun describe-class (name class stream)
  (binding* ((by-name (not class))
             ((name class) (if class
                               (values (class-name class) name)
                               (values name (find-class name nil)))))
    (when class
      (let ((metaclass-name (class-name (class-of class))))
        (pprint-logical-block (stream nil)
          (when by-name
            (format stream "~@:_~A names the ~(~A~) ~S:"
                    name metaclass-name class)
            (pprint-indent :block 2 stream))
          (describe-deprecation 'type name stream)
          (describe-documentation class t stream)
          (when (sb-mop:class-finalized-p class)
            (describe-stuff "Class precedence-list"
                            (mapcar #'class-name-or-class (sb-mop:class-precedence-list class))
                            stream))
          (describe-stuff "Direct superclasses"
                          (mapcar #'class-name-or-class (sb-mop:class-direct-superclasses class))
                          stream)
          (let ((subs (mapcar #'class-name-or-class (sb-mop:class-direct-subclasses class))))
            (if subs
                (describe-stuff "Direct subclasses" subs stream)
                (format stream "~@:_No subclasses.")))
          (unless (sb-mop:class-finalized-p class)
            (format stream "~@:_Not yet finalized."))
          (if (eq 'structure-class metaclass-name)
              (let* ((dd (find-defstruct-description name))
                     (slots (dd-slots dd)))
                (if slots
                    (format stream "~@:_Slots:~:{~@:_  ~S~
                                    ~@:_    Type: ~A ~@[~A~]~
                                    ~@:_    Initform: ~S~}"
                            (mapcar (lambda (dsd)
                                      (list
                                       (dsd-name dsd)
                                       (dsd-type dsd)
                                       (unless (eq t (dsd-raw-type dsd))
                                         "(unboxed)")
                                       (dsd-default dsd)))
                                    slots))
                    (format stream "~@:_No slots.")))
              (let ((slots (sb-mop:class-direct-slots class)))
                (if slots
                    (format stream "~@:_Direct slots:~:{~@:_  ~S~
                                    ~@[~@:_    Type: ~S~]~
                                    ~@[~@:_    Allocation: ~S~]~
                                    ~@[~@:_    Initargs: ~{~S~^, ~}~]~
                                    ~@[~@:_    Initform: ~S~]~
                                    ~@[~@:_    Readers: ~{~S~^, ~}~]~
                                    ~@[~@:_    Writers: ~{~S~^, ~}~]~
                                    ~@[~@:_    Documentation:~@:_     ~@<~@;~A~:>~]~}"
                            (mapcar (lambda (slotd)
                                      (list (sb-mop:slot-definition-name slotd)
                                            (let ((type (sb-mop:slot-definition-type slotd)))
                                              (unless (eq t type) type))
                                            (let ((alloc (sb-mop:slot-definition-allocation slotd)))
                                              (unless (eq :instance alloc) alloc))
                                            (sb-mop:slot-definition-initargs slotd)
                                            (sb-mop:slot-definition-initform slotd)
                                            (sb-mop:slot-definition-readers slotd)
                                            (sb-mop:slot-definition-writers slotd)
                                            ;; FIXME: does this get the prefix right?
                                            (quiet-doc slotd t)))
                                    slots))
                    (format stream "~@:_No direct slots."))))
          (pprint-indent :block 0 stream)
          (pprint-newline :mandatory stream))))))

(defun describe-instance (object stream)
  (let* ((class (class-of object))
         (slotds (sb-mop:class-slots class))
         (max-slot-name-length 0)
         (plist nil))

    ;; Figure out a good width for the slot-name column.
    (flet ((adjust-slot-name-length (name)
             (setf max-slot-name-length
                   (max max-slot-name-length (length (symbol-name name))))))
      (dolist (slotd slotds)
        (adjust-slot-name-length (sb-mop:slot-definition-name slotd))
        (push slotd (getf plist (sb-mop:slot-definition-allocation slotd))))
      (setf max-slot-name-length  (min (+ max-slot-name-length 3) 30)))

    ;; Now that we know the width, we can print.
    (flet ((describe-slot (name value)
             (format stream "~%  ~A~VT = ~A" name max-slot-name-length
                     (prin1-to-line value))))
      (sb-pcl::doplist (allocation slots) plist
        (format stream "~%Slots with ~S allocation:" allocation)
        (dolist (slotd (nreverse slots))
          (describe-slot
           (sb-mop:slot-definition-name slotd)
           (sb-pcl::slot-value-or-default object (sb-mop:slot-definition-name slotd))))))
    (unless slotds
      (format stream "~@:_No slots."))
    (terpri stream)))

(defun quiet-doc (object type)
  (handler-bind ((warning #'muffle-warning))
    (documentation object type)))

(defun describe-documentation (object type stream &optional undoc newline)
  (let ((doc (quiet-doc object type)))
    (cond (doc
           (format stream "~@:_Documentation:~@:_")
           (pprint-logical-block (stream nil :per-line-prefix "  ")
             (princ doc stream)))
          (undoc
           (format stream "~@:_(undocumented)")))
    (when newline
      (pprint-newline :mandatory stream))))

(defun describe-stuff (label list stream &key (escape t))
  (when list
    (if escape
        (format stream "~@:_~A:~@<~;~{ ~S~^,~:_~}~;~:>" label list)
        (format stream "~@:_~A:~@<~;~{ ~A~^,~:_~}~;~:>" label list))))

(defun describe-variable (name stream)
  (let* ((kind (info :variable :kind name))
         (wot (ecase kind
                (:special "a special variable")
                (:macro "a symbol macro")
                (:constant "a constant variable")
                (:global "a global variable")
                (:unknown "an undefined variable")
                (:alien "an alien variable"))))
    (when (and (eq kind :unknown) (not (boundp name)))
      (return-from describe-variable))
    (pprint-logical-block (stream nil)
      (format stream "~@:_~A names ~A:" name wot)
      (pprint-indent :block 2 stream)
      (describe-deprecation 'variable name stream)
      (when (eq (info :variable :where-from name) :declared)
        (format stream "~@:_Declared type: ~S"
                (type-specifier (info :variable :type name))))
      (when (info :variable :always-bound name)
        (format stream "~@:_Declared always-bound."))
      (cond
        ((eq kind :alien)
         (let ((info (info :variable :alien-info name)))
           (format stream "~@:_Value: ~S" (eval name))
           (format stream "~@:_Type: ~S"
                   (sb-alien-internals:unparse-alien-type
                    (sb-alien::heap-alien-info-type info)))
           (format stream "~@:_Address: #x~8,'0X"
                   (sap-int (sb-alien::heap-alien-info-sap info)))))
        ((eq kind :macro)
         (let ((expansion (info :variable :macro-expansion name)))
           (format stream "~@:_Expansion: ~S" expansion)))
        ((boundp name)
         (format stream "~:@_Value: ~S" (symbol-value name)))
        ((not (eq kind :unknown))
         (format stream "~:@_Currently unbound.")))
      (describe-documentation name 'variable stream)
      (terpri stream))))

(defun describe-lambda-list (lambda-list stream)
  (let ((*print-circle* nil)
        (*print-level* 24)
        (*print-length* 24))
    (format stream "~@:_Lambda-list: ~:A" lambda-list)))

(defun describe-function-source (function stream)
  (if (compiled-function-p (the function function))
      (let* ((code (fun-code-header (%fun-fun function)))
             (info (sb-kernel:%code-debug-info code)))
        (when info
          (let ((source (sb-c::debug-info-source info)))
            (when source
              (let ((namestring (sb-c::debug-source-namestring source)))
                ;; This used to also report the times the source was created
                ;; and compiled, but that seems more like noise than useful
                ;; information -- but FWIW that are to be had as
                ;; SB-C::DEBUG-SOUCE-CREATED/COMPILED.
                (cond (namestring
                       (format stream "~@:_Source file: ~A" namestring))
                      ((sb-di:debug-source-form source)
                       (format stream "~@:_Source form:~@:_  ~S"
                               (sb-di:debug-source-form source)))))))))
      (let ((source
             (typecase function
               #+sb-eval
               (sb-eval:interpreted-function
                (sb-eval:interpreted-function-source-location function))
               #+sb-fasteval
               (sb-interpreter:interpreted-function
                (sb-interpreter:fun-source-location function)))))
        (when source
          (let ((namestring (sb-c:definition-source-location-namestring source)))
            (when namestring
              (format stream "~@:_Source file: ~A" namestring)))))))

(defun describe-function (name function stream)
  (let ((name (if function (fun-name function) name)))
    (if (not (or function (and (legal-fun-name-p name) (fboundp name))))
        ;; Not defined, but possibly the type is declared, or we have
        ;; compiled calls to it.
        (when (legal-fun-name-p name)
          (multiple-value-bind (from sure) (info :function :where-from name)
            (when (or (eq :declared from) (and sure (eq :assumed from)))
              (pprint-logical-block (stream nil)
                (format stream "~%~A names an undefined function" name)
                (pprint-indent :block 2 stream)
                (format stream "~@:_~:(~A~) type: ~S"
                        from
                        (type-specifier (proclaimed-ftype name)))))))
        ;; Defined.
        (multiple-value-bind (fun what lambda-list derived-type declared-type
                              inline methods)
            (cond ((and (not function) (symbolp name) (special-operator-p name))
                   ;; The function in the symbol is irrelevant.
                   ;; Use the def-ir1-translator function for source location.
                   (let ((fun (info :function :ir1-convert name)))
                     (values fun "a special operator" (%fun-lambda-list fun))))
                  ((and (not function) (symbolp name) (macro-function name))
                   (let ((fun (macro-function name)))
                     (values fun "a macro" (%fun-lambda-list fun))))
                  (t
                   (let* ((fun (or function (fdefinition name)))
                          (derived-type (and function
                                             (%fun-type function)))
                          (legal-name-p (legal-fun-name-p name))
                          (ctype (and legal-name-p
                                      (proclaimed-ftype name)))
                          (type (and ctype (type-specifier ctype)))
                          (from (and legal-name-p
                                     (info :function :where-from name)))
                          declared-type)
                     (cond ((not type))
                           ((eq from :declared)
                            (setf declared-type type))
                           ((and (not derived-type)
                                 (member from '(:defined-method :defined)))
                            (setf derived-type type)))
                     (unless derived-type
                       (setf derived-type (%fun-type fun)))
                     (if (typep fun 'standard-generic-function)
                         (values fun
                                 "a generic function"
                                 (sb-mop:generic-function-lambda-list fun)
                                 derived-type
                                 declared-type
                                 nil
                                 (or (sb-mop:generic-function-methods fun)
                                     :none))
                         (values fun
                                 (if (compiled-function-p fun)
                                     "a compiled function"
                                     "an interpreted function")
                                 (%fun-lambda-list fun)
                                 derived-type
                                 declared-type
                                 (cons
                                  (info :function :inlinep name)
                                  (info :function :inline-expansion-designator
                                        name)))))))
          (pprint-logical-block (stream nil)
            (unless function
              (format stream "~%~A names ~A:" name what)
              (pprint-indent :block 2 stream))
            (describe-deprecation 'function name stream)
            (describe-lambda-list lambda-list stream)
            (when declared-type
              (format stream "~@:_Declared type: ~S" declared-type))
            (when (and derived-type
                       (not (equal declared-type derived-type)))
              (format stream "~@:_Derived type: ~S" derived-type))
            (describe-documentation name 'function stream)
            (when (car inline)
              (format stream "~@:_Inline proclamation: ~
                              ~A (~:[no ~;~]inline expansion available)"
                      (car inline)
                      (cdr inline)))
            (awhen (info :function :info name)
              (awhen (sb-c::decode-ir1-attributes (sb-c::fun-info-attributes it))
                  (format stream "~@:_Known attributes: ~(~{~A~^, ~}~)" it)))
            (when methods
              (format stream "~@:_Method-combination: ~S"
                      (sb-pcl::method-combination-type-name
                       (sb-pcl:generic-function-method-combination fun)))
              (cond ((eq :none methods)
                     (format stream "~@:_No methods."))
                    (t
                     (pprint-newline :mandatory stream)
                     (pprint-logical-block (stream nil)
                       (format stream "Methods:")
                       (dolist (method methods)
                         (pprint-indent :block 2 stream)
                         (format stream "~@:_(~A ~{~S ~}~:S)"
                                 name
                                 (method-qualifiers method)
                                 (sb-pcl::unparse-specializers
                                  fun (sb-mop:method-specializers method)))
                         (pprint-indent :block 4 stream)
                         (describe-documentation method t stream nil))))))
            (describe-function-source fun stream)
            (terpri stream)))))
  (unless function
    (awhen (and (legal-fun-name-p name) (compiler-macro-function name))
      (pprint-logical-block (stream nil)
        (format stream "~@:_~A has a compiler-macro:" name)
        (pprint-indent :block 2 stream)
        (describe-documentation it t stream)
        (describe-function-source it stream))
      (terpri stream))
    (when (and (consp name) (eq 'setf (car name)) (not (cddr name)))
      (let* ((name2 (second name))
             (expander (info :setf :expander name2)))
        (cond ((typep expander '(and symbol (not null)))
               (pprint-logical-block (stream nil)
                 (format stream "~&~A has setf-expansion: ~S"
                         name expander)
                 (pprint-indent :block 2 stream)
                 (describe-documentation name2 'setf stream))
               (terpri stream))
              (expander
               (when (listp expander)
                 (setq expander (cdr expander)))
               (pprint-logical-block (stream nil)
                 (format stream "~&~A has a complex setf-expansion:"
                         name)
                 (pprint-indent :block 2 stream)
                 (describe-lambda-list (%fun-lambda-list expander) stream)
                 (describe-documentation name2 'setf stream t)
                 (describe-function-source expander stream))
               (terpri stream)))))
    (when (symbolp name)
      (describe-function `(setf ,name) nil stream))))

(defun describe-type (name stream)
  (let* ((kind (info :type :kind name))
         (fun (and kind (info :type :expander name)))
         (fun (if (listp fun) (car fun) fun)))
    (when fun
      (pprint-newline :mandatory stream)
      (pprint-logical-block (stream nil)
        (format stream "~@:_~A names a ~@[primitive~* ~]type-specifier:"
                name (eq kind :primitive))
        (pprint-indent :block 2 stream)
        (describe-deprecation 'type name stream)
        (describe-documentation name 'type stream (eq t fun))
        (when (functionp fun)
          (describe-lambda-list (%fun-lambda-list fun) stream)
          (multiple-value-bind (expansion ok)
              (handler-case (typexpand-1 name)
                (error () (values nil nil)))
            (when ok
              (format stream "~@:_Expansion: ~S" expansion)))))
      (terpri stream))))
