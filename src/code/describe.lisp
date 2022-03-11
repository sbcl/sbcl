;;;; the DESCRIBE system

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

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
  "Return (VALUES DEFINING-LAMBDA-EXPRESSION CLOSURE-P NAME), where
  DEFINING-LAMBDA-EXPRESSION is NIL if unknown, or a suitable argument
  to COMPILE otherwise, CLOSURE-P is non-NIL if the function's definition
  might have been enclosed in some non-null lexical environment, and
  NAME is some name (for debugging only) or NIL if there is no name."
  (declare (type function fun))
  (etypecase fun
    (interpreted-function
     #+sb-eval
     (let ((name (sb-eval:interpreted-function-name fun))
           (lambda-list (sb-eval:interpreted-function-lambda-list fun))
           (declarations (sb-eval:interpreted-function-declarations fun))
           (body (sb-eval:interpreted-function-body fun)))
       (values `(lambda ,lambda-list
                  ,@(when declarations `((declare ,@declarations)))
                  ,@body)
               t name))
     #+sb-fasteval
     (sb-interpreter:fun-lambda-expression fun))
    (function
     (let ((name (%fun-name fun))
           (fun (%fun-fun fun)))
       (acond ((%simple-fun-lexpr fun)
               (values it t name))
              ((legal-fun-name-p name)
               (let ((exp (fun-name-inline-expansion name)))
                 ;; Isn't this (NOT EXP) wrong if it's a syntactic closure?
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
               (%with-output-to-string (s)
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

(defun call-as-describe-block (thunk stream format-control format-arguments)
  (pprint-logical-block (stream nil)
    (pprint-newline :mandatory stream)
    (when format-control
      (apply #'format stream format-control format-arguments)
      (pprint-indent :block 2 stream))
    (when thunk
      (funcall thunk stream)))
  (terpri stream))

(defmacro describe-block ((stream-symbol
                           &optional format-control &rest format-arguments)
                          &body body)
  `(call-as-describe-block
    ,(if body
         `(lambda (,stream-symbol) ,@body)
         nil)
    ,stream-symbol ,format-control (list ,@format-arguments)))

(defun describe (object &optional (stream-designator *standard-output*))
  "Print a description of OBJECT to STREAM-DESIGNATOR."
  ;; This DECLARE works around a compiler bug that FTYPE does not force
  ;; type-checking of the optional argument.
  (declare (stream-designator stream-designator))
  (let ((stream (out-stream-from-designator stream-designator))
        (*print-right-margin* (or *print-right-margin* 72))
        (*print-circle* t)
        (*print-pretty* t)
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

;;;; DESCRIBE-OBJECT Protocol
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

(defgeneric describe-object (object stream))

(defgeneric object-self-string (object))

(defgeneric object-type-string (object))

;;; Methods for builtin objects

(defmethod object-self-string ((object t))
  (prin1-to-line object))

(defmethod object-self-string ((object symbol))
  (let ((*package* (find-package :keyword)))
    (prin1-to-string object)))

(defmethod object-type-string ((object t))
  (let ((type (class-name-or-class (class-of object))))
    (if (symbolp type)
        (string-downcase type)
        (prin1-to-string type))))

(defmethod object-type-string ((object cons))
  (if (listp (cdr object)) "list" "cons"))

(defmethod object-type-string ((object function))
  (typecase object
    (simple-fun "compiled function")
    (closure "compiled closure")
    (interpreted-function "interpreted function")
    (generic-function "generic-function")
    (t "funcallable-instance")))

(defmethod object-type-string ((object array))
  (cond
    ((or (stringp object) (bit-vector-p object))
     (format nil "~@[simple-~*~]~A"
             (typep object 'simple-array)
             (typecase object
               (base-string "base-string")
               (string "string")
               (t "bit-vector"))))
    ((simple-vector-p object)
     "simple-vector")
    (t
     (format nil "~@[simple ~*~]~@[specialized ~*~]~:[array~;vector~]"
             (typep object 'simple-array)
             (neq t (array-element-type object))
             (vectorp object)))))

(defmethod object-type-string ((object character))
  (typecase object
    (standard-char "standard-char")
    (base-char "base-char")
    #+sb-unicode (t "character"))) ; unreachable if no unicode

(macrolet ((def (class &optional (string (string-downcase (class-name (find-class class)))))
             `(defmethod object-type-string ((object ,class))
                ,string)))

  (def hash-table)
  (def condition)
  (def structure-object)
  (def standard-object)
  (def stream)
  (def sb-gray:fundamental-stream "gray stream")
  (def package))

(defun print-standard-describe-header (object stream)
  (format stream "~&~A~%  [~A]~%"
          (object-self-string object)
          (object-type-string object)))

;;; Catch-all.

(defmethod describe-object ((object t) stream)
  (print-standard-describe-header object stream))

(defmethod describe-object ((object cons) stream)
  (print-standard-describe-header object stream)
  (describe-function object nil stream))

(defmethod describe-object ((object function) stream)
  (print-standard-describe-header object stream)
  (describe-function nil object stream)
  (when (funcallable-instance-p object)
    (describe-instance object stream)))

(defmethod describe-object ((object class) stream)
  (print-standard-describe-header object stream)
  (describe-class nil object stream)
  (describe-instance object stream))

(defmethod describe-object ((object sb-pcl::slot-object) stream)
  (print-standard-describe-header object stream)
  (describe-instance object stream))

(defmethod describe-object ((object pathname) stream)
  (print-standard-describe-header object stream)
  (loop for name across #(host device directory name type version)
        for i from (get-dsd-index pathname host)
        do (awhen (%instance-ref object i)
             (format stream "~%  ~10A = ~A" name
                     (prin1-to-line (if (eq name 'directory) (car it) it)))))
  (terpri stream))

(defmethod describe-object ((object character) stream)
  (print-standard-describe-header object stream)
  (format stream "~%Char-code: ~S~%Char-name: ~A"
          (char-code object) (char-name object)))

(defmethod describe-object ((object array) stream)
  (print-standard-describe-header object stream)
  (format stream "~%Element-type: ~/sb-impl:print-type-specifier/"
          (array-element-type object))
  (cond
    ((not (vectorp object))
     (format stream "~%Dimensions: ~S" (array-dimensions object)))
    ((array-has-fill-pointer-p object)
     (format stream "~%Fill-pointer: ~S~%Size: ~S"
             (fill-pointer object)
             (array-total-size object)))
    (t
     (format stream "~%Length: ~S" (length object))))
  (let ((*print-array* nil))
    (unless (typep object 'simple-array)
      (format stream "~%Adjustable: ~:[no~;yes~]" (adjustable-array-p object))
      (multiple-value-bind (to offset) (array-displacement object)
        (if to
            (format stream "~%Displaced-to: ~A~%Displaced-offset: ~S"
                    (prin1-to-line to) offset)
            (format stream "~%Displaced: no"))))
    (when (and (not (array-displacement object)) (array-header-p object))
      (format stream "~%Storage vector: ~A"
              (prin1-to-line (array-storage-vector object))))
    (terpri stream)))

(defmethod describe-object ((object hash-table) stream)
  (print-standard-describe-header object stream)
  ;; Don't print things which are already apparent from the printed
  ;; representation -- COUNT, TEST, and WEAKNESS
  (format stream "~%Occupancy: ~,1F" (float (/ (hash-table-count object)
                                               (hash-table-size object))))
  (format stream "~%Rehash-threshold: ~S" (hash-table-rehash-threshold object))
  (format stream "~%Rehash-size: ~S" (hash-table-rehash-size object))
  (format stream "~%Size: ~S" (hash-table-size object))
  (format stream "~%Synchronized: ~:[no~;yes~]" (hash-table-synchronized-p object))
  (terpri stream))

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

  ;; Declaration specifier
  (describe-declaration symbol stream)

  (awhen (sb-c::policy-quality-name-p symbol)
    (describe-block (stream "~A names a~:[ dependent~;n~] optimization policy quality:"
                            symbol (minusp it))
      (describe-documentation symbol 'optimize stream t)))

  ;; Print out properties.
  (let ((plist (symbol-plist symbol)))
    (when plist
      (describe-block (stream "Symbol-plist:")
        (doplist (key value) plist
          (format stream "~@:_~A -> ~A"
                  (prin1-to-line key :columns 2 :reserve 5)
                  (prin1-to-line value :columns 2 :reserve 5)))))))

(defmethod describe-object ((object package) stream)
  (print-standard-describe-header object stream)
  (describe-block (stream)
    (describe-documentation object t stream)
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
        (do-external-symbols (ext object)
          (push ext exports))
        (let ((implemented (humanize (package-implemented-by-list object)))
              (implements (humanize (package-implements-list object)))
              (this (list (package-name object))))
          (when (package-locked-p object)
            (format stream "~@:_Locked."))
          (when (set-difference implemented this :test #'string=)
            (out "Implemented-by-list" implemented))
          (when (set-difference implements this :test #'string=)
            (out "Implements-list" implements)))
        (out "Nicknames" (humanize (package-nicknames object)))
        (out "Use-list" (humanize (package-use-list object)))
        (out "Used-by-list" (humanize (package-used-by-list object)))
        (out "Shadows" (humanize (package-shadowing-symbols object)))
        (out "Exports" (humanize exports))
        (format stream "~@:_~S internal symbols."
                (package-internal-symbol-count object))))))

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
                               (values (class-name class) class)
                               (values name (find-class name nil)))))
    (when class
      (let ((metaclass-name (class-name (class-of class))))
        (describe-block (stream (when by-name "~A names the ~(~A~) ~S:")
                                name metaclass-name class)
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
          (when (eq :sealed (classoid-state (sb-pcl::class-classoid class)))
            (format stream "~@:_Sealed."))
          (if (eq 'structure-class metaclass-name)
              (let* ((dd (find-defstruct-description name))
                     (slots (dd-slots dd)))
                (if slots
                    (format stream "~@:_Slots:~:{~@:_  ~S~
                                    ~@:_    Type: ~/sb-impl:print-type-specifier/ ~@[~A~]~
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
                                    ~@[~@:_    Type: ~/sb-impl:print-type-specifier/~]~
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
                    (format stream "~@:_No direct slots.")))))))))

(defun describe-instance (object stream)
  (let* ((class (class-of object))
         (slotds (sb-mop:class-slots class))
         (max-slot-name-length 30)
         (plist nil))

    ;; Figure out a good width for the slot-name column.
    (flet ((adjust-slot-name-length (name)
             (setf max-slot-name-length
                   (max max-slot-name-length (length (symbol-name name))))))
      (dolist (slotd slotds)
        (adjust-slot-name-length (sb-mop:slot-definition-name slotd))
        (push slotd (getf plist (sb-mop:slot-definition-allocation slotd)))))

    ;; Now that we know the width, we can print.
    (flet ((describe-slot (name value)
             (format stream "~%  ~VA = ~A"
                     max-slot-name-length name (prin1-to-line value))))
      (doplist (allocation slots) plist
        (format stream "~%Slots with ~S allocation:" allocation)
        (dolist (slotd (nreverse slots))
          (describe-slot
           (sb-mop:slot-definition-name slotd)
           (sb-pcl::slot-value-for-printing object (sb-mop:slot-definition-name slotd))))))
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
    (describe-block (stream "~A names ~A:" name wot)
      (describe-deprecation 'variable name stream)
      (when (eq (info :variable :where-from name) :declared)
        (format stream "~@:_Declared type: ~/sb-impl:print-type/"
                (info :variable :type name)))
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
      (describe-documentation name 'variable stream))))

(defun describe-lambda-list (lambda-list stream)
  (let ((*print-circle* nil)
        (*print-level* 24)
        (*print-length* 100))
    (format stream "~@:_Lambda-list: ~/sb-impl:print-lambda-list/" lambda-list)))

(defun describe-argument-precedence-order (argument-list stream)
  (let ((*print-circle* nil)
        (*print-level* 24)
        (*print-length* 100))
    (format stream "~@:_Argument precedence order: ~
                    ~/sb-impl:print-lambda-list/"
            argument-list)))

(defun describe-function-source (function stream)
  (declare (function function))
  (typecase function
    (generic-function
     (let ((source (sb-pcl::definition-source function)))
       (when source
         (format stream "~@:_Source file: ~A"
                 (sb-c:definition-source-location-namestring source)))))
    (compiled-function
     (binding* ((code (fun-code-header (%fun-fun function)))
                (info (sb-kernel:%code-debug-info code) :exit-if-null)
                (source (sb-c::debug-info-source info) :exit-if-null))
       (acond ((debug-source-namestring source)
               (format stream "~@:_Source file: ~A" it))
              ((%simple-fun-lexpr (%fun-fun function))
               (format stream "~@:_Source form:~@:_  ~S" it)))))
    (t
     (let ((source
             (typecase function
               (interpreted-function
                #+sb-eval (sb-eval:interpreted-function-source-location function)
                #+sb-fasteval (sb-interpreter:fun-source-location function)))))
       (when source
         (let ((namestring (sb-c:definition-source-location-namestring source)))
           (when namestring
             (format stream "~@:_Source file: ~A" namestring))))))))

(defun describe-function (name function stream)
  (let ((name (if function (%fun-name function) name)))
    (if (not (or function (and (legal-fun-name-p name) (fboundp name))))
        ;; Not defined, but possibly the type is declared, or we have
        ;; compiled calls to it.
        (when (legal-fun-name-p name)
          (multiple-value-bind (from sure) (info :function :where-from name)
            (when (or (eq :declared from) (and sure (eq :assumed from)))
              (describe-block (stream "~A names an undefined function" name)
                (format stream "~@:_~:(~A~) type: ~/sb-impl:print-type/"
                        from (global-ftype name))))))
        ;; Defined.
        (multiple-value-bind (fun what lambda-list derived-type declared-type
                              inline methods argument-precedence-order)
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
                                             (%fun-ftype function)))
                          (legal-name-p (legal-fun-name-p name))
                          (ctype (and legal-name-p
                                      (global-ftype name)))
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
                       (setf derived-type (%fun-ftype fun)))
                     (if (typep fun 'standard-generic-function)
                         (values fun
                                 "a generic function"
                                 (sb-mop:generic-function-lambda-list fun)
                                 derived-type
                                 declared-type
                                 nil
                                 (or (sb-mop:generic-function-methods fun)
                                     :none)
                                 ;; Argument precedence order
                                 ;; information is only interesting
                                 ;; for two or more required
                                 ;; parameters.
                                 (let ((order (sb-mop:generic-function-argument-precedence-order
                                               fun)))
                                   (when (>= (length order) 2)
                                     order)))
                         (values fun
                                 (if (compiled-function-p fun)
                                     "a compiled function"
                                     "an interpreted function")
                                 (%fun-lambda-list fun)
                                 derived-type
                                 declared-type
                                 (cons
                                  (info :function :inlinep name)
                                  (fun-name-inline-expansion name)))))))
          (describe-block (stream (unless function "~A names ~A:") name what)
            (describe-deprecation 'function name stream)
            (describe-lambda-list lambda-list stream)
            (when argument-precedence-order
              (describe-argument-precedence-order argument-precedence-order stream))
            (awhen (sb-c::fun-name-dx-args name)
              (let* ((keys (member-if #'symbolp it))
                     (positional (ldiff it keys)))
                (format stream "~@:_Dynamic-extent arguments:~
~@[ positional=~A~]~A~@[ keyword=~S~]"
                        positional (if (and positional keys) "," "") keys)))
            (when declared-type
              (format stream "~@:_Declared type: ~
                              ~/sb-impl:print-type-specifier/"
                      declared-type))
            (when (and derived-type
                       (not (equal declared-type derived-type)))
              (format stream "~@:_Derived type: ~
                              ~/sb-impl:print-type-specifier/"
                      derived-type))
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
                       (sb-mop:generic-function-method-combination fun)))
              (cond ((eq :none methods)
                     (format stream "~@:_No methods."))
                    (t
                     (pprint-newline :mandatory stream)
                     (pprint-logical-block (stream nil)
                       (format stream "Methods:")
                       (dolist (method methods)
                         (pprint-indent :block 2 stream)
                         (format stream "~@:_(~A ~{~S ~}~
                                         ~/sb-impl:print-lambda-list/)"
                                 name
                                 (method-qualifiers method)
                                 (sb-pcl::unparse-specializers
                                  fun (sb-mop:method-specializers method)))
                         (pprint-indent :block 4 stream)
                         (describe-documentation method t stream nil))))))
            (describe-function-source fun stream)))))
  (unless function
    (awhen (and (legal-fun-name-p name) (compiler-macro-function name))
      (describe-block (stream "~A has a compiler-macro:" name)
        (describe-documentation it t stream)
        (describe-function-source it stream)))
    ;; It seems entirely bogus to claim that, for example (SETF CAR)
    ;; has a setf expander when what we mean is that CAR has.
    (when (and (consp name) (eq 'setf (car name)) (not (cddr name)))
      (let* ((name2 (second name))
             (expander (info :setf :expander name2)))
        (cond ((typep expander '(cons symbol))
               (describe-block (stream "~A has setf-expansion: ~S"
                                       name (car expander))
                 (describe-documentation name2 'setf stream)))
              (expander
               (when (listp expander)
                 (setq expander (cdr expander)))
               (describe-block (stream "~A has a complex setf-expansion:"
                                       name)
                 (describe-lambda-list (%fun-lambda-list expander) stream)
                 (describe-documentation name2 'setf stream t)
                 (describe-function-source expander stream))))))
    (when (symbolp name)
      (describe-function `(setf ,name) nil stream))))

(defun describe-type (name stream)
  (let* ((kind (info :type :kind name))
         (fun (and kind (info :type :expander name)))
         (fun (if (listp fun) (car fun) fun)))
    (when fun
      (describe-block (stream "~A names a ~@[primitive~* ~]type-specifier:"
                              name (eq kind :primitive))
        (describe-deprecation 'type name stream)
        (describe-documentation name 'type stream (eq t fun))
        (when (functionp fun)
          (describe-lambda-list (%fun-lambda-list fun) stream)
          (multiple-value-bind (expansion ok)
              (handler-case (typexpand-1 name)
                (error () (values nil nil)))
            (when ok
              (format stream "~@:_Expansion: ~S" expansion))))))))

(defun describe-declaration (name stream)
  (let ((kind (cond
                ((member name '(ignore ignorable
                                dynamic-extent
                                special
                                type ftype
                                optimize
                                inline notineline
                                declaration))
                 "a standard")
                ((member name '(global always-bound
                                freeze-type
                                muffle-conditions unmuffle-conditions
                                disable-package-locks enable-package-locks
                                maybe-inline
                                deprecated))
                 "an SBCL-specific")
                ((info :declaration :known name)
                 "a user-defined"))))
    (when kind
      (describe-block (stream "~A names ~A declaration." name kind)))))
