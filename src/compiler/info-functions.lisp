;;;; miscellaneous functions which use INFO
;;;;
;;;; (In CMU CL, these were in globaldb.lisp. They've been moved here
;;;; because references to INFO can't be compiled correctly until
;;;; globaldb initialization is complete, and the SBCL technique for
;;;; initializing the global database in the cross-compiler isn't
;;;; completed until load time.)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;;; internal utilities defined in terms of INFO

;;; Check that NAME is a valid function name, returning the name if
;;; OK, and signalling an error if not. In addition to checking for
;;; basic well-formedness, we also check that symbol names are not NIL
;;; or the name of a special form.
(defun check-fun-name (name)
  (typecase name
    (list
     (unless (legal-fun-name-p name)
       (compiler-error "illegal function name: ~S" name)))
    (symbol
     (when (eq (info :function :kind name) :special-form)
       (compiler-error "Special form is an illegal function name: ~S" name)))
    (t
     (compiler-error "illegal function name: ~S" name)))
  (values))

;;; Record a new function definition, and check its legality.
(defun proclaim-as-fun-name (name)

  ;; legal name?
  (check-fun-name name)

  ;; KLUDGE: This can happen when eg. compiling a NAMED-LAMBDA, and isn't
  ;; guarded against elsewhere -- so we want to assert package locks here. The
  ;; reason we do it only when stomping on existing stuff is because we want
  ;; to keep
  ;;   (WITHOUT-PACKAGE-LOCKS (DEFUN LOCKED:FOO ...))
  ;; viable, which requires no compile-time violations in the harmless cases.
  (with-single-package-locked-error ()
    (flet ((assert-it ()
             (assert-symbol-home-package-unlocked name "proclaiming ~S as a function")))

      (let ((kind (info :function :kind name)))
        ;; scrubbing old data I: possible collision with a macro
        (when (and (fboundp name) (eq :macro kind))
          (assert-it)
          (compiler-style-warn "~S was previously defined as a macro." name)
          (setf (info :function :where-from name) :assumed)
          (clear-info :function :macro-function name))

        (unless (eq :function kind)
          (assert-it)
          (setf (info :function :kind name) :function)))))

  ;; scrubbing old data II: dangling forward references
  ;;
  ;; (This could happen if someone executes PROCLAIM FTYPE at
  ;; macroexpansion time, which is bad style, or at compile time, e.g.
  ;; in EVAL-WHEN (:COMPILE) inside something like DEFSTRUCT, in which
  ;; case it's reasonable style. Either way, NAME is no longer a free
  ;; function.)
  (when (boundp '*free-funs*)       ; when compiling
    (remhash name *free-funs*))

  ;; It's uncommon for users to define both a SETF macro and function for
  ;; the same thing, so usually we warn about it, but offer a somewhat arbitrary
  ;; escape to avoid a warning - if the function gets an ftype proclaimed
  ;; before DEFSETF (or -EXPANDER) is used, we don't complain about the macro.
  ;; SBCL itself provides most SETFable things both ways even when CLHS does not
  ;; say specifically that #'(SETF x) exists.
  ;; But when building SBCL the order of function/macro definition is not
  ;; directly controlled because most SETF functions are defined en masse by
  ;; 'setf-funs' with no regard for what exists, and we also have to respect
  ;; all other constraints on build order to make bootstrap work.
  ;; So, ... just don't warn when running the cross-compiler, which means
  ;; don't compile this line in when building the cross-compiler.
  #-sb-xc-host(note-if-setf-fun-and-macro name)

  (values))

;;; This is called to do something about SETF functions that overlap
;;; with SETF macros. Perhaps we should interact with the user to see
;;; whether the macro should be blown away, but for now just give a
;;; warning. Due to the weak semantics of the (SETF FUNCTION) name, we
;;; can't assume that they aren't just naming a function (SETF FOO)
;;; for the heck of it. NAME is already known to be well-formed.
(defun note-if-setf-fun-and-macro (name)
  (when (and (consp name)
             (eq (car name) 'setf))
    (let ((stem (second name)))
      (when (or (info :setf :inverse stem) (info :setf :expander stem))
        (compiler-style-warn
         "defining function ~S when ~S already has a SETF macro"
         name stem))))
  (values))

;;; Make NAME no longer be a function name: clear everything back to
;;; the default.
(defun undefine-fun-name (name)
  (when name
    (macrolet ((frob (&rest types)
                 `(clear-info-values
                   name ',(mapcar (lambda (x)
                                    (type-info-number
                                     (type-info-or-lose :function x)))
                                  types))))
      ;; Note that this does not clear the :DEFINITION.
      ;; That's correct, because if we lose the association between a
      ;; symbol and its #<fdefn> object, it could lead to creation of
      ;; a non-unique #<fdefn> for a name.
      (frob :info
            :type
            :where-from
            :inlinep
            :kind
            :macro-function
            :inline-expansion-designator
            :source-transform
            :structure-accessor
            :assumed-type)))
  (values))

;;; part of what happens with DEFUN, also with some PCL stuff: Make
;;; NAME known to be a function definition.
(defun become-defined-fun-name (name)
  (proclaim-as-fun-name name)
  (when (eq (info :function :where-from name) :assumed)
    (setf (info :function :where-from name) :defined)
    (if (info :function :assumed-type name)
        (clear-info :function :assumed-type name))))

;;; Decode any raw (INFO :FUNCTION :INLINE-EXPANSION-DESIGNATOR FUN-NAME)
;;; value into a lambda expression, or return NIL if there is none.
(declaim (ftype (function ((or symbol cons)) list) fun-name-inline-expansion))
(defun fun-name-inline-expansion (fun-name)
  (let ((info (info :function :inline-expansion-designator fun-name)))
    (if (functionp info)
        (funcall info)
        info)))

;;;; ANSI Common Lisp functions which are defined in terms of the info
;;;; database

(defun sb!xc:macro-function (symbol &optional env)
  #!+sb-doc
  "If SYMBOL names a macro in ENV, returns the expansion function,
else returns NIL. If ENV is unspecified or NIL, use the global environment
only."
  (declare (symbol symbol))
  (let* ((fenv (when env (lexenv-funs env)))
         (local-def (cdr (assoc symbol fenv))))
    (if local-def
        (if (and (consp local-def) (eq (car local-def) 'macro))
            (cdr local-def)
            nil)
        (values (info :function :macro-function symbol)))))

(defun (setf sb!xc:macro-function) (function symbol &optional environment)
  (declare (symbol symbol) (type function function))
  (when environment
    ;; Note: Technically there could be an ENV optional argument to SETF
    ;; MACRO-FUNCTION, but since ANSI says that the consequences of
    ;; supplying a non-nil one are undefined, we don't allow it.
    ;; (Thus our implementation of this unspecified behavior is to
    ;; complain. SInce the behavior is unspecified, this is conforming.:-)
    (error "Non-NIL environment argument in SETF of MACRO-FUNCTION ~S: ~S"
           symbol environment))
  (when (eq (info :function :kind symbol) :special-form)
    (error "~S names a special form." symbol))
  (with-single-package-locked-error (:symbol symbol "setting the macro-function of ~S")
    (setf (info :function :kind symbol) :macro)
    (setf (info :function :macro-function symbol) function)
    ;; This is a nice thing to have in the target SBCL, but in the
    ;; cross-compilation host it's not nice to mess with
    ;; (SYMBOL-FUNCTION FOO) where FOO might be a symbol in the
    ;; cross-compilation host's COMMON-LISP package.
    #-sb-xc-host
    (setf (symbol-function symbol)
          (lambda (&rest args)
            (declare (ignore args))
            ;; (ANSI specification of FUNCALL says that this should be
            ;; an error of type UNDEFINED-FUNCTION, not just SIMPLE-ERROR.)
            (error 'undefined-function :name symbol))))
  function)

(defun fun-locally-defined-p (name env)
  (and env
       (let ((fun (cdr (assoc name (lexenv-funs env) :test #'equal))))
         (and fun (not (global-var-p fun))))))

(defun sb!xc:compiler-macro-function (name &optional env)
  #!+sb-doc
  "If NAME names a compiler-macro in ENV, return the expansion function, else
return NIL. Can be set with SETF when ENV is NIL."
  (legal-fun-name-or-type-error name)
  ;; CLHS 3.2.2.1: Creating a lexical binding for the function name
  ;; not only creates a new local function or macro definition, but
  ;; also shadows[2] the compiler macro.
  (unless (fun-locally-defined-p name env)
    ;; Note: CMU CL used to return NIL here when a NOTINLINE
    ;; declaration was in force. That's fairly logical, given the
    ;; specified effect of NOTINLINE declarations on compiler-macro
    ;; expansion. However, (1) it doesn't seem to be consistent with
    ;; the ANSI spec for COMPILER-MACRO-FUNCTION, and (2) it would
    ;; give surprising behavior for (SETF (COMPILER-MACRO-FUNCTION
    ;; FOO) ...) in the presence of a (PROCLAIM '(NOTINLINE FOO)). So
    ;; we don't do it.
    (values (info :function :compiler-macro-function name))))

(defun (setf sb!xc:compiler-macro-function) (function name &optional env)
  (declare (type (or symbol list) name)
           (type (or function null) function))
  (when env
    ;; ANSI says this operation is undefined.
    (error "can't SETF COMPILER-MACRO-FUNCTION when ENV is non-NIL"))
  (when (eq (info :function :kind name) :special-form)
    (error "~S names a special form." name))
  (with-single-package-locked-error
      (:symbol name "setting the compiler-macro-function of ~A")
    (setf (info :function :compiler-macro-function name) function)
    function))

;;;; a subset of DOCUMENTATION functionality for bootstrapping

;;; FDOCUMENTATION is like DOCUMENTATION, but with less functionality,
;;; and implemented with DEFUN instead of DEFGENERIC so that it can
;;; run before CLOS is set up. Supported DOC-TYPE values are
;;;   FUNCTION
;;;   SETF
;;;   STRUCTURE
;;;   T
;;;   TYPE
;;;   VARIABLE
;;; FIXME: Other types end up in INFO :RANDOM-DOCUMENTATION :STUFF. I
;;; should add some code to monitor this and make sure that nothing is
;;; unintentionally being sent to never never land this way.
;;; FIXME: Rename FDOCUMENTATION to BDOCUMENTATION, by analogy with
;;; DEF!STRUCT and DEF!MACRO and so forth. And consider simply saving
;;; all the BDOCUMENTATION entries in a *BDOCUMENTATION* hash table
;;; and slamming them into PCL once PCL gets going.
(defun fdocumentation (x doc-type)
  (case doc-type
    (variable
     (typecase x
       (symbol (values (info :variable :documentation x)))))
    ;; FUNCTION is not used at the momemnt, just here for symmetry.
    (function
     (cond ((functionp x)
            (%fun-doc x))
           ((and (legal-fun-name-p x) (fboundp x))
            (%fun-doc (or (and (symbolp x) (macro-function x))
                          (fdefinition x))))))
    (structure
     (typecase x
       (symbol (cond
                 ((eq (info :type :kind x) :instance)
                  (values (info :type :documentation x)))
                 ((info :typed-structure :info x)
                  (values (info :typed-structure :documentation x)))))))
    (type
     (typecase x
       (structure-class (values (info :type :documentation (class-name x))))
       (t (and (typep x 'symbol) (values (info :type :documentation x))))))
    (setf (values (info :setf :documentation x)))
    ((t)
     (typecase x
       (function (%fun-doc x))
       (package (package-doc-string x))
       (structure-class (values (info :type :documentation (class-name x))))
       ((or symbol cons)
        (random-documentation x doc-type))))
    (t
     (when (typep x '(or symbol cons))
       (random-documentation x doc-type)))))

(defun (setf fdocumentation) (string name doc-type)
  (declare (type (or null string) string))
  (let ((info-number
         (macrolet ((info-number (class type)
                      (type-info-number (type-info-or-lose class type))))
           (case doc-type
             (variable (info-number :variable :documentation))
             (structure
              (cond ((eq (info :type :kind name) :instance)
                     (info-number :type :documentation))
                    ((info :typed-structure :info name)
                     (info-number :typed-structure :documentation))))
             (type (info-number :type :documentation))
             (setf (info-number :setf :documentation))))))
    (cond (info-number
           (if string
               (set-info-value name info-number string)
               (clear-info-values name (list info-number))))
          ((eq doc-type 'function)
           ;; FIXME: this silently loses
           ;; * (setf (documentation '(a bad name) 'function) "x") => "x"
           ;; * (documentation '(a bad name) 'function) => NIL
           ;; which is fine because as noted in pcl/documentation.lsp
           ;;   even for supported doc types an implementation is permitted
           ;;   to discard docs at any time
           ;; but should a warning be issued just as for an unknown DOC-TYPE?
           ;;
           ;; And there's additional weirdness if you do, in this order -
           ;;  * (setf (documentation 'foo 'function) "hi")
           ;;  * (defun foo () "hey" 1)
           ;;  * (documentation 'foo 'function) => "hi" ; should be "hey"
           ;; CLHS says regarding DEFUN:
           ;; " Documentation is attached as a documentation string to
           ;;   /name/ (as kind function) and to the /function object/."
           (when (legal-fun-name-p name)
             (setf (%fun-doc (fdefinition name)) string)))
          ((typep name '(or symbol cons))
           (setf (random-documentation name doc-type) string))))
  string)

(defun random-documentation (name type)
  (cdr (assoc type (info :random-documentation :stuff name))))

(defun (setf random-documentation) (new-value name type)
  (let ((pair (assoc type (info :random-documentation :stuff name))))
    (if pair
        (setf (cdr pair) new-value)
        (push (cons type new-value)
              (info :random-documentation :stuff name))))
  new-value)
