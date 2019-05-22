;;;; lots of basic macros for the target SBCL

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;;; ASSERT and CHECK-TYPE

;;; ASSERT is written this way, to call ASSERT-ERROR, because of how
;;; closures are compiled. RESTART-CASE has forms with closures that
;;; the compiler causes to be generated at the top of any function
;;; using RESTART-CASE, regardless of whether they are needed. Thus if
;;; we just wrapped a RESTART-CASE around the call to ERROR, we'd have
;;; to do a significant amount of work at runtime allocating and
;;; deallocating the closures regardless of whether they were ever
;;; needed.
;;;
;;; ASSERT-ERROR isn't defined until a later file because it uses the
;;; macro RESTART-CASE, which isn't defined until a later file.
;;;
(sb-xc:defmacro assert (test-form &optional places datum &rest arguments
                            &environment env)
  "Signals an error if the value of TEST-FORM is NIL. Returns NIL.

   Optional DATUM and ARGUMENTS can be used to change the signaled
   error condition and are interpreted as in (APPLY #'ERROR DATUM
   ARGUMENTS).

   Continuing from the signaled error using the CONTINUE restart will
   allow the user to alter the values of the SETFable locations
   specified in PLACES and then start over with TEST-FORM.

   If TEST-FORM is of the form

     (FUNCTION ARG*)

   where FUNCTION is a function (but not a special operator like
   CL:OR, CL:AND, etc.) the results of evaluating the ARGs will be
   included in the error report if the assertion fails."
  (collect ((bindings) (infos))
    (let* ((func (if (listp test-form) (car test-form)))
           (new-test
            (if (and (typep func '(and symbol (not null)))
                     (not (sb-xc:macro-function func env))
                     (not (sb-xc:special-operator-p func))
                     (proper-list-p (cdr test-form)))
                ;; TEST-FORM is a function call. We do not attempt this
                ;; if TEST-FORM is a macro invocation or special form.
                `(,func ,@(mapcar (lambda (place)
                                    (if (sb-xc:constantp place env)
                                        place
                                        (with-unique-names (temp)
                                          (bindings `(,temp ,place))
                                          (infos `(list ',place ,temp))
                                          temp)))
                                  (rest test-form)))
                ;; For all other cases, just evaluate TEST-FORM
                ;; and don't report any details if the assertion fails.
                test-form))
           (try '#:try)
           (done  '#:done))
      ;; If TEST-FORM, potentially using values from BINDINGS, does not
      ;; hold, enter a loop which reports the assertion error,
      ;; potentially changes PLACES, and retries TEST-FORM.
      `(tagbody
        ,try
          (let ,(bindings)
            (when ,new-test
              (go ,done))

            (assert-error ',test-form
                          ,@(and (or (infos) places datum
                                     arguments)
                                 `((list ,@(infos))))
                          ,@(and (or places datum
                                     arguments)
                                 `(',places))
                          ,@(and (or places datum
                                     arguments)
                                 `(,datum))
                          ,@arguments))
          ,@(mapcar (lambda (place)
                      `(setf ,place (assert-prompt ',place ,place)))
                    places)
          (go ,try)
        ,done))))

(defun assert-prompt (name value)
  (cond ((y-or-n-p "The old value of ~S is ~S.~
                    ~%Do you want to supply a new value? "
                   name value)
         (format *query-io* "~&Type a form to be evaluated:~%")
         (flet ((read-it () (eval (read *query-io*))))
           (if (symbolp name) ;help user debug lexical variables
               (progv (list name) (list value) (read-it))
               (read-it))))
        (t value)))

;;; CHECK-TYPE is written this way, to call CHECK-TYPE-ERROR, because
;;; of how closures are compiled. RESTART-CASE has forms with closures
;;; that the compiler causes to be generated at the top of any
;;; function using RESTART-CASE, regardless of whether they are
;;; needed. Because it would be nice if CHECK-TYPE were cheap to use,
;;; and some things (e.g., READ-CHAR) can't afford this excessive
;;; consing, we bend backwards a little.
;;;
;;; CHECK-TYPE-ERROR isn't defined until a later file because it uses
;;; the macro RESTART-CASE, which isn't defined until a later file.
(sb-xc:defmacro check-type (place type &optional type-string
                                &environment env)
  "Signal a restartable error of type TYPE-ERROR if the value of PLACE
is not of the specified type. If an error is signalled and the restart
is used to return, this can only return if the STORE-VALUE restart is
invoked. In that case it will store into PLACE and start over."
  ;; Detect a common user-error.
  (when (and (consp type) (eq 'quote (car type)))
    (error 'simple-reference-error
           :format-control "Quoted type specifier in ~S: ~S"
           :format-arguments (list 'check-type type)
           :references '((:ansi-cl :macro check-type))))
  ;; KLUDGE: We use a simpler form of expansion if PLACE is just a
  ;; variable to work around Python's blind spot in type derivation.
  ;; For more complex places getting the type derived should not
  ;; matter so much anyhow.
  (let ((expanded (%macroexpand place env)))
    (if (symbolp expanded)
        `(do ()
             ((typep ,place ',type))
           (setf ,place (check-type-error ',place ,place ',type
                                          ,@(and type-string
                                                 `(,type-string)))))
        (let ((value (gensym)))
          `(do ((,value ,place ,place))
               ((typep ,value ',type))
             (setf ,place
                   (check-type-error ',place ,value ',type
                                     ,@(and type-string
                                            `(,type-string)))))))))

;;;; DEFINE-SYMBOL-MACRO

(sb-xc:defmacro define-symbol-macro (name expansion)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (sb-c::%define-symbol-macro ',name ',expansion (sb-c:source-location))))

(defun sb-c::%define-symbol-macro (name expansion source-location)
  (unless (symbolp name)
    (error 'simple-type-error :datum name :expected-type 'symbol
           :format-control "Symbol macro name is not a symbol: ~S."
           :format-arguments (list name)))
  (with-single-package-locked-error
      (:symbol name "defining ~A as a symbol-macro"))
  (let ((kind (info :variable :kind name)))
    (case kind
     ((:macro :unknown)
      (when source-location
        (setf (info :source-location :symbol-macro name) source-location))
      (setf (info :variable :kind name) :macro)
      (setf (info :variable :macro-expansion name) expansion))
     (t
      (%program-error "Symbol ~S is already defined as ~A."
                      name (case kind
                             (:alien "an alien variable")
                             (:constant "a constant")
                             (:special "a special variable")
                             (:global "a global variable")
                             (t kind))))))
  name)

;;;; DEFINE-COMPILER-MACRO

(sb-xc:defmacro define-compiler-macro (name lambda-list &body body)
  "Define a compiler-macro for NAME."
  (check-designator name define-compiler-macro)
  (when (and (symbolp name) (special-operator-p name))
    (%program-error "cannot define a compiler-macro for a special operator: ~S"
                    name))
  ;; DEBUG-NAME is called primarily for its side-effect of asserting
  ;; that (COMPILER-MACRO-FUNCTION x) is not a legal function name.
  (let ((def (make-macro-lambda (sb-c::debug-name 'compiler-macro name)
                                lambda-list body 'define-compiler-macro name
                                :accessor 'sb-c::compiler-macro-args)))
    `(progn
          (eval-when (:compile-toplevel)
           (sb-c::%compiler-defmacro :compiler-macro-function ',name))
          (eval-when (:compile-toplevel :load-toplevel :execute)
           (sb-c::%define-compiler-macro ',name ,def)))))

(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  (defun sb-c::%define-compiler-macro (name definition)
    (sb-c::warn-if-compiler-macro-dependency-problem name)
    ;; FIXME: warn about incompatible lambda list with
    ;; respect to parent function?
    (setf (sb-xc:compiler-macro-function name) definition)
    name))

;;;; CASE, TYPECASE, and friends

;;; Make this a full warning during SBCL build.
(define-condition duplicate-case-key-warning (#-sb-xc-host style-warning #+sb-xc-host warning)
  ((key :initarg :key
        :reader case-warning-key)
   (case-kind :initarg :case-kind
              :reader case-warning-case-kind)
   (occurrences :initarg :occurrences
                :type list
                :reader duplicate-case-key-warning-occurrences))
  (:report
    (lambda (condition stream)
      (format stream
        "Duplicate key ~S in ~S form, ~
         occurring in~{~#[~; and~]~{ the ~:R clause:~%~<  ~S~:>~}~^,~}."
        (case-warning-key condition)
        (case-warning-case-kind condition)
        (duplicate-case-key-warning-occurrences condition)))))

;;; CASE-BODY returns code for all the standard "case" macros. NAME is
;;; the macro name, and KEYFORM is the thing to case on. MULTI-P
;;; indicates whether a branch may fire off a list of keys; otherwise,
;;; a key that is a list is interpreted in some way as a single key.
;;; When MULTI-P, TEST is applied to the value of KEYFORM and each key
;;; for a given branch; otherwise, TEST is applied to the value of
;;; KEYFORM and the entire first element, instead of each part, of the
;;; case branch. When ERRORP, no OTHERWISE-CLAUSEs are recognized,
;;; and an ERROR form is generated where control falls off the end
;;; of the ordinary clauses. When PROCEEDP, it is an error to
;;; omit ERRORP, and the ERROR form generated is executed within a
;;; RESTART-CASE allowing KEYFORM to be set and retested.
(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
(defun case-body (name keyform cases multi-p test errorp proceedp needcasesp)
  (unless (or cases (not needcasesp))
    (warn "no clauses in ~S" name))
  (let ((keyform-value (gensym))
        (clauses ())
        (keys ())
        (keys-seen (make-hash-table :test #'eql)))
    (do* ((cases cases (cdr cases))
          (case (car cases) (car cases))
          (case-position 1 (1+ case-position)))
         ((null cases) nil)
      (flet ((check-clause (case-keys)
               (loop for k in case-keys
                  for existing = (gethash k keys-seen)
                  do (when existing
                       (warn 'duplicate-case-key-warning
                             :key k
                             :case-kind name
                             :occurrences `(,existing (,case-position (,case))))))
               (let ((record (list case-position (list case))))
                 (dolist (k case-keys)
                   (setf (gethash k keys-seen) record)))))
        (unless (list-of-length-at-least-p case 1)
          (with-current-source-form (cases)
            (error "~S -- bad clause in ~S" case name)))
        (with-current-source-form (case)
          (destructuring-bind (keyoid &rest forms) case
            (cond (;; an OTHERWISE-CLAUSE
                   ;;
                   ;; By the way... The old code here tried gave
                   ;; STYLE-WARNINGs for normal-clauses which looked as
                   ;; though they might've been intended to be
                   ;; otherwise-clauses. As Tony Martinez reported on
                   ;; sbcl-devel 2004-11-09 there are sometimes good
                   ;; reasons to write clauses like that; and as I noticed
                   ;; when trying to understand the old code so I could
                   ;; understand his patch, trying to guess which clauses
                   ;; don't have good reasons is fundamentally kind of a
                   ;; mess. SBCL does issue style warnings rather
                   ;; enthusiastically, and I have often justified that by
                   ;; arguing that we're doing that to detect issues which
                   ;; are tedious for programmers to detect for by
                   ;; proofreading (like small typoes in long symbol
                   ;; names, or duplicate function definitions in large
                   ;; files). This doesn't seem to be an issue like that,
                   ;; and I can't think of a comparably good justification
                   ;; for giving STYLE-WARNINGs for legal code here, so
                   ;; now we just hope the programmer knows what he's
                   ;; doing. -- WHN 2004-11-20
                   (and (not errorp) ; possible only in CASE or TYPECASE,
                                        ; not in [EC]CASE or [EC]TYPECASE
                        (memq keyoid '(t otherwise))
                        (null (cdr cases)))
                   (push `(t nil ,@forms) clauses))
                  ((and multi-p (listp keyoid))
                   (setf keys (nconc (reverse keyoid) keys))
                   (check-clause keyoid)
                   (push `((or ,@(mapcar (lambda (key)
                                           `(,test ,keyform-value ',key))
                                         keyoid))
                           nil
                           ,@forms)
                         clauses))
                  (t
                   (when (and (eq name 'case)
                              (cdr cases)
                              (memq keyoid '(t otherwise)))
                     (error 'simple-reference-error
                            :format-control
                            "~@<~IBad ~S clause:~:@_  ~S~:@_~S allowed as the key ~
                           designator only in the final otherwise-clause, not in a ~
                           normal-clause. Use (~S) instead, or move the clause to the ~
                           correct position.~:@>"
                            :format-arguments (list 'case case keyoid keyoid)
                            :references `((:ansi-cl :macro case))))
                   (push keyoid keys)
                   (check-clause (list keyoid))
                   (push `((,test ,keyform-value ',keyoid)
                           nil
                           ,@forms)
                         clauses)))))))
    (setq keys
          (nreverse (mapcon (lambda (tail)
                              (unless (member (car tail) (cdr tail))
                                (list (car tail))))
                            keys)))
    (case-body-aux name keyform keyform-value clauses keys errorp proceedp
                   `(,(if multi-p 'member 'or) ,@keys))))

;;; CASE-BODY-AUX provides the expansion once CASE-BODY has groveled
;;; all the cases. Note: it is not necessary that the resulting code
;;; signal case-failure conditions, but that's what KMP's prototype
;;; code did. We call CASE-BODY-ERROR, because of how closures are
;;; compiled. RESTART-CASE has forms with closures that the compiler
;;; causes to be generated at the top of any function using the case
;;; macros, regardless of whether they are needed.
;;;
;;; The CASE-BODY-ERROR function is defined later, when the
;;; RESTART-CASE macro has been defined.
(defun case-body-aux (name keyform keyform-value clauses keys
                      errorp proceedp expected-type)
  (if proceedp
      (let ((block (gensym))
            (again (gensym)))
        `(let ((,keyform-value ,keyform))
           (block ,block
             (tagbody
                ,again
                (return-from
                 ,block
                  (cond ,@(nreverse clauses)
                        (t
                         (setf ,keyform-value
                               (setf ,keyform
                                     (case-body-error
                                      ',name ',keyform ,keyform-value
                                      ',expected-type ',keys)))
                         (go ,again))))))))
      `(let ((,keyform-value ,keyform))
         (declare (ignorable ,keyform-value)) ; e.g. (CASE KEY (T))
         (cond
           ,@(nreverse clauses)
           ,@(when errorp
               `((t (,(ecase name
                        (etypecase 'etypecase-failure)
                        (ecase 'ecase-failure))
                     ,keyform-value ',keys))))))))
) ; EVAL-WHEN

(sb-xc:defmacro case (keyform &body cases)
  "CASE Keyform {({(Key*) | Key} Form*)}*
  Evaluates the Forms in the first clause with a Key EQL to the value of
  Keyform. If a singleton key is T then the clause is a default clause."
  (case-body 'case keyform cases t 'eql nil nil nil))

(sb-xc:defmacro ccase (keyform &body cases)
  "CCASE Keyform {({(Key*) | Key} Form*)}*
  Evaluates the Forms in the first clause with a Key EQL to the value of
  Keyform. If none of the keys matches then a correctable error is
  signalled."
  (case-body 'ccase keyform cases t 'eql t t t))

(sb-xc:defmacro ecase (keyform &body cases)
  "ECASE Keyform {({(Key*) | Key} Form*)}*
  Evaluates the Forms in the first clause with a Key EQL to the value of
  Keyform. If none of the keys matches then an error is signalled."
  (case-body 'ecase keyform cases t 'eql t nil t))

(sb-xc:defmacro typecase (keyform &body cases)
  "TYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
  is true."
  (case-body 'typecase keyform cases nil 'typep nil nil nil))

(sb-xc:defmacro ctypecase (keyform &body cases)
  "CTYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
  is true. If no form is satisfied then a correctable error is signalled."
  (case-body 'ctypecase keyform cases nil 'typep t t t))

(sb-xc:defmacro etypecase (keyform &body cases)
  "ETYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
  is true. If no form is satisfied then an error is signalled."
  (case-body 'etypecase keyform cases nil 'typep t nil t))

;;; Compile a version of BODY for all TYPES, and dispatch to the
;;; correct one based on the value of VAR. This was originally used
;;; only for strings, hence the name. Renaming it to something more
;;; generic might not be a bad idea.
(sb-xc:defmacro string-dispatch ((&rest types) var &body body)
  (let ((fun (sb-xc:gensym "STRING-DISPATCH-FUN")))
    `(flet ((,fun (,var)
              ,@body))
       (declare (inline ,fun))
       (etypecase ,var
         ,@(loop for type in types
                 ;; TRULY-THE allows transforms to take advantage of the type
                 ;; information without need for constraint propagation.
                 collect `(,type (,fun (truly-the ,type ,var))))))))

;;;; WITH-FOO i/o-related macros

(sb-xc:defmacro with-open-stream ((var stream) &body body)
  (multiple-value-bind (forms decls) (parse-body body nil)
    `(let ((,var ,stream))
       ,@decls
       (unwind-protect
            (progn ,@forms)
         (close ,var)))))

(sb-xc:defmacro with-open-file ((stream filespec &rest options)
                                &body body)
  (multiple-value-bind (forms decls) (parse-body body nil)
    (let ((abortp (gensym)))
      `(let ((,stream (open ,filespec ,@options))
             (,abortp t))
         ,@decls
         (unwind-protect
              (multiple-value-prog1
                  (progn ,@forms)
                (setq ,abortp nil))
           (when ,stream
             (close ,stream :abort ,abortp)))))))

(sb-xc:defmacro with-input-from-string ((var string &key index start end)
                                            &body forms-decls)
  (multiple-value-bind (forms decls) (parse-body forms-decls nil)
    `(let ((,var
            ;; Should (WITH-INPUT-FROM-STRING (stream str :start nil :end 5))
            ;; pass the explicit NIL, and thus get an error? It's logical
            ;; because an explicit NIL does not mean "default" in any other
            ;; string operation. So why does it here?
            ,(if (null end)
                 `(make-string-input-stream ,string ,@(if start (list start)))
                 `(make-string-input-stream ,string ,(or start 0) ,end))))
         ,@decls
         (multiple-value-prog1
             (unwind-protect
                  (progn ,@forms)
               (close ,var))
           ,@(when index
               `((setf ,index (string-input-stream-current ,var))))))))

(sb-xc:defmacro with-output-to-string
    ((var &optional string &key (element-type ''character))
     &body forms-decls)
  (multiple-value-bind (forms decls) (parse-body forms-decls nil)
    (if string
        (let ((element-type-var (gensym)))
          `(let ((,var (make-fill-pointer-output-stream ,string))
                 ;; ELEMENT-TYPE isn't currently used for anything
                 ;; (see FILL-POINTER-OUTPUT-STREAM FIXME in stream.lisp),
                 ;; but it still has to be evaluated for side-effects.
                 (,element-type-var ,element-type))
             (declare (ignore ,element-type-var))
             ,@decls
             (unwind-protect
                  (progn ,@forms)
               (close ,var))))
        `(let ((,var (make-string-output-stream
                      ;; CHARACTER is the default element-type of
                      ;; string-ouput-stream, save a few bytes when passing it
                      ,@(and (not (equal element-type ''character))
                             `(:element-type ,element-type)))))
           ,@decls
           (unwind-protect
                (progn ,@forms)
             (close ,var))
           (get-output-stream-string ,var)))))

;;;; miscellaneous macros

(sb-xc:defmacro nth-value (n form &environment env)
  "Evaluate FORM and return the Nth value (zero based)
 without consing a temporary list of values."
  ;; FIXME: The above is true, if slightly misleading.  The
  ;; MULTIPLE-VALUE-BIND idiom [ as opposed to MULTIPLE-VALUE-CALL
  ;; (LAMBDA (&REST VALUES) (NTH N VALUES)) ] does indeed not cons at
  ;; runtime.  However, for large N (say N = 200), COMPILE on such a
  ;; form will take longer than can be described as adequate, as the
  ;; optional dispatch mechanism for the M-V-B gets increasingly
  ;; hairy.
  (let ((val (and (sb-xc:constantp n env) (constant-form-value n env))))
    (if (and (integerp val) (<= 0 val (or #+(or x86-64 arm64 riscv) ;; better DEFAULT-UNKNOWN-VALUES
                                          1000
                                          10))) ; Arbitrary limit.
        (let ((dummy-list (make-gensym-list val))
              (keeper (sb-xc:gensym "KEEPER")))
          `(multiple-value-bind (,@dummy-list ,keeper) ,form
             (declare (ignore ,@dummy-list))
             ,keeper))
      ;; &MORE conversion handily deals with non-constant N,
      ;; avoiding the unstylish practice of inserting FORM into the
      ;; expansion more than once to pick off a few small values.
      ;; This is not as good as above, because it uses TAIL-CALL-VARIABLE.
        `(multiple-value-call
             (lambda (n &rest list) (nth (truly-the index n) list))
           (the index ,n) ,form))))

(sb-xc:defmacro declaim (&rest specs)
  "DECLAIM Declaration*
  Do a declaration or declarations for the global environment."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(mapcar (lambda (spec)
                 `(sb-c::%proclaim ',spec (sb-c:source-location)))
               specs)))

;; Avoid unknown return values in emitted code for PRINT-UNREADABLE-OBJECT
(sb-xc:proclaim '(ftype (sfunction (t t t &optional t) null)
                        %print-unreadable-object))
(sb-xc:defmacro print-unreadable-object ((object stream &key type identity)
                                             &body body)
  "Output OBJECT to STREAM with \"#<\" prefix, \">\" suffix, optionally
  with object-type prefix and object-identity suffix, and executing the
  code in BODY to provide possible further output."
  ;; Note: possibly out-of-order keyword argument evaluation.
  ;; But almost always the :TYPE and :IDENTITY are each literal T or NIL,
  ;; and so the LOGIOR expression reduces to a fixed value from 0 to 3.
  (let ((call `(%print-unreadable-object
                ,object ,stream (logior (if ,type 1 0) (if ,identity 2 0)))))
    (if body
        (let ((fun (make-symbol "THUNK")))
          `(dx-flet ((,fun () ,@body)) (,@call #',fun)))
        call)))

(sb-xc:defmacro ignore-errors (&rest forms)
  "Execute FORMS handling ERROR conditions, returning the result of the last
  form, or (VALUES NIL the-ERROR-that-was-caught) if an ERROR was handled."
  `(handler-case (progn ,@forms)
     (error (condition) (values nil condition))))

;; A macroexpander helper. Not sure where else to put this.
(defun funarg-bind/call-forms (funarg arg-forms)
  (if (typep funarg
             '(or (cons (eql function) (cons (satisfies legal-fun-name-p) null))
                  (cons (eql quote) (cons symbol null))
                  (cons (eql lambda))))
      (values nil `(funcall ,funarg . ,arg-forms))
    (let ((fn-sym (sb-xc:gensym))) ; for ONCE-ONLY-ish purposes
      (values `((,fn-sym (%coerce-callable-to-fun ,funarg)))
              `(sb-c::%funcall ,fn-sym . ,arg-forms)))))

;;; Ordinarily during self-build, nothing would need this macro except the
;;; calls in src/code/list, and src/code/seq.
;;; However, if cons profiling is enbled, then all calls to COPY-LIST
;;; transform into the macro, so it must be available early.
(sb-xc:defmacro copy-list-macro (input &key check-proper-list)
  ;; Unless CHECK-PROPER-LIST is true, the list is copied correctly
  ;; even if the list is not terminated by NIL. The new list is built
  ;; by CDR'ing SPLICE which is always at the tail of the new list.
  (with-unique-names (orig copy splice cell)
    ;; source transform gave input the ONCE-ONLY treatment already.
    `(when ,input
       (let ((,copy (list (car ,input))))
         (do ((,orig (cdr ,input) (cdr ,orig))
              (,splice ,copy
                       (let ((,cell (list (car ,orig))))
                         (rplacd (truly-the cons ,splice) ,cell)
                         ,cell)))
           (,@(if check-proper-list
                  `((endp ,orig))
                  `((atom ,orig)
                    ;; always store the CDR even if NIL. A blind write
                    ;; is cheaper than a branch around a write.
                    (rplacd (truly-the cons ,splice) ,orig)))
            ,copy))))))
