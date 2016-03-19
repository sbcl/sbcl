;;;; various user-level definitions which need to be done particularly
;;;; early

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;; Helper for making the DX closure allocation in macros expanding
;;; to CALL-WITH-FOO less ugly.
(defmacro dx-flet (functions &body forms)
  `(flet ,functions
     (declare (truly-dynamic-extent ,@(mapcar (lambda (func) `#',(car func))
                                              functions)))
     ,@forms))

;;; Another similar one.
(defmacro dx-let (bindings &body forms)
  `(let ,bindings
     (declare (truly-dynamic-extent
               ,@(mapcar (lambda (bind) (if (listp bind) (car bind) bind))
                         bindings)))
     ,@forms))

;; Define "exchanged subtract" So that DECF on a symbol requires no LET binding:
;;  (DECF I (EXPR)) -> (SETQ I (XSUBTRACT (EXPR) I))
;; which meets the CLHS 5.1.3 requirement to eval (EXPR) prior to reading
;; the old value of I. Formerly in 'setf' but too late to avoid full calls.
(declaim (inline xsubtract))
(defun xsubtract (a b) (- b a))

;;;; GENSYM tricks

;;; Automate an idiom often found in macros:
;;;   (LET ((FOO (GENSYM "FOO"))
;;;         (MAX-INDEX (GENSYM "MAX-INDEX-")))
;;;     ...)
;;;
;;; "Good notation eliminates thought." -- Eric Siggia
;;;
;;; Incidentally, this is essentially the same operator which
;;; _On Lisp_ calls WITH-GENSYMS.
(defmacro with-unique-names (symbols &body body)
  (declare (notinline every)) ; because we can't inline ALPHA-CHAR-P
  `(let ,(mapcar (lambda (symbol)
                   (let* ((symbol-name (symbol-name symbol))
                          (stem (if (every #'alpha-char-p symbol-name)
                                    symbol-name
                                    (concatenate 'string symbol-name "-"))))
                     `(,symbol (sb!xc:gensym ,stem))))
                 symbols)
     ,@body))

;;; Return a list of N gensyms. (This is a common suboperation in
;;; macros and other code-manipulating code.)
(defun make-gensym-list (n &optional name)
  (let ((arg (if name (string name) "G")))
    (loop repeat n collect (sb!xc:gensym arg))))

;;;; miscellany

;;; Lots of code wants to get to the KEYWORD package or the
;;; COMMON-LISP package without a lot of fuss, so we cache them in
;;; variables on the host, or use L-T-V forms on the target.
(macrolet ((def-it (sym expr)
             #+sb-xc-host
             `(progn (declaim (type package ,sym))
                     (defvar ,sym ,expr))
             #-sb-xc-host
             ;; We don't need to declaim the type. FIND-PACKAGE
             ;; returns a package, and L-T-V propagates types.
             ;; It's ugly how it achieves that, but it's a separate concern.
             `(define-symbol-macro ,sym (load-time-value ,expr t))))
  (def-it *cl-package* (find-package "COMMON-LISP"))
  (def-it *keyword-package* (find-package "KEYWORD")))

(declaim (inline singleton-p))
(defun singleton-p (list)
  (and (listp list) (null (rest list)) list))

;;; Concatenate together the names of some strings and symbols,
;;; producing a symbol in the current package.
(defun symbolicate (&rest things)
  (declare (dynamic-extent things))
  (values
     (intern
      (if (singleton-p things)
          (string (first things))
          (let* ((length (reduce #'+ things
                                 :key (lambda (x) (length (string x)))))
                 (name (make-array length :element-type 'character))
                 (index 0))
            (dolist (thing things name)
              (let ((x (string thing)))
                (replace name x :start1 index)
                (incf index (length x)))))))))

(defun gensymify (x)
  (if (symbolp x)
      (sb!xc:gensym (symbol-name x))
      (sb!xc:gensym)))

;;; like SYMBOLICATE, but producing keywords
(defun keywordicate (&rest things)
  (let ((*package* *keyword-package*))
    (apply #'symbolicate things)))

;;; Access *PACKAGE* in a way which lets us recover when someone has
;;; done something silly like (SETF *PACKAGE* :CL-USER) in unsafe code.
;;; (Such an assignment is undefined behavior, so it's sort of reasonable for
;;; it to cause the system to go totally insane afterwards, but it's a
;;; fairly easy mistake to make, so let's try to recover gracefully instead.)
(defun sane-package ()
  ;; Perhaps it's possible for *PACKAGE* to be set to a non-package in some
  ;; host Lisp, but in SBCL it isn't, and the PACKAGEP test below would be
  ;; elided unless forced to be NOTINLINE.
  (declare (notinline packagep))
  (let* ((maybe-package *package*)
         (packagep (packagep maybe-package)))
    ;; And if we don't also always check for deleted packages - as was true
    ;; when the "#+sb-xc-host" reader condition was absent - then half of the
    ;; COND becomes unreachable, making this function merely return *PACKAGE*
    ;; in the cross-compiler, producing a code deletion note.
    (cond ((and packagep
                ;; For good measure, we also catch the problem of
                ;; *PACKAGE* being bound to a deleted package.
                ;; Technically, this is not undefined behavior in itself,
                ;; but it will immediately lead to undefined to behavior,
                ;; since almost any operation on a deleted package is
                ;; undefined.
                (package-%name maybe-package))
           maybe-package)
          (t
           ;; We're in the undefined behavior zone. First, munge the
           ;; system back into a defined state.
           (let ((really-package
                  (load-time-value (find-package :cl-user) t)))
             (setf *package* really-package)
             ;; Then complain.
             (error 'simple-type-error
                    :datum maybe-package
                    :expected-type '(and package (satisfies package-name))
                    :format-control
                    "~@<~S can't be a ~A: ~2I~_It has been reset to ~S.~:>"
                    :format-arguments (list '*package*
                                            (if packagep
                                                "deleted package"
                                                (type-of maybe-package))
                                            really-package)))))))

;;; Access *DEFAULT-PATHNAME-DEFAULTS*, issuing a warning if its value
;;; is silly. (Unlike the vaguely-analogous SANE-PACKAGE, we don't
;;; actually need to reset the variable when it's silly, since even
;;; crazy values of *DEFAULT-PATHNAME-DEFAULTS* don't leave the system
;;; in a state where it's hard to recover interactively.)
(defun sane-default-pathname-defaults ()
  (let* ((dfd *default-pathname-defaults*)
         (dfd-dir (pathname-directory dfd)))
    ;; It's generally not good to use a relative pathname for
    ;; *DEFAULT-PATHNAME-DEFAULTS*, since relative pathnames
    ;; are defined by merging into a default pathname (which is,
    ;; by default, *DEFAULT-PATHNAME-DEFAULTS*).
    (when (and (consp dfd-dir)
               (eql (first dfd-dir) :relative))
      (warn
       "~@<~S is a relative pathname. (But we'll try using it anyway.)~@:>"
       '*default-pathname-defaults*))
    dfd))

;;; Give names to elements of a numeric sequence.
(defmacro defenum ((&key (start 0) (step 1))
                   &rest identifiers)
  (declare (type integer start step))
  (let ((value (- start step)))
    `(progn
       ,@(mapcar (lambda (id)
                   (incf value step)
                   (when id
                     (multiple-value-bind (sym docstring)
                         (if (consp id)
                             (values (car id) (cdr id))
                             (values id nil))
                       `(def!constant ,sym ,value ,@docstring))))
                 identifiers))))

;;; a helper function for various macros which expect clauses of a
;;; given length, etc.
;;;
;;; Return true if X is a proper list whose length is between MIN and
;;; MAX (inclusive).
(defun proper-list-of-length-p (x min &optional (max min))
  ;; FIXME: This implementation will hang on circular list
  ;; structure. Since this is an error-checking utility, i.e. its
  ;; job is to deal with screwed-up input, it'd be good style to fix
  ;; it so that it can deal with circular list structure.
  (cond ((minusp max) nil)
        ((null x) (zerop min))
        ((consp x)
         (and (plusp max)
              (proper-list-of-length-p (cdr x)
                                       (if (plusp (1- min))
                                           (1- min)
                                           0)
                                       (1- max))))
        (t nil)))

(defun proper-list-p (x)
  (unless (consp x)
    (return-from proper-list-p (null x)))
  (let ((rabbit (cdr x))
        (turtle x))
    (flet ((pop-rabbit ()
             (when (eql rabbit turtle) ; circular
               (return-from proper-list-p nil))
             (when (atom rabbit)
               (return-from proper-list-p (null rabbit)))
             (pop rabbit)))
      (loop (pop-rabbit)
            (pop-rabbit)
            (pop turtle)))))

;;; Helpers for defining error-signalling NOP's for "not supported
;;; here" operations.
(defmacro define-unsupported-fun (name &optional
                                  (doc "Unsupported on this platform.")
                                  (control
                                   "~S is unsupported on this platform ~
                                    (OS, CPU, whatever)."
                                   controlp)
                                  arguments)
  (declare (ignorable doc))
  `(defun ,name (&rest args)
     #!+sb-doc
     ,doc
     (declare (ignore args))
     (error 'unsupported-operator
            :format-control ,control
            :format-arguments (if ,controlp ',arguments (list ',name)))))

;;; Anaphoric macros
(defmacro awhen (test &body body)
  `(let ((it ,test))
     (when it ,@body)))

(defmacro acond (&rest clauses)
  (if (null clauses)
      `()
      (destructuring-bind ((test &body body) &rest rest) clauses
        (let ((it (copy-symbol 'it)))
          `(let ((,it ,test))
             (if ,it
                 ;; Just like COND - no body means return the tested value.
                 ,(if body
                      `(let ((it ,it)) (declare (ignorable it)) ,@body)
                      it)
                 (acond ,@rest)))))))

;; This is not an 'extension', but is needed super early, so ....
(defmacro sb!xc:defconstant (name value &optional (doc nil docp))
  #!+sb-doc
  "Define a global constant, saying that the value is constant and may be
  compiled into code. If the variable already has a value, and this is not
  EQL to the new value, the code is not portable (undefined behavior). The
  third argument is an optional documentation string for the variable."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (sb!c::%defconstant ',name ,value (sb!c:source-location)
                         ,@(and docp `(',doc)))))
