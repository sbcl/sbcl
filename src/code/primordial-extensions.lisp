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

(in-package "SB-IMPL")

;;; Helper for making the DX closure allocation in macros expanding
;;; to CALL-WITH-FOO less ugly.
(defmacro dx-flet (functions &body forms)
  `(flet ,functions
     (declare (dynamic-extent ,@(mapcar (lambda (func) `#',(car func)) functions)))
     ,@forms))

;;; Another similar one.
(defmacro dx-let (bindings &body forms)
  `(let ,bindings
     (declare (dynamic-extent
               ,@(mapcar (lambda (bind) (if (listp bind) (car bind) bind))
                         bindings)))
     ,@forms))

;;; like Scheme's named LET
(defmacro named-let (name binds &body body)
  (dolist (x binds)
    (unless (proper-list-of-length-p x 2)
      (error "malformed NAMED-LET variable spec: ~S" x)))
  `(labels ((,name ,(mapcar #'first binds) ,@body))
     (,name ,@(mapcar #'second binds))))

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
  `(let ,(mapcar (lambda (symbol)
                   (let* ((symbol-name (symbol-name symbol))
                          (stem (if (find #\- symbol-name)
                                    (string (gensymify* symbol-name "-"))
                                    symbol-name)))
                     `(,symbol (gensym ,stem))))
                 symbols)
     ,@body))

;;; Return a list of N gensyms. (This is a common suboperation in
;;; macros and other code-manipulating code.)
(defun make-gensym-list (n &optional name)
  (let ((arg (if name (string name) "G")))
    (loop repeat n collect (gensym arg))))

;;;; miscellany

;;; Lots of code wants to get to the KEYWORD package or the
;;; COMMON-LISP package without going through FIND-PACKAGE, so we refer to them
;;; as constants which is ever so slightly more efficient than a defglobal.
;;; DEFINE-SYMBOL-MACRO should be ok in any host lisp. We used to distrust it,
;;; but it is specified by CLHS, as are package constants, so there should
;;; be no need to fear this idiom.
(macrolet ((def-it (sym name) `(define-symbol-macro ,sym ,(find-package name))))
  ;; *CL-PACKAGE* is always COMMON-LISP, not XC-STRICT-CL on the host, because the latter
  ;; is just a means to avoid inheriting symbols that are not supposed to be in the CL:
  ;; package but might be due to non-ansi-compliance of the host.
  (def-it *cl-package* "COMMON-LISP")
  (def-it *keyword-package* "KEYWORD"))

(declaim (inline singleton-p))
(defun singleton-p (list)
  (and (listp list) (null (rest list)) list))

(defun gensymify (x)
  (if (symbolp x)
      (gensym (symbol-name x))
      (gensym)))

(labels ((symbol-concat (package ignore-lock &rest things)
           (dx-let ((strings (make-array (length things)))
                    (length 0)
                    (only-base-chars t))
             ;; This loop is nearly like DO-REST-ARG
             ;; but it works on the host too.
             (loop for index from 0 below (length things)
                   do (let* ((thing (nth index things))
                             (s (if (integerp thing)
                                    (write-to-string thing :base 10 :radix nil :pretty nil)
                                    (string thing)))
                             (l (length s)))
                        (setf (svref strings index) s)
                        (incf length l)
                        #+sb-unicode
                        (when (and (typep s '(array character (*)))
                                   ;; BASE-CHAR-p isn't a standard predicate.
                                   ;; and host ignores ELT-TYPE anyway.
                                   #-sb-xc-host (notevery #'base-char-p s))
                          (setq only-base-chars nil))))
             ;; We copy the string when interning, so DX is ok.
             (dx-let ((name (make-array (if package length 0)
                                        :element-type 'character))
                      (elt-type (if only-base-chars 'base-char 'character))
                      (start 0))
               (unless package
                 ;; MAKE-SYMBOL doesn't copy NAME (unless non-simple).
                 (setq name (make-array length :element-type elt-type)))
               (dotimes (index (length things)
                               (if package
                                   (values (%intern name length package elt-type
                                                    ignore-lock))
                                   (make-symbol name)))
                 (let ((s (svref strings index)))
                   (replace name s :start1 start)
                   (incf start (length s)))))))
         #+sb-xc-host (%intern (name length package elt-type dummy)
                        (declare (ignore length elt-type dummy))
                        ;; Copy, in case the host respects the DX declaration,
                        ;; but does not copy, which makes our assumption wrong.
                        (intern (copy-seq name) package)))
  ;; Concatenate together the names of some strings and symbols,
  ;; producing a symbol in the current package.
  (defun symbolicate (&rest things)
    (apply #'symbol-concat (sane-package) nil things))
  ;; "bang" means intern even if the specified package is locked.
  ;; Obviously it takes a package, so it doesn't need PACKAGE- in its name.
  ;; The main use is to create interned temp vars. It really seems like there
  ;; ought to be a single package into which all such vars go,
  ;; avoiding any interning in locked packages.
  (defun symbolicate! (package &rest things)
    (apply #'symbol-concat (find-package package) t things))
  ;; SYMBOLICATE in given package respecting package-lock.
  (defun package-symbolicate (package &rest things)
    (apply #'symbol-concat (find-package package) nil things))
  ;; like SYMBOLICATE, but producing keywords
  (defun keywordicate (&rest things)
    (apply #'symbol-concat *keyword-package* nil things))
  ;; like the above, but producing an uninterned symbol.
  ;; [we already have GENSYMIFY, and naming this GENSYMICATE for
  ;; consistency with the above would not be particularly enlightening
  ;; as to how it isn't just GENSYMIFY]
  (defun gensymify* (&rest things)
    (apply #'symbol-concat nil nil things)))

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
           (let ((really-package #.(find-package :cl-user)))
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
                       `(defconstant ,sym ,value ,@docstring))))
                 identifiers))))

;;; a helper function for various macros which expect clauses of a
;;; given length, etc.
;;;
;;; Return true if X is a proper list whose length is between MIN and
;;; MAX (inclusive).
;;; Running time is bounded by MAX for circular inputs.
(defun proper-list-of-length-p (x min &optional (max min))
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

;;; Brent's cycle detection algorithm for sequences of iterated
;;; function values (the rabbit). It's faster than Floyd's. See
;;; PROPER-LIST-P for a simple example.
;;;
;;; TURTLE is lexically bound to INITIAL-VALUE, which should be one
;;; step behind the rabbit. BODY computes the next value in the
;;; sequence. Assuming the next value is in the variable RABBIT, BODY
;;; can detect a circularity with (EQ TURTLE RABBIT). If no
;;; circularity is detected, then (and only then) BODY must call
;;; (UPDATE-TURTLE RABBIT).
;;;
;;; Positive N-LAG-BITS values make the turtle slower to start moving,
;;; which slightly reduces the overhead in the non-circular case but
;;; catches circularities a bit later.
(defmacro with-turtle (((turtle &optional initial-value) &key (n-lag-bits 0))
                       &body body)
  ;; Instead of the usual implementation with two variables (one to
  ;; count the steps the turtle rested, another for the current power
  ;; of two limit), we use a single STEP variable and test it for
  ;; being a power of 2, which tightens the code and lessens register
  ;; pressure.
  ;;
  ;; This should be WITH-UNIQUE-NAMES, but GENSYM is not yet defined
  ;; in the cross-compiler.
  (let ((step (make-symbol "STEP")))
    (flet ((update-turtle-body ()
             `(progn
                ;; Is STEP + 1 a power of 2?
                ;;
                ;; Because the standard algorithm (when N-LAG-BITS is
                ;; 0) finds the STEP with the smallest
                ;; POWER-OF-TWO-CEILING that's greater than the
                ;; position of both the cycle's start and its length,
                ;; we run out of memory before STEP ceases to be a
                ;; WORD provided that the values in the sequence do
                ;; exist at the same time in the image (i.e. not
                ;; lazily generated).
                (when (zerop (let ((step ,step))
                               (logand step (locally #-sb-xc-host
                                              (declare (optimize (safety 0)))
                                                     (setq ,step (1+ step))))))
                  (setq ,turtle rabbit)))))
      `(let ((,turtle ,initial-value)
             (,step ,(ash 1 n-lag-bits)))
         #-sb-xc-host (declare (type sb-vm:word ,step))
         (macrolet ((update-turtle (rabbit)
                      ;; Avoid nested backquotes ...
                      (subst rabbit 'rabbit ',(update-turtle-body)))
                    (reset-turtle ()
                      '(setq ,turtle ,initial-value
                        ,step ,(ash 1 n-lag-bits))))
           ,@body)))))

(defun proper-list-p (x)
  (unless (consp x)
    (return-from proper-list-p (null x)))
  (let ((rabbit (cdr x)))
    (with-turtle ((turtle x) :n-lag-bits 2)
      (loop
        ;; Check for circularity. Use EQ because TURTLE is a CONS.
        (when (eq rabbit turtle)
          (return nil))
        (when (atom rabbit)
          (return (null rabbit)))
        ;; RABBIT is a CONS here.
        (update-turtle rabbit)
        (pop rabbit)))))

(declaim (inline ensure-list))
(defun ensure-list (thing)
  (if (listp thing) thing (list thing)))

(defun recons (old-cons car cdr)
  "If CAR is eq to the car of OLD-CONS and CDR is eq to the CDR, return
  OLD-CONS, otherwise return (cons CAR CDR)."
  (if (and (eq car (car old-cons)) (eq cdr (cdr old-cons)))
      old-cons
      (cons car cdr)))

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

(defvar *!removable-symbols* nil)

(defun %defconstant-eqx-value (symbol expr eqx)
  (declare (type function eqx))
  (if (boundp symbol)
      (let ((oldval (symbol-value symbol)))
        ;; %DEFCONSTANT will give a choice of how to proceeed on error.
        (if (funcall eqx oldval expr) oldval expr))
      expr))

;;; generalization of DEFCONSTANT to values which are the same not
;;; under EQL but under e.g. EQUAL or EQUALP
;;;
;;; DEFCONSTANT-EQX is to be used instead of DEFCONSTANT for values
;;; which are appropriately compared using the function given by the
;;; EQX argument instead of EQL.
;;;
#+sb-xc-host
(defmacro defconstant-eqx (symbol expr eqx &optional doc)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ;; We use DEFVAR rather than DEFCONSTANT as a host effect in
     ;; order to avoid differences in host decisions about inlining
     ;; the value of the constant, with knock-on effects on EQLity
     ;; of references to internal parts of the constant.
     (defvar ,symbol (%defconstant-eqx-value ',symbol ,expr ,eqx)
       ,@(when doc (list doc)))))

#-sb-xc-host
(defmacro defconstant-eqx (symbol expr eqx &optional doc)
  `(defconstant ,symbol (%defconstant-eqx-value ',symbol ,expr ,eqx)
     ,@(when doc (list doc))))

;;; utility for coalescing list substructure in quoted constants, for
;;; ease of guarding against differences in host compilers with
;;; respect to coalescing structure in (host) fasl files, which might
;;; then be used in compiling the target.
(defun hash-cons (list)
  (declare (type list list)
           #-sb-xc-host
           (notinline gethash3 %puthash))
  (let ((table (make-hash-table :test 'equal)))
    (labels ((hc (thing)
               (cond
                 ((atom thing) thing)
                 ((gethash thing table))
                 (t (setf (gethash thing table)
                          (cons (hc (car thing)) (hc (cdr thing))))))))
      (hc list))))

;;; These are shorthand.
;;; If in some policy we decide to open-code MEMBER, there should be
;;; no discernable difference between MEMQ and (MEMBER ... :test 'EQ).
(declaim (inline memq assq))
(defun memq (item list)
  "Return tail of LIST beginning with first element EQ to ITEM."
  (member item list :test #'eq))

(defun assq (item alist)
  "Return the first pair of alist where item is EQ to the key of pair."
  (assoc item alist :test #'eq))
