;;;; This file contains things for the extensions packages (SB-EXT and
;;;; also "internal extensions" SB-INT) which can't be built at
;;;; cross-compile time, and perhaps also some things which might as
;;;; well not be built at cross-compile time because they're not
;;;; needed then. Things which can't be built at cross-compile time
;;;; (e.g. because they need machinery which only exists inside SBCL's
;;;; implementation of the LISP package) do not belong in this file.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

(defun delq (item list)
  "Delete all LIST entries EQ to ITEM (destructively modifying LIST),
and return the modified LIST."
  (declare (explicit-check))
  (let ((list list))
    (do ((x list (cdr x))
         (splice '()))
        ((endp x) list)
      (cond ((eq item (car x))
             (if (null splice)
                 (setq list (cdr x))
                 (rplacd splice (cdr x))))
            (t (setq splice x)))))) ; Move splice along to include element.

(defun posq (item list)
  "Return the position of the first element EQ to ITEM."
  (declare (inline position))
  (position item list :test #'eq))

(defun interned-symbol-p (x) (and (symbolp x) (symbol-package x)))

;;;; variables initialization and shutdown sequences

;;; (Most of the save-a-core functionality is defined later, in its
;;; own file, but we'd like to have these symbols declared special and
;;; initialized ASAP.)

(declaim (type list *save-hooks* *init-hooks* *exit-hooks*))

(define-load-time-global *save-hooks* nil
  "A list of function designators which are called in an unspecified
order before creating a saved core image.

Unused by SBCL itself: reserved for user and applications.")

(define-load-time-global *init-hooks* nil
  "A list of function designators which are called in an unspecified
order when a saved core image starts up, after the system itself has
been initialized, but before non-user threads such as the finalizer
thread have been started.

Unused by SBCL itself: reserved for user and applications.")

(define-load-time-global *exit-hooks* nil
  "A list of function designators which are called in an unspecified
order when SBCL process exits.

Unused by SBCL itself: reserved for user and applications.

Using (SB-EXT:EXIT :ABORT T), or calling exit(3) directly circumvents
these hooks.")

(defun call-hooks (kind hooks &key (on-error :error))
  (dolist (hook hooks)
    (handler-case
        (funcall hook)
      (serious-condition (c)
        (if (eq :warn on-error)
            (warn "Problem running ~A hook ~S:~%  ~A" kind hook c)
            (with-simple-restart (continue "Skip this ~A hook." kind)
              (error "Problem running ~A hook ~S:~%  ~A" kind hook c)))))))

;;; Binary search for simple vectors
(defun binary-search* (value seq key)
  (declare (simple-vector seq))
  (declare (function key))
  (labels ((recurse (start end)
             (when (< start end)
               (let* ((i (+ start (truncate (- end start) 2)))
                      (elt (svref seq i))
                      (key-value (funcall key elt)))
                 (cond ((< value key-value)
                        (recurse start i))
                       ((> value key-value)
                        (recurse (1+ i) end))
                       (t
                        i))))))
    (recurse 0 (length seq))))

(defun binary-search (value seq &key (key #'identity))
  (let ((index (binary-search* value seq key)))
    (if index
        (svref seq index))))

(defun double-vector-binary-search (value vector)
  (declare (simple-vector vector)
           (optimize speed)
           (integer value))
  (labels ((recurse (start end)
             (declare (type index start end))
             (when (< start end)
               (let* ((i (+ start (truncate (- end start) 2)))
                      (elt (svref vector (truly-the index (* 2 i)))))
                 (declare (type integer elt)
                          (type index i))
                 (cond ((< value elt)
                        (recurse start i))
                       ((> value elt)
                        (recurse (1+ i) end))
                       (t
                        (svref vector (truly-the index (1+ (* 2 i))))))))))
    (recurse 0 (truncate (length vector) 2))))


;;;; helpers for C library calls

;;; Signal a SIMPLE-CONDITION/ERROR condition associated with an ANSI C
;;; errno problem, arranging for the condition's print representation
;;; to be similar to the ANSI C perror(3) style.
(defun simple-perror (prefix-string
                      &key
                      (errno (get-errno))
                      (simple-error 'simple-error)
                      other-condition-args)
  (declare (type symbol simple-error))
  (aver (subtypep simple-error 'simple-condition))
  (aver (subtypep simple-error 'error))
  (apply #'error
         simple-error
         :format-control "~@<~A: ~2I~_~A~:>"
         :format-arguments (list prefix-string (strerror errno))
         other-condition-args))

;;; Constructing shortish strings one character at a time. More efficient then
;;; a string-stream, as can directly use simple-base-strings when applicable,
;;; and if the maximum size is know doesn't need to copy the result at all --
;;; but if the result is going to be HUGE, string-streams will win.
(defmacro with-push-char ((&key (element-type 'character) (initial-size 28)) &body body)
  (with-unique-names (string size pointer)
    `(let* ((,size ,initial-size)
            (,string (make-array ,size :element-type ',element-type))
            (,pointer 0))
       (declare (type (integer 0 ,array-dimension-limit) ,size)
                (type (integer 0 ,(1- array-dimension-limit)) ,pointer)
                (type (simple-array ,element-type (*)) ,string))
       (flet ((push-char (char)
                (declare (optimize (sb-c:insert-array-bounds-checks 0)))
                (when (= ,pointer ,size)
                  (let ((old ,string))
                    (setf ,size (* 2 (+ ,size 2))
                          ,string (make-array ,size :element-type ',element-type))
                    (replace ,string old)))
                (setf (char ,string ,pointer) char)
                (incf ,pointer))
              (get-pushed-string ()
                (let ((string ,string)
                      (size ,pointer))
                  (setf ,size 0
                        ,pointer 0
                        ,string ,(coerce "" `(simple-array ,element-type (*))))
                  ;; This is really local, so we can be destructive!
                  (%shrink-vector string size)
                  string)))
         ,@body))))

(defmacro with-locked-hash-table ((hash-table) &body body)
  "Limits concurrent accesses to HASH-TABLE for the duration of BODY.
If HASH-TABLE is synchronized, BODY will execute with exclusive
ownership of the table. If HASH-TABLE is not synchronized, BODY will
execute with other WITH-LOCKED-HASH-TABLE bodies excluded -- exclusion
of hash-table accesses not surrounded by WITH-LOCKED-HASH-TABLE is
unspecified."
  ;; Needless to say, this also excludes some internal bits, but
  ;; getting there is too much detail when "unspecified" says what
  ;; is important -- unpredictable, but harmless.
  `(sb-thread:with-recursive-lock ((hash-table-lock ,hash-table))
     ,@body))

(defmacro find-package-restarts ((package-designator &optional reader)
                                 &body body)
  (let ((package `(or ,(if reader
                           '*reader-package*
                           '*package*)
                      (sane-package))))
    `(locally
       (restart-case ,@body
         (continue ()
           :report (lambda (stream)
                     (format stream "Use the current package, ~a."
                             (package-name ,package)))
           (return (values ,package
                           ,@(and reader
                                  '(:current)))))
         (retry ()
           :report "Retry finding the package.")
         (use-value (value)
           :report "Specify a different package"
           :interactive
           (lambda ()
             (read-evaluated-form-of-type 'package-designator))
           (when (packagep value)
             (return (values value ,@(and reader
                                          '(nil)))))
           (setf ,package-designator (the package-designator value)))
         ,@(and reader
             `((unintern ()
                         :report "Read the symbol as uninterned."
                         (return (values nil :uninterned)))))
         ,@(and reader
             `((symbol (value)
                       :report "Specify a symbol to return"
                       :interactive
                       (lambda ()
                         (read-evaluated-form-of-type 'symbol))
                       (values value :symbol)))))
       (go retry))))

;;;; Deprecating stuff

(defun print-deprecation-replacements (stream replacements &optional colonp atp)
  (declare (ignore colonp atp))
  ;; I don't think this is callable during cross-compilation, is it?
  ;; Anyway, the format string tokenizer can not handle APPLY on its own.
  (apply #'format stream
         (sb-format:tokens "~#[~;~
             Use ~/sb-ext:print-symbol-with-prefix/ instead.~;~
             Use ~/sb-ext:print-symbol-with-prefix/ or ~
             ~/sb-ext:print-symbol-with-prefix/ instead.~:;~
             Use~@{~#[~; or~] ~
             ~/sb-ext:print-symbol-with-prefix/~^,~} instead.~
           ~]")
         replacements))

(defun print-deprecation-message (namespace name software version
                                  &optional replacements stream)
  (format stream
           "The ~(~A~) ~/sb-ext:print-symbol-with-prefix/ has been ~
            deprecated as of ~@[~A ~]version ~A.~
            ~@[~2%~/sb-impl::print-deprecation-replacements/~]"
          namespace name software version replacements))

(defmacro define-deprecated-function (state version name replacements lambda-list
                                      &body body)
  (declare (type deprecation-state state)
           (type string version)
           (type function-name name)
           (type (or function-name list) replacements)
           (type list lambda-list))
  `(progn
     (declaim (deprecated
               ,state ("SBCL" ,version)
               (function ,name ,@(when replacements
                                   `(:replacement ,replacements)))))
     ,(ecase state
        ((:early :late)
         `(defun ,name ,lambda-list
            ,@body))
        ((:final)
         `',name))))

(defmacro define-deprecated-variable (state version name
                                      &key (value nil valuep) replacement)
  (declare (type deprecation-state state)
           (type string version)
           (type symbol name))
  `(progn
     (declaim (deprecated
               ,state ("SBCL" ,version)
               (variable ,name ,@(when replacement
                                   `(:replacement ,replacement)))))
     ,(ecase state
        ((:early :late)
         `(defvar ,name ,@(when valuep (list value))))
        ((:final)
         `',name))))

(define-load-time-global *deprecated-exports* nil)

(defun deprecate-export (package symbol state version)
  (declare (type deprecation-state state)
           (type string version)
           (type symbol symbol))
  (setf (getf (getf *deprecated-exports* package) symbol)
        (cons state version)))

(defun check-deprecated-export (package symbol)
  (let ((state (getf (getf *deprecated-exports* package) symbol)))
    (when state
      (deprecation-warn (car state) "SBCL" (cdr state) 'symbol
                        (format nil "~A:~A" (package-name package) symbol)
                        (list symbol))
      t)))
