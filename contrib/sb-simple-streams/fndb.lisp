;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: COMMON-LISP -*-

;;; This code is in the public domain.

;;; The cmucl implementation of simple-streams was done by Paul Foley,
;;; who placed the code in the public domain.  Sbcl port by Rudi
;;; Schlatte.

(in-package "COMMON-LISP")

;; .../compiler/knownfun.lisp


#||

Paul Foley (private conversation, 2003-05-17):

BTW, the RESULT-TYPE-OPEN-CLASS function in fndb.lisp is buggy.
Here's a (smarter) replacement:

;; .../compiler/knownfun.lisp
(defun result-type-open-class (call)
  (declare (type sb-c::combination call))
  (let* ((not-set '#:not-set)
         (not-constant '#:not-constant)
         (direction not-set)
         (if-exists not-set)
         (if-does-not-exist not-set)
         (class not-set))
    ;; find (the first occurence of) each interesting keyword argument
    (do ((args (cdr (combination-args call)) (cddr args)))
        ((null args))
      (macrolet ((maybe-set (var)
                   `(when (and (eq ,var not-set) (cadr args))
                      (if (constant-continuation-p (cadr args))
                        (setq ,var (continuation-value (cadr args)))
                        (setq ,var not-constant)))))
        (case (continuation-value (car args))
          (:direction (maybe-set direction))
          (:if-exists (maybe-set if-exists))
          (:if-does-not-exist (maybe-set if-does-not-exist))
          (:class (maybe-set class)))))
    ;; and set default values for any that weren't set above
    (when (eq direction not-set) (setq direction :input))
    (when (eq if-exists not-constant) (setq if-exists nil))
    (when (eq if-does-not-exist not-constant) (set if-does-not-exist nil))
    (when (or (eq class not-set) (eq class not-constant)) (setq class 'stream))
    ;; now, NIL is a possible result only in the following cases:
    ;;   direction is :probe or not-constant and :if-does-not-exist is not
    ;;     :error
    ;;   direction is :output or :io or not-constant and :if-exists is nil
    ;;   :if-does-not-exist is nil
    (if (or (and (or (eq direction :probe) (eq direction not-constant))
                 (not (eq if-does-not-exist :error)))
            (and (or (eq direction :output) (eq direction :io)
                     (eq direction not-constant))
                 (eq if-exists nil))
            (eq if-does-not-exist nil))
      (specifier-type `(or null ,class))
      (specifier-type class))))

TODO (rudi 2003-05-19): make the above work, make (defknown open) use it.

||#


;; This adds keywords for :MAPPED, :INPUT-HANDLE :OUTPUT-HANDLE.
;; But [BUG?] why is the first arg type T instead of PATHNAME-DESIGNATOR?
(sb-c:defknown open (t &rest t
                       &key (:direction (member :input :output :io :probe))
                       (:element-type sb-kernel:type-specifier)
                       (:if-exists (member :error :new-version :rename
                                                  :rename-and-delete :overwrite
                                                  :append :supersede nil))
                       (:if-does-not-exist (member :error :create nil))
                       (:external-format sb-kernel:external-format-designator)
                       (:class (or symbol class))
                       (:mapped (member t nil))
                       (:input-handle (or null fixnum stream))
                       (:output-handle (or null fixnum stream))
                       #+win32 (:overlapped t))
    (or stream null)
    ()
  ;; :derive-type #'result-type-open-class
  :overwrite-fndb-silently t)

(sb-c:defknown listen (&optional sb-kernel:stream-designator
                                 (or null (integer 1 10) (member character)))
    sb-kernel:generalized-boolean (sb-c::unsafely-flushable)
  :overwrite-fndb-silently t)

(sb-c:defknown read-sequence (sequence stream &key (:start sb-int:index)
                                       (:end sb-kernel:sequence-end)
                                       (:partial-fill boolean))
    (sb-int:index) ()
  :overwrite-fndb-silently t)

(sb-c:defknown clear-input (&optional stream boolean) null ()
  :overwrite-fndb-silently t)
