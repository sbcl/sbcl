;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: COMMON-LISP -*-

;;; This code is in the public domain.

;;; The cmucl implementation of simple-streams was done by Paul Foley,
;;; who placed the code in the public domain.  Sbcl port by Rudi
;;; Schlatte.

(in-package "COMMON-LISP")

;; .../compiler/knownfun.lisp

;; TODO: I suppose sbcl internals have sufficiently diverged from
;; cmucl that this does not work after my primitive translation
;; attempt.  This is used in the cmucl version to compute (via
;; :derive-type arg to defknown) the return type of open.  For the
;; time being, the new defknown form for open does not specify its
;; return type.
#+nil
(defun result-type-open-class (call)
  (declare (type sb-c::combination call))
  (do ((args (sb-c::combination-args call) (cdr args)))
      ((null args))
    (let ((leaf (sb-c::ref-leaf (sb-c::continuation-use (car args)))))
      (when (and (typep leaf 'sb-kernel:constant)
		 (eq (sb-c::constant-value leaf) :class)
		 (cdr args))
	(let ((leaf (sb-c::ref-leaf (sb-c::continuation-use (cadr args)))))
	  (return (if (typep leaf 'sb-kernel:constant)
		      (find-class (sb-c::constant-value leaf) nil)
		      nil)))))))

(handler-bind ((error #'(lambda (condition) (declare (ignore condition))
                                (continue))))
  (sb-c:defknown open (t &rest t
                         &key (:direction (member :input :output :io :probe))
                         (:element-type sb-kernel:type-specifier)
                         (:if-exists (member :error :new-version :rename
                                             :rename-and-delete :overwrite
                                             :append :supersede nil))
                         (:if-does-not-exist (member :error :create nil))
                         (:external-format (member :default))
                         (:class (or symbol class))
                         (:mapped (member t nil))
                         (:input-handle (or null fixnum stream))
                         (:output-handle (or null fixnum stream))
                         &allow-other-keys)
    (or stream null)
    ()
    ;; :derive-type #'result-type-open-class
    )

  (sb-c:defknown listen (&optional sb-kernel:streamlike
                                   (or null (integer 1 10) (member 'character)))
    boolean (sb-c::unsafely-flushable sb-c::explicit-check))

  (sb-c:defknown read-sequence (sequence stream &key (:start sb-int:index)
                                         (:end sb-kernel:sequence-end)
                                         (:partial-fill boolean))
    (sb-int:index) ())

  (sb-c:defknown clear-input (&optional stream boolean) null
                 (sb-c::explicit-check)))
