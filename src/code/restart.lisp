;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;; A comment formerly here about "for DX allocation" was incredibly misleading.
;;; We (apparently) never want DX allocation of restarts, except in
;;; INITIAL-THREAD-FUNCTION-TRAMPOLINE where we do, but only "accidentally".
;;; The accident is that if there is exactly ONE escape point, or there is more
;;; than one and they all return the same fixed number of values,
;;; then the generated code is right. If they don't, there's a problem.

;;; So, following this DEFSTRUCT was a NOTINLINE proclamation, which we need,
;;; because now that the compiler is smart enough to inline any structure
;;; constructor anywhere, it can and does. I'm puzzled why, because I thought
;;; the way this worked was that a local INLINE declaration would always
;;; "succeed" in finding an inline expansion, but I didn't think that the
;;; compiler would actually choose of its own free will to do local inlining,
;;; even though the standard says that's what it *can* do unless told not to.

;;; Then, if it does inline MAKE-RESTART, a problem occurs in RESTART-BIND
;;; with a TRULY-DYNAMIC-EXTENT declaration, because restarts go on the stack
;;; that can't. But I think that itself is a bug. They *should* go on the stack,
;;; because the chain of cons cells pointing to them is on the stack.
;;; The restarts are otherwise inaccessible. I'm pretty sure this
;;; is actually a compiler bug.

;;; So in a nutshell, there are 2 issues:
;;; (1) DX seems legitimate, but there may be a compiler bug.
;;; (2) The logic about when to assume that a structure constructor
;;;     can automatically be inlined is tricker than its author understands.

(declaim (notinline make-restart))

;;;; This defstruct should appear before any use of WITH-CONDITION-RESTARTS
;;;; so that the slot accessors are transformed.

(defstruct (restart (:constructor make-restart
                        ;; Having TEST-FUNCTION at the end allows
                        ;; to not replicate its default value in RESTART-BIND.
                        (name function
                         &optional report-function
                                   interactive-function
                                   test-function))
                    (:copier nil) (:predicate nil))
  (name (missing-arg) :type symbol :read-only t)
  (function (missing-arg) :type function :read-only t)
  (report-function nil :type (or null function) :read-only t)
  (interactive-function nil :type (or null function) :read-only t)
  (test-function (lambda (cond) (declare (ignore cond)) t) :type function :read-only t)
  ;; the list of conditions which are currently associated to the
  ;; restart. maintained by WITH-CONDITION-RESTARTS in a neither
  ;; thread- nor interrupt-safe way. This should not be a problem
  ;; however, since safe uses of restarts have to assume dynamic
  ;; extent.
  (associated-conditions '() :type list))

#!-sb-fluid (declaim (freeze-type restart))
