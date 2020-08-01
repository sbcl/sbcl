;;;; converting symbols from SB-XC::FOO to COMMON-LISP::FOO when
;;;; cross-compiling (so that we can maintain distinct SB-XC versions
;;;; of fundamental COMMON-LISP things like PROCLAIM and CLASS and
;;;; ARRAY-RANK-LIMIT, so that we don't trash the cross-compilation
;;;; host when defining the cross-compiler, but the distinctions go
;;;; away in the target system)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-INT")

;;; In the target system's compiler, uncrossing is just identity.
#-sb-xc-host
(progn
  (declaim (inline uncross))
  (defun uncross (x) x))
;;; In the cross-compiler, uncrossing is slightly less trivial.

;;; This condition is only a STYLE-WARNING because generally it isn't important
;;; in practice to recurse through anything except CONSes anyway.
#|
#+sb-show
(define-condition uncross-rcr-failure (style-warning)
  ((form :initarg :form :reader uncross-rcr-failure-form))
  (:report (lambda (c s)
             (format s
                     "UNCROSS couldn't recurse through ~S~%~
                      (which is OK as long as there are no SB-XC symbols ~
                      down there)"
                     (uncross-rcr-failure-form c)))))
|#

;;; When cross-compiling, EVAL-WHEN :COMPILE-TOPLEVEL code is executed
;;; in the host Common Lisp, not the target. A certain amount of
;;; dancing around is required in order for this to work more or less
;;; correctly. (Fortunately, more or less correctly is good enough --
;;; it only needs to work on the EVAL-WHEN expressions found in the
;;; SBCL sources themselves, and we can exercise self-control to keep
;;; them from including anything which too strongly resembles a
;;; language lawyer's test case.)
;;;
;;; In order to make the dancing happen, we need to make a distinction
;;; between SB-XC and COMMON-LISP when we're executing a form at
;;; compile time (i.e. within EVAL-WHEN :COMPILE-TOPLEVEL) but we need
;;; to treat SB-XC as synonymous with COMMON-LISP otherwise. This
;;; can't be done by making SB-XC a nickname of COMMON-LISP, because
;;; the reader processes things before EVAL-WHEN, so by the time
;;; EVAL-WHEN :COMPILE-TOPLEVEL saw a form, the distinction it needs
;;; would be lost. Instead, we read forms preserving this distinction
;;; (treating SB-XC as a separate package), and only when we're about
;;; to process them (for any situation other than EVAL-WHEN
;;; (:COMPILE-TOPLEVEL)) do we call UNCROSS on them to obliterate the
;;; distinction.
#+sb-xc-host
(let ((;; KLUDGE: We don't currently try to handle circular program
       ;; structure, but we do at least detect it and complain about
       ;; it..
       inside? (make-hash-table)))
  (defun uncross (form)
    (labels ((uncross-symbol (symbol)
               ;; If SYMBOL's logical home package is CL: (meaning that its physical
               ;; home package is XC-STRICT-CL or SB-XC or CL, or depending on
               ;; the host's design, some other package exposed via CL:),
               ;; then return the symbol as found via XC-STRICT-CL.
               ;; This ensures that symbols that are used for their identity and
               ;; not function compare as EQ after uncrossing, which they would not
               ;; if for example, we altered (EQ (FLONUM-FORMAT x) 'SHORT-FLOAT)
               ;; to compare against CL:SHORT-FLOAT.
               (if (eq (sb-xc:symbol-package symbol) *cl-package*)
                   (find-symbol (symbol-name symbol) "XC-STRICT-CL")
                   symbol))
             (rcr (form) ; recursive part
               (cond ((symbolp form)
                      (uncross-symbol form))
                     ((or (numberp form)
                          (characterp form)
                          (stringp form))
                      form)
                     (t
                      ;; If we reach here, FORM is something with
                      ;; internal structure which could include
                      ;; symbols in the SB-XC package.
                      (when (gethash form inside?)
                        (let ((*print-circle* t))
                          ;; This code could probably be generalized
                          ;; to work on circular structure, but it
                          ;; seems easier just to avoid putting any
                          ;; circular structure into the bootstrap
                          ;; code.
                          (error "circular structure in ~S" form)))
                      (setf (gethash form inside?) t)
                      (unwind-protect
                          (typecase form
                            (cons
                             (recons form (rcr (car form)) (rcr (cdr form))))
                            (t
                             ;; KLUDGE: There are other types
                             ;; (especially (ARRAY T) and
                             ;; STRUCTURE-OBJECT, but also HASH-TABLE
                             ;; and perhaps others) which could hold
                             ;; symbols. In principle we should handle
                             ;; those types as well. Failing that, we
                             ;; could give warnings for them. However,
                             ;; the current system works for
                             ;; bootstrapping in practice (because we
                             ;; don't use those constructs that way)
                             ;; and the warnings more annoying than
                             ;; useful, so I simply turned the
                             ;; warnings off. -- WHN 20001105
                             #+nil (warn 'uncross-rcr-failure :form form)
                             form))
                        (remhash form inside?))))))
      (clrhash inside?)
      (rcr form))))
