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
;;; to treat SB-XC as synonymous with COMMON-LISP otherwise.
#+sb-xc-host
(defun uncross (form)
  (labels ((rcr (form) ; recursive part
             (cond ((symbolp form)
                    ;; If SYMBOL's logical home package is CL: (meaning that its physical
                    ;; home package is XC-STRICT-CL or SB-XC or CL, or depending on
                    ;; the host's design, some other package exposed via CL:),
                    ;; then return the symbol as found via XC-STRICT-CL.
                    ;; This ensures that symbols that are used for their identity and
                    ;; not function compare as EQ after uncrossing, which they would not
                    ;; if for example, we altered (EQ (TYPE-OF x) 'SHORT-FLOAT)
                    ;; to compare against CL:SHORT-FLOAT.
                    (if (eq (sb-xc:symbol-package form) *cl-package*)
                        (find-symbol (symbol-name form) #.(find-package "XC-STRICT-CL"))
                        form))
                   ((consp form)
                    (recons form (rcr (car form)) (rcr (cdr form))))
                   (t form))))
    (rcr form)))
