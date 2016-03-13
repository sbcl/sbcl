;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;;; the DEF!CONSTANT macro

;;; FIXME: This code was created by cut-and-paste from the
;;; corresponding code for DEF!TYPE. DEF!CONSTANT and DEF!TYPE
;;; are currently very parallel, and if we ever manage to
;;; rationalize the use of UNCROSS in the cross-compiler, they should
;;; become completely parallel, at which time they should be merged to
;;; eliminate the duplicate code.

;;; *sigh* -- Even the comments are cut'n'pasted :-/ If I were more
;;; confident in my understanding, I might try to do drastic surgery,
;;; but my head is currently spinning (host? target? both?) so I'll go
;;; for the minimal changeset... -- CSR, 2002-05-11
(defmacro def!constant (&whole whole name value &optional doc)
  (declare (ignore value doc #-sb-xc-host name))
  `(progn
     #-sb-xc-host
     (defconstant ,@(cdr whole))
     #+sb-xc-host
     ,(unless (eql (find-symbol (symbol-name name) :cl) name)
        `(defconstant ,@(cdr whole)))
     #+sb-xc-host
     ,(let ((form `(sb!xc:defconstant ,@(cdr whole))))
        (if (boundp '*delayed-def!constants*)
            `(push ',form *delayed-def!constants*)
            form))))

;;; machinery to implement DEF!CONSTANT delays
#+sb-xc-host
(progn
  (/show "binding *DELAYED-DEF!CONSTANTS*")
  (defvar *delayed-def!constants* nil)
  (/show "done binding *DELAYED-DEF!CONSTANTS*")
  (defun force-delayed-def!constants ()
    (if (boundp '*delayed-def!constants*)
        (progn
          (mapc #'eval *delayed-def!constants*)
          (makunbound '*delayed-def!constants*))
        ;; This condition is probably harmless if it comes up when
        ;; interactively experimenting with the system by loading a
        ;; source file into it more than once. But it's worth warning
        ;; about it because it definitely shouldn't come up in an
        ;; ordinary build process.
        (warn "*DELAYED-DEF!CONSTANTS* is already unbound."))))

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
(let () ; ensure non-toplevelness
  ;; :compile-toplevel for #+sb-xc-host is (mostly) irrelevant,
  ;; since the fasl file will be loaded.
  ;; the #-sb-xc-host code is different though.
  (#+sb-xc-host defmacro
   #-sb-xc-host sb!xc:defmacro
    defconstant-eqx (symbol expr eqx &optional doc)
      `(def!constant ,symbol
         (%defconstant-eqx-value ',symbol ,expr ,eqx)
         ,@(when doc (list doc)))))

;; We want DEFCONSTANT-EQX to work in cold-load so that non-EQL-comparable
;; constants (like BYTE specifiers) can be accessed immediately in cold-init.
;; There are two issues: (1) we can't have expressions like (BYTE s p)
;; reach the fopcompiler, because it would emit a fop-funcall. That would
;; entail conversion of arguments from target to host integers, then
;; eval'ing and pushing a target object. It's easier to fold BYTE now and
;; have it dumped in the usual way. SB!XC:CONSTANTP recognizes that BYTE
;; can be folded; and (2) we must avoid %DEFCONSTANT-EQX-VALUE.
#+sb-xc
(eval-when (:compile-toplevel) ; SB!XC:DEFMACRO took care of load-time
  (sb!xc:defmacro defconstant-eqx (symbol expr eqx &optional doc)
    (declare (ignore eqx))
    `(sb!c::%defconstant ',symbol
                         ,(if (sb!xc:constantp expr)
                              (list 'quote (constant-form-value expr))
                              expr)
                         (sb!c:source-location)
                         ,@(when doc (list doc)))))
