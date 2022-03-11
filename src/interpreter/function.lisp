;;;; A partially-compiling interpreted EVAL

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-INTERPRETER")

(defstruct (interpreted-fun-prototype
             (:predicate proto-fn-p)
             (:conc-name proto-fn-)
             (:constructor %make-proto-fn
                           (name lambda-list decls forms docstring
                                 &optional (pretty-arglist lambda-list)))
             (:copier nil))
  (source-location
   (let ((s (sb-c::make-definition-source-location)))
     (if (sb-c::definition-source-location-namestring s) s)))
  (name        0 :read-only nil)
  (lambda-list 0 :read-only nil)
  (pretty-arglist) ; same as what a generic-function terms it
  ;; BODY encompasses all three of the following slots.
  (decls       0 :read-only t)
  (forms       0 :read-only t)
  (docstring   nil)
  (type        nil) ; computed on demand
  (cookie      nil) ; nonce for memoization of macros
  (%frame      nil))

(declaim (freeze-type interpreted-fun-prototype))

;; !defstruct-with-alternate-metaclass is unslammable and the
;; RECOMPILE restart doesn't work on it.  This is the main reason why
;; this stuff is split out into its own file.  Also, it lets the
;; INTERPRETED-FUNCTION type be declared before it is used in
;; compiler/main and code/deftypes-for-target.

;; This is 8 words: header, layout, trampoline, fin-fun, info[4]
(sb-kernel:!defstruct-with-alternate-metaclass
 interpreted-function
 :slot-names (%proto-fn env frame cookie)
 :constructor %make-interpreted-function
 :superclass-name function
 :metaclass-name static-classoid
 :metaclass-constructor make-static-classoid
 :dd-type funcallable-structure)

;;; FIXME: alternate metaclass structures have dumb accessors
;;; whose transforms don't contain TRULY-THE like for regular structures
;;; because we don't have any way to declare types.
;;; (and a DEFKNOWN isn't enough)
(declaim (inline fun-proto-fn))
(defun fun-proto-fn (f)
  (truly-the interpreted-fun-prototype (interpreted-function-%proto-fn f)))

(defun fun-lambda-expression (fun)
  (let* ((proto-fn (fun-proto-fn fun))
         (name (proto-fn-name proto-fn))
         (named-p (neq name 0)))
    (values (append (if named-p (list 'named-lambda name) '(lambda))
                    (list (proto-fn-lambda-list proto-fn))
                    (proto-fn-decls proto-fn)
                    (proto-fn-forms proto-fn))
            ;; CLHS permits returning T as the safe default,
            ;; but we can return a better value. No function's env is NIL,
            ;; because it needs to capture a policy. If the only elements
            ;; present are policy, we'll say it's the null env.
            (let ((env (interpreted-function-env fun)))
              (not (and (basic-env-p env)
                        (null (env-parent env))
                        (null (env-payload env))
                        (null (env-symbols env))
                        (null (declarations (env-contour env))))))
            ;; Internally we can distinguish absence of a name
            ;; from presence of a NIL name, but not in the external API.
            (if named-p name))))

;; INTERPRETED-FUNCTION-mumble accessors are slots in the primitive object.
;; FUN-mumble accessors are indirections through the PROTO-FN.

(defun fun-forms (fun)
  (proto-fn-forms (fun-proto-fn fun)))

(defun fun-name (f)
  (proto-fn-name (fun-proto-fn f)))

(defun fun-source-location (f)
  (proto-fn-source-location (fun-proto-fn f)))
