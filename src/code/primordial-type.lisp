;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

(!begin-collecting-cold-init-forms)

(define-type-class named :enumerable nil :might-contain-other-types nil)

(macrolet ((frob (type global-sym)
            `(progn
               #+sb-xc-host
               (progn (defvar ,global-sym
                        (!make-named-type (interned-type-hash ',type 'named
                                             ,(case type
                                                ((nil t)
                                                 `(sb-vm::saetp-index-or-lose ',type))
                                                (* 31)))
                                          ',type))
                      ;; Make it known as a constant in the cross-compiler.
                      (setf (info :variable :kind ',global-sym) :constant))
               (!cold-init-forms
                #+sb-xc (sb-c::%defconstant ',global-sym ,global-sym
                                            (sb-c::source-location))
                (setf (info :type :builtin ',type) ,global-sym
                      (info :type :kind ',type) :primitive)))))
   ;; KLUDGE: In ANSI, * isn't really the name of a type, it's just a
   ;; special symbol which can be stuck in some places where an
   ;; ordinary type can go, e.g. (ARRAY * 1) instead of (ARRAY T 1).
   ;; In SBCL it also used to denote universal VALUES type.
   (frob * *wild-type*)
   (frob nil *empty-type*)
   (frob t *universal-type*)
   ;; new in sbcl-0.9.5: these used to be CLASSOID types, but that
   ;; view of them was incompatible with requirements on the MOP
   ;; metaobject class hierarchy: the INSTANCE and
   ;; FUNCALLABLE-INSTANCE types are disjoint (instances have
   ;; instance-pointer-lowtag; funcallable-instances have
   ;; fun-pointer-lowtag), while FUNCALLABLE-STANDARD-OBJECT is
   ;; required to be a subclass of STANDARD-OBJECT.  -- CSR,
   ;; 2005-09-09
   (frob instance *instance-type*)
   (frob funcallable-instance *funcallable-instance-type*)
   ;; new in sbcl-1.0.3.3: necessary to act as a join point for the
   ;; extended sequence hierarchy.  (Might be removed later if we use
   ;; a dedicated FUNDAMENTAL-SEQUENCE class for this.)
   (frob extended-sequence *extended-sequence-type*))

(!defun-from-collected-cold-init-forms !primordial-type-cold-init)

;;; A HAIRY-TYPE represents anything too weird to be described
;;; reasonably or to be useful, such as NOT, SATISFIES, unknown types,
;;; and unreasonably complicated types involving AND. We just remember
;;; the original type spec.
;;; A possible improvement would be for HAIRY-TYPE to have a subtype
;;; named SATISFIES-TYPE for the hairy types which are specifically
;;; of the form (SATISFIES pred) so that we don't have to examine
;;; the sexpr repeatedly to decide whether it takes that form.
;;; And as a further improvement, we might want a table that maps
;;; predicates to their exactly recognized type when possible.
;;; We have such a table in fact - *BACKEND-PREDICATE-TYPES*
;;; as a starting point. But something like PLUSP isn't in there.
;;; On the other hand, either of these points may not be sources of
;;; inefficiency, and the latter if implemented might have undesirable
;;; user-visible ramifications, though it seems unlikely.
(defstruct (hairy-type (:include ctype
                        (class-info (type-class-or-lose 'hairy)))
                       (:constructor %make-hairy-type (specifier))
                       (:constructor !make-interned-hairy-type
                           (specifier &aux (hash-value (interned-type-hash))))
                       (:copier nil))
  ;; the Common Lisp type-specifier of the type we represent
  (specifier nil :type t :read-only t))
