;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

(!begin-collecting-cold-init-forms)

(!define-type-class named :enumerable nil :might-contain-other-types nil)

(macrolet ((frob (type global-sym)
            `(progn
               #+sb-xc-host
               (progn (defvar ,global-sym
                        (mark-ctype-interned (make-named-type :name ',type)))
                      ;; Make it known as a constant in the cross-compiler.
                      (setf (info :variable :kind ',global-sym) :constant))
               (!cold-init-forms
                #+sb-xc (sb!c::%defconstant ',global-sym ,global-sym
                                            (sb!c::source-location))
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

;;; a vector that maps type codes to layouts, used for quickly finding
;;; the layouts of built-in classes
(defglobal **built-in-class-codes** #()) ; initialized in cold load
(declaim (type simple-vector **built-in-class-codes**))

(!defun-from-collected-cold-init-forms !primordial-type-cold-init)
