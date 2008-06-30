;;;; type-related stuff which exists only in the target SBCL runtime

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

;;; If TYPE is a type that we can do a compile-time test on, then
;;; return whether the object is of that type as the first value and
;;; second value true. Otherwise return NIL, NIL.
;;;
;;; We give up on unknown types and pick off FUNCTION- and COMPOUND-
;;; types. For STRUCTURE- types, we require that the type be defined
;;; in both the current and compiler environments, and that the
;;; INCLUDES be the same.
;;;
;;; KLUDGE: This should probably be a type method instead of a big
;;; ETYPECASE. But then the type method system should probably be CLOS
;;; too, and until that happens wedging more stuff into it might be
;;; messy. So I've left it a big ETYPECASE. -- 2001-03-16
(defun ctypep (obj type)
  (declare (type ctype type))
  (etypecase type
    ((or numeric-type
         named-type
         member-type
         array-type
         character-set-type
         built-in-classoid
         cons-type)
     (values (%typep obj type) t))
    (classoid
     (if (if (csubtypep type (specifier-type 'function))
             (funcallable-instance-p obj)
             (%instancep obj))
         (if (eq (classoid-layout type)
                 (info :type :compiler-layout (classoid-name type)))
             (values (sb!xc:typep obj type) t)
             (values nil nil))
         (values nil t)))
    (compound-type
     (funcall (etypecase type
                (intersection-type #'every/type)
                (union-type #'any/type))
              #'ctypep
              obj
              (compound-type-types type)))
    (fun-type
     (values (functionp obj) t))
    (unknown-type
     (values nil nil))
    (alien-type-type
     (values (alien-typep obj (alien-type-type-alien-type type)) t))
    (negation-type
     (multiple-value-bind (res win)
         (ctypep obj (negation-type-type type))
       (if win
           (values (not res) t)
           (values nil nil))))
    (hairy-type
     ;; Now the tricky stuff.
     (let* ((hairy-spec (hairy-type-specifier type))
            (symbol (if (consp hairy-spec) (car hairy-spec) hairy-spec)))
       (ecase symbol
         (and
          (if (atom hairy-spec)
              (values t t)
              (dolist (spec (cdr hairy-spec) (values t t))
                (multiple-value-bind (res win)
                    (ctypep obj (specifier-type spec))
                  (unless win (return (values nil nil)))
                  (unless res (return (values nil t)))))))
         (not
          (multiple-value-bind (res win)
              (ctypep obj (specifier-type (cadr hairy-spec)))
            (if win
                (values (not res) t)
                (values nil nil))))
         (satisfies
          ;; If the SATISFIES function is not foldable, we cannot answer!
          (let* ((form `(,(second hairy-spec) ',obj)))
            (multiple-value-bind (ok result)
                (sb!c::constant-function-call-p form nil nil)
              (values (not (null result)) ok)))))))))

;;; Return the layout for an object. This is the basic operation for
;;; finding out the "type" of an object, and is used for generic
;;; function dispatch. The standard doesn't seem to say as much as it
;;; should about what this returns for built-in objects. For example,
;;; it seems that we must return NULL rather than LIST when X is NIL
;;; so that GF's can specialize on NULL.
#!-sb-fluid (declaim (inline layout-of))
(defun layout-of (x)
  (declare (optimize (speed 3) (safety 0)))
  (cond ((%instancep x) (%instance-layout x))
        ((funcallable-instance-p x) (%funcallable-instance-layout x))
        ((null x)
         ;; Note: was #.((CLASS-LAYOUT (SB!XC:FIND-CLASS 'NULL))).
         ;; I (WHN 19990209) replaced this with an expression evaluated at
         ;; run time in order to make it easier to build the cross-compiler.
         ;;
         ;; KLUDGE: Since there's a DEFTRANSFORM for FIND-CLASSOID on
         ;; constant names which creates non-cold-loadable code, we
         ;; can't just use (CLASSOID-LAYOUT (FIND-CLASSOID 'NULL))
         ;; here. The original (WHN 19991004) solution was to locally
         ;; notinline FIND-CLASSOID. However, the full call to
         ;; FIND-CLASSOID caused suboptimal register allocation in PCL
         ;; dfuns. So instead we now use a special variable which is
         ;; initialized during cold init. -- JES, 2006-07-04
         *null-classoid-layout*)
        (t (svref *built-in-class-codes* (widetag-of x)))))

#!-sb-fluid (declaim (inline classoid-of))
(defun classoid-of (object)
  #!+sb-doc
  "Return the class of the supplied object, which may be any Lisp object, not
   just a CLOS STANDARD-OBJECT."
  (layout-classoid (layout-of object)))

;;;; miscellaneous interfaces

;;; Clear memoization of all type system operations that can be
;;; altered by type definition/redefinition.
;;;
;;; FIXME: This should be autogenerated.
(defun clear-type-caches ()
  (declare (special *type-system-initialized*))
  (when *type-system-initialized*
    (dolist (sym '(values-specifier-type-cache-clear
                   values-type-union-cache-clear
                   type-union2-cache-clear
                   values-subtypep-cache-clear
                   csubtypep-cache-clear
                   type-intersection2-cache-clear
                   values-type-intersection-cache-clear
                   type=-cache-clear))
      (funcall (the function (symbol-function sym)))))
  (values))

;;; This is like TYPE-OF, only we return a CTYPE structure instead of
;;; a type specifier, and we try to return the type most useful for
;;; type checking, rather than trying to come up with the one that the
;;; user might find most informative.
(declaim (ftype (function (t) ctype) ctype-of))
(defun-cached (ctype-of
               :hash-function (lambda (x) (logand (sxhash x) #x1FF))
               :hash-bits 9
               :init-wrapper !cold-init-forms)
              ((x eq))
  (typecase x
    (function
     (if (funcallable-instance-p x)
         (classoid-of x)
         (specifier-type (sb!impl::%fun-type x))))
    (symbol
     (make-member-type :members (list x)))
    (number
     (ctype-of-number x))
    (array
     (let ((etype (specifier-type (array-element-type x))))
       (make-array-type :dimensions (array-dimensions x)
                        :complexp (not (typep x 'simple-array))
                        :element-type etype
                        :specialized-element-type etype)))
    (cons
     (make-cons-type *universal-type* *universal-type*))
    (character
     (specifier-type 'character))
    (t
     (classoid-of x))))

(!defun-from-collected-cold-init-forms !target-type-cold-init)
