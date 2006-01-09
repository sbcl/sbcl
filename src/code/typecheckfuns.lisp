;;;; Out-of-line structure slot accessor functions need to do type
;;;; tests. These accessor functions aren't called very often, so it's
;;;; unreasonable to implement them all as different compiled
;;;; functions, because that's too much bloat. But when they are
;;;; called, it's unreasonable to just punt to interpreted TYPEP,
;;;; because that's unreasonably slow. The system implemented here
;;;; tries to be a reasonable compromise solution to this problem.
;;;;
;;;; Structure accessor functions are still implemented as closures,
;;;; but now one of the closed-over variables is a function which does
;;;; the type test, i.e. a typecheckfun. When a type can be expanded
;;;; fully into known types at compile time, we compile a LAMBDA which
;;;; does TYPEP on it, and use that. If the function can't be expanded
;;;; at compile time, then it can't be compiled efficiently anyway, so
;;;; we just emit a note.
;;;;
;;;; As a further wrinkle on this, we reuse the typecheckfuns, so that
;;;; the dozens of slot accessors which have e.g. :TYPE SYMBOL can all
;;;; share the same typecheckfun instead of having to keep dozens of
;;;; equivalent typecheckfun copies floating around. We can also pull
;;;; a few other tricks to reduce bloat, like implementing all
;;;; typecheckfuns for structure classes as a closure over structure
;;;; LAYOUTs.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;;; setting up to precompile code for common types once and for all

;;; initialization value for *COMMON-TYPESPECS*
(eval-when (:compile-toplevel)
  (defvar *compile-time-common-typespecs*
    (let (;; When we generate collections of common specialized
          ;; array types, what should their element types be?
          (common-element-typespecs
           ;; Note: This table is pretty arbitrary, just things I use a lot
           ;; or see used a lot. If someone has ideas for better values,
           ;; lemme know. -- WHN 2001-10-15
           #(t
             character
             bit fixnum
             #!+#.(cl:if (cl:= 32 sb!vm:n-word-bits) '(and) '(or))
             (unsigned-byte 32)
             #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
             (unsigned-byte 64)
             #!+#.(cl:if (cl:= 32 sb!vm:n-word-bits) '(and) '(or))
             (signed-byte 32)
             #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
             (signed-byte 64)
             single-float double-float)))
      (coerce (remove-duplicates
               (mapcar (lambda (typespec)
                         (type-specifier (specifier-type typespec)))
                       ;; Note: This collection of input values is
                       ;; pretty arbitrary, just inspired by things I
                       ;; use a lot or see being used a lot in the
                       ;; system. If someone has ideas for better
                       ;; values, lemme know. -- WHN 2001-10-15
                       (concatenate
                        'list
                        ;; non-array types
                        '(bit
                          boolean
                          character
                          cons
                          double-float
                          fixnum
                          hash-table
                          index
                          integer
                          list
                          package
                          signed-byte
                          (signed-byte 8)
                          single-float
                          structure-object
                          symbol
                          unsigned-byte
                          (unsigned-byte 8)
                          #!+#.(cl:if (cl:= 32 sb!vm:n-word-bits) '(and) '(or))
                          (unsigned-byte 32)
                          #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
                          (unsigned-byte 64))
                        ;; systematic names for array types
                        (map 'list
                             (lambda (element-type)
                               `(simple-array ,element-type 1))
                             common-element-typespecs)
                        (map 'list
                             (lambda (element-type)
                               `(vector ,element-type))
                             common-element-typespecs)
                        ;; idiosyncratic names for array types
                        '(simple-vector
                          bit-vector simple-bit-vector
                          string simple-string)))
               :test #'equal)
              'simple-vector))))

;;; What are the common testable types? (If a slot accessor looks up
;;; one of these types, it doesn't need to supply a compiled TYPEP
;;; function to initialize the possibly-empty entry: instead it's
;;; guaranteed that the entry is there. Hopefully this will reduce
;;; compile time and object file bloat.)
(declaim (type simple-vector *common-typespecs*))
(defvar *common-typespecs*)
#-sb-xc (eval-when (:compile-toplevel :load-toplevel :execute)
          (setf *common-typespecs*
                #.*compile-time-common-typespecs*))
;; (#+SB-XC initialization is handled elsewhere, at cold init time.)

(defun ctype-is-common-typecheckfun-type-p (ctype)
  (position (type-specifier ctype) *common-typespecs*
            :test #'equal))

(defun typecheck-failure (arg typespec)
  (error 'type-error :datum arg :expected-type typespec))

;;; memoization cache for typecheckfuns: a map from fully-expanded type
;;; specifiers to functions which test the type of their argument
(defvar *typecheckfuns*
  #-sb-xc (make-hash-table :test 'equal)
  ;; (#+SB-XC initialization is handled elsewhere, at cold init time.)
  )

;;; Memoize the FORM which returns a typecheckfun for TYPESPEC.
(defmacro memoized-typecheckfun-form (form typespec)
  (with-unique-names (n-typespec)
    `(let ((,n-typespec ,typespec))
       (or (gethash ,n-typespec *typecheckfuns*)
           (setf (gethash ,n-typespec *typecheckfuns*)
                 ,form)))))

#+sb-xc
(defun !typecheckfuns-cold-init ()
  (/show0 "in typecheckfuns-cold-init")
  (setf *typecheckfuns* (make-hash-table :test 'equal))
  ;; Initialize the table of common typespecs.
  (setf *common-typespecs* #.*compile-time-common-typespecs*)
  ;; Initialize *TYPECHECKFUNS* with typecheckfuns for common typespecs.
  (/show0 "typecheckfuns-cold-init initial setfs done")
  (macrolet ((macro ()
               `(progn
                  ,@(map 'list
                         (lambda (typespec)
                           `(progn
                              (/show0 "setf")
                              (setf (gethash ',typespec *typecheckfuns*)
                                    (progn
                                      (/show0 "lambda")
                                      (lambda (arg)
                                        (unless (typep arg ',typespec)
                                          (typecheck-failure arg ',typespec))
                                        (values))))))
                         *common-typespecs*))))
    (macro))
  (values))

;;; Return a trivial best-you-can-expect-when-you-don't-predefine-the-type
;;; implementation of a function which checks the type of its argument.
(defun interpreted-typecheckfun (typespec)
  ;; Note that we don't and shouldn't memoize this, since otherwise the
  ;; user could do
  ;;   (DEFSTRUCT FOO (X NIL :TYPE XYTYPE))
  ;;   (DEFTYPE XYTYPE () (OR SYMBOL CHARACTER))
  ;;   (DEFSTRUCT BAR (Y NIL :TYPE XYTYPE))
  ;; and be unpleasantly surprised when the memoized old interpreted
  ;; type check from the FOO-X slot setter interfered with the
  ;; construction of a shiny new compiled type check for the BAR-Y
  ;; slot setter.
  (lambda (arg)
    (unless (typep arg typespec)
      (typecheck-failure arg typespec))
    (values)))

;;; Type checks for structure objects are all implemented the same
;;; way, with only the LAYOUT varying, so they're practically begging
;;; to be implemented as closures over the layout.
(defun %structure-object-typecheckfun (typespec)
  (let ((layout (compiler-layout-or-lose typespec)))
    (lambda (arg)
      (unless (typep-to-layout arg layout)
        (typecheck-failure arg typespec))
      (values))))
(defun structure-object-typecheckfun (typespec)
  (memoized-typecheckfun-form (%structure-object-typecheckfun typespec)
                              typespec))

;;; General type checks need the full compiler, not just stereotyped
;;; closures. We arrange for UNMEMOIZED-TYPECHECKFUN to be produced
;;; for us at compile time (or it can be skipped if the compiler knows
;;; that the memoization lookup can't fail).
(defun general-typecheckfun (typespec &optional unmemoized-typecheckfun)
  (or (gethash typespec *typecheckfuns*)
      (setf (gethash typespec *typecheckfuns*) unmemoized-typecheckfun)
      ;; UNMEMOIZED-TYPECHECKFUN shouldn't be NIL unless the compiler
      ;; knew that the memo would exist, so we shouldn't be here.
      (bug "no typecheckfun memo for ~S" typespec)))

(defun ctype-needs-to-be-interpreted-p (ctype)
  ;; What we should really do is factor out the code in
  ;; (DEFINE-SOURCE-TRANSFORM TYPEP ..) so that it can be shared here.
  ;; Until then this toy version should be good enough for some testing.
  (warn "FIXME: This is just a toy stub CTYPE-NEEDS-TO-BE-INTERPRETED-P.")
  (not (or (position (type-specifier ctype)
                     *common-typespecs*
                     :test #'equal)
           (member-type-p ctype)
           (numeric-type-p ctype)
           (array-type-p ctype)
           (cons-type-p ctype)
           (intersection-type-p ctype)
           (union-type-p ctype)
           (negation-type-p ctype)
           (character-set-type-p ctype))))

;;; Evaluate (at load/execute time) to a function which checks that
;;; its argument is of the specified type.
;;;
;;; The name is slightly misleading, since some cases are memoized, so
;;; we might reuse a value which was made earlier instead of creating
;;; a new one from scratch.
(declaim (ftype (sfunction (t) function) typespec-typecheckfun))
(defun typespec-typecheckfun (typespec)
  ;; a general-purpose default case, hopefully overridden by the
  ;; DEFINE-COMPILER-MACRO implementation
  (interpreted-typecheckfun typespec))

;;; If we know the value of the typespec at compile time, we might
;;; well be able to avoid interpreting it at runtime.
(define-compiler-macro typespec-typecheckfun (&whole whole typespec-form)
  (if (and (consp typespec-form)
           (eql (first typespec-form) 'quote))
      (let* ((typespec (second typespec-form))
             (ctype (specifier-type typespec)))
        (aver (= 2 (length typespec-form)))
        (cond ((structure-classoid-p ctype)
               `(structure-object-typecheckfun ,typespec-form))
              ((ctype-needs-to-be-interpreted-p ctype)
               whole) ; i.e. give up compiler macro
              (t
               `(let ((typespec ,typespec-form))
                  (general-typecheckfun
                   typespec
                   ;; Unless we know that the function is already in the
                   ;; memoization cache
                   ,@(unless (ctype-is-common-typecheckfun-type-p ctype)
                       ;; Note that we're arranging for the
                       ;; UNMEMOIZED-TYPECHECKFUN argument value to be
                       ;; constructed at compile time. This means the
                       ;; compiler does the work of compiling the function,
                       ;; and the loader does the work of loading the
                       ;; function, regardless of whether the runtime check
                       ;; for "is it in the memoization cache?" succeeds.
                       ;; (Then if the memoization check succeeds, the
                       ;; compiled and loaded function is eventually GCed.)
                       ;; The wasted motion in the case of a successful
                       ;; memoization check is unfortunate, but it avoids
                       ;; having to invoke the compiler at load time when
                       ;; memoization fails, which is probably more
                       ;; important.
                       `((lambda (arg)
                           (unless (typep arg typespec)
                             (typecheck-failure arg typespec))))))))))
      whole)) ; i.e. give up compiler macro
