;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;; (Note that when cross-compiling, SB!XC:TYPEP is interpreted as a
;;; test that the host Lisp object OBJECT translates to a target SBCL
;;; type TYPE. This behavior is needed e.g. to test for the validity
;;; of numeric subtype bounds read when cross-compiling.)
(defun typep (object type &optional environment)
  #!+sb-doc
  "Is OBJECT of type TYPE?"
  (declare (type lexenv-designator environment) (ignore environment))
  (declare (explicit-check))
  ;; Actually interpreting types at runtime is done by %TYPEP. The
  ;; cost of the extra function call here should be negligible
  ;; compared to the cost of interpreting types. (And the compiler
  ;; tries hard to optimize away the interpretation of types at
  ;; runtime, and when it succeeds, we never get here anyway.)
  (%%typep object (specifier-type type)))

;;; the actual TYPEP engine. The compiler only generates calls to this
;;; function when it can't figure out anything more intelligent to do.
(defun %typep (object specifier)
  ;; Checking CTYPE-P on the specifier, as used to be done, is not right.
  ;; If the specifier were a CTYPE we shouldn't have gotten here.
  (declare (explicit-check))
  (%%typep object (specifier-type specifier)))

(defun %%typep (object type &optional (strict t))
  (declare (type ctype type))
  (etypecase type
    (named-type
     (ecase (named-type-name type)
       ((* t) t)
       ((instance) (%instancep object))
       ((funcallable-instance) (funcallable-instance-p object))
       ((extended-sequence) (extended-sequence-p object))
       ((nil) nil)))
    (numeric-type
     (and (numberp object)
          (let (;; I think this works because of an invariant of the
                ;; two components of a COMPLEX are always coerced to
                ;; be the same, e.g. (COMPLEX 1.0 3/2) => #C(1.0 1.5).
                ;; Dunno why that holds, though -- ANSI? Python
                ;; tradition? marsh faerie spirits? -- WHN 2001-10-27
                (num (if (complexp object)
                         (realpart object)
                         object)))
            (ecase (numeric-type-class type)
              (integer (integerp num))
              (rational (rationalp num))
              (float
               (ecase (numeric-type-format type)
                 (short-float (typep num 'short-float))
                 (single-float (typep num 'single-float))
                 (double-float (typep num 'double-float))
                 (long-float (typep num 'long-float))
                 ((nil) (floatp num))))
              ((nil) t)))
          (flet ((bound-test (val)
                   (let ((low (numeric-type-low type))
                         (high (numeric-type-high type)))
                     (and (cond ((null low) t)
                                ((listp low) (> val (car low)))
                                (t (>= val low)))
                          (cond ((null high) t)
                                ((listp high) (< val (car high)))
                                (t (<= val high)))))))
            (ecase (numeric-type-complexp type)
              ((nil) t)
              (:complex
               (and (complexp object)
                    (bound-test (realpart object))
                    (bound-test (imagpart object))))
              (:real
               (and (not (complexp object))
                    (bound-test object)))))))
    (array-type
     (and (arrayp object)
          (or (eq (array-type-complexp type) :maybe)
              (eq (not (simple-array-p object))
                  (array-type-complexp type)))
          (let ((want (array-type-dimensions type)))
            (or (eq want '*)
                (if (array-header-p object)
                    (do ((rank (array-rank object))
                         (axis 0 (1+ axis))
                         (want want (cdr want)))
                        ((= axis rank) (null want))
                     (let ((dim (car want)))
                       (unless (or (eq dim '*)
                                   (eq dim (%array-dimension object axis)))
                         (return nil))))
                    (let ((dim (car want)))
                      (and (or (eq dim '*) (eq dim (length object)))
                           (not (cdr want)))))))
          ;; FIXME: treatment of compound types involving unknown types
          ;; is generally bogus throughout the system, e.g.
          ;;   (TYPEP MY-ARRAY '(ARRAY (OR BAD1 BAD2) *)) => T
          ;; because (OR BAD1 BAD2) is not represented as an UNKNOWN-TYPE,
          ;; and has specialized type '*.
          ;; One way to fix this is that every CTYPE needs a bit to indicate
          ;; whether any subpart of it is unknown, or else when parsing,
          ;; we should always return an UNKNOWN if any subpart is unknown,
          ;; or else any time we use a CTYPE, we do a deep traversal
          ;; to detect embedded UNKNOWNs (which seems bad for performance).
          (if (unknown-type-p (array-type-element-type type))
              ;; better to fail this way than to get bogosities like
              ;;   (TYPEP (MAKE-ARRAY 11) '(ARRAY SOME-UNDEFINED-TYPE)) => T
              (error "~@<unknown element type in array type: ~2I~_~S~:>"
                     (type-specifier type))
              t)
          (or (eq (array-type-specialized-element-type type) *wild-type*)
              (values (type= (array-type-specialized-element-type type)
                             ;; FIXME: not the most efficient.
                             (specifier-type (array-element-type
                                              object)))))))
    (member-type
     (when (member-type-member-p object type)
       t))
    (classoid
     #+sb-xc-host (ctypep object type)
     ;; It might be more efficient to check that OBJECT is either INSTANCEP
     ;; or FUNCALLABLE-INSTANCE-P before making this call.
     ;; But doing that would change the behavior if %%TYPEP were ever called
     ;; with a built-in classoid whose members are not instances.
     ;; e.g. (%%typep (find-fdefn 'car) (specifier-type 'fdefn))
     ;; I'm not sure if that can happen.
     #-sb-xc-host (classoid-typep (layout-of object) type object))
    (union-type
     (some (lambda (union-type-type) (%%typep object union-type-type strict))
           (union-type-types type)))
    (intersection-type
     (every (lambda (intersection-type-type)
              (%%typep object intersection-type-type strict))
            (intersection-type-types type)))
    (cons-type
     (and (consp object)
          (%%typep (car object) (cons-type-car-type type) strict)
          (%%typep (cdr object) (cons-type-cdr-type type) strict)))
    #!+sb-simd-pack
    (simd-pack-type
     (and (simd-pack-p object)
          (let* ((tag (%simd-pack-tag object))
                 (name (nth tag *simd-pack-element-types*)))
            (not (not (member name (simd-pack-type-element-type type)))))))
    (character-set-type
     (and (characterp object)
         (let ((code (char-code object))
               (pairs (character-set-type-pairs type)))
           (dolist (pair pairs nil)
             (destructuring-bind (low . high) pair
               (when (<= low code high)
                 (return t)))))))
    (unknown-type
     ;; dunno how to do this ANSIly -- WHN 19990413
     #+sb-xc-host (error "stub: %%TYPEP UNKNOWN-TYPE in xcompilation host")
     ;; Parse it again to make sure it's really undefined.
     (let ((reparse (specifier-type (unknown-type-specifier type))))
       (if (typep reparse 'unknown-type)
           (error "unknown type specifier: ~S"
                  (unknown-type-specifier reparse))
           (%%typep object reparse strict))))
    (negation-type
     (not (%%typep object (negation-type-type type) strict)))
    (hairy-type
     ;; Now the tricky stuff.
     (let* ((hairy-spec (hairy-type-specifier type))
            (symbol (car hairy-spec)))
       (ecase symbol
         (and
          (every (lambda (spec) (%%typep object (specifier-type spec) strict))
                 (rest hairy-spec)))
         ;; Note: it should be safe to skip OR here, because union
         ;; types can always be represented as UNION-TYPE in general
         ;; or other CTYPEs in special cases; we never need to use
         ;; HAIRY-TYPE for them.
         (not
          (unless (proper-list-of-length-p hairy-spec 2)
            (error "invalid type specifier: ~S" hairy-spec))
          (not (%%typep object (specifier-type (cadr hairy-spec)) strict)))
         (satisfies
          (unless (proper-list-of-length-p hairy-spec 2)
            (error "invalid type specifier: ~S" hairy-spec))
          (values (funcall (symbol-function (cadr hairy-spec)) object))))))
    (alien-type-type
     (sb!alien-internals:alien-typep object (alien-type-type-alien-type type)))
    (fun-type
     (if strict
         (error "Function types are not a legal argument to TYPEP:~%  ~S"
                (type-specifier type))
         (and (functionp object)
              (csubtypep (specifier-type (sb!impl::%fun-type object)) type))))))

;;; Do a type test from a class cell, allowing forward reference and
;;; redefinition.
(defun classoid-cell-typep (obj-layout cell object)
  (let ((classoid (classoid-cell-classoid cell)))
    (unless classoid
      (error "The class ~S has not yet been defined."
             (classoid-cell-name cell)))
    (classoid-typep obj-layout classoid object)))

;;; Test whether OBJ-LAYOUT is from an instance of CLASSOID.
(defun classoid-typep (obj-layout classoid object)
  (declare (optimize speed))
  ;; FIXME & KLUDGE: We could like to grab the *WORLD-LOCK* here (to ensure that
  ;; class graph doesn't change while we're doing the typep test), but in
  ;; pratice that causes trouble -- deadlocking against the compiler
  ;; if compiler output (or macro, or compiler-macro expansion) causes
  ;; another thread to do stuff. Not locking is a shoddy bandaid as it is remains
  ;; easy to trigger the same problem using a different code path -- but in practice
  ;; locking here makes Slime unusable with :SPAWN in post *WORLD-LOCK* world. So...
  ;; -- NS 2008-12-16
  (multiple-value-bind (obj-layout layout)
      (do ((layout (classoid-layout classoid) (classoid-layout classoid))
           (i 0 (+ i 1))
           (obj-layout obj-layout))
          ((and (not (layout-invalid obj-layout))
                (not (layout-invalid layout)))
           (values obj-layout layout))
        (aver (< i 2))
        (when (layout-invalid obj-layout)
          (setq obj-layout (update-object-layout-or-invalid object layout)))
        (%ensure-classoid-valid classoid layout "typep"))
    (let ((obj-inherits (layout-inherits obj-layout)))
      (or (eq obj-layout layout)
          (dotimes (i (length obj-inherits) nil)
            (when (eq (svref obj-inherits i) layout)
              (return t)))))))
