;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

;;; (Note that when cross-compiling, SB-XC:TYPEP is interpreted as a
;;; test that the host Lisp object OBJECT translates to a target SBCL
;;; type TYPE. This behavior is needed e.g. to test for the validity
;;; of numeric subtype bounds read when cross-compiling.)
(defun typep (object type &optional environment)
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
 (named-let recurse ((object object) (type type))
  (etypecase type
    (named-type
     (ecase (named-type-name type)
       ((* t) t)
       ((instance) (%instancep object))
       ((funcallable-instance) (funcallable-instance-p object))
       ((extended-sequence) (extended-sequence-p object))
       ((nil) nil)))
    (numeric-type (number-typep object type))
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
     ;; It might be more efficient to check that OBJECT is either INSTANCEP
     ;; or FUNCALLABLE-INSTANCE-P before making this call.
     ;; But doing that would change the behavior if %%TYPEP were ever called
     ;; with a built-in classoid whose members are not instances.
     ;; e.g. (%%typep (find-fdefn 'car) (specifier-type 'fdefn))
     ;; I'm not sure if that can happen.
     (classoid-typep (layout-of object) type object))
    (union-type
     (some (lambda (union-type-type) (recurse object union-type-type))
           (union-type-types type)))
    (intersection-type
     (every (lambda (intersection-type-type) (recurse object intersection-type-type))
            (intersection-type-types type)))
    (cons-type
     (and (consp object)
          (recurse (car object) (cons-type-car-type type))
          (recurse (cdr object) (cons-type-cdr-type type))))
    #+sb-simd-pack
    (simd-pack-type
     (and (simd-pack-p object)
          (let* ((tag (%simd-pack-tag object))
                 (name (nth tag *simd-pack-element-types*)))
            (not (not (member name (simd-pack-type-element-type type)))))))
    #+sb-simd-pack-256
    (simd-pack-256-type
     (and (simd-pack-256-p object)
          (let* ((tag (%simd-pack-256-tag object))
                 (name (nth tag *simd-pack-element-types*)))
            (not (not (member name (simd-pack-256-type-element-type type)))))))
    (character-set-type
     (and (characterp object)
          (character-in-charset-p object type)))
    (unknown-type
     ;; Parse it again to make sure it's really undefined.
     (let ((reparse (specifier-type (unknown-type-specifier type))))
       (if (typep reparse 'unknown-type)
           (error "unknown type specifier: ~S"
                  (unknown-type-specifier reparse))
           (recurse object reparse))))
    (negation-type
     (not (recurse object (negation-type-type type))))
    (hairy-type
     ;; Now the tricky stuff.
     (let* ((hairy-spec (hairy-type-specifier type))
            (symbol (car hairy-spec)))
       (ecase symbol
         (and
          (every (lambda (spec) (recurse object (specifier-type spec)))
                 (rest hairy-spec)))
         ;; Note: it should be safe to skip OR here, because union
         ;; types can always be represented as UNION-TYPE in general
         ;; or other CTYPEs in special cases; we never need to use
         ;; HAIRY-TYPE for them.
         (not
          (unless (proper-list-of-length-p hairy-spec 2)
            (error "invalid type specifier: ~S" hairy-spec))
          (not (recurse object (specifier-type (cadr hairy-spec)))))
         (satisfies
          (unless (proper-list-of-length-p hairy-spec 2)
            (error "invalid type specifier: ~S" hairy-spec))
          (and (funcall (symbol-function (cadr hairy-spec)) object) t)))))
    (alien-type-type
     (sb-alien-internals:alien-typep object (alien-type-type-alien-type type)))
    (fun-type
     (case strict
      ((functionp) (functionp object)) ; least strict
      ((nil) ; medium strict
       (and (functionp object)
            (csubtypep (specifier-type (sb-impl::%fun-type object)) type)))
      (t ; strict
       (error "Function types are not a legal argument to TYPEP:~%  ~S"
              (type-specifier type))))))))

(defun cached-typep (cache object)
  (let* ((type (cdr cache))
         (ctype (if (ctype-p type)
                    type
                    (specifier-type type))))
    (if (unknown-type-p ctype)
        (%%typep object ctype)
        ;; Most of the time an undefined type becomes defined is
        ;; through structure or class definition, optimize that case
        (let ((fun
                (if (classoid-p ctype)
                    (lambda (cache object)
                      ;; TODO: structures can be optimized even further
                      (block nil
                        (classoid-typep
                         (typecase object
                           (instance (%instance-layout object))
                           (funcallable-instance
                            (%funcallable-instance-layout object))
                           (t (return)))
                         (cdr (truly-the cons cache))
                         object)))
                    (lambda (cache object)
                      (%%typep object (cdr (truly-the cons cache)))))))
          (setf (cdr cache) ctype)
          (sb-thread:barrier (:write))
          (setf (car cache) fun)
          (funcall fun cache object)))))

;;; Do a type test from a class cell, allowing forward reference and
;;; redefinition.
(defun classoid-cell-typep (cell object)
  (let ((layout (typecase object
                  (instance (%instance-layout object))
                  (funcallable-instance (%funcallable-instance-layout object))
                  (t (return-from classoid-cell-typep))))
        (classoid (classoid-cell-classoid cell)))
    (unless classoid
      (error "The class ~S has not yet been defined."
             (classoid-cell-name cell)))
    (classoid-typep layout classoid object)))

;;; Test whether OBJ-LAYOUT is from an instance of CLASSOID.

;;; IMPORTANT: If none of the classes involved (directly or indirectly)
;;; in a call to CLASSOID-TYPEP gets redefined during execution of the predicate,
;;; the layout update loop should require at most 2 iterations.
;;; Theoretically, ensuring validity of the classoid layout and the object layout
;;; could be done in either order, * HOWEVER * it is less racy to perform
;;; them in this exact order. Consider the case that OBJ-LAYOUT is T
;;; for a class that satisfies CLASS-FINALIZED-P and suppose these operations were
;;; reversed from the order below. UPDATE-OBJECT-LAYOUT-OR-INVALID is going to make
;;; a new layout, registering it and installing into the classoid.
;;; Then %ENSURE-CLASSOID-VALID is going to call %FORCE-CACHE-FLUSHES which is going
;;; to make yet another new layout. The "transitivity of wrapper updates" usually
;;; causes the first new layout to automatically update to the second new layout,
;;; except that the other thread has already fetched the old layout.
;;; But by using the order below, there will not be two new layouts made, only one,
;;; because UPDATE-OBJECT-LAYOUT-OR-INVALID is able to use the layout
;;; that was updated into the classoid by %ENSURE-CLASSOID-VALID.
;;; All other things being equal, one new layout is better than two.
;;; At least I think that's what happens.
;;; So consider what happens if two threads are both doing this -
;;; with the opposite order, there could have been as many as 5 new layouts
;;; created (empirically observed via instrumentation of MAKE-LAYOUT) -
;;; two per thread; plus one more, which is the failing one.
;;; It was even possible to have *BOTH* threads fail the AVER, although more often it
;;; was just one of them that fails.
;;; With the order of operations below, I observed no failures in hundreds
;;; of thousands of iterations of 'classoid-typep.impure.lisp'

(defun classoid-typep (obj-layout classoid object)
  ;; FIXME & KLUDGE: We could like to grab the *WORLD-LOCK* here (to ensure that
  ;; class graph doesn't change while we're doing the typep test), but in
  ;; practice that causes trouble -- deadlocking against the compiler
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
        (%ensure-classoid-valid classoid layout "typep")
        (when (layout-invalid obj-layout)
          (setq obj-layout (update-object-layout-or-invalid object layout))))
    (or (eq obj-layout layout)
        (let ((obj-inherits (layout-inherits obj-layout)))
          (dotimes (i (length obj-inherits) nil)
            (when (eq (svref obj-inherits i) layout)
              (return t)))))))
