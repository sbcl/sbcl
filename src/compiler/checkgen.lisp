;;;; This file implements type check generation. This is a phase that
;;;; runs at the very end of IR1. If a type check is too complex for
;;;; the back end to directly emit in-line, then we transform the check
;;;; into an explicit conditional using TYPEP.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;;; cost estimation

;;; Return some sort of guess about the cost of a call to a function.
;;; If the function has some templates, we return the cost of the
;;; cheapest one, otherwise we return the cost of CALL-NAMED. Calling
;;; this with functions that have transforms can result in relatively
;;; meaningless results (exaggerated costs.)
;;;
;;; We special-case NULL, since it does have a source tranform and is
;;; interesting to us.
(defun fun-guessed-cost (name)
  (declare (symbol name))
  (let ((info (info :function :info name))
	(call-cost (template-cost (template-or-lose 'call-named))))
    (if info
	(let ((templates (fun-info-templates info)))
	  (if templates
	      (template-cost (first templates))
	      (case name
		(null (template-cost (template-or-lose 'if-eq)))
		(t call-cost))))
	call-cost)))

;;; Return some sort of guess for the cost of doing a test against
;;; TYPE. The result need not be precise as long as it isn't way out
;;; in space. The units are based on the costs specified for various
;;; templates in the VM definition.
(defun type-test-cost (type)
  (declare (type ctype type))
  (or (let ((check (type-check-template type)))
	(if check
	    (template-cost check)
	    (let ((found (cdr (assoc type *backend-type-predicates*
				     :test #'type=))))
	      (if found
		  (+ (fun-guessed-cost found) (fun-guessed-cost 'eq))
		  nil))))
      (typecase type
	(compound-type
	 (reduce #'+ (compound-type-types type) :key 'type-test-cost))
	(member-type
	 (* (length (member-type-members type))
	    (fun-guessed-cost 'eq)))
	(numeric-type
	 (* (if (numeric-type-complexp type) 2 1)
	    (fun-guessed-cost
	     (if (csubtypep type (specifier-type 'fixnum)) 'fixnump 'numberp))
	    (+ 1
	       (if (numeric-type-low type) 1 0)
	       (if (numeric-type-high type) 1 0))))
	(cons-type
	 (+ (type-test-cost (specifier-type 'cons))
	    (fun-guessed-cost 'car)
	    (type-test-cost (cons-type-car-type type))
	    (fun-guessed-cost 'cdr)
	    (type-test-cost (cons-type-cdr-type type))))
	(t
	 (fun-guessed-cost 'typep)))))

(defun-cached
    (weaken-type :hash-bits 8
                 :hash-function (lambda (x)
                                  (logand (type-hash-value x) #xFF)))
    ((type eq))
  (declare (type ctype type))
  (let ((min-cost (type-test-cost type))
        (min-type type)
        (found-super nil))
    (dolist (x *backend-type-predicates*)
      (let ((stype (car x)))
        (when (and (csubtypep type stype)
                   (not (union-type-p stype)))
          (let ((stype-cost (type-test-cost stype)))
            (when (or (< stype-cost min-cost)
                      (type= stype type))
              ;; If the supertype is equal in cost to the type, we
              ;; prefer the supertype. This produces a closer
              ;; approximation of the right thing in the presence of
              ;; poor cost info.
              (setq found-super t
                    min-type stype
                    min-cost stype-cost))))))
    (if found-super
        min-type
        *universal-type*)))

(defun weaken-values-type (type)
  (declare (type ctype type))
  (cond ((eq type *wild-type*) type)
        ((not (values-type-p type))
         (weaken-type type))
        (t
         (make-values-type :required (mapcar #'weaken-type
                                             (values-type-required type))
                           :optional (mapcar #'weaken-type
                                             (values-type-optional type))
                           :rest (acond ((values-type-rest type)
                                         (weaken-type it)))))))

;;;; checking strategy determination

;;; Return the type we should test for when we really want to check
;;; for TYPE. If type checking policy is "fast", then we return a
;;; weaker type if it is easier to check. First we try the defined
;;; type weakenings, then look for any predicate that is cheaper.
(defun maybe-weaken-check (type policy)
  (declare (type ctype type))
  (ecase (policy policy type-check)
    (0 *wild-type*)
    (2 (weaken-values-type type))
    (3 type)))

;;; This is like VALUES-TYPES, only we mash any complex function types
;;; to FUNCTION.
(defun no-fun-values-types (type)
  (declare (type ctype type))
  (multiple-value-bind (res count) (values-types type)
    (values (mapcar (lambda (type)
		      (if (fun-type-p type)
			  (specifier-type 'function)
			  type))
		    res)
	    count)))

;;; Switch to disable check complementing, for evaluation.
(defvar *complement-type-checks* t)

;;; CONT is a continuation we are doing a type check on and TYPES is a
;;; list of types that we are checking its values against. If we have
;;; proven that CONT generates a fixed number of values, then for each
;;; value, we check whether it is cheaper to then difference between
;;; the proven type and the corresponding type in TYPES. If so, we opt
;;; for a :HAIRY check with that test negated. Otherwise, we try to do
;;; a simple test, and if that is impossible, we do a hairy test with
;;; non-negated types. If true, FORCE-HAIRY forces a hairy type check.
;;;
;;; When doing a non-negated check, we call MAYBE-WEAKEN-CHECK to
;;; weaken the test to a convenient supertype (conditional on policy.)
;;; If SPEED is 3, or DEBUG-INFO is not particularly important (DEBUG
;;; <= 1), then we allow weakened checks to be simple, resulting in
;;; less informative error messages, but saving space and possibly
;;; time.
;;;
;;; FIXME: I don't quite understand this, but it looks as though
;;; that means type checks are weakened when SPEED=3 regardless of
;;; the SAFETY level, which is not the right thing to do.
(defun maybe-negate-check (cont types original-types force-hairy)
  (declare (type continuation cont) (list types))
  (multiple-value-bind (ptypes count)
      (no-fun-values-types (continuation-derived-type cont))
    (if (eq count :unknown)
        (if (and (every #'type-check-template types) (not force-hairy))
            (values :simple types)
            (values :hairy (mapcar (lambda (x) (list nil x x)) types)))
        (let ((res (mapcar (lambda (p c a)
                             (let ((diff (type-difference p c)))
                               (if (and diff
                                        (< (type-test-cost diff)
                                           (type-test-cost c))
                                        *complement-type-checks*)
                                   (list t diff a)
                                   (list nil c a))))
                           ptypes types original-types)))
          (cond ((or force-hairy (find-if #'first res))
                 (values :hairy res))
                ((every #'type-check-template types)
                 (values :simple types))
                (t
                 (values :hairy res)))))))

;;; Determines whether CONT's assertion is:
;;;  -- checkable by the back end (:SIMPLE), or
;;;  -- not checkable by the back end, but checkable via an explicit 
;;;     test in type check conversion (:HAIRY), or
;;;  -- not reasonably checkable at all (:TOO-HAIRY).
;;;
;;; A type is checkable if it either represents a fixed number of
;;; values (as determined by VALUES-TYPES), or it is the assertion for
;;; an MV-BIND. A type is simply checkable if all the type assertions
;;; have a TYPE-CHECK-TEMPLATE. In this :SIMPLE case, the second value
;;; is a list of the type restrictions specified for the leading
;;; positional values.
;;;
;;; We force a check to be hairy even when there are fixed values if
;;; we are in a context where we may be forced to use the unknown
;;; values convention anyway. This is because IR2tran can't generate
;;; type checks for unknown values continuations but people could
;;; still be depending on the check being done. We only care about
;;; EXIT and RETURN (not MV-COMBINATION) since these are the only
;;; contexts where the ultimate values receiver
;;;
;;; In the :HAIRY case, the second value is a list of triples of
;;; the form:
;;;    (NOT-P TYPE ORIGINAL-TYPE)
;;;
;;; If true, the NOT-P flag indicates a test that the corresponding
;;; value is *not* of the specified TYPE. ORIGINAL-TYPE is the type
;;; asserted on this value in the continuation, for use in error
;;; messages. When NOT-P is true, this will be different from TYPE.
;;;
;;; This allows us to take what has been proven about CONT's type into
;;; consideration. If it is cheaper to test for the difference between
;;; the derived type and the asserted type, then we check for the
;;; negation of this type instead.
(defun cast-check-types (cast force-hairy)
  (declare (type cast cast))
  (let* ((cont (node-cont cast))
         (ctype (coerce-to-values (cast-type-to-check cast)))
         (atype (coerce-to-values (cast-asserted-type cast)))
         (value (cast-value cast))
         (vtype (continuation-derived-type value))
         (dest (continuation-dest cont)))
    (aver (not (eq ctype *wild-type*)))
    (multiple-value-bind (ctypes count) (no-fun-values-types ctype)
      (multiple-value-bind (atypes acount) (no-fun-values-types atype)
        (multiple-value-bind (vtypes vcount) (values-types vtype)
          (declare (ignore vtypes))
          (aver (eq count acount))
          (cond ((not (eq count :unknown))
                 (if (or (exit-p dest)
                         (and (return-p dest)
                              (multiple-value-bind (ignore count)
                                  (values-types (return-result-type dest))
                                (declare (ignore ignore))
                                (eq count :unknown))))
                     (maybe-negate-check value ctypes atypes t)
                     (maybe-negate-check value ctypes atypes force-hairy)))
                ((and (continuation-single-value-p cont)
                      (or (not (args-type-rest ctype))
                          (eq (args-type-rest ctype) *universal-type*)))
                 (principal-continuation-single-valuify cont)
                 (let ((creq (car (args-type-required ctype))))
                   (multiple-value-setq (ctype atype)
                     (if creq
                         (values creq (car (args-type-required atype)))
                         (values (car (args-type-optional ctype))
                                 (car (args-type-optional atype)))))
                   (maybe-negate-check value
                                       (list ctype) (list atype)
                                       force-hairy)))
                ((and (mv-combination-p dest)
                      (eq (mv-combination-kind dest) :local))
                 (let* ((fun-ref (continuation-use (mv-combination-fun dest)))
                        (length (length (lambda-vars (ref-leaf fun-ref)))))
                   (maybe-negate-check value
                                       ;; FIXME
                                       (adjust-list (values-type-types ctype)
                                                    length
                                                    *universal-type*)
                                       (adjust-list (values-type-types atype)
                                                    length
                                                    *universal-type*)
                                       force-hairy)))
                ((not (eq vcount :unknown))
                 (maybe-negate-check value
                                     (values-type-out ctype vcount)
                                     (values-type-out atype vcount)
                                     t))
                (t
                 (values :too-hairy nil))))))))

;;; Do we want to do a type check?
(defun worth-type-check-p (cast)
  (declare (type cast cast))
  (let* ((cont (node-cont cast))
         (dest (continuation-dest cont)))
    (cond ((not (cast-type-check cast))
           nil)
          ((and (combination-p dest)
                (call-full-like-p dest)
                ;; The theory is that the type assertion is
                ;; from a declaration in (or on) the callee,
                ;; so the callee should be able to do the
                ;; check. We want to let the callee do the
                ;; check, because it is possible that by the
                ;; time of call that declaration will be
                ;; changed and we do not want to make people
                ;; recompile all calls to a function when they
                ;; were originally compiled with a bad
                ;; declaration. (See also bug 35.)
                (immediately-used-p cont cast)
                (values-subtypep (continuation-externally-checkable-type cont)
                                 (cast-type-to-check cast)))
           nil)
          (t
           t))))

;;; Return true if CONT is a continuation whose type the back end is
;;; likely to want to check. Since we don't know what template the
;;; back end is going to choose to implement the continuation's DEST,
;;; we use a heuristic. We always return T unless:
;;;  -- nobody uses the value, or
;;;  -- safety is totally unimportant, or
;;;  -- the continuation is an argument to an unknown function, or
;;;  -- the continuation is an argument to a known function that has
;;;     no IR2-CONVERT method or :FAST-SAFE templates that are
;;;     compatible with the call's type.
(defun probable-type-check-p (cast)
  (declare (type cast cast))
  (let* ((cont (node-cont cast))
         (dest (continuation-dest cont)))
    (cond ((not dest) nil)
          (t t))
    #+nil
    (cond ((or (not dest)
	       (policy dest (zerop safety)))
	   nil)
	  ((basic-combination-p dest)
	   (let ((kind (basic-combination-kind dest)))
	     (cond ((eq cont (basic-combination-fun dest)) t)
		   ((eq kind :local) t)
                   ((eq kind :full)
                    (and (combination-p dest)
                         (not (values-subtypep ; explicit THE
                               (continuation-externally-checkable-type cont)
                               (continuation-type-to-check cont)))))

		   ((eq kind :error) nil)
                   ;; :ERROR means that we have an invalid syntax of
                   ;; the call and the callee will detect it before
                   ;; thinking about types.

		   ((fun-info-ir2-convert kind) t)
		   (t
		    (dolist (template (fun-info-templates kind) nil)
		      (when (eq (template-ltn-policy template) :fast-safe)
			(multiple-value-bind (val win)
			    (valid-fun-use dest (template-type template))
			  (when (or val (not win)) (return t)))))))))
	  (t t))))

;;; Return a lambda form that we can convert to do a hairy type check
;;; of the specified TYPES. TYPES is a list of the format returned by
;;; CONTINUATION-CHECK-TYPES in the :HAIRY case.
;;;
;;; Note that we don't attempt to check for required values being
;;; unsupplied. Such checking is impossible to efficiently do at the
;;; source level because our fixed-values conventions are optimized
;;; for the common MV-BIND case.
(defun make-type-check-form (types)
  (let ((temps (make-gensym-list (length types))))
    `(multiple-value-bind ,temps
         'dummy
       ,@(mapcar (lambda (temp type)
                   (let* ((spec
                           (let ((*unparse-fun-type-simplify* t))
                             (type-specifier (second type))))
                          (test (if (first type) `(not ,spec) spec)))
                     `(unless (typep ,temp ',test)
                        (%type-check-error
                         ,temp
                         ',(type-specifier (third type))))))
                 temps
                 types)
       (values ,@temps))))

;;; Splice in explicit type check code immediately before the node
;;; which is CONT's DEST. This code receives the value(s) that were
;;; being passed to CONT, checks the type(s) of the value(s), then
;;; passes them on to CONT.
(defun convert-type-check (cast types)
  (declare (type cast cast) (type list types))
  (let ((cont (cast-value cast))
        (length (length types)))
    (filter-continuation cont (make-type-check-form types))
    (reoptimize-continuation (cast-value cast))
    (setf (cast-type-to-check cast) *wild-type*)
    (setf (cast-%type-check cast) nil)
    (let* ((atype (cast-asserted-type cast))
           (atype (cond ((not (values-type-p atype))
		         atype)
			((= length 1)
                         (single-value-type atype))
                        (t
			 (make-values-type
                          :required (values-type-out atype length)))))
           (dtype (node-derived-type cast))
           (dtype (make-values-type
                   :required (values-type-out dtype length))))
      (setf (cast-asserted-type cast) atype)
      (setf (node-derived-type cast) dtype)))

  (values))

;;; Check all possible arguments of CAST and emit type warnings for
;;; those with type errors. If the value of USE is being used for a
;;; variable binding, we figure out which one for source context. If
;;; the value is a constant, we print it specially.
(defun cast-check-uses (cast)
  (declare (type cast cast))
  (let* ((cont (node-cont cast))
         (dest (continuation-dest cont))
         (value (cast-value cast))
         (atype (cast-asserted-type cast)))
    (do-uses (use value)
      (let ((dtype (node-derived-type use)))
        (unless (values-types-equal-or-intersect dtype atype)
          (let* ((*compiler-error-context* use)
                 (atype-spec (type-specifier atype))
                 (what (when (and (combination-p dest)
                                  (eq (combination-kind dest) :local))
                         (let ((lambda (combination-lambda dest))
                               (pos (position-or-lose
                                     cont (combination-args dest))))
                           (format nil "~:[A possible~;The~] binding of ~S"
                                   (and (continuation-use cont)
                                        (eq (functional-kind lambda) :let))
                                   (leaf-source-name (elt (lambda-vars lambda)
                                                          pos)))))))
            (cond ((and (ref-p use) (constant-p (ref-leaf use)))
                   (compiler-warn "~:[This~;~:*~A~] is not a ~<~%~9T~:;~S:~>~%  ~S"
                                  what atype-spec (constant-value (ref-leaf use))))
                  (t
                   (compiler-warn
                    "~:[Result~;~:*~A~] is a ~S, ~<~%~9T~:;not a ~S.~>"
                    what (type-specifier dtype) atype-spec))))))))
  (values))

;;; Loop over all blocks in COMPONENT that have TYPE-CHECK set,
;;; looking for continuations with TYPE-CHECK T. We do two mostly
;;; unrelated things: detect compile-time type errors and determine if
;;; and how to do run-time type checks.
;;;
;;; If there is a compile-time type error, then we mark the
;;; continuation and emit a warning if appropriate. This part loops
;;; over all the uses of the continuation, since after we convert the
;;; check, the :DELETED kind will inhibit warnings about the types of
;;; other uses.
;;;
;;; If a continuation is too complex to be checked by the back end, or
;;; is better checked with explicit code, then convert to an explicit
;;; test. Assertions that can checked by the back end are passed
;;; through. Assertions that can't be tested are flamed about and
;;; marked as not needing to be checked.
;;;
;;; If we determine that a type check won't be done, then we set
;;; TYPE-CHECK to :NO-CHECK. In the non-hairy cases, this is just to
;;; prevent us from wasting time coming to the same conclusion again
;;; on a later iteration. In the hairy case, we must indicate to LTN
;;; that it must choose a safe implementation, since IR2 conversion
;;; will choke on the check.
;;;
;;; The generation of the type checks is delayed until all the type
;;; check decisions have been made because the generation of the type
;;; checks creates new nodes whose derived types aren't always updated
;;; which may lead to inappropriate template choices due to the
;;; modification of argument types.
(defun generate-type-checks (component)
  (collect ((casts))
    (do-blocks (block component)
      (when (block-type-check block)
	(do-nodes (node cont block)
          (when (and (cast-p node)
                     (cast-type-check node))
            (cast-check-uses node)
            (cond ((worth-type-check-p node)
                   (casts (cons node (not (probable-type-check-p node)))))
                  (t
                   (setf (cast-%type-check node) nil)
                   (setf (cast-type-to-check node) *wild-type*)))))
	(setf (block-type-check block) nil)))
    (dolist (cast (casts))
      (destructuring-bind (cast . force-hairy) cast
        (multiple-value-bind (check types)
            (cast-check-types cast force-hairy)
          (ecase check
            (:simple)
            (:hairy
             (convert-type-check cast types))
            (:too-hairy
             (let ((*compiler-error-context* cast))
               (when (policy cast (>= safety inhibit-warnings))
                 (compiler-notify
                  "type assertion too complex to check:~% ~S."
                  (type-specifier (coerce-to-values (cast-asserted-type cast))))))
             (setf (cast-type-to-check cast) *wild-type*)
             (setf (cast-%type-check cast) nil)))))))
  (values))
