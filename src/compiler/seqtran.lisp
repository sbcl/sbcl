;;;; optimizers for list and sequence functions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;; Regarding several reader-conditionalized MUFFLE-CONDITION declarations
;;; throughout this file, here's why: Transforms produce sexprs that as far
;;; as the compiler is concerned could have been user-supplied.
;;; In as much as the forms mention a type-name, that name had best be
;;; recognized, or else it's a style-warning (or worse).
;;; And in cross-compilation, COMPILER-NOTE isn't known early enough for
;;; the transforms in this file to refer to a known type.
;;; But mysteriously the xc would report a style-warning about COMPILER-NOTE
;;; - or any descendant - being unknown, and then go on with life.
;;; How was this possible? Well, it's only trying to _parse_ the type
;;; on account of the declarations saying to muffle things of that type.
;;; Indeed the declaration merits a warning.
;;; But this is an extremely sad and confusing state of affairs,
;;; because while we expect some CODE-DELETION-NOTEs, we don't expect to see
;;; that CODE-DELETION-NOTE is an undefined type.
;;; Alternatively, we could invent DEFINE!-CONDITION which would cause
;;; the cross-compiler to be born knowing all the required types.
;;; Moreover, it would be nice if some of the declarations were commented
;;; with their reason for existence.


;;;; mapping onto lists: the MAPFOO functions

;; This expander allows a compiler-macro for FN to take effect by eliding
;; a LET binding of it.  Attempting to self-optimize like that isn't the usual
;; SBCL way, however this is a countermeasure to an inhibition of a later
;; optimization, and it is not an onerous change to the expander.
;; We've gone to the trouble of inlining MAPfoo, but the inlined code
;; prevented use of a compiler-macro because %FUNCALL (as opposed to FUNCALL)
;; is not recognized. "Fixing" the compiler to understand %FUNCALL being
;; the same isn't enough: the funarg must be a literal form because we can't
;; know that a variable arg is a never-modified binding of 'F or #'F
;; until IR1 has figured that out, at which point it is too late.
;; [However, see lp# 632368 which asks for something like that.]
;;
;; Also, you might think there to be a subtle difference in behavior from
;; delaying the reference to #'F versus referencing it once. But there is no
;; difference - either way will use the #<fdefn> of F in the call.
;; Of course, it would be ridiculously unportable to to rely on the
;; fact that F can be changed (for its next call) while funcalling it.
;;
(defun mapfoo-transform (fn arglists accumulate take-car)
  (collect ((do-clauses)
            (args-to-fn)
            (tests))
    (let ((n-first (gensym)))
      (dolist (a (if accumulate
                     arglists
                     `(,n-first ,@(rest arglists))))
        (let ((v (gensym)))
          (do-clauses `(,v ,a (cdr ,v)))
          (tests `(endp ,v))
          (args-to-fn (if take-car `(car ,v) v))))

      (binding* (((fn-binding call) (funarg-bind/call-forms fn (args-to-fn)))
                 (endtest `(or ,@(tests))))
        `(let ,fn-binding
           ,(ecase accumulate
             (:nconc
              (let ((temp (gensym))
                    (map-result (gensym)))
                `(let ((,map-result
                        ;; MUFFLE- is not injected when cross-compiling.
                        ;; See top of file for explanation.
                        (locally
                            #-sb-xc-host
                            (declare (muffle-conditions compiler-note))
                          (list nil))))
                   (declare (truly-dynamic-extent ,map-result))
                   (do-anonymous ((,temp ,map-result) . ,(do-clauses))
                     (,endtest (cdr ,map-result))
                     (setq ,temp (last (nconc ,temp ,call)))))))
             (:list
              (let ((temp (gensym))
                    (map-result (gensym)))
                `(let ((,map-result
                        ;; MUFFLE- is not injected when cross-compiling.
                        ;; See top of file for explanation.
                        (locally
                            #-sb-xc-host
                            (declare (muffle-conditions compiler-note))
                          (list nil))))
                   (declare (truly-dynamic-extent ,map-result))
                   (do-anonymous ((,temp ,map-result) . ,(do-clauses))
                     (,endtest (truly-the list (cdr ,map-result)))
                     ;; Accumulate using %RPLACD. RPLACD becomes (SETF CDR)
                     ;; which becomes %RPLACD but relies on "defsetfs".
                     ;; This is for effect, not value, so makes no difference.
                     (%rplacd ,temp (setq ,temp (list ,call)))))))
             ((nil)
              `(let ((,n-first ,(first arglists)))
                 (do-anonymous ,(do-clauses)
                   (,endtest (truly-the list ,n-first))
                   ,call)))))))))

(define-source-transform mapc (function list &rest more-lists)
  (mapfoo-transform function (cons list more-lists) nil t))

(define-source-transform mapcar (function list &rest more-lists)
  (mapfoo-transform function (cons list more-lists) :list t))

(define-source-transform mapcan (function list &rest more-lists)
  (mapfoo-transform function (cons list more-lists) :nconc t))

(define-source-transform mapl (function list &rest more-lists)
  (mapfoo-transform function (cons list more-lists) nil nil))

(define-source-transform maplist (function list &rest more-lists)
  (mapfoo-transform function (cons list more-lists) :list nil))

(define-source-transform mapcon (function list &rest more-lists)
  (mapfoo-transform function (cons list more-lists) :nconc nil))

;;;; mapping onto sequences: the MAP function

;;; MAP is %MAP plus a check to make sure that any length specified in
;;; the result type matches the actual result. We also wrap it in a
;;; TRULY-THE for the most specific type we can determine.
(deftransform map ((result-type-arg fun seq &rest seqs) * * :node node)
  (let* ((seq-names (make-gensym-list (1+ (length seqs))))
         (bare `(%map result-type-arg fun ,@seq-names))
         (constant-result-type-arg-p (constant-lvar-p result-type-arg))
         ;; what we know about the type of the result. (Note that the
         ;; "result type" argument is not necessarily the type of the
         ;; result, since NIL means the result has NULL type.)
         (result-type (if (not constant-result-type-arg-p)
                          'consed-sequence
                          (let ((result-type-arg-value
                                 (lvar-value result-type-arg)))
                            (if (null result-type-arg-value)
                                'null
                                result-type-arg-value)))))
    `(lambda (result-type-arg fun ,@seq-names)
       (truly-the ,result-type
         ,(cond ((policy node (< safety 3))
                 ;; ANSI requires the length-related type check only
                 ;; when the SAFETY quality is 3... in other cases, we
                 ;; skip it, because it could be expensive.
                 bare)
                ((not constant-result-type-arg-p)
                 `(sequence-of-checked-length-given-type ,bare
                                                         result-type-arg))
                (t
                 (let ((result-ctype (ir1-transform-specifier-type
                                      result-type)))
                   (if (array-type-p result-ctype)
                       (let ((dims (array-type-dimensions result-ctype)))
                         (unless (singleton-p dims)
                           (give-up-ir1-transform "invalid sequence type"))
                         (let ((dim (first dims)))
                           (if (eq dim '*)
                               bare
                               `(vector-of-checked-length-given-length ,bare
                                                                       ,dim))))
                       ;; FIXME: this is wrong, as not all subtypes of
                       ;; VECTOR are ARRAY-TYPEs [consider, for
                       ;; example, (OR (VECTOR T 3) (VECTOR T
                       ;; 4))]. However, it's difficult to see what we
                       ;; should put here... maybe we should
                       ;; GIVE-UP-IR1-TRANSFORM if the type is a
                       ;; subtype of VECTOR but not an ARRAY-TYPE?
                       bare))))))))

;;; Return a DO loop, mapping a function FUN to elements of
;;; sequences. SEQS is a list of lvars, SEQ-NAMES - list of variables,
;;; bound to sequences, INTO - a variable, which is used in
;;; MAP-INTO. RESULT and BODY are forms, which can use variables
;;; FUNCALL-RESULT, containing the result of application of FUN, and
;;; INDEX, containing the current position in sequences.
(defun build-sequence-iterator (seqs seq-names &key result into body fast)
  (declare (type list seqs seq-names)
           (type symbol into))
  (collect ((bindings)
            (declarations)
            (vector-lengths)
            (tests)
            (places)
            (around))
    (let ((found-vector-p nil))
      (flet ((process-vector (length)
               (unless found-vector-p
                 (setq found-vector-p t)
                 (bindings `(index 0 (1+ index)))
                 (declarations `(type index index)))
               (vector-lengths length)))
        (loop for seq of-type lvar in seqs
           for seq-name in seq-names
           for type = (lvar-type seq)
           do (cond ((csubtypep type (specifier-type 'list))
                     (with-unique-names (index)
                       (bindings `(,index ,seq-name (cdr ,index)))
                       (declarations `(type list ,index))
                       (places `(car ,index))
                       (tests `(endp ,index))))
                    ((or (csubtypep type (specifier-type '(simple-array * 1)))
                         (and (not fast)
                              (csubtypep type (specifier-type 'vector))))
                     (process-vector `(length ,seq-name))
                     (places `(locally (declare (optimize (insert-array-bounds-checks 0)))
                                (aref ,seq-name index))))
                    ((csubtypep type (specifier-type 'vector))
                     (let ((data  (gensym "DATA"))
                           (start (gensym "START"))
                           (end   (gensym "END")))
                       (around `(with-array-data ((,data ,seq-name)
                                                  (,start)
                                                  (,end (length ,seq-name)))))
                       (process-vector `(- ,end ,start))
                       (places `(locally (declare (optimize (insert-array-bounds-checks 0)))
                                  (aref ,data (truly-the index (+ index ,start)))))))
                    (t
                     (give-up-ir1-transform
                      "can't determine sequence argument type"))))
        (when into
          (process-vector `(array-dimension ,into 0))))
      (when found-vector-p
        (bindings `(length (min ,@(vector-lengths))))
        (tests `(>= index length)))
      (let ((body `(do (,@(bindings))
                       ((or ,@(tests)) ,result)
                     (declare ,@(declarations))
                     (let ((funcall-result (funcall fun ,@(places))))
                       (declare (ignorable funcall-result))
                       ,body))))
        (if (around)
            (reduce (lambda (wrap body) (append wrap (list body)))
                    (around)
                    :from-end t
                    :initial-value body)
            body)))))

;;; Try to compile %MAP efficiently when we can determine sequence
;;; argument types at compile time.
(deftransform %map ((result-type fun seq &rest seqs) * *
                    :node node :policy (>= speed space))
  "open code"
  (unless (constant-lvar-p result-type)
    (give-up-ir1-transform "RESULT-TYPE argument not constant"))
  (flet ( ;; 1-valued SUBTYPEP, fails unless second value of SUBTYPEP is true
         (1subtypep (x y)
           (multiple-value-bind (subtype-p valid-p) (sb!xc:subtypep x y)
             (if valid-p
                 subtype-p
                 (give-up-ir1-transform
                  "can't analyze sequence type relationship")))))
    (let* ((result-type-value (lvar-value result-type))
           (result-supertype (cond ((null result-type-value) 'null)
                                   ((1subtypep result-type-value 'vector)
                                    'vector)
                                   ((1subtypep result-type-value 'list)
                                    'list)
                                   (t
                                    (give-up-ir1-transform
                                     "result type unsuitable")))))
      (cond ((and (eq result-supertype 'list) (null seqs))
             ;; The consing arity-1 cases can be implemented
             ;; reasonably efficiently as function calls, and the cost
             ;; of consing should be significantly larger than
             ;; function call overhead, so we always compile these
             ;; cases as full calls regardless of speed-versus-space
             ;; optimization policy.
             '(%map-to-list-arity-1 fun seq))
            ;; (We use the same idiom, of returning a LAMBDA from
            ;; DEFTRANSFORM, as is used in the DEFTRANSFORMs for
            ;; FUNCALL and ALIEN-FUNCALL, and for the same
            ;; reason: we need to get the runtime values of each
            ;; of the &REST vars.)
            ((eq result-supertype 'vector)
             (let* ((all-seqs (cons seq seqs))
                    (seq-args (make-gensym-list (length all-seqs))))
               `(lambda (result-type fun ,@seq-args)
                  (map-into (make-sequence result-type
                                           (min ,@(loop for arg in seq-args
                                                        collect `(length ,arg))))
                            fun ,@seq-args))))
            (t
             (let* ((all-seqs (cons seq seqs))
                    (seq-args (make-gensym-list (length all-seqs))))
               (multiple-value-bind (push-dacc result)
                   (ecase result-supertype
                     (null (values nil nil))
                     (list (values `(push funcall-result acc)
                                   `(nreverse acc))))
                 (catch-give-up-ir1-transform
                     (`(lambda (result-type fun ,@seq-args)
                         (declare (ignore result-type))
                         (let ((fun (%coerce-callable-to-fun fun))
                               (acc nil))
                           (declare (type list acc))
                           (declare (ignorable acc))
                           ,(build-sequence-iterator
                             all-seqs seq-args
                             :result result
                             :body push-dacc
                             :fast (policy node (> speed space))))))
                   (if (and (null result-type-value) (null seqs))
                       '(%map-for-effect-arity-1 fun seq)
                       (%give-up))))))))))

;;; MAP-INTO
(deftransform map-into ((result fun &rest seqs)
                        (vector * &rest *)
                        * :node node)
  "open code"
  (let* ((seqs-names (make-gensym-list (length seqs)))
         (result-type (lvar-type result))
         (non-complex-vector-type-p (csubtypep result-type
                                               (specifier-type '(simple-array * 1)))))
    (catch-give-up-ir1-transform
        (`(lambda (result fun ,@seqs-names)
            ,(if (and (policy node (> speed space))
                      (not non-complex-vector-type-p))
                 (let ((data  (gensym "DATA"))
                       (start (gensym "START"))
                       (end   (gensym "END")))
                   `(with-array-data ((,data result)
                                      (,start)
                                      (,end))
                      (declare (ignore ,end))
                      ,(build-sequence-iterator
                        seqs seqs-names
                        :result '(when (array-has-fill-pointer-p result)
                                  (setf (fill-pointer result) index))
                        :into 'result
                        :body `(locally (declare (optimize (insert-array-bounds-checks 0)))
                                 (setf (aref ,data (truly-the index (+ index ,start)))
                                       funcall-result))
                        :fast t)))
                 (build-sequence-iterator
                  seqs seqs-names
                  :result '(when (array-has-fill-pointer-p result)
                            (setf (fill-pointer result) index))
                  :into 'result
                  :body '(locally (declare (optimize (insert-array-bounds-checks 0)))
                          (setf (aref result index) funcall-result))))
            result))
      (cond #-sb-xc-host
            ;; %%vector-map-into-funs%% is not defined in xc
            ;; if something needs to be faster in the compiler, it
            ;; should declare the input sequences instead.
            ((and non-complex-vector-type-p
                  (array-type-p result-type)
                  (not (eq (array-type-specialized-element-type result-type)
                           *wild-type*)))
             (let ((saetp (find-saetp-by-ctype (array-type-specialized-element-type result-type))))
               (unless saetp
                 (give-up-ir1-transform "Uknown upgraded array element type of the result"))
               (let ((mapper (%fun-name (svref sb!impl::%%vector-map-into-funs%%
                                               (sb!vm:saetp-typecode saetp)))))
                 `(progn (,mapper result 0 (length result) (%coerce-callable-to-fun fun) seqs)
                         result))))
            (t
             (%give-up))))))


;;; FIXME: once the confusion over doing transforms with known-complex
;;; arrays is over, we should also transform the calls to (AND (ARRAY
;;; * (*)) (NOT (SIMPLE-ARRAY * (*)))) objects.
(deftransform elt ((s i) ((simple-array * (*)) *) *)
  '(aref s i))

(deftransform elt ((s i) (list *) * :policy (< safety 3))
  '(nth i s))

(deftransform %setelt ((s i v) ((simple-array * (*)) * *) *)
  '(setf (aref s i) v))

(deftransform %setelt ((s i v) (list * *) * :policy (< safety 3))
  '(setf (car (nthcdr i s)) v))

(deftransform %check-vector-sequence-bounds ((vector start end)
                                             (vector * *) *
                                             :node node)
  (if (policy node (= 0 insert-array-bounds-checks))
      '(or end (length vector))
      '(let ((length (length vector)))
         (if (<= 0 start (or end length) length)
             (or end length)
             (sequence-bounding-indices-bad-error vector start end)))))

(def!type eq-comparable-type ()
  '(or fixnum (not number)))

;;; True if EQL comparisons involving type can be simplified to EQ.
(defun eq-comparable-type-p (type)
  (csubtypep type (specifier-type 'eq-comparable-type)))

(defun specialized-list-seek-function-name (function-name key-functions &optional variant)
  (or (find-symbol (with-simple-output-to-string (s)
                     ;; Write "%NAME-FUN1-FUN2-FUN3", etc. Not only is
                     ;; this ever so slightly faster then FORMAT, this
                     ;; way we are also proof against *PRINT-CASE*
                     ;; frobbing and such.
                     (write-char #\% s)
                     (write-string (symbol-name function-name) s)
                     (dolist (f key-functions)
                       (write-char #\- s)
                       (write-string (symbol-name f) s))
                     (when variant
                       (write-char #\- s)
                       (write-string (symbol-name variant) s)))
                   (load-time-value (find-package "SB!KERNEL") t))
      (bug "Unknown list item seek transform: name=~S, key-functions=~S variant=~S"
           function-name key-functions variant)))

(defparameter *list-open-code-limit* 128)

(defun transform-list-item-seek (name item list key test test-not node)
  (when (and test test-not)
    (abort-ir1-transform "Both ~S and ~S supplied to ~S." :test :test-not name))
  ;; If TEST is EQL, drop it.
  (when (and test (lvar-fun-is test '(eql)))
    (setf test nil))
  ;; Ditto for KEY IDENTITY.
  (when (and key (lvar-fun-is key '(identity)))
    (setf key nil))
  ;; Key can legally be NIL, but if it's NIL for sure we pretend it's
  ;; not there at all. If it might be NIL, make up a form to that
  ;; ensures it is a function.
  (multiple-value-bind (key key-form)
      (when key
        (let ((key-type (lvar-type key))
              (null-type (specifier-type 'null)))
          (cond ((csubtypep key-type null-type)
                 (values nil nil))
                ((csubtypep null-type key-type)
                 (values key '(if key
                               (%coerce-callable-to-fun key)
                               #'identity)))
                (t
                 (values key (ensure-lvar-fun-form key 'key))))))
    (let* ((c-test (cond ((and test (lvar-fun-is test '(eq)))
                          (setf test nil)
                          'eq)
                         ((and (not test) (not test-not))
                          (when (eq-comparable-type-p (lvar-type item))
                            'eq))))
           (funs (delete nil (list (when key (list key 'key))
                                   (when test (list test 'test))
                                   (when test-not (list test-not 'test-not)))))
           (target-expr (if key '(%funcall key target) 'target))
           (test-expr (cond (test `(%funcall test item ,target-expr))
                            (test-not `(not (%funcall test-not item ,target-expr)))
                            (c-test `(,c-test item ,target-expr))
                            (t `(eql item ,target-expr)))))
      (labels ((open-code (tail)
                 (when tail
                   `(if (let ((this ',(car tail)))
                          ,(ecase name
                                  ((assoc rassoc)
                                   (let ((cxx (if (eq name 'assoc) 'car 'cdr)))
                                     `(and this (let ((target (,cxx this)))
                                                  ,test-expr))))
                                  (member
                                   `(let ((target this))
                                      ,test-expr))))
                        ',(ecase name
                                 ((assoc rassoc) (car tail))
                                 (member tail))
                        ,(open-code (cdr tail)))))
               (ensure-fun (args)
                 (if (eq 'key (second args))
                     key-form
                     (apply #'ensure-lvar-fun-form args))))
        (let* ((cp (constant-lvar-p list))
               (c-list (when cp (lvar-value list))))
          (cond ((and cp c-list (member name '(assoc rassoc member))
                      (policy node (>= speed space))
                      (not (nthcdr *list-open-code-limit* c-list)))
                 `(let ,(mapcar (lambda (fun) `(,(second fun) ,(ensure-fun fun))) funs)
                    ,(open-code c-list)))
                ((and cp (not c-list))
                 ;; constant nil list
                 (if (eq name 'adjoin)
                     '(list item)
                     nil))
                (t
                 ;; specialized out-of-line version
                 `(,(specialized-list-seek-function-name name (mapcar #'second funs) c-test)
                    item list ,@(mapcar #'ensure-fun funs)))))))))

(defun transform-list-pred-seek (name pred list key node)
  ;; If KEY is IDENTITY, drop it.
  (when (and key (lvar-fun-is key '(identity)))
    (setf key nil))
  ;; Key can legally be NIL, but if it's NIL for sure we pretend it's
  ;; not there at all. If it might be NIL, make up a form to that
  ;; ensures it is a function.
  (multiple-value-bind (key key-form)
      (when key
        (let ((key-type (lvar-type key))
              (null-type (specifier-type 'null)))
          (cond ((csubtypep key-type null-type)
                 (values nil nil))
                ((csubtypep null-type key-type)
                 (values key '(if key
                               (%coerce-callable-to-fun key)
                               #'identity)))
                (t
                 (values key (ensure-lvar-fun-form key 'key))))))
    (let ((test-expr `(%funcall pred ,(if key '(%funcall key target) 'target)))
          (pred-expr (ensure-lvar-fun-form pred 'pred)))
      (when (member name '(member-if-not assoc-if-not rassoc-if-not))
        (setf test-expr `(not ,test-expr)))
      (labels ((open-code (tail)
                 (when tail
                   `(if (let ((this ',(car tail)))
                          ,(ecase name
                                  ((assoc-if assoc-if-not rassoc-if rassoc-if-not)
                                   (let ((cxx (if (member name '(assoc-if assoc-if-not)) 'car 'cdr)))
                                     `(and this (let ((target (,cxx this)))
                                                  ,test-expr))))
                                  ((member-if member-if-not)
                                   `(let ((target this))
                                      ,test-expr))))
                        ',(ecase name
                                 ((assoc-if assoc-if-not rassoc-if rassoc-if-not)
                                  (car tail))
                                 ((member-if member-if-not)
                                  tail))
                        ,(open-code (cdr tail))))))
        (let* ((cp (constant-lvar-p list))
               (c-list (when cp (lvar-value list))))
          (cond ((and cp c-list (policy node (>= speed space))
                      (not (nthcdr *list-open-code-limit* c-list)))
                 `(let ((pred ,pred-expr)
                        ,@(when key `((key ,key-form))))
                    ,(open-code c-list)))
                ((and cp (not c-list))
                 ;; constant nil list -- nothing to find!
                 nil)
                (t
                 ;; specialized out-of-line version
                 `(,(specialized-list-seek-function-name name (when key '(key)))
                    ,pred-expr list ,@(when key (list key-form))))))))))

(macrolet ((def (name &optional if/if-not)
             (let ((basic (symbolicate "%" name))
                   (basic-eq (symbolicate "%" name "-EQ"))
                   (basic-key (symbolicate "%" name "-KEY"))
                   (basic-key-eq (symbolicate "%" name "-KEY-EQ")))
               `(progn
                  (deftransform ,name ((item list &key key test test-not) * * :node node)
                    (transform-list-item-seek ',name item list key test test-not node))
                  (deftransform ,basic ((item list) (eq-comparable-type t))
                    `(,',basic-eq item list))
                  (deftransform ,basic-key ((item list) (eq-comparable-type t))
                    `(,',basic-key-eq item list))
                  ,@(when if/if-not
                          (let ((if-name (symbolicate name "-IF"))
                                (if-not-name (symbolicate name "-IF-NOT")))
                            `((deftransform ,if-name ((pred list &key key) * * :node node)
                                (transform-list-pred-seek ',if-name pred list key node))
                              (deftransform ,if-not-name ((pred list &key key) * * :node node)
                                (transform-list-pred-seek ',if-not-name pred list key node)))))))))
  (def adjoin)
  (def assoc  t)
  (def member t)
  (def rassoc t))

(deftransform memq ((item list) (t (constant-arg list)))
  (labels ((rec (tail)
             (if tail
                 `(if (eq item ',(car tail))
                      ',tail
                      ,(rec (cdr tail)))
                 nil)))
    (rec (lvar-value list))))

;;; A similar transform used to apply to MEMBER and ASSOC, but since
;;; TRANSFORM-LIST-ITEM-SEEK now takes care of them those transform
;;; would never fire, and (%MEMBER-TEST ITEM LIST #'EQ) should be
;;; almost as fast as MEMQ.
(deftransform delete ((item list &key test) (t list &rest t) *)
  "convert to EQ test"
  (let ((type (lvar-type item)))
    (unless (or (and test (lvar-fun-is test '(eq)))
                (and (eq-comparable-type-p type)
                     (or (not test) (lvar-fun-is test '(eql)))))
      (give-up-ir1-transform)))
  `(delq item list))

(deftransform delete-if ((pred list) (t list))
  "open code"
  '(do ((x list (cdr x))
        (splice '()))
       ((endp x) list)
     (cond ((funcall pred (car x))
            (if (null splice)
                (setq list (cdr x))
                (rplacd splice (cdr x))))
           (t (setq splice x)))))

(deftransform fill ((seq item &key (start 0) (end nil))
                    (list t &key (:start t) (:end t)))
  '(list-fill* seq item start end))

(deftransform fill ((seq item &key (start 0) (end nil))
                    (vector t &key (:start t) (:end t))
                    *
                    :node node)
  (let* ((type (lvar-type seq))
         (element-ctype (array-type-upgraded-element-type type))
         (element-type (type-specifier element-ctype))
         (saetp (unless (eq *wild-type* element-ctype)
                  (find-saetp-by-ctype element-ctype))))
    (cond ((eq *wild-type* element-ctype)
           (delay-ir1-transform node :constraint)
           `(vector-fill* seq item start end))
          ((and saetp (sb!vm::valid-bit-bash-saetp-p saetp))
           (let* ((n-bits (sb!vm:saetp-n-bits saetp))
                  (basher-name (format nil "UB~D-BASH-FILL" n-bits))
                  (basher (or (find-symbol basher-name
                                           (load-time-value
                                            (find-package "SB!KERNEL") t))
                              (abort-ir1-transform
                               "Unknown fill basher, please report to sbcl-devel: ~A"
                               basher-name)))
                  (kind (cond ((sb!vm:saetp-fixnum-p saetp) :tagged)
                              ((member element-type '(character base-char)) :char)
                              ((eq element-type 'single-float) :single-float)
                              #!+64-bit
                              ((eq element-type 'double-float) :double-float)
                              #!+64-bit
                              ((equal element-type '(complex single-float))
                               :complex-single-float)
                              (t
                               (aver (integer-type-p element-ctype))
                               :bits)))
                  ;; BASH-VALUE is a word that we can repeatedly smash
                  ;; on the array: for less-than-word sized elements it
                  ;; contains multiple copies of the fill item.
                  (bash-value
                   (if (constant-lvar-p item)
                       (let ((tmp (lvar-value item)))
                         (unless (ctypep tmp element-ctype)
                           (abort-ir1-transform "~S is not ~S" tmp element-type))
                         (let* ((bits
                                 (ldb (byte n-bits 0)
                                      (ecase kind
                                        (:tagged
                                         (ash tmp sb!vm:n-fixnum-tag-bits))
                                        (:char
                                         (char-code tmp))
                                        (:bits
                                         tmp)
                                        (:single-float
                                         (single-float-bits tmp))
                                        #!+64-bit
                                        (:double-float
                                         (logior (ash (double-float-high-bits tmp) 32)
                                                 (double-float-low-bits tmp)))
                                        #!+64-bit
                                        (:complex-single-float
                                         (logior (ash (single-float-bits (imagpart tmp)) 32)
                                                 (ldb (byte 32 0)
                                                      (single-float-bits (realpart tmp))))))))
                                (res bits))
                           (loop for i of-type sb!vm:word from n-bits by n-bits
                                 until (= i sb!vm:n-word-bits)
                                 do (setf res (ldb (byte sb!vm:n-word-bits 0)
                                                   (logior res (ash bits i)))))
                           res))
                       (progn
                         (delay-ir1-transform node :constraint)
                        `(let* ((bits (ldb (byte ,n-bits 0)
                                           ,(ecase kind
                                                   (:tagged
                                                    `(ash item ,sb!vm:n-fixnum-tag-bits))
                                                   (:char
                                                    `(char-code item))
                                                   (:bits
                                                    `item)
                                                   (:single-float
                                                    `(single-float-bits item))
                                                   #!+64-bit
                                                   (:double-float
                                                    `(logior (ash (double-float-high-bits item) 32)
                                                             (double-float-low-bits item)))
                                                   #!+64-bit
                                                   (:complex-single-float
                                                    `(logior (ash (single-float-bits (imagpart item)) 32)
                                                             (ldb (byte 32 0)
                                                                  (single-float-bits (realpart item))))))))
                                (res bits))
                           (declare (type sb!vm:word res))
                           ,@(unless (= sb!vm:n-word-bits n-bits)
                                     `((loop for i of-type sb!vm:word from ,n-bits by ,n-bits
                                             until (= i sb!vm:n-word-bits)
                                             do (setf res
                                                      (ldb (byte ,sb!vm:n-word-bits 0)
                                                           (logior res (ash bits (truly-the (integer 0 ,(- sb!vm:n-word-bits n-bits)) i))))))))
                           res)))))
             (values
              ;; KLUDGE: WITH-ARRAY data in its full glory is going to mess up
              ;; dynamic-extent for MAKE-ARRAY :INITIAL-ELEMENT initialization.
              (if (csubtypep (lvar-type seq) (specifier-type '(simple-array * (*))))
                  `(let* ((len (length seq))
                          (end (or end len))
                          (bound (1+ end)))
                     ;; Minor abuse CHECK-BOUND for bounds checking.
                     ;; (- END START) may still end up negative, but
                     ;; the basher handle that.
                     (,basher ,bash-value seq
                              (check-bound seq bound start)
                              (- (if end (check-bound seq bound end) len)
                                 start)))
               `(with-array-data ((data seq)
                                  (start start)
                                  (end end)
                                  :check-fill-pointer t)
                  (declare (type (simple-array ,element-type 1) data))
                  (declare (type index start end))
                  (declare (optimize (safety 0) (speed 3)))
                  (,basher ,bash-value data start (- end start))
                  seq))
              `((declare (type ,element-type item))))))
          ((policy node (> speed space))
           (values
            `(with-array-data ((data seq)
                               (start start)
                               (end end)
                               :check-fill-pointer t)
               (declare (type (simple-array ,element-type 1) data))
               (declare (type index start end))
               ;; WITH-ARRAY-DATA did our range checks once and for all, so
               ;; it'd be wasteful to check again on every AREF...
               (declare (optimize (safety 0) (speed 3)))
               (do ((i start (1+ i)))
                   ((= i end) seq)
                 (declare (type index i))
                 (setf (aref data i) item)))
            ;; ... though we still need to check that the new element can fit
            ;; into the vector in safe code. -- CSR, 2002-07-05
            `((declare (type ,element-type item)))))
          ((csubtypep type (specifier-type 'string))
           '(string-fill* seq item start end))
          (t
           '(vector-fill* seq item start end)))))

(deftransform fill ((seq item &key (start 0) (end nil))
                    ((and sequence (not vector) (not list)) t &key (:start t) (:end t)))
  `(sb!sequence:fill seq item
                     :start start
                     :end (%check-generic-sequence-bounds seq start end)))

;;;; hairy sequence transforms

;;; FIXME: no hairy sequence transforms in SBCL?
;;;
;;; There used to be a bunch of commented out code about here,
;;; containing the (apparent) beginning of hairy sequence transform
;;; infrastructure. People interested in implementing better sequence
;;; transforms might want to look at it for inspiration, even though
;;; the actual code is ancient CMUCL -- and hence bitrotted. The code
;;; was deleted in 1.0.7.23.

;;;; string operations

;;; We transform the case-sensitive string predicates into a non-keyword
;;; version. This is an IR1 transform so that we don't have to worry about
;;; changing the order of evaluation.
(macrolet ((def (fun pred*)
             `(deftransform ,fun ((string1 string2 &key (start1 0) end1
                                                         (start2 0) end2)
                                   * *)
                `(,',pred* string1 string2 start1 end1 start2 end2))))
  (def string< string<*)
  (def string> string>*)
  (def string<= string<=*)
  (def string>= string>=*)
  (def string= string=*)
  (def string/= string/=*))

;;; Return a form that tests the free variables STRING1 and STRING2
;;; for the ordering relationship specified by LESSP and EQUALP. The
;;; start and end are also gotten from the environment. Both strings
;;; must be SIMPLE-BASE-STRINGs.
(macrolet ((def (name lessp equalp)
             `(deftransform ,name ((string1 string2 start1 end1 start2 end2)
                                   (simple-base-string simple-base-string t t t t) *)
                `(let* ((end1 (if (not end1) (length string1) end1))
                        (end2 (if (not end2) (length string2) end2))
                        (index (sb!impl::%sp-string-compare
                                string1 start1 end1 string2 start2 end2)))
                  (if index
                      (cond ((= index end1)
                             ,(if ',lessp 'index nil))
                            ((= (+ index (- start2 start1)) end2)
                             ,(if ',lessp nil 'index))
                            ((,(if ',lessp 'char< 'char>)
                               (schar string1 index)
                               (schar string2
                                      (truly-the index
                                                 (+ index
                                                    (truly-the fixnum
                                                               (- start2
                                                                  start1))))))
                             index)
                            (t nil))
                      ,(if ',equalp 'end1 nil))))))
  (def string<* t nil)
  (def string<=* t t)
  (def string>* nil nil)
  (def string>=* nil t))

(deftransform string=* ((string1 string2 start1 end1 start2 end2)
                        (string string
                                (constant-arg (eql 0))
                                (constant-arg null)
                                (constant-arg (eql 0))
                                (constant-arg null)))
  (cond ((and (constant-lvar-p string1)
              (equal (lvar-value string1) ""))
         `(zerop (length string2)))
        ((and (constant-lvar-p string2)
              (equal (lvar-value string2) ""))
         `(zerop (length string1)))
        (t
         (give-up-ir1-transform))))

(deftransform string/=* ((string1 string2 start1 end1 start2 end2)
                         (string string
                                 (constant-arg (eql 0))
                                 (constant-arg null)
                                 (constant-arg (eql 0))
                                 (constant-arg null)))
  (cond ((and (constant-lvar-p string1)
              (equal (lvar-value string1) ""))
         (lvar-type string2)
         `(and (plusp (length string2))
               0))
        ((and (constant-lvar-p string2)
              (equal (lvar-value string2) ""))
         `(and (plusp (length string1))
               0))
        (t
         (give-up-ir1-transform))))

(macrolet ((def (name result-fun)
             `(deftransform ,name ((string1 string2 start1 end1 start2 end2)
                                   (simple-base-string simple-base-string t t t t) *)
                `(,',result-fun
                  (sb!impl::%sp-string-compare
                   string1 start1 (or end1 (length string1))
                   string2 start2 (or end2 (length string2)))))))
  (def string=* not) ; FIXME: this xform looks counterproductive.
  (def string/=* identity))

(deftransform string/=* ((str1 str2 start1 end1 start2 end2) * * :node node
                         :important nil)
  ;; An IF node doesn't care about the mismatch index.
  ;; Transforming to (not (string= ..)) would lead to internal confusion
  ;; due to incorrect typing: STRING/= can't return T, so return 0 for true.
  (if (if-p (node-dest node))
      `(if (string=* str1 str2 start1 end1 start2 end2) nil 0)
      (give-up-ir1-transform)))

(deftransform string ((x) (symbol)) '(symbol-name x))

;;;; transforms for sequence functions

;;; Moved here from generic/vm-tran.lisp to satisfy clisp.  Only applies
;;; to vectors based on simple arrays.
(def!constant vector-data-bit-offset
  (* sb!vm:vector-data-offset sb!vm:n-word-bits))

;;; FIXME: In the copy loops below, we code the loops in a strange
;;; fashion:
;;;
;;; (do ((i (+ src-offset length) (1- i)))
;;;     ((<= i 0) ...)
;;;   (... (aref foo (1- i)) ...))
;;;
;;; rather than the more natural (and seemingly more efficient):
;;;
;;; (do ((i (1- (+ src-offset length)) (1- i)))
;;;     ((< i 0) ...)
;;;   (... (aref foo i) ...))
;;;
;;; (more efficient because we don't have to do the index adjusting on
;;; every iteration of the loop)
;;;
;;; We do this to avoid a suboptimality in SBCL's backend.  In the
;;; latter case, the backend thinks I is a FIXNUM (which it is), but
;;; when used as an array index, the backend thinks I is a
;;; POSITIVE-FIXNUM (which it is).  However, since the backend thinks of
;;; these as distinct storage classes, it cannot coerce a move from a
;;; FIXNUM TN to a POSITIVE-FIXNUM TN.  The practical effect of this
;;; deficiency is that we have two extra moves and increased register
;;; pressure, which can lead to some spectacularly bad register
;;; allocation.  (sub-FIXME: the register allocation even with the
;;; strangely written loops is not always excellent, either...).  Doing
;;; it the first way, above, means that I is always thought of as a
;;; POSITIVE-FIXNUM and there are no issues.
;;;
;;; Besides, the *-WITH-OFFSET machinery will fold those index
;;; adjustments in the first version into the array addressing at no
;;; performance penalty!

;;; This transform is critical to the performance of string streams.  If
;;; you tweak it, make sure that you compare the disassembly, if not the
;;; performance of, the functions implementing string streams
;;; (e.g. SB!IMPL::STRING-OUCH).
(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  (defun !make-replace-transform (saetp sequence-type1 sequence-type2)
    `(deftransform replace ((seq1 seq2 &key (start1 0) (start2 0) end1 end2)
                            (,sequence-type1 ,sequence-type2 &rest t)
                            ,sequence-type1
                            :node node)
       `(let* ((len1 (length seq1))
               (len2 (length seq2))
               (end1 (or end1 len1))
               (end2 (or end2 len2))
               (replace-len (min (- end1 start1) (- end2 start2))))
          ,(unless (policy node (= insert-array-bounds-checks 0))
             `(progn
                (unless (<= 0 start1 end1 len1)
                  (sequence-bounding-indices-bad-error seq1 start1 end1))
                (unless (<= 0 start2 end2 len2)
                  (sequence-bounding-indices-bad-error seq2 start2 end2))))
          ,',(cond
               ((and saetp (sb!vm:valid-bit-bash-saetp-p saetp))
                (let* ((n-element-bits (sb!vm:saetp-n-bits saetp))
                       (bash-function (intern (format nil "UB~D-BASH-COPY"
                                                      n-element-bits)
                                              (find-package "SB!KERNEL"))))
                  `(funcall (function ,bash-function) seq2 start2
                    seq1 start1 replace-len)))
               (t
                `(if (and
                      ;; If the sequence types are different, SEQ1 and
                      ;; SEQ2 must be distinct arrays.
                      ,(eql sequence-type1 sequence-type2)
                      (eq seq1 seq2) (> start1 start2))
                     (do ((i (truly-the index (+ start1 replace-len -1))
                             (1- i))
                          (j (truly-the index (+ start2 replace-len -1))
                             (1- j)))
                         ((< i start1))
                       (declare (optimize (insert-array-bounds-checks 0)))
                       (setf (aref seq1 i) (aref seq2 j)))
                     (do ((i start1 (1+ i))
                          (j start2 (1+ j))
                          (end (+ start1 replace-len)))
                         ((>= i end))
                       (declare (optimize (insert-array-bounds-checks 0)))
                       (setf (aref seq1 i) (aref seq2 j))))))
          seq1))))

(macrolet
    ((define-replace-transforms ()
       (loop for saetp across sb!vm:*specialized-array-element-type-properties*
             for sequence-type = `(simple-array ,(sb!vm:saetp-specifier saetp) (*))
             unless (= (sb!vm:saetp-typecode saetp) sb!vm::simple-array-nil-widetag)
             collect (!make-replace-transform saetp sequence-type sequence-type)
             into forms
             finally (return `(progn ,@forms))))
     (define-one-transform (sequence-type1 sequence-type2)
       (!make-replace-transform nil sequence-type1 sequence-type2)))
  (define-replace-transforms)
  #!+sb-unicode
  (progn
   (define-one-transform (simple-array base-char (*)) (simple-array character (*)))
   (define-one-transform (simple-array character (*)) (simple-array base-char (*)))))

;;; Expand simple cases of UB<SIZE>-BASH-COPY inline.  "simple" is
;;; defined as those cases where we are doing word-aligned copies from
;;; both the source and the destination and we are copying from the same
;;; offset from both the source and the destination.  (The last
;;; condition is there so we can determine the direction to copy at
;;; compile time rather than runtime.  Remember that UB<SIZE>-BASH-COPY
;;; acts like memmove, not memcpy.)  These conditions may seem rather
;;; restrictive, but they do catch common cases, like allocating a (* 2
;;; N)-size buffer and blitting in the old N-size buffer in.

(defun frob-bash-transform (src src-offset
                            dst dst-offset
                            length n-elems-per-word)
  (declare (ignore src dst length))
  (let ((n-bits-per-elem (truncate sb!vm:n-word-bits n-elems-per-word)))
    (multiple-value-bind (src-word src-elt)
        (truncate (lvar-value src-offset) n-elems-per-word)
      (multiple-value-bind (dst-word dst-elt)
          (truncate (lvar-value dst-offset) n-elems-per-word)
        ;; Avoid non-word aligned copies.
        (unless (and (zerop src-elt) (zerop dst-elt))
          (give-up-ir1-transform))
        ;; Avoid copies where we would have to insert code for
        ;; determining the direction of copying.
        (unless (= src-word dst-word)
          (give-up-ir1-transform))
        ;; FIXME: The cross-compiler doesn't optimize TRUNCATE properly,
        ;; so we have to do its work here.
        `(let ((end (+ ,src-word ,(if (= n-elems-per-word 1)
                                      'length
                                      `(truncate (the index length) ,n-elems-per-word)))))
           (declare (type index end))
           ;; Handle any bits at the end.
           (when (logtest length (1- ,n-elems-per-word))
             (let* ((extra (mod length ,n-elems-per-word))
                    ;; FIXME: The shift amount on this ASH is
                    ;; *always* negative, but the backend doesn't
                    ;; have a NEGATIVE-FIXNUM primitive type, so we
                    ;; wind up with a pile of code that tests the
                    ;; sign of the shift count prior to shifting when
                    ;; all we need is a simple negate and shift
                    ;; right.  Yuck.
                    (mask (ash #.(1- (ash 1 sb!vm:n-word-bits))
                               (* (- extra ,n-elems-per-word)
                                  ,n-bits-per-elem))))
               (setf (%vector-raw-bits dst end)
                     (logior
                      (logandc2 (%vector-raw-bits dst end)
                                (ash mask
                                     ,(ecase *backend-byte-order*
                                        (:little-endian 0)
                                        (:big-endian `(* (- ,n-elems-per-word extra)
                                                         ,n-bits-per-elem)))))
                      (logand (%vector-raw-bits src end)
                              (ash mask
                                   ,(ecase *backend-byte-order*
                                      (:little-endian 0)
                                      (:big-endian `(* (- ,n-elems-per-word extra)
                                                       ,n-bits-per-elem)))))))))
           ;; Copy from the end to save a register.
           (do ((i end (1- i)))
               ((<= i ,src-word))
             (setf (%vector-raw-bits dst (1- i))
                   (%vector-raw-bits src (1- i))))
           (values))))))

#.
(let ((arglist '((src src-offset dst dst-offset length)
                 ((simple-unboxed-array (*)) (constant-arg index)
                  (simple-unboxed-array (*)) (constant-arg index)
                  index)
                 *)))
  (loop for i = 1 then (* i 2)
     for name = (intern (format nil "UB~D-BASH-COPY" i) "SB!KERNEL")
     collect `(deftransform ,name ,arglist
                (frob-bash-transform src src-offset
                                     dst dst-offset length
                                     ,(truncate sb!vm:n-word-bits i))) into forms
     until (= i sb!vm:n-word-bits)
     finally (return `(progn ,@forms))))

;;; We expand copy loops inline in SUBSEQ and COPY-SEQ if we're copying
;;; arrays with elements of size >= the word size.  We do this because
;;; we know the arrays cannot alias (one was just consed), therefore we
;;; can determine at compile time the direction to copy, and for
;;; word-sized elements, UB<WORD-SIZE>-BASH-COPY will do a bit of
;;; needless checking to figure out what's going on.  The same
;;; considerations apply if we are copying elements larger than the word
;;; size, with the additional twist that doing it inline is likely to
;;; cons far less than calling REPLACE and letting generic code do the
;;; work.
;;;
;;; However, we do not do this for elements whose size is < than the
;;; word size because we don't want to deal with any alignment issues
;;; inline.  The UB*-BASH-COPY transforms might fix things up later
;;; anyway.

(defun inlineable-copy-vector-p (type)
  (and (array-type-p type)
       ;; The two transforms that use this test already specify that their
       ;; sequence argument is a VECTOR,
       ;; so this seems like it would be more efficient as
       ;;  and (not (array-type-complexp type))
       ;;      (not (eq (array-type-element-type type) *wild-type*))
       ;; Anyway it no longer works to write this as a single specifier
       ;; '(or (simple-unboxed-array (*)) simple-vector) because that
       ;; type is just (simple-array * (*)) which isn't amenable to
       ;; inline copying since we don't know what it holds.
       (or (csubtypep type (specifier-type '(simple-unboxed-array (*))))
           (csubtypep type (specifier-type 'simple-vector)))))

(defun maybe-expand-copy-loop-inline (src src-offset dst dst-offset length
                                      element-type)
  (let ((saetp (find-saetp element-type)))
    (aver saetp)
    (if (>= (sb!vm:saetp-n-bits saetp) sb!vm:n-word-bits)
        (expand-aref-copy-loop src src-offset dst dst-offset length)
        `(locally (declare (optimize (safety 0)))
           (replace ,dst ,src :start1 ,dst-offset :start2 ,src-offset :end1 ,length)))))

(defun expand-aref-copy-loop (src src-offset dst dst-offset length)
  (if (eql src-offset dst-offset)
      `(do ((i (+ ,src-offset ,length) (1- i)))
           ((<= i ,src-offset))
         (declare (optimize (insert-array-bounds-checks 0)))
         (setf (aref ,dst (1- i)) (aref ,src (1- i))))
      ;; KLUDGE: The compiler is not able to derive that (+ offset
      ;; length) must be a fixnum, but arrives at (unsigned-byte 29).
      ;; We, however, know it must be so, as by this point the bounds
      ;; have already been checked.
      `(do ((i (truly-the fixnum (+ ,src-offset ,length)) (1- i))
            (j (+ ,dst-offset ,length) (1- j)))
           ((<= i ,src-offset))
         (declare (optimize (insert-array-bounds-checks 0))
                  (type (integer 0 #.sb!xc:array-dimension-limit) j i))
         (setf (aref ,dst (1- j)) (aref ,src (1- i))))))

;;; MAKE-SEQUENCE, SUBSEQ, COPY-SEQ

(deftransform make-sequence ((result-type size &key initial-element) * *)
  (multiple-value-bind (spec type)
      (and (constant-lvar-p result-type)
           (let ((spec (lvar-value result-type)))
             (values spec (ir1-transform-specifier-type spec))))
    (unless type
      (give-up-ir1-transform))
    (if (type= type (specifier-type 'list))
        `(%make-list size initial-element)
        (multiple-value-bind (elt-type dim complexp)
            (cond ((and (union-type-p type)
                        (csubtypep type (specifier-type 'string)))
                   (let* ((types (union-type-types type))
                          (first (first types)))
                     (when (array-type-p first)
                       (let ((dim (first (array-type-dimensions first)))
                             (complexp (array-type-complexp first)))
                         ;; Require sameness of dim and complexp. Give up on
                         ;;   (OR (VECTOR CHARACTER) (VECTOR BASE-CHAR 2))
                         ;; which eventually fails in the call to the function.
                         (when (every (lambda (x)
                                        (and (array-type-p x)
                                             (eql (first (array-type-dimensions x))
                                                  dim)
                                             (eq (array-type-complexp x) complexp)))
                                      (rest types))
                           (values
                            `(or ,@(mapcar
                                    (lambda (x)
                                      (type-specifier (array-type-element-type x)))
                                    types))
                            dim complexp))))))
                  ((and (array-type-p type)
                        (csubtypep type (specifier-type 'vector)))
                   (when (contains-unknown-type-p (array-type-element-type type))
                     (give-up-ir1-transform "~S is an unknown vector type" spec))
                   (values (let ((et (array-type-element-type type)))
                             ;; VECTOR means (VECTOR T)
                             (if (type= et *wild-type*)
                                 t
                                 (type-specifier et)))
                           (first (array-type-dimensions type))
                           (array-type-complexp type))))
          ;; Don't transform if size is present in the specifier
          ;; and the SIZE argument is not known to be equal.
          (cond ((and (or (eq '* dim)
                          (and dim (constant-lvar-p size) (eql (lvar-value size) dim)))
                      ;; not sure what it would mean to make it non-simple
                      (neq complexp t))
                 `(make-array size :element-type ',elt-type
                              ,@(when initial-element
                                  `(:initial-element initial-element))))
                ;; no transform, but we can detect some style issues
                (t
                 (when dim         ; was a recognizable vector subtype
                   (let* ((elt-ctype (specifier-type elt-type))
                          (saetp (find-saetp-by-ctype elt-ctype)))
                     (cond ((not initial-element)
                            (let ((default-initial-element
                                    (sb!vm:saetp-initial-element-default saetp)))
                              (unless (ctypep default-initial-element elt-ctype)
                                ;; As with MAKE-ARRAY, this is merely undefined
                                ;; behavior, not an error.
                                (compiler-style-warn
                                 "The default initial element ~S is not a ~S."
                                 default-initial-element elt-type))))
                           ;; In would be possible in some cases,
                           ;; like :INITIAL-ELEMENT (IF X #\x #\y) in a call
                           ;; to MAKE-SEQUENCE '(VECTOR (MEMBER #\A #\B))
                           ;; to detect erroneous non-constants initializers,
                           ;; but it is not important enough to bother with.
                           ((and (constant-lvar-p initial-element)
                                 (not (ctypep (lvar-value initial-element)
                                              elt-ctype)))
                            ;; MAKE-ARRAY considers this a warning, not an error.
                            (compiler-warn "~S ~S is not a ~S"
                                           :initial-element
                                           (lvar-value initial-element)
                                           elt-type)))))
                 (give-up-ir1-transform)))))))

(deftransform subseq ((seq start &optional end)
                      (vector t &optional t)
                      *
                      :node node)
  (let ((type (lvar-type seq)))
    (cond
      ((and (inlineable-copy-vector-p type)
            (policy node (> speed space)))
       (let ((element-type (type-specifier (array-type-specialized-element-type type))))
         `(let* ((length (length seq))
                 (end (or end length)))
            ,(unless (policy node (zerop insert-array-bounds-checks))
                     '(progn
                       (unless (<= 0 start end length)
                         (sequence-bounding-indices-bad-error seq start end))))
            (let* ((size (- end start))
                   (result (make-array size :element-type ',element-type)))
              ,(maybe-expand-copy-loop-inline 'seq (if (constant-lvar-p start)
                                                       (lvar-value start)
                                                       'start)
                                              'result 0 'size element-type)
              result))))
      (t
       '(vector-subseq* seq start end)))))

(deftransform subseq ((seq start &optional end)
                      (list t &optional t))
  `(list-subseq* seq start end))

(deftransform subseq ((seq start &optional end)
                      ((and sequence (not vector) (not list)) t &optional t))
  '(sb!sequence:subseq seq start end))

(deftransform copy-seq ((seq) (vector))
  (let ((type (lvar-type seq)))
    (cond ((inlineable-copy-vector-p type)
           (let ((element-type (type-specifier (array-type-specialized-element-type type))))
             `(let* ((length (length seq))
                     (result (make-array length :element-type ',element-type)))
                ,(maybe-expand-copy-loop-inline 'seq 0 'result 0 'length element-type)
                result)))
          (t
           '(vector-subseq* seq 0 nil)))))

(deftransform copy-seq ((seq) (list))
  '(list-copy-seq* seq))

(deftransform copy-seq ((seq) ((and sequence (not vector) (not list))))
  '(sb!sequence:copy-seq seq))

;;; FIXME: it really should be possible to take advantage of the
;;; macros used in code/seq.lisp here to avoid duplication of code,
;;; and enable even funkier transformations.
(deftransform search ((pattern text &key (start1 0) (start2 0) end1 end2
                               (test #'eql)
                               (key #'identity)
                               from-end)
                      (vector vector &rest t)
                      *
                      :node node
                      :policy (> speed (max space safety)))
  "open code"
  (flet ((maybe (x)
           (when (lvar-p x)
             (if (constant-lvar-p x)
                 (when (lvar-value x)
                   :yes)
                 :maybe))))
    (let ((from-end (when (lvar-p from-end)
                     (unless (constant-lvar-p from-end)
                       (give-up-ir1-transform ":FROM-END is not constant."))
                     (lvar-value from-end)))
         (key? (maybe key))
         (test? (maybe test))
         (check-bounds-p (policy node (plusp insert-array-bounds-checks))))
     `(block search
        (flet ((oops (vector start end)
                 (sequence-bounding-indices-bad-error vector start end)))
          (declare (ignorable #'oops))
          (let* ((len1 (length pattern))
                 (len2 (length text))
                 (end1 (or end1 len1))
                 (end2 (or end2 len2))
                 ,@(case key?
                     (:yes `((key (%coerce-callable-to-fun key))))
                     (:maybe `((key (when key
                                      (%coerce-callable-to-fun key))))))
                 ,@(when test?
                     `((test (%coerce-callable-to-fun test)))))
            (declare (type index start1 start2 end1 end2))
            ,@(when check-bounds-p
                `((unless (<= start1 end1 len1)
                    (oops pattern start1 end1))
                  (unless (<= start2 end2 len2)
                    (oops pattern start2 end2))))
            (when (= end1 start1)
              (return-from search (if from-end
                                      end2
                                      start2)))
            (do (,(if from-end
                      '(index2 (- end2 (- end1 start1)) (1- index2))
                      '(index2 start2 (1+ index2))))
                (,(if from-end
                      '(< index2 start2)
                      '(>= index2 end2))
                 nil)
              ;; INDEX2 is FIXNUM, not an INDEX, as right before the loop
              ;; terminates is hits -1 when :FROM-END is true and :START2
              ;; is 0.
              (declare (type fixnum index2))
              (when (do ((index1 start1 (1+ index1))
                         (index2 index2 (1+ index2)))
                        ((>= index1 end1) t)
                      (declare (type index index1 index2)
                               (optimize (insert-array-bounds-checks 0)))
                      ,@(unless from-end
                          '((when (= index2 end2)
                              (return-from search nil))))
                      (unless (,@(if test?
                                     `(funcall test)
                                     `(eql))
                               ,(case key?
                                  (:yes `(funcall key (aref pattern index1)))
                                  (:maybe `(let ((elt (aref pattern index1)))
                                             (if key
                                                 (funcall key elt)
                                                 elt)))
                                  (otherwise `(aref pattern index1)))
                               ,(case key?
                                  (:yes `(funcall key (aref text index2)))
                                  (:maybe `(let ((elt (aref text index2)))
                                             (if key
                                                 (funcall key elt)
                                                 elt)))
                                  (otherwise `(aref text index2))))
                        (return nil)))
                (return index2)))))))))


;;; Open-code CONCATENATE for strings. It would be possible to extend
;;; this transform to non-strings, but I chose to just do the case that
;;; should cover 95% of CONCATENATE performance complaints for now.
;;;   -- JES, 2007-11-17
;;;
;;; Only handle the simple result type cases. If somebody does (CONCATENATE
;;; '(STRING 6) ...) their code won't be optimized, but nobody does that in
;;; practice.
;;;
;;; Limit full open coding based on length of constant sequences. Default
;;; value is chosen so that other parts of the compiler (constraint propagation
;;; mainly) won't go nonlinear too badly. It's not an exact number -- but
;;; in the right ballpark.
(defvar *concatenate-open-code-limit* 129)

(deftransform concatenate ((result-type &rest lvars)
                           ((constant-arg
                             (member string simple-string base-string simple-base-string))
                            &rest sequence)
                           * :node node)
  (let ((vars (make-gensym-list (length lvars)))
        (type (lvar-value result-type)))
    (if (policy node (<= speed space))
        ;; Out-of-line
        `(lambda (.dummy. ,@vars)
           (declare (ignore .dummy.))
           ,(ecase type
              ((string simple-string)
               `(%concatenate-to-string ,@vars))
              ((base-string simple-base-string)
               `(%concatenate-to-base-string ,@vars))))
        ;; Inline
        (let* ((element-type (ecase type
                               ((string simple-string) 'character)
                               ((base-string simple-base-string) 'base-char)))
               (lvar-values (loop for lvar in lvars
                                  collect (when (constant-lvar-p lvar)
                                            (lvar-value lvar))))
               (lengths
                 (loop for value in lvar-values
                       for var in vars
                       collect (if value
                                   (length value)
                                   `(sb!impl::string-dispatch ((simple-array * (*))
                                                               sequence)
                                                              ,var
                                      #-sb-xc-host
                                      (declare (muffle-conditions compiler-note))
                                      (length ,var)))))
               (non-constant-start
                 (loop for value in lvar-values
                       while (and (stringp value)
                                    (< (length value) *concatenate-open-code-limit*))
                       sum (length value))))
          `(lambda (.dummy. ,@vars)
             (declare (ignore .dummy.)
                      (ignorable ,@vars))
             (declare (optimize (insert-array-bounds-checks 0)))
             (let* ((.length. (+ ,@lengths))
                    (.pos. ,non-constant-start)
                    (.string. (make-string .length. :element-type ',element-type)))
               (declare (type index .length. .pos.)
                        #-sb-xc-host (muffle-conditions compiler-note)
                        (ignorable .pos.))
               ,@(loop with constants = -1
                       for first = t then nil
                       for value in lvar-values
                       for var in vars
                       collect
                       (cond ((and (stringp value)
                                   (< (length value) *concatenate-open-code-limit*))
                              ;; Fold the array reads for constant arguments
                              `(progn
                                 ,@(loop for c across value
                                         for i from 0
                                         collect
                                         ;; Without truly-the we get massive numbers
                                         ;; of pointless error traps.
                                         `(setf (aref .string.
                                                      (truly-the index ,(if constants
                                                                            (incf constants)
                                                                            `(+ .pos. ,i))))
                                                ,c))
                                 ,(unless constants
                                    `(incf (truly-the index .pos.) ,(length value)))))
                             (t
                              (prog1
                                  `(sb!impl::string-dispatch
                                       (#!+sb-unicode
                                        (simple-array character (*))
                                        (simple-array base-char (*))
                                        t)
                                       ,var
                                     (replace .string. ,var
                                              ,@(cond ((not constants)
                                                       '(:start1 .pos.))
                                                      ((plusp non-constant-start)
                                                       `(:start1 ,non-constant-start))))
                                     (incf (truly-the index .pos.) (length ,var)))
                                (setf constants nil)))))
               .string.))))))

;;;; CONS accessor DERIVE-TYPE optimizers

(defoptimizer (car derive-type) ((cons))
  ;; This and CDR needs to use LVAR-CONSERVATIVE-TYPE because type inference
  ;; gets confused by things like (SETF CAR).
  (let ((type (lvar-conservative-type cons))
        (null-type (specifier-type 'null)))
    (cond ((eq type null-type)
           null-type)
          ((cons-type-p type)
           (cons-type-car-type type)))))

(defoptimizer (cdr derive-type) ((cons))
  (let ((type (lvar-conservative-type cons))
        (null-type (specifier-type 'null)))
    (cond ((eq type null-type)
           null-type)
          ((cons-type-p type)
           (cons-type-cdr-type type)))))

;;;; FIND, POSITION, and their -IF and -IF-NOT variants

;;; We want to make sure that %FIND-POSITION is inline-expanded into
;;; %FIND-POSITION-IF only when %FIND-POSITION-IF has an inline
;;; expansion, so we factor out the condition into this function.
(defun check-inlineability-of-find-position-if (sequence from-end)
  (let ((ctype (lvar-type sequence)))
    (cond ((csubtypep ctype (specifier-type 'vector))
           ;; It's not worth trying to inline vector code unless we
           ;; know a fair amount about it at compile time.
           (upgraded-element-type-specifier-or-give-up sequence)
           (unless (constant-lvar-p from-end)
             (give-up-ir1-transform
              "FROM-END argument value not known at compile time")))
          ((csubtypep ctype (specifier-type 'list))
           ;; Inlining on lists is generally worthwhile.
           )
          (t
           (give-up-ir1-transform
            "sequence type not known at compile time")))))

;;; %FIND-POSITION-IF and %FIND-POSITION-IF-NOT for LIST data
(defun %find/position-if-list-expansion (sense from-end start end node)
  (declare (ignore from-end))
  ;; Circularity detection slows things down. It is permissible not to.
  ;; In fact, FIND is given as an archetypal example of a function that
  ;; "should be prepared to signal an error" but might not [CLHS 1.4.2].
  ;; We relax the definition of "safe" from safety=3 to >=2.
  (let ((safe (policy node (>= safety 2)))
        ;; The secondary value is inconsequential when flowing into a non-MV
        ;; combination, so we avoid counting loop iterations if possible.
        ;; This is limited in power, but good enough, for want of a proper
        ;; dead-code-elimination phase of the compiler.
        (indexed
         (not (and (lvar-single-value-p (node-lvar node))
                   (constant-lvar-p start)
                   (eql (lvar-value start) 0)
                   (constant-lvar-p end)
                   (null (lvar-value end))))))
    `(let ((find nil)
           (position nil))
       (flet ((bounds-error ()
                (sequence-bounding-indices-bad-error sequence start end)))
         (if (and end (> start end))
             (bounds-error)
           (do ((slow sequence (cdr slow))
                ,@(when safe '((fast (cdr sequence) (cddr fast))))
                ,@(when indexed '((index 0 (+ index 1)))))
               ((cond ((null slow)
                       (,@(if indexed
                              '(if (and end (> end index)) (bounds-error))
                              '(progn))
                        (return (values find position))))
                      ,@(when indexed
                          '(((and end (>= index end))
                             (return (values find position)))))
                      ,@(when safe
                          '(((eq slow fast)
                             (circular-list-error sequence)))))
                (bug "never"))
             (declare (list slow ,@(and safe '(fast)))
                      ;; If you have as many as INDEX conses on a 32-bit build,
                      ;; then you've either used up 4GB of memory (impossible)
                      ;; or you're stuck in a circular list in unsafe code.
                      ;; Correspondingly larger limit for 64-bit.
                      ,@(and indexed '((index index))))
             (,@(if indexed '(when (>= index start)) '(progn))
               (let ((element (car slow)))
                 ;; This hack of dealing with non-NIL FROM-END for list data
                 ;; by iterating forward through the list and keeping track of
                 ;; the last time we found a match might be more screwy than
                 ;; what the user expects, but it seems to be allowed by the
                 ;; ANSI standard. (And if the user is screwy enough to ask
                 ;; for FROM-END behavior on list data, turnabout is fair play.)
                 ;;
                 ;; It's also not enormously efficient, calling PREDICATE
                 ;; and KEY more often than necessary; but all the alternatives
                 ;; seem to have their own efficiency problems.
                 (,sense (funcall predicate (funcall key element))
                   (if from-end
                       (setf find element position ,(and indexed 'index))
                       (return (values element ,(and indexed 'index)))))))))))))

(macrolet ((def (name condition)
             `(deftransform ,name ((predicate sequence from-end start end key)
                                   (function list t t t function)
                                   *
                                   :node node
                                   :policy (> speed space))
                "expand inline"
                (%find/position-if-list-expansion ',condition
                                                  from-end start end node))))
  (def %find-position-if when)
  (def %find-position-if-not unless))

;;; %FIND-POSITION for LIST data can be expanded into %FIND-POSITION-IF
;;; without loss of efficiency. (I.e., the optimizer should be able
;;; to straighten everything out.)
(deftransform %find-position ((item sequence from-end start end key test)
                              (t list t t t t t)
                              *
                              :policy (> speed space))
  "expand inline"
  '(%find-position-if (let ((test-fun (%coerce-callable-to-fun test)))
                        ;; The order of arguments for asymmetric tests
                        ;; (e.g. #'<, as opposed to order-independent
                        ;; tests like #'=) is specified in the spec
                        ;; section 17.2.1 -- the O/Zi stuff there.
                        (lambda (i)
                          (funcall test-fun item i)))
                      sequence
                      from-end
                      start
                      end
                      (%coerce-callable-to-fun key)))

;;; The inline expansions for the VECTOR case are saved as macros so
;;; that we can share them between the DEFTRANSFORMs and the default
;;; cases in the DEFUNs. (This isn't needed for the LIST case, because
;;; the DEFTRANSFORMs for LIST are less choosy about when to expand.)
(defun %find-position-or-find-position-if-vector-expansion (sequence-arg
                                                            from-end
                                                            start
                                                            end-arg
                                                            element
                                                            done-p-expr)
  (with-unique-names (offset block index n-sequence sequence end)
    `(let* ((,n-sequence ,sequence-arg))
       (with-array-data ((,sequence ,n-sequence :offset-var ,offset)
                         (,start ,start)
                         (,end ,end-arg)
                         :check-fill-pointer t)
         (block ,block
           (macrolet ((maybe-return ()
                        ;; WITH-ARRAY-DATA has already performed bounds
                        ;; checking, so we can safely elide the checks
                        ;; in the inner loop.
                        '(let ((,element (locally (declare (optimize (insert-array-bounds-checks 0)))
                                           (aref ,sequence ,index))))
                          (when ,done-p-expr
                            (return-from ,block
                              (values ,element
                                      (- ,index ,offset)))))))
             (if ,from-end
                 (loop for ,index
                       ;; (If we aren't fastidious about declaring that
                       ;; INDEX might be -1, then (FIND 1 #() :FROM-END T)
                       ;; can send us off into never-never land, since
                       ;; INDEX is initialized to -1.)
                       of-type index-or-minus-1
                       from (1- ,end) downto ,start do
                       (maybe-return))
                 (loop for ,index of-type index from ,start below ,end do
                          (maybe-return))))
           (values nil nil))))))

(def!macro %find-position-vector-macro (item sequence
                                             from-end start end key test)
  (with-unique-names (element)
    (%find-position-or-find-position-if-vector-expansion
     sequence
     from-end
     start
     end
     element
     ;; (See the LIST transform for a discussion of the correct
     ;; argument order, i.e. whether the searched-for ,ITEM goes before
     ;; or after the checked sequence element.)
     `(funcall ,test ,item (funcall ,key ,element)))))

(def!macro %find-position-if-vector-macro (predicate sequence
                                                     from-end start end key)
  (with-unique-names (element)
    (%find-position-or-find-position-if-vector-expansion
     sequence
     from-end
     start
     end
     element
     `(funcall ,predicate (funcall ,key ,element)))))

(def!macro %find-position-if-not-vector-macro (predicate sequence
                                                         from-end start end key)
  (with-unique-names (element)
    (%find-position-or-find-position-if-vector-expansion
     sequence
     from-end
     start
     end
     element
     `(not (funcall ,predicate (funcall ,key ,element))))))

;;; %FIND-POSITION, %FIND-POSITION-IF and %FIND-POSITION-IF-NOT for
;;; VECTOR data
(deftransform %find-position-if ((predicate sequence from-end start end key)
                                 (function vector t t t function)
                                 *
                                 :policy (> speed space))
  "expand inline"
  (check-inlineability-of-find-position-if sequence from-end)
  '(%find-position-if-vector-macro predicate sequence
                                   from-end start end key))

(deftransform %find-position-if-not ((predicate sequence from-end start end key)
                                     (function vector t t t function)
                                     *
                                     :policy (> speed space))
  "expand inline"
  (check-inlineability-of-find-position-if sequence from-end)
  '(%find-position-if-not-vector-macro predicate sequence
                                       from-end start end key))

(deftransform %find-position ((item sequence from-end start end key test)
                              (t vector t t t function function)
                              *
                              :policy (> speed space))
  "expand inline"
  (check-inlineability-of-find-position-if sequence from-end)
  '(%find-position-vector-macro item sequence
    from-end start end key test))

(deftransform %find-position ((item sequence from-end start end key test)
                              (t bit-vector t t t t t)
                              * :node node)
  (when (and test (lvar-fun-is test '(eq eql equal)))
    (setf test nil))
  (when (and key (lvar-fun-is key '(identity)))
    (setf key nil))
  (when (or test key)
    (delay-ir1-transform node :optimize)
    (give-up-ir1-transform "non-trivial :KEY or :TEST"))
  (catch 'not-a-bit
    `(with-array-data ((bits sequence :offset-var offset)
                       (start start)
                       (end end)
                       :check-fill-pointer t)
       (let ((p ,(if (constant-lvar-p item)
                     (case (lvar-value item)
                       (0 `(%bit-position/0 bits from-end start end))
                       (1 `(%bit-position/1 bits from-end start end))
                       (otherwise (throw 'not-a-bit `(values nil nil))))
                     `(%bit-position item bits from-end start end))))
         (if p
             (values item (the index (- (truly-the index p) offset)))
             (values nil nil))))))

(deftransform %find-position ((item sequence from-end start end key test)
                              (character string t t t function function)
                              *
                              :policy (> speed space))
  (if (eq '* (upgraded-element-type-specifier sequence))
      (let ((form
             `(sb!impl::string-dispatch ((simple-array character (*))
                                         (simple-array base-char (*))
                                         (simple-array nil (*)))
                  sequence
                (%find-position item sequence from-end start end key test))))
        (if (csubtypep (lvar-type sequence) (specifier-type 'simple-string))
            form
            ;; Otherwise we'd get three instances of WITH-ARRAY-DATA from
            ;; %FIND-POSITION.
            `(with-array-data ((sequence sequence :offset-var offset)
                               (start start)
                               (end end)
                               :check-fill-pointer t)
               (multiple-value-bind (elt index) ,form
                 (values elt (when (fixnump index) (- index offset)))))))
      ;; The type is known exactly, other transforms will take care of it.
      (give-up-ir1-transform)))

;;; logic to unravel :TEST, :TEST-NOT, and :KEY options in FIND,
;;; POSITION-IF, etc.
(define-source-transform effective-find-position-test (test test-not)
  (once-only ((test test)
              (test-not test-not))
    `(cond
      ((and ,test ,test-not)
       (error "can't specify both :TEST and :TEST-NOT"))
      (,test (%coerce-callable-to-fun ,test))
      (,test-not
       ;; (Without DYNAMIC-EXTENT, this is potentially horribly
       ;; inefficient, but since the TEST-NOT option is deprecated
       ;; anyway, we don't care.)
       (complement (%coerce-callable-to-fun ,test-not)))
      (t #'eql))))
(define-source-transform effective-find-position-key (key)
  (once-only ((key key))
    `(if ,key
         (%coerce-callable-to-fun ,key)
         #'identity)))

(macrolet ((define-find-position (fun-name values-index)
             `(deftransform ,fun-name ((item sequence &key
                                             from-end (start 0) end
                                             key test test-not)
                                       (t (or list vector) &rest t))
                '(nth-value ,values-index
                            (%find-position item sequence
                                            from-end start
                                            end
                                            (effective-find-position-key key)
                                            (effective-find-position-test
                                             test test-not))))))
  (define-find-position find 0)
  (define-find-position position 1))

(macrolet ((define-find-position-if (fun-name values-index)
             `(deftransform ,fun-name ((predicate sequence &key
                                                  from-end (start 0)
                                                  end key)
                                       (t (or list vector) &rest t))
                '(nth-value
                  ,values-index
                  (%find-position-if (%coerce-callable-to-fun predicate)
                                     sequence from-end
                                     start end
                                     (effective-find-position-key key))))))
  (define-find-position-if find-if 0)
  (define-find-position-if position-if 1))

;;; the deprecated functions FIND-IF-NOT and POSITION-IF-NOT. We
;;; didn't bother to worry about optimizing them, except note that on
;;; Sat, Oct 06, 2001 at 04:22:38PM +0100, Christophe Rhodes wrote on
;;; sbcl-devel
;;;
;;;     My understanding is that while the :test-not argument is
;;;     deprecated in favour of :test (complement #'foo) because of
;;;     semantic difficulties (what happens if both :test and :test-not
;;;     are supplied, etc) the -if-not variants, while officially
;;;     deprecated, would be undeprecated were X3J13 actually to produce
;;;     a revised standard, as there are perfectly legitimate idiomatic
;;;     reasons for allowing the -if-not versions equal status,
;;;     particularly remove-if-not (== filter).
;;;
;;;     This is only an informal understanding, I grant you, but
;;;     perhaps it's worth optimizing the -if-not versions in the same
;;;     way as the others?
;;;
;;; FIXME: Maybe remove uses of these deprecated functions within the
;;; implementation of SBCL.
(macrolet ((define-find-position-if-not (fun-name values-index)
               `(deftransform ,fun-name ((predicate sequence &key
                                          from-end (start 0)
                                          end key)
                                         (t (or list vector) &rest t))
                 '(nth-value
                   ,values-index
                   (%find-position-if-not (%coerce-callable-to-fun predicate)
                    sequence from-end
                    start end
                    (effective-find-position-key key))))))
  (define-find-position-if-not find-if-not 0)
  (define-find-position-if-not position-if-not 1))

(macrolet ((define-trimmer-transform (fun-name leftp rightp)
             `(deftransform ,fun-name ((char-bag string)
                                       (t simple-string))
                (let ((find-expr
                       (if (constant-lvar-p char-bag)
                           ;; If the bag is constant, use MEMBER
                           ;; instead of FIND, since we have a
                           ;; deftransform for MEMBER that can
                           ;; open-code all of the comparisons when
                           ;; the list is constant. -- JES, 2007-12-10
                           `(not (member (schar string index)
                                         ',(coerce (lvar-value char-bag) 'list)
                                         :test #'char=))
                           '(not (find (schar string index) char-bag :test #'char=)))))
                  `(flet ((char-not-in-bag (index)
                            ,find-expr))
                     (let* ((end (length string))
                            (left-end (if ,',leftp
                                          (do ((index 0 (1+ index)))
                                              ((or (= index (the fixnum end))
                                                   (char-not-in-bag index))
                                               index)
                                            (declare (fixnum index)))
                                          0))
                            (right-end (if ,',rightp
                                           (do ((index (1- end) (1- index)))
                                               ((or (< index left-end)
                                                    (char-not-in-bag index))
                                                (1+ index))
                                             (declare (fixnum index)))
                                           end)))
                       (if (and (eql left-end 0)
                                (eql right-end (length string)))
                           string
                           (subseq string left-end right-end))))))))
  (define-trimmer-transform string-left-trim t nil)
  (define-trimmer-transform string-right-trim nil t)
  (define-trimmer-transform string-trim t t))


;;; (partially) constant-fold backq-* functions, or convert to their
;;; plain CL equivalent (now that they're not needed for pprinting).

;; Pop constant values from the end, list/list* them if any, and link
;; the remainder with list* at runtime.
(defun transform-backq-list-or-list* (function values)
  (let ((gensyms (make-gensym-list (length values)))
        (reverse (reverse values))
        (constants '()))
    (loop while (and reverse
                     (constant-lvar-p (car reverse)))
          do (push (lvar-value (pop reverse))
                   constants))
    (if (null constants)
        `(lambda ,gensyms
           (,function ,@gensyms))
        (let ((tail (apply function constants)))
          (if (null reverse)
              `',tail
              (let* ((nvariants (length reverse))
                     (variants (subseq gensyms 0 nvariants)))
                `(lambda ,gensyms
                   (declare (ignore ,@(subseq gensyms nvariants)))
                   ,(if tail
                        `(list* ,@variants ',tail)
                        `(list ,@variants)))))))))

(deftransform sb!impl::|List| ((&rest elts))
  (transform-backq-list-or-list* 'list elts))

(deftransform sb!impl::|List*| ((&rest elts))
  (transform-backq-list-or-list* 'list* elts))

;; Merge adjacent constant values
(deftransform sb!impl::|Append| ((&rest elts))
  (let ((gensyms (make-gensym-list (length elts)))
        (acc nil)
        (ignored '())
        (arguments '()))
    (flet ((convert-accumulator ()
             (let ((constant (apply 'append (nreverse (shiftf acc nil)))))
               (when constant
                 (push `',constant arguments)))))
      (loop for gensym in gensyms
            for (elt . next) on elts by #'cdr
            do (cond ((constant-lvar-p elt)
                      (let ((elt (lvar-value elt)))
                        (when (and next (not (proper-list-p elt)))
                          (abort-ir1-transform
                           "Non-list or improper list spliced in ~
                            the middle of a backquoted list."))
                        (push gensym ignored)
                        (push elt acc)))
                     (t
                      (convert-accumulator)
                      (push gensym arguments)))
            finally (convert-accumulator)))
    (let ((arguments (nreverse arguments)))
      `(lambda ,gensyms
         (declare (ignore ,@ignored))
         (append ,@arguments)))))
