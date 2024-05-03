;;;; optimizers for list and sequence functions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

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
          (do-clauses `(,v (the* (list :use-annotations t :source-form ,a) ,a)
                           (cdr ,v)))
          (tests `(endp ,v))
          (args-to-fn (if take-car `(car ,v) v))))

      (binding* (((fn-binding call) (funarg-bind/call-forms fn (args-to-fn)))
                 (endtest `(or ,@(tests))))
        `(let ,fn-binding
           ,(ecase accumulate
             (:nconc
              (let ((last (gensym "LAST"))
                    (map-result (gensym)))
                `(let ((,map-result
                         ;; MUFFLE- is not injected when cross-compiling.
                         ;; See top of file for explanation.
                         (locally
                             #-sb-xc-host
                           (declare (muffle-conditions compiler-note))
                           (list nil))))
                   (declare (dynamic-extent ,map-result))
                   (do-anonymous ((,last ,map-result) . ,(do-clauses))
                     (,endtest (cdr ,map-result))
                     (let ((result ,call))
                       (when result
                         (psetf ,last result
                                (cdr (last ,last)) result)))))))
             (:list
              (let ((temp (gensym))
                    (map-result (gensym)))
                `(let ((,map-result
                        ;; MUFFLE- is not injected when cross-compiling.
                        ;; See top of file for explanation.
                        (locally
                            #-sb-xc-host
                            (declare (muffle-conditions compiler-note))
                            (unaligned-dx-cons nil))))
                   (declare (dynamic-extent ,map-result))
                   (do-anonymous ((,temp ,map-result) . ,(do-clauses))
                     (,endtest
                      (%rplacd ,temp nil) ;; replace the 0
                      (truly-the list (cdr ,map-result)))
                     ;; Accumulate using %RPLACD. RPLACD becomes (SETF CDR)
                     ;; which becomes %RPLACD but relies on "defsetfs".
                     ;; This is for effect, not value, so makes no difference.
                     (%rplacd ,temp (setq ,temp
                                          ;; 0 is not written to the heap
                                          (cons ,call 0)))))))
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
  (let* ((seq-names (make-gensym-list (length seqs)))
         (constant-result-type-arg-p (constant-lvar-p result-type-arg))
         (nil-p)
         ;; what we know about the type of the result. (Note that the
         ;; "result type" argument is not necessarily the type of the
         ;; result, since NIL means the result has NULL type.)
         (result-type (if constant-result-type-arg-p
                          (let ((result-type-arg-value
                                  (lvar-value result-type-arg)))
                            (cond (result-type-arg-value)
                                  (t
                                   (setf nil-p t)
                                   'null)))
                          'consed-sequence))
         (result-ctype (ir1-transform-specifier-type result-type)))
    `(lambda (result-type-arg fun seq ,@seq-names)
       (the* (,result-type :context map)
             (truly-the
              ,(cond (nil-p
                      'null)
                     ((csubtypep result-ctype (specifier-type 'vector))
                      (strip-array-dimensions-and-complexity result-ctype t))
                     ((csubtypep result-ctype (specifier-type 'list))
                      'list)
                     (t
                      t))
              (%map result-type-arg fun seq ,@seq-names))))))

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
           (multiple-value-bind (subtype-p valid-p)
               (csubtypep x (specifier-type y))
             (if valid-p
                 subtype-p
                 (give-up-ir1-transform
                  "can't analyze sequence type relationship")))))
    (let* ((result-type-value (lvar-value result-type))
           (result-type-ctype (ir1-transform-specifier-type result-type-value))
           (result-supertype (cond ((null result-type-value) 'null)
                                   ((1subtypep result-type-ctype 'vector)
                                    'vector)
                                   ((1subtypep result-type-ctype 'list)
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
                  (map-into (locally
                                #-sb-xc-host
                                (declare (muffle-conditions array-initial-element-mismatch))
                              (make-sequence result-type
                                             ,(if (cdr seq-args)
                                                  `(min ,@(loop for arg in seq-args
                                                                collect `(length ,arg)))
                                                  `(length ,(car seq-args)))))
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
(defmacro mapper-from-typecode (typecode)
  #+sb-xc-host
  `(svref ,(let ((a (make-array 256)))
             (dovector (info sb-vm:*specialized-array-element-type-properties* a)
               (setf (aref a (sb-vm:saetp-typecode info))
                     (package-symbolicate "SB-IMPL" "VECTOR-MAP-INTO/"
                                          (sb-vm:saetp-primitive-type-name info)))))
          ,typecode)
  #-sb-xc-host
  `(%fun-name (svref sb-impl::%%vector-map-into-funs%% ,typecode)))

(deftransform map-into ((result fun &rest seqs)
                        (vector t &rest t)
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
      (cond ((and non-complex-vector-type-p
                  (array-type-p result-type)
                  (not (eq (array-type-specialized-element-type result-type)
                           *wild-type*)))
             (let ((saetp (find-saetp-by-ctype (array-type-specialized-element-type result-type))))
               (unless saetp
                 (give-up-ir1-transform "Uknown upgraded array element type of the result"))
               `(lambda (result fun ,@seqs-names)
                  (,(mapper-from-typecode (sb-vm:saetp-typecode saetp))
                   result 0 (length result) (%coerce-callable-to-fun fun) ,@seqs-names)
                  result)))
            (t
             (%give-up))))))

(deftransform map-into ((result fun &rest sequences)
                        (list &rest t)
                        * :policy (>= speed space))
  (if sequences
      (let ((seqs-names (make-gensym-list (length sequences))))
        `(lambda (result fun ,@seqs-names)
           (let ((node result))
             (block nil
               (%map nil (lambda (,@seqs-names)
                           (when (endp node)
                             (return))
                           (setf (car node) (funcall fun ,@seqs-names))
                           (setf node (cdr node)))
                     ,@seqs-names))
             result)))
      `(let ((node result))
         (loop (when (endp node)
                 (return))
               (setf (car node) (funcall fun))
               (setf node (cdr node)))
         result)))


;;; FIXME: once the confusion over doing transforms with known-complex
;;; arrays is over, we should also transform the calls to (AND (ARRAY
;;; * (*)) (NOT (SIMPLE-ARRAY * (*)))) objects.
(deftransform elt ((s i) ((simple-array * (*)) t) *)
  '(aref s i))

(deftransform elt ((s i) (list t) * :policy (< safety 3))
  '(nth i s))

(deftransform %setelt ((s i v) ((simple-array * (*)) t t) *)
  '(setf (aref s i) v))

(deftransform %setelt ((s i v) (list t t) * :policy (< safety 3))
  '(setf (car (nthcdr i s)) v))

(deftransform %check-vector-sequence-bounds ((vector start end)
                                             (vector t t) *
                                             :node node)
  (if (policy node (= 0 insert-array-bounds-checks))
      '(or end (length vector))
      '(let ((length (length vector)))
         (if (<= 0 start (or end length) length)
             (or end length)
             (sequence-bounding-indices-bad-error vector start end)))))

(sb-xc:deftype eq-comparable-type ()
  '(or fixnum #+64-bit single-float (not number)))

;;; True if EQL comparisons involving type can be simplified to EQ.
(defun eq-comparable-type-p (type)
  (csubtypep type (specifier-type 'eq-comparable-type)))

(defun specialized-list-seek-function-name (function-name key-functions &optional variant)
  (or (find-symbol (%with-output-to-string (s)
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
                   #.(find-package "SB-KERNEL"))
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

  (when (and (member name '(member assoc rassoc))
             ;; If the test was EQL, we've already changed it to NIL.
             (or (not test) (lvar-fun-is test '(eq)))
             (not test-not) ; keep it simple, no other keywords allowed
             (not key)
             (constant-lvar-p list))
    (let ((items (lvar-value list)))
      ;; spec says MEMBER "Should be prepared to signal an error of type type-error
      ;; if list is not a proper list." This optimization can't do that.
      ;; TRY-mumble will figure out based on what function it is trying to transform
      ;; whether all keys are acceptable.
      (when (and (slow-findhash-allowed node) (proper-list-p items))
        (let* ((conditional (if-p (node-dest node)))
               (expr (try-perfect-find/position-map
                      name
                      (if conditional ''(t)) ; returned value if present in the mapping
                      (lvar-type item) items nil node)))
          (when expr
            ;; The value delivered to an IF node must be a list because MEMBER and MEMQ
            ;; are declared in fndb to return a list. If it were just the symbol T,
            ;; then type inference would get all whacky on you.
            (when conditional
              (derive-node-type node (specifier-type 'list) :from-scratch t)) ; erase any cons types
            (return-from transform-list-item-seek expr))))))

  ;; Key can legally be NIL, but if it's NIL for sure we pretend it's
  ;; not there at all. If it might be NIL, make up a form to that
  ;; ensures it is a function.
  (multiple-value-bind (key key-form)
      (when key
        (let ((key-type (lvar-type key))
              (null-type (specifier-type 'null)))
          (cond ((csubtypep key-type null-type)
                 (values nil nil))
                ((types-equal-or-intersect null-type key-type)
                 (values key '(if key
                               (%coerce-callable-to-fun key)
                               #'identity)))
                (t
                 (values key (ensure-lvar-fun-form key 'key))))))
    (let* ((c-test (cond ((and test (lvar-fun-is test '(eq)))
                          (setf test nil)
                          'eq)
                         ((and (not test) (not test-not))
                          (when (cond ((or (neq name 'adjoin)
                                           (not key))
                                       (eq-comparable-type-p (lvar-type item)))
                                      (t
                                       (let ((type (lvar-fun-type key t t)))
                                         (when (fun-type-p type)
                                           (eq-comparable-type-p
                                            (single-value-type (fun-type-returns type)))))))
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
          (cond ((not (proper-list-p c-list))
                 (abort-ir1-transform "Argument to ~a is not a proper list." name))
                ((and cp c-list (member name '(assoc rassoc member))
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
                ((types-equal-or-intersect null-type key-type)
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
          (cond ((and cp c-list
                      (proper-list-p c-list)
                      (policy node (>= speed space))
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

(defun change-test-based-on-item (test item)
  (or
   (and (neq item *universal-type*)
        (neq item *wild-type*)
        (case test
          (eql
           (when (csubtypep item (specifier-type 'eq-comparable-type))
             'eq))
          (equal
           (when (csubtypep item (specifier-type '(not (or
                                                        cons
                                                        bit-vector
                                                        string
                                                        pathname))))
             (change-test-based-on-item 'eql item)))
          (equalp
           (cond ((csubtypep item (specifier-type '(not (or number
                                                         character
                                                         cons
                                                         array
                                                         pathname
                                                         instance
                                                         hash-table))))
                  (change-test-based-on-item 'eql item))
                 ((multiple-value-bind (p value) (type-singleton-p item)
                    (when (and p
                               (characterp value)
                               (not (both-case-p value)))
                      (change-test-based-on-item 'eq item))))))
          (char-equal
           (multiple-value-bind (p value) (type-singleton-p item)
             (when (and p
                        (characterp value)
                        (not (both-case-p value)))
               'char=)))))
   test))

(defun change-test-lvar-based-on-item (test item)
  (let ((test (if test
                  (lvar-fun-is test '(eql equal equalp char-equal))
                  'eql)))
    (when test
      (unless (eq (shiftf test (change-test-based-on-item test (lvar-type item)))
                  test)
        test))))

(macrolet ((def (name &optional if/if-not)
             (let ((basic (symbolicate "%" name))
                   (basic-eq (symbolicate "%" name "-EQ"))
                   (basic-key (symbolicate "%" name "-KEY"))
                   (basic-key-eq (symbolicate "%" name "-KEY-EQ"))
                   (test (symbolicate "%" name "-TEST"))
                   (key-test (symbolicate "%" name "-KEY-TEST")))
               `(progn
                  (deftransform ,name ((item list &key key test test-not) * * :node node)
                    (transform-list-item-seek ',name item list key test test-not node))
                  (deftransform ,basic ((item list) (eq-comparable-type t) * :important nil)
                    `(,',basic-eq item list))
                  ,(unless (eq name 'adjoin) ;; applies KEY to ITEM.
                     `(deftransform ,basic-key ((item list key) (eq-comparable-type t t) * :important nil)
                        `(,',basic-key-eq item list key)))
                  (deftransform ,test ((item list test) (t t t) * :node node)
                    (let ((test (lvar-fun-is test '(eq eql equal equalp char-equal))))
                      (case (change-test-based-on-item test (lvar-type item))
                        (eq
                         `(,',basic-eq item list))
                        (eql
                         `(,',basic item list))
                        (t
                         (give-up-ir1-transform)))))
                  (deftransform ,key-test ((item list key test) (t t t t) * :important nil)
                    (let ((test (lvar-fun-is test '(eq eql
                                                    ,@(unless (eq name 'adjoin)
                                                        '(equal equalp char-equal))))))
                      (case ,(if (eq name 'adjoin)
                                 'test
                                 '(change-test-based-on-item test (lvar-type item)))
                        (eq
                         `(,',basic-key-eq item list key))
                        (eql
                         `(,',basic-key item list key))
                        (t
                         (give-up-ir1-transform)))))
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

(defun find-basher (saetp &optional item node)
  (let* ((element-type (sb-vm:saetp-specifier saetp))
         (element-ctype (sb-vm:saetp-ctype saetp))
         (n-bits (sb-vm:saetp-n-bits saetp))
         (kind (cond ((sb-vm:saetp-fixnum-p saetp) :tagged)
                     ((member element-type '(character base-char)) :char)
                     ((eq element-type 'single-float) :single-float)
                     #+64-bit
                     ((eq element-type 'double-float) :double-float)
                     #+64-bit
                     ((equal element-type '(complex single-float))
                      :complex-single-float)
                     (t
                      (aver (integer-type-p element-ctype))
                      :bits))))
    (if (and item (constant-lvar-p item))
        (let* ((basher-name (format nil "UB~D-BASH-FILL" n-bits))
               (basher (or (find-symbol basher-name #.(find-package "SB-KERNEL"))
                           (abort-ir1-transform
                            "Unknown fill basher, please report to sbcl-devel: ~A"
                            basher-name)))
               (tmp (lvar-value item)))
          (unless (ctypep tmp element-ctype)
            (abort-ir1-transform "~S is not ~S" tmp element-type))
          (values
           basher
           ;; Construct a word that we can repeatedly smash
           ;; on the array: for less-than-word sized elements it
           ;; contains multiple copies of the fill item.
           (let* ((bits
                    (ldb (byte n-bits 0)
                         (ecase kind
                           (:tagged
                            (ash tmp sb-vm:n-fixnum-tag-bits))
                           (:char
                            (char-code tmp))
                           (:bits
                            tmp)
                           (:single-float
                            (single-float-bits tmp))
                           #+64-bit
                           (:double-float
                            (double-float-bits tmp))
                           #+64-bit
                           (:complex-single-float
                            #+big-endian
                            (logior (ash (single-float-bits (realpart tmp)) 32)
                                    (ldb (byte 32 0)
                                         (single-float-bits (imagpart tmp))))
                            #+little-endian
                            (logior (ash (single-float-bits (imagpart tmp)) 32)
                                    (ldb (byte 32 0)
                                         (single-float-bits (realpart tmp))))))))
                  (res bits))
             (loop for i of-type sb-vm:word from n-bits by n-bits
                   until (= i sb-vm:n-word-bits)
                   do (setf res (ldb (byte sb-vm:n-word-bits 0)
                                     (logior res (ash bits i)))))
             res)))
        (progn
          (when node
            (delay-ir1-transform node :constraint))
          (let* ((with
                   (ecase kind
                     (:tagged
                      'fixnum)
                     (:char
                      (if (= n-bits sb-vm:n-word-bits)
                          'word
                          (format nil "UB~A" n-bits)))
                     (:bits
                      (cond ((not (csubtypep element-ctype (specifier-type 'unsigned-byte)))
                             (format nil "SB~A" n-bits))
                            ((= n-bits sb-vm:n-word-bits)
                             'word)
                            (t
                             (format nil "UB~A" n-bits))))
                     (:single-float
                      'single-float)
                     #+64-bit
                     (:double-float
                      'double-float)
                     #+64-bit
                     (:complex-single-float
                      'complex-single-float)))
                 (basher-name (if (eq with 'word)
                                  (format nil "UB~D-BASH-FILL" n-bits)
                                  (format nil "UB~D-BASH-FILL-WITH-~A"
                                          n-bits (string with)))))
            (values
             (or (find-symbol basher-name #.(find-package "SB-KERNEL"))
                 (abort-ir1-transform
                  "Unknown fill basher, please report to sbcl-devel: ~A"
                  basher-name))
             (if (eq kind :char)
                 '(char-code item)
                 'item)))))))

(deftransform quickfill ((seq item) (vector t) * :node node)
  ;; The QUICKFILL function has no START,END lexical vars, but if
  ;; the transform hits the bashable non-simple or non-bashable case,
  ;; it will invoke WITH-ARRAY-DATA using these variables.
  `(let ((start 0) (end nil))
     (declare (ignorable start end))
     ,(fill-transform 'quickfill node seq item nil nil)))
(deftransform fill ((seq item &key (start 0) (end nil))
                    (vector t &key (:start t) (:end t))
                    *
                    :node node)
  (fill-transform 'fill node seq item start end))
(defun fill-transform (fun-name node seq item start end)
  (add-annotation seq
                  (make-lvar-sequence-bounds-annotation :deps (list start end)
                                                        :source-path (node-source-path node)))
  (let* ((type (lvar-type seq))
         (element-ctype (array-type-upgraded-element-type type))
         (element-type (type-specifier element-ctype))
         (saetp (unless (eq *wild-type* element-ctype)
                  (find-saetp-by-ctype element-ctype))))
    (cond ((eq *wild-type* element-ctype)
           (delay-ir1-transform node :constraint)
           `(vector-fill* seq item start end))
          ((and (csubtypep type (specifier-type 'simple-base-string))
                (not start)
                (not end)
                (policy node (>= speed space)))
           (let ((multiplier (logand #x0101010101010101 most-positive-word)))
             `(let* ((value ,(if (and (constant-lvar-p item)
                                      (typep (lvar-value item) 'base-char))
                                 (* multiplier (char-code (lvar-value item)))
                                 ;; Use multiplication if it's known to be cheap
                                 #+(or x86 x86-64)
                                 `(* ,multiplier (char-code (the base-char item)))
                                 #-(or x86 x86-64)
                                 '(let ((code (char-code (the base-char item))))
                                   (setf code (dpb code (byte 8 8) code))
                                   (setf code (dpb code (byte 16 16) code))
                                   #+64-bit (dpb code (byte 32 32) code))))
                     (len (vector-length seq))
                     (words (truncate len sb-vm:n-word-bytes)))
                (dotimes (index words)
                  (declare (optimize (speed 3) (safety 0))
                           (type index index))
                  (setf (%vector-raw-bits seq index) value))
                ;; For 64-bit:
                ;;  if 1 more byte should be written, then shift-towards-start 56
                ;;  if 2 more bytes ...               then shift-towards-start 48
                ;;  etc
                ;; This correctly rewrites the trailing null in its proper place.
                (let ((bits (ash (mod len sb-vm:n-word-bytes) 3)))
                  (when (plusp bits)
                    (setf (%vector-raw-bits seq words)
                          (shift-towards-start value (- bits)))))
                seq)))
          ((and (csubtypep type (specifier-type 'simple-bit-vector))
                (not start)
                (not end)
                (policy node (>= speed space)))
           `(let ((value (logand (- (the bit item)) most-positive-word)))
              ;; Unlike for the SIMPLE-BASE-STRING case, we are allowed to touch
              ;; bits beyond LENGTH with impunity.
              (dotimes (index (ceiling (vector-length seq) sb-vm:n-word-bits))
                (declare (optimize (speed 3) (safety 0))
                         (type index index))
                (setf (%vector-raw-bits seq index) value))
              seq))
          ;; FIXME: this case takes over before we get a chance to select
          ;; a variant of the SPLAT vop that can use XMM registers.
          ;; Should this be #-x86-64 then?
          ((and (array-type-p type)
                (not (array-type-complexp type))
                (or (not start)
                    (and (constant-lvar-p start)
                         (eql (lvar-value start) 0)))
                (not end)
                (typep (array-type-dimensions type) '(cons number null))
                (<= (car (array-type-dimensions type))
                    (cond #+soft-card-marks
                          ((eq element-ctype *universal-type*)
                           ;; Each write will have a store barrier,
                           ;; marking it pretty large.
                           2)
                          (t
                           10))))
           `(progn
              ,@(loop for i below (car (array-type-dimensions type))
                      collect `(setf (aref seq ,i) item))
              seq))
          #+x86-64
          ((and (type= element-ctype *universal-type*)
                (csubtypep (lvar-type seq) (specifier-type '(simple-array * (*))))
                ;; FIXME: why can't this work with arbitrary START and END?
                ;; VECTOR-FILL/T certainly seems to take them.
                (or (not start)
                    (and (constant-lvar-p start)
                         (eql (lvar-value start) 0)))
                (or (not end)
                    ;; QUICKFILL always fills the whole vector, but I anticipate
                    ;; supplying END to avoid a call to VECTOR-LENGTH
                    (eq fun-name 'quickfill)))
           ;; VECTOR-LENGTH entails one fewer transform than LENGTH
           ;; and it too can derive a constant length if known.
           '(vector-fill/t seq item 0 (vector-length seq)))
          ((and saetp (sb-vm:valid-bit-bash-saetp-p saetp))
           (multiple-value-bind (basher bash-value) (find-basher saetp item node)
             (values
              ;; KLUDGE: WITH-ARRAY data in its full glory is going to mess up
              ;; dynamic-extent for MAKE-ARRAY :INITIAL-ELEMENT initialization.
              (cond
                ((eq fun-name 'quickfill)
                 ;; array is simple, and out-of-bounds can't happen
                 `(,basher ,bash-value seq 0 (vector-length seq)))
                ;; FIXME: isn't this (NOT (CONSERVATIVE-ARRAY-TYPE-COMPLEXP (lvar-type seq))) ?
                ((csubtypep (lvar-type seq) (specifier-type '(simple-array * (*))))
                  `(block nil
                     (tagbody
                        (let* ((len (vector-length seq))
                               (end (cond (end
                                           (when (> end len)
                                             (go bad-index))
                                           end)
                                          (len))))
                          (return (,basher ,bash-value seq
                                           ,(if (or (not start)
                                                    (and (constant-lvar-p start)
                                                         (eql (lvar-value start) 0)))
                                                0
                                                `(if (> start end)
                                                     (go bad-index)
                                                     start))
                                           (- end start))))
                      bad-index
                        (sequence-bounding-indices-bad-error seq start end))))
                (t
                  `(with-array-data ((data seq)
                                     (start start)
                                     (end end)
                                     :check-fill-pointer t)
                     (declare (type (simple-array ,element-type 1) data))
                     (declare (type index start end))
                     (declare (optimize (safety 0) (speed 3)))
                     (,basher ,bash-value data start (- end start))
                     seq)))
              `((declare (type ,element-type item))))))
          ;; OK, it's not a "bashable" array type.
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
               ;; Force bounds-checks to 0 even if local policy had it >0.
               (declare (optimize (safety 0) (speed 3)
                                  (insert-array-bounds-checks 0)))
               ,(cond #+x86-64
                      ((type= element-ctype *universal-type*)
                       '(vector-fill/t data item start end))
                      (t
                       `(do ((i start (1+ i)))
                            ((= i end))
                          (declare (type index i))
                          (setf (aref data i) item))))
               seq)
            ;; ... though we still need to check that the new element can fit
            ;; into the vector in safe code. -- CSR, 2002-07-05
            `((declare (type ,element-type item)))))
          ((csubtypep type (specifier-type 'string))
           '(string-fill* seq item start end))
          (t
           '(vector-fill* seq item start end)))))

(deftransform fill ((seq item &key (start 0) (end nil))
                    ((and sequence (not vector) (not list)) t &key (:start t) (:end t)))
  `(sb-sequence:fill seq item
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
;;; must be simple.
(macrolet ((def (name test index)
             `(deftransform ,name ((string1 string2 start1 end1 start2 end2)
                                   (simple-string simple-string t t t t) *)
                `(multiple-value-bind (index diff)
                     (%sp-string-compare string1 string2 start1 end1 start2 end2)
                   (if ,',test
                       ,,(if index ''index 'nil)
                       ,,(if index 'nil ''index))))))
  (def string<* (< diff 0) t)
  (def string<=* (> diff 0) nil)
  (def string>* (> diff 0) t)
  (def string>=* (< diff 0) nil))

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
         `(and (plusp (length string2))
               0))
        ((and (constant-lvar-p string2)
              (equal (lvar-value string2) ""))
         `(and (plusp (length string1))
               0))
        (t
         (give-up-ir1-transform))))

(deftransform string=*
    ((string1 string2 start1 end1 start2 end2) (simple-base-string simple-base-string t t t t) *)
  `(simple-base-string= string1 string2 start1 end1 start2 end2))

#+sb-unicode
(deftransform string=*
    ((string1 string2 start1 end1 start2 end2) (simple-character-string simple-character-string t t t t) *)
  `(simple-character-string= string1 string2 start1 end1 start2 end2))

(deftransform string/=*
    ((string1 string2 start1 end1 start2 end2) (simple-string simple-string t t t t) *)
  `(multiple-value-bind (index diff)
       (%sp-string-compare string1 string2 start1 end1 start2 end2)
     (declare (ignorable index))
     (if (,'/= diff 0)
         ,'index
         nil)))

(defun string-compare-transform (string1 string2 start1 end1 start2 end2)
  (let* ((start1 (if start1
                     (lvar-value start1)
                     0))
         (start2 (if start2
                     (lvar-value start2)
                     0))
         (lengths1 (vector-type-lengths (lvar-type string1)))
         (lengths2 (vector-type-lengths (lvar-type string2)))
         (end1 (and end1 (lvar-value end1)))
         (end2 (and end2 (lvar-value end2))))
    (if (and lengths1
             lengths2
             (loop for length1 in lengths1
                   never
                   (loop for length2 in lengths2
                         thereis
                         (let ((end1 (or end1 length1))
                               (end2 (or end2 length2)))
                           (or (not (and (<= start1 end1 length1)
                                         (<= start2 end2 length2)))
                               (= (- end1 start1)
                                  (- end2 start2)))))))
        nil
        (give-up-ir1-transform))))

(deftransforms (string=* simple-base-string=
                         simple-character-string=)
    ((string1 string2 start1 end1 start2 end2)
     (t t (constant-arg t) (constant-arg t) (constant-arg t) (constant-arg t)))
  (string-compare-transform string1 string2 start1 end1 start2 end2))

(deftransform string-equal ((string1 string2 &key start1 end1 start2 end2)
                            (t t &key (:start1 (constant-arg t))
                               (:start2 (constant-arg t))
                               (:end1 (constant-arg t))
                               (:end2 (constant-arg t))))
  (string-compare-transform string1 string2 start1 end1 start2 end2))

(deftransform string/=* ((str1 str2 start1 end1 start2 end2) * * :node node
                         :important nil)
  ;; An IF node doesn't care about the mismatch index.
  ;; Transforming to (not (string= ..)) would lead to internal confusion
  ;; due to incorrect typing: STRING/= can't return T, so return 0 for true.
  (if (if-p (node-dest node))
      `(if (string=* str1 str2 start1 end1 start2 end2) nil 0)
      (give-up-ir1-transform)))

(defun check-sequence-test (item sequence test key node)
  (let ((item (lvar-type item)))
    (when (or (not test)
              (lvar-fun-is test '(eq eql equal equalp two-arg-=)))
      (labels ((sequence-element-type (type)
                 (cond ((array-type-p type)
                        (let ((elt-type (array-type-element-type type)))
                          (if (eq elt-type *wild-type*)
                              *universal-type*
                              elt-type)))
                       ((csubtypep type (specifier-type 'string))
                        (specifier-type 'character))
                       (t
                        *universal-type*))))
        (multiple-value-bind (key-type key) (and key
                                                 (lvar-fun-type key))
          (let ((*compiler-error-context* node))
            (when (and (or (not key)
                           (eq key 'identity))
                       (not (types-equal-or-intersect item (sequence-element-type (lvar-type sequence)))))
              (compiler-style-warn "Item of type ~s can't be found in a sequence of type ~s."
                                   (type-specifier item)
                                   (type-specifier (lvar-type sequence))))
            (when (fun-type-p key-type)
              (let ((returns (single-value-type (fun-type-returns key-type))))
                (unless (types-equal-or-intersect item returns)
                  (compiler-style-warn "Item of type ~s can't be found using :key ~s which returns ~s."
                                       (type-specifier item)
                                       key
                                       (type-specifier returns)))))))))))

(defun check-sequence-ranges (string start end node &optional (suffix "") sequence-name)
  (let* ((type (lvar-type string))
         (lengths (vector-type-lengths type))
         (annotation (find-if #'lvar-sequence-bounds-annotation-p (lvar-annotations string))))
    (when annotation
      (when (shiftf (lvar-annotation-fired annotation) t)
        (return-from check-sequence-ranges)))
    (flet ((arg-type (x)
             (typecase x
               (constant (ctype-of (constant-value x)))
               (lvar (lvar-type x))
               (t (leaf-type x)))))
      (flet ((check (index name length-type)
               (when index
                 (let ((index-type (arg-type index)))
                   (unless (types-equal-or-intersect index-type
                                                     (specifier-type length-type))
                     (let ((*compiler-error-context* node))
                       (compiler-warn "Bad :~a~a ~a for~a ~a"
                                      name suffix
                                      (type-specifier index-type)
                                      (if sequence-name
                                          (format nil " for ~a of type" sequence-name)
                                          suffix)
                                      (type-specifier type))
                       t))))))
        (loop for length in lengths
              thereis
              (check start "start" `(integer 0 ,length)))
        (loop for length in lengths
              thereis
              (check end "end" `(or null (integer 0 ,length)))))
      (when (and start end)
        (let* ((start-type (arg-type start))
               (start-interval (type-approximate-interval start-type))
               (end-type (arg-type end))
               (end-interval (type-approximate-interval end-type)))
          (when (and (interval-p start-interval)
                     (interval-p end-interval)
                     (interval-< end-interval start-interval))
            (let ((*compiler-error-context* node))
              (compiler-warn ":start~a ~a is greater than :end~a ~a"
                             suffix
                             (type-specifier start-type)
                             suffix
                             (type-specifier end-type)))))))))
(defoptimizers ir2-hook
    (string=* string<* string>* string<=* string>=*
     %sp-string-compare simple-base-string=
     #+sb-unicode simple-character-string=)
    ((string1 string2 start1 end1 start2 end2) node)
  (check-sequence-ranges string1 start1 end1 node 1 'string1)
  (check-sequence-ranges string2 start2 end2 node 2 'string2))

(defoptimizers ir2-hook
    (string-equal string-not-equal string-greaterp string-lessp)
    ((string1 string2 &key start1 end1 start2 end2) node)
  (check-sequence-ranges string1 start1 end1 node 1 'string1)
  (check-sequence-ranges string2 start2 end2 node 2 'string2))

(defoptimizers ir2-hook
    (string-downcase string-upcase
     nstring-downcase nstring-upcase
     string-capitalize nstring-capitalize)
    ((string &key start end) node)
  (check-sequence-ranges string start end node))

(defoptimizers ir2-hook
    (find-if find-if-not position-if position-if-not
     remove-if remove-if-not delete-if delete-if-not
     count-if count-if-not
     reduce remove-duplicates delete-duplicates)
    ((x sequence &key start end &allow-other-keys) node)
  (check-sequence-ranges sequence start end node))

(defoptimizers ir2-hook
    (find position
     remove delete
     count)
    ((item sequence &key key test start end &allow-other-keys) node)
  (check-sequence-ranges sequence start end node)
  (check-sequence-test item sequence test key node))

(defoptimizers ir2-hook
    (remove-duplicates delete-duplicates)
    ((sequence &key start end &allow-other-keys) node)
  (check-sequence-ranges sequence start end node))

(defoptimizer (%find-position ir2-hook) ((item sequence from-end start end key test) node)
  (check-sequence-ranges sequence start end node)
  (check-sequence-test item sequence test key node))

(defoptimizers ir2-hook
    (%find-position-if %find-position-if-not)
    ((predicate sequence from-end start end key) node)
  (check-sequence-ranges sequence start end node))

(defoptimizer (fill ir2-hook) ((sequence item &key start end) node)
  (check-sequence-ranges sequence start end node))

(defoptimizer (search ir2-hook) ((sub-sequence1 main-sequence2 &key start1 end1 start2 end2 &allow-other-keys) node)
  (check-sequence-ranges sub-sequence1 start1 end1 node 1 'sub-sequence1)
  (check-sequence-ranges main-sequence2 start2 end2 node 2 'main-sequence2))

(defoptimizer (mismatch ir2-hook) ((sequence1 sequence2 &key start1 end1 start2 end2 &allow-other-keys) node)
  (check-sequence-ranges sequence1 start1 end1 node 1 'sequence1)
  (check-sequence-ranges sequence2 start2 end2 node 2 'sequence2))

(defoptimizer (vector-subseq* ir2-hook) ((vector start end) node)
  (check-sequence-ranges vector start end node))

(defun string-cmp-deriver (string1 string2 start1 end1 start2 end2 &optional equality)
  (flet ((dims (string start end)
           (let* ((type (lvar-type string))
                  (length (vector-type-length type))
                  (start (cond ((not start)
                                0)
                               ((constant-lvar-p start)
                                (lvar-value start))))
                  (end (cond ((not end)
                              length)
                             ((constant-lvar-p end)
                              (or (lvar-value end)
                                  length)))))
             (values
              start
              end
              (and start end
                   (- end start))))))
    (multiple-value-bind (start1 end1 length1)
        (dims string1 start1 end1)
      (let (low
            high
            (length2 (nth-value 2 (dims string2 start2 end2))))
        (when start1
          (setf low start1))
        (when end1
          (setf high end1))
        (when (and length2 start1)
          (let ((high2 (+ start1 length2)))
            (when (or (not high)
                      (> high high2))
              (setf high high2))))
        (when (or low high)
          (let ((type (make-numeric-type :class 'integer :high  high :low low)))
            (if (and equality length1 length2
                     (/= length1 length2))
                (if (eq equality '%sp-string-compare)
                    (make-values-type (list type
                                            (specifier-type '(and integer (not (eql 0))))))
                    type)
                (type-union type
                            (specifier-type 'null)))))))))

(macrolet ((def (name &optional equality)
             `(defoptimizer (,name derive-type) ((string1 string2 start1 end1 start2 end2))
                (string-cmp-deriver string1 string2 start1 end1 start2 end2 ,equality))))
  (def string<*)
  (def string>*)
  (def string<=*)
  (def string>=*)
  (def string/=* t)
  (def %sp-string-compare '%sp-string-compare))

(macrolet ((def (name &optional equality)
             `(defoptimizer (,name derive-type) ((string1 string2 &key start1 end1 start2 end2))
                (string-cmp-deriver string1 string2 start1 end1 start2 end2 ,equality))))
  (def string-greaterp)
  (def string-lessp)
  (def string-not-equal t))

(deftransform string ((x) (symbol)) '(symbol-name x))
(deftransform string ((x) (string)) '(progn x))

;;;; transforms for sequence functions

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
;;; (e.g. SB-IMPL::BASE-STRING-SOUT).
(defun transform-replace-bashable (bash-function node)
  ;; This is a little circuitous - we transform REPLACE into BASH-COPY
  ;; and then possibly transform BASH-COPY into an unrolled loop.
  ;; There ought to be a way to see if the BASH-COPY transform applies.
  `(let* ((len1 (length seq1))
          (len2 (length seq2))
          (end1 (or end1 len1))
          (end2 (or end2 len2))
          (replace-len (min (- end1 start1) (- end2 start2))))
     ,@(when (policy node (/= insert-array-bounds-checks 0))
         '((unless (<= 0 start1 end1 len1)
             (sequence-bounding-indices-bad-error seq1 start1 end1))
           (unless (<= 0 start2 end2 len2)
             (sequence-bounding-indices-bad-error seq2 start2 end2))))
     (,bash-function seq2 start2 seq1 start1 replace-len)
     seq1))
(defun transform-replace (same-types-p node)
  `(let* ((len1 (length seq1))
          (len2 (length seq2))
          (end1 (or end1 len1))
          (end2 (or end2 len2))
          (replace-len (min (- end1 start1) (- end2 start2))))
     ,@(when (policy node (/= insert-array-bounds-checks 0))
         '((unless (<= 0 start1 end1 len1)
             (sequence-bounding-indices-bad-error seq1 start1 end1))
           (unless (<= 0 start2 end2 len2)
             (sequence-bounding-indices-bad-error seq2 start2 end2))))
     ,(flet ((down ()
               '(do ((i (truly-the (or (eql -1) index) (+ start1 replace-len -1)) (1- i))
                     (j (truly-the (or (eql -1) index) (+ start2 replace-len -1)) (1- j)))
                 ((< j start2))
                 (declare (optimize (insert-array-bounds-checks 0)))
                 (setf (aref seq1 i) (data-vector-ref seq2 j))))
             (up ()
               '(do ((i start1 (1+ i))
                     (j start2 (1+ j))
                     (end (+ start1 replace-len)))
                 ((>= i end))
                 (declare (optimize (insert-array-bounds-checks 0)))
                 (setf (aref seq1 i) (data-vector-ref seq2 j)))))
        ;; "If sequence-1 and sequence-2 are the same object and the region being modified
        ;;  overlaps the region being copied from, then it is as if the entire source region
        ;;  were copied to another place and only then copied back into the target region.
        ;;  However, if sequence-1 and sequence-2 are not the same, but the region being modified
        ;;  overlaps the region being copied from (perhaps because of shared list structure or
        ;;  displaced arrays), then after the replace operation the subsequence of sequence-1
        ;;  being modified will have unpredictable contents."
        (if same-types-p ; source and destination sequences could be EQ
            `(if (and (eq seq1 seq2) (> start1 start2)) ,(down) ,(up))
            (up)))
     seq1))

(deftransform replace ((seq1 seq2 &key (start1 0) (start2 0) end1 end2)
                       ((simple-array * (*)) (simple-array * (*)) &rest t) (simple-array * (*))
                       :node node)
  (let ((et1 (ctype-array-specialized-element-types (lvar-type seq1)))
        (et2 (ctype-array-specialized-element-types (lvar-type seq2))))
    (if (and (typep et1 '(cons * null))
             (typep et2 '(cons * null))
             (eq (car et1) (car et2)))
        (let ((saetp (find-saetp-by-ctype (car et1))))
          (if (sb-vm:valid-bit-bash-saetp-p saetp)
              (transform-replace-bashable
               (intern (format nil "UB~D-BASH-COPY" (sb-vm:saetp-n-bits saetp))
                       #.(find-package "SB-KERNEL"))
               node)
              (transform-replace t node)))
        (give-up-ir1-transform))))
#+sb-unicode
(progn
(deftransform replace ((seq1 seq2 &key (start1 0) (start2 0) end1 end2)
                       (simple-base-string simple-character-string &rest t) simple-base-string
                       :node node)
  (transform-replace nil node))
(deftransform replace ((seq1 seq2 &key (start1 0) (start2 0) end1 end2)
                       (simple-character-string simple-base-string &rest t) simple-character-string
                       :node node)
  (transform-replace nil node)))

(defoptimizer (replace ir2-hook) ((seq1 seq2 &key start1 end1 start2 end2) node)
  (flet ((element-type (lvar)
           (type-array-element-type (lvar-type lvar))))
    (let ((type1 (element-type seq1))
          (type2 (element-type seq2)))
      (check-sequence-ranges seq1 start1 end1 node 1 'target-sequence1)
      (check-sequence-ranges seq2 start2 end2 node 2 'source-sequence2)
      (cond ((eq type1 *wild-type*))
            ((eq type2 *wild-type*)
             (when (constant-lvar-p seq2)
               (map nil (lambda (x)
                          (unless (ctypep x type1)
                            (let ((*compiler-error-context* node))
                              (compiler-warn "The source sequence has an element ~s incompatible with the target array element type ~a."
                                             x
                                             (type-specifier type1)))
                            (return-from replace-ir2-hook-optimizer))
                          x)
                    (lvar-value seq2))))
            ((not (types-equal-or-intersect type1 type2))
             (let ((*compiler-error-context* node))
               (compiler-warn "Incompatible array element types: ~a and ~a"
                              (type-specifier type1)
                              (type-specifier type2))))))))

(defoptimizer (%make-array ir2-hook) ((dimensions widetag n-bits &key initial-contents &allow-other-keys) node)
  (when (and (constant-lvar-p widetag)
             initial-contents)
    (let* ((saetp (find (lvar-value widetag) sb-vm:*specialized-array-element-type-properties*
                        :key #'sb-vm:saetp-typecode))
           (element-type (sb-vm:saetp-ctype saetp))
           (initial-contents-type (lvar-type initial-contents))
           (initial-contents-element-type (type-array-element-type initial-contents-type)))
      (cond ((not (or (eq initial-contents-element-type *wild-type*)
                      (types-equal-or-intersect element-type initial-contents-element-type)))
             (let ((*compiler-error-context* node))
               (compiler-warn "Incompatible :initial-contents ~s for :element-type ~a."
                              (type-specifier initial-contents-type)
                              (sb-vm:saetp-specifier saetp))))
            ((constant-lvar-p initial-contents)
             (let ((initial-contents (lvar-value initial-contents)))
               (when (sequencep initial-contents)
                 (map nil (lambda (x)
                            (unless (ctypep x element-type)
                              (let ((*compiler-error-context* node))
                                (compiler-warn ":initial-contents has an element ~s incompatible with :element-type ~a."
                                               x
                                               (type-specifier element-type)))
                              (return-from %make-array-ir2-hook-optimizer))
                            x)
                      initial-contents))))))))

(defun check-sequence-item (item seq node format-string)
  (let ((seq-type (lvar-type seq))
        (item-type (lvar-type item)))
    (when (neq item-type *wild-type*)
      (let ((element-type (type-array-element-type seq-type)))
        (unless (or (eq element-type *wild-type*)
                    (types-equal-or-intersect item-type element-type))
          (let ((*compiler-error-context* node))
            (compiler-warn format-string
                           (type-specifier item-type)
                           (type-specifier seq-type))))))))

(defoptimizers ir2-hook
    (substitute substitute-if substitute-if-not
                nsubstitute nsubstitute-if nsubstitute-if-not)
    ((new x seq &key start end &allow-other-keys) node)
  (check-sequence-ranges seq start end node)
  (check-sequence-item new seq node "Can't substitute ~a into ~a"))

(defoptimizer (vector-fill* ir2-hook) ((seq item start end) node)
  (check-sequence-ranges seq start end node)
  (check-sequence-item item seq node "Can't fill ~a into ~a"))

(defoptimizer (vector-push ir2-hook) ((item vector) node)
  (check-sequence-item item vector node "Can't push ~a into ~a"))

(defoptimizer (vector-push-extend ir2-hook) ((item vector &optional min-extension) node)
  (check-sequence-item item vector node "Can't push ~a into ~a"))

(defun check-concatenate (type sequences node &optional (description "concatenate"))
  (let ((result-element-type (if (ctype-p type)
                                 type
                                 (type-array-element-type (or (careful-specifier-type type)
                                                              (return-from check-concatenate))))))
    (unless (or (eq result-element-type *wild-type*)
                (eq result-element-type *universal-type*))
      (loop for i from 0
            for sequence in sequences
            for sequence-type = (lvar-type sequence)
            for element-type = (type-array-element-type sequence-type)
            do (unless (or (eq element-type *wild-type*)
                           (types-equal-or-intersect element-type result-element-type))
                 (let ((*compiler-error-context* node))
                   (compiler-warn "Can't ~a ~s into ~s"
                                  description
                                  (type-specifier sequence-type)
                                  (if (ctype-p type)
                                      (type-specifier (make-array-type '(*)
                                                                       :specialized-element-type type
                                                                       :element-type type))
                                      type))))))))

(defoptimizer (%concatenate-to-string ir2-hook) ((&rest args) node)
  (check-concatenate 'string args node))

(defoptimizer (%concatenate-to-base-string ir2-hook) ((&rest args) node)
  (check-concatenate 'base-string args node))

(defoptimizer (%concatenate-to-vector ir2-hook) ((widetag &rest args) node)
  (when (constant-lvar-p widetag)
    (check-concatenate (sb-vm:saetp-ctype
                        (find (lvar-value widetag)
                              sb-vm:*specialized-array-element-type-properties*
                              :key #'sb-vm:saetp-typecode))
                       args node)))

(defoptimizer (merge ir2-hook) ((type sequence1 sequence2 predicate &key &allow-other-keys) node)
  (when (constant-lvar-p type)
   (check-concatenate (lvar-value type) (list sequence1 sequence2) node "merge")))

;;; Expand simple cases of UB<SIZE>-BASH-COPY inline.  "simple" is
;;; defined as those cases where we are doing word-aligned copies from
;;; both the source and the destination and we are copying from the same
;;; offset from both the source and the destination.  (The last
;;; condition is there so we can determine the direction to copy at
;;; compile time rather than runtime.  Remember that UB<SIZE>-BASH-COPY
;;; acts like memmove, not memcpy.)  These conditions may seem rather
;;; restrictive, but they do catch common cases, like allocating a (* 2
;;; N)-size buffer and blitting in the old N-size buffer in.

(defun make-bash-copy-transform (n-bits-per-elem)
  (deftransform transform-bash-copy ((src src-offset dst dst-offset length)
                                     * *
                                     :defun-only lambda)
    (declare (ignore src dst length))
    (binding* ((n-elems-per-word (truncate sb-vm:n-word-bits n-bits-per-elem))
               ((src-word src-elt) (truncate (lvar-value src-offset) n-elems-per-word))
               ((dst-word dst-elt) (truncate (lvar-value dst-offset) n-elems-per-word)))
      ;; Avoid non-word aligned copies.
      (unless (and (zerop src-elt) (zerop dst-elt))
        (give-up-ir1-transform))
      ;; Avoid copies where we would have to insert code for
      ;; determining the direction of copying.
      (unless (= src-word dst-word)
        (give-up-ir1-transform))
      `(let ((end (+ ,src-word (truncate (the index length) ,n-elems-per-word)))
             (extra (mod length ,n-elems-per-word)))
         (declare (type index end))
         ;; Handle any bits at the end.
         (unless (zerop extra)
           ;; MASK selects just the bits that we want from the ending word of
           ;; the source array. The number of bits to shift out is
           ;;   (- n-word-bits (* extra n-bits-per-elem))
           ;; which is equal mod n-word-bits to the expression below.
           (let ((mask (shift-towards-start
                        most-positive-word (* extra ,(- n-bits-per-elem)))))
             (%set-vector-raw-bits
              dst end (logior (logand (%vector-raw-bits src end) mask)
                              (logandc2 (%vector-raw-bits dst end) mask)))))
         ;; Copy from the end to save a register.
         (do ((i (1- end) (1- i)))
             ((< i ,src-word))
           (%set-vector-raw-bits dst i (%vector-raw-bits src i)))
         (values)))))

;;; Detect misuse with sb-devel. "Misuse" means mismatched array element types
#-sb-devel
(loop for i = 1 then (* i 2)
      do (%deftransform (package-symbolicate "SB-KERNEL" "UB" i "-BASH-COPY")
                        nil
                        '(function ((simple-unboxed-array (*)) (constant-arg index)
                                    (simple-unboxed-array (*)) (constant-arg index)
                                    index) *)
                        (make-bash-copy-transform i))
      until (= i sb-vm:n-word-bits))

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
    (if (>= (sb-vm:saetp-n-bits saetp) sb-vm:n-word-bits)
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
                  (type (integer 0 #.array-dimension-limit) j i))
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
                                    (sb-vm:saetp-initial-element-default saetp)))
                              (unless (ctypep default-initial-element elt-ctype)
                                ;; As with MAKE-ARRAY, this is merely undefined
                                ;; behavior, not an error.
                                (compiler-style-warn
                                 'initial-element-mismatch-style-warning
                                 :format-control "The default initial element ~S is not a ~S."
                                 :format-arguments (list default-initial-element elt-type)))))
                           ;; In would be possible in some cases,
                           ;; like :INITIAL-ELEMENT (IF X #\x #\y) in a call
                           ;; to MAKE-SEQUENCE '(VECTOR (MEMBER #\A #\B))
                           ;; to detect erroneous non-constants initializers,
                           ;; but it is not important enough to bother with.
                           ((and (constant-lvar-p initial-element)
                                 (not (ctypep (lvar-value initial-element)
                                              elt-ctype)))
                            ;; MAKE-ARRAY considers this a warning, not an error.
                            (compiler-warn 'array-initial-element-mismatch
                                           :format-control "~S ~S is not a ~S"
                                           :format-arguments
                                           (list :initial-element (lvar-value initial-element) elt-type))))))
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
            ,@(when (policy node (/= insert-array-bounds-checks 0))
                '((unless (<= 0 start end length)
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
  '(sb-sequence:subseq seq start end))

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
  '(sb-sequence:copy-seq seq))

(deftransform search ((pattern text &key start1 start2 end1 end2 test test-not
                               key from-end)
                      ((constant-arg sequence) t &rest t))
  (if key
      (give-up-ir1-transform)
      (let* ((pattern (lvar-value pattern))
             (pattern-start (cond ((not (proper-sequence-p pattern))
                                   (give-up-ir1-transform))
                                  ((not start1)
                                   0)
                                  ((constant-lvar-p start1)
                                   (lvar-value start1))
                                  (t
                                   (give-up-ir1-transform))))
             (pattern-end (cond ((not end1)
                                 (length pattern))
                                ((constant-lvar-p end1)
                                 (or (lvar-value end1)
                                     (length pattern)))
                                (t
                                 (give-up-ir1-transform))))
             (pattern (if (and (= (- pattern-end pattern-start) 1)
                               (sequence-of-length-at-least-p pattern
                                                              (1+ pattern-start)))
                          (elt pattern pattern-start)
                          (give-up-ir1-transform))))
        (macrolet ((maybe-arg (arg &optional (key (keywordicate arg)))
                     `(and ,arg `(,,key ,',arg))))
          `(position ',pattern text
                     ,@(maybe-arg start2 :start)
                     ,@(maybe-arg end2 :end)
                     ,@(maybe-arg test)
                     ,@(maybe-arg test-not)
                     ,@(maybe-arg from-end))))))

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

(defoptimizer (search derive-type) ((sequence1 sequence2
                                               &key start1 end1 start2 end2
                                               from-end
                                               &allow-other-keys))
  (let* ((constant-start1 (and start1
                               (constant-lvar-p start1)
                               (lvar-value start1)))
         (constant-end1 (and end1
                             (constant-lvar-p end1)
                             (lvar-value end1)))
         (constant-start2 (and start2
                               (constant-lvar-p start2)
                               (lvar-value start2)))
         (constant-end2 (and end2
                             (constant-lvar-p end2)
                             (lvar-value end2)))
         (not-from-end (unsupplied-or-nil from-end))
         (min-result (or constant-start2 0))
         (max-result (or constant-end2 (1- array-dimension-limit)))
         (max2 (sequence-lvar-dimensions sequence2))
         (max-result (if (integerp max2)
                         (min max-result max2)
                         max-result))
         (min1 (nth-value 1 (sequence-lvar-dimensions sequence1)))
         (min-sequence1-length (cond ((and constant-start1 constant-end1)
                                      (- constant-end1 constant-start1))
                                     ((and constant-end1 (not start1))
                                      constant-end1)
                                     ((and constant-start1
                                           (not end1)
                                           (integerp min1))
                                      (- min1 constant-start1))
                                     ((or start1 end1 (not (integerp min1)))
                                      ;; The result can be equal to MAX-RESULT only when
                                      ;; searching for "" and :start2 being equal to :end2
                                      ;; or :from-end t
                                      (if (or (not not-from-end)
                                              (and start2
                                                   (not constant-start2))
                                              (= max-result min-result))
                                          0
                                          1))
                                     (t
                                      min1))))
    (specifier-type `(or (integer ,min-result
                                  ,(- max-result min-sequence1-length))
                         null))))

(defun index-into-sequence-derive-type (sequence start end &key (inclusive t))
  (let* ((constant-start (and start
                              (constant-lvar-p start)
                              (lvar-value start)))
         (constant-end (and end
                            (constant-lvar-p end)
                            (lvar-value end)))
         (min-result (or constant-start 0))
         (max-result (or constant-end (1- array-dimension-limit)))
         (max (sequence-lvar-dimensions sequence))
         (max-result (if (integerp max)
                         (min max-result max)
                         max-result)))
    (values min-result (if inclusive
                           max-result
                           (1- max-result)))))

(defoptimizer (mismatch derive-type) ((sequence1 sequence2
                                                 &key start1 end1
                                                 &allow-other-keys))
  (declare (ignorable sequence2))
  ;; Could be as smart as the SEARCH one above but I ran out of steam.
  (multiple-value-bind (min max) (index-into-sequence-derive-type sequence1 start1 end1)
    (specifier-type `(or (integer ,min ,max) null))))

(defun position-derive-type (item sequence start end key test test-not)
  (multiple-value-bind (min max)
      (index-into-sequence-derive-type sequence start end :inclusive nil)
    (when (>= max min)
      (let ((integer-range `(integer ,min ,max))
            (definitely-foundp nil))
        ;; Figure out whether this call will not return NIL.
        ;; This could be smarter about the keywords args, but the primary intent
        ;; is to avoid a style-warning about arithmetic in such forms such as
        ;;  (1+ (position (the (member :x :y) item) #(:foo :bar :x :y))).
        ;; In that example, a more exact bound could be determined too.
        (cond ((or (not (constant-lvar-p sequence))
                   start end key test test-not
                   (not item)))
              (t
               (let ((const-seq (lvar-value sequence))
                     (item-type (lvar-type item)))
                 (when (and (or (vectorp const-seq) (proper-list-p const-seq))
                            (member-type-p item-type))
                   (setq definitely-foundp t) ; assume best case
                   (block nil
                     (mapc-member-type-members
                      (lambda (possibility)
                        (unless (find possibility const-seq)
                          (setq definitely-foundp nil)
                          (return)))
                      item-type))))))
        (specifier-type (if definitely-foundp
                            integer-range
                            `(or ,integer-range null)))))))

(defun find-derive-type (item sequence key test start end from-end)
  (declare (ignore start end from-end))
  (let ((type *universal-type*)
        (key-identity-p (or (not key)
                            (lvar-value-is key nil)
                            (lvar-fun-is key '(identity)))))
    (flet ((fun-accepts-type (fun-lvar argument)
             (when fun-lvar
               (let ((fun-type (lvar-fun-type fun-lvar t t)))
                 (when (fun-type-p fun-type)
                   (let ((arg (nth argument (fun-type-n-arg-types (1+ argument) fun-type))))
                     (when arg
                       (setf type
                             (type-intersection type arg)))))))))
      (when (and item
                 key-identity-p
                 (or (not test)
                     (lvar-fun-is test '(eq eql char= char-equal))
                     (lvar-value-is test nil)))
        ;; Maybe FIND returns ITEM itself (or an EQL number).
        (setf type (lvar-type item)))
      ;; Should return something the functions can accept
      (if key-identity-p
          (fun-accepts-type test (if item 1 0)) ;; the -if variants.
          (fun-accepts-type key 0)))
    (let ((upgraded-type (type-array-element-type (lvar-type sequence))))
      (unless (eq upgraded-type *wild-type*)
        (setf type
              (type-intersection type upgraded-type))))
    (unless (eq type *empty-type*)
      (type-union type
                  (specifier-type 'null)))))

(defoptimizer (find derive-type) ((item sequence &key key test
                                        start end from-end))
  (find-derive-type item sequence key test start end from-end))

(defoptimizer (find-if derive-type) ((predicate sequence &key key start end from-end))
  (find-derive-type nil sequence key predicate start end from-end))

(defoptimizer (find-if-not derive-type) ((predicate sequence &key key start end from-end))
  (find-derive-type nil sequence key predicate start end from-end))

(defoptimizer (position derive-type) ((item sequence
                                            &key start end
                                            key test test-not
                                            &allow-other-keys))
  (position-derive-type item sequence start end key test test-not))

(defoptimizer (position-if derive-type) ((function sequence
                                                   &key start end
                                                   &allow-other-keys))
  (declare (ignore function))
  (multiple-value-bind (min max)
      (index-into-sequence-derive-type sequence start end :inclusive nil)
    (specifier-type `(or (integer ,min ,max) null))))

(defoptimizer (position-if-not derive-type) ((function sequence
                                                       &key start end
                                                       &allow-other-keys))
  (declare (ignore function))
  (multiple-value-bind (min max)
      (index-into-sequence-derive-type sequence start end :inclusive nil)
    (specifier-type `(or (integer ,min ,max) null))))

(defoptimizer (%find-position derive-type) ((item sequence from-end start end key test))
  (let ((find (find-derive-type item sequence key test start end from-end))
        (position (position-derive-type item sequence start end key test nil)))
    (when (or find position)
      (make-values-type (list (or find *universal-type*)
                              (or position *universal-type*))))))

(defoptimizer (%find-position-if derive-type) ((predicate sequence from-end start end key))
  (let ((find (find-derive-type nil sequence key predicate start end from-end))
        (position (position-derive-type nil sequence start end key predicate nil)))
    (when (or find position)
      (make-values-type (list (or find *universal-type*)
                              (or position *universal-type*))))))

(defoptimizer (%find-position-if-not derive-type) ((predicate sequence from-end start end key))
  (let ((find (find-derive-type nil sequence key predicate start end from-end))
        (position (position-derive-type nil sequence start end key predicate nil)))
    (when (or find position)
      (make-values-type (list (or find *universal-type*)
                              (or position *universal-type*))))))

(defoptimizer (count derive-type) ((item sequence
                                         &key start end
                                         &allow-other-keys))
  (declare (ignore item))
  (multiple-value-bind (min max)
      (index-into-sequence-derive-type sequence start end)
    (specifier-type `(integer 0 ,(- max min)))))

(defoptimizer (count-if derive-type) ((function sequence
                                                &key start end
                                                &allow-other-keys))
  (declare (ignore function))
  (multiple-value-bind (min max)
      (index-into-sequence-derive-type sequence start end)
    (specifier-type `(integer 0 ,(- max min)))))

(defoptimizer (count-if-not derive-type) ((function sequence
                                                    &key start end
                                                    &allow-other-keys))
  (declare (ignore function))
  (multiple-value-bind (min max)
      (index-into-sequence-derive-type sequence start end)
    (specifier-type `(integer 0 ,(- max min)))))

(defoptimizer (subseq derive-type) ((sequence start &optional end) node)
  (let* ((sequence-type (lvar-type sequence))
         (constant-start (and (constant-lvar-p start)
                              (lvar-value start)))
         (constant-end (and end
                            (constant-lvar-p end)
                            (lvar-value end)))
         (index-length (and constant-start constant-end
                            (- constant-end constant-start)))
         (list-type (specifier-type 'list)))
    (flet ((bad ()
             (let ((*compiler-error-context* node))
               (compiler-warn "Bad bounding indices ~s, ~s for ~
                               ~/sb-impl:print-type/"
                              constant-start constant-end sequence-type))))
      (cond ((and index-length
                  (minusp index-length))
             ;; Would be a good idea to transform to something like
             ;; %compile-time-type-error
             (bad))
            ((csubtypep sequence-type list-type)
             (let ((null-type (specifier-type 'null)))
               (cond ((csubtypep sequence-type null-type)
                      (cond ((or (and constant-start
                                      (plusp constant-start))
                                 (and index-length
                                      (plusp index-length)))
                             (bad))
                            ((eql constant-start 0)
                             null-type)
                            (t
                             list-type)))
                     ((not index-length)
                      list-type)
                     ((zerop index-length)
                      null-type)
                     (t
                      (specifier-type 'cons)))))
            ((csubtypep sequence-type (specifier-type 'vector))
             (let* ((dimensions
                      ;; Can't trust lengths from non-simple vectors due to
                      ;; fill-pointer and adjust-array
                      (and (csubtypep sequence-type (specifier-type 'simple-array))
                           (ctype-array-dimensions sequence-type)))
                    (dimensions-length
                      (and (singleton-p dimensions)
                           (integerp (car dimensions))
                           (car dimensions)))
                    (length (cond (index-length)
                                  ((and dimensions-length
                                        (not end)
                                        constant-start)
                                   (- dimensions-length constant-start))))
                    (simplified (simplify-vector-type sequence-type)))
               (cond ((and dimensions-length
                           (or
                            (and constant-start
                                 (> constant-start dimensions-length))
                            (and constant-end
                                 (> constant-end dimensions-length))))
                      (bad))
                     (length
                      (type-intersection simplified
                                         (specifier-type `(simple-array * (,length)))))
                     (t
                      simplified))))
            ((not index-length)
             nil)
            ((zerop index-length)
             (specifier-type '(not cons)))
            (t
             (specifier-type '(not null)))))))

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

(defun string-concatenate-transform (node type lvars)
  (let ((vars (make-gensym-list (length lvars))))
    (if (policy node (<= speed space))
        ;; Out-of-line
        (let ((constants-to-string
                ;; Strings are handled more efficiently by
                ;; %concatenate-to-* functions
                (loop for var in vars
                      for lvar in lvars
                      collect (if (and (constant-lvar-p lvar)
                                       (proper-sequence-p (lvar-value lvar))
                                       (every #'characterp (lvar-value lvar)))
                                  (coerce (lvar-value lvar) 'string)
                                  var))))
          `(lambda (.dummy. ,@vars)
             (declare (ignore .dummy.)
                      (ignorable ,@vars))
             ,(ecase type
                ((string simple-string)
                 `(%concatenate-to-string ,@constants-to-string))
                ((base-string simple-base-string)
                 `(%concatenate-to-base-string ,@constants-to-string)))))
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
                                   `(sb-impl::string-dispatch ((simple-array * (*))
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
                                  `(sb-impl::string-dispatch
                                       (#+sb-unicode
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

(defun vector-specifier-widetag (type)
  ;; FIXME: This only accepts vectors without dimensions even though
  ;; it's not that hard to support them for the concatenate transform,
  ;; but it's probably not used often enough to bother.
  (cond ((and (array-type-p type)
              (equal (array-type-dimensions type) '(*)))
         (let* ((el-ctype (array-type-element-type type))
                (el-ctype (if (eq el-ctype *wild-type*)
                              *universal-type*
                              el-ctype))
                (saetp (find-saetp-by-ctype el-ctype)))
           (when saetp
             (sb-vm:saetp-typecode saetp))))
        ((and (union-type-p type)
              (csubtypep type (specifier-type 'string))
              (loop for type in (union-type-types type)
                    always (and (array-type-p type)
                                (equal (array-type-dimensions type) '(*)))))
         #+sb-unicode
         sb-vm:simple-character-string-widetag
         #-sb-unicode
         sb-vm:simple-base-string-widetag)))

(deftransform concatenate ((result-type &rest lvars)
                           ((constant-arg t)
                            &rest sequence)
                           * :node node)
  (let* ((type (ir1-transform-specifier-type (lvar-value result-type)))
         (vector-widetag (vector-specifier-widetag type)))
    (flet ((coerce-constants (vars type)
             ;; Lists are faster to iterate over than vectors of
             ;; unknown type.
             (loop for var in vars
                   for lvar in lvars
                   collect (if (and (constant-lvar-p lvar)
                                    (proper-sequence-p (lvar-value lvar))
                                    (not (typep (lvar-value lvar) type)))
                               `',(coerce (lvar-value lvar) type)
                               var))))

      (cond ((type= type (specifier-type 'list))
             (let ((vars (make-gensym-list (length lvars))))
               `(lambda (type ,@vars)
                  (declare (ignore type)
                           (ignorable ,@vars))
                  (%concatenate-to-list ,@(coerce-constants vars 'list)))))
            ((not vector-widetag)
             (give-up-ir1-transform))
            ((= vector-widetag sb-vm:simple-base-string-widetag)
             (string-concatenate-transform node 'simple-base-string lvars))
            #+sb-unicode
            ((= vector-widetag sb-vm:simple-character-string-widetag)
             (string-concatenate-transform node 'string lvars))
            ;; FIXME: other vectors may use inlined expansion from
            ;; STRING-CONCATENATE-TRANSFORM as well.
            (t
             (let ((vars (make-gensym-list (length lvars))))
               `(lambda (type ,@vars)
                  (declare (ignore type)
                           (ignorable ,@vars))
                  ,(if (= vector-widetag sb-vm:simple-vector-widetag)
                       `(%concatenate-to-simple-vector
                         ,@(coerce-constants vars 'vector))
                       `(%concatenate-to-vector
                         ,vector-widetag ,@(coerce-constants vars 'list))))))))))

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
                   (lvar-value-is end nil)))))
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
                (sb-impl::unreachable))
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

(defun sequence-element-type (sequence key)
  (let ((key-return-type *universal-type*))
    (when key
      (multiple-value-bind (type name) (lvar-fun-type key)
        (cond ((eq name 'identity)
               (setf key nil))
              (t
               (setf key-return-type (if (fun-type-p type)
                                         (single-value-type (fun-type-returns type))
                                         *universal-type*))
               (if (constant-fold-arg-p name)
                   (setf key name)
                   (return-from sequence-element-type key-return-type))))))
    (let ((type (sequence-elements-type sequence key)))
      (if (or (eq type *universal-type*)
              (eq type *wild-type*))
          key-return-type
          type))))

(deftransform %find-position ((item sequence from-end start end key test))
  (let* ((test (lvar-fun-is test '(eql equal equalp char-equal)))
         (test-origin test))
    (when test
      (setf test (change-test-based-on-item test (lvar-type item)))
      (unless (eq test 'eq)
        (let ((elt (sequence-element-type sequence key)))
          (setf test (change-test-based-on-item test elt))
          (when (and (eq test 'equalp)
                     (csubtypep (lvar-type item) (specifier-type 'integer))
                     (csubtypep elt (specifier-type 'integer)))
            (setf test (if (or (csubtypep (lvar-type item) (specifier-type 'fixnum))
                               (csubtypep elt (specifier-type 'fixnum)))
                           'eq
                           'eql))))))
    (if (eq test test-origin)
        (give-up-ir1-transform)
        `(%find-position item sequence from-end start end key #',test))))

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
    (let ((maybe-return
            ;; WITH-ARRAY-DATA has already performed bounds
            ;; checking, so we can safely elide the checks
            ;; in the inner loop.
            `(let ((,element (locally (declare (optimize (insert-array-bounds-checks 0)))
                               (aref ,sequence ,index))))
               (when ,done-p-expr
                 (return-from ,block
                   (values ,element
                           (- ,index ,offset)))))))
     `(let* ((,n-sequence ,sequence-arg))
        (with-array-data ((,sequence ,n-sequence :offset-var ,offset)
                          (,start ,start)
                          (,end ,end-arg)
                          :check-fill-pointer t)
          (block ,block
            (if ,from-end
                (loop for ,index
                      ;; (If we aren't fastidious about declaring that
                      ;; INDEX might be -1, then (FIND 1 #() :FROM-END T)
                      ;; can send us off into never-never land, since
                      ;; INDEX is initialized to -1.)
                      of-type index-or-minus-1
                      from (1- ,end) downto ,start
                      do
                      ,maybe-return)
                (loop for ,index of-type index from ,start below ,end
                      do
                      ,maybe-return))
            (values nil nil)))))))

(sb-xc:defmacro %find-position-vector-macro (item sequence
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

(sb-xc:defmacro %find-position-if-vector-macro (predicate sequence
                                                     from-end start end key)
  (with-unique-names (element)
    (%find-position-or-find-position-if-vector-expansion
     sequence
     from-end
     start
     end
     element
     `(funcall ,predicate (funcall ,key ,element)))))

(sb-xc:defmacro %find-position-if-not-vector-macro (predicate sequence
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
                              :node node)
  "expand inline"
  (check-inlineability-of-find-position-if sequence from-end)
  (unless
      (or (policy node (> speed space))
          ;; These have compact inline expansion
          (and (or (not key)
                   (lvar-fun-is key '(identity)))
               (and (constant-lvar-p start)
                    (eql (lvar-value start) 0))
               (and (constant-lvar-p end)
                    (null (lvar-value end)))
               (csubtypep (lvar-type sequence) (specifier-type 'simple-array))
               (let ((element-type (array-type-upgraded-element-type (lvar-type sequence)))
                     (test (lvar-fun-name* test))
                     (item (lvar-type item)))
                 (when (neq element-type *wild-type*)
                   (case (type-specifier element-type)
                     ((double-float single-float)
                      (and (csubtypep item element-type)
                           (memq test '(= eql equal equalp))))
                     ((t)
                      (eq test 'eq))
                     (character
                      (or (memq test '(eq eql equal char=))
                          (and (eq test 'char-equal)
                               (or (csubtypep item (specifier-type 'base-char))
                                   (and (constant-lvar-p sequence)
                                        (every (lambda (x) (typep x 'base-char))
                                               (lvar-value sequence)))))))
                     (base-char
                      (memq test '(eq eql equal char= char-equal)))
                     (t
                      (and (csubtypep element-type (specifier-type 'integer))
                           (csubtypep item element-type)
                           (memq test '(eq eql equal equalp =)))))))))
    (give-up-ir1-transform))
  ;; Delay to prefer the string and bit-vector transforms
  (delay-ir1-transform node :constraint)
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
    (give-up-ir1-transform "non-trivial :KEY or :TEST"))
  (block not-a-bit
    `(with-array-data ((bits sequence :offset-var offset)
                       (start start)
                       (end end)
                       :check-fill-pointer t)
       (let ((p ,(let* ((dir (cond ((not (constant-lvar-p from-end)) 0) ; unknown
                                   ((lvar-value from-end) 2) ; reverse
                                   (t 1))) ; forward
                        (from-end-arg (if (eql dir 0) '(from-end) '())))
                   (if (constant-lvar-p item)
                       (case (lvar-value item)
                         (0 `(,(elt #(%bit-position/0 %bit-pos-fwd/0 %bit-pos-rev/0) dir)
                              bits ,@from-end-arg start end))
                         (1 `(,(elt #(%bit-position/1 %bit-pos-fwd/1 %bit-pos-rev/1) dir)
                              bits ,@from-end-arg start end))
                         (otherwise (return-from not-a-bit `(values nil nil))))
                       `(,(elt #(%bit-position %bit-pos-fwd %bit-pos-rev) dir)
                         item bits ,@from-end-arg start end)))))
           (if p
               (values item (the index (- (truly-the index p) offset)))
               (values nil nil))))))

(deftransform %find-position ((item sequence from-end start end key test)
                              (character string t t t function function)
                              *
                              :policy (> speed space))
  (if (eq '* (upgraded-element-type-specifier sequence))
      (let ((form
             `(sb-impl::string-dispatch ((simple-array character (*))
                                         (simple-array base-char (*)))
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
      ;; :TEST of NIL (whether implicit or explicit) means #'EQL.
      ;; This behavior is not specified by CLHS, but is fairly conventional.
      ;; (KEY is expressly specified as allowing NIL, but TEST is not)
      ;; In our implementation, it has to be this way because we don't track
      ;; whether the :TEST and :TEST-NOT args were actually present.
      (t #'eql))))
(define-source-transform effective-find-position-key (key)
  (once-only ((key key))
    `(if ,key
         (%coerce-callable-to-fun ,key)
         #'identity)))

(defun note-perfect-hash-used (description expr)
  (declare (ignorable description))
  #+nil
  (let ((*print-pretty* t) (*print-right-margin* 200)
        (*print-lines* nil) (*print-level* nil) (*print-length* nil))
    (format t "~&;; NOTE: ~A~%-> ~A~%" description expr))
  expr)

;;; Construct a form which computes a 32-bit hash given an object in ITEM (which
;;; customarily is named literally 'ITEM) whose values should be - but might not be -
;;; one of the objects in KEYS. If it is not, the expression's result should be
;;; irrelevant. (Calling code has to do some kind of "hit" test)
;;; The 32-bit hash is then fed into a perfect hash expression.
;;; TODO:
;;; 1. There is potential for more optimization.
;;;    For example, let's say the key set includes only symbols and characters.
;;;    Clearly we have to call SYMBOLP or some variant thereof prior to dereferencing
;;;    the HASH slot of a symbol. For non-symbols, it doesn't really matter if the
;;;    item is a character, so we could use (ASH (GET-LISP-OBJ-ADDRESS OBJ) -32)
;;;    instead of doing CHARACTERP and CHAR-CODE. They come out the same, and for
;;;    non-characters it doesn't matter what the result is.
;;; 2. this should probably take a ":MISS" argument which is a block name to return
;;;    from if the key type doesn't match any of the accepted types
;;;    rather than returning 0.
(defun prehash-for-perfect-hash (item keys)
  (let (symbolp fixnump characterp)
    (dolist (key keys)
      (cond ((symbolp key) (setq symbolp t))
            ((fixnump key) (setq fixnump t))
            ((characterp key) (setq characterp t))))
    (collect ((calc))
      (when symbolp
        (if (vop-existsp :translate hash-as-if-symbol-name)
            (calc '((pointerp item)
                    (hash-as-if-symbol-name item)))
            ;; NON-NULL-SYMBOL-P is the less expensive test as it omits the OR
            ;; which accepts NIL along with OTHER-POINTER objects.
            (calc `((,(if (member nil keys) 'symbolp 'non-null-symbol-p) item)
                    (symbol-name-hash (truly-the symbol item))))))
      (when fixnump
        (calc '((fixnump item) (ldb (byte 32 0) (truly-the fixnum item)))))
      (when characterp
        (calc '((characterp item) (char-code (truly-the character item)))))
      (let ((calc `(cond ,@(calc) (t 0))))
        (if (eq item 'item) calc (subst item 'item calc))))))

;;; This tries to optimize for MEMBER directed to an IF node by not using a value vector.
;;; FIND directed to an IF is a little funny because if you find a NIL then it has to
;;; return NIL; but FIND does not use a value vector anyway, so there is nothing gained
;;; by avoiding a value vector.
;;; This can optimize out one CAR or CDR operation in CDR of ASSOC or CAR of ASSOC,
;;; but (TODO) it can't completely optimize out CADR in (CADR (ASSOC x '((:s1 val1) ...)))
;;; enough though it should be equivalent to (CAR (ASSOC x '((:s1 . val1) ...))).
(defun try-perfect-find/position-map (fun-name conditional lvar-type items from-end node)
  (declare (type (member find position member assoc rassoc) fun-name))
  ;; It's certainly not worth doing a hash calculation for 2 keys.
  ;; And it's usually not worth it for 3 keys. At least for the MEMBER operation, the code size
  ;; is not smaller using a hash, and there are still 3 conditional jumps: one to test whether
  ;; the arg is POINTERP, one to see if the perfect hash is 0..2, and one to see if there was a
  ;; hit in the key vector. Straightforwardly testing takes 3 jumps, so just do that.
  (when (< (length items) (if (eq fun-name 'member) 4 3))
    (return-from try-perfect-find/position-map))
  (let ((hashable
          ;; TODO: allow (OR CHARACTER FIXNUM) also
          (every (lambda (item)
                   (case fun-name
                     (assoc (and (listp item)
                                 (symbolp (car item))))
                     (rassoc (and (listp item)
                                  (symbolp (cdr item))))
                     (t (symbolp item))))
                 items)))
    (unless hashable
      (return-from try-perfect-find/position-map)))
  ;; alists can contain NIL which does not represent a pair at all.
  ;; (Why is such a seemingly random stipulation even part of the language?)
  (when (member fun-name '(assoc rassoc))
    (setf items (remove-if #'null items)))
  (let ((alistp) ; T if an alist, :SYNTHETIC if we avoid using conses in the mapping
        (map (make-hash-table)))
    ;; Optimize out the CDR operation in (CDR (ASSOC ...)) respectively
    ;; the CAR in (CAR (RASSOC ...)).
    ;; CADR and SECOND would have been converted as (CAR (CDR ...)
    ;; so it works for those also.
    (when (member fun-name '(assoc rassoc))
      (setq alistp t)
      (let ((expect (if (eq fun-name 'assoc) '(cdr) '(car)))
            (dest (node-dest node)))
        (when (and (combination-p dest)
                   (lvar-fun-is (combination-fun dest) expect)
                   (let* ((args (combination-args dest)) (arg (first args)))
                     (and (singleton-p args)
                          (lvar-has-single-use-p arg)
                          (eq (lvar-use arg) node))))
          (setq alistp :synthetic))))
    (cond ((vectorp items)
           (dotimes (position (length items))
             (let ((elt (svref items position)))
               ;; FROM-END will replace an entry already in MAP, as doing so exhibits
               ;; the desired behavior of using the rightmost match.
               ;; Otherwise, when *not* FROM-END, take only the leftmost occurrence.
               (when (or from-end (not (gethash elt map)))
                 (setf (gethash elt map) position)))))
          (t
           (aver (not from-end))
           (do ((list items (cdr list)))
               ((endp list))
             (ecase fun-name
               (member
                (let ((elt (car list)))
                  (unless (gethash elt map) (setf (gethash elt map) list))))
               (assoc
                (let* ((pair (car list)) (key (car pair)))
                  (unless (gethash key map)
                    (setf (gethash key map) (if (eq alistp t) pair (cdr pair))))))
               (rassoc
                (let* ((pair (car list)) (key (cdr pair)))
                  (unless (gethash key map)
                    (setf (gethash key map) (if (eq alistp t) pair (car pair))))))))))
    (flet () ; XXX: reindent from here down
      ;; Sort to avoid sensitivity to the hash-table iteration order when cross-compiling.
      ;; Not necessary for the target but not worth a #+/- either.
      ;; TODO: rather than sorting, compute KEYS from the originally-specified ITEMS after
      ;; removing duplicates. If we permit keys to be (OR CHARACTER SYMBOL FIXNUM)
      ;; there is not really a good sort order on a mixture of those, though I suppose
      ;; we could sort by the hash, since that has to be unique or the transform fails.
      (binding* ((keys (sort (loop for k being each hash-key of map collect k) #'string<))
                 (hashes (map '(simple-array (unsigned-byte 32) (*)) #'symbol-name-hash keys))
                 (n (length hashes))
                 (certainp (csubtypep lvar-type (specifier-type `(member ,@keys))))
                 (pow2size (power-of-two-ceiling n))
                 ;; FIXME: I messed up the minimal/non-minimal thing that was
                 ;; trying to simplify the calculation at the expense of a few extra cells.
                 ;; Minimal will always be right.
                 (minimal t)
                 (lambda (make-perfect-hash-lambda hashes items minimal) :exit-if-null)
                 (keyspace-size (if minimal n pow2size))
                 (domain (sb-xc:make-array keyspace-size :initial-element 0))
                 (range
                  (cond ((eq fun-name 'position)
                         (sb-xc:make-array keyspace-size
                                           :element-type
                                           (cond ((<= n #x100) '(unsigned-byte 8))
                                                 ((<= n #x10000) '(unsigned-byte 16))
                                                 (t '(unsigned-byte 32)))
                                           :initial-element 0))
                        ((or conditional (eq fun-name 'find)) nil)
                        ;; if ALISTP=T then use a single array of cons cells,
                        ;; which the user wants (or seems to)
                        ((eq alistp t) domain)
                        (t (sb-xc:make-array keyspace-size))))
                 (phashfun (sb-c::compile-perfect-hash lambda hashes)))
        (when certainp
          (when conditional
            (aver (eq fun-name 'member)) ; return whatever expression CONDITIONAL is
            (return-from try-perfect-find/position-map conditional))
          (when (eq fun-name 'find) ; nothing to do. Wasted some time, no big deal
            (return-from try-perfect-find/position-map 'item))) ; transform arg is always named ITEM
        (maphash (lambda (key val &aux (phash (funcall phashfun (symbol-name-hash key))))
                   (cond ((eq alistp t)
                          (setf (aref domain phash) val)) ; VAL is the (key . val) pair
                         (t
                          (setf (svref domain phash) key)
                          (when range (setf (aref range phash) val)))))
                 map)
        (when (eq alistp :synthetic)
          (let* ((car/cdr (node-dest node))
                 (fun (lvar-use (combination-fun car/cdr))))
            (aver (ref-p fun))
            (when (every #'fixnump range)
              (setq range (coerce-to-smallest-eltype range)))
            (change-ref-leaf fun (find-free-fun 'values "?") :recklessly t)
            (setf (combination-fun-info car/cdr) (info :function :info 'values))
            ;; This is cargo-culted from a related transform on MEMBER where we cause it to
            ;; return a value that is not based on the input list directly.
            (derive-node-type node (specifier-type 't) :from-scratch t)
            (reoptimize-node car/cdr)))
        ;; TRULY-THE around PHASH is warranted when CERTAINP because while the compiler can
        ;; derive the type of the final LOGAND, it's a complete mystery to it that the range
        ;; of the perfect hash is smaller than 2^N.
        (note-perfect-hash-used
         `(,fun-name ,conditional ,items)
         `(let* ((hash ,(prehash-for-perfect-hash 'item keys))
                 (phash (,lambda hash)))
            ,(if certainp
                 `(aref ,range (truly-the (mod ,n) phash))
                 (let* ((key-expr (if (eq alistp t)
                                      `(,(if (eq fun-name 'assoc) 'car 'cdr) key)
                                      'key))
                        (expr `(let ((key (svref ,domain phash)))
                                 (if (eq ,key-expr item)
                                     ,(cond (conditional) ; return whatever this expression is
                                            ((eq fun-name 'find) 'item)
                                            ((eq range domain)
                                             'key)
                                            (t
                                             `(aref ,range phash)))))))
                   ;; An unexpected symbol-hash fed into a minimal perfect hash function
                   ;; can produce garbage out, so we have to bounds-check it.
                   ;; Otherwise, with a non-minimal hash function, the table size is
                   ;; exactly right for the number of bits of output of the function
                   (if minimal
                       `(if (< phash ,n)
                            ,expr)
                       expr)))))))))

(macrolet ((define-find-position (fun-name values-index)
             `(deftransform ,fun-name ((item sequence &key
                                             from-end (start 0) end
                                             key test test-not)
                                       (t (or list vector) &rest t)
                                       * :node node)
                (when (and (constant-lvar-p sequence)
                           (or (proper-sequence-p (lvar-value sequence))
                               (give-up-ir1-transform))
                           (zerop (length (lvar-value sequence))))
                  (if (and test test-not)
                      ;; even though one kwd arg could legit be NIL, it's not interesting.
                      (give-up-ir1-transform)
                      (return-from ,fun-name
                        '(lambda (&rest args) (declare (ignore args)) nil))))
                (let ((effective-test
                        (unless test-not
                          (if test (lvar-fun-name* test) 'eql)))
                      (test-form '(effective-find-position-test test test-not))
                      (const-seq (when (constant-lvar-p sequence)
                                   (lvar-value sequence))))
                  ;; Destructive modification of constants is illegal.
                  ;; Therefore if this sequence would have been output as a code header
                  ;; constant, its contents can't change. We don't need to reference
                  ;; the sequence itself to compare elements.
                  ;; There are two transforms to try in this situation:
                  ;; 1) If we can make a perfect map of N symbols, then do that. No upper bound
                  ;;    on N. This could be enhanced to take fixnums and characters- any objects for
                  ;;    which the hash values are computable at compile-time.
                  ;; 2) Otherwise, use COND, not to exceed some length limit.
                  (when (and const-seq
                             (member effective-test '(eql eq char= char-equal))
                             (not start) (not end) (not key)
                             (or (not from-end) (constant-lvar-p from-end)))
                    (let* ((items (coerce const-seq 'simple-vector))
                           ;; It seems silly to use :from-end and a constant list
                           ;; in a way where it actually matters (with repeated elements),
                           ;; but we either have to do it right or not do it.
                           (reversedp (and from-end (lvar-value from-end)))
                           (or-eq-transform-p (and (memq effective-test '(eql eq char=))
                                                   (or-eq-transform-p items))))
                      (awhen (and (memq effective-test '(eql eq))
                                  (not or-eq-transform-p)
                                  (slow-findhash-allowed node)
                                  (try-perfect-find/position-map
                                   ',fun-name nil (lvar-type item) items reversedp nil))
                        (return-from ,fun-name
                          `(lambda (item sequence &rest rest)
                             (declare (ignore sequence rest))
                             ,it)))
                      (when (or or-eq-transform-p
                                (<= (length items) 10))
                        (let ((clauses (loop for x across items for i from 0
                                             ;; Later transforms will change EQL to EQ if appropriate.
                                             collect `((,effective-test item ',x)
                                                       ,(ecase ',fun-name
                                                          (position i)
                                                          (find
                                                           (cond
                                                             ((memq effective-test '(eq char=))
                                                              'item)
                                                             ((and (eq effective-test 'eql)
                                                                   (sb-xc:typep x 'eq-comparable-type))
                                                              'item)
                                                             ((and (eq effective-test 'char-equal)
                                                                   (if (characterp x)
                                                                       (not (both-case-p x))
                                                                       (give-up-ir1-transform)))
                                                              'item)
                                                             (t
                                                              `',x))))))))
                          (return-from ,fun-name
                            `(lambda (item sequence &rest rest)
                               (declare (ignore sequence rest))
                               (cond ,@(if reversedp (nreverse clauses) clauses))))))))
                  `(nth-value ,',values-index
                              (%find-position item sequence
                                              from-end start
                                              end
                                              (effective-find-position-key key)
                                              ,test-form))))))
  (define-find-position find 0)
  (define-find-position position 1))

;;; Lower :test
(macrolet ((def (fun-name)
             `(deftransform ,fun-name ((item sequence &key
                                             from-end start end
                                             key test test-not)
                                       (t  &rest t))
                (macrolet ((maybe-arg (arg &optional (key (keywordicate arg)))
                             `(and ,arg `(,,key ,',arg))))
                  (let ((test (and (not test-not)
                                   (change-test-lvar-based-on-item test item))))
                    (if test
                        `(,',fun-name item sequence :test ',test
                                      ,@(maybe-arg from-end)
                                      ,@(maybe-arg start)
                                      ,@(maybe-arg end)
                                      ,@(maybe-arg key)
                                      ,@(maybe-arg test-not))
                        (give-up-ir1-transform)))))))
  (def find)
  (def position))

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



;;; Pop anonymous constant values from the end, list/list* them if
;;; any, and link the remainder with list* at runtime. We don't try to
;;; fold named constant references, because while theoretically
;;; possible, in addition to needing to make a load form for a
;;; structure recording the constant name which wraps the constant
;;; value, the dumper would have to learn how to patch constant values
;;; into list structure, to deal with the load form potentially being
;;; evaluated for value earlier than the constant definition is
;;; loaded.
(defun transform-backq-list-or-list* (function values)
  (let ((gensyms (make-gensym-list (length values)))
        (reverse (reverse values))
        (constants '()))
    (loop while (and reverse
                     (constant-lvar-p (car reverse))
                     (legal-immediate-constant-p
                      (nth-value 1 (lvar-value (car reverse)))))
          do (push (lvar-value (pop reverse)) constants))
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

(deftransform sb-impl::|List| ((&rest elts))
  (transform-backq-list-or-list* 'list elts))

(deftransform sb-impl::|List*| ((&rest elts))
  (transform-backq-list-or-list* 'list* elts))

(deftransform sb-impl::|Vector| ((&rest elts))
  (let ((gensyms (make-gensym-list (length elts)))
        constants)
    ;; There's not much that can be done with semi-constant vectors-
    ;; either we're going to call VECTOR at compile-time or runtime.
    ;; There's little point to building up intermediate lists in the partially
    ;; constant case. There are ways to expand using MULTIPLE-VALUE-CALL that
    ;; might avoid consing intermediate lists if ,@ is involved
    ;; though I doubt it would provide benefit to many real-world scenarios.
    (dolist (elt elts)
      (cond ((and (constant-lvar-p elt)
                  (legal-immediate-constant-p (nth-value 1 (lvar-value elt))))
             (push (lvar-value elt) constants))
            (t
             (setq constants :fail)
             (return))))
    `(lambda ,gensyms
       ,@(cond ((listp constants)
                `((declare (ignore ,@gensyms))
                  ,(apply 'vector (nreverse constants))))
               (t
                `((vector ,@gensyms)))))))

;; Merge adjacent constant values
(deftransform sb-impl::|Append| ((&rest elts))
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

(deftransform reverse ((sequence) (vector) * :important nil)
  `(sb-impl::vector-reverse sequence))

(deftransform reverse ((sequence) (list) * :important nil)
  `(sb-impl::list-reverse sequence))

(deftransform nreverse ((sequence) (vector) * :important nil)
  `(sb-impl::vector-nreverse sequence))

(deftransform nreverse ((sequence) (list) * :important nil)
  `(sb-impl::list-nreverse sequence))

(deftransforms (intersection nintersection)
    ((list1 list2 &key key test test-not))
  (let ((null-type (specifier-type 'null)))
    (cond ((or (csubtypep (lvar-type list1) null-type)
               (csubtypep (lvar-type list2) null-type))
           nil)
          ((and (same-leaf-ref-p list1 list2)
                (not test-not)
                (not key)
                (or (not test)
                    (lvar-fun-is test '(eq eql equal equalp))))
           'list1)
          (t
           (give-up-ir1-transform)))))

(deftransform nunion ((list1 list2 &key key test test-not))
  (let ((null-type (specifier-type 'null)))
    (cond ((csubtypep (lvar-type list1) null-type)
           'list2)
          ((csubtypep (lvar-type list2) null-type)
           'list1)
          ((and (same-leaf-ref-p list1 list2)
                (not test-not)
                (not key)
                (or (not test)
                    (lvar-fun-is test '(eq eql equal equalp))))
           'list1)
          (t
           (give-up-ir1-transform)))))

(deftransform union ((list1 list2 &key key test test-not))
  (let ((null-type (specifier-type 'null)))
    (flet ((to-adjoin (a b)
             (when (constant-lvar-p a)
               (let ((value (lvar-value a)))
                 (when (typep value '(cons * null))
                   `(adjoin ',(car value) ,b
                            ,@(and test '(:test test))
                            ,@(and test-not '(:test-not test-not))))))))
      (cond ((csubtypep (lvar-type list1) null-type)
             'list2)
            ((csubtypep (lvar-type list2) null-type)
             'list1)
            ((and (same-leaf-ref-p list1 list2)
                  (not test-not)
                  (not key)
                  (or (not test)
                      (lvar-fun-is test '(eq eql equal equalp))))
             'list1)
            ((and (not key)
                  (or (to-adjoin list1 'list2)
                      (to-adjoin list2 'list1))))
            (t
             (give-up-ir1-transform))))))

(defoptimizer (union derive-type) ((list1 list2 &rest args))
  (let ((cons-type (specifier-type 'cons)))
    (if (or (csubtypep (lvar-type list1) cons-type)
            (csubtypep (lvar-type list2) cons-type))
        cons-type
        (specifier-type 'list))))

(defoptimizer (nunion derive-type) ((list1 list2 &rest args))
  (let ((cons-type (specifier-type 'cons)))
    (if (or (csubtypep (lvar-type list1) cons-type)
            (csubtypep (lvar-type list2) cons-type))
        cons-type
        (specifier-type 'list))))

(deftransforms (set-difference nset-difference)
    ((list1 list2 &key key test test-not))
  (let ((null-type (specifier-type 'null)))
    (cond ((csubtypep (lvar-type list1) null-type)
           nil)
          ((csubtypep (lvar-type list2) null-type)
           'list1)
          ((and (same-leaf-ref-p list1 list2)
                (not test-not)
                (not key)
                (or (not test)
                    (lvar-fun-is test '(eq eql equal equalp))))
           nil)
          (t
           (give-up-ir1-transform)))))

(deftransform subsetp ((list1 list2 &key key test test-not))
  (cond ((csubtypep (lvar-type list1) (specifier-type 'null))
         t)
        ((and (same-leaf-ref-p list1 list2)
              (not test-not)
              (not key)
              (or (not test)
                  (lvar-fun-is test '(eq eql equal equalp))))
         t)
        (t
         (give-up-ir1-transform))))

(deftransforms (set-exclusive-or nset-exclusive-or)
    ((list1 list2 &key key test test-not))
  (let ((null-type (specifier-type 'null)))
    (cond ((csubtypep (lvar-type list1) null-type)
           'list2)
          ((csubtypep (lvar-type list2) null-type)
           'list1)
          ((and (same-leaf-ref-p list1 list2)
                (not test-not)
                (not key)
                (or (not test)
                    (lvar-fun-is test '(eq eql equal equalp))))
           'list1)
          (t
           (give-up-ir1-transform)))))

(deftransform tree-equal ((list1 list2 &key test test-not))
  (cond ((and (same-leaf-ref-p list1 list2)
              (not test-not)
              (or (not test)
                  (lvar-fun-is test '(eq eql equal equalp))))
         t)
        ((and (not test-not)
              (or (not test)
                  (lvar-fun-is test '(eql))))
         `(sb-impl::tree-equal-eql list1 list2))
        (t
         (give-up-ir1-transform))))

(defun vector-type-length (type)
  (catch 'give-up-ir1-transform
    (return-from
     vector-type-length
      (let* ((dim (array-type-dimensions-or-give-up type)))
        (when (and (typep dim '(cons integer null))
                   (not (conservative-array-type-complexp type)))
          (first dim)))))
  nil)

(defun vector-type-lengths (type)
  (if (union-type-p type)
      (loop with lengths
            for type in (union-type-types type)
            do (pushnew (or (vector-type-length type)
                            (return))
                        lengths)
            finally (return lengths))
      (let ((length (vector-type-length type)))
        (when length
          (list length)))))

(defoptimizer (reduce derive-type) ((fun sequence
                                         &key
                                         initial-value
                                         key
                                         start
                                         end
                                         &allow-other-keys))
  (multiple-value-bind (fun-type name) (lvar-fun-type fun t t)
    (when (fun-type-p fun-type)
      (let* ((initial-value-type (and initial-value
                                      (lvar-type initial-value)))
             (sequence-type (lvar-type sequence))
             (element-type
               (cond ((and key
                           (multiple-value-bind (key-type name) (lvar-fun-type key t t)
                             (cond ((eq name 'identity)
                                    nil)
                                   ((fun-type-p key-type)
                                    (single-value-type (fun-type-returns key-type)))
                                   (t
                                    *universal-type*)))))
                     ((csubtypep sequence-type (specifier-type 'array))
                      (let ((upgraded-type
                              (array-type-upgraded-element-type sequence-type)))
                        (if (eq upgraded-type *wild-type*)
                            *universal-type*
                            upgraded-type)))))
             (end (if end
                      (and (constant-lvar-p end)
                           (or (lvar-value end)
                               (vector-type-length sequence-type)))
                      (vector-type-length sequence-type)))
             (start (if start
                        (and (constant-lvar-p start)
                             (lvar-value start))
                        0))
             (length (and start end
                          (- end start))))
        ;; Calling the type deriver would be more universal, but
        ;; type derivers expect a combination, but even then there's
        ;; not a lot of standard functions which are usually used
        ;; with REDUCE and which benefit from improved type
        ;; derivation.
        (or
         (when (and (eq name '+)
                    element-type
                    (neq element-type *wild-type*)
                    (neq element-type *universal-type*))
           (let* ((non-empty (typep length '(integer 1)))
                  (identity-p (and (not initial-value)
                                   (not non-empty))))
             (labels ((try (type)
                        (let ((type (specifier-type type)))
                          (when (csubtypep element-type type)
                            (cond (identity-p
                                   (type-union type
                                               (specifier-type '(eql 0))))
                                  (initial-value
                                   (let ((contagion (numeric-contagion type initial-value-type
                                                                       :rational nil
                                                                       :unsigned t)))
                                     (if non-empty
                                         contagion
                                         (type-union contagion initial-value-type))))
                                  (t
                                   type))))))
               (some #'try '(double-float single-float float unsigned-byte integer rational real)))))
         (let ((fun-result (single-value-type (fun-type-returns fun-type))))
           (cond (initial-value-type
                  (type-union initial-value-type fun-result))
                 ((typep length '(integer 2))
                  fun-result)
                 (element-type
                  (type-union fun-result element-type)))))))))

(defoptimizer (nth derive-type) ((n list))
  (when (constant-lvar-p list)
    (let* ((list (lvar-value list))
           (rest list)
           type
           (seen (list list)))
      (loop for element = (pop rest)
            do (setf type
                     (if type
                         (type-union (ctype-of element) type)
                         (ctype-of element)))
            until (or (memq rest seen)
                      (atom rest))
            do (push rest seen)
            finally (unless (or rest
                                (let ((n-int (type-approximate-interval (lvar-type n))))
                                  (and n-int
                                       (interval<n n-int (length list)))))
                      (setf type (type-union (specifier-type 'null) type))))
            type)))

(defoptimizer (car constraint-propagate-if) ((list))
  (values list (specifier-type 'cons) nil nil t))

(setf (fun-info-constraint-propagate-if (fun-info-or-lose 'cdr))
      #'car-constraint-propagate-if-optimizer)
