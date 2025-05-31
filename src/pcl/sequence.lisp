;;;; Extensible sequences, based on the proposal by Christophe Rhodes.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(in-package "SB-IMPL")

;;;; basic protocol
(define-condition sequence:protocol-unimplemented (type-error
                                                   reference-condition)
  ((operation :initarg :operation
              :reader sequence:protocol-unimplemented-operation))
  (:default-initargs
   :operation (missing-arg)
   :references '((:sbcl :node "Extensible Sequences")))
  (:report
   (lambda (condition stream)
     (let ((operation (sequence::protocol-unimplemented-operation condition))
           (datum (type-error-datum condition)))
       (format stream "~@<The operation ~
                       ~/sb-ext:print-symbol-with-prefix/ is not ~
                       implemented for ~A which is an instance of the ~
                       ~/sb-ext:print-symbol-with-prefix/ subclass ~
                       ~S.~@:>"
               operation datum 'sequence (class-of datum)))))
  (:documentation
   "This error is signaled if a sequence operation is applied to an
   instance of a sequence class that does not support the
   operation."))

(defun sequence:protocol-unimplemented (operation sequence)
  (error 'sequence:protocol-unimplemented
         :datum sequence
         :expected-type '(or list vector)
         :operation operation))

(defgeneric sequence:emptyp (sequence)
  (:method ((s list)) (null s))
  (:method ((s vector)) (zerop (length s)))
  (:method ((s sequence)) (zerop (length s)))
  (:documentation
   "Returns T if SEQUENCE is an empty sequence and NIL
   otherwise. Signals an error if SEQUENCE is not a sequence."))

(defgeneric sequence:length (sequence)
  (:method ((s list)) (length s))
  (:method ((s vector)) (length s))
  (:method ((s sequence))
    (sequence:protocol-unimplemented 'sequence:length s))
  (:documentation
   "Returns the length of SEQUENCE or signals a PROTOCOL-UNIMPLEMENTED
   error if the sequence protocol is not implemented for the class of
   SEQUENCE."))

(defgeneric sequence:elt (sequence index)
  (:method ((s list) index) (elt s index))
  (:method ((s vector) index) (elt s index))
  (:method ((s sequence) index)
    (sequence:protocol-unimplemented 'sequence:elt s))
  (:documentation
   "Returns the element at position INDEX of SEQUENCE or signals a
   PROTOCOL-UNIMPLEMENTED error if the sequence protocol is not
   implemented for the class of SEQUENCE."))

(defgeneric (setf sequence:elt) (new-value sequence index)
  (:argument-precedence-order sequence new-value index)
  (:method (new-value (s list) index) (setf (elt s index) new-value))
  (:method (new-value (s vector) index) (setf (elt s index) new-value))
  (:method (new-value (s sequence) index)
    (sequence:protocol-unimplemented '(setf sequence:elt) s))
  (:documentation
   "Replaces the element at position INDEX of SEQUENCE with NEW-VALUE
   and returns NEW-VALUE or signals a PROTOCOL-UNIMPLEMENTED error if
   the sequence protocol is not implemented for the class of
   SEQUENCE."))

(defgeneric sequence:make-sequence-like
    (sequence length &key initial-element initial-contents)
  (:method ((s list) length &key
            (initial-element nil iep) (initial-contents nil icp))
    (cond
      ((and icp iep) (error "supplied both ~S and ~S to ~S" :initial-element :initial-contents 'make-sequence-like))
      (iep (make-list length :initial-element initial-element))
      (icp (unless (= (length initial-contents) length)
             (error "length mismatch in ~S" 'make-sequence-like))
           (let ((result (make-list length)))
             (replace result initial-contents)
             result))
      (t (make-list length))))
  (:method ((s vector) length &key
            (initial-element nil iep) (initial-contents nil icp))
    (cond
      ((and icp iep) (error "supplied both ~S and ~S to ~S" :initial-element :initial-contents 'make-sequence-like))
      (iep (make-array length :element-type (array-element-type s)
                       :initial-element initial-element))
      (icp (make-array length :element-type (array-element-type s)
                       :initial-contents initial-contents))
      (t (make-array length :element-type (array-element-type s)))))
  (:method ((s sequence) length &key initial-element initial-contents)
    (declare (ignore initial-element initial-contents))
    (sequence:protocol-unimplemented 'sequence:make-sequence-like s))
  (:documentation
   "Returns a freshly allocated sequence of length LENGTH and of the
   same class as SEQUENCE. Elements of the new sequence are
   initialized to INITIAL-ELEMENT, if supplied, initialized to
   INITIAL-CONTENTS if supplied, or undefined if neither is supplied.
   Signals a PROTOCOL-UNIMPLEMENTED error if the sequence protocol is
   not implemented for the class of SEQUENCE."))

(defgeneric sequence:adjust-sequence
    (sequence length &key initial-element initial-contents)
  (:method ((s list) length &key initial-element (initial-contents nil icp))
    (if (eql length 0)
        nil
        (let ((olength (length s)))
          (cond
            ((eql length olength) (if icp (replace s initial-contents) s))
            ((< length olength)
             (rplacd (nthcdr (1- length) s) nil)
             (if icp (replace s initial-contents) s))
            ((null s)
             (let ((return (make-list length :initial-element initial-element)))
               (if icp (replace return initial-contents) return)))
            (t (rplacd (nthcdr (1- olength) s)
                       (make-list (- length olength)
                                  :initial-element initial-element))
               (if icp (replace s initial-contents) s))))))
  (:method ((s vector) length &rest args &key (initial-contents nil icp) initial-element)
    (declare (ignore initial-element))
    (cond
      ((and (array-has-fill-pointer-p s)
            (>= (array-total-size s) length))
       (setf (fill-pointer s) length)
       (if icp (replace s initial-contents) s))
      ((eql (length s) length)
       (if icp (replace s initial-contents) s))
      (t (apply #'adjust-array s length args))))
  (:method ((s sequence) length &rest args)
    (declare (ignore args))
    (sequence:protocol-unimplemented 'sequence:adjust-sequence s))
  (:documentation
   "Returns destructively modified SEQUENCE or a freshly allocated
   sequence of the same class as SEQUENCE of length LENGTH. Elements
   of the returned sequence are initialized to INITIAL-ELEMENT, if
   supplied, initialized to INITIAL-CONTENTS if supplied, or identical
   to the elements of SEQUENCE if neither is supplied. Signals a
   PROTOCOL-UNIMPLEMENTED error if the sequence protocol is not
   implemented for the class of SEQUENCE."))


;;;; iterator protocol

;;; The general protocol

(defgeneric sequence:make-sequence-iterator (sequence &key from-end start end)
  (:method ((s vector) &key from-end (start 0) end)
    (make-vector-iterator s from-end start end))
  (:method ((s list) &key from-end (start 0) end)
    (make-list-iterator s from-end start end))
  (:method ((s sequence) &key from-end (start 0) end)
    (multiple-value-bind (iterator limit from-end)
        (sequence:make-simple-sequence-iterator
         s :from-end from-end :start start :end end)
      (values iterator limit from-end
              #'sequence:iterator-step #'sequence:iterator-endp
              #'sequence:iterator-element #'(setf sequence:iterator-element)
              #'sequence:iterator-index #'sequence:iterator-copy)))
  (:method ((s t) &key from-end start end)
    (declare (ignore from-end start end))
    (error 'type-error
           :datum s
           :expected-type 'sequence))
  (:documentation
   "Returns a sequence iterator for SEQUENCE or, if START and/or END
   are supplied, the subsequence bounded by START and END as nine
   values:

   1. iterator state
   2. limit
   3. from-end
   4. step function
   5. endp function
   6. element function
   7. setf element function
   8. index function
   9. copy state function

   If FROM-END is NIL, the constructed iterator visits the specified
   elements in the order in which they appear in SEQUENCE. Otherwise,
   the elements are visited in the opposite order."))

;;; magic termination value for list :from-end t
(define-load-time-global *exhausted* (cons nil nil))

(defun make-list-iterator (list from-end start end)
  (multiple-value-bind (iterator limit from-end)
      (if from-end
          (let* ((termination (if (= start 0) *exhausted* (nthcdr (1- start) list)))
                 (init (if (<= (or end (length list)) start)
                           termination
                           (if end (last list (- (length list) (1- end))) (last list)))))
            (values init termination t))
          (cond
            ((not end) (values (nthcdr start list) nil nil))
            (t (let ((st (nthcdr start list)))
                 (values st (nthcdr (- end start) st) nil)))))
    (values iterator limit from-end
            (if from-end
                (lambda (list iterator from-end)
                  (declare (ignore from-end))
                  (if (eq iterator list)
                      *exhausted*
                      (do* ((cdr list (cdr cdr)))
                           ((eq (cdr cdr) iterator) cdr))))
                (lambda (list iterator from-end)
                  (declare (ignore list from-end))
                  (cdr iterator)))
            (lambda (list iterator limit from-end)
              (declare (ignore list from-end))
              (eq iterator limit))
            (lambda (list iterator)
              (declare (ignore list))
              (car iterator))
            (lambda (new-value list iterator)
              (declare (ignore list))
              (setf (car iterator) new-value))
            (lambda (list iterator)
              (loop for cdr on list
                    for i from 0
                    when (eq cdr iterator)
                    return i))
            (lambda (list iterator)
              (declare (ignore list))
              iterator))))

(defun make-vector-iterator (vector from-end start end)
  (let* ((end (or end (length vector)))
         (iterator (if from-end
                       (1- end)
                       start))
         (limit (if from-end
                    (1- start)
                    end)))
    (values iterator limit from-end
            (if from-end
                (lambda (sequence iterator from-end)
                  (declare (ignore sequence from-end))
                  (1- iterator))
                (lambda (sequence iterator from-end)
                  (declare (ignore sequence from-end))
                  (1+ iterator)))
            (lambda (sequence iterator limit from-end)
              (declare (ignore sequence from-end))
              (= iterator limit))
            (lambda (sequence iterator)
              (aref sequence iterator))
            (lambda (new-value sequence iterator)
              (setf (aref sequence iterator) new-value))
            (lambda (sequence iterator)
              (declare (ignore sequence))
              iterator)
            (lambda (sequence iterator)
              (declare (ignore sequence))
              iterator))))

;;; the simple protocol: the simple iterator returns three values,
;;; STATE, LIMIT and FROM-END.
(defgeneric sequence:make-simple-sequence-iterator
    (sequence &key from-end start end)
  (:method ((s list) &key from-end (start 0) end)
    (if from-end
        (let* ((termination (if (= start 0) *exhausted* (nthcdr (1- start) s)))
               (init (if (<= (or end (length s)) start)
                         termination
                         (if end (last s (- (length s) (1- end))) (last s)))))
          (values init termination t))
        (cond
          ((not end) (values (nthcdr start s) nil nil))
          (t (let ((st (nthcdr start s)))
               (values st (nthcdr (- end start) st) nil))))))
  (:method ((s vector) &key from-end (start 0) end)
    (let ((end (or end (length s))))
      (if from-end
          (values (1- end) (1- start) t)
          (values start end nil))))
  (:method ((s sequence) &key from-end (start 0) end)
    (let ((end (or end (length s))))
      (if from-end
          (values (1- end) (1- start) from-end)
          (values start end nil))))
  (:documentation
   "Returns a sequence iterator for SEQUENCE, START, END and FROM-END
   as three values:

   1. iterator state
   2. limit
   3. from-end

   The returned iterator can be used with the generic iterator
   functions ITERATOR-STEP, ITERATOR-ENDP, ITERATOR-ELEMENT, (SETF
   ITERATOR-ELEMENT), ITERATOR-INDEX and ITERATOR-COPY."))

(defgeneric sequence:iterator-step (sequence iterator from-end)
  (:method ((s list) iterator from-end)
    (if from-end
        (if (eq iterator s)
            *exhausted*
            (do* ((xs s (cdr xs)))
                 ((eq (cdr xs) iterator) xs)))
        (cdr iterator)))
  (:method ((s vector) iterator from-end)
    (if from-end
        (1- iterator)
        (1+ iterator)))
  (:method ((s sequence) iterator from-end)
    (if from-end
        (1- iterator)
        (1+ iterator)))
  (:documentation
   "Moves ITERATOR one position forward or backward in SEQUENCE
   depending on the iteration direction encoded in FROM-END."))

(defgeneric sequence:iterator-endp (sequence iterator limit from-end)
  (:method ((s list) iterator limit from-end)
    (eq iterator limit))
  (:method ((s vector) iterator limit from-end)
    (= iterator limit))
  (:method ((s sequence) iterator limit from-end)
    (= iterator limit))
  (:documentation
   "Returns non-NIL when ITERATOR has reached LIMIT (which may
   correspond to the end of SEQUENCE) with respect to the iteration
   direction encoded in FROM-END."))

(defgeneric sequence:iterator-element (sequence iterator)
  (:method ((s list) iterator)
    (car iterator))
  (:method ((s vector) iterator)
    (aref s iterator))
  (:method ((s sequence) iterator)
    (sequence:elt s iterator))
  (:documentation
   "Returns the element of SEQUENCE associated to the position of
   ITERATOR."))

(defgeneric (setf sequence:iterator-element) (new-value sequence iterator)
  (:method (o (s list) iterator)
    (setf (car iterator) o))
  (:method (o (s vector) iterator)
    (setf (aref s iterator) o))
  (:method (o (s sequence) iterator)
    (setf (sequence:elt s iterator) o))
  (:documentation
   "Destructively modifies SEQUENCE by replacing the sequence element
   associated to position of ITERATOR with NEW-VALUE."))

(defgeneric sequence:iterator-index (sequence iterator)
  (:method ((s list) iterator)
    ;; FIXME: this sucks.  (In my defence, it is the equivalent of the
    ;; Apple implementation in Dylan...)
    (loop for l on s for i from 0 when (eq l iterator) return i))
  (:method ((s vector) iterator) iterator)
  (:method ((s sequence) iterator) iterator)
  (:documentation
   "Returns the position of ITERATOR in SEQUENCE."))

(defgeneric sequence:iterator-copy (sequence iterator)
  (:method ((s list) iterator) iterator)
  (:method ((s vector) iterator) iterator)
  (:method ((s sequence) iterator) iterator)
  (:documentation
   "Returns a copy of ITERATOR which also traverses SEQUENCE but can
   be mutated independently of ITERATOR."))

(defun %make-sequence-iterator (sequence from-end start end)
  (typecase sequence
    (vector
     (make-vector-iterator sequence from-end start end))
    (list
     (make-list-iterator sequence from-end start end))
    (t
     (sequence:make-sequence-iterator sequence :end end :start start
                                               :from-end from-end))))

(defmacro sequence:with-sequence-iterator
    ((&whole vars
      &optional iterator limit from-end-p
                step endp element set-element index copy)
     (sequence &key from-end (start 0) end) &body body)
  "Executes BODY with the elements of VARS bound to the iteration
  state returned by MAKE-SEQUENCE-ITERATOR for SEQUENCE and
  ARGS. Elements of VARS may be NIL in which case the corresponding
  value returned by MAKE-SEQUENCE-ITERATOR is ignored."
  (declare (ignore iterator limit from-end-p
                   step endp element set-element index copy))
  (let* ((ignored '())
         (vars (mapcar (lambda (x)
                         (or x (let ((name (gensym)))
                                 (push name ignored)
                                 name)))
                       vars)))
    `(multiple-value-bind (,@vars)
         (%make-sequence-iterator ,sequence ,from-end ,start ,end)
       (declare (type function ,@(nthcdr 3 vars))
                (ignore ,@ignored))
       ,@body)))

(defmacro sequence:with-sequence-iterator-functions
    ((&optional step endp elt setf index copy)
     (sequence &rest args &key from-end start end)
     &body body)
  "Executes BODY with the names STEP, ENDP, ELT, SETF, INDEX and COPY
bound to local functions which execute the iteration state query and
mutation functions returned by MAKE-SEQUENCE-ITERATOR for SEQUENCE and
ARGS. When some names are not supplied or NIL is supplied for a given
name, no local functions are established for those names. The
functions established for STEP, ENDP, ELT, SETF, INDEX and COPY have
dynamic extent."
  (declare (ignore from-end start end))
  (let ((nstate (gensym "STATE")) (nlimit (gensym "LIMIT"))
        (nfrom-end (gensym "FROM-END-")) (nstep (gensym "STEP"))
        (nendp (gensym "ENDP")) (nelt (gensym "ELT"))
        (nsetf (gensym "SETF")) (nindex (gensym "INDEX"))
        (ncopy (gensym "COPY")))
    (flet ((binding (name lambda-list body)
             (when name
               `((,name ,lambda-list ,body)))))
     `(sequence:with-sequence-iterator
          (,nstate ,nlimit ,nfrom-end ,nstep ,nendp ,nelt ,nsetf ,nindex ,ncopy)
          (,sequence,@args)
        (declare (ignorable ,nstate ,nlimit ,nfrom-end ,nstep ,nendp ,nelt
                            ,nsetf ,nindex ,ncopy))
        (flet (,@(binding step '() `(setq ,nstate (funcall ,nstep ,sequence,nstate ,nfrom-end)))
               ,@(binding endp '() `(funcall ,nendp ,sequence,nstate ,nlimit ,nfrom-end))
               ,@(binding elt '() `(funcall ,nelt ,sequence,nstate))
               ,@(binding setf '(new-value) `(funcall ,nsetf new-value ,sequence ,nstate))
               ,@(binding index '() `(funcall ,nindex ,sequence,nstate))
               ,@(binding copy '() `(funcall ,ncopy ,sequence,nstate)))
          ,@body)))))

(defun sequence:canonize-test (test test-not)
  (cond
    (test (if (functionp test) test (fdefinition test)))
    (test-not (if (functionp test-not)
                  (complement test-not)
                  (complement (fdefinition test-not))))
    (t #'eql)))

(defun sequence:canonize-key (key)
  (or (and key (if (functionp key) key (fdefinition key))) #'identity))

;;;; LOOP support.  (DOSEQUENCE support is present in the core SBCL
;;;; code).
(defun sb-loop::loop-elements-iteration-path (variable data-type prep-phrases)
  (let (of-phrase)
    (loop for (prep . rest) in prep-phrases do
          (ecase prep
            ((:of :in) (if of-phrase
                           (sb-loop::loop-error "Too many prepositions")
                           (setq of-phrase rest)))))
    (let ((it (gensym "ITER"))
          (lim (gensym "LIMIT"))
          (f-e (gensym "FROM-END"))
          (step (gensym "STEP"))
          (endp (gensym "ENDP"))
          (elt (gensym "ELT"))
          (seq (gensym "SEQ")))
      (push `(let ((,seq ,(car of-phrase)))) (sb-loop::wrappers sb-loop::*loop*))
      (push `(sequence:with-sequence-iterator (,it ,lim ,f-e ,step ,endp ,elt) (,seq))
            (sb-loop::wrappers sb-loop::*loop*))
      `(((,variable nil ,data-type)) () () nil (funcall ,endp ,seq ,it ,lim ,f-e)
        (,variable (funcall ,elt ,seq ,it) ,it (funcall ,step ,seq ,it ,f-e))))))

;;;; generic implementations for sequence functions.

(defgeneric sequence:map (result-prototype function sequence &rest sequences)
  (:documentation
   "Implements CL:MAP for extended sequences.

    RESULT-PROTOTYPE corresponds to the RESULT-TYPE of CL:MAP but
    receives a prototype instance of an extended sequence class
    instead of a type specifier. By dispatching on RESULT-PROTOTYPE,
    methods on this generic function specify how extended sequence
    classes act when they are specified as the result type in a CL:MAP
    call. RESULT-PROTOTYPE may not be fully initialized and thus
    should only be used for dispatch and to determine its class.

    Another difference to CL:MAP is that FUNCTION is a function, not a
    function designator."))

(defmethod sequence:map ((result-prototype sequence) (function function)
                         (sequence sequence) &rest sequences)
  (let ((sequences (list* sequence sequences))
        (min-length 0))
    (declare (dynamic-extent sequences))
    ;; Visit elements of SEQUENCES in parallel to determine length of
    ;; the result. Determining the length of the result like this
    ;; allows cases like
    ;;
    ;;   (map 'my-sequence 'my-fun (circular-list 1 2 3) '(4 5 6))
    ;;
    ;; to return a sequence with three elements.
    (flet ((counting-visit (&rest args)
             (declare (dynamic-extent args)
                      (ignore args))
             (incf min-length)))
      (%map-for-effect #'counting-visit sequences))
    ;; Map local function over SEQUENCES that steps through the result
    ;; sequence and stores results of applying FUNCTION.
    (binding* ((result (sequence:make-sequence-like result-prototype min-length))
               ((state nil from-end step nil nil setelt)
                (sequence:make-sequence-iterator result)))
      (declare (type function step setelt))
      (flet ((one-element (&rest args)
               (declare (dynamic-extent args))
               (funcall setelt (apply function args) result state)
               (setq state (funcall step result state from-end))))
        (%map-for-effect #'one-element sequences))
      result)))

;;; FIXME: COUNT, POSITION and FIND share an awful lot of structure.
;;; They could usefully be defined in an OAOO way.
(defgeneric sequence:count
    (item sequence &key from-end start end test test-not key)
  (:argument-precedence-order sequence item))
(defmethod sequence:count
    (item (sequence sequence) &key from-end (start 0) end test test-not key)
  (let ((test (sequence:canonize-test test test-not))
        (key (sequence:canonize-key key)))
    (sequence:with-sequence-iterator (state limit from-end step endp elt)
        (sequence :from-end from-end :start start :end end)
      (do ((count 0))
          ((funcall endp sequence state limit from-end) count)
        (let ((o (funcall elt sequence state)))
          (when (funcall test item (funcall key o))
            (incf count))
          (setq state (funcall step sequence state from-end)))))))

(defgeneric sequence:count-if (pred sequence &key from-end start end key)
  (:argument-precedence-order sequence pred))
(defmethod sequence:count-if
    (pred (sequence sequence) &key from-end (start 0) end key)
  (let ((key (sequence:canonize-key key)))
    (sequence:with-sequence-iterator (state limit from-end step endp elt)
        (sequence :from-end from-end :start start :end end)
      (do ((count 0))
          ((funcall endp sequence state limit from-end) count)
        (let ((o (funcall elt sequence state)))
          (when (funcall pred (funcall key o))
            (incf count))
          (setq state (funcall step sequence state from-end)))))))

(defgeneric sequence:count-if-not (pred sequence &key from-end start end key)
  (:argument-precedence-order sequence pred))
(defmethod sequence:count-if-not
    (pred (sequence sequence) &key from-end (start 0) end key)
  (let ((key (sequence:canonize-key key)))
    (sequence:with-sequence-iterator (state limit from-end step endp elt)
        (sequence :from-end from-end :start start :end end)
      (do ((count 0))
          ((funcall endp sequence state limit from-end) count)
        (let ((o (funcall elt sequence state)))
          (unless (funcall pred (funcall key o))
            (incf count))
          (setq state (funcall step sequence state from-end)))))))

(defgeneric sequence:find
    (item sequence &key from-end start end test test-not key)
  (:argument-precedence-order sequence item))
(defmethod sequence:find
    (item (sequence sequence) &key from-end (start 0) end test test-not key)
  (let ((test (sequence:canonize-test test test-not))
        (key (sequence:canonize-key key)))
    (sequence:with-sequence-iterator (state limit from-end step endp elt)
        (sequence :from-end from-end :start start :end end)
      (do ()
          ((funcall endp sequence state limit from-end) nil)
        (let ((o (funcall elt sequence state)))
          (when (funcall test item (funcall key o))
            (return o))
          (setq state (funcall step sequence state from-end)))))))

(defgeneric sequence:find-if (pred sequence &key from-end start end key)
  (:argument-precedence-order sequence pred))
(defmethod sequence:find-if
    (pred (sequence sequence) &key from-end (start 0) end key)
  (let ((key (sequence:canonize-key key)))
    (sequence:with-sequence-iterator (state limit from-end step endp elt)
        (sequence :from-end from-end :start start :end end)
      (do ()
          ((funcall endp sequence state limit from-end) nil)
        (let ((o (funcall elt sequence state)))
          (when (funcall pred (funcall key o))
            (return o))
          (setq state (funcall step sequence state from-end)))))))

(defgeneric sequence:find-if-not (pred sequence &key from-end start end key)
  (:argument-precedence-order sequence pred))
(defmethod sequence:find-if-not
    (pred (sequence sequence) &key from-end (start 0) end key)
  (let ((key (sequence:canonize-key key)))
    (sequence:with-sequence-iterator (state limit from-end step endp elt)
        (sequence :from-end from-end :start start :end end)
      (do ()
          ((funcall endp sequence state limit from-end) nil)
        (let ((o (funcall elt sequence state)))
          (unless (funcall pred (funcall key o))
            (return o))
          (setq state (funcall step sequence state from-end)))))))

(defgeneric sequence:position
    (item sequence &key from-end start end test test-not key)
  (:argument-precedence-order sequence item))
(defmethod sequence:position
    (item (sequence sequence) &key from-end (start 0) end test test-not key)
  (let ((test (sequence:canonize-test test test-not))
        (key (sequence:canonize-key key)))
    (sequence:with-sequence-iterator (state limit from-end step endp elt)
        (sequence :from-end from-end :start start :end end)
      (do ((s (if from-end -1 1))
           (pos (if from-end (1- (or end (length sequence))) start) (+ pos s)))
          ((funcall endp sequence state limit from-end) nil)
        (let ((o (funcall elt sequence state)))
          (when (funcall test item (funcall key o))
            (return pos))
          (setq state (funcall step sequence state from-end)))))))

(defgeneric sequence:position-if (pred sequence &key from-end start end key)
  (:argument-precedence-order sequence pred))
(defmethod sequence:position-if
    (pred (sequence sequence) &key from-end (start 0) end key)
  (let ((key (sequence:canonize-key key)))
    (sequence:with-sequence-iterator (state limit from-end step endp elt)
        (sequence :from-end from-end :start start :end end)
      (do ((s (if from-end -1 1))
           (pos (if from-end (1- (or end (length sequence))) start) (+ pos s)))
          ((funcall endp sequence state limit from-end) nil)
        (let ((o (funcall elt sequence state)))
          (when (funcall pred (funcall key o))
            (return pos))
          (setq state (funcall step sequence state from-end)))))))

(defgeneric sequence:position-if-not
    (pred sequence &key from-end start end key)
  (:argument-precedence-order sequence pred))
(defmethod sequence:position-if-not
    (pred (sequence sequence) &key from-end (start 0) end key)
  (let ((key (sequence:canonize-key key)))
    (sequence:with-sequence-iterator (state limit from-end step endp elt)
        (sequence :from-end from-end :start start :end end)
      (do ((s (if from-end -1 1))
           (pos (if from-end (1- (or end (length sequence))) start) (+ pos s)))
          ((funcall endp sequence state limit from-end) nil)
        (let ((o (funcall elt sequence state)))
          (unless (funcall pred (funcall key o))
            (return pos))
          (setq state (funcall step sequence state from-end)))))))

(defgeneric sequence:subseq (sequence start &optional end))
(defmethod sequence:subseq ((sequence sequence) start &optional end)
  (let* ((end (or end (length sequence)))
         (length (- end start))
         (result (sequence:make-sequence-like sequence length)))
    (sequence:with-sequence-iterator (state limit from-end step endp elt)
        (sequence :start start :end end)
      (declare (ignore limit endp))
      (sequence:with-sequence-iterator (rstate rlimit rfrom-end rstep rendp relt rsetelt)
          (result)
        (declare (ignore rlimit rendp relt))
        (do ((i 0 (+ i 1)))
            ((>= i length) result)
          (funcall rsetelt (funcall elt sequence state) result rstate)
          (setq state (funcall step sequence state from-end))
          (setq rstate (funcall rstep result rstate rfrom-end)))))))

(defgeneric sequence:copy-seq (sequence))
(defmethod sequence:copy-seq ((sequence sequence))
  (sequence:subseq sequence 0))

(defgeneric sequence:fill (sequence item &key start end))
(defmethod sequence:fill ((sequence sequence) item &key (start 0) end)
  (sequence:with-sequence-iterator (state limit from-end step endp elt setelt)
      (sequence :start start :end end)
    (declare (ignore elt))
    (do ()
        ((funcall endp sequence state limit from-end) sequence)
      (funcall setelt item sequence state)
      (setq state (funcall step sequence state from-end)))))

(defgeneric sequence:nsubstitute
    (new old sequence &key start end from-end test test-not count key)
  (:argument-precedence-order sequence new old))
(defmethod sequence:nsubstitute (new old (sequence sequence) &key (start 0)
                                 end from-end test test-not count key)
  (let ((test (sequence:canonize-test test test-not))
        (key (sequence:canonize-key key)))
    (sequence:with-sequence-iterator (state limit from-end step endp elt setelt)
        (sequence :start start :end end :from-end from-end)
      (do ((c 0))
          ((or (and count (>= c count))
               (funcall endp sequence state limit from-end))
           sequence)
        (when (funcall test old (funcall key (funcall elt sequence state)))
          (incf c)
          (funcall setelt new sequence state))
        (setq state (funcall step sequence state from-end))))))

(defgeneric sequence:nsubstitute-if
    (new predicate sequence &key start end from-end count key)
  (:argument-precedence-order sequence new predicate))
(defmethod sequence:nsubstitute-if
    (new predicate (sequence sequence) &key (start 0) end from-end count key)
  (let ((key (sequence:canonize-key key)))
    (sequence:with-sequence-iterator (state limit from-end step endp elt setelt)
        (sequence :start start :end end :from-end from-end)
      (do ((c 0))
          ((or (and count (>= c count))
               (funcall endp sequence state limit from-end))
           sequence)
        (when (funcall predicate (funcall key (funcall elt sequence state)))
          (incf c)
          (funcall setelt new sequence state))
        (setq state (funcall step sequence state from-end))))))

(defgeneric sequence:nsubstitute-if-not
    (new predicate sequence &key start end from-end count key)
  (:argument-precedence-order sequence new predicate))
(defmethod sequence:nsubstitute-if-not
    (new predicate (sequence sequence) &key (start 0) end from-end count key)
  (let ((key (sequence:canonize-key key)))
    (sequence:with-sequence-iterator (state limit from-end step endp elt setelt)
        (sequence :start start :end end :from-end from-end)
      (do ((c 0))
          ((or (and count (>= c count))
               (funcall endp sequence state limit from-end))
           sequence)
        (unless (funcall predicate (funcall key (funcall elt sequence state)))
          (incf c)
          (funcall setelt new sequence state))
        (setq state (funcall step sequence state from-end))))))

(defgeneric sequence:substitute
    (new old sequence &key start end from-end test test-not count key)
  (:argument-precedence-order sequence new old))
(defmethod sequence:substitute (new old (sequence sequence) &rest args &key
                                (start 0) end from-end test test-not count key)
  (declare (dynamic-extent args))
  (declare (ignore start end from-end test test-not count key))
  (let ((result (copy-seq sequence)))
    (apply #'sequence:nsubstitute new old result args)))

(defgeneric sequence:substitute-if
    (new predicate sequence &key start end from-end count key)
  (:argument-precedence-order sequence new predicate))
(defmethod sequence:substitute-if (new predicate (sequence sequence) &rest args
                                   &key (start 0) end from-end count key)
  (declare (dynamic-extent args))
  (declare (ignore start end from-end count key))
  (let ((result (copy-seq sequence)))
    (apply #'sequence:nsubstitute-if new predicate result args)))

(defgeneric sequence:substitute-if-not
    (new predicate sequence &key start end from-end count key)
  (:argument-precedence-order sequence new predicate))
(defmethod sequence:substitute-if-not
    (new predicate (sequence sequence) &rest args &key
     (start 0) end from-end count key)
  (declare (dynamic-extent args))
  (declare (ignore start end from-end count key))
  (let ((result (copy-seq sequence)))
    (apply #'sequence:nsubstitute-if-not new predicate result args)))

(defun %sequence-replace (sequence1 sequence2 start1 end1 start2 end2)
  (sequence:with-sequence-iterator (state1 limit1 from-end1 step1 endp1 elt1 setelt1)
      (sequence1 :start start1 :end end1)
    (declare (ignore elt1))
    (sequence:with-sequence-iterator (state2 limit2 from-end2 step2 endp2 elt2)
        (sequence2 :start start2 :end end2)
      (do ()
          ((or (funcall endp1 sequence1 state1 limit1 from-end1)
               (funcall endp2 sequence2 state2 limit2 from-end2))
           sequence1)
        (funcall setelt1 (funcall elt2 sequence2 state2) sequence1 state1)
        (setq state1 (funcall step1 sequence1 state1 from-end1))
        (setq state2 (funcall step2 sequence2 state2 from-end2))))))

(defgeneric sequence:replace
    (sequence1 sequence2 &key start1 end1 start2 end2)
  (:argument-precedence-order sequence2 sequence1))
(defmethod sequence:replace
    ((sequence1 sequence) (sequence2 sequence) &key
     (start1 0) end1 (start2 0) end2)
  (cond
    ((eq sequence1 sequence2)
     (let ((replaces (subseq sequence2 start2 end2)))
       (%sequence-replace sequence1 replaces start1 end1 0 nil)))
    (t (%sequence-replace sequence1 sequence2 start1 end1 start2 end2))))

(defgeneric sequence:nreverse (sequence))
(defmethod sequence:nreverse ((sequence sequence))
  ;; FIXME: this, in particular the :from-end iterator, will suck
  ;; mightily if the user defines a list-like structure.
  (let ((length (length sequence)))
    (sequence:with-sequence-iterator (state1 limit1 from-end1 step1 endp1 elt1 setelt1)
        (sequence :end (floor length 2))
      (sequence:with-sequence-iterator (state2 limit2 from-end2 step2 endp2 elt2 setelt2)
          (sequence :start (ceiling length 2) :from-end t)
        (declare (ignore limit2 endp2))
        (do ()
            ((funcall endp1 sequence state1 limit1 from-end1) sequence)
          (let ((x (funcall elt1 sequence state1))
                (y (funcall elt2 sequence state2)))
            (funcall setelt1 y sequence state1)
            (funcall setelt2 x sequence state2))
          (setq state1 (funcall step1 sequence state1 from-end1))
          (setq state2 (funcall step2 sequence state2 from-end2)))))))

(defgeneric sequence:reverse (sequence))
(defmethod sequence:reverse ((sequence sequence))
  (let ((result (copy-seq sequence)))
    (sequence:nreverse result)))

(defgeneric sequence:concatenate (result-prototype &rest sequences)
  (:documentation
   "Implements CL:CONCATENATE for extended sequences.

    RESULT-PROTOTYPE corresponds to the RESULT-TYPE of CL:CONCATENATE
    but receives a prototype instance of an extended sequence class
    instead of a type specifier. By dispatching on RESULT-PROTOTYPE,
    methods on this generic function specify how extended sequence
    classes act when they are specified as the result type in a
    CL:CONCATENATE call. RESULT-PROTOTYPE may not be fully initialized
    and thus should only be used for dispatch and to determine its
    class."))

(defmethod sequence:concatenate ((result-prototype sequence) &rest sequences)
  (let* ((lengths (mapcar #'length sequences))
         (result (sequence:make-sequence-like
                  result-prototype (reduce #'+ lengths))))
    (loop with index = 0
       for sequence in sequences
       for length in lengths do
         (replace result sequence :start1 index)
         (incf index length))
    result))

(defgeneric sequence:reduce
    (function sequence &key from-end start end initial-value)
  (:argument-precedence-order sequence function))
(defmethod sequence:reduce
    (function (sequence sequence) &key from-end (start 0) end key
     (initial-value nil ivp))
  (let ((key (sequence:canonize-key key)))
    (sequence:with-sequence-iterator (state limit from-end step endp elt)
        (sequence :start start :end end :from-end from-end)
      (if (funcall endp sequence state limit from-end)
          (if ivp initial-value (funcall function))
          (do* ((state state (funcall step sequence state from-end))
                (value (cond
                         (ivp initial-value)
                         (t (prog1
                                (funcall key (funcall elt sequence state))
                              (setq state (funcall step sequence state from-end)))))))
               ((funcall endp sequence state limit from-end) value)
            (let ((e (funcall key (funcall elt sequence state))))
              (if from-end
                  (setq value (funcall function e value))
                  (setq value (funcall function value e)))))))))

(defgeneric sequence:mismatch (sequence1 sequence2 &key from-end start1 end1
                               start2 end2 test test-not key))
(defmethod sequence:mismatch
    ((sequence1 sequence) (sequence2 sequence) &key from-end (start1 0) end1
     (start2 0) end2 test test-not key)
  (let ((test (sequence:canonize-test test test-not))
        (key (sequence:canonize-key key)))
    (sequence:with-sequence-iterator (state1 limit1 from-end1 step1 endp1 elt1)
        (sequence1 :start start1 :end end1 :from-end from-end)
      (sequence:with-sequence-iterator (state2 limit2 from-end2 step2 endp2 elt2)
          (sequence2 :start start2 :end end2 :from-end from-end)
        (if from-end
            (do ((result (or end1 (length sequence1)) (1- result))
                 (e1 (funcall endp1 sequence1 state1 limit1 from-end1)
                     (funcall endp1 sequence1 state1 limit1 from-end1))
                 (e2 (funcall endp2 sequence2 state2 limit2 from-end2)
                     (funcall endp2 sequence2 state2 limit2 from-end2)))
                ((or e1 e2) (if (and e1 e2) nil result))
              (let ((o1 (funcall key (funcall elt1 sequence1 state1)))
                    (o2 (funcall key (funcall elt2 sequence2 state2))))
                (unless (funcall test o1 o2)
                  (return result))
                (setq state1 (funcall step1 sequence1 state1 from-end1))
                (setq state2 (funcall step2 sequence2 state2 from-end2))))
            (do ((result start1 (1+ result))
                 (e1 (funcall endp1 sequence1 state1 limit1 from-end1)
                     (funcall endp1 sequence1 state1 limit1 from-end1))
                 (e2 (funcall endp2 sequence2 state2 limit2 from-end2)
                     (funcall endp2 sequence2 state2 limit2 from-end2)))
                ((or e1 e2) (if (and e1 e2) nil result))
              (let ((o1 (funcall key (funcall elt1 sequence1 state1)))
                    (o2 (funcall key (funcall elt2 sequence2 state2))))
                (unless (funcall test o1 o2)
                  (return result)))
              (setq state1 (funcall step1 sequence1 state1 from-end1))
              (setq state2 (funcall step2 sequence2 state2 from-end2))))))))

(defgeneric sequence:search (sequence1 sequence2 &key from-end start1 end1
                             start2 end2 test test-not key))
(defmethod sequence:search
    ((sequence1 sequence) (sequence2 sequence) &key from-end (start1 0) end1
     (start2 0) end2 test test-not key)
  (let* ((test (sequence:canonize-test test test-not))
         (key (sequence:canonize-key key))
         (range1 (- (or end1 (length sequence1)) start1))
         (range2 (- (or end2 (length sequence2)) start2))
         (count (1+ (- range2 range1))))
    (when (minusp count)
      (return-from sequence:search nil))
    ;; Create an iteration state for SEQUENCE1 for the interesting
    ;;range within SEQUENCE1. To compare this range against ranges in
    ;;SEQUENCE2, we copy START-STATE1 and then mutate the copy.
    (sequence:with-sequence-iterator (start-state1 nil from-end1
                                      step1 nil elt1 nil nil copy1)
        (sequence1 :start start1 :end end1 :from-end from-end)
      ;; Create an iteration state for the interesting range within
      ;; SEQUENCE2.
      (sequence:with-sequence-iterator (start-state2 nil from-end2
                                        step2 nil elt2 nil index2 copy2)
          (sequence2 :start start2 :end end2 :from-end from-end)
        ;; Copy both iterators at all COUNT possible match positions.
        (dotimes (i count)
          (let ((state1 (funcall copy1 sequence1 start-state1))
                (state2 (funcall copy2 sequence2 start-state2)))
            ;; Determine whether there is a match at the current
            ;; position. Return immediately, if there is a match.
            (dotimes
                (j range1
                   (return-from sequence:search
                     (let ((position (funcall index2 sequence2 start-state2)))
                       (if from-end (- position range1 -1) position))))
              (unless (funcall test
                               (funcall key (funcall elt1 sequence1 state1))
                               (funcall key (funcall elt2 sequence2 state2)))
                (return nil))
              (setq state1 (funcall step1 sequence1 state1 from-end1))
              (setq state2 (funcall step2 sequence2 state2 from-end2))))
          (setq start-state2 (funcall step2 sequence2 start-state2 from-end2)))))))

(defgeneric sequence:delete
    (item sequence &key from-end test test-not start end count key)
  (:argument-precedence-order sequence item))
(defmethod sequence:delete (item (sequence sequence) &key
                            from-end test test-not (start 0) end count key)
  (let ((test (sequence:canonize-test test test-not))
        (key (sequence:canonize-key key))
        (c 0))
    (sequence:with-sequence-iterator (state1 limit1 from-end1 step1 endp1 elt1 setelt1)
        (sequence :start start :end end :from-end from-end)
      (declare (ignore limit1 endp1 elt1))
      (sequence:with-sequence-iterator (state2 limit2 from-end2 step2 endp2 elt2)
          (sequence :start start :end end :from-end from-end)
        (flet ((finish ()
                 (if from-end
                     (replace sequence sequence
                              :start1 start :end1 (- (length sequence) c)
                              :start2 (+ start c) :end2 (length sequence))
                     (unless (or (null end) (= end (length sequence)))
                       (replace sequence sequence :start2 end :start1 (- end c)
                                :end1 (- (length sequence) c))))
                 (sequence:adjust-sequence sequence (- (length sequence) c))))
          (do ()
              ((funcall endp2 sequence state2 limit2 from-end2) (finish))
            (let ((e (funcall elt2 sequence state2)))
              (loop
               (when (and count (>= c count))
                 (return))
               (if (funcall test item (funcall key e))
                   (progn
                     (incf c)
                     (setq state2 (funcall step2 sequence state2 from-end2))
                     (when (funcall endp2 sequence state2 limit2 from-end2)
                       (return-from sequence:delete (finish)))
                     (setq e (funcall elt2 sequence state2)))
                   (return)))
              (funcall setelt1 e sequence state1))
            (setq state1 (funcall step1 sequence state1 from-end1))
            (setq state2 (funcall step2 sequence state2 from-end2))))))))

(defgeneric sequence:delete-if
    (predicate sequence &key from-end start end count key)
  (:argument-precedence-order sequence predicate))
(defmethod sequence:delete-if (predicate (sequence sequence) &key
                               from-end (start 0) end count key)
  (let ((key (sequence:canonize-key key))
        (c 0))
    (sequence:with-sequence-iterator (state1 limit1 from-end1 step1 endp1 elt1 setelt1)
        (sequence :start start :end end :from-end from-end)
      (declare (ignore limit1 endp1 elt1))
      (sequence:with-sequence-iterator (state2 limit2 from-end2 step2 endp2 elt2)
          (sequence :start start :end end :from-end from-end)
        (flet ((finish ()
                 (if from-end
                     (replace sequence sequence
                              :start1 start :end1 (- (length sequence) c)
                              :start2 (+ start c) :end2 (length sequence))
                     (unless (or (null end) (= end (length sequence)))
                       (replace sequence sequence :start2 end :start1 (- end c)
                                :end1 (- (length sequence) c))))
                 (sequence:adjust-sequence sequence (- (length sequence) c))))
          (do ()
              ((funcall endp2 sequence state2 limit2 from-end2) (finish))
            (let ((e (funcall elt2 sequence state2)))
              (loop
               (when (and count (>= c count))
                 (return))
               (if (funcall predicate (funcall key e))
                   (progn
                     (incf c)
                     (setq state2 (funcall step2 sequence state2 from-end2))
                     (when (funcall endp2 sequence state2 limit2 from-end2)
                       (return-from sequence:delete-if (finish)))
                     (setq e (funcall elt2 sequence state2)))
                   (return)))
              (funcall setelt1 e sequence state1))
            (setq state1 (funcall step1 sequence state1 from-end1))
            (setq state2 (funcall step2 sequence state2 from-end2))))))))

(defgeneric sequence:delete-if-not
    (predicate sequence &key from-end start end count key)
  (:argument-precedence-order sequence predicate))
(defmethod sequence:delete-if-not (predicate (sequence sequence) &key
                                   from-end (start 0) end count key)
  (let ((key (sequence:canonize-key key))
        (c 0))
    (sequence:with-sequence-iterator (state1 limit1 from-end1 step1 endp1 elt1 setelt1)
        (sequence :start start :end end :from-end from-end)
      (declare (ignore limit1 endp1 elt1))
      (sequence:with-sequence-iterator (state2 limit2 from-end2 step2 endp2 elt2)
          (sequence :start start :end end :from-end from-end)
        (flet ((finish ()
                 (if from-end
                     (replace sequence sequence
                              :start1 start :end1 (- (length sequence) c)
                              :start2 (+ start c) :end2 (length sequence))
                     (unless (or (null end) (= end (length sequence)))
                       (replace sequence sequence :start2 end :start1 (- end c)
                                :end1 (- (length sequence) c))))
                 (sequence:adjust-sequence sequence (- (length sequence) c))))
          (do ()
              ((funcall endp2 sequence state2 limit2 from-end2) (finish))
            (let ((e (funcall elt2 sequence state2)))
              (loop
               (when (and count (>= c count))
                 (return))
               (if (funcall predicate (funcall key e))
                   (return)
                   (progn
                     (incf c)
                     (setq state2 (funcall step2 sequence state2 from-end2))
                     (when (funcall endp2 sequence state2 limit2 from-end2)
                       (return-from sequence:delete-if-not (finish)))
                     (setq e (funcall elt2 sequence state2)))))
              (funcall setelt1 e sequence state1))
            (setq state1 (funcall step1 sequence state1 from-end1))
            (setq state2 (funcall step2 sequence state2 from-end2))))))))

(defgeneric sequence:remove
    (item sequence &key from-end test test-not start end count key)
  (:argument-precedence-order sequence item))
(defmethod sequence:remove (item (sequence sequence) &rest args &key
                            from-end test test-not (start 0) end count key)
  (declare (dynamic-extent args))
  (declare (ignore from-end test test-not start end count key))
  (let ((result (copy-seq sequence)))
    (apply #'sequence:delete item result args)))

(defgeneric sequence:remove-if
    (predicate sequence &key from-end start end count key)
  (:argument-precedence-order sequence predicate))
(defmethod sequence:remove-if (predicate (sequence sequence) &rest args &key
                               from-end (start 0) end count key)
  (declare (dynamic-extent args))
  (declare (ignore from-end start end count key))
  (let ((result (copy-seq sequence)))
    (apply #'sequence:delete-if predicate result args)))

(defgeneric sequence:remove-if-not
    (predicate sequence &key from-end start end count key)
  (:argument-precedence-order sequence predicate))
(defmethod sequence:remove-if-not (predicate (sequence sequence) &rest args
                                   &key from-end (start 0) end count key)
  (declare (dynamic-extent args))
  (declare (ignore from-end start end count key))
  (let ((result (copy-seq sequence)))
    (apply #'sequence:delete-if-not predicate result args)))

(defgeneric sequence:delete-duplicates
    (sequence &key from-end test test-not start end key))
(defmethod sequence:delete-duplicates
    ((sequence sequence) &key from-end test test-not (start 0) end key)
  (let ((test (sequence:canonize-test test test-not))
        (key (sequence:canonize-key key))
        (c 0))
    (sequence:with-sequence-iterator (state1 limit1 from-end1 step1 endp1 elt1 setelt1)
        (sequence :start start :end end :from-end from-end)
      (declare (ignore limit1 endp1 elt1))
      (sequence:with-sequence-iterator (state2 limit2 from-end2 step2 endp2 elt2)
          (sequence :start start :end end :from-end from-end)
        (flet ((finish ()
                 (if from-end
                     (replace sequence sequence
                              :start1 start :end1 (- (length sequence) c)
                              :start2 (+ start c) :end2 (length sequence))
                     (unless (or (null end) (= end (length sequence)))
                       (replace sequence sequence :start2 end :start1 (- end c)
                                :end1 (- (length sequence) c))))
                 (sequence:adjust-sequence sequence (- (length sequence) c))))
          (do ((end (or end (length sequence)))
               (step 0 (1+ step)))
              ((funcall endp2 sequence state2 limit2 from-end2) (finish))
            (let ((e (funcall elt2 sequence state2)))
              (loop
               ;; FIXME: replace with POSITION once position is
               ;; working
               (if (> (count (funcall key e) sequence :test test :key key
                             :start (if from-end start (+ start step 1))
                             :end (if from-end (- end step 1) end))
                      0)
                   (progn
                     (incf c)
                     (incf step)
                     (setq state2 (funcall step2 sequence state2 from-end2))
                     (when (funcall endp2 sequence state2 limit2 from-end2)
                       (return-from sequence:delete-duplicates (finish)))
                     (setq e (funcall elt2 sequence state2)))
                   (progn
                     (return))))
              (funcall setelt1 e sequence state1))
            (setq state1 (funcall step1 sequence state1 from-end1))
            (setq state2 (funcall step2 sequence state2 from-end2))))))))

(defgeneric sequence:remove-duplicates
    (sequence &key from-end test test-not start end key))
(defmethod sequence:remove-duplicates
    ((sequence sequence) &rest args &key from-end test test-not (start 0) end key)
  (declare (dynamic-extent args))
  (declare (ignore from-end test test-not start end key))
  (let ((result (copy-seq sequence)))
    (apply #'sequence:delete-duplicates result args)))

(defun %sort-with-temp-vector (sorter sequence predicate &rest args)
  (declare (type function sorter))
  (let* ((length (length sequence))
         (vector (make-array length)))
    (sequence:with-sequence-iterator (state limit from-end step endp elt)
        (sequence)
      (declare (ignore limit  endp))
      (do ((i 0 (1+ i)))
          ((>= i length))
        (setf (aref vector i) (funcall elt sequence state))
        (setq state (funcall step sequence state from-end))))
    (apply sorter vector predicate args)
    (sequence:with-sequence-iterator (state limit from-end step endp elt setelt)
        (sequence)
      (declare (ignore limit endp elt))
      (do ((i 0 (1+ i)))
          ((>= i length) sequence)
        (funcall setelt (aref vector i) sequence state)
        (setq state (funcall step sequence state from-end))))))

(defgeneric sequence:sort (sequence predicate &key key))
(defmethod sequence:sort ((sequence sequence) predicate &rest args &key key)
  (declare (dynamic-extent args)
           (ignore key))
  (apply #'%sort-with-temp-vector #'sort sequence predicate args))

(defgeneric sequence:stable-sort (sequence predicate &key key))
(defmethod sequence:stable-sort
    ((sequence sequence) predicate &rest args &key key)
  (declare (dynamic-extent args)
           (ignore key))
  (apply #'%sort-with-temp-vector #'stable-sort sequence predicate args))

(defgeneric sequence:merge (result-prototype sequence1 sequence2 predicate &key key)
  (:documentation
   "Implements CL:MERGE for extended sequences.

    RESULT-PROTOTYPE corresponds to the RESULT-TYPE of CL:MERGE but
    receives a prototype instance of an extended sequence class
    instead of a type specifier. By dispatching on RESULT-PROTOTYPE,
    methods on this generic function specify how extended sequence
    classes act when they are specified as the result type in a
    CL:MERGE call. RESULT-PROTOTYPE may not be fully initialized and
    thus should only be used for dispatch and to determine its class.

    Another difference to CL:MERGE is that PREDICATE is a function,
    not a function designator."))

(defmethod sequence:merge ((result-prototype sequence)
                           (sequence1 sequence) (sequence2 sequence)
                           (predicate function) &key key)
  (let ((key-function (when key
                        (%coerce-callable-to-fun key)))
        (result (sequence:make-sequence-like
                 result-prototype (+ (length sequence1) (length sequence2))))
        endp1 elt1 key1 endp2 elt2 key2)
    (sequence:with-sequence-iterator-functions
        (step-result nil nil setelt-result index-result) (result)
      (sequence:with-sequence-iterator-functions
          (step1 endp1 elt1 nil index1) (sequence1)
        (sequence:with-sequence-iterator-functions
            (step2 endp2 elt2 nil index2) (sequence2)
          (labels ((pop/no-key1 ()
                     (unless (setf endp1 (endp1))
                       (setf elt1 (elt1))))
                   (pop/no-key2 ()
                     (unless (setf endp2 (endp2))
                       (setf elt2 (elt2))))
                   (pop/key1 ()
                     (unless (setf endp1 (endp1))
                       (setf key1 (funcall (truly-the function key-function)
                                           (setf elt1 (elt1))))))
                   (pop/key2 ()
                     (unless (setf endp2 (endp2))
                       (setf key2 (funcall (truly-the function key-function)
                                           (setf elt2 (elt2))))))
                   (pop-one/no-key ()
                     (if (funcall predicate elt2 elt1) ; see comment in MERGE-LIST*
                         (prog1 elt2 (step2) (pop/no-key2))
                         (prog1 elt1 (step1) (pop/no-key1))))
                   (pop-one/key ()
                     (if (funcall predicate key2 key1)
                         (prog1 elt2 (step2) (pop/key2))
                         (prog1 elt1 (step1) (pop/key1)))))
            (declare (dynamic-extent #'pop-one/no-key #'pop-one/key))
            ;; Populate ENDP{1,2}, ELT{1,2} and maybe KEY{1,2}.
            (cond (key-function (pop/key1) (pop/key2))
                  (t (pop/no-key1) (pop/no-key2)))
            (loop with pop-one = (if key-function #'pop-one/key #'pop-one/no-key) do
                 (cond
                   (endp2 ; batch-replace rest of SEQUENCE1 if SEQUENCE2 exhausted
                    (unless endp1
                      (replace result sequence1 :start1 (index-result) :start2 (index1)))
                    (return))
                   (endp1
                    (unless endp2
                      (replace result sequence2 :start1 (index-result) :start2 (index2)))
                    (return))
                   (t
                    (setelt-result (funcall pop-one))
                    (step-result))))))))
    result))
