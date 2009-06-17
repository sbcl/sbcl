;;;; Extensible sequences, based on the proposal by Christophe Rhodes.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(in-package "SB-IMPL")

;;;; basic protocol
(define-condition sequence::protocol-unimplemented (type-error)
  ())

(defun sequence::protocol-unimplemented (sequence)
  (error 'sequence::protocol-unimplemented
         :datum sequence :expected-type '(or list vector)))

(defgeneric sequence:length (sequence)
  (:method ((s list)) (length s))
  (:method ((s vector)) (length s))
  (:method ((s sequence)) (sequence::protocol-unimplemented s)))

(defgeneric sequence:elt (sequence index)
  (:method ((s list) index) (elt s index))
  (:method ((s vector) index) (elt s index))
  (:method ((s sequence) index) (sequence::protocol-unimplemented s)))

(defgeneric (setf sequence:elt) (new-value sequence index)
  (:argument-precedence-order sequence new-value index)
  (:method (new-value (s list) index) (setf (elt s index) new-value))
  (:method (new-value (s vector) index) (setf (elt s index) new-value))
  (:method (new-value (s sequence) index)
    (sequence::protocol-unimplemented s)))

(defgeneric sequence:make-sequence-like
    (sequence length &key initial-element initial-contents)
  (:method ((s list) length &key
            (initial-element nil iep) (initial-contents nil icp))
    (cond
      ((and icp iep) (error "bar"))
      (iep (make-list length :initial-element initial-element))
      (icp (unless (= (length initial-contents) length)
             (error "foo"))
           (let ((result (make-list length)))
             (replace result initial-contents)
             result))
      (t (make-list length))))
  (:method ((s vector) length &key
            (initial-element nil iep) (initial-contents nil icp))
    (cond
      ((and icp iep) (error "foo"))
      (iep (make-array length :element-type (array-element-type s)
                       :initial-element initial-element))
      (icp (make-array length :element-type (array-element-type s)
                       :initial-contents initial-contents))
      (t (make-array length :element-type (array-element-type s)))))
  (:method ((s sequence) length &key initial-element initial-contents)
    (declare (ignore initial-element initial-contents))
    (sequence::protocol-unimplemented s)))

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
  (:method (new-value (s sequence) &rest args)
    (declare (ignore args))
    (sequence::protocol-unimplemented s)))

;;;; iterator protocol

;;; The general protocol

(defgeneric sequence:make-sequence-iterator (sequence &key from-end start end)
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
           :expected-type 'sequence)))

;;; the simple protocol: the simple iterator returns three values,
;;; STATE, LIMIT and FROM-END.

;;; magic termination value for list :from-end t
(defvar *exhausted* (cons nil nil))

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
          (values start end nil)))))

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
        (1+ iterator))))

(defgeneric sequence:iterator-endp (sequence iterator limit from-end)
  (:method ((s list) iterator limit from-end)
    (eq iterator limit))
  (:method ((s vector) iterator limit from-end)
    (= iterator limit))
  (:method ((s sequence) iterator limit from-end)
    (= iterator limit)))

(defgeneric sequence:iterator-element (sequence iterator)
  (:method ((s list) iterator)
    (car iterator))
  (:method ((s vector) iterator)
    (aref s iterator))
  (:method ((s sequence) iterator)
    (elt s iterator)))

(defgeneric (setf sequence:iterator-element) (new-value sequence iterator)
  (:method (o (s list) iterator)
    (setf (car iterator) o))
  (:method (o (s vector) iterator)
    (setf (aref s iterator) o))
  (:method (o (s sequence) iterator)
    (setf (elt s iterator) o)))

(defgeneric sequence:iterator-index (sequence iterator)
  (:method ((s list) iterator)
    ;; FIXME: this sucks.  (In my defence, it is the equivalent of the
    ;; Apple implementation in Dylan...)
    (loop for l on s for i from 0 when (eq l iterator) return i))
  (:method ((s vector) iterator) iterator)
  (:method ((s sequence) iterator) iterator))

(defgeneric sequence:iterator-copy (sequence iterator)
  (:method ((s list) iterator) iterator)
  (:method ((s vector) iterator) iterator)
  (:method ((s sequence) iterator) iterator))

(defmacro sequence:with-sequence-iterator
    ((&rest vars) (s &rest args &key from-end start end) &body body)
  (declare (ignore from-end start end))
  `(multiple-value-bind (,@vars) (sequence:make-sequence-iterator ,s ,@args)
    (declare (type function ,@(nthcdr 3 vars)))
    ,@body))

(defmacro sequence:with-sequence-iterator-functions
    ((step endp elt setf index copy)
     (s &rest args &key from-end start end)
     &body body)
  (declare (ignore from-end start end))
  (let ((nstate (gensym "STATE")) (nlimit (gensym "LIMIT"))
        (nfrom-end (gensym "FROM-END-")) (nstep (gensym "STEP"))
        (nendp (gensym "ENDP")) (nelt (gensym "ELT"))
        (nsetf (gensym "SETF")) (nindex (gensym "INDEX"))
        (ncopy (gensym "COPY")))
    `(sequence:with-sequence-iterator
         (,nstate ,nlimit ,nfrom-end ,nstep ,nendp ,nelt ,nsetf ,nindex ,ncopy)
       (,s ,@args)
       (flet ((,step () (setq ,nstate (funcall ,nstep ,s ,nstate ,nfrom-end)))
              (,endp () (funcall ,nendp ,s ,nstate ,nlimit ,nfrom-end))
              (,elt () (funcall ,nelt ,s ,nstate))
              (,setf (new-value) (funcall ,nsetf new-value ,s ,nstate))
              (,index () (funcall ,nindex ,s ,nstate))
              (,copy () (funcall ,ncopy ,s ,nstate)))
         (declare (truly-dynamic-extent #',step #',endp #',elt
                                  #',setf #',index #',copy))
         ,@body))))

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
(defun loop-elements-iteration-path (variable data-type prep-phrases)
  (let (of-phrase)
    (loop for (prep . rest) in prep-phrases do
          (ecase prep
            ((:of :in) (if of-phrase
                           (sb-loop::loop-error "Too many prepositions")
                           (setq of-phrase rest)))))
    (destructuring-bind (it lim f-e step endp elt seq)
        (loop repeat 7 collect (gensym))
      (push `(let ((,seq ,(car of-phrase)))) sb-loop::*loop-wrappers*)
      (push `(sequence:with-sequence-iterator (,it ,lim ,f-e ,step ,endp ,elt) (,seq))
            sb-loop::*loop-wrappers*)
    `(((,variable nil ,data-type)) () () nil (funcall ,endp ,seq ,it ,lim ,f-e)
      (,variable (funcall ,elt ,seq ,it) ,it (funcall ,step ,seq ,it ,f-e))))))
(sb-loop::add-loop-path
 '(element elements) 'loop-elements-iteration-path sb-loop::*loop-ansi-universe*
 :preposition-groups '((:of :in)) :inclusive-permitted nil)

;;;; generic implementations for sequence functions.

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
  (declare (truly-dynamic-extent args))
  (declare (ignore start end from-end test test-not count key))
  (let ((result (copy-seq sequence)))
    (apply #'sequence:nsubstitute new old result args)))

(defgeneric sequence:substitute-if
    (new predicate sequence &key start end from-end count key)
  (:argument-precedence-order sequence new predicate))
(defmethod sequence:substitute-if (new predicate (sequence sequence) &rest args
                                   &key (start 0) end from-end count key)
  (declare (truly-dynamic-extent args))
  (declare (ignore start end from-end count key))
  (let ((result (copy-seq sequence)))
    (apply #'sequence:nsubstitute-if new predicate result args)))

(defgeneric sequence:substitute-if-not
    (new predicate sequence &key start end from-end count key)
  (:argument-precedence-order sequence new predicate))
(defmethod sequence:substitute-if-not
    (new predicate (sequence sequence) &rest args &key
     (start 0) end from-end count key)
  (declare (truly-dynamic-extent args))
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
  (let ((test (sequence:canonize-test test test-not))
        (key (sequence:canonize-key key))
        (mainend2 (- (or end2 (length sequence2))
                     (- (or end1 (length sequence1)) start1))))
    (when (< mainend2 0)
      (return-from sequence:search nil))
    (sequence:with-sequence-iterator (statem limitm from-endm stepm endpm)
        (sequence2 :start start2 :end mainend2 :from-end from-end)
      (do ((s2 (if from-end mainend2 0) (if from-end (1- s2) (1+ s2))))
          (nil)
        (sequence:with-sequence-iterator (state1 limit1 from-end1 step1 endp1 elt1)
            (sequence1 :start start1 :end end1)
          (sequence:with-sequence-iterator (state2 limit2 from-end2 step2 endp2 elt2)
              (sequence2 :start s2)
            (declare (ignore limit2 endp2))
            (when (do ()
                      ((funcall endp1 sequence1 state1 limit1 from-end1) t)
                    (let ((o1 (funcall key (funcall elt1 sequence1 state1)))
                          (o2 (funcall key (funcall elt2 sequence2 state2))))
                      (unless (funcall test o1 o2)
                        (return nil)))
                    (setq state1 (funcall step1 sequence1 state1 from-end1))
                    (setq state2 (funcall step2 sequence2 state2 from-end2)))
              (return-from sequence:search s2))))
        (when (funcall endpm sequence2 statem limitm from-endm)
          (return nil))
        (setq statem (funcall stepm sequence2 statem from-endm))))))

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
          (declare (truly-dynamic-extent #'finish))
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
          (declare (truly-dynamic-extent #'finish))
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
          (declare (truly-dynamic-extent #'finish))
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
  (declare (truly-dynamic-extent args))
  (declare (ignore from-end test test-not start end count key))
  (let ((result (copy-seq sequence)))
    (apply #'sequence:delete item result args)))

(defgeneric sequence:remove-if
    (predicate sequence &key from-end start end count key)
  (:argument-precedence-order sequence predicate))
(defmethod sequence:remove-if (predicate (sequence sequence) &rest args &key
                               from-end (start 0) end count key)
  (declare (truly-dynamic-extent args))
  (declare (ignore from-end start end count key))
  (let ((result (copy-seq sequence)))
    (apply #'sequence:delete-if predicate result args)))

(defgeneric sequence:remove-if-not
    (predicate sequence &key from-end start end count key)
  (:argument-precedence-order sequence predicate))
(defmethod sequence:remove-if-not (predicate (sequence sequence) &rest args
                                   &key from-end (start 0) end count key)
  (declare (truly-dynamic-extent args))
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
          (declare (truly-dynamic-extent #'finish))
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
  (declare (truly-dynamic-extent args))
  (declare (ignore from-end test test-not start end key))
  (let ((result (copy-seq sequence)))
    (apply #'sequence:delete-duplicates result args)))

(defgeneric sequence:sort (sequence predicate &key key))
(defmethod sequence:sort ((sequence sequence) predicate &rest args &key key)
  (declare (truly-dynamic-extent args))
  (declare (ignore key))
  (let* ((length (length sequence))
         (vector (make-array length)))
    (sequence:with-sequence-iterator (state limit from-end step endp elt)
        (sequence)
      (declare (ignore limit  endp))
      (do ((i 0 (1+ i)))
          ((>= i length))
        (setf (aref vector i) (funcall elt sequence state))
        (setq state (funcall step sequence state from-end))))
    (apply #'sort vector predicate args)
    (sequence:with-sequence-iterator (state limit from-end step endp elt setelt)
        (sequence)
      (declare (ignore limit endp elt))
      (do ((i 0 (1+ i)))
          ((>= i length) sequence)
        (funcall setelt (aref vector i) sequence state)
        (setq state (funcall step sequence state from-end))))))

(defgeneric sequence:stable-sort (sequence predicate &key key))
(defmethod sequence:stable-sort
    ((sequence sequence) predicate &rest args &key key)
  (declare (truly-dynamic-extent args))
  (declare (ignore key))
  (let* ((length (length sequence))
         (vector (make-array length)))
    (sequence:with-sequence-iterator (state limit from-end step endp elt)
        (sequence)
      (declare (ignore limit  endp))
      (do ((i 0 (1+ i)))
          ((>= i length))
        (setf (aref vector i) (funcall elt sequence state))
        (setq state (funcall step sequence state from-end))))
    (apply #'stable-sort vector predicate args)
    (sequence:with-sequence-iterator (state limit from-end step endp elt setelt)
        (sequence)
      (declare (ignore limit endp elt))
      (do ((i 0 (1+ i)))
          ((>= i length) sequence)
        (funcall setelt (aref vector i) sequence state)
        (setq state (funcall step sequence state from-end))))))
