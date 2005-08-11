;;;; generic SEQUENCEs
;;;;
;;;; KLUDGE: comment from original CMU CL source:
;;;;   Be careful when modifying code. A lot of the structure of the
;;;;   code is affected by the fact that compiler transforms use the
;;;;   lower level support functions. If transforms are written for
;;;;   some sequence operation, note how the END argument is handled
;;;;   in other operations with transforms.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;;; utilities

(eval-when (:compile-toplevel)

(defparameter *sequence-keyword-info*
  ;; (name default supplied-p adjustment new-type)
  `((count nil
           nil
           (etypecase count
             (null (1- most-positive-fixnum))
             (fixnum (max 0 count))
             (integer (if (minusp count)
                          0
                          (1- most-positive-fixnum))))
           (mod #.sb!xc:most-positive-fixnum))
    ,@(mapcan (lambda (names)
                (destructuring-bind (start end length sequence) names
                  (list
                   `(,start
                     0
                     nil
                     (if (<= 0 ,start ,length)
                         ,start
                         (signal-bounding-indices-bad-error ,sequence
                                                            ,start ,end))
                     index)
                  `(,end
                    nil
                    nil
                    (if (or (null ,end) (<= ,start ,end ,length))
                        ;; Defaulting of NIL is done inside the
                        ;; bodies, for ease of sharing with compiler
                        ;; transforms.
                        ;;
                        ;; FIXME: defend against non-number non-NIL
                        ;; stuff?
                        ,end
                        (signal-bounding-indices-bad-error ,sequence
                                                           ,start ,end))
                    (or null index)))))
              '((start end length sequence)
                (start1 end1 length1 sequence1)
                (start2 end2 length2 sequence2)))
    (key nil
         nil
         (and key (%coerce-callable-to-fun key))
         (or null function))
    (test #'eql
          nil
          (%coerce-callable-to-fun test)
          function)
    (test-not nil
              nil
              (and test-not (%coerce-callable-to-fun test-not))
              (or null function))
    ))

(sb!xc:defmacro define-sequence-traverser (name args &body body)
  (multiple-value-bind (body declarations docstring)
      (parse-body body :doc-string-allowed t)
    (collect ((new-args) (new-declarations) (adjustments))
      (dolist (arg args)
        (case arg
          ;; FIXME: make this robust.  And clean.
          ((sequence)
           (new-args arg)
           (adjustments '(length (etypecase sequence
                                   (list (length sequence))
                                   (vector (length sequence)))))
           (new-declarations '(type index length)))
          ((sequence1)
           (new-args arg)
           (adjustments '(length1 (etypecase sequence1
                                    (list (length sequence1))
                                    (vector (length sequence1)))))
           (new-declarations '(type index length1)))
          ((sequence2)
           (new-args arg)
           (adjustments '(length2 (etypecase sequence2
                                    (list (length sequence2))
                                    (vector (length sequence2)))))
           (new-declarations '(type index length2)))
          ((function predicate)
           (new-args arg)
           (adjustments `(,arg (%coerce-callable-to-fun ,arg))))
          (t (let ((info (cdr (assoc arg *sequence-keyword-info*))))
               (cond (info
                      (destructuring-bind (default supplied-p adjuster type) info
                        (new-args `(,arg ,default ,@(when supplied-p (list supplied-p))))
                        (adjustments `(,arg ,adjuster))
                        (new-declarations `(type ,type ,arg))))
                     (t (new-args arg)))))))
      `(defun ,name ,(new-args)
         ,@(when docstring (list docstring))
         ,@declarations
         (let* (,@(adjustments))
           (declare ,@(new-declarations))
           ,@body)))))

;;; SEQ-DISPATCH does an efficient type-dispatch on the given SEQUENCE.
;;;
;;; FIXME: It might be worth making three cases here, LIST,
;;; SIMPLE-VECTOR, and VECTOR, instead of the current LIST and VECTOR.
;;; It tends to make code run faster but be bigger; some benchmarking
;;; is needed to decide.
(sb!xc:defmacro seq-dispatch (sequence list-form array-form)
  `(if (listp ,sequence)
       ,list-form
       ,array-form))

(sb!xc:defmacro make-sequence-like (sequence length)
  #!+sb-doc
  "Return a sequence of the same type as SEQUENCE and the given LENGTH."
  `(if (typep ,sequence 'list)
       (make-list ,length)
       (progn
         ;; This is only called from places which have already deduced
         ;; that the SEQUENCE argument is actually a sequence.  So
         ;; this would be a candidate place for (AVER (TYPEP ,SEQUENCE
         ;; 'VECTOR)), except that this seems to be a performance
         ;; hotspot.
         (make-array ,length
                     :element-type (array-element-type ,sequence)))))

(sb!xc:defmacro bad-sequence-type-error (type-spec)
  `(error 'simple-type-error
          :datum ,type-spec
          :expected-type '(satisfies is-a-valid-sequence-type-specifier-p)
          :format-control "~S is a bad type specifier for sequences."
          :format-arguments (list ,type-spec)))

(sb!xc:defmacro sequence-type-length-mismatch-error (type length)
  `(error 'simple-type-error
          :datum ,length
          :expected-type (cond ((array-type-p ,type)
                                `(eql ,(car (array-type-dimensions ,type))))
                               ((type= ,type (specifier-type 'null))
                                '(eql 0))
                               ((cons-type-p ,type)
                                '(integer 1))
                               (t (bug "weird type in S-T-L-M-ERROR")))
          ;; FIXME: this format control causes ugly printing.  There's
          ;; probably some ~<~@:_~> incantation that would make it
          ;; nicer. -- CSR, 2002-10-18
          :format-control "The length requested (~S) does not match the type restriction in ~S."
          :format-arguments (list ,length (type-specifier ,type))))

(sb!xc:defmacro sequence-type-too-hairy (type-spec)
  ;; FIXME: Should this be a BUG? I'm inclined to think not; there are
  ;; words that give some but not total support to this position in
  ;; ANSI.  Essentially, we are justified in throwing this on
  ;; e.g. '(OR SIMPLE-VECTOR (VECTOR FIXNUM)), but maybe not (by ANSI)
  ;; on '(CONS * (CONS * NULL)) -- CSR, 2002-10-18

  ;; On the other hand, I'm not sure it deserves to be a type-error,
  ;; either. -- bem, 2005-08-10
  `(error 'simple-program-error
          :format-control "~S is too hairy for sequence functions."
          :format-arguments (list ,type-spec)))
) ; EVAL-WHEN

(defun is-a-valid-sequence-type-specifier-p (type)
  (let ((type (specifier-type type)))
    (or (csubtypep type (specifier-type 'list))
        (csubtypep type (specifier-type 'vector)))))

;;; It's possible with some sequence operations to declare the length
;;; of a result vector, and to be safe, we really ought to verify that
;;; the actual result has the declared length.
(defun vector-of-checked-length-given-length (vector declared-length)
  (declare (type vector vector))
  (declare (type index declared-length))
  (let ((actual-length (length vector)))
    (unless (= actual-length declared-length)
      (error 'simple-type-error
             :datum vector
             :expected-type `(vector ,declared-length)
             :format-control
             "Vector length (~W) doesn't match declared length (~W)."
             :format-arguments (list actual-length declared-length))))
  vector)
(defun sequence-of-checked-length-given-type (sequence result-type)
  (let ((ctype (specifier-type result-type)))
    (if (not (array-type-p ctype))
        sequence
        (let ((declared-length (first (array-type-dimensions ctype))))
          (if (eq declared-length '*)
              sequence
              (vector-of-checked-length-given-length sequence
                                                     declared-length))))))

(declaim (ftype (function (sequence index) nil) signal-index-too-large-error))
(defun signal-index-too-large-error (sequence index)
  (let* ((length (length sequence))
         (max-index (and (plusp length)
                         (1- length))))
    (error 'index-too-large-error
           :datum index
           :expected-type (if max-index
                              `(integer 0 ,max-index)
                              ;; This seems silly, is there something better?
                              '(integer 0 (0))))))

(defun signal-bounding-indices-bad-error (sequence start end)
  (let ((length (length sequence)))
    (error 'bounding-indices-bad-error
           :datum (cons start end)
           :expected-type `(cons (integer 0 ,length)
                                 (or null (integer ,start ,length)))
           :object sequence)))

(defun elt (sequence index)
  #!+sb-doc "Return the element of SEQUENCE specified by INDEX."
  (etypecase sequence
    (list
     (do ((count index (1- count))
          (list sequence (cdr list)))
         ((= count 0)
          (if (endp list)
              (signal-index-too-large-error sequence index)
              (car list)))
       (declare (type (integer 0) count))))
    (vector
     (when (>= index (length sequence))
       (signal-index-too-large-error sequence index))
     (aref sequence index))))

(defun %setelt (sequence index newval)
  #!+sb-doc "Store NEWVAL as the component of SEQUENCE specified by INDEX."
  (etypecase sequence
    (list
     (do ((count index (1- count))
          (seq sequence))
         ((= count 0) (rplaca seq newval) newval)
       (declare (fixnum count))
       (if (atom (cdr seq))
           (signal-index-too-large-error sequence index)
           (setq seq (cdr seq)))))
    (vector
     (when (>= index (length sequence))
       (signal-index-too-large-error sequence index))
     (setf (aref sequence index) newval))))

(defun length (sequence)
  #!+sb-doc "Return an integer that is the length of SEQUENCE."
  (etypecase sequence
    (vector (length (truly-the vector sequence)))
    (list (length (truly-the list sequence)))))

(defun make-sequence (type length &key (initial-element nil iep))
  #!+sb-doc
  "Return a sequence of the given TYPE and LENGTH, with elements initialized
  to INITIAL-ELEMENT."
  (declare (fixnum length))
  (let* ((adjusted-type
          (typecase type
            (atom (cond
                    ((eq type 'string) '(vector character))
                    ((eq type 'simple-string) '(simple-array character (*)))
                    (t type)))
            (cons (cond
                    ((eq (car type) 'string) `(vector character ,@(cdr type)))
                    ((eq (car type) 'simple-string)
                     `(simple-array character ,(if (cdr type)
                                                   (cdr type)
                                                   '(*))))
                    (t type)))
            (t type)))
         (type (specifier-type adjusted-type)))
    (cond ((csubtypep type (specifier-type 'list))
           (cond
             ((type= type (specifier-type 'list))
              (make-list length :initial-element initial-element))
             ((eq type *empty-type*)
              (bad-sequence-type-error nil))
             ((type= type (specifier-type 'null))
              (if (= length 0)
                  'nil
                  (sequence-type-length-mismatch-error type length)))
             ((cons-type-p type)
              (multiple-value-bind (min exactp)
                  (sb!kernel::cons-type-length-info type)
                (if exactp
                    (unless (= length min)
                      (sequence-type-length-mismatch-error type length))
                    (unless (>= length min)
                      (sequence-type-length-mismatch-error type length)))
                (make-list length :initial-element initial-element)))
             ;; We'll get here for e.g. (OR NULL (CONS INTEGER *)),
             ;; which may seem strange and non-ideal, but then I'd say
             ;; it was stranger to feed that type in to MAKE-SEQUENCE.
             (t (sequence-type-too-hairy (type-specifier type)))))
          ((csubtypep type (specifier-type 'vector))
           (cond
             (;; is it immediately obvious what the result type is?
              (typep type 'array-type)
              (progn
                (aver (= (length (array-type-dimensions type)) 1))
                (let* ((etype (type-specifier
                               (array-type-specialized-element-type type)))
                       (etype (if (eq etype '*) t etype))
                       (type-length (car (array-type-dimensions type))))
                  (unless (or (eq type-length '*)
                              (= type-length length))
                    (sequence-type-length-mismatch-error type length))
                  ;; FIXME: These calls to MAKE-ARRAY can't be
                  ;; open-coded, as the :ELEMENT-TYPE argument isn't
                  ;; constant.  Probably we ought to write a
                  ;; DEFTRANSFORM for MAKE-SEQUENCE.  -- CSR,
                  ;; 2002-07-22
                  (if iep
                      (make-array length :element-type etype
                                  :initial-element initial-element)
                      (make-array length :element-type etype)))))
             (t (sequence-type-too-hairy (type-specifier type)))))
          (t (bad-sequence-type-error (type-specifier type))))))

;;;; SUBSEQ
;;;;
;;;; The support routines for SUBSEQ are used by compiler transforms,
;;;; so we worry about dealing with END being supplied or defaulting
;;;; to NIL at this level.

(defun vector-subseq* (sequence start &optional end)
  (declare (type vector sequence))
  (declare (type index start))
  (declare (type (or null index) end))
  (when (null end)
    (setf end (length sequence)))
  (unless (<= 0 start end (length sequence))
    (signal-bounding-indices-bad-error sequence start end))
  (do ((old-index start (1+ old-index))
       (new-index 0 (1+ new-index))
       (copy (make-sequence-like sequence (- end start))))
      ((= old-index end) copy)
    (declare (fixnum old-index new-index))
    (setf (aref copy new-index)
          (aref sequence old-index))))

(defun list-subseq* (sequence start &optional end)
  (declare (type list sequence))
  ;; the INDEX declaration isn't actually mandatory, but it's true for
  ;; all practical purposes.
  (declare (type index start))
  (declare (type (or null index) end))
  (do ((list sequence (cdr list))
       (index 0 (1+ index))
       (result nil))
      (nil)
    (cond
      ((null list) (if (or (and end (> end index))
                           (< index start))
                       (signal-bounding-indices-bad-error sequence start end)
                       (return (nreverse result))))
      ((< index start) nil)
      ((and end (= index end)) (return (nreverse result)))
      (t (push (car list) result)))))

(defun subseq (sequence start &optional end)
  #!+sb-doc
  "Return a copy of a subsequence of SEQUENCE starting with element number
   START and continuing to the end of SEQUENCE or the optional END."
  (seq-dispatch sequence
                (list-subseq* sequence start end)
                (vector-subseq* sequence start end)))

;;;; COPY-SEQ

(eval-when (:compile-toplevel :execute)

(sb!xc:defmacro vector-copy-seq (sequence)
  `(let ((length (length (the vector ,sequence))))
     (declare (fixnum length))
     (do ((index 0 (1+ index))
          (copy (make-sequence-like ,sequence length)))
         ((= index length) copy)
       (declare (fixnum index))
       (setf (aref copy index) (aref ,sequence index)))))

(sb!xc:defmacro list-copy-seq (list)
  `(if (atom ,list) '()
       (let ((result (cons (car ,list) '()) ))
         (do ((x (cdr ,list) (cdr x))
              (splice result
                      (cdr (rplacd splice (cons (car x) '() ))) ))
             ((atom x) (unless (null x)
                               (rplacd splice x))
                       result)))))

) ; EVAL-WHEN

(defun copy-seq (sequence)
  #!+sb-doc "Return a copy of SEQUENCE which is EQUAL to SEQUENCE but not EQ."
  (seq-dispatch sequence
                (list-copy-seq* sequence)
                (vector-copy-seq* sequence)))

;;; internal frobs

(defun list-copy-seq* (sequence)
  (list-copy-seq sequence))

(defun vector-copy-seq* (sequence)
  (declare (type vector sequence))
  (vector-copy-seq sequence))

;;;; FILL

(eval-when (:compile-toplevel :execute)

(sb!xc:defmacro vector-fill (sequence item start end)
  `(do ((index ,start (1+ index)))
       ((= index (the fixnum ,end)) ,sequence)
     (declare (fixnum index))
     (setf (aref ,sequence index) ,item)))

(sb!xc:defmacro list-fill (sequence item start end)
  `(do ((current (nthcdr ,start ,sequence) (cdr current))
        (index ,start (1+ index)))
       ((or (atom current) (and end (= index (the fixnum ,end))))
        sequence)
     (declare (fixnum index))
     (rplaca current ,item)))

) ; EVAL-WHEN

;;; The support routines for FILL are used by compiler transforms, so we
;;; worry about dealing with END being supplied or defaulting to NIL
;;; at this level.

(defun list-fill* (sequence item start end)
  (declare (list sequence))
  (list-fill sequence item start end))

(defun vector-fill* (sequence item start end)
  (declare (vector sequence))
  (when (null end) (setq end (length sequence)))
  (vector-fill sequence item start end))

(define-sequence-traverser fill (sequence item &key start end)
  #!+sb-doc "Replace the specified elements of SEQUENCE with ITEM."
  (seq-dispatch sequence
                (list-fill* sequence item start end)
                (vector-fill* sequence item start end)))

;;;; REPLACE

(eval-when (:compile-toplevel :execute)

;;; If we are copying around in the same vector, be careful not to copy the
;;; same elements over repeatedly. We do this by copying backwards.
(sb!xc:defmacro mumble-replace-from-mumble ()
  `(if (and (eq target-sequence source-sequence) (> target-start source-start))
       (let ((nelts (min (- target-end target-start)
                         (- source-end source-start))))
         (do ((target-index (+ (the fixnum target-start) (the fixnum nelts) -1)
                            (1- target-index))
              (source-index (+ (the fixnum source-start) (the fixnum nelts) -1)
                            (1- source-index)))
             ((= target-index (the fixnum (1- target-start))) target-sequence)
           (declare (fixnum target-index source-index))
           ;; disable bounds checking
           (declare (optimize (safety 0)))
           (setf (aref target-sequence target-index)
                 (aref source-sequence source-index))))
       (do ((target-index target-start (1+ target-index))
            (source-index source-start (1+ source-index)))
           ((or (= target-index (the fixnum target-end))
                (= source-index (the fixnum source-end)))
            target-sequence)
         (declare (fixnum target-index source-index))
         ;; disable bounds checking
         (declare (optimize (safety 0)))
         (setf (aref target-sequence target-index)
               (aref source-sequence source-index)))))

(sb!xc:defmacro list-replace-from-list ()
  `(if (and (eq target-sequence source-sequence) (> target-start source-start))
       (let ((new-elts (subseq source-sequence source-start
                               (+ (the fixnum source-start)
                                  (the fixnum
                                       (min (- (the fixnum target-end)
                                               (the fixnum target-start))
                                            (- (the fixnum source-end)
                                               (the fixnum source-start))))))))
         (do ((n new-elts (cdr n))
              (o (nthcdr target-start target-sequence) (cdr o)))
             ((null n) target-sequence)
           (rplaca o (car n))))
       (do ((target-index target-start (1+ target-index))
            (source-index source-start (1+ source-index))
            (target-sequence-ref (nthcdr target-start target-sequence)
                                 (cdr target-sequence-ref))
            (source-sequence-ref (nthcdr source-start source-sequence)
                                 (cdr source-sequence-ref)))
           ((or (= target-index (the fixnum target-end))
                (= source-index (the fixnum source-end))
                (null target-sequence-ref) (null source-sequence-ref))
            target-sequence)
         (declare (fixnum target-index source-index))
         (rplaca target-sequence-ref (car source-sequence-ref)))))

(sb!xc:defmacro list-replace-from-mumble ()
  `(do ((target-index target-start (1+ target-index))
        (source-index source-start (1+ source-index))
        (target-sequence-ref (nthcdr target-start target-sequence)
                             (cdr target-sequence-ref)))
       ((or (= target-index (the fixnum target-end))
            (= source-index (the fixnum source-end))
            (null target-sequence-ref))
        target-sequence)
     (declare (fixnum source-index target-index))
     (rplaca target-sequence-ref (aref source-sequence source-index))))

(sb!xc:defmacro mumble-replace-from-list ()
  `(do ((target-index target-start (1+ target-index))
        (source-index source-start (1+ source-index))
        (source-sequence (nthcdr source-start source-sequence)
                         (cdr source-sequence)))
       ((or (= target-index (the fixnum target-end))
            (= source-index (the fixnum source-end))
            (null source-sequence))
        target-sequence)
     (declare (fixnum target-index source-index))
     (setf (aref target-sequence target-index) (car source-sequence))))

) ; EVAL-WHEN

;;;; The support routines for REPLACE are used by compiler transforms, so we
;;;; worry about dealing with END being supplied or defaulting to NIL
;;;; at this level.

(defun list-replace-from-list* (target-sequence source-sequence target-start
                                target-end source-start source-end)
  (when (null target-end) (setq target-end (length target-sequence)))
  (when (null source-end) (setq source-end (length source-sequence)))
  (list-replace-from-list))

(defun list-replace-from-vector* (target-sequence source-sequence target-start
                                  target-end source-start source-end)
  (when (null target-end) (setq target-end (length target-sequence)))
  (when (null source-end) (setq source-end (length source-sequence)))
  (list-replace-from-mumble))

(defun vector-replace-from-list* (target-sequence source-sequence target-start
                                  target-end source-start source-end)
  (when (null target-end) (setq target-end (length target-sequence)))
  (when (null source-end) (setq source-end (length source-sequence)))
  (mumble-replace-from-list))

(defun vector-replace-from-vector* (target-sequence source-sequence
                                    target-start target-end source-start
                                    source-end)
  (when (null target-end) (setq target-end (length target-sequence)))
  (when (null source-end) (setq source-end (length source-sequence)))
  (mumble-replace-from-mumble))

#!+sb-unicode
(defun simple-character-string-replace-from-simple-character-string*
    (target-sequence source-sequence
     target-start target-end source-start source-end)
  (declare (type (simple-array character (*)) target-sequence source-sequence))
  (when (null target-end) (setq target-end (length target-sequence)))
  (when (null source-end) (setq source-end (length source-sequence)))
  (mumble-replace-from-mumble))

(define-sequence-traverser replace
    (sequence1 sequence2 &key start1 end1 start2 end2)
  #!+sb-doc
  "The target sequence is destructively modified by copying successive
   elements into it from the source sequence."
  (let* (;; KLUDGE: absent either rewriting FOO-REPLACE-FROM-BAR, or
         ;; excessively polluting DEFINE-SEQUENCE-TRAVERSER, we rebind
         ;; these things here so that legacy code gets the names it's
         ;; expecting.  We could use &AUX instead :-/.
         (target-sequence sequence1)
         (source-sequence sequence2)
         (target-start start1)
         (source-start start2)
         (target-end (or end1 length1))
         (source-end (or end2 length2)))
    (seq-dispatch target-sequence
                  (seq-dispatch source-sequence
                                (list-replace-from-list)
                                (list-replace-from-mumble))
                  (seq-dispatch source-sequence
                                (mumble-replace-from-list)
                                (mumble-replace-from-mumble)))))

;;;; REVERSE

(eval-when (:compile-toplevel :execute)

(sb!xc:defmacro vector-reverse (sequence)
  `(let ((length (length ,sequence)))
     (declare (fixnum length))
     (do ((forward-index 0 (1+ forward-index))
          (backward-index (1- length) (1- backward-index))
          (new-sequence (make-sequence-like sequence length)))
         ((= forward-index length) new-sequence)
       (declare (fixnum forward-index backward-index))
       (setf (aref new-sequence forward-index)
             (aref ,sequence backward-index)))))

(sb!xc:defmacro list-reverse-macro (sequence)
  `(do ((new-list ()))
       ((endp ,sequence) new-list)
     (push (pop ,sequence) new-list)))

) ; EVAL-WHEN

(defun reverse (sequence)
  #!+sb-doc
  "Return a new sequence containing the same elements but in reverse order."
  (seq-dispatch sequence
                (list-reverse* sequence)
                (vector-reverse* sequence)))

;;; internal frobs

(defun list-reverse* (sequence)
  (list-reverse-macro sequence))

(defun vector-reverse* (sequence)
  (vector-reverse sequence))

;;;; NREVERSE

(eval-when (:compile-toplevel :execute)

(sb!xc:defmacro vector-nreverse (sequence)
  `(let ((length (length (the vector ,sequence))))
     (when (>= length 2)
       (do ((left-index 0 (1+ left-index))
            (right-index (1- length) (1- right-index)))
           ((<= right-index left-index))
         (declare (type index left-index right-index))
         (rotatef (aref ,sequence left-index)
                  (aref ,sequence right-index))))
     ,sequence))

(sb!xc:defmacro list-nreverse-macro (list)
  `(do ((1st (cdr ,list) (if (endp 1st) 1st (cdr 1st)))
        (2nd ,list 1st)
        (3rd '() 2nd))
       ((atom 2nd) 3rd)
     (rplacd 2nd 3rd)))

) ; EVAL-WHEN

(defun list-nreverse* (sequence)
  (list-nreverse-macro sequence))

(defun vector-nreverse* (sequence)
  (vector-nreverse sequence))

(defun nreverse (sequence)
  #!+sb-doc
  "Return a sequence of the same elements in reverse order; the argument
   is destroyed."
  (seq-dispatch sequence
                (list-nreverse* sequence)
                (vector-nreverse* sequence)))

;;;; CONCATENATE

(eval-when (:compile-toplevel :execute)

(sb!xc:defmacro concatenate-to-list (sequences)
  `(let ((result (list nil)))
     (do ((sequences ,sequences (cdr sequences))
          (splice result))
         ((null sequences) (cdr result))
       (let ((sequence (car sequences)))
         ;; FIXME: It appears to me that this and CONCATENATE-TO-MUMBLE
         ;; could benefit from a DO-SEQUENCE macro.
         (seq-dispatch sequence
                       (do ((sequence sequence (cdr sequence)))
                           ((atom sequence))
                         (setq splice
                               (cdr (rplacd splice (list (car sequence))))))
                       (do ((index 0 (1+ index))
                            (length (length sequence)))
                           ((= index length))
                         (declare (fixnum index length))
                         (setq splice
                               (cdr (rplacd splice
                                            (list (aref sequence index)))))))))))

(sb!xc:defmacro concatenate-to-mumble (output-type-spec sequences)
  `(do ((seqs ,sequences (cdr seqs))
        (total-length 0)
        (lengths ()))
       ((null seqs)
        (do ((sequences ,sequences (cdr sequences))
             (lengths lengths (cdr lengths))
             (index 0)
             (result (make-sequence ,output-type-spec total-length)))
            ((= index total-length) result)
          (declare (fixnum index))
          (let ((sequence (car sequences)))
            (seq-dispatch sequence
                          (do ((sequence sequence (cdr sequence)))
                              ((atom sequence))
                            (setf (aref result index) (car sequence))
                            (setq index (1+ index)))
                          (do ((jndex 0 (1+ jndex))
                               (this-length (car lengths)))
                              ((= jndex this-length))
                            (declare (fixnum jndex this-length))
                            (setf (aref result index)
                                  (aref sequence jndex))
                            (setq index (1+ index)))))))
     (let ((length (length (car seqs))))
       (declare (fixnum length))
       (setq lengths (nconc lengths (list length)))
       (setq total-length (+ total-length length)))))

) ; EVAL-WHEN

(defun concatenate (output-type-spec &rest sequences)
  #!+sb-doc
  "Return a new sequence of all the argument sequences concatenated together
  which shares no structure with the original argument sequences of the
  specified OUTPUT-TYPE-SPEC."
  (let ((type (specifier-type output-type-spec)))
  (cond
    ((csubtypep type (specifier-type 'list))
     (cond
       ((type= type (specifier-type 'list))
        (apply #'concat-to-list* sequences))
       ((eq type *empty-type*)
        (bad-sequence-type-error nil))
       ((type= type (specifier-type 'null))
        (if (every (lambda (x) (or (null x)
                                   (and (vectorp x) (= (length x) 0))))
                   sequences)
            'nil
            (sequence-type-length-mismatch-error
             type
             ;; FIXME: circular list issues.
             (reduce #'+ sequences :key #'length))))
       ((cons-type-p type)
        (multiple-value-bind (min exactp)
            (sb!kernel::cons-type-length-info type)
          (let ((length (reduce #'+ sequences :key #'length)))
            (if exactp
                (unless (= length min)
                  (sequence-type-length-mismatch-error type length))
                (unless (>= length min)
                  (sequence-type-length-mismatch-error type length)))
            (apply #'concat-to-list* sequences))))
       (t (sequence-type-too-hairy (type-specifier type)))))
    ((csubtypep type (specifier-type 'vector))
     (apply #'concat-to-simple* output-type-spec sequences))
    (t
     (bad-sequence-type-error output-type-spec)))))

;;; internal frobs
;;; FIXME: These are weird. They're never called anywhere except in
;;; CONCATENATE. It seems to me that the macros ought to just
;;; be expanded directly in CONCATENATE, or in CONCATENATE-STRING
;;; and CONCATENATE-LIST variants. Failing that, these ought to be local
;;; functions (FLET).
(defun concat-to-list* (&rest sequences)
  (concatenate-to-list sequences))
(defun concat-to-simple* (type &rest sequences)
  (concatenate-to-mumble type sequences))

;;;; MAP and MAP-INTO

;;; helper functions to handle arity-1 subcases of MAP
(declaim (ftype (function (function sequence) list) %map-list-arity-1))
(declaim (ftype (function (function sequence) simple-vector)
                %map-simple-vector-arity-1))
(macrolet ((dosequence ((i sequence) &body body)
             (once-only ((sequence sequence))
               `(etypecase ,sequence
                  (list (dolist (,i ,sequence) ,@body))
                  (simple-vector (dovector (,i sequence) ,@body))
                  (vector (dovector (,i sequence) ,@body))))))
  (defun %map-to-list-arity-1 (fun sequence)
    (let ((reversed-result nil)
          (really-fun (%coerce-callable-to-fun fun)))
      (dosequence (element sequence)
        (push (funcall really-fun element)
              reversed-result))
      (nreverse reversed-result)))
  (defun %map-to-simple-vector-arity-1 (fun sequence)
    (let ((result (make-array (length sequence)))
          (index 0)
          (really-fun (%coerce-callable-to-fun fun)))
      (declare (type index index))
      (dosequence (element sequence)
        (setf (aref result index)
              (funcall really-fun element))
        (incf index))
      result))
  (defun %map-for-effect-arity-1 (fun sequence)
    (let ((really-fun (%coerce-callable-to-fun fun)))
      (dosequence (element sequence)
        (funcall really-fun element)))
    nil))

;;; helper functions to handle arity-N subcases of MAP
;;;
;;; KLUDGE: This is hairier, and larger, than need be, because we
;;; don't have DYNAMIC-EXTENT. With DYNAMIC-EXTENT, we could define
;;; %MAP-FOR-EFFECT, and then implement the
;;; other %MAP-TO-FOO functions reasonably efficiently by passing closures to
;;; %MAP-FOR-EFFECT. (DYNAMIC-EXTENT would help a little by avoiding
;;; consing each closure, and would help a lot by allowing us to define
;;; a closure (LAMBDA (&REST REST) <do something with (APPLY FUN REST)>)
;;; with the REST list allocated with DYNAMIC-EXTENT. -- WHN 20000920
(macrolet (;; Execute BODY in a context where the machinery for
           ;; UPDATED-MAP-APPLY-ARGS has been set up.
           (with-map-state (sequences &body body)
             `(let* ((%sequences ,sequences)
                     (%iters (mapcar (lambda (sequence)
                                       (etypecase sequence
                                         (list sequence)
                                         (vector 0)))
                                     %sequences))
                     (%apply-args (make-list (length %sequences))))
                (declare (type list %sequences %iters %apply-args))
                ,@body))
           ;; Return a list of args to pass to APPLY for the next
           ;; function call in the mapping, or NIL if no more function
           ;; calls should be made (because we've reached the end of a
           ;; sequence arg).
           (updated-map-apply-args ()
             '(do ((in-sequences  %sequences  (cdr in-sequences))
                   (in-iters      %iters      (cdr in-iters))
                   (in-apply-args %apply-args (cdr in-apply-args)))
                  ((null in-sequences)
                   %apply-args)
                (declare (type list in-sequences in-iters in-apply-args))
                (let ((i (car in-iters)))
                  (declare (type (or list index) i))
                  (if (listp i)
                      (if (null i)      ; if end of this sequence
                          (return nil)
                          (setf (car in-apply-args) (car i)
                                (car in-iters) (cdr i)))
                      (let ((v (the vector (car in-sequences))))
                        (if (>= i (length v)) ; if end of this sequence
                            (return nil)
                            (setf (car in-apply-args) (aref v i)
                                  (car in-iters) (1+ i)))))))))
  (defun %map-to-list (func sequences)
    (declare (type function func))
    (declare (type list sequences))
    (with-map-state sequences
      (loop with updated-map-apply-args
            while (setf updated-map-apply-args (updated-map-apply-args))
            collect (apply func updated-map-apply-args))))
  (defun %map-to-vector (output-type-spec func sequences)
    (declare (type function func))
    (declare (type list sequences))
    (let ((min-len (with-map-state sequences
                     (do ((counter 0 (1+ counter)))
                         ;; Note: Doing everything in
                         ;; UPDATED-MAP-APPLY-ARGS here is somewhat
                         ;; wasteful; we even do some extra consing.
                         ;; And stepping over every element of
                         ;; VECTORs, instead of just grabbing their
                         ;; LENGTH, is also wasteful. But it's easy
                         ;; and safe. (If you do rewrite it, please
                         ;; try to make sure that
                         ;;   (MAP NIL #'F SOME-CIRCULAR-LIST #(1))
                         ;; does the right thing.)
                         ((not (updated-map-apply-args))
                          counter)
                       (declare (type index counter))))))
      (declare (type index min-len))
      (with-map-state sequences
        (let ((result (make-sequence output-type-spec min-len))
              (index 0))
          (declare (type index index))
          (loop with updated-map-apply-args
                while (setf updated-map-apply-args (updated-map-apply-args))
                do
                (setf (aref result index)
                      (apply func updated-map-apply-args))
                (incf index))
          result))))
  (defun %map-for-effect (func sequences)
    (declare (type function func))
    (declare (type list sequences))
    (with-map-state sequences
      (loop with updated-map-apply-args
            while (setf updated-map-apply-args (updated-map-apply-args))
            do
            (apply func updated-map-apply-args))
      nil)))

  "FUNCTION must take as many arguments as there are sequences provided.
  The result is a sequence of type OUTPUT-TYPE-SPEC such that element I
  is the result of applying FUNCTION to element I of each of the argument
  sequences."

;;; %MAP is just MAP without the final just-to-be-sure check that
;;; length of the output sequence matches any length specified
;;; in RESULT-TYPE.
(defun %map (result-type function first-sequence &rest more-sequences)
  (let ((really-fun (%coerce-callable-to-fun function))
        (type (specifier-type result-type)))
    ;; Handle one-argument MAP NIL specially, using ETYPECASE to turn
    ;; it into something which can be DEFTRANSFORMed away. (It's
    ;; fairly important to handle this case efficiently, since
    ;; quantifiers like SOME are transformed into this case, and since
    ;; there's no consing overhead to dwarf our inefficiency.)
    (if (and (null more-sequences)
             (null result-type))
        (%map-for-effect-arity-1 really-fun first-sequence)
        ;; Otherwise, use the industrial-strength full-generality
        ;; approach, consing O(N-ARGS) temporary storage (which can have
        ;; DYNAMIC-EXTENT), then using O(N-ARGS * RESULT-LENGTH) time.
        (let ((sequences (cons first-sequence more-sequences)))
          (cond
            ((eq type *empty-type*) (%map-for-effect really-fun sequences))
            ((csubtypep type (specifier-type 'list))
             (%map-to-list really-fun sequences))
            ((csubtypep type (specifier-type 'vector))
             (%map-to-vector result-type really-fun sequences))
            (t
             (bad-sequence-type-error result-type)))))))

(defun map (result-type function first-sequence &rest more-sequences)
  (apply #'%map
         result-type
         function
         first-sequence
         more-sequences))

;;; KLUDGE: MAP has been rewritten substantially since the fork from
;;; CMU CL in order to give reasonable performance, but this
;;; implementation of MAP-INTO still has the same problems as the old
;;; MAP code. Ideally, MAP-INTO should be rewritten to be efficient in
;;; the same way that the corresponding cases of MAP have been
;;; rewritten. Instead of doing it now, though, it's easier to wait
;;; until we have DYNAMIC-EXTENT, at which time it should become
;;; extremely easy to define a reasonably efficient MAP-INTO in terms
;;; of (MAP NIL ..). -- WHN 20000920
(defun map-into (result-sequence function &rest sequences)
  (let* ((fp-result
          (and (arrayp result-sequence)
               (array-has-fill-pointer-p result-sequence)))
         (len (apply #'min
                     (if fp-result
                         (array-dimension result-sequence 0)
                         (length result-sequence))
                     (mapcar #'length sequences))))

    (when fp-result
      (setf (fill-pointer result-sequence) len))

    (let ((really-fun (%coerce-callable-to-fun function)))
      (dotimes (index len)
        (setf (elt result-sequence index)
              (apply really-fun
                     (mapcar (lambda (seq) (elt seq index))
                             sequences))))))
  result-sequence)

;;;; quantifiers

;;; We borrow the logic from (MAP NIL ..) to handle iteration over
;;; arbitrary sequence arguments, both in the full call case and in
;;; the open code case.
(macrolet ((defquantifier (name found-test found-result
                                &key doc (unfound-result (not found-result)))
             `(progn
                ;; KLUDGE: It would be really nice if we could simply
                ;; do something like this
                ;;  (declaim (inline ,name))
                ;;  (defun ,name (pred first-seq &rest more-seqs)
                ;;    ,doc
                ;;    (flet ((map-me (&rest rest)
                ;;             (let ((pred-value (apply pred rest)))
                ;;               (,found-test pred-value
                ;;                 (return-from ,name
                ;;                   ,found-result)))))
                ;;      (declare (inline map-me))
                ;;      (apply #'map nil #'map-me first-seq more-seqs)
                ;;      ,unfound-result))
                ;; but Python doesn't seem to be smart enough about
                ;; inlining and APPLY to recognize that it can use
                ;; the DEFTRANSFORM for MAP in the resulting inline
                ;; expansion. I don't have any appetite for deep
                ;; compiler hacking right now, so I'll just work
                ;; around the apparent problem by using a compiler
                ;; macro instead. -- WHN 20000410
                (defun ,name (pred first-seq &rest more-seqs)
                  #!+sb-doc ,doc
                  (flet ((map-me (&rest rest)
                           (let ((pred-value (apply pred rest)))
                             (,found-test pred-value
                                          (return-from ,name
                                            ,found-result)))))
                    (declare (inline map-me))
                    (apply #'map nil #'map-me first-seq more-seqs)
                    ,unfound-result))
                ;; KLUDGE: It would be more obviously correct -- but
                ;; also significantly messier -- for PRED-VALUE to be
                ;; a gensym. However, a private symbol really does
                ;; seem to be good enough; and anyway the really
                ;; obviously correct solution is to make Python smart
                ;; enough that we can use an inline function instead
                ;; of a compiler macro (as above). -- WHN 20000410
                ;;
                ;; FIXME: The DEFINE-COMPILER-MACRO here can be
                ;; important for performance, and it'd be good to have
                ;; it be visible throughout the compilation of all the
                ;; target SBCL code. That could be done by defining
                ;; SB-XC:DEFINE-COMPILER-MACRO and using it here,
                ;; moving this DEFQUANTIFIER stuff (and perhaps other
                ;; inline definitions in seq.lisp as well) into a new
                ;; seq.lisp, and moving remaining target-only stuff
                ;; from the old seq.lisp into target-seq.lisp.
                (define-compiler-macro ,name (pred first-seq &rest more-seqs)
                  (let ((elements (make-gensym-list (1+ (length more-seqs))))
                        (blockname (gensym "BLOCK")))
                    (once-only ((pred pred))
                      `(block ,blockname
                         (map nil
                              (lambda (,@elements)
                                (let ((pred-value (funcall ,pred ,@elements)))
                                  (,',found-test pred-value
                                    (return-from ,blockname
                                      ,',found-result))))
                              ,first-seq
                              ,@more-seqs)
                         ,',unfound-result)))))))
  (defquantifier some when pred-value :unfound-result nil :doc
  "Apply PREDICATE to the 0-indexed elements of the sequences, then
   possibly to those with index 1, and so on. Return the first
   non-NIL value encountered, or NIL if the end of any sequence is reached.")
  (defquantifier every unless nil :doc
  "Apply PREDICATE to the 0-indexed elements of the sequences, then
   possibly to those with index 1, and so on. Return NIL as soon
   as any invocation of PREDICATE returns NIL, or T if every invocation
   is non-NIL.")
  (defquantifier notany when nil :doc
  "Apply PREDICATE to the 0-indexed elements of the sequences, then
   possibly to those with index 1, and so on. Return NIL as soon
   as any invocation of PREDICATE returns a non-NIL value, or T if the end
   of any sequence is reached.")
  (defquantifier notevery unless t :doc
  "Apply PREDICATE to 0-indexed elements of the sequences, then
   possibly to those with index 1, and so on. Return T as soon
   as any invocation of PREDICATE returns NIL, or NIL if every invocation
   is non-NIL."))

;;;; REDUCE

(eval-when (:compile-toplevel :execute)

(sb!xc:defmacro mumble-reduce (function
                               sequence
                               key
                               start
                               end
                               initial-value
                               ref)
  `(do ((index ,start (1+ index))
        (value ,initial-value))
       ((>= index ,end) value)
     (setq value (funcall ,function value
                          (apply-key ,key (,ref ,sequence index))))))

(sb!xc:defmacro mumble-reduce-from-end (function
                                        sequence
                                        key
                                        start
                                        end
                                        initial-value
                                        ref)
  `(do ((index (1- ,end) (1- index))
        (value ,initial-value)
        (terminus (1- ,start)))
       ((<= index terminus) value)
     (setq value (funcall ,function
                          (apply-key ,key (,ref ,sequence index))
                          value))))

(sb!xc:defmacro list-reduce (function
                             sequence
                             key
                             start
                             end
                             initial-value
                             ivp)
  `(let ((sequence (nthcdr ,start ,sequence)))
     (do ((count (if ,ivp ,start (1+ ,start))
                 (1+ count))
          (sequence (if ,ivp sequence (cdr sequence))
                    (cdr sequence))
          (value (if ,ivp ,initial-value (apply-key ,key (car sequence)))
                 (funcall ,function value (apply-key ,key (car sequence)))))
         ((>= count ,end) value))))

(sb!xc:defmacro list-reduce-from-end (function
                                      sequence
                                      key
                                      start
                                      end
                                      initial-value
                                      ivp)
  `(let ((sequence (nthcdr (- (length ,sequence) ,end)
                           (reverse ,sequence))))
     (do ((count (if ,ivp ,start (1+ ,start))
                 (1+ count))
          (sequence (if ,ivp sequence (cdr sequence))
                    (cdr sequence))
          (value (if ,ivp ,initial-value (apply-key ,key (car sequence)))
                 (funcall ,function (apply-key ,key (car sequence)) value)))
         ((>= count ,end) value))))

) ; EVAL-WHEN

(define-sequence-traverser reduce
    (function sequence &key key from-end start end (initial-value nil ivp))
  (declare (type index start))
  (let ((start start)
        (end (or end length)))
    (declare (type index start end))
    (cond ((= end start)
           (if ivp initial-value (funcall function)))
          ((listp sequence)
           (if from-end
               (list-reduce-from-end function sequence key start end
                                     initial-value ivp)
               (list-reduce function sequence key start end
                            initial-value ivp)))
          (from-end
           (when (not ivp)
             (setq end (1- (the fixnum end)))
             (setq initial-value (apply-key key (aref sequence end))))
           (mumble-reduce-from-end function sequence key start end
                                   initial-value aref))
          (t
           (when (not ivp)
             (setq initial-value (apply-key key (aref sequence start)))
             (setq start (1+ start)))
           (mumble-reduce function sequence key start end
                          initial-value aref)))))

;;;; DELETE

(eval-when (:compile-toplevel :execute)

(sb!xc:defmacro mumble-delete (pred)
  `(do ((index start (1+ index))
        (jndex start)
        (number-zapped 0))
       ((or (= index (the fixnum end)) (= number-zapped count))
        (do ((index index (1+ index))           ; Copy the rest of the vector.
             (jndex jndex (1+ jndex)))
            ((= index (the fixnum length))
             (shrink-vector sequence jndex))
          (declare (fixnum index jndex))
          (setf (aref sequence jndex) (aref sequence index))))
     (declare (fixnum index jndex number-zapped))
     (setf (aref sequence jndex) (aref sequence index))
     (if ,pred
         (incf number-zapped)
         (incf jndex))))

(sb!xc:defmacro mumble-delete-from-end (pred)
  `(do ((index (1- (the fixnum end)) (1- index)) ; Find the losers.
        (number-zapped 0)
        (losers ())
        this-element
        (terminus (1- start)))
       ((or (= index terminus) (= number-zapped count))
        (do ((losers losers)                     ; Delete the losers.
             (index start (1+ index))
             (jndex start))
            ((or (null losers) (= index (the fixnum end)))
             (do ((index index (1+ index))       ; Copy the rest of the vector.
                  (jndex jndex (1+ jndex)))
                 ((= index (the fixnum length))
                  (shrink-vector sequence jndex))
               (declare (fixnum index jndex))
               (setf (aref sequence jndex) (aref sequence index))))
          (declare (fixnum index jndex))
          (setf (aref sequence jndex) (aref sequence index))
          (if (= index (the fixnum (car losers)))
              (pop losers)
              (incf jndex))))
     (declare (fixnum index number-zapped terminus))
     (setq this-element (aref sequence index))
     (when ,pred
       (incf number-zapped)
       (push index losers))))

(sb!xc:defmacro normal-mumble-delete ()
  `(mumble-delete
    (if test-not
        (not (funcall test-not item (apply-key key (aref sequence index))))
        (funcall test item (apply-key key (aref sequence index))))))

(sb!xc:defmacro normal-mumble-delete-from-end ()
  `(mumble-delete-from-end
    (if test-not
        (not (funcall test-not item (apply-key key this-element)))
        (funcall test item (apply-key key this-element)))))

(sb!xc:defmacro list-delete (pred)
  `(let ((handle (cons nil sequence)))
     (do ((current (nthcdr start sequence) (cdr current))
          (previous (nthcdr start handle))
          (index start (1+ index))
          (number-zapped 0))
         ((or (= index (the fixnum end)) (= number-zapped count))
          (cdr handle))
       (declare (fixnum index number-zapped))
       (cond (,pred
              (rplacd previous (cdr current))
              (incf number-zapped))
             (t
              (setq previous (cdr previous)))))))

(sb!xc:defmacro list-delete-from-end (pred)
  `(let* ((reverse (nreverse (the list sequence)))
          (handle (cons nil reverse)))
     (do ((current (nthcdr (- (the fixnum length) (the fixnum end)) reverse)
                   (cdr current))
          (previous (nthcdr (- (the fixnum length) (the fixnum end)) handle))
          (index start (1+ index))
          (number-zapped 0))
         ((or (= index (the fixnum end)) (= number-zapped count))
          (nreverse (cdr handle)))
       (declare (fixnum index number-zapped))
       (cond (,pred
              (rplacd previous (cdr current))
              (incf number-zapped))
             (t
              (setq previous (cdr previous)))))))

(sb!xc:defmacro normal-list-delete ()
  '(list-delete
    (if test-not
        (not (funcall test-not item (apply-key key (car current))))
        (funcall test item (apply-key key (car current))))))

(sb!xc:defmacro normal-list-delete-from-end ()
  '(list-delete-from-end
    (if test-not
        (not (funcall test-not item (apply-key key (car current))))
        (funcall test item (apply-key key (car current))))))

) ; EVAL-WHEN

(define-sequence-traverser delete
    (item sequence &key from-end test test-not start
          end count key)
  #!+sb-doc
  "Return a sequence formed by destructively removing the specified ITEM from
  the given SEQUENCE."
  (declare (fixnum start))
  (let ((end (or end length)))
    (declare (type index end))
    (seq-dispatch sequence
                  (if from-end
                      (normal-list-delete-from-end)
                      (normal-list-delete))
                  (if from-end
                      (normal-mumble-delete-from-end)
                      (normal-mumble-delete)))))

(eval-when (:compile-toplevel :execute)

(sb!xc:defmacro if-mumble-delete ()
  `(mumble-delete
    (funcall predicate (apply-key key (aref sequence index)))))

(sb!xc:defmacro if-mumble-delete-from-end ()
  `(mumble-delete-from-end
    (funcall predicate (apply-key key this-element))))

(sb!xc:defmacro if-list-delete ()
  '(list-delete
    (funcall predicate (apply-key key (car current)))))

(sb!xc:defmacro if-list-delete-from-end ()
  '(list-delete-from-end
    (funcall predicate (apply-key key (car current)))))

) ; EVAL-WHEN

(define-sequence-traverser delete-if
    (predicate sequence &key from-end start key end count)
  #!+sb-doc
  "Return a sequence formed by destructively removing the elements satisfying
  the specified PREDICATE from the given SEQUENCE."
  (declare (fixnum start))
  (let ((end (or end length)))
    (declare (type index end))
    (seq-dispatch sequence
                  (if from-end
                      (if-list-delete-from-end)
                      (if-list-delete))
                  (if from-end
                      (if-mumble-delete-from-end)
                      (if-mumble-delete)))))

(eval-when (:compile-toplevel :execute)

(sb!xc:defmacro if-not-mumble-delete ()
  `(mumble-delete
    (not (funcall predicate (apply-key key (aref sequence index))))))

(sb!xc:defmacro if-not-mumble-delete-from-end ()
  `(mumble-delete-from-end
    (not (funcall predicate (apply-key key this-element)))))

(sb!xc:defmacro if-not-list-delete ()
  '(list-delete
    (not (funcall predicate (apply-key key (car current))))))

(sb!xc:defmacro if-not-list-delete-from-end ()
  '(list-delete-from-end
    (not (funcall predicate (apply-key key (car current))))))

) ; EVAL-WHEN

(define-sequence-traverser delete-if-not
    (predicate sequence &key from-end start end key count)
  #!+sb-doc
  "Return a sequence formed by destructively removing the elements not
  satisfying the specified PREDICATE from the given SEQUENCE."
  (declare (fixnum start))
  (let ((end (or end length)))
    (declare (type index end))
    (seq-dispatch sequence
                  (if from-end
                      (if-not-list-delete-from-end)
                      (if-not-list-delete))
                  (if from-end
                      (if-not-mumble-delete-from-end)
                      (if-not-mumble-delete)))))

;;;; REMOVE

(eval-when (:compile-toplevel :execute)

;;; MUMBLE-REMOVE-MACRO does not include (removes) each element that
;;; satisfies the predicate.
(sb!xc:defmacro mumble-remove-macro (bump left begin finish right pred)
  `(do ((index ,begin (,bump index))
        (result
         (do ((index ,left (,bump index))
              (result (make-sequence-like sequence length)))
             ((= index (the fixnum ,begin)) result)
           (declare (fixnum index))
           (setf (aref result index) (aref sequence index))))
        (new-index ,begin)
        (number-zapped 0)
        (this-element))
       ((or (= index (the fixnum ,finish))
            (= number-zapped count))
        (do ((index index (,bump index))
             (new-index new-index (,bump new-index)))
            ((= index (the fixnum ,right)) (shrink-vector result new-index))
          (declare (fixnum index new-index))
          (setf (aref result new-index) (aref sequence index))))
     (declare (fixnum index new-index number-zapped))
     (setq this-element (aref sequence index))
     (cond (,pred (incf number-zapped))
           (t (setf (aref result new-index) this-element)
              (setq new-index (,bump new-index))))))

(sb!xc:defmacro mumble-remove (pred)
  `(mumble-remove-macro 1+ 0 start end length ,pred))

(sb!xc:defmacro mumble-remove-from-end (pred)
  `(let ((sequence (copy-seq sequence)))
     (mumble-delete-from-end ,pred)))

(sb!xc:defmacro normal-mumble-remove ()
  `(mumble-remove
    (if test-not
        (not (funcall test-not item (apply-key key this-element)))
        (funcall test item (apply-key key this-element)))))

(sb!xc:defmacro normal-mumble-remove-from-end ()
  `(mumble-remove-from-end
    (if test-not
        (not (funcall test-not item (apply-key key this-element)))
        (funcall test item (apply-key key this-element)))))

(sb!xc:defmacro if-mumble-remove ()
  `(mumble-remove (funcall predicate (apply-key key this-element))))

(sb!xc:defmacro if-mumble-remove-from-end ()
  `(mumble-remove-from-end (funcall predicate (apply-key key this-element))))

(sb!xc:defmacro if-not-mumble-remove ()
  `(mumble-remove (not (funcall predicate (apply-key key this-element)))))

(sb!xc:defmacro if-not-mumble-remove-from-end ()
  `(mumble-remove-from-end
    (not (funcall predicate (apply-key key this-element)))))

;;; LIST-REMOVE-MACRO does not include (removes) each element that satisfies
;;; the predicate.
(sb!xc:defmacro list-remove-macro (pred reverse?)
  `(let* ((sequence ,(if reverse?
                         '(reverse (the list sequence))
                         'sequence))
          (%start ,(if reverse? '(- length end) 'start))
          (%end ,(if reverse? '(- length start) 'end))
          (splice (list nil))
          (results (do ((index 0 (1+ index))
                        (before-start splice))
                       ((= index (the fixnum %start)) before-start)
                     (declare (fixnum index))
                     (setq splice
                           (cdr (rplacd splice (list (pop sequence))))))))
     (do ((index %start (1+ index))
          (this-element)
          (number-zapped 0))
         ((or (= index (the fixnum %end)) (= number-zapped count))
          (do ((index index (1+ index)))
              ((null sequence)
               ,(if reverse?
                    '(nreverse (the list (cdr results)))
                    '(cdr results)))
            (declare (fixnum index))
            (setq splice (cdr (rplacd splice (list (pop sequence)))))))
       (declare (fixnum index number-zapped))
       (setq this-element (pop sequence))
       (if ,pred
           (setq number-zapped (1+ number-zapped))
           (setq splice (cdr (rplacd splice (list this-element))))))))

(sb!xc:defmacro list-remove (pred)
  `(list-remove-macro ,pred nil))

(sb!xc:defmacro list-remove-from-end (pred)
  `(list-remove-macro ,pred t))

(sb!xc:defmacro normal-list-remove ()
  `(list-remove
    (if test-not
        (not (funcall test-not item (apply-key key this-element)))
        (funcall test item (apply-key key this-element)))))

(sb!xc:defmacro normal-list-remove-from-end ()
  `(list-remove-from-end
    (if test-not
        (not (funcall test-not item (apply-key key this-element)))
        (funcall test item (apply-key key this-element)))))

(sb!xc:defmacro if-list-remove ()
  `(list-remove
    (funcall predicate (apply-key key this-element))))

(sb!xc:defmacro if-list-remove-from-end ()
  `(list-remove-from-end
    (funcall predicate (apply-key key this-element))))

(sb!xc:defmacro if-not-list-remove ()
  `(list-remove
    (not (funcall predicate (apply-key key this-element)))))

(sb!xc:defmacro if-not-list-remove-from-end ()
  `(list-remove-from-end
    (not (funcall predicate (apply-key key this-element)))))

) ; EVAL-WHEN

(define-sequence-traverser remove
    (item sequence &key from-end test test-not start
          end count key)
  #!+sb-doc
  "Return a copy of SEQUENCE with elements satisfying the test (default is
   EQL) with ITEM removed."
  (declare (fixnum start))
  (let ((end (or end length)))
    (declare (type index end))
    (seq-dispatch sequence
                  (if from-end
                      (normal-list-remove-from-end)
                      (normal-list-remove))
                  (if from-end
                      (normal-mumble-remove-from-end)
                      (normal-mumble-remove)))))

(define-sequence-traverser remove-if
    (predicate sequence &key from-end start end count key)
  #!+sb-doc
  "Return a copy of sequence with elements satisfying PREDICATE removed."
  (declare (fixnum start))
  (let ((end (or end length)))
    (declare (type index end))
    (seq-dispatch sequence
                  (if from-end
                      (if-list-remove-from-end)
                      (if-list-remove))
                  (if from-end
                      (if-mumble-remove-from-end)
                      (if-mumble-remove)))))

(define-sequence-traverser remove-if-not
    (predicate sequence &key from-end start end count key)
  #!+sb-doc
  "Return a copy of sequence with elements not satisfying PREDICATE removed."
  (declare (fixnum start))
  (let ((end (or end length)))
    (declare (type index end))
    (seq-dispatch sequence
                  (if from-end
                      (if-not-list-remove-from-end)
                      (if-not-list-remove))
                  (if from-end
                      (if-not-mumble-remove-from-end)
                      (if-not-mumble-remove)))))

;;;; REMOVE-DUPLICATES

;;; Remove duplicates from a list. If from-end, remove the later duplicates,
;;; not the earlier ones. Thus if we check from-end we don't copy an item
;;; if we look into the already copied structure (from after :start) and see
;;; the item. If we check from beginning we check into the rest of the
;;; original list up to the :end marker (this we have to do by running a
;;; do loop down the list that far and using our test.
(defun list-remove-duplicates* (list test test-not start end key from-end)
  (declare (fixnum start))
  (let* ((result (list ())) ; Put a marker on the beginning to splice with.
         (splice result)
         (current list)
         (end (or end (length list)))
         (hash (and (> (- end start) 20)
                    test
                    (not key)
                    (not test-not)
                    (or (eql test #'eql)
                        (eql test #'eq)
                        (eql test #'equal)
                        (eql test #'equalp))
                    (make-hash-table :test test :size (- end start)))))
    (do ((index 0 (1+ index)))
        ((= index start))
      (declare (fixnum index))
      (setq splice (cdr (rplacd splice (list (car current)))))
      (setq current (cdr current)))
    (if hash
        (do ((index start (1+ index)))
            ((or (and end (= index (the fixnum end)))
                 (atom current)))
          (declare (fixnum index))
          ;; The hash table contains links from values that are
          ;; already in result to the cons cell *preceding* theirs
          ;; in the list.  That is, for each value v in the list,
          ;; v and (cadr (gethash v hash)) are equal under TEST.
          (let ((prev (gethash (car current) hash)))
            (cond
             ((not prev)
              (setf (gethash (car current) hash) splice)
              (setq splice (cdr (rplacd splice (list (car current))))))
             ((not from-end)
              (let* ((old (cdr prev))
                     (next (cdr old)))
                (if next
                  (let ((next-val (car next)))
                    ;; (assert (eq (gethash next-val hash) old))
                    (setf (cdr prev) next
                          (gethash next-val hash) prev
                          (gethash (car current) hash) splice
                          splice (cdr (rplacd splice (list (car current))))))
                  (setf (car old) (car current)))))))
          (setq current (cdr current)))
      (do ((index start (1+ index)))
          ((or (and end (= index (the fixnum end)))
               (atom current)))
        (declare (fixnum index))
        (if (or (and from-end
                     (not (if test-not
                              (member (apply-key key (car current))
                                      (nthcdr (1+ start) result)
                                      :test-not test-not
                                      :key key)
                            (member (apply-key key (car current))
                                    (nthcdr (1+ start) result)
                                    :test test
                                    :key key))))
                (and (not from-end)
                     (not (do ((it (apply-key key (car current)))
                               (l (cdr current) (cdr l))
                               (i (1+ index) (1+ i)))
                              ((or (atom l) (and end (= i (the fixnum end))))
                               ())
                            (declare (fixnum i))
                            (if (if test-not
                                    (not (funcall test-not
                                                  it
                                                  (apply-key key (car l))))
                                  (funcall test it (apply-key key (car l))))
                                (return t))))))
            (setq splice (cdr (rplacd splice (list (car current))))))
        (setq current (cdr current))))
    (do ()
        ((atom current))
      (setq splice (cdr (rplacd splice (list (car current)))))
      (setq current (cdr current)))
    (cdr result)))

(defun vector-remove-duplicates* (vector test test-not start end key from-end
                                         &optional (length (length vector)))
  (declare (vector vector) (fixnum start length))
  (when (null end) (setf end (length vector)))
  (let ((result (make-sequence-like vector length))
        (index 0)
        (jndex start))
    (declare (fixnum index jndex))
    (do ()
        ((= index start))
      (setf (aref result index) (aref vector index))
      (setq index (1+ index)))
    (do ((elt))
        ((= index end))
      (setq elt (aref vector index))
      ;; FIXME: Relying on POSITION allowing both :TEST and :TEST-NOT
      ;; arguments simultaneously is a little fragile, since ANSI says
      ;; we can't depend on it, so we need to remember to keep that
      ;; extension in our implementation. It'd probably be better to
      ;; rewrite this to avoid passing both (as
      ;; LIST-REMOVE-DUPLICATES* was rewritten ca. sbcl-0.7.12.18).
      (unless (or (and from-end
                       (position (apply-key key elt) result
                                 :start start :end jndex
                                 :test test :test-not test-not :key key))
                  (and (not from-end)
                       (position (apply-key key elt) vector
                                 :start (1+ index) :end end
                                 :test test :test-not test-not :key key)))
        (setf (aref result jndex) elt)
        (setq jndex (1+ jndex)))
      (setq index (1+ index)))
    (do ()
        ((= index length))
      (setf (aref result jndex) (aref vector index))
      (setq index (1+ index))
      (setq jndex (1+ jndex)))
    (shrink-vector result jndex)))

(define-sequence-traverser remove-duplicates
    (sequence &key test test-not start end from-end key)
  #!+sb-doc
  "The elements of SEQUENCE are compared pairwise, and if any two match,
   the one occurring earlier is discarded, unless FROM-END is true, in
   which case the one later in the sequence is discarded. The resulting
   sequence is returned.

   The :TEST-NOT argument is deprecated."
  (declare (fixnum start))
  (seq-dispatch sequence
                (if sequence
                    (list-remove-duplicates* sequence test test-not
                                              start end key from-end))
                (vector-remove-duplicates* sequence test test-not
                                            start end key from-end)))

;;;; DELETE-DUPLICATES

(defun list-delete-duplicates* (list test test-not key from-end start end)
  (declare (fixnum start))
  (let ((handle (cons nil list)))
    (do ((current (nthcdr start list) (cdr current))
         (previous (nthcdr start handle))
         (index start (1+ index)))
        ((or (and end (= index (the fixnum end))) (null current))
         (cdr handle))
      (declare (fixnum index))
      (if (do ((x (if from-end
                      (nthcdr (1+ start) handle)
                      (cdr current))
                  (cdr x))
               (i (1+ index) (1+ i)))
              ((or (null x)
                   (and (not from-end) end (= i (the fixnum end)))
                   (eq x current))
               nil)
            (declare (fixnum i))
            (if (if test-not
                    (not (funcall test-not
                                  (apply-key key (car current))
                                  (apply-key key (car x))))
                    (funcall test
                             (apply-key key (car current))
                             (apply-key key (car x))))
                (return t)))
          (rplacd previous (cdr current))
          (setq previous (cdr previous))))))

(defun vector-delete-duplicates* (vector test test-not key from-end start end
                                         &optional (length (length vector)))
  (declare (vector vector) (fixnum start length))
  (when (null end) (setf end (length vector)))
  (do ((index start (1+ index))
       (jndex start))
      ((= index end)
       (do ((index index (1+ index))            ; copy the rest of the vector
            (jndex jndex (1+ jndex)))
           ((= index length)
            (shrink-vector vector jndex)
            vector)
         (setf (aref vector jndex) (aref vector index))))
    (declare (fixnum index jndex))
    (setf (aref vector jndex) (aref vector index))
    (unless (position (apply-key key (aref vector index)) vector :key key
                      :start (if from-end start (1+ index)) :test test
                      :end (if from-end jndex end) :test-not test-not)
      (setq jndex (1+ jndex)))))

(define-sequence-traverser delete-duplicates
    (sequence &key test test-not start end from-end key)
  #!+sb-doc
  "The elements of SEQUENCE are examined, and if any two match, one is
   discarded. The resulting sequence, which may be formed by destroying the
   given sequence, is returned.

   The :TEST-NOT argument is deprecated."
  (seq-dispatch sequence
    (if sequence
        (list-delete-duplicates* sequence test test-not key from-end start end))
    (vector-delete-duplicates* sequence test test-not key from-end start end)))

;;;; SUBSTITUTE

(defun list-substitute* (pred new list start end count key test test-not old)
  (declare (fixnum start end count))
  (let* ((result (list nil))
         elt
         (splice result)
         (list list))      ; Get a local list for a stepper.
    (do ((index 0 (1+ index)))
        ((= index start))
      (declare (fixnum index))
      (setq splice (cdr (rplacd splice (list (car list)))))
      (setq list (cdr list)))
    (do ((index start (1+ index)))
        ((or (= index end) (null list) (= count 0)))
      (declare (fixnum index))
      (setq elt (car list))
      (setq splice
            (cdr (rplacd splice
                         (list
                          (cond
                           ((case pred
                                   (normal
                                    (if test-not
                                        (not
                                         (funcall test-not old (apply-key key elt)))
                                        (funcall test old (apply-key key elt))))
                                   (if (funcall test (apply-key key elt)))
                                   (if-not (not (funcall test (apply-key key elt)))))
                            (decf count)
                            new)
                                (t elt))))))
      (setq list (cdr list)))
    (do ()
        ((null list))
      (setq splice (cdr (rplacd splice (list (car list)))))
      (setq list (cdr list)))
    (cdr result)))

;;; Replace old with new in sequence moving from left to right by incrementer
;;; on each pass through the loop. Called by all three substitute functions.
(defun vector-substitute* (pred new sequence incrementer left right length
                           start end count key test test-not old)
  (declare (fixnum start count end incrementer right))
  (let ((result (make-sequence-like sequence length))
        (index left))
    (declare (fixnum index))
    (do ()
        ((= index start))
      (setf (aref result index) (aref sequence index))
      (setq index (+ index incrementer)))
    (do ((elt))
        ((or (= index end) (= count 0)))
      (setq elt (aref sequence index))
      (setf (aref result index)
            (cond ((case pred
                          (normal
                            (if test-not
                                (not (funcall test-not old (apply-key key elt)))
                                (funcall test old (apply-key key elt))))
                          (if (funcall test (apply-key key elt)))
                          (if-not (not (funcall test (apply-key key elt)))))
                   (setq count (1- count))
                   new)
                  (t elt)))
      (setq index (+ index incrementer)))
    (do ()
        ((= index right))
      (setf (aref result index) (aref sequence index))
      (setq index (+ index incrementer)))
    result))

(eval-when (:compile-toplevel :execute)

(sb!xc:defmacro subst-dispatch (pred)
  `(if (listp sequence)
       (if from-end
           (nreverse (list-substitute* ,pred
                                       new
                                       (reverse sequence)
                                       (- (the fixnum length)
                                          (the fixnum end))
                                       (- (the fixnum length)
                                          (the fixnum start))
                                       count key test test-not old))
           (list-substitute* ,pred
                             new sequence start end count key test test-not
                             old))
      (if from-end
          (vector-substitute* ,pred new sequence -1 (1- (the fixnum length))
                              -1 length (1- (the fixnum end))
                              (1- (the fixnum start))
                              count key test test-not old)
          (vector-substitute* ,pred new sequence 1 0 length length
           start end count key test test-not old))))

) ; EVAL-WHEN

(define-sequence-traverser substitute
    (new old sequence &key from-end test test-not
         start count end key)
  #!+sb-doc
  "Return a sequence of the same kind as SEQUENCE with the same elements,
  except that all elements equal to OLD are replaced with NEW."
  (declare (fixnum start))
  (let ((end (or end length)))
    (declare (type index end))
    (subst-dispatch 'normal)))

;;;; SUBSTITUTE-IF, SUBSTITUTE-IF-NOT

(define-sequence-traverser substitute-if
    (new predicate sequence &key from-end start end count key)
  #!+sb-doc
  "Return a sequence of the same kind as SEQUENCE with the same elements
  except that all elements satisfying the PRED are replaced with NEW."
  (declare (fixnum start))
  (let ((end (or end length))
        (test predicate)
        (test-not nil)
        old)
    (declare (type index length end))
    (subst-dispatch 'if)))

(define-sequence-traverser substitute-if-not
    (new predicate sequence &key from-end start end count key)
  #!+sb-doc
  "Return a sequence of the same kind as SEQUENCE with the same elements
  except that all elements not satisfying the PRED are replaced with NEW."
  (declare (fixnum start))
  (let ((end (or end length))
        (test predicate)
        (test-not nil)
        old)
    (declare (type index length end))
    (subst-dispatch 'if-not)))

;;;; NSUBSTITUTE

(define-sequence-traverser nsubstitute
    (new old sequence &key from-end test test-not
         end count key start)
  #!+sb-doc
  "Return a sequence of the same kind as SEQUENCE with the same elements
  except that all elements equal to OLD are replaced with NEW. SEQUENCE
  may be destructively modified."
  (declare (fixnum start))
  (let ((end (or end length)))
    (if (listp sequence)
        (if from-end
            (let ((length (length sequence)))
              (nreverse (nlist-substitute*
                         new old (nreverse (the list sequence))
                         test test-not (- length end) (- length start)
                         count key)))
            (nlist-substitute* new old sequence
                               test test-not start end count key))
        (if from-end
            (nvector-substitute* new old sequence -1
                                 test test-not (1- end) (1- start) count key)
            (nvector-substitute* new old sequence 1
                                 test test-not start end count key)))))

(defun nlist-substitute* (new old sequence test test-not start end count key)
  (declare (fixnum start count end))
  (do ((list (nthcdr start sequence) (cdr list))
       (index start (1+ index)))
      ((or (= index end) (null list) (= count 0)) sequence)
    (declare (fixnum index))
    (when (if test-not
              (not (funcall test-not old (apply-key key (car list))))
              (funcall test old (apply-key key (car list))))
      (rplaca list new)
      (setq count (1- count)))))

(defun nvector-substitute* (new old sequence incrementer
                            test test-not start end count key)
  (declare (fixnum start incrementer count end))
  (do ((index start (+ index incrementer)))
      ((or (= index end) (= count 0)) sequence)
    (declare (fixnum index))
    (when (if test-not
              (not (funcall test-not
                            old
                            (apply-key key (aref sequence index))))
              (funcall test old (apply-key key (aref sequence index))))
      (setf (aref sequence index) new)
      (setq count (1- count)))))

;;;; NSUBSTITUTE-IF, NSUBSTITUTE-IF-NOT

(define-sequence-traverser nsubstitute-if
    (new predicate sequence &key from-end start end count key)
  #!+sb-doc
  "Return a sequence of the same kind as SEQUENCE with the same elements
   except that all elements satisfying PREDICATE are replaced with NEW.
   SEQUENCE may be destructively modified."
  (declare (fixnum start))
  (let ((end (or end length)))
    (declare (fixnum end))
    (if (listp sequence)
        (if from-end
            (let ((length (length sequence)))
              (nreverse (nlist-substitute-if*
                         new predicate (nreverse (the list sequence))
                         (- length end) (- length start) count key)))
            (nlist-substitute-if* new predicate sequence
                                  start end count key))
        (if from-end
            (nvector-substitute-if* new predicate sequence -1
                                    (1- end) (1- start) count key)
            (nvector-substitute-if* new predicate sequence 1
                                    start end count key)))))

(defun nlist-substitute-if* (new test sequence start end count key)
  (declare (fixnum end))
  (do ((list (nthcdr start sequence) (cdr list))
       (index start (1+ index)))
      ((or (= index end) (null list) (= count 0)) sequence)
    (when (funcall test (apply-key key (car list)))
      (rplaca list new)
      (setq count (1- count)))))

(defun nvector-substitute-if* (new test sequence incrementer
                               start end count key)
  (do ((index start (+ index incrementer)))
      ((or (= index end) (= count 0)) sequence)
    (when (funcall test (apply-key key (aref sequence index)))
      (setf (aref sequence index) new)
      (setq count (1- count)))))

(define-sequence-traverser nsubstitute-if-not
    (new predicate sequence &key from-end start end count key)
  #!+sb-doc
  "Return a sequence of the same kind as SEQUENCE with the same elements
   except that all elements not satisfying PREDICATE are replaced with NEW.
   SEQUENCE may be destructively modified."
  (declare (fixnum start))
  (let ((end (or end length)))
    (declare (fixnum end))
    (if (listp sequence)
        (if from-end
            (let ((length (length sequence)))
              (nreverse (nlist-substitute-if-not*
                         new predicate (nreverse (the list sequence))
                         (- length end) (- length start) count key)))
            (nlist-substitute-if-not* new predicate sequence
                                      start end count key))
        (if from-end
            (nvector-substitute-if-not* new predicate sequence -1
                                        (1- end) (1- start) count key)
            (nvector-substitute-if-not* new predicate sequence 1
                                        start end count key)))))

(defun nlist-substitute-if-not* (new test sequence start end count key)
  (declare (fixnum end))
  (do ((list (nthcdr start sequence) (cdr list))
       (index start (1+ index)))
      ((or (= index end) (null list) (= count 0)) sequence)
    (when (not (funcall test (apply-key key (car list))))
      (rplaca list new)
      (decf count))))

(defun nvector-substitute-if-not* (new test sequence incrementer
                                   start end count key)
  (do ((index start (+ index incrementer)))
      ((or (= index end) (= count 0)) sequence)
    (when (not (funcall test (apply-key key (aref sequence index))))
      (setf (aref sequence index) new)
      (decf count))))

;;;; FIND, POSITION, and their -IF and -IF-NOT variants

(defun effective-find-position-test (test test-not)
  (effective-find-position-test test test-not))
(defun effective-find-position-key (key)
  (effective-find-position-key key))

;;; shared guts of out-of-line FIND, POSITION, FIND-IF, and POSITION-IF
(macrolet (;; shared logic for defining %FIND-POSITION and
           ;; %FIND-POSITION-IF in terms of various inlineable cases
           ;; of the expression defined in FROB and VECTOR*-FROB
           (frobs ()
             `(etypecase sequence-arg
                (list (frob sequence-arg from-end))
                (vector
                 (with-array-data ((sequence sequence-arg :offset-var offset)
                                   (start start)
                                   (end (%check-vector-sequence-bounds
                                         sequence-arg start end)))
                   (multiple-value-bind (f p)
                       (macrolet ((frob2 () '(if from-end
                                                 (frob sequence t)
                                                 (frob sequence nil))))
                         (typecase sequence
                           (simple-vector (frob2))
                           (simple-base-string (frob2))
                           (t (vector*-frob sequence))))
                     (declare (type (or index null) p))
                     (values f (and p (the index (- p offset))))))))))
  (defun %find-position (item sequence-arg from-end start end key test)
    (macrolet ((frob (sequence from-end)
                 `(%find-position item ,sequence
                                  ,from-end start end key test))
               (vector*-frob (sequence)
                 `(%find-position-vector-macro item ,sequence
                                               from-end start end key test)))
      (frobs)))
  (defun %find-position-if (predicate sequence-arg from-end start end key)
    (macrolet ((frob (sequence from-end)
                 `(%find-position-if predicate ,sequence
                                     ,from-end start end key))
               (vector*-frob (sequence)
                 `(%find-position-if-vector-macro predicate ,sequence
                                                  from-end start end key)))
      (frobs)))
  (defun %find-position-if-not (predicate sequence-arg from-end start end key)
    (macrolet ((frob (sequence from-end)
                 `(%find-position-if-not predicate ,sequence
                                         ,from-end start end key))
               (vector*-frob (sequence)
                 `(%find-position-if-not-vector-macro predicate ,sequence
                                                  from-end start end key)))
      (frobs))))

;;; the user interface to FIND and POSITION: just interpreter stubs,
;;; nowadays.
(defun find (item sequence &key from-end (start 0) end key test test-not)
  ;; FIXME: this can't be the way to go, surely?
  (find item sequence :from-end from-end :start start :end end :key key
        :test test :test-not test-not))
(defun position (item sequence &key from-end (start 0) end key test test-not)
  (position item sequence :from-end from-end :start start :end end :key key
            :test test :test-not test-not))

;;; the user interface to FIND-IF and POSITION-IF, entirely analogous
;;; to the interface to FIND and POSITION
(defun find-if (predicate sequence &key from-end (start 0) end key)
  (find-if predicate sequence :from-end from-end :start start
           :end end :key key))
(defun position-if (predicate sequence &key from-end (start 0) end key)
  (position-if predicate sequence :from-end from-end :start start
               :end end :key key))

(defun find-if-not (predicate sequence &key from-end (start 0) end key)
  (find-if-not predicate sequence :from-end from-end :start start
           :end end :key key))
(defun position-if-not (predicate sequence &key from-end (start 0) end key)
  (position-if-not predicate sequence :from-end from-end :start start
               :end end :key key))

;;;; COUNT-IF, COUNT-IF-NOT, and COUNT

(eval-when (:compile-toplevel :execute)

(sb!xc:defmacro vector-count-if (notp from-end-p predicate sequence)
  (let ((next-index (if from-end-p '(1- index) '(1+ index)))
        (pred `(funcall ,predicate (apply-key key (aref ,sequence index)))))
    `(let ((%start ,(if from-end-p '(1- end) 'start))
           (%end ,(if from-end-p '(1- start) 'end)))
      (do ((index %start ,next-index)
           (count 0))
          ((= index (the fixnum %end)) count)
        (declare (fixnum index count))
        (,(if notp 'unless 'when) ,pred
          (setq count (1+ count)))))))

(sb!xc:defmacro list-count-if (notp from-end-p predicate sequence)
  (let ((pred `(funcall ,predicate (apply-key key (pop sequence)))))
    `(let ((%start ,(if from-end-p '(- length end) 'start))
           (%end ,(if from-end-p '(- length start) 'end))
           (sequence ,(if from-end-p '(reverse sequence) 'sequence)))
      (do ((sequence (nthcdr %start ,sequence))
           (index %start (1+ index))
           (count 0))
          ((or (= index (the fixnum %end)) (null sequence)) count)
        (declare (fixnum index count))
        (,(if notp 'unless 'when) ,pred
          (setq count (1+ count)))))))


) ; EVAL-WHEN

(define-sequence-traverser count-if (pred sequence &key from-end start end key)
  #!+sb-doc
  "Return the number of elements in SEQUENCE satisfying PRED(el)."
  (declare (fixnum start))
  (let ((end (or end length))
        (pred (%coerce-callable-to-fun pred)))
    (declare (type index end))
    (seq-dispatch sequence
                  (if from-end
                      (list-count-if nil t pred sequence)
                      (list-count-if nil nil pred sequence))
                  (if from-end
                      (vector-count-if nil t pred sequence)
                      (vector-count-if nil nil pred sequence)))))

(define-sequence-traverser count-if-not
    (pred sequence &key from-end start end key)
  #!+sb-doc
  "Return the number of elements in SEQUENCE not satisfying TEST(el)."
  (declare (fixnum start))
  (let ((end (or end length))
        (pred (%coerce-callable-to-fun pred)))
    (declare (type index end))
    (seq-dispatch sequence
                  (if from-end
                      (list-count-if t t pred sequence)
                      (list-count-if t nil pred sequence))
                  (if from-end
                      (vector-count-if t t pred sequence)
                      (vector-count-if t nil pred sequence)))))

(define-sequence-traverser count
    (item sequence &key from-end start end
          key (test #'eql test-p) (test-not nil test-not-p))
  #!+sb-doc
  "Return the number of elements in SEQUENCE satisfying a test with ITEM,
   which defaults to EQL."
  (declare (fixnum start))
  (when (and test-p test-not-p)
    ;; ANSI Common Lisp has left the behavior in this situation unspecified.
    ;; (CLHS 17.2.1)
    (error ":TEST and :TEST-NOT are both present."))
  (let ((end (or end length)))
    (declare (type index end))
    (let ((%test (if test-not-p
                     (lambda (x)
                       (not (funcall test-not item x)))
                     (lambda (x)
                       (funcall test item x)))))
      (seq-dispatch sequence
                    (if from-end
                        (list-count-if nil t %test sequence)
                        (list-count-if nil nil %test sequence))
                    (if from-end
                        (vector-count-if nil t %test sequence)
                        (vector-count-if nil nil %test sequence))))))



;;;; MISMATCH

(eval-when (:compile-toplevel :execute)

(sb!xc:defmacro match-vars (&rest body)
  `(let ((inc (if from-end -1 1))
         (start1 (if from-end (1- (the fixnum end1)) start1))
         (start2 (if from-end (1- (the fixnum end2)) start2))
         (end1 (if from-end (1- (the fixnum start1)) end1))
         (end2 (if from-end (1- (the fixnum start2)) end2)))
     (declare (fixnum inc start1 start2 end1 end2))
     ,@body))

(sb!xc:defmacro matchify-list ((sequence start length end) &body body)
  (declare (ignore end)) ;; ### Should END be used below?
  `(let ((,sequence (if from-end
                        (nthcdr (- (the fixnum ,length) (the fixnum ,start) 1)
                                (reverse (the list ,sequence)))
                        (nthcdr ,start ,sequence))))
     (declare (type list ,sequence))
     ,@body))

) ; EVAL-WHEN

(eval-when (:compile-toplevel :execute)

(sb!xc:defmacro if-mismatch (elt1 elt2)
  `(cond ((= (the fixnum index1) (the fixnum end1))
          (return (if (= (the fixnum index2) (the fixnum end2))
                      nil
                      (if from-end
                          (1+ (the fixnum index1))
                          (the fixnum index1)))))
         ((= (the fixnum index2) (the fixnum end2))
          (return (if from-end (1+ (the fixnum index1)) index1)))
         (test-not
          (if (funcall test-not (apply-key key ,elt1) (apply-key key ,elt2))
              (return (if from-end (1+ (the fixnum index1)) index1))))
         (t (if (not (funcall test (apply-key key ,elt1)
                              (apply-key key ,elt2)))
                (return (if from-end (1+ (the fixnum index1)) index1))))))

(sb!xc:defmacro mumble-mumble-mismatch ()
  `(do ((index1 start1 (+ index1 (the fixnum inc)))
        (index2 start2 (+ index2 (the fixnum inc))))
       (())
     (declare (fixnum index1 index2))
     (if-mismatch (aref sequence1 index1) (aref sequence2 index2))))

(sb!xc:defmacro mumble-list-mismatch ()
  `(do ((index1 start1 (+ index1 (the fixnum inc)))
        (index2 start2 (+ index2 (the fixnum inc))))
       (())
     (declare (fixnum index1 index2))
     (if-mismatch (aref sequence1 index1) (pop sequence2))))

(sb!xc:defmacro list-mumble-mismatch ()
  `(do ((index1 start1 (+ index1 (the fixnum inc)))
        (index2 start2 (+ index2 (the fixnum inc))))
       (())
     (declare (fixnum index1 index2))
     (if-mismatch (pop sequence1) (aref sequence2 index2))))

(sb!xc:defmacro list-list-mismatch ()
  `(do ((sequence1 sequence1)
        (sequence2 sequence2)
        (index1 start1 (+ index1 (the fixnum inc)))
        (index2 start2 (+ index2 (the fixnum inc))))
       (())
     (declare (fixnum index1 index2))
     (if-mismatch (pop sequence1) (pop sequence2))))

) ; EVAL-WHEN

(define-sequence-traverser mismatch
    (sequence1 sequence2
               &key from-end test test-not
               start1 end1 start2 end2 key)
  #!+sb-doc
  "The specified subsequences of SEQUENCE1 and SEQUENCE2 are compared
   element-wise. If they are of equal length and match in every element, the
   result is NIL. Otherwise, the result is a non-negative integer, the index
   within SEQUENCE1 of the leftmost position at which they fail to match; or,
   if one is shorter than and a matching prefix of the other, the index within
   SEQUENCE1 beyond the last position tested is returned. If a non-NIL
   :FROM-END argument is given, then one plus the index of the rightmost
   position in which the sequences differ is returned."
  (declare (fixnum start1 start2))
  (let* ((end1 (or end1 length1))
         (end2 (or end2 length2)))
    (declare (type index end1 end2))
    (match-vars
     (seq-dispatch sequence1
       (matchify-list (sequence1 start1 length1 end1)
         (seq-dispatch sequence2
           (matchify-list (sequence2 start2 length2 end2)
             (list-list-mismatch))
           (list-mumble-mismatch)))
       (seq-dispatch sequence2
         (matchify-list (sequence2 start2 length2 end2)
           (mumble-list-mismatch))
         (mumble-mumble-mismatch))))))

;;; search comparison functions

(eval-when (:compile-toplevel :execute)

;;; Compare two elements and return if they don't match.
(sb!xc:defmacro compare-elements (elt1 elt2)
  `(if test-not
       (if (funcall test-not (apply-key key ,elt1) (apply-key key ,elt2))
           (return nil)
           t)
       (if (not (funcall test (apply-key key ,elt1) (apply-key key ,elt2)))
           (return nil)
           t)))

(sb!xc:defmacro search-compare-list-list (main sub)
  `(do ((main ,main (cdr main))
        (jndex start1 (1+ jndex))
        (sub (nthcdr start1 ,sub) (cdr sub)))
       ((or (endp main) (endp sub) (<= end1 jndex))
        t)
     (declare (type (integer 0) jndex))
     (compare-elements (car sub) (car main))))

(sb!xc:defmacro search-compare-list-vector (main sub)
  `(do ((main ,main (cdr main))
        (index start1 (1+ index)))
       ((or (endp main) (= index end1)) t)
     (compare-elements (aref ,sub index) (car main))))

(sb!xc:defmacro search-compare-vector-list (main sub index)
  `(do ((sub (nthcdr start1 ,sub) (cdr sub))
        (jndex start1 (1+ jndex))
        (index ,index (1+ index)))
       ((or (<= end1 jndex) (endp sub)) t)
     (declare (type (integer 0) jndex))
     (compare-elements (car sub) (aref ,main index))))

(sb!xc:defmacro search-compare-vector-vector (main sub index)
  `(do ((index ,index (1+ index))
        (sub-index start1 (1+ sub-index)))
       ((= sub-index end1) t)
     (compare-elements (aref ,sub sub-index) (aref ,main index))))

(sb!xc:defmacro search-compare (main-type main sub index)
  (if (eq main-type 'list)
      `(seq-dispatch ,sub
                     (search-compare-list-list ,main ,sub)
                     (search-compare-list-vector ,main ,sub))
      `(seq-dispatch ,sub
                     (search-compare-vector-list ,main ,sub ,index)
                     (search-compare-vector-vector ,main ,sub ,index))))

) ; EVAL-WHEN

;;;; SEARCH

(eval-when (:compile-toplevel :execute)

(sb!xc:defmacro list-search (main sub)
  `(do ((main (nthcdr start2 ,main) (cdr main))
        (index2 start2 (1+ index2))
        (terminus (- end2 (the (integer 0) (- end1 start1))))
        (last-match ()))
       ((> index2 terminus) last-match)
     (declare (type (integer 0) index2))
     (if (search-compare list main ,sub index2)
         (if from-end
             (setq last-match index2)
             (return index2)))))

(sb!xc:defmacro vector-search (main sub)
  `(do ((index2 start2 (1+ index2))
        (terminus (- end2 (the (integer 0) (- end1 start1))))
        (last-match ()))
       ((> index2 terminus) last-match)
     (declare (type (integer 0) index2))
     (if (search-compare vector ,main ,sub index2)
         (if from-end
             (setq last-match index2)
             (return index2)))))

) ; EVAL-WHEN

(define-sequence-traverser search
    (sequence1 sequence2
               &key from-end test test-not
               start1 end1 start2 end2 key)
  (declare (fixnum start1 start2))
  (let ((end1 (or end1 length1))
        (end2 (or end2 length2)))
    (seq-dispatch sequence2
                  (list-search sequence2 sequence1)
                  (vector-search sequence2 sequence1))))
