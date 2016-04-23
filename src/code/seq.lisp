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

(defun %check-generic-sequence-bounds (seq start end)
  (let ((length (sb!sequence:length seq)))
    (if (<= 0 start (or end length) length)
        (or end length)
        (sequence-bounding-indices-bad-error seq start end))))

(eval-when (:compile-toplevel :load-toplevel :execute)

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
    ;; Entries for {start,end}{,1,2}
    ,@(mapcan (lambda (names)
                (destructuring-bind (start end length sequence) names
                  (list
                   `(,start
                     0
                     nil
                     ;; Only evaluate LENGTH (which may be expensive)
                     ;; if START is non-NIL.
                     (if (or (zerop ,start) (<= 0 ,start ,length))
                         ,start
                         (sequence-bounding-indices-bad-error ,sequence ,start ,end))
                     index)
                   `(,end
                     nil
                     nil
                     ;; Only evaluate LENGTH (which may be expensive)
                     ;; if END is non-NIL.
                     (if (or (null ,end) (<= ,start ,end ,length))
                         ;; Defaulting of NIL is done inside the
                         ;; bodies, for ease of sharing with compiler
                         ;; transforms.
                         ;;
                         ;; FIXME: defend against non-number non-NIL
                         ;; stuff?
                         ,end
                         (sequence-bounding-indices-bad-error ,sequence ,start ,end))
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
              (or null function))))

(sb!xc:defmacro define-sequence-traverser (name args &body body)
  (multiple-value-bind (body declarations docstring) (parse-body body t)
    (collect ((new-args)
              (new-declarations)
              ;; Things which are definitely used in any code path.
              (rebindings/eager)
              ;; Things which may be used/are only used in certain
              ;; code paths (e.g. length).
              (rebindings/lazy))
      (dolist (arg args)
        (case arg
          ;; FIXME: make this robust.  And clean.
          ((sequence sequence1 sequence2)
           (let* ((length-var (ecase arg
                                (sequence  'length)
                                (sequence1 'length1)
                                (sequence2 'length2)))
                  (cache-var (symbolicate length-var '#:-cache)))
             (new-args arg)
             (rebindings/eager `(,cache-var nil))
             (rebindings/lazy
              `(,length-var (truly-the
                             index
                             (or ,cache-var (setf ,cache-var (length ,arg))))))))
          ((function predicate)
           (new-args arg)
           (rebindings/eager `(,arg (%coerce-callable-to-fun ,arg))))
          (t
           (let ((info (cdr (assoc arg *sequence-keyword-info*))))
             (cond (info
                    (destructuring-bind (default supplied-p adjuster type) info
                      (new-args `(,arg ,default ,@(when supplied-p (list supplied-p))))
                      (rebindings/eager `(,arg ,adjuster))
                      (new-declarations `(type ,type ,arg))))
                   (t (new-args arg)))))))
      `(defun ,name ,(new-args)
         ,@(when docstring (list docstring))
         ,@declarations
         (symbol-macrolet (,@(rebindings/lazy))
           (let* (,@(rebindings/eager))
             (declare ,@(new-declarations))
             ,@body
             ))))))

;;; SEQ-DISPATCH does an efficient type-dispatch on the given SEQUENCE.
;;;
;;; FIXME: It might be worth making three cases here, LIST,
;;; SIMPLE-VECTOR, and VECTOR, instead of the current LIST and VECTOR.
;;; It tends to make code run faster but be bigger; some benchmarking
;;; is needed to decide.
(sb!xc:defmacro seq-dispatch
    (sequence list-form array-form &optional other-form)
  `(if (listp ,sequence)
       (let ((,sequence (truly-the list ,sequence)))
         (declare (ignorable ,sequence))
         ,list-form)
       ,@(if other-form
             `((if (arrayp ,sequence)
                   (let ((,sequence (truly-the vector ,sequence)))
                     (declare (ignorable ,sequence))
                     ,array-form)
                   ,other-form))
             `((let ((,sequence (truly-the vector ,sequence)))
                 (declare (ignorable ,sequence))
                 ,array-form)))))

;; Same as above, but don't assume that ARRAYP implies VECTORP,
;; and call SEQUENCEP only after checking for not LISTP and not VECTORP.
;; This is for dispatching within sequence functions that have the
;; EXPLICIT-CHECK attribute. [Because there is no way to have the compiler
;; insert checks for a subset of arguments, it is inconvenient to declare
;; things like DELETE-IF with explicit-check to suppress checking of the
;; 'sequence' because then you have to manually check all other arguments.]
(sb!xc:defmacro seq-dispatch-checking
    (sequence list-form array-form &optional other-form)
  `(if (listp ,sequence)
       (let ((,sequence (truly-the list ,sequence)))
         (declare (ignorable ,sequence))
         ,list-form)
       ,@(if other-form
             `((if (vectorp ,sequence)
                   (let ((,sequence (truly-the vector ,sequence)))
                     (declare (ignorable ,sequence))
                     ,array-form)
                   ;; This could assert (THE EXTENDED-SEQUENCE ,sequence)
                   ;; for a slight win, however the error string would be wrong.
                   ;; It needs to be "<x> is not a SEQUENCE", not "<x> is not
                   ;; an EXTENDED-SEQUENCE".
                   (let ((,sequence (the sequence ,sequence)))
                     ,other-form)))
             `((let ((,sequence (the vector ,sequence)))
                 (declare (ignorable ,sequence))
                 ,array-form)))))

(sb!xc:defmacro %make-sequence-like (sequence length)
  #!+sb-doc
  "Return a sequence of the same type as SEQUENCE and the given LENGTH."
  `(seq-dispatch ,sequence
     (make-list ,length)
     (make-vector-like ,sequence ,length)
     (sb!sequence:make-sequence-like ,sequence ,length)))

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

(sb!xc:defmacro when-extended-sequence-type
    ((type-specifier type
      &key
      expandedp
      (expanded (gensym "EXPANDED"))
      (class (gensym "CLASS"))
      (prototype (gensym "PROTOTYPE") prototypep))
     &body body)
  (once-only ((type-specifier type-specifier) (type type))
    `(when (csubtypep ,type (specifier-type 'sequence))
       (binding* ((,expanded ,(if expandedp
                                  type-specifier
                                  `(typexpand ,type-specifier)))
                  (,class (if (typep ,expanded 'class)
                              ,expanded
                              (find-class ,expanded nil))
                          :exit-if-null)
                  (,prototype (sb!mop:class-prototype
                               (sb!pcl:ensure-class-finalized ,class))))
         ,@(unless prototypep `((ignore ,prototype)))
         ,@body))))

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

(declaim (ftype (function (t t t) nil) sequence-bounding-indices-bad-error))
(defun sequence-bounding-indices-bad-error (sequence start end)
  (let ((size (length sequence)))
    (error 'bounding-indices-bad-error
           :datum (cons start end)
           :expected-type `(cons (integer 0 ,size)
                                 (integer ,start ,size))
           :object sequence)))

(declaim (ftype (function (t t t) nil) array-bounding-indices-bad-error))
(defun array-bounding-indices-bad-error (array start end)
  (let ((size (array-total-size array)))
    (error 'bounding-indices-bad-error
           :datum (cons start end)
           :expected-type `(cons (integer 0 ,size)
                                 (integer ,start ,size))
           :object array)))

(declaim (ftype (function (t) nil) circular-list-error))
(defun circular-list-error (list)
  (let ((*print-circle* t))
    (error 'simple-type-error
           :format-control "List is circular:~%  ~S"
           :format-arguments (list list)
           :datum list
           :type '(and list (satisfies list-length)))))



(defun emptyp (sequence)
  #!+sb-doc
  "Returns T if SEQUENCE is an empty sequence and NIL
   otherwise. Signals an error if SEQUENCE is not a sequence."
  (seq-dispatch sequence
                (null sequence)
                (zerop (length sequence))
                (sb!sequence:emptyp sequence)))

(defun elt (sequence index)
  #!+sb-doc "Return the element of SEQUENCE specified by INDEX."
  (seq-dispatch sequence
                (do ((count index (1- count))
                     (list sequence (cdr list)))
                    ((= count 0)
                     (if (endp list)
                         (signal-index-too-large-error sequence index)
                         (car list)))
                  (declare (type (integer 0) count)))
                (progn
                  (when (>= index (length sequence))
                    (signal-index-too-large-error sequence index))
                  (aref sequence index))
                (sb!sequence:elt sequence index)))

(defun %setelt (sequence index newval)
  #!+sb-doc "Store NEWVAL as the component of SEQUENCE specified by INDEX."
  (seq-dispatch sequence
                (do ((count index (1- count))
                     (seq sequence))
                    ((= count 0) (rplaca seq newval) newval)
                  (declare (fixnum count))
                  (if (atom (cdr seq))
                      (signal-index-too-large-error sequence index)
                      (setq seq (cdr seq))))
                (progn
                  (when (>= index (length sequence))
                    (signal-index-too-large-error sequence index))
                  (setf (aref sequence index) newval))
                (setf (sb!sequence:elt sequence index) newval)))

(defun length (sequence)
  #!+sb-doc "Return an integer that is the length of SEQUENCE."
  (declare (explicit-check))
  (seq-dispatch-checking sequence
                (length sequence)
                (length sequence)
                (sb!sequence:length sequence)))

(defun make-sequence (result-type length &key (initial-element nil iep))
  #!+sb-doc
  "Return a sequence of the given RESULT-TYPE and LENGTH, with
  elements initialized to INITIAL-ELEMENT."
  (declare (index length) (explicit-check))
  (let* ((expanded-type (typexpand result-type))
         (adjusted-type
          (typecase expanded-type
            (atom (cond
                    ((eq expanded-type 'string) '(vector character))
                    ((eq expanded-type 'simple-string)
                     '(simple-array character (*)))
                    (t expanded-type)))
            (cons (cond
                    ((eq (car expanded-type) 'string)
                     `(vector character ,@(cdr expanded-type)))
                    ((eq (car expanded-type) 'simple-string)
                     `(simple-array character ,(if (cdr expanded-type)
                                                   (cdr expanded-type)
                                                   '(*))))
                    (t expanded-type)))))
         (type (specifier-type adjusted-type))
         (list-type (specifier-type 'list)))
    (cond ((csubtypep type list-type)
           (cond
             ((type= type list-type)
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
              (aver (= (length (array-type-dimensions type)) 1))
              (let* ((etype (type-specifier
                             (array-type-specialized-element-type type)))
                     (etype (if (eq etype '*) t etype))
                     (type-length (car (array-type-dimensions type))))
                (unless (or (eq type-length '*)
                            (= type-length length))
                  (sequence-type-length-mismatch-error type length))
                (if iep
                    (make-array length :element-type etype
                                :initial-element initial-element)
                    (make-array length :element-type etype))))
             (t (sequence-type-too-hairy (type-specifier type)))))
          ((when-extended-sequence-type
               (expanded-type type :expandedp t :prototype prototype)
             ;; This function has the EXPLICIT-CHECK declaration, so
             ;; we manually assert that it returns a SEQUENCE.
             (the extended-sequence
                  (if iep
                      (sb!sequence:make-sequence-like
                       prototype length :initial-element initial-element)
                      (sb!sequence:make-sequence-like
                       prototype length)))))
          (t (bad-sequence-type-error (type-specifier type))))))

;;;; SUBSEQ
;;;;

(!define-array-dispatch vector-subseq-dispatch (array start end)
  (declare (optimize speed (safety 0)))
  (declare (type index start end))
  (subseq array start end))

;;;; The support routines for SUBSEQ are used by compiler transforms,
;;;; so we worry about dealing with END being supplied or defaulting
;;;; to NIL at this level.

(defun vector-subseq* (sequence start end)
  (declare (type vector sequence))
  (declare (type index start)
           (type (or null index) end)
           (optimize speed))
  (with-array-data ((data sequence)
                    (start start)
                    (end end)
                    :check-fill-pointer t
                    :force-inline t)
    (vector-subseq-dispatch data start end)))

(defun list-subseq* (sequence start end)
  (declare (type list sequence)
           (type unsigned-byte start)
           (type (or null unsigned-byte) end))
  (flet ((oops ()
           (sequence-bounding-indices-bad-error sequence start end)))
    (let ((pointer sequence))
      (unless (zerop start)
        ;; If START > 0 the list cannot be empty. So CDR down to
        ;; it START-1 times, check that we still have something, then
        ;; CDR the final time.
        ;;
        ;; If START was zero, the list may be empty if END is NIL or
        ;; also zero.
        (when (> start 1)
          (setf pointer (nthcdr (1- start) pointer)))
        (if pointer
            (pop pointer)
            (oops)))
      (if end
          (let ((n (- end start)))
            (declare (integer n))
            (when (minusp n)
              (oops))
            (when (plusp n)
              (let* ((head (list nil))
                     (tail head))
                (macrolet ((pop-one ()
                             `(let ((tmp (list (pop pointer))))
                                (setf (cdr tail) tmp
                                      tail tmp))))
                  ;; Bignum case
                  (loop until (fixnump n)
                        do (pop-one)
                           (decf n))
                  ;; Fixnum case, but leave last element, so we should
                  ;; still have something left in the sequence.
                  (let ((m (1- n)))
                    (declare (fixnum m))
                    (loop repeat m
                          do (pop-one)))
                  (unless pointer
                    (oops))
                  ;; OK, pop the last one.
                  (pop-one)
                  (cdr head)))))
            (loop while pointer
                  collect (pop pointer))))))

(defun subseq (sequence start &optional end)
  #!+sb-doc
  "Return a copy of a subsequence of SEQUENCE starting with element number
   START and continuing to the end of SEQUENCE or the optional END."
  (seq-dispatch sequence
    (list-subseq* sequence start end)
    (vector-subseq* sequence start end)
    (sb!sequence:subseq sequence start end)))

;;;; COPY-SEQ

(defun copy-seq (sequence)
  #!+sb-doc "Return a copy of SEQUENCE which is EQUAL to SEQUENCE but not EQ."
  (seq-dispatch sequence
    (list-copy-seq* sequence)
    (vector-subseq* sequence 0 nil)
    (sb!sequence:copy-seq sequence)))

(defun list-copy-seq* (sequence)
  (!copy-list-macro sequence :check-proper-list t))

;;;; FILL

(defun list-fill* (sequence item start end)
  (declare (type list sequence)
           (type unsigned-byte start)
           (type (or null unsigned-byte) end))
  (flet ((oops ()
           (sequence-bounding-indices-bad-error sequence start end)))
    (let ((pointer sequence))
      (unless (zerop start)
        ;; If START > 0 the list cannot be empty. So CDR down to it
        ;; START-1 times, check that we still have something, then CDR
        ;; the final time.
        ;;
        ;; If START was zero, the list may be empty if END is NIL or
        ;; also zero.
        (unless (= start 1)
          (setf pointer (nthcdr (1- start) pointer)))
        (if pointer
            (pop pointer)
            (oops)))
      (if end
          (let ((n (- end start)))
            (declare (integer n))
            (when (minusp n)
              (oops))
            (when (plusp n)
              (loop repeat n
                    do (setf pointer (cdr (rplaca pointer item))))))
          (loop while pointer
                do (setf pointer (cdr (rplaca pointer item)))))))
  sequence)

(defun vector-fill* (sequence item start end)
  (with-array-data ((data sequence)
                    (start start)
                    (end end)
                    :force-inline t
                    :check-fill-pointer t)
    (let ((setter (!find-data-vector-setter data)))
      (declare (optimize (speed 3) (safety 0)))
      (do ((index start (1+ index)))
          ((= index end) sequence)
        (declare (index index))
        (funcall setter data index item)))))

(defun string-fill* (sequence item start end)
  (declare (string sequence))
  (with-array-data ((data sequence)
                    (start start)
                    (end end)
                    :force-inline t
                    :check-fill-pointer t)
    ;; DEFTRANSFORM for FILL will turn these into
    ;; calls to UB*-BASH-FILL.
    (etypecase data
      #!+sb-unicode
      ((simple-array character (*))
       (let ((item (locally (declare (optimize (safety 3)))
                     (the character item))))
         (fill data item :start start :end end)))
      ((simple-array base-char (*))
       (let ((item (locally (declare (optimize (safety 3)))
                     (the base-char item))))
         (fill data item :start start :end end))))))

(defun fill (sequence item &key (start 0) end)
  #!+sb-doc
  "Replace the specified elements of SEQUENCE with ITEM."
  (seq-dispatch sequence
   (list-fill* sequence item start end)
   (vector-fill* sequence item start end)
   (sb!sequence:fill sequence item
                     :start start
                     :end (%check-generic-sequence-bounds sequence start end))))

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
    (sequence1 sequence2 &rest args &key start1 end1 start2 end2)
  #!+sb-doc
  "Destructively modifies SEQUENCE1 by copying successive elements
into it from the SEQUENCE2.

Elements are copied to the subseqeuence bounded by START1 and END1,
from the subsequence bounded by START2 and END2. If these subsequences
are not of the same length, then the shorter length determines how
many elements are copied."
  (declare (truly-dynamic-extent args))
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
        (list-replace-from-mumble)
        (apply #'sb!sequence:replace sequence1 sequence2 args))
      (seq-dispatch source-sequence
        (mumble-replace-from-list)
        (mumble-replace-from-mumble)
        (apply #'sb!sequence:replace sequence1 sequence2 args))
      (apply #'sb!sequence:replace sequence1 sequence2 args))))

;;;; REVERSE
(defun reverse (sequence)
  #!+sb-doc
  "Return a new sequence containing the same elements but in reverse order."
  (declare (explicit-check))
  (seq-dispatch-checking sequence
    (list-reverse sequence)
    (vector-reverse sequence)
    ;; The type deriver says that LIST => LIST and VECTOR => VECTOR
    ;; but does not claim to know anything about extended-sequences.
    ;; So this could theoretically return any subtype of SEQUENCE
    ;; given an EXTENDED-SEQUENCE as input. But fndb says this returns
    ;; a CONSED-SEQUENCE, which precludes non-simple vectors.
    ;; But a CLOS sequence can apparently decide to return a LIST when
    ;; reversed. [Is that too weird? Make this EXTENDED-SEQUENCE maybe?]
    (the consed-sequence (values (sb!sequence:reverse sequence)))))

(defun list-reverse (list)
  (do ((new-list ()))
      ((endp list) new-list)
    (push (pop list) new-list)))

(defmacro word-specialized-vector-tag-p (tag)
  `(or
    ,@(loop for saetp across sb!vm:*specialized-array-element-type-properties*
           when (and (eq (sb!vm:saetp-n-bits saetp) sb!vm:n-word-bits)
                     (not (eq (sb!vm:saetp-specifier saetp) t)))
           collect `(eq ,tag ,(sb!vm:saetp-typecode saetp)))))

(defun reverse-word-specialized-vector (from to end)
  (declare (vector from))
  (do ((length (length to))
       (left-index 0 (1+ left-index))
       (right-index end))
      ((= left-index length))
    (declare (type index left-index right-index))
    (decf right-index)
    (setf (%vector-raw-bits to left-index)
          (%vector-raw-bits from right-index)))
  to)

(defun vector-reverse (vector)
  (declare (vector vector))
  (let ((length (length vector)))
    (with-array-data ((vector vector) (start) (end)
                      :check-fill-pointer t)
      (declare (ignore start))
      (let* ((tag (%other-pointer-widetag vector))
             (new-vector (allocate-vector-with-widetag tag length)))
        (cond ((= tag sb!vm:simple-vector-widetag)
               (do ((left-index 0 (1+ left-index))
                    (right-index end))
                   ((= left-index length))
                 (declare (type index left-index right-index))
                 (decf right-index)
                 (setf (svref new-vector left-index)
                       (svref vector right-index))))
              ((word-specialized-vector-tag-p tag)
               (reverse-word-specialized-vector vector new-vector end))
              (t
               (let ((getter (the function (svref %%data-vector-reffers%% tag)))
                     (setter (the function (svref %%data-vector-setters%% tag))))
                 (declare (fixnum length))
                 (do ((forward-index 0 (1+ forward-index))
                      (backward-index (1- end) (1- backward-index)))
                     ((= forward-index length))
                   (declare (fixnum forward-index backward-index))
                   (funcall setter new-vector forward-index
                            (funcall getter vector backward-index))))))
        new-vector))))

;;;; NREVERSE

(defun list-nreverse (list)
  (do ((1st (cdr list) (if (endp 1st) 1st (cdr 1st)))
       (2nd list 1st)
       (3rd '() 2nd))
      ((atom 2nd) 3rd)
    (rplacd 2nd 3rd)))

(defun nreverse-word-specialized-vector (vector start end)
  (do ((left-index start (1+ left-index))
       (right-index (1- end) (1- right-index)))
      ((<= right-index left-index))
    (declare (type index left-index right-index))
    (let ((left (%vector-raw-bits vector left-index))
          (right (%vector-raw-bits vector right-index)))
      (setf (%vector-raw-bits vector left-index) right
            (%vector-raw-bits vector right-index) left)))
  vector)

(defun vector-nreverse (vector)
  (declare (vector vector))
  (when (> (length vector) 1)
    (with-array-data ((vector vector) (start) (end)
                      :check-fill-pointer t)
      (let ((tag (%other-pointer-widetag vector)))
        (cond ((= tag sb!vm:simple-vector-widetag)
               (do ((left-index start (1+ left-index))
                    (right-index (1- end) (1- right-index)))
                   ((<= right-index left-index))
                 (declare (type index left-index right-index))
                 (let ((left (svref vector left-index))
                       (right (svref vector right-index)))
                   (setf (svref vector left-index) right
                         (svref vector right-index) left))))
              ((word-specialized-vector-tag-p tag)
               (nreverse-word-specialized-vector vector start end))
              (t
               (let* ((getter (the function (svref %%data-vector-reffers%% tag)))
                      (setter (the function (svref %%data-vector-setters%% tag))))
                 (do ((left-index start (1+ left-index))
                      (right-index (1- end) (1- right-index)))
                     ((<= right-index left-index))
                   (declare (type index left-index right-index))
                   (let ((left (funcall getter vector left-index))
                         (right (funcall getter vector right-index)))
                     (funcall setter vector left-index right)
                     (funcall setter vector right-index left)))))))))
  vector)

(defun nreverse (sequence)
  #!+sb-doc
  "Return a sequence of the same elements in reverse order; the argument
   is destroyed."
  (declare (explicit-check))
  (seq-dispatch-checking sequence
    (list-nreverse sequence)
    (vector-nreverse sequence)
    ;; The type deriver for this is 'result-type-first-arg',
    ;; meaning it should return definitely an EXTENDED-SEQUENCE
    ;; and not a list or vector.
    (the extended-sequence (values (sb!sequence:nreverse sequence)))))


(defmacro sb!sequence:dosequence ((element sequence &optional return) &body body)
  #!+sb-doc
  "Executes BODY with ELEMENT subsequently bound to each element of
  SEQUENCE, then returns RETURN."
  (multiple-value-bind (forms decls) (parse-body body nil)
    (once-only ((sequence sequence))
      (with-unique-names (state limit from-end step endp elt)
        `(block nil
           (seq-dispatch ,sequence
             (dolist (,element ,sequence ,return) ,@body)
             (do-vector-data (,element ,sequence ,return) ,@body)
             (multiple-value-bind (,state ,limit ,from-end ,step ,endp ,elt)
                 (sb!sequence:make-sequence-iterator ,sequence)
               (declare (function ,step ,endp ,elt))
               (do ((,state ,state (funcall ,step ,sequence ,state ,from-end)))
                   ((funcall ,endp ,sequence ,state ,limit ,from-end)
                    (let ((,element nil))
                      ,@(filter-dolist-declarations decls)
                      (declare (ignorable ,element))
                      ,return))
                 (let ((,element (funcall ,elt ,sequence ,state)))
                   ,@decls
                   (tagbody
                      ,@forms))))))))))


;;;; CONCATENATE

(defun concatenate (result-type &rest sequences)
  #!+sb-doc
  "Return a new sequence of all the argument sequences concatenated together
  which shares no structure with the original argument sequences of the
  specified RESULT-TYPE."
  (declare (explicit-check)
           (dynamic-extent sequences))
  (flet ((concat-to-simple* (type-spec sequences)
           (do ((seqs sequences (cdr seqs))
                (total-length 0)
                (lengths ()))
               ((null seqs)
                (do ((sequences sequences (cdr sequences))
                     (lengths lengths (cdr lengths))
                     (index 0)
                     (result (make-sequence type-spec total-length)))
                    ((= index total-length) result)
                  (declare (fixnum index))
                  (let ((sequence (car sequences)))
                    (sb!sequence:dosequence (e sequence)
                      (setf (aref result index) e)
                      (incf index)))))
             (let ((length (length (car seqs))))
               (declare (fixnum length))
               (setq lengths (nconc lengths (list length)))
               (setq total-length (+ total-length length))))))
    (case result-type
      ;; Pick up some common cases first
      (list
       (apply #'%concatenate-to-list sequences))
      ((vector simple-vector)
       (apply #'%concatenate-to-simple-vector sequences))
      #!+sb-unicode
      ((string simple-string)
       (apply #'%concatenate-to-string sequences))
      ((simple-base-string #!-sb-unicode string #!-sb-unicode simple-string)
       (apply #'%concatenate-to-base-string sequences))
      (t
       (let ((type (specifier-type result-type)))
         (cond
           ((csubtypep type (specifier-type 'list))
            (cond
              ((type= type (specifier-type 'list))
               (apply #'%concatenate-to-list sequences))
              ((eq type *empty-type*)
               (bad-sequence-type-error nil))
              ((type= type (specifier-type 'null))
               (unless (every #'emptyp sequences)
                 (sequence-type-length-mismatch-error
                  type (reduce #'+ sequences :key #'length))) ; FIXME: circular list issues.
               '())
              ((cons-type-p type)
               (multiple-value-bind (min exactp)
                   (sb!kernel::cons-type-length-info type)
                 (let ((length (reduce #'+ sequences :key #'length)))
                   (if exactp
                       (unless (= length min)
                         (sequence-type-length-mismatch-error type length))
                       (unless (>= length min)
                         (sequence-type-length-mismatch-error type length)))
                   (apply #'%concatenate-to-list sequences))))
              (t (sequence-type-too-hairy (type-specifier type)))))
           ((csubtypep type (specifier-type 'vector))
            (concat-to-simple* result-type sequences))
           ((when-extended-sequence-type
                (result-type type :expandedp nil :prototype prototype)
              ;; This function has the EXPLICIT-CHECK declaration,
              ;; so we manually assert that it returns a SEQUENCE.
              (the extended-sequence
                   (apply #'sb!sequence:concatenate prototype sequences))))
           (t
            (bad-sequence-type-error result-type))))))))

;;; Efficient out-of-line concatenate for strings. Compiler transforms
;;; CONCATENATE 'STRING &co into these.
(macrolet ((def (name element-type &rest dispatch)
             `(defun ,name (&rest sequences)
                (declare (explicit-check)
                         (optimize (sb!c::insert-array-bounds-checks 0)))
                (let ((length 0))
                  (declare (index length))
                  (do-rest-arg ((seq) sequences)
                    (incf length (length seq)))
                  (let ((result (make-array length :element-type ',element-type))
                        (start 0))
                    (declare (index start))
                    (do-rest-arg ((seq) sequences)
                      (string-dispatch (,@dispatch t)
                                       seq
                        (let ((length (length seq)))
                          (replace result seq :start1 start)
                          (incf start length))))
                    result)))))
  #!+sb-unicode
  (def %concatenate-to-string character
    (simple-array character (*)) (simple-array base-char (*)))
  (def %concatenate-to-base-string base-char
    (simple-array base-char (*)) #!+sb-unicode (simple-array character (*)))
  (def %concatenate-to-simple-vector t simple-vector))

(defun %concatenate-to-list (&rest sequences)
  (declare (explicit-check))
  (let* ((result (list nil))
         (splice result))
    (do-rest-arg ((sequence) sequences)
      (sb!sequence:dosequence (e sequence)
        (setf splice (cdr (rplacd splice (list e))))))
    (cdr result)))

(defun %concatenate-to-vector (widetag &rest sequences)
  (declare (explicit-check))
  (let ((length 0))
    (declare (index length))
    (do-rest-arg ((seq) sequences)
      (incf length (length seq)))
    (let ((result (allocate-vector-with-widetag widetag length))
          (setter (the function (svref %%data-vector-setters%% widetag)))
          (index 0))
      (declare (index index))
      (do-rest-arg ((seq) sequences)
        (sb!sequence:dosequence (e seq)
          (funcall setter result index e)
          (incf index)))
      result)))

;;;; MAP

;;; helper functions to handle arity-1 subcases of MAP
(defun %map-to-list-arity-1 (fun sequence)
  (declare (explicit-check))
  (let ((reversed-result nil)
        (really-fun (%coerce-callable-to-fun fun)))
    (sb!sequence:dosequence (element sequence)
      (push (funcall really-fun element)
            reversed-result))
    (nreverse reversed-result)))
(defun %map-to-simple-vector-arity-1 (fun sequence)
  (declare (explicit-check))
  (let ((result (make-array (length sequence)))
        (index 0)
        (really-fun (%coerce-callable-to-fun fun)))
    (declare (type index index))
    (sb!sequence:dosequence (element sequence)
      (setf (aref result index)
            (funcall really-fun element))
      (incf index))
    result))
(defun %map-for-effect-arity-1 (fun sequence)
  (declare (explicit-check))
  (let ((really-fun (%coerce-callable-to-fun fun)))
    (sb!sequence:dosequence (element sequence)
      (funcall really-fun element)))
  nil)

(declaim (maybe-inline %map-for-effect))
(defun %map-for-effect (fun sequences)
  (declare (type function fun) (type list sequences))
  (let ((%sequences sequences)
        (%iters (mapcar (lambda (s)
                          (seq-dispatch s
                            s
                            0
                            (multiple-value-list
                             (sb!sequence:make-sequence-iterator s))))
                        sequences))
        (%apply-args (make-list (length sequences))))
    ;; this is almost efficient (except in the general case where we
    ;; trampoline to MAKE-SEQUENCE-ITERATOR; if we had DX allocation
    ;; of MAKE-LIST, the whole of %MAP would be cons-free.
    ;; TODO: on x86-64, we do have. Now see if the above remark is true.
    (declare (type list %sequences %iters %apply-args))
    (loop
     (do ((in-sequences  %sequences  (cdr in-sequences))
          (in-iters      %iters      (cdr in-iters))
          (in-apply-args %apply-args (cdr in-apply-args)))
         ((null in-sequences) (apply fun %apply-args))
       (let ((i (car in-iters)))
         (declare (type (or list index) i))
         (cond
           ((listp (car in-sequences))
            (if (null i)
                (return-from %map-for-effect nil)
                (setf (car in-apply-args) (car i)
                      (car in-iters) (cdr i))))
           ((typep i 'index)
            (let ((v (the vector (car in-sequences))))
              (if (>= i (length v))
                  (return-from %map-for-effect nil)
                  (setf (car in-apply-args) (aref v i)
                        (car in-iters) (1+ i)))))
           (t
            ;; While on one hand this could benefit from a zero-safety ds-bind,
            ;; on the other, why not coerce these tuples to vectors or structs?
            (destructuring-bind (state limit from-end step endp elt &rest ignore)
                i
              (declare (type function step endp elt)
                       (ignore ignore))
              (let ((s (car in-sequences)))
                (if (funcall endp s state limit from-end)
                    (return-from %map-for-effect nil)
                    (progn
                      (setf (car in-apply-args) (funcall elt s state))
                      (setf (caar in-iters) (funcall step s state from-end)))))))))))))
(defun %map-to-list (fun sequences)
  (declare (type function fun)
           (type list sequences))
  (let ((result nil))
    (flet ((f (&rest args)
             (declare (truly-dynamic-extent args))
             (push (apply fun args) result)))
      (declare (truly-dynamic-extent #'f))
      (%map-for-effect #'f sequences))
    (nreverse result)))
(defun %map-to-vector (output-type-spec fun sequences)
  (declare (type function fun)
           (type list sequences))
  (let ((min-len 0))
    (flet ((f (&rest args)
             (declare (truly-dynamic-extent args))
             (declare (ignore args))
             (incf min-len)))
      (declare (truly-dynamic-extent #'f))
      (%map-for-effect #'f sequences))
    (let ((result (make-sequence output-type-spec min-len))
          (i 0))
      (declare (type (simple-array * (*)) result))
      (flet ((f (&rest args)
               (declare (truly-dynamic-extent args))
               (setf (aref result i) (apply fun args))
               (incf i)))
        (declare (truly-dynamic-extent #'f))
        (%map-for-effect #'f sequences))
      result)))

;;; %MAP is just MAP without the final just-to-be-sure check that
;;; length of the output sequence matches any length specified
;;; in RESULT-TYPE.
(defun %map (result-type function &rest sequences)
  (declare (explicit-check))
  (declare (dynamic-extent sequences))
  ;; Everything that we end up calling uses %COERCE-TO-CALLABLE
  ;; on FUNCTION so we don't need to declare it of type CALLABLE here.
  ;; Additionally all the arity-1 mappers use SEQ-DISPATCH which asserts
  ;; that the input is a SEQUENCE. Despite SEQ-DISPATCH being "less safe"
  ;; than SEQ-DISPATCH-CHECKING, both are in fact equally safe, because
  ;; the ARRAY case (which assumes that all arrays are vectors) utilizes
  ;; %WITH-ARRAY-DATA/FP which asserts that its input is a vector.
  (labels ((slower-map (type)
             (let ((really-fun (%coerce-callable-to-fun function)))
               (cond
                 ((eq type *empty-type*)
                  (%map-for-effect really-fun sequences))
                 ((csubtypep type (specifier-type 'list))
                  (%map-to-list really-fun sequences))
                 ((csubtypep type (specifier-type 'vector))
                  (%map-to-vector result-type really-fun sequences))
                 ((when-extended-sequence-type
                      (result-type type :expandedp nil :prototype prototype)
                    ;; This function has the EXPLICIT-CHECK
                    ;; declaration, so we manually assert that it
                    ;; returns a SEQUENCE.
                    (the extended-sequence
                         (apply #'sb!sequence:map
                                prototype really-fun sequences))))
                 (t
                  (bad-sequence-type-error result-type))))))
    ;; Handle some easy cases faster
    (if (/= (length sequences) 1)
        (slower-map (specifier-type result-type))
        (let ((first-sequence (fast-&rest-nth 0 sequences)))
          (case result-type
           ((nil)
            (%map-for-effect-arity-1 function first-sequence))
           ((list cons)
            (%map-to-list-arity-1 function first-sequence))
           ((vector simple-vector)
            (%map-to-simple-vector-arity-1 function first-sequence))
           (t
            (let ((type (specifier-type result-type)))
              (cond ((eq type *empty-type*)
                     (%map-for-effect-arity-1 function first-sequence))
                    ((csubtypep type (specifier-type 'list))
                     (%map-to-list-arity-1 function first-sequence))
                    ((csubtypep type (specifier-type '(vector t)))
                     (%map-to-simple-vector-arity-1 function first-sequence))
                    (t
                     (slower-map type))))))))))

(defun map (result-type function first-sequence &rest more-sequences)
  (declare (explicit-check))
  (let ((result
         (apply #'%map result-type function first-sequence more-sequences)))
    (if (or (eq result-type 'nil) (typep result result-type))
        result
        (error 'simple-type-error
               :format-control "MAP result ~S is not a sequence of type ~S"
               :datum result
               :expected-type result-type
               :format-arguments (list result result-type)))))

;;;; MAP-INTO

(defmacro map-into-lambda (sequences params &body body)
  (check-type sequences symbol)
  `(flet ((f ,params ,@body))
     (declare (truly-dynamic-extent #'f))
     ;; Note (MAP-INTO SEQ (LAMBDA () ...)) is a different animal,
     ;; hence the awkward flip between MAP and LOOP.
     (if ,sequences
         (apply #'%map nil #'f ,sequences)
         (loop (f)))))

(!define-array-dispatch vector-map-into (data start end fun sequences)
  (declare (type index start end)
           (type function fun)
           (type list sequences))
  (declare (explicit-check))
  (let ((index start))
    (declare (type index index))
    (block mapping
      (map-into-lambda sequences (&rest args)
        (declare (truly-dynamic-extent args))
        (when (eql index end)
          (return-from mapping))
        (setf (aref data index) (apply fun args))
        (incf index)))
    index))

;;; Uses the machinery of (MAP NIL ...). For non-vectors we avoid
;;; computing the length of the result sequence since we can detect
;;; the end during mapping (if MAP even gets that far).
;;;
;;; For each result type, define a mapping function which is
;;; responsible for replacing RESULT-SEQUENCE elements and for
;;; terminating itself if the end of RESULT-SEQUENCE is reached.
;;; The mapping function is defined with MAP-INTO-LAMBDA.
;;;
;;; MAP-INTO-LAMBDAs are optimized since they are the inner loops.
;;; Because we are manually doing bounds checking with known types,
;;; safety is turned off for vectors and lists but kept for generic
;;; sequences.
(defun map-into (result-sequence function &rest sequences)
  (declare (optimize (sb!c::check-tag-existence 0)))
  (let ((really-fun (%coerce-callable-to-fun function)))
    (etypecase result-sequence
      (vector
       (with-array-data ((data result-sequence) (start) (end)
                         ;; MAP-INTO ignores fill pointer when mapping
                         :check-fill-pointer nil)
         (let ((new-end (vector-map-into data start end really-fun sequences)))
           (when (array-has-fill-pointer-p result-sequence)
             (setf (fill-pointer result-sequence) (- new-end start))))))
      (list
       (let ((node result-sequence))
         (declare (type list node))
         (map-into-lambda sequences (&rest args)
           (declare (truly-dynamic-extent args))
           (cond ((null node)
                  (return-from map-into result-sequence))
                 ((not (listp (cdr node)))
                  (error 'simple-type-error
                         :format-control "~a is not a proper list"
                         :format-arguments (list result-sequence)
                         :expected-type 'list
                         :datum result-sequence)))
           (setf (car node) (apply really-fun args))
           (setf node (cdr node)))))
      (sequence
       (multiple-value-bind (iter limit from-end)
           (sb!sequence:make-sequence-iterator result-sequence)
         (map-into-lambda sequences (&rest args)
           (declare (truly-dynamic-extent args) (optimize speed))
           (when (sb!sequence:iterator-endp result-sequence
                                            iter limit from-end)
             (return-from map-into result-sequence))
           (setf (sb!sequence:iterator-element result-sequence iter)
                 (apply really-fun args))
           (setf iter (sb!sequence:iterator-step result-sequence
                                                           iter from-end)))))))
  result-sequence)

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

(define-sequence-traverser reduce (function sequence &rest args &key key
                                   from-end start end (initial-value nil ivp))
  (declare (type index start)
           (truly-dynamic-extent args))
  (seq-dispatch sequence
    (let ((end (or end length)))
      (declare (type index end))
      (if (= end start)
          (if ivp initial-value (funcall function))
          (if from-end
              (list-reduce-from-end function sequence key start end
                                    initial-value ivp)
              (list-reduce function sequence key start end
                           initial-value ivp))))
    (let ((end (or end length)))
      (declare (type index end))
      (if (= end start)
          (if ivp initial-value (funcall function))
          (if from-end
              (progn
                (when (not ivp)
                  (setq end (1- (the fixnum end)))
                  (setq initial-value (apply-key key (aref sequence end))))
                (mumble-reduce-from-end function sequence key start end
                                        initial-value aref))
              (progn
                (when (not ivp)
                  (setq initial-value (apply-key key (aref sequence start)))
                  (setq start (1+ start)))
                (mumble-reduce function sequence key start end
                               initial-value aref)))))
    (apply #'sb!sequence:reduce function sequence args)))

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
     (declare (truly-dynamic-extent handle))
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
     (declare (truly-dynamic-extent handle))
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
    (item sequence &rest args &key from-end test test-not start
     end count key)
  #!+sb-doc
  "Return a sequence formed by destructively removing the specified ITEM from
  the given SEQUENCE."
  (declare (type fixnum start)
           (truly-dynamic-extent args))
  (seq-dispatch sequence
    (let ((end (or end length)))
      (declare (type index end))
      (if from-end
          (normal-list-delete-from-end)
          (normal-list-delete)))
    (let ((end (or end length)))
      (declare (type index end))
      (if from-end
          (normal-mumble-delete-from-end)
          (normal-mumble-delete)))
    (apply #'sb!sequence:delete item sequence args)))

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
    (predicate sequence &rest args &key from-end start key end count)
  #!+sb-doc
  "Return a sequence formed by destructively removing the elements satisfying
  the specified PREDICATE from the given SEQUENCE."
  (declare (type fixnum start)
           (truly-dynamic-extent args))
  (seq-dispatch sequence
    (let ((end (or end length)))
      (declare (type index end))
      (if from-end
          (if-list-delete-from-end)
          (if-list-delete)))
    (let ((end (or end length)))
      (declare (type index end))
      (if from-end
          (if-mumble-delete-from-end)
          (if-mumble-delete)))
    (apply #'sb!sequence:delete-if predicate sequence args)))

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
    (predicate sequence &rest args &key from-end start end key count)
  #!+sb-doc
  "Return a sequence formed by destructively removing the elements not
  satisfying the specified PREDICATE from the given SEQUENCE."
  (declare (type fixnum start)
           (truly-dynamic-extent args))
  (seq-dispatch sequence
    (let ((end (or end length)))
      (declare (type index end))
      (if from-end
          (if-not-list-delete-from-end)
          (if-not-list-delete)))
    (let ((end (or end length)))
      (declare (type index end))
      (if from-end
          (if-not-mumble-delete-from-end)
          (if-not-mumble-delete)))
    (apply #'sb!sequence:delete-if-not predicate sequence args)))

;;;; REMOVE

(eval-when (:compile-toplevel :execute)

;;; MUMBLE-REMOVE-MACRO does not include (removes) each element that
;;; satisfies the predicate.
(sb!xc:defmacro mumble-remove-macro (bump left begin finish right pred)
  `(do ((index ,begin (,bump index))
        (result
         (do ((index ,left (,bump index))
              (result (%make-sequence-like sequence length)))
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
            ((= index (the fixnum ,right)) (%shrink-vector result new-index))
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
    (item sequence &rest args &key from-end test test-not start
     end count key)
  #!+sb-doc
  "Return a copy of SEQUENCE with elements satisfying the test (default is
   EQL) with ITEM removed."
  (declare (type fixnum start)
           (truly-dynamic-extent args))
  (seq-dispatch sequence
    (let ((end (or end length)))
      (declare (type index end))
      (if from-end
          (normal-list-remove-from-end)
          (normal-list-remove)))
    (let ((end (or end length)))
      (declare (type index end))
      (if from-end
          (normal-mumble-remove-from-end)
          (normal-mumble-remove)))
    (apply #'sb!sequence:remove item sequence args)))

(define-sequence-traverser remove-if
    (predicate sequence &rest args &key from-end start end count key)
  #!+sb-doc
  "Return a copy of sequence with elements satisfying PREDICATE removed."
  (declare (type fixnum start)
           (truly-dynamic-extent args))
  (seq-dispatch sequence
    (let ((end (or end length)))
      (declare (type index end))
      (if from-end
          (if-list-remove-from-end)
          (if-list-remove)))
    (let ((end (or end length)))
      (declare (type index end))
      (if from-end
          (if-mumble-remove-from-end)
          (if-mumble-remove)))
    (apply #'sb!sequence:remove-if predicate sequence args)))

(define-sequence-traverser remove-if-not
    (predicate sequence &rest args &key from-end start end count key)
  #!+sb-doc
  "Return a copy of sequence with elements not satisfying PREDICATE removed."
  (declare (type fixnum start)
           (truly-dynamic-extent args))
  (seq-dispatch sequence
    (let ((end (or end length)))
      (declare (type index end))
      (if from-end
          (if-not-list-remove-from-end)
          (if-not-list-remove)))
    (let ((end (or end length)))
      (declare (type index end))
      (if from-end
          (if-not-mumble-remove-from-end)
          (if-not-mumble-remove)))
    (apply #'sb!sequence:remove-if-not predicate sequence args)))

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
  (let ((result (%make-sequence-like vector length))
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
      (unless (or (and from-end
                       (if test-not
                           (position (apply-key key elt) result
                                     :start start :end jndex
                                     :test-not test-not :key key)
                           (position (apply-key key elt) result
                                     :start start :end jndex
                                     :test test :key key)))
                  (and (not from-end)
                       (if test-not
                           (position (apply-key key elt) vector
                                     :start (1+ index) :end end
                                     :test-not test-not :key key)
                           (position (apply-key key elt) vector
                                     :start (1+ index) :end end
                                     :test test :key key))))
        (setf (aref result jndex) elt)
        (setq jndex (1+ jndex)))
      (setq index (1+ index)))
    (do ()
        ((= index length))
      (setf (aref result jndex) (aref vector index))
      (setq index (1+ index))
      (setq jndex (1+ jndex)))
    (%shrink-vector result jndex)))

(define-sequence-traverser remove-duplicates
    (sequence &rest args &key test test-not start end from-end key)
  #!+sb-doc
  "The elements of SEQUENCE are compared pairwise, and if any two match,
   the one occurring earlier is discarded, unless FROM-END is true, in
   which case the one later in the sequence is discarded. The resulting
   sequence is returned.

   The :TEST-NOT argument is deprecated."
  (declare (fixnum start)
           (truly-dynamic-extent args))
  (seq-dispatch sequence
    (if sequence
        (list-remove-duplicates* sequence test test-not
                                 start end key from-end))
    (vector-remove-duplicates* sequence test test-not start end key from-end)
    (apply #'sb!sequence:remove-duplicates sequence args)))

;;;; DELETE-DUPLICATES

(defun list-delete-duplicates* (list test test-not key from-end start end)
  (declare (fixnum start))
  (let ((handle (cons nil list)))
    (declare (truly-dynamic-extent handle))
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
            (shrink-vector vector jndex))
         (setf (aref vector jndex) (aref vector index))))
    (declare (fixnum index jndex))
    (setf (aref vector jndex) (aref vector index))
    (unless (if test-not
                (position (apply-key key (aref vector index)) vector :key key
                          :start (if from-end start (1+ index))
                          :end (if from-end jndex end)
                          :test-not test-not)
                (position (apply-key key (aref vector index)) vector :key key
                          :start (if from-end start (1+ index))
                          :end (if from-end jndex end)
                          :test test))
      (setq jndex (1+ jndex)))))

(define-sequence-traverser delete-duplicates
    (sequence &rest args &key test test-not start end from-end key)
  #!+sb-doc
  "The elements of SEQUENCE are examined, and if any two match, one is
   discarded. The resulting sequence, which may be formed by destroying the
   given sequence, is returned.

   The :TEST-NOT argument is deprecated."
  (declare (truly-dynamic-extent args))
  (seq-dispatch sequence
    (when sequence
      (list-delete-duplicates* sequence test test-not
                               key from-end start end))
    (vector-delete-duplicates* sequence test test-not key from-end start end)
    (apply #'sb!sequence:delete-duplicates sequence args)))

;;;; SUBSTITUTE

(defun list-substitute* (pred new list start end count key test test-not old)
  (declare (fixnum start end count)
           (type (or null function) key)
           (optimize speed))
  (let* ((result (list nil))
         (test (or test-not test))
         (test-not (or test-not
                       (eq pred 'if-not)))
         elt
         (splice result)
         (list list))      ; Get a local list for a stepper.
    (declare (function test))
    (do ((index 0 (1+ index)))
        ((= index start))
      (declare (fixnum index))
      (setf splice (cdr (rplacd splice (list (car list))))
            list (cdr list)))
    (do ((index start (1+ index)))
        ((or (= index end) (null list) (= count 0)))
      (declare (fixnum index))
      (setf elt (car list)
            splice
            (cdr (rplacd splice
                         (list
                          (cond ((let* ((elt (apply-key key elt))
                                        (value (if (eq pred 'normal)
                                                   (funcall test old elt)
                                                   (funcall test elt))))
                                   (if test-not
                                       (not value)
                                       value))
                                 (decf count)
                                 new)
                                (t elt)))))
            list (cdr list)))
    (do ()
        ((null list))
      (setf splice (cdr (rplacd splice (list (car list))))
            list (cdr list)))
    (cdr result)))

;;; Replace old with new in sequence moving from left to right by incrementer
;;; on each pass through the loop. Called by all three substitute functions.
(defun vector-substitute* (pred new sequence incrementer left right length
                           start end count key test test-not old)
  (declare (fixnum start count end incrementer right)
           (type (or null function) key))
  (let* ((result (make-vector-like sequence length))
         (getter (the function (svref %%data-vector-reffers%%
                                      (%other-pointer-widetag sequence))))
         (setter (the function (svref %%data-vector-setters%%
                                      (%other-pointer-widetag result))))
         (test (or test-not test))
         (test-not (or test-not
                       (eq pred 'if-not)))
         (index left))
    (declare (fixnum index)
             (function test))
    (do ()
        ((= index start))
      (funcall setter result index
               (funcall getter sequence index))
      (incf index incrementer))
    (do ((elt))
        ((or (= index end) (= count 0)))
      (setf elt (funcall getter sequence index))
      (funcall setter result index
               (cond ((let* ((elt (apply-key key elt))
                             (value (if (eq pred 'normal)
                                        (funcall test old elt)
                                        (funcall test elt))))
                        (if test-not
                            (not value)
                            value))
                      (decf count)
                      new)
                     (t elt)))
      (incf index incrementer))
    (do ()
        ((= index right))
      (funcall setter result index
               (funcall getter sequence index))
      (incf index incrementer))
    result))

(eval-when (:compile-toplevel :execute)

(sb!xc:defmacro subst-dispatch (pred)
  `(seq-dispatch sequence
     (let ((end (or end length)))
       (declare (type index end))
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
                             old)))

     (let ((end (or end length)))
       (declare (type index end))
       (if from-end
           (vector-substitute* ,pred new sequence -1 (1- (the fixnum length))
                               -1 length (1- (the fixnum end))
                               (1- (the fixnum start))
                               count key test test-not old)
           (vector-substitute* ,pred new sequence 1 0 length length
                               start end count key test test-not old)))

    ;; FIXME: wow, this is an odd way to implement the dispatch.  PRED
    ;; here is (QUOTE [NORMAL|IF|IF-NOT]).  Not only is this pretty
    ;; pointless, but also LIST-SUBSTITUTE* and VECTOR-SUBSTITUTE*
    ;; dispatch once per element on PRED's run-time identity.
    ,(ecase (cadr pred)
       ((normal) `(apply #'sb!sequence:substitute new old sequence args))
       ((if) `(apply #'sb!sequence:substitute-if new predicate sequence args))
       ((if-not) `(apply #'sb!sequence:substitute-if-not new predicate sequence args)))))
) ; EVAL-WHEN

(define-sequence-traverser substitute
    (new old sequence &rest args &key from-end test test-not
         start count end key)
  #!+sb-doc
  "Return a sequence of the same kind as SEQUENCE with the same elements,
  except that all elements equal to OLD are replaced with NEW."
  (declare (type fixnum start)
           (truly-dynamic-extent args))
  (subst-dispatch 'normal))

;;;; SUBSTITUTE-IF, SUBSTITUTE-IF-NOT

(define-sequence-traverser substitute-if
    (new predicate sequence &rest args &key from-end start end count key)
  #!+sb-doc
  "Return a sequence of the same kind as SEQUENCE with the same elements
  except that all elements satisfying the PRED are replaced with NEW."
  (declare (type fixnum start)
           (truly-dynamic-extent args))
  (let ((test predicate)
        (test-not nil)
        old)
    (subst-dispatch 'if)))

(define-sequence-traverser substitute-if-not
    (new predicate sequence &rest args &key from-end start end count key)
  #!+sb-doc
  "Return a sequence of the same kind as SEQUENCE with the same elements
  except that all elements not satisfying the PRED are replaced with NEW."
  (declare (type fixnum start)
           (truly-dynamic-extent args))
  (let ((test predicate)
        (test-not nil)
        old)
    (subst-dispatch 'if-not)))

;;;; NSUBSTITUTE

(define-sequence-traverser nsubstitute
    (new old sequence &rest args &key from-end test test-not
         end count key start)
  #!+sb-doc
  "Return a sequence of the same kind as SEQUENCE with the same elements
  except that all elements equal to OLD are replaced with NEW. SEQUENCE
  may be destructively modified."
  (declare (type fixnum start)
           (truly-dynamic-extent args))
  (seq-dispatch sequence
    (let ((end (or end length)))
      (declare (type index end))
      (if from-end
          (nreverse (nlist-substitute*
                     new old (nreverse (the list sequence))
                     test test-not (- length end) (- length start)
                     count key))
          (nlist-substitute* new old sequence
                             test test-not start end count key)))
    (let ((end (or end length)))
      (declare (type index end))
      (if from-end
          (nvector-substitute* new old sequence -1
                               test test-not (1- end) (1- start) count key)
          (nvector-substitute* new old sequence 1
                               test test-not start end count key)))
    (apply #'sb!sequence:nsubstitute new old sequence args)))

(defun nlist-substitute* (new old sequence test test-not start end count key)
  (declare (fixnum start count end)
           (type (or null function) key))
  (do ((test (or test-not test))
       (list (nthcdr start sequence) (cdr list))
       (index start (1+ index)))
      ((or (= index end) (null list) (= count 0)) sequence)
    (declare (fixnum index)
             (function test))
    (let ((value (funcall test old (apply-key key (car list)))))
     (when (if test-not
               (not value)
               value)
       (rplaca list new)
       (decf count)))))

(defun nvector-substitute* (new old sequence incrementer
                            test test-not start end count key)
  (declare (fixnum start count end)
           (type (integer -1 1) incrementer)
           (type (or null function) key))
  (let* ((test (or test-not test))
         (tag (%other-pointer-widetag sequence))
         (getter (the function (svref %%data-vector-reffers%% tag)))
         (setter (the function (svref %%data-vector-setters%% tag))))
    (declare (function test))
    (do ((index start (+ index incrementer)))
        ((or (= index end) (= count 0)) sequence)
      (declare (fixnum index))
      (let* ((value (apply-key key (funcall getter sequence index)))
             (test (and (funcall test old value) 0)))
        (when (if test-not
                  (not test)
                  test)
          (funcall setter sequence index new)
          (decf count))))))

;;;; NSUBSTITUTE-IF, NSUBSTITUTE-IF-NOT

(define-sequence-traverser nsubstitute-if
    (new predicate sequence &rest args &key from-end start end count key)
  #!+sb-doc
  "Return a sequence of the same kind as SEQUENCE with the same elements
   except that all elements satisfying PREDICATE are replaced with NEW.
   SEQUENCE may be destructively modified."
  (declare (type fixnum start)
           (truly-dynamic-extent args))
  (seq-dispatch sequence
    (let ((end (or end length)))
      (declare (type index end))
      (if from-end
          (nreverse (nlist-substitute-if*
                     new predicate (nreverse (the list sequence))
                     (- length end) (- length start) count key))
          (nlist-substitute-if* new predicate sequence
                                start end count key)))
    (let ((end (or end length)))
      (declare (type index end))
      (if from-end
          (nvector-substitute-if* new predicate sequence -1
                                  (1- end) (1- start) count key)
          (nvector-substitute-if* new predicate sequence 1
                                  start end count key)))
    (apply #'sb!sequence:nsubstitute-if new predicate sequence args)))

(defun nlist-substitute-if* (new test sequence start end count key)
  (declare (type fixnum start end count)
           (type (or null function) key)
           (type function test)) ; coercion is done by caller
  (do ((list (nthcdr start sequence) (cdr list))
       (index start (1+ index)))
      ((or (= index end) (null list) (= count 0)) sequence)
    (declare (fixnum index))
    (when (funcall test (apply-key key (car list)))
      (rplaca list new)
      (decf count))))

(defun nvector-substitute-if* (new test sequence incrementer
                               start end count key)
  (declare (type fixnum end count)
           (type (integer -1 1) incrementer)
           (type (or null function) key)
           (type function test)) ; coercion is done by caller
  (let* ((tag (%other-pointer-widetag sequence))
         (getter (the function (svref %%data-vector-reffers%% tag)))
         (setter (the function (svref %%data-vector-setters%% tag))))
    (do ((index start (+ index incrementer)))
        ((or (= index end) (= count 0)) sequence)
      (declare (fixnum index))
      (when (funcall test (apply-key key (funcall getter sequence index)))
        (funcall setter sequence index new)
        (decf count)))))

(define-sequence-traverser nsubstitute-if-not
    (new predicate sequence &rest args &key from-end start end count key)
  #!+sb-doc
  "Return a sequence of the same kind as SEQUENCE with the same elements
   except that all elements not satisfying PREDICATE are replaced with NEW.
   SEQUENCE may be destructively modified."
  (declare (type fixnum start)
           (truly-dynamic-extent args))
  (seq-dispatch sequence
    (let ((end (or end length)))
      (declare (fixnum end))
      (if from-end
          (nreverse (nlist-substitute-if-not*
                     new predicate (nreverse (the list sequence))
                     (- length end) (- length start) count key))
          (nlist-substitute-if-not* new predicate sequence
                                    start end count key)))
    (let ((end (or end length)))
      (declare (fixnum end))
      (if from-end
          (nvector-substitute-if-not* new predicate sequence -1
                                      (1- end) (1- start) count key)
          (nvector-substitute-if-not* new predicate sequence 1
                                      start end count key)))
    (apply #'sb!sequence:nsubstitute-if-not new predicate sequence args)))

(defun nlist-substitute-if-not* (new test sequence start end count key)
  (declare (type fixnum start end count)
           (type (or null function) key)
           (type function test))        ; coercion is done by caller
  (do ((list (nthcdr start sequence) (cdr list))
       (index start (1+ index)))
      ((or (= index end) (null list) (= count 0)) sequence)
    (declare (fixnum index))
    (when (not (funcall test (apply-key key (car list))))
      (rplaca list new)
      (decf count))))

(defun nvector-substitute-if-not* (new test sequence incrementer
                                   start end count key)
  (declare (type fixnum end count)
           (type (integer -1 1) incrementer)
           (type (or null function) key)
           (type function test))        ; coercion is done by caller
  (let* ((tag (%other-pointer-widetag sequence))
         (getter (the function (svref %%data-vector-reffers%% tag)))
         (setter (the function (svref %%data-vector-setters%% tag))))
    (do ((index start (+ index incrementer)))
        ((or (= index end) (= count 0)) sequence)
      (declare (fixnum index))
      (when (not (funcall test (apply-key key (funcall getter sequence index))))
        (funcall setter sequence index new)
        (decf count)))))

;;;; FIND, POSITION, and their -IF and -IF-NOT variants

(defun effective-find-position-test (test test-not)
  (effective-find-position-test test test-not))
(defun effective-find-position-key (key)
  (effective-find-position-key key))

;;; shared guts of out-of-line FIND, POSITION, FIND-IF, and POSITION-IF
(macrolet (;; shared logic for defining %FIND-POSITION and
           ;; %FIND-POSITION-IF in terms of various inlineable cases
           ;; of the expression defined in FROB and VECTOR*-FROB
           (frobs (&optional bit-frob)
             `(seq-dispatch sequence-arg
               (frob sequence-arg from-end)
               (with-array-data ((sequence sequence-arg :offset-var offset)
                                 (start start)
                                 (end end)
                                 :check-fill-pointer t)
                 (multiple-value-bind (f p)
                     (macrolet ((frob2 () `(if from-end
                                               (frob sequence t)
                                               (frob sequence nil))))
                       (typecase sequence
                         #!+sb-unicode
                         ((simple-array character (*)) (frob2))
                         ((simple-array base-char (*)) (frob2))
                         ,@(when bit-frob
                             `((simple-bit-vector
                                (if (and (typep item 'bit)
                                         (eq #'identity key)
                                         (or (eq #'eq test)
                                             (eq #'eql test)
                                             (eq #'equal test)))
                                    (let ((p (%bit-position item sequence
                                                            from-end start end)))
                                      (if p
                                          (values item p)
                                          (values nil nil)))
                                    (vector*-frob sequence)))))
                         (t
                          (vector*-frob sequence))))
                   (declare (type (or index null) p))
                   (values f (and p (the index (- p offset)))))))))
  (defun %find-position (item sequence-arg from-end start end key test)
    (macrolet ((frob (sequence from-end)
                 `(%find-position item ,sequence
                                  ,from-end start end key test))
               (vector*-frob (sequence)
                 `(%find-position-vector-macro item ,sequence
                                               from-end start end key test)))
      (frobs t)))
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

(defun find
    (item sequence &rest args &key from-end (start 0) end key test test-not)
  (declare (truly-dynamic-extent args))
  (seq-dispatch sequence
    (nth-value 0 (%find-position
                  item sequence from-end start end
                  (effective-find-position-key key)
                  (effective-find-position-test test test-not)))
    (nth-value 0 (%find-position
                  item sequence from-end start end
                  (effective-find-position-key key)
                  (effective-find-position-test test test-not)))
    (apply #'sb!sequence:find item sequence args)))
(defun position
    (item sequence &rest args &key from-end (start 0) end key test test-not)
  (declare (truly-dynamic-extent args))
  (seq-dispatch sequence
    (nth-value 1 (%find-position
                  item sequence from-end start end
                  (effective-find-position-key key)
                  (effective-find-position-test test test-not)))
    (nth-value 1 (%find-position
                  item sequence from-end start end
                  (effective-find-position-key key)
                  (effective-find-position-test test test-not)))
    (apply #'sb!sequence:position item sequence args)))

(defun find-if (predicate sequence &rest args &key from-end (start 0) end key)
  (declare (truly-dynamic-extent args))
  (seq-dispatch sequence
    (nth-value 0 (%find-position-if
                  (%coerce-callable-to-fun predicate)
                  sequence from-end start end
                  (effective-find-position-key key)))
    (nth-value 0 (%find-position-if
                  (%coerce-callable-to-fun predicate)
                  sequence from-end start end
                  (effective-find-position-key key)))
    (apply #'sb!sequence:find-if predicate sequence args)))
(defun position-if
    (predicate sequence &rest args &key from-end (start 0) end key)
  (declare (truly-dynamic-extent args))
  (seq-dispatch sequence
    (nth-value 1 (%find-position-if
                  (%coerce-callable-to-fun predicate)
                  sequence from-end start end
                  (effective-find-position-key key)))
    (nth-value 1 (%find-position-if
                  (%coerce-callable-to-fun predicate)
                  sequence from-end start end
                  (effective-find-position-key key)))
    (apply #'sb!sequence:position-if predicate sequence args)))

(defun find-if-not
    (predicate sequence &rest args &key from-end (start 0) end key)
  (declare (truly-dynamic-extent args))
  (seq-dispatch sequence
    (nth-value 0 (%find-position-if-not
                  (%coerce-callable-to-fun predicate)
                  sequence from-end start end
                  (effective-find-position-key key)))
    (nth-value 0 (%find-position-if-not
                  (%coerce-callable-to-fun predicate)
                  sequence from-end start end
                  (effective-find-position-key key)))
    (apply #'sb!sequence:find-if-not predicate sequence args)))
(defun position-if-not
    (predicate sequence &rest args &key from-end (start 0) end key)
  (declare (truly-dynamic-extent args))
  (seq-dispatch sequence
    (nth-value 1 (%find-position-if-not
                  (%coerce-callable-to-fun predicate)
                  sequence from-end start end
                  (effective-find-position-key key)))
    (nth-value 1 (%find-position-if-not
                  (%coerce-callable-to-fun predicate)
                  sequence from-end start end
                  (effective-find-position-key key)))
    (apply #'sb!sequence:position-if-not predicate sequence args)))

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

(define-sequence-traverser count-if
    (pred sequence &rest args &key from-end start end key)
  #!+sb-doc
  "Return the number of elements in SEQUENCE satisfying PRED(el)."
  (declare (type fixnum start)
           (truly-dynamic-extent args))
  (let ((pred (%coerce-callable-to-fun pred)))
    (seq-dispatch sequence
      (let ((end (or end length)))
        (declare (type index end))
        (if from-end
            (list-count-if nil t pred sequence)
            (list-count-if nil nil pred sequence)))
      (let ((end (or end length)))
        (declare (type index end))
        (if from-end
            (vector-count-if nil t pred sequence)
            (vector-count-if nil nil pred sequence)))
      (apply #'sb!sequence:count-if pred sequence args))))

(define-sequence-traverser count-if-not
    (pred sequence &rest args &key from-end start end key)
  #!+sb-doc
  "Return the number of elements in SEQUENCE not satisfying TEST(el)."
  (declare (type fixnum start)
           (truly-dynamic-extent args))
  (let ((pred (%coerce-callable-to-fun pred)))
    (seq-dispatch sequence
      (let ((end (or end length)))
        (declare (type index end))
        (if from-end
            (list-count-if t t pred sequence)
            (list-count-if t nil pred sequence)))
      (let ((end (or end length)))
        (declare (type index end))
        (if from-end
            (vector-count-if t t pred sequence)
            (vector-count-if t nil pred sequence)))
      (apply #'sb!sequence:count-if-not pred sequence args))))

(define-sequence-traverser count
    (item sequence &rest args &key from-end start end
          key (test #'eql test-p) (test-not nil test-not-p))
  #!+sb-doc
  "Return the number of elements in SEQUENCE satisfying a test with ITEM,
   which defaults to EQL."
  (declare (type fixnum start)
           (truly-dynamic-extent args))
  (when (and test-p test-not-p)
    ;; ANSI Common Lisp has left the behavior in this situation unspecified.
    ;; (CLHS 17.2.1)
    (error ":TEST and :TEST-NOT are both present."))
  (let ((%test (if test-not-p
                   (lambda (x)
                     (not (funcall test-not item x)))
                   (lambda (x)
                     (funcall test item x)))))
    (seq-dispatch sequence
      (let ((end (or end length)))
        (declare (type index end))
        (if from-end
            (list-count-if nil t %test sequence)
            (list-count-if nil nil %test sequence)))
      (let ((end (or end length)))
        (declare (type index end))
        (if from-end
            (vector-count-if nil t %test sequence)
            (vector-count-if nil nil %test sequence)))
      (apply #'sb!sequence:count item sequence args))))

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
    (sequence1 sequence2 &rest args &key from-end test test-not
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
  (declare (type fixnum start1 start2))
  (declare (truly-dynamic-extent args))
  (seq-dispatch sequence1
    (seq-dispatch sequence2
      (let ((end1 (or end1 length1))
            (end2 (or end2 length2)))
        (declare (type index end1 end2))
        (match-vars
         (matchify-list (sequence1 start1 length1 end1)
           (matchify-list (sequence2 start2 length2 end2)
             (list-list-mismatch)))))
      (let ((end1 (or end1 length1))
            (end2 (or end2 length2)))
        (declare (type index end1 end2))
        (match-vars
         (matchify-list (sequence1 start1 length1 end1)
           (list-mumble-mismatch))))
      (apply #'sb!sequence:mismatch sequence1 sequence2 args))
    (seq-dispatch sequence2
      (let ((end1 (or end1 length1))
            (end2 (or end2 length2)))
        (declare (type index end1 end2))
        (match-vars
         (matchify-list (sequence2 start2 length2 end2)
           (mumble-list-mismatch))))
      (let ((end1 (or end1 length1))
            (end2 (or end2 length2)))
        (declare (type index end1 end2))
        (match-vars
         (mumble-mumble-mismatch)))
      (apply #'sb!sequence:mismatch sequence1 sequence2 args))
    (apply #'sb!sequence:mismatch sequence1 sequence2 args)))


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
         (search-compare-list-vector ,main ,sub)
         ;; KLUDGE: just hack it together so that it works
         (return-from search (apply #'sb!sequence:search sequence1 sequence2 args)))
      `(seq-dispatch ,sub
         (search-compare-vector-list ,main ,sub ,index)
         (search-compare-vector-vector ,main ,sub ,index)
         (return-from search (apply #'sb!sequence:search sequence1 sequence2 args)))))

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
    (sequence1 sequence2 &rest args &key
     from-end test test-not start1 end1 start2 end2 key)
  (declare (type fixnum start1 start2)
           (truly-dynamic-extent args))
  (seq-dispatch sequence2
    (let ((end1 (or end1 length1))
          (end2 (or end2 length2)))
      (declare (type index end1 end2))
      (list-search sequence2 sequence1))
    (let ((end1 (or end1 length1))
          (end2 (or end2 length2)))
      (declare (type index end1 end2))
      (vector-search sequence2 sequence1))
    (apply #'sb!sequence:search sequence1 sequence2 args)))

;;; FIXME: this was originally in array.lisp; it might be better to
;;; put it back there, and make DOSEQUENCE and SEQ-DISPATCH be in
;;; a new early-seq.lisp file.
(defun fill-data-vector (vector dimensions initial-contents)
  (declare (explicit-check))
  (let ((index 0))
    (labels ((frob (axis dims contents)
               (cond ((null dims)
                      (setf (aref vector index) contents)
                      (incf index))
                     (t
                      (unless (typep contents 'sequence)
                        (error "malformed :INITIAL-CONTENTS: ~S is not a ~
                                sequence, but ~W more layer~:P needed."
                               contents
                               (- (length dimensions) axis)))
                      (unless (= (length contents) (car dims))
                        (error "malformed :INITIAL-CONTENTS: Dimension of ~
                                axis ~W is ~W, but ~S is ~W long."
                               axis (car dims) contents (length contents)))
                      (sb!sequence:dosequence (content contents)
                        (frob (1+ axis) (cdr dims) content))))))
      (frob 0 dimensions initial-contents))))
