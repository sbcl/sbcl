;;;; SORT and friends

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(defun sort-vector (vector start end predicate-fun key-fun-or-nil)
  (sort-vector vector start end predicate-fun key-fun-or-nil))

;;; This is MAYBE-INLINE because it's not too hard to have an
;;; application where sorting is a major bottleneck, and inlining it
;;; allows the compiler to make enough optimizations that it might be
;;; worth the (large) cost in space.
(declaim (maybe-inline sort))
(defun sort (sequence predicate &rest args &key key)
  #!+sb-doc
  "Destructively sort SEQUENCE. PREDICATE should return non-NIL if
   ARG1 is to precede ARG2."
  (declare (truly-dynamic-extent args))
  (let ((predicate-fun (%coerce-callable-to-fun predicate)))
    (seq-dispatch sequence
      (stable-sort-list sequence
                        predicate-fun
                        (if key (%coerce-callable-to-fun key) #'identity))
      (let ((key-fun-or-nil (and key (%coerce-callable-to-fun key))))
        (with-array-data ((vector (the vector sequence))
                          (start)
                          (end)
                          :check-fill-pointer t)
          (sort-vector vector start end predicate-fun key-fun-or-nil))
        sequence)
      (apply #'sb!sequence:sort sequence predicate args))))

;;;; stable sorting
(defun stable-sort (sequence predicate &rest args &key key)
  #!+sb-doc
  "Destructively sort SEQUENCE. PREDICATE should return non-NIL if
   ARG1 is to precede ARG2."
  (declare (truly-dynamic-extent args))
  (let ((predicate-fun (%coerce-callable-to-fun predicate)))
    (seq-dispatch sequence
      (stable-sort-list sequence
                        predicate-fun
                        (if key (%coerce-callable-to-fun key) #'identity))
      (if (typep sequence 'simple-vector)
          (stable-sort-simple-vector sequence
                                     predicate-fun
                                     (and key (%coerce-callable-to-fun key)))
          (stable-sort-vector sequence
                              predicate-fun
                              (and key (%coerce-callable-to-fun key))))
      (apply #'sb!sequence:stable-sort sequence predicate args))))

;;; FUNCALL-USING-KEY saves us a function call sometimes.
(eval-when (:compile-toplevel :execute)
  (sb!xc:defmacro funcall2-using-key (pred key one two)
    `(if ,key
         (funcall ,pred (funcall ,key ,one)
                  (funcall ,key  ,two))
         (funcall ,pred ,one ,two)))
) ; EVAL-WHEN

;;;; stable sort of lists

(defun last-cons-of (list)
  (loop (let ((rest (rest list)))
          (if rest
              (setf list rest)
              (return list)))))

;;; Destructively merge LIST-1 with LIST-2 (given that they're already
;;; sorted w.r.t. PRED-FUN on KEY-FUN, giving output sorted the same
;;; way). In the resulting list, elements of LIST-1 are guaranteed to
;;; come before equal elements of LIST-2.
;;;
;;; Return (VALUES HEAD TAILTAIL), where HEAD is the same value you'd
;;; expect from MERGE, and TAILTAIL is the last cons in the list (i.e.
;;; the last cons in the list which NRECONC calls TAIL).
(defun merge-lists* (list-1 list-2 pred-fun key-fun)
  (declare (type list list-1 list-2))
  (declare (type function pred-fun key-fun))
  (cond ((null list-1) (values list-2 (last-cons-of list-2)))
        ((null list-2) (values list-1 (last-cons-of list-1)))
        (t (let* ((reversed-result-so-far nil)
                  (key-1 (funcall key-fun (car list-1)))
                  (key-2 (funcall key-fun (car list-2))))
             (loop
              (macrolet ((frob (list-i key-i other-list)
                           `(progn
                              ;; basically
                              ;;   (PUSH (POP ,LIST-I) REVERSED-RESULT-SO-FAR),
                              ;; except doing some fancy footwork to
                              ;; reuse the cons cell:
                              (psetf (cdr ,list-i) reversed-result-so-far
                                     reversed-result-so-far ,list-i
                                     ,list-i (cdr ,list-i))
                              ;; Now maybe we're done.
                              (if (endp ,list-i)
                                  (return (values (nreconc
                                                   reversed-result-so-far
                                                   ,other-list)
                                                  (last-cons-of
                                                   ,other-list)))
                                  (setf ,key-i
                                        (funcall key-fun (car ,list-i)))))))
                ;; Note that by making KEY-2 the first arg to
                ;; PRED-FUN, we arrange that if PRED-FUN is a function
                ;; in the #'< style, the outcome is stably sorted.
                (if (funcall pred-fun key-2 key-1)
                    (frob list-2 key-2 list-1)
                    (frob list-1 key-1 list-2))))))))

;;; STABLE-SORT-LIST uses a bottom-up merge sort. First a pass is made
;;; over the list grabbing one element at a time and merging it with
;;; the next one to form pairs of sorted elements. Then N is doubled,
;;; and elements are taken in runs of two, merging one run with the
;;; next to form quadruples of sorted elements. This continues until N
;;; is large enough that the inner loop only runs for one iteration;
;;; that is, there are only two runs that can be merged, the first run
;;; starting at the beginning of the list, and the second being the
;;; remaining elements.
(defun stable-sort-list (list pred-fun key-fun)
  (let ((head (cons :header list))  ; head holds on to everything
        (n 1)                       ; bottom-up size of lists to be merged
        unsorted                    ; unsorted is the remaining list to be
                                    ;   broken into n size lists and merged
        list-1                      ; list-1 is one length n list to be merged
        last)                       ; last points to the last visited cell
    (declare (type function pred-fun key-fun)
             (type fixnum n))
    (loop
     ;; Start collecting runs of N at the first element.
     (setf unsorted (cdr head))
     ;; Tack on the first merge of two N-runs to the head holder.
     (setf last head)
     (let ((n-1 (1- n)))
       (declare (fixnum n-1))
       (loop
        (setf list-1 unsorted)
        (let ((temp (nthcdr n-1 list-1))
              list-2)
          (cond (temp
                 ;; There are enough elements for a second run.
                 (setf list-2 (cdr temp))
                 (setf (cdr temp) nil)
                 (setf temp (nthcdr n-1 list-2))
                 (cond (temp
                        (setf unsorted (cdr temp))
                        (setf (cdr temp) nil))
                       ;; The second run goes off the end of the list.
                       (t (setf unsorted nil)))
                 (multiple-value-bind (merged-head merged-last)
                     (merge-lists* list-1 list-2 pred-fun key-fun)
                   (setf (cdr last) merged-head
                         last merged-last))
                 (if (null unsorted) (return)))
                ;; If there is only one run, then tack it on to the end.
                (t (setf (cdr last) list-1)
                   (return)))))
       (setf n (ash n 1)) ; (+ n n)
       ;; If the inner loop only executed once, then there were only
       ;; enough elements for two runs given n, so all the elements
       ;; have been merged into one list. This may waste one outer
       ;; iteration to realize.
       (if (eq list-1 (cdr head))
           (return list-1))))))

;;;; stable sort of vectors

;;; Stable sorting vectors is done with the same algorithm used for
;;; lists, using a temporary vector to merge back and forth between it
;;; and the given vector to sort.

(eval-when (:compile-toplevel :execute)

;;; STABLE-SORT-MERGE-VECTORS* takes a source vector with subsequences,
;;;    start-1 (inclusive) ... end-1 (exclusive) and
;;;    end-1 (inclusive) ... end-2 (exclusive),
;;; and merges them into a target vector starting at index start-1.

(sb!xc:defmacro stable-sort-merge-vectors* (source target start-1 end-1 end-2
                                                     pred key source-ref
                                                     target-ref)
  (let ((i (gensym))
        (j (gensym))
        (target-i (gensym)))
    `(let ((,i ,start-1)
           (,j ,end-1) ; start-2
           (,target-i ,start-1))
       (declare (fixnum ,i ,j ,target-i))
       (loop
        (cond ((= ,i ,end-1)
               (loop (if (= ,j ,end-2) (return))
                     (setf (,target-ref ,target ,target-i)
                           (,source-ref ,source ,j))
                     (incf ,target-i)
                     (incf ,j))
               (return))
              ((= ,j ,end-2)
               (loop (if (= ,i ,end-1) (return))
                     (setf (,target-ref ,target ,target-i)
                           (,source-ref ,source ,i))
                     (incf ,target-i)
                     (incf ,i))
               (return))
              ((funcall2-using-key ,pred ,key
                                   (,source-ref ,source ,j)
                                   (,source-ref ,source ,i))
               (setf (,target-ref ,target ,target-i)
                     (,source-ref ,source ,j))
               (incf ,j))
              (t (setf (,target-ref ,target ,target-i)
                       (,source-ref ,source ,i))
                 (incf ,i)))
        (incf ,target-i)))))

;;; VECTOR-MERGE-SORT is the same algorithm used to stable sort lists,
;;; but it uses a temporary vector. DIRECTION determines whether we
;;; are merging into the temporary (T) or back into the given vector
;;; (NIL).
(sb!xc:defmacro vector-merge-sort (vector pred key vector-ref)
  (with-unique-names
      (vector-len n direction unsorted start-1 end-1 end-2 temp i)
    `(let* ((,vector-len (length (the vector ,vector)))
            (,n 1)            ; bottom-up size of contiguous runs to be merged
            (,direction t)    ; t vector --> temp    nil temp --> vector
            (,temp (make-array ,vector-len))
            (,unsorted 0)   ; unsorted..vector-len are the elements that need
                                        ; to be merged for a given n
            (,start-1 0))   ; one n-len subsequence to be merged with the next
       (declare (fixnum ,vector-len ,n ,unsorted ,start-1)
                (simple-vector ,temp))
       (loop
         ;; for each n, we start taking n-runs from the start of the vector
         (setf ,unsorted 0)
         (loop
           (setf ,start-1 ,unsorted)
           (let ((,end-1 (+ ,start-1 ,n)))
             (declare (fixnum ,end-1))
             (cond ((< ,end-1 ,vector-len)
                    ;; there are enough elements for a second run
                    (let ((,end-2 (+ ,end-1 ,n)))
                      (declare (fixnum ,end-2))
                      (if (> ,end-2 ,vector-len) (setf ,end-2 ,vector-len))
                      (setf ,unsorted ,end-2)
                      (if ,direction
                          (stable-sort-merge-vectors*
                           ,vector ,temp
                           ,start-1 ,end-1 ,end-2 ,pred ,key ,vector-ref svref)
                          (stable-sort-merge-vectors*
                           ,temp ,vector
                           ,start-1 ,end-1 ,end-2 ,pred ,key svref ,vector-ref))
                      (if (= ,unsorted ,vector-len) (return))))
                   ;; if there is only one run, copy those elements to the end
                   (t (if ,direction
                          (do ((,i ,start-1 (1+ ,i)))
                              ((= ,i ,vector-len))
                            (declare (fixnum ,i))
                            (setf (svref ,temp ,i) (,vector-ref ,vector ,i)))
                          (do ((,i ,start-1 (1+ ,i)))
                              ((= ,i ,vector-len))
                            (declare (fixnum ,i))
                            (setf (,vector-ref ,vector ,i) (svref ,temp ,i))))
                      (return)))))
         ;; If the inner loop only executed once, then there were only enough
         ;; elements for two subsequences given n, so all the elements have
         ;; been merged into one list. Start-1 will have remained 0 upon exit.
         (when (zerop ,start-1)
           (when ,direction
             ;; if we just merged into the temporary, copy it all back
             ;; to the given vector.
             (dotimes (,i ,vector-len)
               (setf (,vector-ref ,vector ,i) (svref ,temp ,i))))
           ;; Kill the new vector to prevent garbage from being retained.
           (%shrink-vector ,temp 0)
           (return ,vector))
         (setf ,n (ash ,n 1))           ; (* 2 n)
         (setf ,direction (not ,direction))))))

) ; EVAL-when

(defun stable-sort-simple-vector (vector pred key)
  (declare (type simple-vector vector)
           (type function pred)
           (type (or null function) key))
  (vector-merge-sort vector pred key svref))

(defun stable-sort-vector (vector pred key)
  (declare (type function pred)
           (type (or null function) key))
  (vector-merge-sort vector pred key aref))

;;;; merging

(eval-when (:compile-toplevel :execute)

;;; MERGE-VECTORS returns a new vector which contains an interleaving
;;; of the elements of VECTOR-1 and VECTOR-2. Elements from VECTOR-2
;;; are chosen only if they are strictly less than elements of
;;; VECTOR-1, (PRED ELT-2 ELT-1), as specified in the manual.
(sb!xc:defmacro merge-vectors (vector-1 length-1 vector-2 length-2
                               result-vector pred key access)
  (let ((result-i (gensym))
        (i (gensym))
        (j (gensym)))
    `(let* ((,result-i 0)
            (,i 0)
            (,j 0))
       (declare (fixnum ,result-i ,i ,j))
       (loop
        (cond ((= ,i ,length-1)
               (loop (if (= ,j ,length-2) (return))
                     (setf (,access ,result-vector ,result-i)
                           (,access ,vector-2 ,j))
                     (incf ,result-i)
                     (incf ,j))
               (return ,result-vector))
              ((= ,j ,length-2)
               (loop (if (= ,i ,length-1) (return))
                     (setf (,access ,result-vector ,result-i)
                           (,access ,vector-1 ,i))
                     (incf ,result-i)
                     (incf ,i))
               (return ,result-vector))
              ((funcall2-using-key ,pred ,key
                                   (,access ,vector-2 ,j) (,access ,vector-1 ,i))
               (setf (,access ,result-vector ,result-i)
                     (,access ,vector-2 ,j))
               (incf ,j))
              (t (setf (,access ,result-vector ,result-i)
                       (,access ,vector-1 ,i))
                 (incf ,i)))
        (incf ,result-i)))))

) ; EVAL-WHEN

(defun merge (result-type sequence1 sequence2 predicate &key key)
  #!+sb-doc
  "Merge the sequences SEQUENCE1 and SEQUENCE2 destructively into a
   sequence of type RESULT-TYPE using PREDICATE to order the elements."
  ;; FIXME: This implementation is remarkably inefficient in various
  ;; ways. In decreasing order of estimated user astonishment, I note:
  ;; full calls to SPECIFIER-TYPE at runtime; copying input vectors
  ;; to lists before doing MERGE-LISTS*; and walking input lists
  ;; (because of the call to MERGE-LISTS*, which walks the list to
  ;; find the last element for its second return value) even in cases
  ;; like (MERGE 'LIST (LIST 1) (LIST 2 3 4 5 ... 1000)) where one list
  ;; can be largely ignored. -- WHN 2003-01-05
  (let ((type (specifier-type result-type)))
    (cond
      ((csubtypep type (specifier-type 'list))
       ;; the VECTOR clause, below, goes through MAKE-SEQUENCE, so
       ;; benefits from the error checking there. Short of
       ;; reimplementing everything, we can't do the same for the LIST
       ;; case, so do relevant length checking here:
       (let ((s1 (coerce sequence1 'list))
             (s2 (coerce sequence2 'list))
             (pred-fun (%coerce-callable-to-fun predicate))
             (key-fun (if key
                          (%coerce-callable-to-fun key)
                          #'identity)))
         (when (type= type (specifier-type 'list))
           (return-from merge (values (merge-lists* s1 s2 pred-fun key-fun))))
         (when (eq type *empty-type*)
           (bad-sequence-type-error nil))
         (when (type= type (specifier-type 'null))
           (if (and (null s1) (null s2))
               (return-from merge 'nil)
               ;; FIXME: This will break on circular lists (as,
               ;; indeed, will the whole MERGE function).
               (sequence-type-length-mismatch-error type
                                                    (+ (length s1)
                                                       (length s2)))))
         (if (cons-type-p type)
             (multiple-value-bind (min exactp)
                 (sb!kernel::cons-type-length-info type)
               (let ((length (+ (length s1) (length s2))))
                 (if exactp
                     (unless (= length min)
                       (sequence-type-length-mismatch-error type length))
                     (unless (>= length min)
                       (sequence-type-length-mismatch-error type length)))
                 (values (merge-lists* s1 s2 pred-fun key-fun))))
             (sequence-type-too-hairy result-type))))
      ((csubtypep type (specifier-type 'vector))
       (let* ((vector-1 (coerce sequence1 'vector))
              (vector-2 (coerce sequence2 'vector))
              (length-1 (length vector-1))
              (length-2 (length vector-2))
              (result (make-sequence result-type (+ length-1 length-2))))
         (declare (vector vector-1 vector-2)
                  (fixnum length-1 length-2))
         (if (and (simple-vector-p result)
                  (simple-vector-p vector-1)
                  (simple-vector-p vector-2))
             (merge-vectors vector-1 length-1 vector-2 length-2
                            result predicate key svref)
             (merge-vectors vector-1 length-1 vector-2 length-2
                            result predicate key aref))))
      ((and (csubtypep type (specifier-type 'sequence))
            (find-class result-type nil))
       (let* ((vector-1 (coerce sequence1 'vector))
              (vector-2 (coerce sequence2 'vector))
              (length-1 (length vector-1))
              (length-2 (length vector-2))
              (temp (make-array (+ length-1 length-2)))
              (result (make-sequence result-type (+ length-1 length-2))))
         (declare (vector vector-1 vector-2) (fixnum length-1 length-2))
         (merge-vectors vector-1 length-1 vector-2 length-2
                        temp predicate key aref)
         (replace result temp)
         result))
      (t (bad-sequence-type-error result-type)))))
