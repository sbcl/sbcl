;;;; SORT and friends

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

(defun sort-vector (vector start end predicate-fun key-fun-or-nil)
  (declare (dynamic-extent predicate-fun key-fun-or-nil))
  (sort-vector vector start end predicate-fun key-fun-or-nil))

;;; This is MAYBE-INLINE because it's not too hard to have an
;;; application where sorting is a major bottleneck, and inlining it
;;; allows the compiler to make enough optimizations that it might be
;;; worth the (large) cost in space.
(declaim (maybe-inline sort stable-sort))
(defun sort (sequence predicate &rest args &key key)
  "Destructively sort SEQUENCE. PREDICATE should return non-NIL if
   ARG1 is to precede ARG2."
  (declare (truly-dynamic-extent args))
  (declare  (dynamic-extent predicate key))
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
      (apply #'sb-sequence:sort sequence predicate args))))

;;;; stable sorting
(defun stable-sort (sequence predicate &rest args &key key)
  "Destructively sort SEQUENCE. PREDICATE should return non-NIL if
   ARG1 is to precede ARG2."
  (declare (truly-dynamic-extent args))
  (declare (dynamic-extent predicate key))
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
      (apply #'sb-sequence:stable-sort sequence predicate args))))

;;; FUNCALL-USING-KEY saves us a function call sometimes.
(eval-when (:compile-toplevel :execute)
  (sb-xc:defmacro funcall2-using-key (pred key one two)
    `(if ,key
         (funcall ,pred (funcall ,key ,one)
                  (funcall ,key  ,two))
         (funcall ,pred ,one ,two)))
) ; EVAL-WHEN

;;;; stable sort of lists
(declaim (maybe-inline merge-lists* stable-sort-list))

;;; Destructively merge LIST-1 with LIST-2 (given that they're already
;;; sorted w.r.t. PRED-FUN on KEY-FUN, giving output sorted the same
;;; way). In the resulting list, elements of LIST-1 are guaranteed to
;;; come before equal elements of LIST-2.
;;;
;;; Enqueues the values in the right order in HEAD's cdr, and returns
;;; the merged list.
(defun merge-lists* (head list1 list2 test key &aux (tail head))
  (declare (type cons head list1 list2)
           (type function test key)
           (optimize speed))
  (declare (dynamic-extent test key))
  (let ((key1 (funcall key (car list1)))
        (key2 (funcall key (car list2))))
    (macrolet ((merge-one (l1 k1 l2)
                 `(progn
                    (setf (cdr tail) ,l1
                          tail       ,l1)
                    (let ((rest (cdr ,l1)))
                      (cond (rest
                             (setf ,l1 rest
                                   ,k1 (funcall key (first rest))))
                            (t
                             (setf (cdr ,l1) ,l2)
                             (return (cdr head))))))))
      (loop
       (if (funcall test key2           ; this way, equivalent
                         key1)          ; values are first popped
           (merge-one list2 key2 list1) ; from list1
           (merge-one list1 key1 list2))))))

;;; Convenience wrapper for CL:MERGE
(declaim (inline merge-lists))
(defun merge-lists (list1 list2 test key)
  (cond ((null list1)
         list2)
        ((null list2)
         list1)
        (t
         (let ((head (cons nil nil)))
           (declare (dynamic-extent head))
           (merge-lists* head list1 list2 test key)))))

;;; Small specialised stable sorts
(declaim (inline stable-sort-list-2 stable-sort-list-3))
(defun stable-sort-list-2 (list test key)
  (declare (type cons list)
           (type function test key))
  (let ((second (cdr list)))
    (declare (type cons second))
    (when (funcall test (funcall key (car second))
                        (funcall key (car list)))
      (rotatef (car list) (car second)))
    (values list second (shiftf (cdr second) nil))))

(defun stable-sort-list-3 (list test key)
  (declare (type cons list)
           (type function test key))
  (let* ((second (cdr list))
         (third  (cdr second))
         (x (car list))
         (y (car second))
         (z (car third)))
    (declare (type cons second third))
    (when (funcall test (funcall key y)
                        (funcall key x))
      (rotatef x y))
    (let ((key-z (funcall key z)))
      (when (funcall test key-z
                          (funcall key y))
        (if (funcall test key-z
                          (funcall key x))
            (rotatef x z y)
            (rotatef z y))))
    (setf (car list)   x
          (car second) y
          (car third)  z)
    (values list third (shiftf (cdr third) nil))))

;;; STABLE-SORT-LIST implements a top-down merge sort. See the closest
;;; intro to algorithms book.  Benchmarks have shown significantly
;;; improved performance over the previous (hairier) bottom-up
;;; implementation, particularly on non-power-of-two sizes: bottom-up
;;; recursed on power-of-two-sized subsequences, which can result in
;;; very unbalanced recursion trees.

;;; The minimum length at which list merge sort will try and detect
;;; it can merge disjoint ranges (e.g. sorted inputs) in constant time.
(defconstant +stable-sort-fast-merge-limit+ 8)

(defun stable-sort-list (list test key &aux (head (cons :head list)))
  (declare (type list list)
           (type function test key)
           (dynamic-extent head))
  (declare (explicit-check))
  (declare (dynamic-extent test key))
  (labels ((merge* (size list1 tail1 list2 tail2 rest)
             (declare (optimize speed)
                      (type (and fixnum unsigned-byte) size)
                      (type cons list1 tail1 list2 tail2))
             (when (>= size +stable-sort-fast-merge-limit+)
               (cond ((not (funcall test (funcall key (car list2))   ; stability
                                         (funcall key (car tail1)))) ; trickery
                      (setf (cdr tail1) list2)
                      (return-from merge* (values list1 tail2 rest)))
                     ((funcall test (funcall key (car tail2))
                                    (funcall key (car list1)))
                      (setf (cdr tail2) list1)
                      (return-from merge* (values list2 tail1 rest)))))
               (values (merge-lists* head list1 list2 test key)
                       (if (null (cdr tail1))
                           tail1
                           tail2)
                       rest))
           (recur (list size)
             (declare (optimize speed)
                      (type cons list)
                      (type (and fixnum unsigned-byte) size))
             (cond ((> size 3)
                    (let ((half (ash size -1)))
                      (multiple-value-bind (list1 tail1 rest)
                          (recur list half)
                        (multiple-value-bind (list2 tail2 rest)
                            (recur rest (- size half))
                          (merge* size list1 tail1 list2 tail2 rest)))))
                   ((= size 3)
                    (stable-sort-list-3 list test key))
                   ((= size 2)
                    (stable-sort-list-2 list test key))
                   (t ; (= size 1)
                    (values list list (shiftf (cdr list) nil))))))
    (when list
      (values (recur list (length list))))))

;;;; stable sort of vectors

;;; Stable sorting vectors is done with the same algorithm used for
;;; lists, using a temporary vector to merge back and forth between it
;;; and the given vector to sort.

(eval-when (:compile-toplevel :execute)

;;; STABLE-SORT-MERGE-VECTORS* takes a source vector with subsequences,
;;;    start-1 (inclusive) ... end-1 (exclusive) and
;;;    end-1 (inclusive) ... end-2 (exclusive),
;;; and merges them into a target vector starting at index start-1.

(sb-xc:defmacro stable-sort-merge-vectors* (source target start-1 end-1 end-2
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
(sb-xc:defmacro vector-merge-sort (vector pred key vector-ref)
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
  (declare (explicit-check))
  (declare (dynamic-extent pred key))
  (if (<= (length vector) 1) ; avoid consing
      vector
      (vector-merge-sort vector pred key svref)))

(defun stable-sort-vector (vector pred key)
  (declare (type function pred)
           (type (or null function) key))
  (declare (explicit-check))
  (declare (dynamic-extent pred key))
  (if (<= (length vector) 1) ; avoid consing
      vector
      (vector-merge-sort vector pred key aref)))

;;;; merging

(eval-when (:compile-toplevel :execute)

;;; MERGE-VECTORS returns a new vector which contains an interleaving
;;; of the elements of VECTOR-1 and VECTOR-2. Elements from VECTOR-2
;;; are chosen only if they are strictly less than elements of
;;; VECTOR-1, (PRED ELT-2 ELT-1), as specified in the manual.
(sb-xc:defmacro merge-vectors (vector-1 length-1 vector-2 length-2
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
  "Merge the sequences SEQUENCE1 and SEQUENCE2 destructively into a
   sequence of type RESULT-TYPE using PREDICATE to order the elements."
  ;; FIXME: This implementation is remarkably inefficient in various
  ;; ways. In decreasing order of estimated user astonishment, I note:
  ;; full calls to SPECIFIER-TYPE at runtime; copying input vectors
  ;; to lists before doing MERGE-LISTS -- WHN 2003-01-05
  (declare (explicit-check))
  (declare (dynamic-extent predicate key))
  (let ((type (specifier-type result-type))
        (pred-fun (%coerce-callable-to-fun predicate))
        ;; Avoid coercing NIL to a function since 2 out of 3 branches of the
        ;; COND below optimize for NIL as the key function. Additionally
        ;; recognize the reverse - when you said #'IDENTITY which can be
        ;; turned into NIL. Also, in the generic case, don't defer a
        ;; type error to the method (even though it also coerces to fun).
        (key-fun (when (and key (neq key #'identity))
                   (%coerce-callable-to-fun key))))
    (cond
      ((csubtypep type (specifier-type 'list))
       ;; the VECTOR clause, below, goes through MAKE-SEQUENCE, so
       ;; benefits from the error checking there. Short of
       ;; reimplementing everything, we can't do the same for the LIST
       ;; case, so do relevant length checking here:
       (let ((s1 (coerce sequence1 'list))
             (s2 (coerce sequence2 'list))
             ;; MERGE-LISTS* does not contain special code for KEY = NIL.
             (key-fun (or key-fun #'identity)))
         (when (type= type (specifier-type 'list))
           (return-from merge (merge-lists s1 s2 pred-fun key-fun)))
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
                 (sb-kernel::cons-type-length-info type)
               (let ((length (+ (length s1) (length s2))))
                 (if exactp
                     (unless (= length min)
                       (sequence-type-length-mismatch-error type length))
                     (unless (>= length min)
                       (sequence-type-length-mismatch-error type length)))
                 (merge-lists s1 s2 pred-fun key-fun)))
             (sequence-type-too-hairy result-type))))
      ((csubtypep type (specifier-type 'vector))
       (let* ((vector-1 (coerce sequence1 'vector))
              (vector-2 (coerce sequence2 'vector))
              (length-1 (length vector-1))
              (length-2 (length vector-2))
              (result (make-sequence result-type (+ length-1 length-2))))
         (if (and (simple-vector-p result)
                  (simple-vector-p vector-1)
                  (simple-vector-p vector-2))
             (merge-vectors vector-1 length-1 vector-2 length-2
                            result pred-fun key-fun svref)
             ;; Some things that could improve the fancy vector case:
             ;; - recognize when the only fancy aspect of both inputs
             ;;   is that they have fill pointers.
             ;; - use the specialized reffer for inputs + output
             (merge-vectors vector-1 length-1 vector-2 length-2
                            result pred-fun key-fun aref))))
      ((when-extended-sequence-type
           (result-type type :expandedp nil :prototype prototype)
         ;; This function has the EXPLICIT-CHECK declaration, so we
         ;; manually assert that it returns a SEQUENCE.
         (the extended-sequence
              ;; GF dispatch deals with the erroneous situation
              ;; wherein either of SEQUENCE1 or SEQUENCE2 is not a
              ;; sequence.  Note that the one builtin method optimizes
              ;; for NIL as the key fun, and we correctly preserve a
              ;; NIL here.
              (sb-sequence:merge
               prototype sequence1 sequence2 pred-fun :key key-fun))))
      (t (bad-sequence-type-error result-type)))))
