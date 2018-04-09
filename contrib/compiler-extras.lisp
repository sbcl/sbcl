;;;; The files
;;;;   compiler-extras.lisp
;;;;   code-extras.lisp
;;;; hold things that I (WHN) am working on which are sufficiently
;;;; closely tied to the system that they want to be under the same
;;;; revision control, but which aren't yet ready for prime time.
;;;;
;;;; Unless you like living dangerously, you don't want to be running
;;;; these. But there might be some value to looking at these files to
;;;; see whether I'm working on optimizing something whose performance
;;;; you care about, so that you can patch it, or write test cases for
;;;; it, or pester me to release it, or whatever.
;;;;
;;;; Throughout 0.6.x, these were mostly performance fixes. Fixes for
;;;; logical bugs tend to go straight into the system, but fixes for
;;;; performance problems can easily introduce logical bugs, and no
;;;; one's going to thank me for prematurely replacing old slow
;;;; correct code with new fast code that I haven't yet discovered to
;;;; be wrong.

(in-package "SB-C")

(declaim (optimize (speed 1) (space 2)))

;;; TO DO for DEFTRANSFORM FILL:
;;;   ?? This DEFTRANSFORM, and the old DEFTRANSFORMs, should only
;;;      apply when SPEED > SPACE.
;;;   ?? Add test cases.

#+nil ; not tested yet..
(deftransform replace ((seq1 seq2 &key (start1 0) end1 (start2 0) end2)
                       (vector vector &key
                               (:start1 index) (:end1 (or index null))
                               (:start2 index) (:end2 (or index null)))
                       *
                       ;; This is potentially an awfully big transform
                       ;; (if things like (EQ SEQ1 SEQ2) aren't known
                       ;; at runtime). We need to make it available
                       ;; inline, since otherwise there's no way to do
                       ;; it efficiently on all array types, but it
                       ;; probably doesn't belong inline all the time.
                       :policy (> speed (1+ space)))
  "open code"
  (let ((et1 (upgraded-element-type-specifier-or-give-up seq1))
        (et2 (upgraded-element-type-specifier-or-give-up seq2)))
    `(let* ((n-copied (min (- end1 start1) (- end2 start2)))
            (effective-end1 (+ start1 n-copied)))
       (if (eq seq1 seq2)
           (with-array-data ((seq seq1)
                             (start (min start1 start2))
                             (end (max end1 end2)))
             (declare (type (simple-array ,et1 1) seq))
             (if (<= start1 start2)
                 (let ((index2 start2))
                   (declare (type index index2))
                   (loop for index1 of-type index
                         from start1 below effective-end1 do
                         (setf (aref seq index1)
                               (aref seq index2))
                         (incf index2)))
                 (let ((index2 (1- end2)))
                   (declare (type (integer -2 #.most-positive-fixnum) index2))
                   (loop for index1 of-type index-or-minus-1
                         from (1- effective-end1) downto start1 do
                         (setf (aref seq index1)
                               (aref seq index2))
                         (decf index2)))))
           (with-array-data ((seq1 seq1) (start1 start1) (end1 end1))
             (declare (type (simple-array ,et1 1) seq1))
             (with-array-data ((seq2 seq2) (start2 start2) (end2 end2))
               (declare (type (simple-array ,et2 1) seq2))
               (let ((index2 start2))
                 (declare (type index index2))
                 (loop for index1 of-type index
                       from start1 below effective-end1 do
                       (setf (aref seq index1)
                             (aref seq index2))
                       (incf index2))))))
       seq1)))

;;; Boyer-Moore search for strings.
;;;
;;; TODO:
;;; * START/END keywords
;;; * a literal :TEST #'CHAR= or :TEST #'EQL is OK (also #'EQ)
;;; * fewer hardcoded constants
;;; * :FROM-END
;;;
;;; * investigate whether we can make this work with a hashtable and a
;;; default for "not in pattern"
(deftransform search ((pattern text)
                      (simple-base-string simple-base-string))
  (unless (constant-lvar-p pattern)
    (give-up-ir1-transform))
  (let* ((pattern (lvar-value pattern))
         (bad-character (make-array 256 :element-type 'fixnum :initial-element (length pattern)))
         (temp (make-array (length pattern) :element-type 'fixnum))
         (good-suffix (make-array (length pattern) :element-type 'fixnum :initial-element (1- (length pattern)))))

    (dotimes (i (1- (length pattern)))
      (setf (aref bad-character (char-code (aref pattern i)))
            (- (length pattern) 1 i)))

    (setf (aref temp (1- (length pattern))) (length pattern))
    (loop with g = (1- (length pattern))
          with f = (1- (length pattern)) ; XXXXXX?
          for i downfrom (- (length pattern) 2) above 0
          if (and (> i g)
                  (< (aref temp (- (+ i (length pattern)) 1 f)) (- i g)))
          do (setf (aref temp i) (aref temp (- (+ i (length pattern)) 1 f)))
          else
          do (progn
               (when (< i g)
                 (setf g i))
               (setf f i)
               (do ()
                   ((not
                     (and (>= g 0)
                         (char= (aref pattern g)
                                (aref pattern (- (+ g (length pattern)) 1 f))))))
                 (decf g))
               (setf (aref temp i) (- f g))))

    (loop with j = 0
          for i downfrom (1- (length pattern)) to -1
          if (or (= i -1) (= (aref temp i) (1+ i)))
          do (do ()
                 ((>= j (- (length pattern) 1 i)))
               (when (= (aref good-suffix j) (length pattern))
                 (setf (aref good-suffix j) (- (length pattern) 1 i)))
               (incf j)))

    (loop for i from 0 below (1- (length pattern))
          do (setf (aref good-suffix (- (length pattern) 1 (aref temp i)))
                   (- (length pattern) 1 i)))

    `(let ((good-suffix ,good-suffix)
           (bad-character ,bad-character))
      (declare (optimize speed (safety 0)))
      (block search
        (do ((j 0))
            ((> j (- (length text) (length pattern))))
          (declare (fixnum j))
          (do ((i (1- (length pattern)) (1- i)))
              ((< i 0) (return-from search j))
            (declare (fixnum i))
            (when (char/= (aref pattern i) (aref text (+ i j)))
              (incf j (max (aref good-suffix i)
                           (+ (- (aref bad-character (char-code (aref text (+ i j))))
                                 (length pattern))
                              (1+ i))))
              (return))))))))


;;; Scan all functions that take funargs, looking to see whether the funarg
;;; is declared as downward (dynamic-extent) or assumed possibly upward.
;;; Print the names of functions for which the number of funargs differs
;;; from the number of DX args. (This is a way to scan for missing decls)
;;; In an ideal world, the compiler would infer that a funarg can be allocated
;;; with dynamic-extent based on it only appearing as the first argument
;;; to FUNCALL/APPLY, and not being closed over.
;;; But we don't need ideal, we just need good enough.
(flet ((fun-funargs-count (type)
         (flet ((funlike-type-p (x)
                  (or (csubtypep x (specifier-type 'function))
                      (type= x (specifier-type '(or function null)))
                      (type= x (specifier-type '(or function symbol))))))
           (+ (count-if #'funlike-type-p (fun-type-required type))
              (count-if #'funlike-type-p (fun-type-optional type))
              (count-if (lambda (x) (funlike-type-p (key-info-type x)))
                        (fun-type-keywords type))))))
  (dotimes (pass 2)
    (format t "~[CL symbols~;other symbols~]:~%" pass)
    (let (result)
      (do-all-symbols (s)
        (when (if (= pass 0)
                  (eq (symbol-package s) (find-package "CL"))
                  (neq (symbol-package s) (find-package "CL")))
          (let ((type (info :function :type s)))
            (when (and s
                       (not (eq type (find-classoid 'function)))
                       (not (typep type 'defstruct-description))
                       (not (eq type :generic-function)))
              (let ((n-funargs (fun-funargs-count type)))
                (when (plusp n-funargs)
                  (let ((dxable-args
                         (let ((info (info :function :inlining-data s)))
                           (when (typep info 'dxable-args)
                             (dxable-args-list info)))))
                    (unless (= n-funargs (length dxable-args))
                      (push (list (length dxable-args) n-funargs s)
                            result)))))))))
      (dolist (x (sort result #'string< :key 'third))
        (format t "~{ ~d ~d ~a~}~%" x)))))
