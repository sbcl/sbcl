;;;; tests related to sequences

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

;;; As reported by Paul Dietz from his ansi-test suite for gcl, REMOVE
;;; malfunctioned when given :START, :END and :FROM-END arguments.
;;; Make sure it doesn't happen again.
(with-test (:name (remove :start :end :from-end))
  (let* ((orig '(1 2 3 2 6 1 2 4 1 3 2 7))
         (x (copy-seq orig))
         (y (remove 3 x :from-end t :start 1 :end 5))
         (z (remove 2 x :from-end t :start 1 :end 5)))
    (assert (equalp orig x))
    (assert (equalp y '(1 2 2 6 1 2 4 1 3 2 7)))
    (assert (equalp z '(1 3 6 1 2 4 1 3 2 7)))))

;;; Similarly, NSUBSTITUTE and friends were getting things wrong with
;;; :START, :END and :FROM-END:
(with-test (:name (nsubstitute :start :end :from-end))
  (assert
   (loop for i from 0 to 9 always
            (loop for j from i to 10 always
                     (loop for c from 0 to (- j i) always
                              (let* ((orig '(a a a a a a a a a a))
                                     (x (copy-seq orig))
                                     (y (nsubstitute 'x 'a x :start i :end j :count c)))
                                (equal y (nconc (make-list i :initial-element 'a)
                                                (make-list c :initial-element 'x)
                                                (make-list (- 10 (+ i c))
                                                           :initial-element 'a))))))))

  (assert
   (loop for i from 0 to 9 always
            (loop for j from i to 10 always
                     (loop for c from 0 to (- j i) always
                              (let* ((orig '(a a a a a a a a a a))
                                     (x (copy-seq orig))
                                     (y (nsubstitute-if 'x (lambda (x) (eq x 'a)) x
                                                        :start i :end j
                                                        :count c :from-end t)))
                                (equal y (nconc (make-list (- j c) :initial-element 'a)
                                                (make-list c :initial-element 'x)
                                                (make-list (- 10 j)
                                                           :initial-element 'a))))))))
  (assert
   (loop for i from 0 to 9 always
            (loop for j from i to 10 always
                     (loop for c from 0 to (- j i) always
                              (let* ((orig '(a a a a a a a a a a))
                                     (x (copy-seq orig))
                                     (y (nsubstitute-if-not 'x (lambda (x)
                                                                 (not (eq x 'a))) x
                                                                 :start i :end j
                                                                 :count c :from-end t)))
                                (equal y (nconc (make-list (- j c) :initial-element 'a)
                                                (make-list c :initial-element 'x)
                                                (make-list (- 10 j)
                                                           :initial-element 'a)))))))))

;;; And equally similarly, REMOVE-DUPLICATES misbehaved when given
;;; :START arguments:

(with-test (:name (remove-duplicates delete-duplicates :start :end))
  (let ((orig (list 0 1 2 0 1 2 0 1 2 0 1 2)))
    (assert (equalp (remove-duplicates orig :start 3 :end 9) '(0 1 2 0 1 2 0 1 2)))
    (assert (equalp (delete-duplicates orig :start 3 :end 9) '(0 1 2 0 1 2 0 1 2)))))

;;; tests of COUNT
(with-test (:name (count))
  (assert (= 1 (count 1 '(1 2 3))))
  (assert (= 2 (count 'z #(z 1 2 3 z))))
  (assert (= 0 (count 'y '(z 1 2 3 z)))))

;;; tests of COUNT-IF and COUNT-IF-NOT
(with-test (:name (count-if count-if-not))
  (macrolet (;; the guts of CCI, abstracted over whether we're testing
             ;; COUNT-IF or COUNT-IF-NOT
             (%cci (expected count-if test sequence-as-list &rest keys)
               `(let* ((list ',sequence-as-list)
                       (simple-vector (coerce list 'simple-vector))
                       (length (length list))
                       (vector (make-array (* 2 length) :fill-pointer length)))
                  (replace vector list :end1 length)
                  (dolist (seq (list list simple-vector vector))
                    (assert (= ,expected (,count-if ,test seq ,@keys))))))
             ;; "Check COUNT-IF"
             (cci (expected test sequence-as-list &rest keys)
               `(progn
                  (%cci ,expected
                        count-if
                        ,test
                        ,sequence-as-list
                        ,@keys)
                  (%cci ,expected
                        count-if-not
                        (complement ,test)
                        ,sequence-as-list
                        ,@keys))))
    (cci 1 #'consp (1 (12) 1))
    (cci 3 #'consp (1 (2) 3 (4) (5) 6))
    (cci 3 #'consp (1 (2) 3 (4) (5) 6) :from-end t)
    (cci 2 #'consp (1 (2) 3 (4) (5) 6) :start 2)
    (cci 0 #'consp (1 (2) 3 (4) (5) 6) :start 2 :end 3)
    (cci 1 #'consp (1 (2) 3 (4) (5) 6) :start 1 :end 3)
    (cci 1 #'consp (1 (2) 3 (4) (5) 6) :start 1 :end 2)
    (cci 0 #'consp (1 (2) 3 (4) (5) 6) :start 2 :end 2)
    (cci 2 #'zerop (0 10 0 11 12))
    (cci 1 #'zerop (0 10 0 11 12) :start 1)
    (cci 2 #'minusp (0 10 0 11 12) :key #'1-)
    (cci 1 #'minusp (0 10 0 11 12) :key #'1- :end 2))

  (multiple-value-bind (fun failure-p warnings style-warnings)
      (checked-compile `(lambda ()
                          (count-if #'zerop '(0 a 0 b c) :start 1))
                       :allow-style-warnings t)
    (declare (ignore failure-p warnings))
    (assert (= (length style-warnings) 1))
    (let ((condition (grab-condition (funcall fun))))
      (assert (eql (type-error-datum condition) 'a))))
  (multiple-value-bind (fun failure-p warnings style-warnings)
      (checked-compile `(lambda ()
                          (count-if #'zerop #(0 a 0 b c) :start 1 :from-end 11))
                       :allow-style-warnings t)
    (declare (ignore failure-p warnings))
    (assert (= (length style-warnings) 1))
    (let ((condition (grab-condition (funcall fun))))
      (assert (eql (type-error-datum condition) 'c)))))

;;; :COUNT may be negative and BIGNUM
(with-test (:name (remove :count :negative bignum))
  (assert (equal (remove 1 '(1 2 3 1) :count 1) '(2 3 1)))
  (assert (equal (remove 1 '(1 2 3 1) :count (* 2 most-positive-fixnum)) '(2 3)))
  (assert (equal (remove 1 '(1 2 3 1) :count (* -2 most-positive-fixnum)) '(1 2 3 1))))

;;; bug reported by Wolfgang Jenkner on sbcl-devel 2003-01-04:
;;; embedded calls of SORT do not work
(with-test (:name (sort :nested-calls))
  (assert (equal (sort (list 0 0 0)
                       (lambda (x y)
                         (if (= x y) ; uses X, Y and SORT return value
                             nil
                             (sort (list 0 0 0) #'<))))
                 '(0 0 0)))

  (assert (equal (sort (list 0 0 0 0 0)
                       (lambda (x y)
                         (declare (ignore x y))
                         (block compare
                           (sort (make-list 11 :initial-element 1)
                                 (let ((counter 7))
                                   (lambda (x y)
                                     (declare (ignore x y))
                                     (when (= (decf counter) 0)
                                       (return-from compare nil))
                                     t))))))
                 '(0 0 0 0 0))))

;;; miscellaneous sanity checks on stuff which could've been broken by
;;; changes in MERGE-LIST* in sbcl-0.7.11.*
(with-test (:name (merge stable-sort :sanity-checks))
  (assert (equal (merge 'list () () '<) ()))
  (assert (equal (merge 'list () (list 1) #'< :key 'identity) '(1)))
  (assert (equal (merge 'list (list 2) () '>) '(2)))
  (assert (equal (merge 'list (list 1 2 4) (list 2 3 7) '<) '(1 2 2 3 4 7)))
  (assert (equal (merge 'list (list 1 2 4) (list -2 3 7) #'<) '(-2 1 2 3 4 7)))
  (assert (equal (merge 'list (list 1 2 4) (vector -2 3 7) '< :key 'abs)
                 '(1 2 -2 3 4 7)))
  (assert (equal (merge 'list (list 1 -2 4) (list -2 3 7) '< :key #'abs)
                 '(1 -2 -2 3 4 7)))
  (assert (equal (stable-sort (list 1 10 2 12 13 3) '<) '(1 2 3 10 12 13)))
  (assert (equal (stable-sort (list 1 10 2 12 13 3) #'< :key '-)
                 '(13 12 10 3 2 1)))
  (assert (equal (stable-sort (list 1 10 2 12 13 3) '> :key #'-)
                 '(1 2 3 10 12 13)))
  (assert (equal (stable-sort (list 1 2 3 -3 -2 -1) '< :key 'abs)
                 '(1 -1 2 -2 3 -3))))

;;; CSR broke FILL by not returning the sequence argument in a transform.
(with-test (:name fill)
  (let* ((s1 (copy-seq "abcde"))
         (s2 (fill s1 #\z)))
    (assert s2)
    (assert (string= s2 "zzzzz"))))

;;; POSITION on displaced arrays with non-zero offset has been broken
;;; for quite a while...
(with-test (:name (position :displaced-array))
  (let* ((x #(1 2 3))
         (y (make-array 2 :displaced-to x :displaced-index-offset 1)))
    (assert (= (position 2 y) 0))))

;;; (SIMPLE-STRING) is a legal type specifier for creation functions
(with-test (:name (make-sequence concatenate map merge coerce simple-string))
  (let ((a (make-sequence '(simple-string) 5))
        (b (concatenate '(simple-string) "a" "bdec"))
        (c (map '(simple-string) 'identity "abcde"))
        (d (merge '(simple-string) (copy-seq "acd") (copy-seq "be") 'char>))
        (e (coerce '(#\a #\b #\c #\e #\d) '(simple-string))))
    (assert (= (length a) 5))
    (assert (string= b "abdec"))
    (assert (string= c "abcde"))
    (assert (string= d "beacd"))
    (assert (string= e "abced"))))

;;; COPY-SEQ "should be prepared to signal an error if sequence is not
;;; a proper sequence".
(with-test (:name (copy-seq type-error))
  (locally (declare (optimize safety))
    (multiple-value-bind (seq err) (ignore-errors (copy-seq (opaque-identity '(1 2 3 . 4))))
      (assert (not seq))
      (assert (typep err 'type-error)))))

;;; UBX-BASH-COPY transform had an inconsistent return type
(with-test (:name (replace (unsigned-byte 8) :return-type))
  (let ((sb-c::*check-consistency* t))
    (checked-compile
     '(lambda (l)
       (declare (type fixnum l))
       (let* ((bsize 128)
              (b1 (make-array bsize :element-type '(unsigned-byte 8)))
              (b2 (make-array l :element-type '(unsigned-byte 8))))
         (replace b1 b2 :start2 0 :end2 l))))))

(with-test (:name :bug-452008)
  ;; FIND & POSITION on lists should check bounds and (in safe code) detect
  ;; circular and dotted lists.
  (labels ((safe (&key speed safety &allow-other-keys)
             (case safety
               (0 (= speed 0))
               (t t)))
           (extra-safe (&key speed safety &allow-other-keys)
             (case safety
               (0 (= speed 0))
               (1 (< speed 2))
               (t t)))
           (test (type expr &key (filter #'safe))
             (checked-compile-and-assert
                 (:optimize `(:compilation-speed nil :space nil :filter ,filter)
                  :allow-style-warnings t)
                 `(lambda () ,expr)
               (() (condition type)))))
    (test 'sb-kernel:bounding-indices-bad-error
          '(find :foo '(1 2 3 :foo) :start 1 :end 5 :from-end t))
    (test 'sb-kernel:bounding-indices-bad-error
          '(position :foo '(1 2 3 :foo) :start 1 :end 5 :from-end t))
    (test 'sb-kernel:bounding-indices-bad-error
          '(find :foo '(1 2 3 :foo) :start 3 :end 0 :from-end t))
    (test 'sb-kernel:bounding-indices-bad-error
          '(position :foo '(1 2 3 :foo) :start 3 :end 0 :from-end t))
    (test 'type-error
          '(let ((list (list 1 2 3 :foo)))
             (find :bar (nconc list list)))
          :filter #'extra-safe)
    (test 'type-error
          '(let ((list (list 1 2 3 :foo)))
             (position :bar (nconc list list)))
          :filter #'extra-safe)))

(with-test (:name :bug-554385)
  ;; FIND-IF shouldn't look through the entire list.
  (assert (= 2 (find-if #'evenp '(1 2 1 1 1 1 1 1 1 1 1 1 :foo))))
  ;; Even though the end bounds are incorrect, the
  ;; element is found before that's an issue.
  (assert (eq :foo (find :foo '(1 2 3 :foo) :start 1 :end 5)))
  (assert (= 3 (position :foo '(1 2 3 :foo) :start 1 :end 5))))

(with-test (:name (search :empty-seq))
  (checked-compile-and-assert ()
      `(lambda (x)
         (declare (simple-vector x))
         (search x #()))
    ((#()) 0))
  (checked-compile-and-assert ()
      `(lambda (x)
         (declare (simple-vector x))
         (search x #(t t t)))
    ((#()) 0))
  (checked-compile-and-assert ()
      `(lambda (x)
         (declare (simple-vector x))
         (search x #(t t t) :end1 0))
    ((#(t t t)) 0))
  (checked-compile-and-assert ()
      `(lambda (x)
         (declare (simple-vector x))
         (search x #(t t t) :key nil))
    ((#()) 0))
  (checked-compile-and-assert ()
      `(lambda (x k)
         (declare (simple-vector x))
         (search x #(t t t) :key k))
    ((#() nil) 0))
  (checked-compile-and-assert (:optimize :safe :allow-warnings 'warning)
      `(lambda (x)
         (declare (simple-vector x))
         (search x #(t t t) :start2 1 :end2 0 :end1 0))
    ((#(t t t)) (condition 'sb-kernel:bounding-indices-bad-error)))
  (assert (eql 1
               (funcall (lambda ()
                          (declare (optimize speed))
                          (search #() #(1 1) :start2 1 :end2 1)))))
  (assert (eql 2
               (funcall (lambda ()
                          (declare (optimize speed))
                          (search #(1) #(1 1) :start1 1 :start2 2)))))
  (assert (eql 2
               (funcall (lambda ()
                          (declare (optimize speed))
                          (search #() #(1 1) :from-end t))))))

(with-test (:name (sort :smoke-test))
  (flet ((iota (n type &aux (i 0))
           (map-into (make-sequence type n)
                     (lambda ()
                       (incf i))))
         (shuffle (n type)
           (let ((vector (let ((i 0))
                           (map-into (make-array n)
                                     (lambda ()
                                       (incf i))))))
             (dotimes (i n (coerce vector type))
               (let ((j (+ i (random (- n i)))))
                 (rotatef (aref vector i) (aref vector j))))))
         (sortedp (x)
           (let* ((nonce (list nil))
                  (prev nonce))
             (every (lambda (x)
                      (prog1 (or (eql prev nonce)
                                 (< prev x))
                        (setf prev x)))
                    x))))
    (dolist (type '(simple-vector list))
      (dolist (size '(7 8 9 13 1023 1024 1025 1536))
        (loop for repeat below 5 do
          (assert (sortedp
                   (sort (funcall (case repeat
                                    (0 #'iota)
                                    (1 (lambda (n type)
                                         (reverse (iota n type))))
                                    (t #'shuffle))
                                  size type)
                         #'<))))))))

(with-test (:name (stable-sort :smoke-test))
  (flet ((iota (n type &aux (i 0))
           (map-into (make-sequence type n)
                     (lambda ()
                       (cons 0 (incf i)))))
         (shuffle (n type)
           (let ((max (truncate (expt n 1/4)))
                 (i   0))
             (map-into (make-sequence type n)
                       (lambda ()
                         (cons (random max) (incf i))))))
         (sortedp (x)
           (let* ((nonce (list nil))
                  (prev nonce))
             (every (lambda (x)
                      (prog1 (or (eql prev nonce)
                                 (< (car prev) (car x))
                                 (and (= (car prev) (car x))
                                      (< (cdr prev) (cdr x))))
                        (setf prev x)))
                    x))))
    (dolist (type '(simple-vector list))
      (dolist (size '(0  1  2  3  4  5  6  7  8
                      9 10 11 12 13 14 15 16 17
                      1023 1024 1025 1536))
        (loop for repeat below 5 do
          (assert
           (sortedp
            (stable-sort (funcall (case repeat
                                    (0 #'iota)
                                    (t #'shuffle))
                                  size type)
                         #'< :key #'car))))))))

(with-test (:name :&more-elt-index-too-large)
  (checked-compile-and-assert
      (:optimize `(:filter ,(lambda (&key safety &allow-other-keys)
                              (= safety 3))))
      `(lambda (&rest args)
         (elt args 0))
    (() (condition 'sb-kernel:index-too-large-error))))

(with-test (:name (sequence:dosequence :on-literals))
  (assert (= (sequence:dosequence (e #(1 2 3)) (return e))
             1)))

(with-test (:name (search :transform-notes))
  (checked-compile `(lambda (s)
                      (declare (optimize (speed 3) (safety 0))
                               (type simple-string s))
                      (search "foo" s))
                   :allow-notes nil))

(with-test (:name (concatenate :two-constants))
  (assert (equal (funcall
                  (lambda () (declare (optimize (speed 3)))
                    (concatenate 'string "a" "b")))
                 "ab")))

(with-test (:name (make-sequence :transform :bug-330299))
  (flet ((test (form &rest args)
           (multiple-value-bind (fun failure-p warnings style-warnings)
               (apply #'checked-compile form args)
             (declare (ignore fun failure-p))
             (assert (= (+ (length warnings) (length style-warnings)) 1)))))
    ;; test case from bug report.
    ;; erroneous situation is caught by MAKE-ARRAY
    (test '(lambda (size)
             (make-sequence 'bit-vector size :initial-element #\0))
          :allow-warnings 'sb-int:type-warning)
    ;; This is transformed, but MAKE-ARRAY does *not* consider it a problem
    ;; since #\x is in the upgraded array type. That's too bad, because
    ;; it's still poor style.
    #+nil
    (test '(lambda (size)
             (make-sequence '(member #\a #\b) size :initial-element #\x)))
    ;; additional tests where the transform gives up but warns
    (test '(lambda (n)
             (make-sequence '(vector (integer 1 15) 5) n :initial-element #\x))
          :allow-warnings t)
    (test '(lambda (n)
            (make-sequence '(vector (integer 1 15) 5) n))
          :allow-style-warnings t)))

;; Precisely type-check result of full call to MAP.
(with-test (:name (map notinline :maximally-safe))
  (assert-error
   (locally (declare (notinline map)) (map '(cons symbol) '+ '(1 2) '(3 4)))
   type-error)
  (assert-error
   (locally (declare (notinline map))
            (map '(cons t (cons t null)) '+ '(1 2 3) '(10 10 10)))
   type-error))

(with-test (:name (search :singleton-transform))
  (checked-compile-and-assert ()
    `(lambda (e) (search '(a) '(b) :end1 e))
    ((0) 0)))

(with-test (:name (search :type-derivation))
  (checked-compile-and-assert
   ()
   `(lambda (a b)
      (eql (search a (the (simple-vector 2) b) :from-end t) 2))
   ((#() #(1 2)) t)
   ((#(1) #(1 2)) nil)))

(with-test (:name (count :no-consing)
            :skipped-on :interpreter)
  (let ((f (checked-compile
            '(lambda (x)
              (count 1 x)))))
    (ctu:assert-no-consing (funcall f #(1 2 3 4)))
    (ctu:assert-no-consing (funcall f '(1 2 3 4)))))

(with-test (:name :hash-based-position)
  (let* ((items '(a b c d d d h e f b g b))
         (f (checked-compile
             `(lambda (x) (position x ',items))))
         (g (checked-compile
             `(lambda (x) (position x ',items :from-end t)))))
    (dolist (x items)
      ;; opaque-identify prevents optimizing the POSITION call
      (assert (= (funcall f x) (position x (opaque-identity items))))
      (assert (= (funcall g x) (position x (opaque-identity items) :from-end t))))
    (assert (not (funcall f 'blah)))
    (assert (not (funcall g 'blah)))))

(with-test (:name :hash-based-position-type-derivation)
  ;; should neither crash nor warn about NIL being fed into ASH
  (checked-compile '(lambda (x)
                     (declare (type (member a b) x))
                     (ash 1 (position x #(a b c d d e f))))))

(with-test (:name :position-empty-seq)
  (assert (not (funcall (checked-compile '(lambda (x) (position x #()))) 1))))

;;; I'm keeping this not-very-great test so that if I decide to re-allow hash collisions
;;; in the hash-based MEMBER transform, then there's already a test for it.
(with-test (:name :hash-based-memq :skipped-on :sbcl)
  (let* ((f (checked-compile
             '(lambda (x)
               (if (member x '(:and :or :not and or not)) t nil))))
         (consts (ctu:find-code-constants f :type 'vector)))
    ;; Since there's no canonical order within a bin - we don't know
    ;; whether bin 0 is {:AND,AND} or {AND,:AND} - this gets tricky to check.
    ;; This is unfortunately a change-detector (if we alter SXHASH, or anything).
    (assert (equalp (car consts) #(:and and :or or :not not 0 0)))))

(with-test (:name :memq-empty-seq)
  (assert (not (funcall (checked-compile '(lambda (x) (member x '()))) 1)))
  (assert (not (funcall (checked-compile '(lambda (x) (sb-int:memq x '()))) 1))))

(with-test (:name :adjoin-key-eq-comparable)
  (checked-compile-and-assert
      ()
      `(lambda (x y)
         (adjoin (list x) y :key 'car))
      ((3d0 '((3d0))) '((3d0)) :test #'equal)))

(with-test (:name :fill-transform-bounds-checks)
  (checked-compile-and-assert
      (:optimize :default)
      `(lambda (item start end)
         (fill (make-array 3 :element-type '(unsigned-byte 8)) item :start start :end end))
    ((2 0 nil) #(2 2 2) :test #'equalp)
    ((2 10 10)  (condition 'sb-kernel:bounding-indices-bad-error))
    ((2 2 1)  (condition 'sb-kernel:bounding-indices-bad-error))
    ((2 10 nil)  (condition 'sb-kernel:bounding-indices-bad-error))))

(with-test (:name :fill-transform-derive-type)
  (assert
   (equal (sb-kernel:%simple-fun-type
           (checked-compile
            '(lambda (x)
              (fill (the (simple-array (unsigned-byte 32) (*)) x) 0))))
          '(FUNCTION (T) (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (*)) &OPTIONAL)))))


(with-test (:name :fill-transform-print-case)
  (let ((*print-case* :downcase))
    (checked-compile-and-assert
        ()
        `(lambda (x)
           (make-array 3 :element-type 'fixnum :initial-element x))
      ((1) #(1 1 1) :test #'equalp))))


(with-test (:name (search :type-derivation))
  (checked-compile-and-assert
      ()
      `(lambda (s)
         (search '(a) s :end1 nil))
    (('(b a)) 1)
    ((#(1)) nil)))

(with-test (:name :array-equalp-non-consing
                  :skipped-on :interpreter)
  (let ((a (make-array 1000 :element-type 'double-float :initial-element 0d0))
        (b (make-array 1000 :element-type 'double-float :initial-element 0d0)))
    (ctu:assert-no-consing (equalp a b))))

(with-test (:name (search :array-equalp-numerics))
  ;; This tests something that wasn't broken, but given that the new algorithm
  ;; is potentially more complicated, it makes sense to test that various
  ;; combinations of numeric arrays compare as equalp when they should.
  (let (arrays (testdata '(7 3 1 5)))
    (sb-int:dovector
        (saetp (remove-if (lambda (x)
                            (not (typep (sb-vm:saetp-ctype x) 'sb-kernel:numeric-type)))
                          sb-vm:*specialized-array-element-type-properties*))
      (let ((et (sb-vm::saetp-specifier saetp)))
        (unless (or (eq et 'bit) (equal et '(unsigned-byte 2)))
          (let ((fancy-array
                 (make-array 4 :element-type et
                               :displaced-to (make-array 5 :element-type et)
                               :displaced-index-offset 1)))
            (replace fancy-array
                     (mapcar (lambda (x) (coerce x et)) testdata))
            (push fancy-array arrays)))))
    ;; All pairs should be EQUALP and it should be commutative
    ;; and they should be EQUALP to a simple-vector.
    (let* ((sv1 (coerce testdata 'simple-vector))
           (sv2 (map 'simple-vector (lambda (x) (coerce x 'single-float)) sv1))
           (sv3 (map 'simple-vector (lambda (x) (coerce x 'double-float)) sv1))
           (sv4 (map 'simple-vector (lambda (x) (coerce x '(complex single-float))) sv1))
           (sv5 (map 'simple-vector (lambda (x) (coerce x '(complex double-float))) sv1))
           (svs (list sv1 sv2 sv3 sv4 sv5)))
      (dolist (x arrays)
        ;; Try simple vectors containing types that are not EQL to the testdata
        (dolist (sv svs)
          (assert (equalp x sv))
          (assert (equalp sv x)))
        ;; Try all other numeric array types
        (dolist (y arrays)
          (assert (equalp x y)))))))

;; lp#1938598
(with-test (:name :vector-replace-self)
  ;; example 1
  (let ((string (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character)))
    (declare (notinline replace))
    (vector-push-extend #\_ string)
    ;; also test it indirectly
    (replace string string :start1 1 :start2 0))
  ;; example 2
  (let ((string (make-array 0 :adjustable t :fill-pointer 0 :element-type 'character)))
    (declare (notinline replace))
    (loop for char across "tset" do (vector-push-extend char string))
    (replace string string :start2 1 :start1 2)
    (assert (string= string "tsse"))))

(with-test (:name :sort-vector-length-1
                  :skipped-on :interpreter)
  (let ((v (vector 5)))
    (ctu:assert-no-consing (stable-sort v #'<))))

(with-test (:name (replace :empty-constant))
  (checked-compile-and-assert
      ()
      `(lambda (v s)
         (replace (the simple-vector v) #() :start1 s))
    ((#(1) 0) #(1) :test #'equalp)))

(with-test (:name :reduce-type-derive)
  (macrolet
      ((check (fun expected)
         `(assert
           (equal (second
                   (third
                    (sb-kernel:%simple-fun-type
                     (checked-compile '(lambda (x)
                                        ,fun)))))
                  ',expected))))
    (check (reduce '+ x)
           t)
    (check (reduce '+ x :end 10)
           number)
    (check (reduce '+ x :initial-value 10)
           number)
    (check (reduce '+ (the (simple-array t (1)) x))
           t)
    (check (reduce '+ (the (simple-array t (2)) x))
           number)
    (check (reduce '+ (the (simple-array t (10)) x) :end 1)
           t)
    (check (reduce '+ (the (simple-array fixnum (*)) x))
           integer)
    (check (reduce '+ (the (simple-array (unsigned-byte 8) (*)) x))
           unsigned-byte)
    (check (reduce '+ (the (simple-array (unsigned-byte 8) (*)) x) :initial-value -1)
           integer)
    (check (reduce '+ (the (simple-array double-float (*)) x) :initial-value 1)
           (or double-float (integer 1 1)))
    (check (reduce '+ (the (simple-array double-float (*)) x) :initial-value 1d0)
           double-float)
    (check (reduce '+ (the (simple-array double-float (*)) x))
           (or double-float (integer 0 0)))
    (check (reduce '+ (the (simple-array double-float (10)) x))
           double-float)
    (check (reduce '+ (the (simple-array double-float (1)) x))
           double-float)
    (check (reduce '+ x :key #'length)
           unsigned-byte)
    (check (reduce '+ x :key #'length :initial-value -1)
           integer)))

(with-test (:name :find-type-derive)
  (macrolet
      ((check (fun expected)
         `(assert
           (type-specifiers-equal
            (second
                   (third
                    (sb-kernel:%simple-fun-type
                     (checked-compile '(lambda (x y)
                                        (declare (ignorable x y))
                                        ,fun)))))
                  ',expected))))
    (check (find x y) t)
    (check (find 1 y) (or (integer 1 1) null))
    (check (find x y :key #'car) list)
    (check (find x y :test #'=) (or number null))
    (check (find x y :key #'car :test #'=) list)
    (check (find x (the vector y) :key #'car) list)
    (check (find-if #'evenp y) (or integer null))
    (check (find-if #'evenp (the list y) :key #'car) list)
    (check (find x (the (simple-array character (*)) y)) (or character null))
    (check (find x (the string y)) (or character null))
    (check (find #\A y :test #'char=) (or (eql #\A) null))
    (check (find #\a y :test #'char-equal) (or standard-char null))))

(with-test (:name :position-type-derive)
  (macrolet
      ((check (fun expected)
         `(assert
           (ctype= (second
                    (third
                     (sb-kernel:%simple-fun-type
                      (checked-compile '(lambda (x y)
                                         (declare (ignorable x y))
                                         ,fun)))))
                   ',expected))))
    (check (position x y) (or (integer 0 (#.(1- array-dimension-limit))) null))
    (check (position x (the (simple-string 10) y)) (or (mod 10) null))
    (check (position x y :end 10) (or (mod 10) null))
    (check (position x (the cons y) :start 5 :end 10) (or (integer 5 9) null))
    (check (position-if x y :end 10) (or (mod 10) null))
    (check (position x y :start (the (integer 10 20) x) :end x) (or (or (integer 10 19) null) null))))

(with-test (:name :string-cmp)
  (macrolet
      ((check (fun expected)
         `(assert
           (ctype= (second
                    (third
                     (sb-kernel:%simple-fun-type
                      (checked-compile '(lambda (x y)
                                         (declare (ignorable x y))
                                         ,fun)))))
                   ',expected))))
    (check (string/= (the simple-string x) (the simple-string y) :end2 0)
           (or (integer 0 0) null))))

(with-test (:name :reverse-specialized-arrays)
  (loop for saetp across sb-vm:*specialized-array-element-type-properties*
        for type = (sb-kernel:type-specifier (sb-vm:saetp-ctype saetp))
        when type
        do
        (let ((value-transformer (cond ((eq type #+sb-unicode 'base-char
                                                 #-sb-unicode 'character)
                                        (lambda (x)
                                          (code-char
                                           (if (>= x sb-int:base-char-code-limit)
                                               (random sb-int:base-char-code-limit)
                                               x))))
                                       #+sb-unicode
                                       ((eq type 'character)
                                        (lambda (x)
                                          (code-char x)))
                                       ((eq type 'bit)
                                        (lambda (x)
                                          x
                                          (random 2)))
                                       ((subtypep type 'integer)
                                        (if (eq type 'fixnum)
                                            #'identity
                                            (let* ((signed (eq (car type) 'signed-byte))
                                                   (width (second type))
                                                   (mod (expt 2 (- width
                                                                   (if signed
                                                                       1
                                                                       0)))))
                                              (if (< mod 1300)
                                                  (lambda (x)
                                                    (if (>= x mod)
                                                        (random mod)
                                                        x))
                                                  (lambda (x)
                                                    x)))))
                                       (t
                                        (lambda (x)
                                          (coerce x type))))))
          (loop for i to (floor 1300
                                (ceiling (sb-vm:saetp-n-bits saetp) sb-vm:n-word-bytes))
                for list = (loop for j from 1 to i
                                 collect (funcall value-transformer j))
                for reverse = (reverse list)
                for vector = (make-array i :element-type type
                                           :initial-contents list)
                do
                (let* ((offset (1+ (random 120)))
                       (prefix (loop for j from 1 to offset
                                     collect (funcall value-transformer j)))
                       (suffix (loop for j from 1 to (- 128 offset)
                                     collect (funcall value-transformer j)))
                       (contents (concatenate 'list prefix list suffix))
                       (source (make-array (+ i 128) :element-type type
                                                     :initial-contents contents))
                       (displaced (make-array i :element-type type
                                                :displaced-to source
                                                :displaced-index-offset offset
                                                :fill-pointer i)))
                  (assert (equal reverse (coerce (reverse displaced) 'list)))
                  (assert (equal reverse (coerce (nreverse displaced) 'list)))
                  (assert (equal prefix (coerce (subseq source 0 offset) 'list)))
                  (assert (equal suffix (coerce (subseq source (+ offset i)) 'list))))
                (assert (equal reverse (coerce (reverse vector) 'list)))
                (assert (equal reverse (coerce (nreverse vector) 'list)))))))

(with-test (:name :list-derived-type)
  (macrolet
      ((check (fun expected)
         `(assert
           (ctype= (second
                    (third
                     (sb-kernel:%simple-fun-type
                      (checked-compile '(lambda (x y)
                                         (declare (ignorable x y))
                                         ,fun)))))
                   ',expected))))
    (check (sort (the (cons (eql 0)) x) y)
           cons)))

(with-test (:name :range-error-fill-transform)
  (assert
   (nth-value 2 (checked-compile `(lambda (x y)
                                    (declare ((simple-base-string 10) x))
                                    (fill x y :start 12))
                                 :allow-warnings t))))

(with-test (:name :find-compile-time-mismatch)
  (assert
   (nth-value 2 (checked-compile `(lambda (c) (find c #*10 :test #'char-equal))
                                 :allow-warnings t))))

(with-test (:name :subseq-nil-array)
  (checked-compile-and-assert
   ()
   `(lambda (s)
      (subseq s 2))
   (((make-array 5 :element-type nil))
    3 :test (lambda (s n)
              (= (car n) (length (car s)))))))

(with-test (:name :use-%bit-pos-fwd/1)
  (assert (equal  (ctu:ir1-named-calls `(lambda (x)
                                          (declare (optimize speed))
                                          (find 1 (the simple-bit-vector x))))
                  '(SB-KERNEL:%BIT-POS-FWD/1))))

(with-test (:name :sort-inlining-warnings)
  (checked-compile `(lambda (x)
                      (declare (optimize (debug 2) (space 0)))
                      (sort x #'< :key #'car))))

(with-test (:name :sort-inline-return-value)
  (checked-compile-and-assert
   ()
   `(lambda (v)
      (declare ((vector t) v))
      (locally (declare (optimize (space 0)))
        (sort v #'<)))
   (((vector 2 1)) #(1 2) :test #'equalp)))

(with-test (:name :read-sequence-type)
  (assert-type
   (lambda (stream)
     (let ((seq (make-string 100)))
       (read-sequence seq stream)))
   (mod 101))
  (assert-type
   (lambda (stream n)
     (let ((seq (make-string n)))
       (read-sequence seq stream :end 10)))
   (mod 11))
  (assert-type
   (lambda (stream)
     (let ((seq (make-string 10)))
       (read-sequence seq stream :start 1)))
   (integer 1 10)))

(with-test (:name (replace :or-null))
  (checked-compile `(lambda (a b)
                      (declare ((or null simple-base-string) a)
                               ((or null (simple-array character (*))) b)
                               (optimize speed))
                      (replace a b))
                   :allow-notes nil)
  (checked-compile `(lambda (a b)
                      (declare ((or null simple-base-string) a)
                               ((simple-array character (*)) b)
                               (optimize speed))
                      (replace a b))
                   :allow-notes nil)
  (checked-compile `(lambda (a b)
                      (declare ((or null simple-base-string) a b)
                               (optimize speed))
                      (replace a b))
                   :allow-notes nil)
  (checked-compile `(lambda ()
                      (coerce nil 'vector))
                   :allow-notes nil)
  (checked-compile `(lambda ()
                      (replace nil #()))
                   :allow-notes nil))

(with-test (:name :reduce-type)
  (assert-type
   (lambda (v)
     (declare ((vector (unsigned-byte 8)) v))
     (reduce #'logior v))
   (unsigned-byte 8)))

(with-test (:name :position-index-errors)
  (assert (= (count 'sb-int:sequence-bounding-indices-bad-error
                    (ctu:ir1-named-calls `(lambda (y)
                                            (declare (optimize speed)
                                                     (simple-string y))
                                            (position #\a y :start 1))))
             1))
  (assert (= (count 'sb-int:sequence-bounding-indices-bad-error
                    (ctu:ir1-named-calls `(lambda (y)
                                            (declare (optimize speed)
                                                     (string y))
                                            (find #\a y :start 1))))
             1)))

(with-test (:name :find-equalp-type)
  (assert-type
   (lambda (y j)
     (declare ((or integer simple-string) j))
     (find j y :test #'equalp))
   (or array number null)))

(with-test (:name :sequence-union-types)
  (assert-type
   (lambda (x)
     (declare ((or cons (simple-vector 10)) x))
     (copy-seq x))
   (or cons (simple-vector 10)))
  (assert-type
   (lambda (x)
     (declare ((or list (vector t 10)) x))
     (reverse x))
   (or list simple-vector))
  (assert-type
   (lambda (x)
     (declare (cons x))
     (copy-seq x))
   cons)
  (assert-type
   (lambda (x)
     (declare ((simple-vector 10) x))
     (remove 10 x))
   simple-vector))

(with-test (:name :concatenate-list-type)
  (assert-type
   (lambda (x)
     (concatenate 'list '(1 2 3) x))
   cons)
  (assert-type
   (lambda (x)
     (declare (cons x))
     (concatenate 'list x x))
   cons))
