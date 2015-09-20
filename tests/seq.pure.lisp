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

(in-package :cl-user)

;;; As reported by Paul Dietz from his ansi-test suite for gcl, REMOVE
;;; malfunctioned when given :START, :END and :FROM-END arguments.
;;; Make sure it doesn't happen again.
(let* ((orig '(1 2 3 2 6 1 2 4 1 3 2 7))
       (x (copy-seq orig))
       (y (remove 3 x :from-end t :start 1 :end 5))
       (z (remove 2 x :from-end t :start 1 :end 5)))
  (assert (equalp orig x))
  (assert (equalp y '(1 2 2 6 1 2 4 1 3 2 7)))
  (assert (equalp z '(1 3 6 1 2 4 1 3 2 7))))

;;; Similarly, NSUBSTITUTE and friends were getting things wrong with
;;; :START, :END and :FROM-END:
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
                                                :initial-element 'a))))))))

;;; And equally similarly, REMOVE-DUPLICATES misbehaved when given
;;; :START arguments:

(let ((orig (list 0 1 2 0 1 2 0 1 2 0 1 2)))
  (assert (equalp (remove-duplicates orig :start 3 :end 9) '(0 1 2 0 1 2 0 1 2)))
  (assert (equalp (delete-duplicates orig :start 3 :end 9) '(0 1 2 0 1 2 0 1 2))))

;;; tests of COUNT
(assert (= 1 (count 1 '(1 2 3))))
(assert (= 2 (count 'z #(z 1 2 3 z))))
(assert (= 0 (count 'y '(z 1 2 3 z))))

;;; tests of COUNT-IF and COUNT-IF-NOT
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
                (format t "~&SEQUENCE-AS-LIST=~S~%" ',sequence-as-list)
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
(multiple-value-bind (v e)
    (ignore-errors (count-if #'zerop '(0 a 0 b c) :start 1))
  (declare (ignore v))
  (assert (eql (type-error-datum e) 'a)))
(multiple-value-bind (v e)
    (ignore-errors (count-if #'zerop #(0 a 0 b c) :start 1 :from-end 11))
  (declare (ignore v))
  (assert (eql (type-error-datum e) 'c)))

;;; :COUNT may be negative and BIGNUM
(assert (equal (remove 1 '(1 2 3 1) :count 1) '(2 3 1)))
(assert (equal (remove 1 '(1 2 3 1) :count (* 2 most-positive-fixnum)) '(2 3)))
(assert (equal (remove 1 '(1 2 3 1) :count (* -2 most-positive-fixnum)) '(1 2 3 1)))

;;; bug reported by Wolfgang Jenkner on sbcl-devel 2003-01-04:
;;; embedded calls of SORT do not work
(assert (equal (sort (list 0 0 0) (lambda (x y) (sort (list 0 0 0) #'<) nil))
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
               '(0 0 0 0 0)))

;;; miscellaneous sanity checks on stuff which could've been broken by
;;; changes in MERGE-LIST* in sbcl-0.7.11.*
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
               '(1 -1 2 -2 3 -3)))

;;; CSR broke FILL by not returning the sequence argument in a transform.
(let* ((s1 (copy-seq "abcde"))
       (s2 (fill s1 #\z)))
  (assert s2)
  (assert (string= s2 "zzzzz")))

;;; POSITION on displaced arrays with non-zero offset has been broken
;;; for quite a while...
(let ((fn (compile nil '(lambda (x) (position x)))))
  (let* ((x #(1 2 3))
         (y (make-array 2 :displaced-to x :displaced-index-offset 1)))
    (assert (= (position 2 y) 0))))

;;; (SIMPLE-STRING) is a legal type specifier for creation functions
(let ((a (make-sequence '(simple-string) 5))
      (b (concatenate '(simple-string) "a" "bdec"))
      (c (map '(simple-string) 'identity "abcde"))
      (d (merge '(simple-string) (copy-seq "acd") (copy-seq "be") 'char>))
      (e (coerce '(#\a #\b #\c #\e #\d) '(simple-string))))
  (assert (= (length a) 5))
  (assert (string= b "abdec"))
  (assert (string= c "abcde"))
  (assert (string= d "beacd"))
  (assert (string= e "abced")))

;;; COPY-SEQ "should be prepared to signal an error if sequence is not
;;; a proper sequence".
(locally (declare (optimize safety))
  (multiple-value-bind (seq err) (ignore-errors (copy-seq '(1 2 3 . 4)))
    (assert (not seq))
    (assert (typep err 'type-error))))

;;; UBX-BASH-COPY transform had an inconsistent return type
(let ((sb-c::*check-consistency* t))
  (handler-bind ((warning #'error))
    (compile nil
             '(lambda (l)
               (declare (type fixnum l))
               (let* ((bsize 128)
                      (b1 (make-array bsize :element-type '(unsigned-byte 8)))
                      (b2 (make-array l :element-type '(unsigned-byte 8))))
                 (replace b1 b2 :start2 0 :end2 l))))))

(with-test (:name :bug-452008)
  ;; FIND & POSITION on lists should check bounds and (in safe code) detect
  ;; circular and dotted lists.
  (dolist (policy '(((speed 3) (safety 2)) ; transformed and safe
                    ((speed 3) (safety 1)) ; transformed and unsafe
                    ((speed 0) (safety 0)))) ; untransformed - safe regardless
    (flet ((test (type expr)
             (let ((lambda `(lambda () (declare (optimize ,@policy)) ,expr)))
               #+nil(let ((*print-circle* t)) (format t "~&test: ~S~%" lambda))
               (let ((got (handler-case (funcall (compile nil lambda))
                            (error (e) (if (typep e type) :error :lose))
                            (:no-error (res) (list :no-error res)))))
                 (unless (eq :error got)
                   (error "wanted an error, got ~S~%" got))))))
      (test 'sb-kernel:bounding-indices-bad-error
            '(find :foo '(1 2 3 :foo) :start 1 :end 5 :from-end t))
      (test 'sb-kernel:bounding-indices-bad-error
            '(position :foo '(1 2 3 :foo) :start 1 :end 5 :from-end t))
      (test 'sb-kernel:bounding-indices-bad-error
            '(find :foo '(1 2 3 :foo) :start 3 :end 0 :from-end t))
      (test 'sb-kernel:bounding-indices-bad-error
            '(position :foo '(1 2 3 :foo) :start 3 :end 0 :from-end t))
      (unless (equal policy '((speed 3) (safety 1)))
        (test 'type-error
              '(let ((list (list 1 2 3 :foo)))
                 (find :bar (nconc list list))))
        (test 'type-error
              '(let ((list (list 1 2 3 :foo)))
                 (position :bar (nconc list list))))))))

(with-test (:name :bug-554385)
  ;; FIND-IF shouldn't look through the entire list.
  (assert (= 2 (find-if #'evenp '(1 2 1 1 1 1 1 1 1 1 1 1 :foo))))
  ;; Even though the end bounds are incorrect, the
  ;; element is found before that's an issue.
  (assert (eq :foo (find :foo '(1 2 3 :foo) :start 1 :end 5)))
  (assert (= 3 (position :foo '(1 2 3 :foo) :start 1 :end 5))))

(with-test (:name (:search :empty-seq))
  (assert (eql 0
               (funcall (compile nil
                                 `(lambda (x)
                                    (declare (optimize (speed 3)) (simple-vector x))
                                    (search x #())))
                        #())))
  (assert (eql 0
               (funcall (compile nil
                                 `(lambda (x)
                                    (declare (optimize (speed 3)) (simple-vector x))
                                    (search x #(t t t))))
                        #())))
  (assert (eql 0
               (funcall (compile nil
                                 `(lambda (x)
                                    (declare (optimize (speed 3)) (simple-vector x))
                                    (search x #(t t t) :end1 0)))
                        #(t t t))))
  (assert (eql 0
               (funcall (compile nil
                                 `(lambda (x)
                                    (declare (optimize (speed 3)) (simple-vector x))
                                    (search x #(t t t) :key nil)))
                        #())))
  (assert (eql 0
               (funcall (compile nil
                                 `(lambda (x k)
                                    (declare (optimize (speed 3)) (simple-vector x))
                                    (search x #(t t t) :key k)))
                        #() nil)))
  (assert (eq :ok
              (handler-case
                  (funcall (compile nil
                                    `(lambda (x)
                                       (declare (optimize (speed 3)) (simple-vector x))
                                       (search x #(t t t) :start2 1 :end2 0 :end1 0)))
                           #(t t t))
                (sb-kernel:bounding-indices-bad-error ()
                  :ok))))
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

(with-test (:name :sort-smoke-test)
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

(with-test (:name :stable-sort-smoke-test)
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
  (assert-error (funcall
                 (compile nil '(lambda (&rest args)
                                (declare (optimize safety))
                                (elt args 0))))
                sb-kernel:index-too-large-error))

(with-test (:name :do-sequence-on-literals)
  (assert (= (sequence:dosequence (e #(1 2 3)) (return e))
             1)))

(with-test (:name :search-transform-notes)
  (assert-no-signal
   (compile nil `(lambda (s)
                   (declare (optimize (speed 3) (safety 0))
                            (type simple-string s))
                   (search "foo" s)))
   sb-ext:compiler-note))

(with-test (:name :concatenate-two-constants)
  (assert (equal (funcall
                  (lambda () (declare (optimize (speed 3)))
                    (concatenate 'string "a" "b")))
                 "ab")))

(with-test (:name :bug-330299-make-sequence-transform)
  ;; test case from bug report.
  ;; erroneous situation is caught by MAKE-ARRAY
  (assert-signal
   (compile nil
            '(lambda (size)
               (make-sequence 'bit-vector size :initial-element #\0))))
  ;; This is transformed, but MAKE-ARRAY does *not* consider it a problem
  ;; since #\x is in the upgraded array type. That's too bad, because
  ;; it's still poor style.
  #+nil
  (assert-signal
   (compile nil
            '(lambda (size)
               (make-sequence '(member #\a #\b) size :initial-element #\x))))
  ;; additional tests where the transform gives up but warns
  (assert-signal
   (compile nil
            '(lambda (n)
               (make-sequence '(vector (integer 1 15) 5) n
                              :initial-element #\x))))
  (assert-signal
   (compile nil
            '(lambda (n)
               (make-sequence '(vector (integer 1 15) 5) n)))))

;; Precisely type-check result of full call to MAP.
(with-test (:name :notinlined-map-maximally-safe)
  (assert-error
   (locally (declare (notinline map)) (map '(cons symbol) '+ '(1 2) '(3 4)))
   type-error)
  (assert-error
   (locally (declare (notinline map))
            (map '(cons t (cons t null)) '+ '(1 2 3) '(10 10 10)))
   type-error))
