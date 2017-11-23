;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was written at
;;;; Carnegie Mellon University and released into the public domain. The
;;;; software is in the public domain and is provided with absolutely no
;;;; warranty. See the COPYING and CREDITS files for more information.

(in-package :sb-concurrency-test)

(deftest queue.1
    (let ((q (make-queue :name 'test-q :initial-contents '(1 2 3))))
      (enqueue 4 q)
      (values (queue-name q)
              (multiple-value-list (dequeue q))
              (list-queue-contents q)))
  test-q
  (1 t)
  (2 3 4))

(deftest queue.2
    (dequeue (make-queue))
  nil
  nil)

(deftest queue.3
    (dequeue (make-queue :initial-contents '(nil)))
  nil
  t)

(deftest queue.4
    (let ((x (make-instance 'structure-object))
          (y (make-queue)))
      ;; I wonder why I thought this needs testing?
      (values (typep x 'queue)
              (queuep x)
              (typep y 'queue)
              (queuep y)))
  nil nil t t)

(deftest queue.5
    (let ((q (make-queue :initial-contents (vector 1 2 3 4 5))))
      (values (= 5 (queue-count q))
              (enqueue 'foo q)
              (= 6 (queue-count q))
              (dequeue q)
              (= 5 (queue-count q))
              (dequeue q)
              (= 4 (queue-count q))
              (dequeue q)
              (= 3 (queue-count q))
              (dequeue q)
              (= 2 (queue-count q))
              (dequeue q)
              (= 1 (queue-count q))
              (not (queue-empty-p q))
              (dequeue q)
              (= 0 (queue-count q))
              (queue-empty-p q)
              (dequeue q)
              (= 0 (queue-count q))
              (queue-empty-p q)))
  t
  foo
  t
  1
  t
  2
  t
  3
  t
  4
  t
  5
  t
  t
  foo
  t
  t
  nil
  t
  t)

#+sb-thread
(deftest queue.t.1
    (let* ((q (make-queue))
           (w (make-semaphore))
           (r (make-semaphore))
           (n 100000)
           (schedulers (list
                        (make-thread (lambda ()
                                       (signal-semaphore r)
                                       (wait-on-semaphore w)
                                       (dotimes (i n)
                                         (enqueue (cons :a i) q))))
                        (make-thread (lambda ()
                                       (signal-semaphore r)
                                       (wait-on-semaphore w)
                                       (dotimes (i n)
                                         (enqueue (cons :b i) q))))
                        (make-thread (lambda ()
                                       (signal-semaphore r)
                                       (wait-on-semaphore w)
                                       (dotimes (i n)
                                         (enqueue (cons :c i) q))))
                        (make-thread (lambda ()
                                       (signal-semaphore r)
                                       (wait-on-semaphore w)
                                       (dotimes (i n)
                                         (enqueue (cons :d i) q)))))))
      (loop repeat 4 do (wait-on-semaphore r))
      (signal-semaphore w 4)
      (mapc #'join-thread schedulers)
      (let (a b c d)
        (loop
          (multiple-value-bind (item ok) (dequeue q)
            (cond (item
                   (assert ok)
                   (case (car item)
                     (:a (push (cdr item) a))
                     (:b (push (cdr item) b))
                     (:c (push (cdr item) c))
                     (:d (push (cdr item) d))))
                  (t
                   (assert (not ok))
                   (return)))))
        (labels ((check-list (list)
                   (when list
                     (if (cdr list)
                         (when (= (first list) (1- (second list)))
                           (check-list (cdr list)))
                         (= (first list) (1- n))))))
          (values (check-list (nreverse a))
                  (check-list (nreverse b))
                  (check-list (nreverse c))
                  (check-list (nreverse d))))))
  t
  t
  t
  t)

#+sb-thread
(deftest queue.t.2
    (let ((q (make-queue))
          (w (make-semaphore))
          (r (make-semaphore)))
      (dotimes (i 1000000)
        (enqueue i q))
      (flet ((dq ()
               (signal-semaphore r)
               (wait-on-semaphore w)
               (let ((last -1))
                 (loop
                  (multiple-value-bind (x ok) (dequeue q)
                    (cond (x
                           (if (and (> x last) ok)
                               (setf last x)
                               (return (list last x ok))))
                          (t
                           (if (not ok)
                               (return t)
                               (return (list last x ok))))))))))
        (let ((deschedulers
                (list (make-thread #'dq)
                      (make-thread #'dq)
                      (make-thread #'dq)
                      (make-thread #'dq))))
          (loop repeat 4 do (wait-on-semaphore r))
          (signal-semaphore w 4)
          (mapcar #'join-thread deschedulers))))
  (t t t t))

#+sb-thread
(deftest queue.t.3
    (let* ((q (make-queue))
           (w (make-semaphore))
           (r (make-semaphore))
           (n 100000)
           (schedulers (list
                        (make-thread (lambda ()
                                       (signal-semaphore r)
                                       (wait-on-semaphore w)
                                       (dotimes (i n)
                                         (enqueue (cons :a i) q))))
                        (make-thread (lambda ()
                                       (signal-semaphore r)
                                       (wait-on-semaphore w)
                                       (dotimes (i n)
                                         (enqueue (cons :b i) q))))
                        (make-thread (lambda ()
                                       (signal-semaphore r)
                                       (wait-on-semaphore w)
                                       (dotimes (i n)
                                         (enqueue (cons :c i) q))))
                        (make-thread (lambda ()
                                       (signal-semaphore r)
                                       (wait-on-semaphore w)
                                       (dotimes (i n)
                                         (enqueue (cons :d i) q)))))))
      (flet ((dq ()
               (let ((a -1)
                     (ac 0)
                     (b -1)
                     (bc 0)
                     (c -1)
                     (cc 0)
                     (d -1)
                     (dc 0))
                 (signal-semaphore r)
                 (wait-on-semaphore w)
                 (loop (multiple-value-bind (item ok) (dequeue q)
                         (cond (item
                                (let ((n (cdr item)))
                                  (macrolet ((test (name c)
                                               `(if (< ,name n)
                                                    (progn
                                                      (setf ,name n)
                                                      (incf ,c))
                                                    (return nil))))
                                    (ecase (car item)
                                      (:a (test a ac))
                                      (:b (test b bc))
                                      (:c (test c cc))
                                      (:d (test d dc))))))
                               (t
                                (assert (not ok))
                                (unless (or (some #'thread-alive-p schedulers)
                                            (not (queue-empty-p q)))
                                  (return (list a ac b bc c cc d dc))))))))))
        (let ((deschedulers (list
                             (make-thread #'dq)
                             (make-thread #'dq)
                             (make-thread #'dq)
                             (make-thread #'dq))))
          (loop repeat 8 do (wait-on-semaphore r))
          (signal-semaphore w 8)
          (let ((a -1)
                (ac 0)
                (b -1)
                (bc 0)
                (c -1)
                (cc 0)
                (d -1)
                (dc 0))
            (mapc (lambda (th)
                    (let ((results (join-thread th)))
                      (when results
                        (destructuring-bind (ta tac tb tbc tc tcc td tdc) results
                          (setf a (max ta a)
                                b (max tb b)
                                c (max tc c)
                                d (max td d))
                          (incf ac tac)
                          (incf bc tbc)
                          (incf cc tcc)
                          (incf dc tdc)))))
                  deschedulers)
            (and (= n ac (1+ a))
                 (= n bc (1+ b))
                 (= n cc (1+ c))
                 (= n dc (1+ d)))))))
  t)
