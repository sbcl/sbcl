;;;; -*-  Lisp -*-
;;;;
;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package :sb-concurrency-test)

(deftest gate.0
    (let ((gate (make-gate :open t)))
      (values (wait-on-gate gate)
              (close-gate gate)
              (wait-on-gate gate :timeout 0.1)))
  t
  t
  nil)

#+sb-thread
(progn
  ;; Create threads waiting until a gate is opened, then open that
  ;; gate and assure that all waiters were waked up. Also make sure
  ;; that interrupting a thread waiting on a gate doesn't make it
  ;; cross the gate if it is closed.
  (deftest gate.1
      (let* ((gate (make-gate))
             (marks (make-array (if (> *cpus* 1) 100 50) :initial-element nil))
             (threads (loop for i from 0 below (length marks)
                            collect (make-thread (lambda (n)
                                                   (wait-on-gate gate)
                                                   (setf (aref marks n) (cons n (aref marks n))))
                                                 :arguments i)))
             (int-gate (make-gate)))
        (sleep 1)
        ;; Considering that OPEN-GATE = CONDITION-BROADCAST, this action of INTERRUPT-THREAD
        ;; with an (OPEN-GATE) call would be expressly forbidden if condition vars were
        ;; implemented by native (pthread) primitives. Generally speaking the pthread intrinsics
        ;; provide better mutex fairness, and I very much doubt we could use our condition
        ;; vars with pthread mutexes.
        ;;
        ;; Debian docs say: "ASYNC-SIGNAL SAFETY
        ;;  The condition functions are not async-signal safe, and should not be called from a
        ;;  signal handler. In particular, calling pthread_cond_signal or pthread_cond_broadcast
        ;;  from a signal handler may deadlock the calling thread."
        ;; https://manpages.debian.org/testing/glibc-doc/pthread_cond_broadcast.3.en.html
        ;;
        ;; OpenGroup calls out cond_signal but does not say cond_broadcast isn't safe.
        ;; I suspect that it means that they both are not safe since they share a doc page:
        ;; "It is not safe to use the pthread_cond_signal() function in a signal handler
        ;;  that is invoked asynchronously."
        ;; https://pubs.opengroup.org/onlinepubs/007904975/functions/pthread_cond_broadcast.html
        (interrupt-thread (car threads) (lambda ()
                                          (unwind-protect
                                               (when (gate-open-p gate)
                                                 (abort-thread))
                                            (open-gate int-gate))))
        (wait-on-gate int-gate)
        (assert (every #'null marks))
        (open-gate gate)
        (mapc #'join-thread threads)
        (dotimes (i (length marks))
          (assert (equal (list i) (aref marks i))))
        t)
    t)

  ;; Assure that CLOSE-GATE can close a gate while other threads are operating
  ;; through that gate. In particular, assure that no operation is performed
  ;; once the gate is closed.
  (deftest gate.2
      (let* ((gate (make-gate))
             (cont (make-gate))
             (marks (make-array (if (> *cpus* 1) 100 50) :initial-element nil))
             (threads (loop for i from 0 below (length marks)
                            collect (make-thread (lambda (n)
                                                   (wait-on-gate gate)
                                                   (when (oddp n)
                                                     (sleep 1.0))
                                                   (wait-on-gate gate)
                                                   (setf (aref marks n) (cons n (aref marks n))))
                                                 :arguments i))))
        (open-gate gate)
        (sleep 0.5)
        (close-gate gate)
        (let (odds evens)
          (loop while threads
                do (push (pop threads) evens)
                   (push (pop threads) odds))
          (mapc #'join-thread evens)
          (loop for i from 0 below (length marks)
                do (if (oddp i)
                       (assert (not (aref marks i)))
                       (assert (equal (list i) (aref marks i)))))
          (open-gate gate)
          (mapc #'join-thread odds)
          (loop for i from 0 below (length marks)
                do (when (oddp i)
                     (assert (equal (list i) (aref marks i)))))
          t))
    t)

  ;; Assures that WAIT-ON-GATE can be interrupted by deadlines.
  (deftest gate-deadline.1
      (let* ((gate (make-gate))
             (waiter (make-thread (lambda ()
                                    (block nil
                                      (handler-bind ((sb-sys:deadline-timeout
                                                       #'(lambda (c)
                                                           (return :deadline))))
                                        (sb-sys:with-deadline (:seconds 0.1)
                                          (wait-on-gate gate))))))))
        (join-thread waiter))
    :deadline)

  ;; Assure that WAIT-ON-GATE can be interrupted by deadlines, and resumed from
  ;; the deadline handler.
  (deftest gate-deadline.2
      (let* ((gate (make-gate))
             (ready (make-gate))
             (cancel nil)
             (waiter (make-thread (lambda ()
                                    (block nil
                                      (handler-bind ((sb-sys:deadline-timeout
                                                       #'(lambda (c)
                                                           (setf cancel t)
                                                           (sb-sys:cancel-deadline c))))
                                        (sb-sys:with-deadline (:seconds 0.1)
                                          (open-gate ready)
                                          (wait-on-gate gate))))))))
        (wait-on-gate ready)
        (sleep 1.0)
        (open-gate gate)
        (values (join-thread waiter) cancel))
    t t)

  (deftest gate-timeout.1
      (let* ((gate (make-gate))
             (waiter (make-thread (lambda ()
                                    (wait-on-gate gate :timeout 0.1)))))
        (join-thread waiter))
    nil)

  (deftest gate-timeout.2
      (let* ((gate (make-gate))
             (waiter (make-thread (lambda ()
                                    (open-gate gate)
                                    (wait-on-gate gate :timeout 0.1)))))
        (join-thread waiter))
    t))
