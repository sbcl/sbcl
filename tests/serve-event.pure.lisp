;;;; tests of SERVE-EVENT
;;;;
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

(when (find-symbol "COMPUTE-POLLFDS" "SB-IMPL")
  (push :compute-pollfds-test *features*))
#+compute-pollfds-test
(import '(sb-impl::make-handler
          sb-impl::handler-descriptor
          sb-impl::handler-bogus
          sb-impl::compute-pollfds))

;; Tests for SERVE-EVENT are somewhat lacking,
;; although RUN-PROGRAM exercises some multiplexed I/O.
;; At any rate, this tests that the utility function COMPUTE-POLLFDS is sane.
;; This can't use :skipped-on because SB-UNIX does not define a POLLFD alien
;; type nor export the relevant lisp symbols unless the poll() syscall exists.
#+compute-pollfds-test
(test-util:with-test (:name :compute-pollfds)
  (labels ((try (list how)
             (multiple-value-bind (pollfds count map)
                 (compute-pollfds list (length list) how)
               (multiple-value-prog1
                   (values (alien-to-native pollfds count) map)
                 (free-alien pollfds))))
           (alien-to-native (pollfds n)
             (let ((result (make-list n)))
               (dotimes (i n result)
                 (setf (elt result i)
                       (list (slot (deref pollfds i) 'sb-unix:fd)
                             (slot (deref pollfds i) 'sb-unix:events))))))
           (bogotify (h)
             ;; a handler becomes bogus when the user hasn't removed
             ;; it from the list, but the kernel said EBADF on it.
             (setf (handler-bogus h) t)
             h)
           (run-test (&rest list)
             (sb-int:binding* (((fds1 map1) (try list nil))
                               ((fds2 map2) (try list t)))
               #+(or) (format t "~&~D handlers, ~D descriptors, ~D non-bogus~@
                                 ~:{(~D #b~b)~:^ ~}~%~S~%"
                              (length list)
                              (length (remove-duplicates
                                       list :key #'handler-descriptor))
                              (length (remove-duplicates
                                       (remove-if #'handler-bogus list)
                                       :key #'handler-descriptor))
                              fds1 map1)
               (loop for handler in list
                     for handler-index from 0
                     for fd-index = (svref map1 handler-index)
                     do (cond ((handler-bogus handler)
                               (assert (null fd-index))) ; isn't in fds[]
                              (t
                               (assert (eql (car (elt fds1 fd-index))
                                            (handler-descriptor handler))))))
               ;; the two algorithms for de-duplication of file descriptors
               ;; should be equivalent.
               (assert (equalp fds1 fds2))
               (assert (equalp map1 map2)))))

    ;; Basic correctness
    (run-test (make-handler :output 1030 #'car)
              (make-handler :input 1028 #'car)
              (make-handler :input 500 #'car)
              (make-handler :output 500 #'car)
              (make-handler :input 1028 #'car)
              (make-handler :input 1030 #'car))

    ;; this test is particularly insidious because descriptor 92
    ;; appears as both a bogus descriptor and non-bogus,
    ;; which probably can't happen in real life.
    (run-test (bogotify (make-handler :output 92 #'car))
              (make-handler :output 91 #'car)
              (make-handler :output 80 #'car)
              (make-handler :input 92 #'car)
              (make-handler :input 2 #'car)
              (bogotify (make-handler :input 5 #'car))
              (make-handler :input 3 #'car)
              (make-handler :input 2 #'cadr)
              (make-handler :input 1 #'car)
              (make-handler :input 2 #'car)
              (make-handler :input 9 #'car)
              (make-handler :input 55 #'car))))
