;;;; callback tests

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

#+(or (not sb-thread) win32 sb-safepoint) (sb-ext:exit :code 104)

(with-scratch-file (solib "so")
  (sb-ext:run-program "/bin/sh"
                      `("run-compiler.sh" "-sbcl-pic" "-sbcl-shared"
                        "-o" ,solib "fcb-threads.c"))
  (sb-alien:load-shared-object solib))

(defglobal *counter* 0)
(declaim (fixnum *counter*))
(defglobal *ok* (list t))

(defglobal *seen-threads* nil)

(sb-alien::define-alien-callback testcb int ((arg1 c-string) (arg2 double))
  (let ((cell (assoc sb-thread:*current-thread* *seen-threads*)))
    (unless cell
      (let* ((thr sb-thread:*current-thread*)
             (string
               (format nil "~s from ~s (~@[TID ~d~] Pthread ~x)~%"
                       arg1 thr
                       (or #+linux (sb-thread::thread-os-tid thr))
                       (sb-thread::thread-primitive-thread thr))))
        (sb-sys:with-pinned-objects (string)
          (sb-unix:unix-write 1 (sb-sys:vector-sap string) 0 (length string)))
        (setq cell (cons thr (1- (floor arg2))))
        (atomic-push cell *seen-threads*)))
    (assert (eql (coerce (incf (cdr cell)) 'double-float)
                 arg2)))
  0)

(defglobal *keepon* t)
(defglobal *n-gcs* 0)

(defun f (n-trials n-threads n-calls)
  (setq *n-gcs* 0)
  (let ((thr
          (sb-thread:make-thread
           (lambda()
             (loop
               (gc)
               (incf *n-gcs*)
               (sleep .001)
               (sb-thread:barrier (:read))
               (if (not *keepon*) (return)))))))
    (dotimes (i n-trials)
      (setq *keepon* t)
      (alien-funcall (extern-alien "call_thing_from_threads"
                                   (function int system-area-pointer int int))
                     (alien-sap testcb) n-threads n-calls)
      (format t "GC'd ~d times~%" *n-gcs*)
      (setq *keepon* nil)
      (sb-thread:barrier (:write))
      (sb-thread:join-thread thr))))

(with-test (:name :call-me-from-many-threads-and-gc)
  ;; two trials, 5 threads, 40 calls each
  (f 2 5 40)
  ;; one trial, 10 threads, 10 calls
  (f 1 10 10))
