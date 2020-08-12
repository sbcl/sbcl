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

;;; This reliably fails with two crashes (both in the same run) on FreeBSD:
;;;  - garbage_collect: no SP known for thread 0x802bea000 (OS 34367133952)
;;;  - failed AVER: (NOT (SB-THREAD::AVL-FIND ADDR SB-THREAD::OLD))

#+(or (not sb-thread) freebsd) (sb-ext:exit :code 104)

#+win32
(with-scratch-file (solib "dll")
  (sb-ext:run-program "gcc"
                      `("-shared" "-o" ,solib "fcb-threads.c")
                      :search t)
  (sb-alien:load-shared-object solib))

#-win32
(if (probe-file "fcb-threads.so")
    ;; Assume the test automator built this for us
    (load-shared-object (truename "fcb-threads.so"))
    ;; Otherwise, write into /tmp so that we never fail to rebuild
    ;; the '.so' if it gets changed, and assume that it's OK to
    ;; delete a mapped file (which it is for *nix).
    (with-scratch-file (solib "so")
      (sb-ext:run-program "/bin/sh"
                          `("run-compiler.sh" "-sbcl-pic" "-sbcl-shared"
                            "-o" ,solib "fcb-threads.c"))
      (sb-alien:load-shared-object solib)))

(defglobal *counter* 0)
(declaim (fixnum *counter*))
(defglobal *ok* (list t))

(defglobal *seen-threads* nil)

;;; A variable commissioned by the department of needless and unnecessary redundancy department
(defglobal *print-greetings-and-salutations* (or #+linux t))

(sb-alien::define-alien-callback testcb int ((arg1 c-string) (arg2 double))
  (let ((cell (assoc sb-thread:*current-thread* *seen-threads*))
        (result (floor (* (length arg1) arg2))))
    (unless cell
      (let* ((thr sb-thread:*current-thread*)
             (string
               (format nil "~s,~f from [~@[TID ~d ~]C-thread ~x] ~s => ~x~%"
                       arg1 arg2
                       (or #+linux (sb-thread::thread-os-tid thr))
                       (sb-thread::thread-primitive-thread thr)
                       thr result)))
        ;; This WRITE kind of has to stay here for timing purposes -
        ;; With it, we get >100 GCs, without it only 3 or 4,
        ;; and the intent of the test is to exercise GC and foreign
        ;; calbacks together, which was formerly bug prone.
        (when *print-greetings-and-salutations*
          #+win32 (progn (write-string string) (force-output))
          #-win32 (sb-sys:with-pinned-objects (string) ; avoid interleaved output this way
                    (sb-unix:unix-write 1 (sb-sys:vector-sap string) 0 (length string))))
        (setq cell (cons thr (1- (floor arg2))))
        (atomic-push cell *seen-threads*)))
    (assert (eql (coerce (incf (cdr cell)) 'double-float)
                 arg2))
    result))

(defglobal *keepon* t)
(defglobal *n-gcs* 0)

(defun f (n-trials n-threads n-calls enable-gcing)
  (setq *n-gcs* 0)
  (let ((thr
         (when enable-gcing
           (sb-thread:make-thread
            (lambda()
              (loop
               (gc)
               (incf *n-gcs*)
               (sleep .0001)
               (sb-thread:barrier (:read))
               (if (not *keepon*) (return))))))))
    (dotimes (i n-trials)
      (setq *keepon* t)
      (with-alien ((testfun (function int system-area-pointer int int)
                            :extern "call_thing_from_threads"))
        (assert (eql (alien-funcall testfun (alien-sap testcb) n-threads n-calls)
                     1)))
      (setq *keepon* nil)
      (sb-thread:barrier (:write))
      #+darwin
      (with-alien ((count int :extern "sigwait_bug_mitigation_count"))
        (when (plusp count)
          (format t "Bug mitigation strategy applied ~D time~:P~%" count)
          (setf count 0)))
      (when thr
        (sb-thread:join-thread thr)
        (format t "GC'd ~d times~%" *n-gcs*)))))

(with-test (:name :call-me-from-1-thread-no-gc
                  :skipped-on (or :interpreter))
  ;; smoke test and no GCing
  (setq *print-greetings-and-salutations* t)
  (f 1 1 1 nil)
  (setq *print-greetings-and-salutations* nil))

(with-test (:name :call-me-from-many-threads-and-gc
                  :skipped-on (or :interpreter :win32))
  ;; two trials, 5 threads, 40 calls each
  (f 2 5 40 t)
  ;; one trial, 10 threads, 10 calls
  (f 1 10 10 t))

;;; The next test hasn't been made to run on windows, but should.
#+win32 (exit :code 104)

;;; Check that you get an error trying to join a foreign thread
(defglobal *my-foreign-thread* nil)
(sb-alien::define-alien-callback tryjointhis int ()
  (setq *my-foreign-thread* sb-thread:*current-thread*)
  (dotimes (i 10)
    (write-char #\.) (force-output)
    (sleep .01))
  0)

(defun tryjoiner ()
  (setq *my-foreign-thread* nil)
  (sb-int:dx-let ((pthread (make-array 1 :element-type 'sb-vm:word)))
    (alien-funcall
     (extern-alien "pthread_create"
                   (function int system-area-pointer unsigned
                             system-area-pointer unsigned))
     (sb-sys:vector-sap pthread) 0 (alien-sap tryjointhis) 0)
    (format t "Alien pthread is ~x~%" (aref pthread 0))
    (let (found)
      (loop
        (setq found *my-foreign-thread*)
        (when found (return))
        (sleep .05))
      (format t "Got ~s~%" found)
      (let ((result (handler-case (sb-thread:join-thread found)
                      (sb-thread:join-thread-error () 'ok))))
        (when (eq result 'ok)
          ;; actually join it to avoid resource leak
          (alien-funcall
           (extern-alien "pthread_join" (function int unsigned unsigned))
           (aref pthread 0)
           0))
        (format t "Pthread joined ~s~%" found)
        result))))

(with-test (:name :try-join-foreign-thread)
  (assert (eq (tryjoiner) 'ok)))

