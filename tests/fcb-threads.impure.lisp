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

#+(or (not sb-thread) freebsd) (invoke-restart 'run-tests::skip-file)

(setf (generation-number-of-gcs-before-promotion 0) 5)
(setf (generation-number-of-gcs-before-promotion 1) 3)

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
                            "-o" ,solib "fcb-threads.c")
                          :output t :error :output)
      (sb-alien:load-shared-object solib)))

;;;; Just exercise a ton of calls from 1 thread
(define-alien-callable perftestcb int () 0)
(defun trivial-call-test (n)
  (with-alien ((testfun (function int system-area-pointer int) :extern "minimal_perftest"))
    (alien-funcall testfun (alien-sap (alien-callable-function 'perftestcb)) n)))


;;; When compiled with APROF, the truly astounding amount of space taken up not by objects
;;; but just closing the open regions in unregister_thread becomes evident:
#|
10 (of 150000 max) profile entries consumed, 367 GCs done
       %        Bytes        Count    Function
 -------  -----------    ---------    --------
  99.7    19590256480       600000    SB-VM::FILLER - SB-VM::FILLER
   0.1       28800144       600003    SB-THREAD::AVLNODE - SB-THREAD::AVLNODE
   0.1       28800000       200000    SB-THREAD::MAKE-FOREIGN-THREAD - SB-THREAD:FOREIGN-THREAD
  00.0        6400000       200000    SB-THREAD:MAKE-MUTEX - SB-THREAD:MUTEX
  00.0        3200000       200000    SB-THREAD::SYS-TLAB-LIST
  00.0           5840          365    (FLET "WITHOUT-GCING-BODY-" :IN SB-KERNEL:SUB-GC) - LIST
  00.0             16            1    SB-THREAD::%ENROLL-NEW-THREADS - LIST
 =======  ===========
 100.0    19657462480
|#
;;; Something is very off about these numbers though. If there are 20,000 thread creation/
;;; destructions, and each thread wastes all of its 4 TLABs, that should be 132KiB per thread,
;;; for roughly 2.6GB of waste in total. How are we seing nearly 8x that?
(with-test (:name :trivial-call-test)
 (cond #+(and x86-64 sb-thread (not win32))
       ((> (sb-c:policy sb-c::*policy* sb-c:instrument-consing) 0)
        (sb-aprof:aprof-run #'trivial-call-test :arguments '(200000)))
       (t
        (time (trivial-call-test 200000)))))

;;;;
(defglobal *counter* 0)
(declaim (fixnum *counter*))
(defglobal *ok* (list t))

(defglobal *seen-threads* nil)

;;; A variable commissioned by the department of needless and unnecessary redundancy department
(defglobal *print-greetings-and-salutations* (or #+linux t))

(defglobal *semaphore* nil)
(define-alien-callable testcb int ((arg1 c-string) (arg2 double))
  (when *semaphore* (sb-thread:signal-semaphore *semaphore*))
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
  (dotimes (trialno n-trials)
    (setq *n-gcs* 0)
    (setq *semaphore* (sb-thread:make-semaphore))
    (let ((watchdog-thread
           (sb-thread:make-thread
            (lambda ()
              ;; Each trial, when it works, takes between .01 to .4 sec
              ;; (which is a tremendous spread),
              ;; so after 20 seconds (50x more than needed), say it failed.
              (let ((result
                     (sb-thread:wait-on-semaphore *semaphore*
                                                  :n (* n-threads n-calls)
                                                  ;; :timeout 20
                                                  )))
                (if result
                    (format t "OK!~%")
                    (sb-sys:os-exit 1))))))
          (gc-thr
           (when enable-gcing
             (sb-thread:make-thread
              (lambda()
                (loop
                 (gc)
                 (incf *n-gcs*)
                 (sleep .0005)
                 (sb-thread:barrier (:read))
                 (if (not *keepon*) (return)))))))
          (start (get-internal-real-time)))
      (setq *keepon* t)
      (with-alien ((testfun (function int system-area-pointer int int)
                            :extern "call_thing_from_threads"))
        (assert (eql (alien-funcall testfun (alien-sap (alien-callable-function 'testcb)) n-threads n-calls)
                     1)))
      (setq *keepon* nil)
      (sb-thread:barrier (:write))
      (let ((stop (get-internal-real-time)))
        (sb-thread:join-thread watchdog-thread)
        (when gc-thr
          (sb-thread:join-thread gc-thr)
          (format t "Trial ~d: GC'd ~d times (Elapsed=~f sec)~%"
                  (1+ trialno) *n-gcs*
                  (/ (- stop start) internal-time-units-per-second)))))))

(with-test (:name :call-me-from-1-thread-no-gc
                  :skipped-on (or :interpreter))
  ;; smoke test and no GCing
  (setq *print-greetings-and-salutations* t)
  (f 1 1 1 nil)
  (setq *print-greetings-and-salutations* nil))

(with-test (:name :call-me-from-many-threads-and-gc
            :skipped-on (or :interpreter (and :x86 :win32)))
  ;; two trials, 5 threads, 40 calls each
  (f 2 5 40 t)
  ;; one trial, 10 threads, 10 calls
  (f 1 10 10 t)
  ;; Crank the number of trials up if you're trying to investigate
  ;; flakes in this test. The larger number of calls is usually
  ;; what gets it to fail.
  ;; 5 trials, 5 threads, 200 calls each
  (f 5 5 200 t))

;;; The next test hasn't been made to run on windows, but should.
#+win32 (invoke-restart 'run-tests::skip-file)

;;; Check that you get an error trying to join a foreign thread
(defglobal *my-foreign-thread* nil)
(define-alien-callable tryjointhis int ()
  (setq *my-foreign-thread* sb-thread:*current-thread*)
  (dotimes (i 10)
    (write-char #\.) (force-output)
    (sleep .01))
  0)

(defun tryjoiner ()
  (setq *my-foreign-thread* nil)
  (sb-int:dx-let ((pthread (make-array 1 :element-type 'sb-vm:word)))
    (sb-sys:with-pinned-objects (pthread)
      (alien-funcall
       (extern-alien "pthread_create"
                     (function int system-area-pointer unsigned
                               system-area-pointer unsigned))
       (sb-sys:vector-sap pthread) 0 (alien-sap (alien-callable-function 'tryjointhis)) 0)
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
            (assert (zerop
                     (alien-funcall
                      (extern-alien "pthread_join" (function int unsigned unsigned))
                      (aref pthread 0)
                      0))))
          (format t "Pthread joined ~s~%" found)
          result)))))

(with-test (:name :try-join-foreign-thread)
  (assert (eq (tryjoiner) 'ok)))

