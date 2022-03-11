#+(or (not sb-thread) win32) (sb-ext:exit :code 104)

;;; Not an exactly an "exit hang" test, but there was a different hang
;;; regarding concurrent JOIN-THREAD on 1 thread.
;;; Even though POSIX threads would consider this to be undefined behavior with
;;; its thread abstraction, it's not undefined behavior in SBCL (for now)
;;; though I do think it's slightly suspicious to depend on this.
(with-test (:name :concurrent-join-thread)
  (let* ((other-guy (sb-thread:make-thread #'sleep :arguments .2 :name "sleepyhead"))
         (joiners
          (loop repeat 4
                collect (sb-thread:make-thread #'sb-thread:join-thread
                                               :arguments other-guy))))
    ;; The joiners should all return
    (mapc 'sb-thread:join-thread joiners)))

;;; This uses the same C source file as fcb-threads.
;;; This is OK in the parallel test runner because WITH-SCRATCH-FILE
;;; includes the PID in the temp file name.
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

;;; Final test: EXIT does not lock up due to (simulated) C++ destructors
;;; or free() or most anything else involved in stopping the main thread.
;;; The point of the test is to mock a Lisp thread that uses foreign code
;;; that uses malloc and free or equivalent from C++.
;;; The behavior being tested is the effect of SB-THREAD:ABORT-THREAD on
;;; a thread that happened to be just at that moment in the foreign code.
;;; We can't - or don't need to - exactly replicate the behavior
;;; of doing a lot of memory allocation. All we need to demonstrate is
;;; that we won't interrupt a malloc() or free().
(defglobal *should-i-keep-going* t)
(defun mess-around-with-foreign-calls ()
  ;; In reality the thread would not permanently own the lock, but this is the
  ;; simplest way to simulate the random occurrence that it does own the lock
  ;; exactly when terminated.
  ;; So make it own the lock forever unless politely (i.e. not forcibly) terminated.
  (alien-funcall (extern-alien "acquire_a_global_lock" (function void)))
  (loop (sb-thread:barrier (:read))
        (unless *should-i-keep-going* (return))
        (sleep .75))
  (format *error-output* "~&Worker thread politely exiting~%")
  (alien-funcall (extern-alien "release_a_global_lock" (function void))))

(sb-thread:make-thread #'mess-around-with-foreign-calls)

(push (compile nil
               '(lambda ()
                 (format t "~&Invoked exit hook~%")
                 (setq *should-i-keep-going* nil)))
      *exit-hooks*)

;;; The actual code under test involved C++ destructors that are
;;; interposed between our call to OS-EXIT and the OS call per se.
;;; Acquiring a globally shared mutex in OS-EXIT simulates that.
(sb-int:encapsulate
 'sb-sys:os-exit
 'simulate-c++-destructors
 (lambda (realfun code &key abort)
   (format t "~&Enter OS-EXIT ~s ~s~%" code abort)
   (alien-funcall (extern-alien "acquire_a_global_lock" (function void)))
   (alien-funcall (extern-alien "release_a_global_lock" (function void)))
   (funcall realfun code :abort abort)))

;;; Give ourselves 3 seconds to exit.
(alien-funcall (extern-alien "prepare_exit_test" (function void int)) 3)
(setq sb-ext:*forcibly-terminate-threads-on-exit* nil)
(exit :code 104)
