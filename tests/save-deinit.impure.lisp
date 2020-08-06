
;;; Bug discovered by users of FriCAS on a single-core virtual machine.
;;; I was able to replicate it exactly as described.
;;; Prior to the fix:
;;;  ./sbcl --load save-deinit.impure.lisp
;;;  While evaluating the form starting at line 30, column 0
;;;    of #P"/tmp/sbcl-sbcl/tests/save-deinit.impure.lisp":
;;;
;;;  debugger invoked on a SB-IMPL::SAVE-WITH-MULTIPLE-THREADS-ERROR in thread
;;;  #<THREAD "main thread" RUNNING {1001038523}>:
;;;    Cannot save core with multiple threads running.
;;;
;;;    Interactive thread (of current session):
;;;      #<THREAD "main thread" RUNNING {1001038523}>
;;;
;;;    Other thread:
;;;      #<THREAD "finalizer" RUNNING {10022A0073}>

;;; The test below is sufficient to show that it has been fixed.

#-sb-thread (exit :code 104)

;;; The test needs to purposely inject a small delay into new lisp thread creation
;;; so it wraps an encapsulation on some piece of primordial thread code.
;;; It formerly did that on the new thread trampoline, which is not the best place,
;;; because I don't want to cons a &REST arg nor know the exact signature in this test.
;;; Encapsulating INIT-THREAD-LOCAL-STORAGE works, but we have to make sure that
;;; *DEADLINE* has been initialized before calling SLEEP.
(defglobal *started-threads* nil)
(sb-int:encapsulate
 'sb-thread::init-thread-local-storage
 'finalizer-bug
 (compile nil '(lambda (f me)
                (push me *started-threads*)
                (prog1 (funcall f me) (sleep .05)))))

(finalize (cons 1 'foo)
          (lambda () (format t "~&finalizer1~%")))
(finalize (cons 2 'foo)
          (lambda () (format t "~&finalizer2~%")))

(setq sb-ext:*save-hooks* (list #'gc))

;;; DEINIT will invoke the save hooks which should cause post-GC
;;; to start the finalizer thread. The finalizer thread has a brief
;;; delay purposely induced before its main body. Therefore it won't
;;; have assigned *CURRENT-THREAD* into *FINALIZER-THREAD* until after
;;; FINALIZER-THREAD-STOP has returned. Therefore the test in DEINIT
;;; will see that thread in all threads, and will have to deal with it.
(let (sb-impl::*streams-closed-by-slad*)
  (sb-impl::deinit)
  (sb-impl::restore-fd-streams)
  (sb-impl::reinit nil))

;;; I think this test would be better as a shell test,
;;; so that we actually call SAVE-LISP-AND-DIE to see that it works.
;;; Why oh why didn't I do that???
(assert (= 1 (length (sb-thread:list-all-threads)))) ; there's 1 now
(assert *started-threads*) ; but there was more than 1
