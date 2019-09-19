
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

(sb-int:encapsulate
 'sb-thread::new-lisp-thread-trampoline
 'finalizer-bug
 (lambda (f thread semaphore userfun args)
   (funcall f thread semaphore
            (lambda (&rest args)
              (sleep .1)
              (apply userfun args))
            args)))

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
(sb-impl::deinit)
(sb-impl::reinit)
