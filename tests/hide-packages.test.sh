#!/bin/sh

. ./subr.sh

set -e

# +INTERNAL-FEATURES+ is a constant, so use a roundabout way of
# clobbering it that avoids a compiler warning and a runtime error.
CLOBBER_INTERNAL_FEATURES="(handler-bind ((simple-error #'continue))
  (sb-vm::close-thread-alloc-region)
  (alien-funcall (extern-alien \"move_rospace_to_dynamic\" (function void int)) 1)
  (when (sb-sys:find-foreign-symbol-address \"test_dirty_all_gc_cards\")
    (alien-funcall (extern-alien \"test_dirty_all_gc_cards\" (function void))))
  (sb-kernel:set-symbol-global-value (eval ''sb-impl:+internal-features+) nil))"

### Apparently the finalizer thread must be stopped before running these tests or else
### it can crash due to the highly unorthodox use of move_rospace_to_dynamic().
### Somehow the finalizer thread dereferenced data from the R/O space
### after that space was reduced to 0 size. Amazingly the finalizer hadn't gotten out
### of its new_thread_trampoline nor assigned anything to 'current_thread'.
### parallel-exec was able to elicit about 3 failures in 20 prior to the fix.
###
### Backtrace from gdb:
### ...
### #8  0x000055d729cf8d0a in call_lossage_handler () at interr.c:116
### #9  0x000055d729cf9127 in corruption_warning_and_maybe_lose (
###     fmt=fmt@entry=0x55d729d2a660 "Received signal %d @ %lx in non-lisp tid %d, resignaling to a lisp thread.") at interr.c:202
### #10 0x000055d729cf97bc in resignal_to_lisp_thread (signal=signal@entry=11,
###     context=context@entry=0x7f9a3e216880) at interrupt.c:218
### #11 0x000055d729cf9843 in low_level_handle_now_handler (signal=11, info=0x7f9a3e2169b0,
###     void_context=0x7f9a3e216880) at interrupt.c:1927
### #12 <signal handler called>
### #13 new_thread_trampoline (arg=<optimized out>) at src/runtime/runtime.h:160
### #14 0x00007f9a3ed20d80 in start_thread (arg=0x7f9a3e217640) at pthread_create.c:481
### #15 0x00007f9a3eaf676f in clone () at ../sysdeps/unix/sysv/linux/x86_64/clone.S:95

### Test 1: assert that no package name string has a reference from any
### heap object aside from the package itself.
### Because SAVE-LISP-AND-DIE coalesces strings, this test provides confidence
### that no string anywhere in the heap is STRING= to a package name.
### The better test is Test 2 below, which only passes on x86[-64] as yet.

run_sbcl <<EOF
#+cheneygc (exit) ; not much can be done for cheney to make this pass
#+sb-devel (exit) ; doesn't pass either

#+sb-thread (sb-impl::finalizer-thread-stop)
(defvar *weak-ptrs* nil)
(progn
  ;; Remove features that are also names of packages.
  ;; This form needs to obfuscate the spellings of victim symbols, or else
  ;; they end up in the *SOURCE-INFO* for this file, which is pinned while
  ;; loading the file, which would enliven the symbols.
  $CLOBBER_INTERNAL_FEATURES
  (dolist (x '("EVAL" "THREAD" "UNICODE"))
    (let ((sym (find-symbol (concatenate 'string '(#\S #\B #\-) x) "KEYWORD")))
      (when sym
        (setf *features* (delete sym *features*))
        (push (make-weak-pointer sym) *weak-ptrs*)
        (unintern sym :keyword))))
  ;; Just clobber these all
  (setq sb-fasl::*features-potentially-affecting-fasl-format* nil)
  (sb-ext:without-package-locks
   (dolist (package (list-all-packages))
     (let ((name (package-name package)))
       (unless (or (string= name "COMMON-LISP")
                   (string= name "COMMON-LISP-USER")
                   (string= name "KEYWORD"))
         (push (make-weak-pointer name) *weak-ptrs*)
         (rename-package name (concatenate 'string "HIDDEN-" name))))))
  ;; The package name hashtable lazily removes keys. Force it do to do so now.
  (sb-impl::%rebuild-package-names sb-kernel::*package-names*))

(gc :gen 7)
(setq *weak-ptrs* (remove-if-not #'weak-pointer-value *weak-ptrs*))
(when *weak-ptrs* (search-roots *weak-ptrs* :print :verbose :criterion :static))
(assert (null *weak-ptrs*))
(format t "Package hiding test 1: PASS~%")
EOF

### Test 2: assert that no string anywhere in the heap is STRING= to the
### name of a system package.
run_sbcl <<EOF

;;; It would be nice to get this test to pass on all backends.
;;; That it doesn't pass doesn't mean the packages aren't hidden correctly.
;;; It just means the test is inadequate.
#-(or x86 x86-64) (exit)
#+sb-devel (exit)

;;; Does not pass with interpreter
(setq sb-ext:*evaluator-mode* :compile)
#+sb-thread (sb-impl::finalizer-thread-stop)

;;; Avoid accidental enlivenment of strings - the forms in this file must
;;; obscure the spellings of package name strings or else they can end up
;;; in the *SOURCE-INFO* which is live as long as this file is being read.
(defvar *pkg-prefix* '(#\S #\B #\-))
(dolist (x '("INT" "KERNEL" "VM"))
  (use-package (concatenate 'string *pkg-prefix* x)))

(defun rename-all-packages ()
  ;; Remove features that are also names of packages.
  $CLOBBER_INTERNAL_FEATURES
  (dolist (x '("EVAL" "THREAD" "UNICODE"))
    (let ((sym (find-symbol (concatenate 'string *pkg-prefix* x) "KEYWORD")))
      (when sym
        (setf *features* (delete sym *features*))
        (unintern sym :keyword))))
  ;; Just clobber these all
  (setq sb-fasl::*features-potentially-affecting-fasl-format* nil)
  (without-package-locks
   (dolist (package (list-all-packages))
     (let ((name (package-name package)))
       (rename-package name (concatenate 'string "HIDDEN-" name)))))
  ;; The package name hashtable lazily removes keys. Force it do to do so now.
  (sb-impl::%rebuild-package-names sb-kernel::*package-names*)
  (sb-sys:scrub-control-stack))

;;; Place all the strings we want to search for onto the stack.
;;; This little rearrangement here is a hack which causes the last item in
;;; 'packages' to be one that we're not interested in for purposes of the test.
;;; The final iteration of RECURSE seems to pin the string naming the package,
;;; which produces a reference to a string that we may want not to see.
(defglobal *undesired-strings*
  (mapcan (lambda (x)
	    (let ((s (package-name x)))
	      (unless (member s '("COMMON-LISP" "COMMON-LISP-USER" "KEYWORD")
			      :test #'string=)
		(list (coerce s 'simple-vector)))))
	  (list-all-packages)))

(flet ((collect-strings (print)
         (let (found-strings)
           (map-allocated-objects
            (lambda (obj type size)
              (declare (ignore type size))
              (when (and (stringp obj) (find obj *undesired-strings* :test #'equalp))
                (when print
                  (format t "Found string in g~d @ ~x ~S~%"
                            (generation-of obj) (get-lisp-obj-address obj) obj))
                (push (make-weak-pointer obj) found-strings)))
            :all)
           found-strings)))
  ;; First we expect to see a bunch of strings.
  (assert (collect-strings nil))
  ;; Do not remove the CL-USER package qualifier here - it's a trick to store
  ;; an allowed string into the reader's token buffer. Otherwise it would hold
  ;; "sb-kernel" which is the most recent package name read (above).
  ;; [Recall, it makes separate strings for the pieces before and after a ":"]
  (CL-USER::rename-all-packages)
  (gc :gen 7)
  ;; Then we expect NOT to see any matching strings.
  (assert (not (collect-strings t)))
  (format t "Package hiding test 2: PASS~%"))
EOF

exit $EXIT_TEST_WIN
