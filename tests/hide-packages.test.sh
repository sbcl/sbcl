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
         (rename-package name (concatenate 'string "HIDDEN-" name)))))))

(gc :gen 7)
(setq *weak-ptrs* (remove-if-not #'weak-pointer-value *weak-ptrs*))
(when *weak-ptrs* (search-roots *weak-ptrs* :print :verbose :criterion :static))
(assert (null *weak-ptrs*))
(format t "Package hiding test 1: PASS~%")
EOF

### Test 2: assert that no string anywhere in the heap is STRING= to the
### name of a system package. Also no uses of undesired symbols
### in lieu of strings. i.e. no (FIND-PACKAGE 'sb-vm)

run_sbcl <<\EOF

;; This test should pass everywhere now
#+sb-devel (exit)

;;; Does not pass with interpreter
(setq sb-ext:*evaluator-mode* :compile)

(fill sb-impl:+internal-features+ :foo)
(fill *features* :foo)
(fill sb-fasl::*features-potentially-affecting-fasl-format* :foo)

;;; If the keyword package sized up, then its former backing vector
;;; became tenured garbage. Fullcgc can remove it
(gc :gen 7)

(defmacro explain (form)
  `(let ((ok ,form))
     ;; Easily see which heap objects were accepted by uncommenting
     #+nil
     (when ok (format t "~&Allowed ~S -> ~S~%" (type-of this) that))
     ok))

(defun points-to-symbol-ok (this that)
  (explain
  (or (eq this that) ; symbol-value
      ;; :SB-THREAD, :SB-UNICODE, :SB-EVAL are pointed to by the
      ;; keyword package
      (and (eq this (sb-impl::symtbl-cells
                     (sb-impl::package-external-symbols
                      (find-package "KEYWORD"))))
           (keywordp that)))))

(defun points-to-string-ok (this that)
  (explain
  (or ;; package can point to its own name
      (and (packagep this) (eq (package-name this) that))
      ;; name table can point to strings
      (and (simple-vector-p this)
           (member this (load-time-value
                         (mapcar #'sb-impl::package-keys (list-all-packages)))))
      ;; keyword points to its name
      (and (keywordp this) (eq (symbol-name this) that)))))

(defvar *wps* nil)
(defun check-undesired (object pointee undesired-symbols undesired-strings hashes)
  (when (or (and (symbolp pointee)
                 (find pointee (the simple-vector undesired-symbols))
                 (not (points-to-symbol-ok object pointee)))
            (and (stringp pointee)
                 (find (sxhash pointee) (the simple-vector hashes))
                 (find pointee (the simple-vector undesired-strings) :test #'string=)
                 (not (points-to-string-ok object pointee))))
    (push (make-weak-pointer object) *wps*)))

(sb-int:dx-let ((undesired-symbols (make-array 4)) ; overestimate
		(undesired-strings (make-array 100)) ; overestimate
		(hashes (make-array 100)))
  ;; Compute symbols we don't want to see
  (let ((i -1))
    (dolist (tail '("EVAL" "THREAD" "UNICODE"))
      (let ((list (find-all-symbols
		   (concatenate 'string "SB-" tail))))
	(dolist (symbol list)
	  (setf (aref undesired-symbols (incf i)) symbol))
	(fill list nil))))
  ;; Compute strings we don't want to see
  (let ((n 0))
    (dolist (pkg (list-all-packages))
      (let ((name (package-name pkg)))
	(when (and (char= (char name 0) #\S)
		   (char= (char name 1) #\B))
	  (setf (aref undesired-strings n) name)
	  (incf n))))
    (sb-kernel:%shrink-vector undesired-strings n)
    (sb-kernel:%shrink-vector hashes n)
    (dotimes (i n)
      (setf (aref hashes i) (sxhash (aref undesired-strings i)))))
  ;; flush token buffer of package qualifier (part preceding the #\:)
  ;; which may be some system package after reading this test.
  (print (read-from-string "keyword:x") (make-broadcast-stream))
  ;; Now walk all heap objects
  (macrolet ((visit (that)
	       `(check-undesired
		 this ,that undesired-symbols undesired-strings hashes)))
    (sb-vm:map-allocated-objects
     (lambda (this type size)
       (declare (ignore type size))
       (sb-vm::do-referenced-object (this visit)))
    :all)))

;;(sb-ext:search-roots *wps* :print :verbose)
(assert (null *wps*))
(format t "Package hiding test 2: PASS~%")
EOF

exit $EXIT_TEST_WIN
