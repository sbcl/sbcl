(with-test (:name :compile-file-error-position-reporting
            :serial t)
  (dolist (input '("data/wonky1.lisp" "data/wonky2.lisp" "data/wonky3.lisp"))
    (let ((expect (with-open-file (f input) (read f))))
      (assert (stringp expect))
      (let ((err-string (with-output-to-string (*error-output*)
                          (compile-file input :print nil
                                              :output-file
                                              (scratch-file-name "fasl")))))
        (assert (search expect err-string))))))

#-sb-thread (invoke-restart 'run-tests::skip-file)

(unlock-package "SB-INT")
(unlock-package "SB-IMPL")
(unlock-package "CL")
(rename-package "COMMON-LISP" "COMMON-LISP" '("CL"  "SB-XC"))

(defun slurp (filename)
  (with-open-file (stream filename :element-type '(unsigned-byte 8))
    (let ((res (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence res stream)
      res)))

;;; Assert that concurrent threads doing the same thing
;;; produce identical output.
(with-test (:name :parallel-compile-file)
  (with-scratch-file (thr1-out "fasl")
  (with-scratch-file (thr2-out "fasl")
  (with-scratch-file (thr3-out "fasl")
    (flet ((work (result)
             (let* ((*standard-output* (make-string-output-stream))
                    (*error-output* *standard-output*))
               (compile-file "../src/code/primordial-extensions"
                             :output-file result)
               (get-output-stream-string *standard-output*))))
      (dotimes (i 1)
        (let ((thr1 (sb-thread:make-thread #'work :arguments (list thr1-out) :name "A"))
              (thr2 (sb-thread:make-thread #'work :arguments (list thr2-out) :name "B"))
              (thr3 (sb-thread:make-thread #'work :arguments (list thr3-out) :name "C")))
          ;; There's nothing really that can be asserted about asynchronous backtraces.
          ;; This call exists solely to prove that the BACKTRACE-ALL-THREADS no longer
          ;; calls lose() if libunwind is absent. See the comment below for an example
          ;; of the expected output.
          #+x86-64 (let ((bts (sb-debug:backtrace-all-threads)))
                     (declare (ignorable bts))
                     #+nil (print bts))
          (let ((string1 (sb-thread:join-thread thr1))
                (string2 (sb-thread:join-thread thr2))
                (string3 (sb-thread:join-thread thr3)))
            (declare (ignorable string1 string2 string3))
            #+nil
            (progn
              (format t "1:~%~A~%" string1)
              (format t "2:~%~A~%" string2)
              (format t "3:~%~A~%" string3)))
          (let ((bytes1 (slurp thr1-out))
                (bytes2 (slurp thr2-out))
                (bytes3 (slurp thr3-out)))
            (assert (equalp bytes1 bytes2))
            (assert (equalp bytes2 bytes3)))
          (delete-file thr1-out)
          (delete-file thr2-out)
          (delete-file thr3-out))))))))

#|
BACKTRACE-ALL-THREADS returned this output in one run:
 (#<SB-THREAD:THREAD tid=3269428 RUNNING {1000FAAC43}>
  . "0: foreign function: fstatat
1: UNIX-STAT
2: %QUERY-FILE-SYSTEM
3: QUERY-FILE-SYSTEM
4: VERIFY-SOURCE-FILE
5: COMPILE-FILE
6: (FLET WORK IN DROP-THRU-TAG-2)
7: (FLET BODY IN RUN)
8: (FLET WITHOUT-INTERRUPTS-BODY- IN RUN)
9: (FLET BODY IN RUN)
10: (FLET WITHOUT-INTERRUPTS-BODY- IN RUN)
11: RUN
12: foreign function: call_into_lisp_
13: foreign function: funcall1
14: foreign function: new_thread_trampoline
")
 (#<SB-THREAD:THREAD tid=3269429 RUNNING {1000FAADB3}>
  . "0: COMPARE-COMPONENT
1: HASHSET-FIND
2: HASHSET-INSERT-IF-ABSENT
3: INTERN-PATHNAME
4: %PARSE-NAMESTRING
5: PARSE-NAMESTRING
6: PATHNAME
7: COMPILE-FILE-PATHNAME
8: COMPILE-FILE
9: (FLET WORK IN DROP-THRU-TAG-2)
10: (FLET BODY IN RUN)
11: (FLET WITHOUT-INTERRUPTS-BODY- IN RUN)
12: (FLET BODY IN RUN)
13: (FLET WITHOUT-INTERRUPTS-BODY- IN RUN)
14: RUN
15: foreign function: call_into_lisp_
16: foreign function: funcall1
17: foreign function: new_thread_trampoline
")

But in another run, the backtrace was unable to get past a libc function
that was compiled without frame-pointers apparently:
(#<SB-THREAD:THREAD tid=3273614 "A" RUNNING {1000FA7453}>
  . "0: foreign function: __open64
(no more frames)
")
|#
