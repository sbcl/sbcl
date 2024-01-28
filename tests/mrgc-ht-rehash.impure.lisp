
#-mark-region-gc (invoke-restart 'run-tests::skip-file)

(setf (extern-alien "check_hash_tables" int) 1)
;; This test is much LESS LIKELY to fail when the hash-table is pinned.
;; Allowing table movement during the COMPILE-FILE operation by reading
;; through an extra seemingly pointless indirection makes it fail faster.
(defun get-table (holder) (car holder))
(compile 'get-table)

(defun run-biggybiggy ()
  (let ((holder (list (make-hash-table :test 'eq)))
        (n-iter 0))
    (loop
      (format t "~&Trial ~D: so far have ~D keys~%" (incf n-iter)
              (hash-table-count (get-table holder)))
      (dotimes (random-iterations (random 10000))
        (let* ((tbl (get-table holder))
               (random-thing (cons (hash-table-count tbl) 'foo)))
          (setf (gethash random-thing tbl) (hash-table-count tbl))))
      ;; This COMPILE-FILE conses about 46 megabytes which seems to do the trick.
      ;; Indeed it very consistently fails for me by trial 60,
      ;; but might fail sooner.
      (compile-file "../src/pcl/walk")
      ;; Expect to fail before this count is reached
      (if (> (hash-table-count (get-table holder)) 1000000) (return)))))
;(compile 'run-biggybiggy) ; use the interpreter. It conses more, which helps actually
#-sbcl (run-biggybiggy)
;;; Until the bug is fixed, we expect to see (approximately):
;;;    Trial 28: so far have 144677 keys
;;;    ; compiling file "../src/pcl/walk.lisp" (written 28 JAN 2024 02:35:17 PM):
;;;    Verifying hashtables
;;;    failed to find key 1002799327 value 3d784 fuzzed_hash=3520c45 ivmask=3ffff bucket=20c45 index=78880
;;;     index=78880 key=10044b47d7 next=0
;;;     index=0 key=4714a next=0
;;;    fatal error encountered in SBCL pid 3350552 tid 3350552:
;;;    ht should be marked for rehash
;;; Putting a WITH-TEST made this fail only after 100 trials.
;;; That's kinda annoying slow. So unfortunately we can't enable this test.
;;; And it needed 178 trials to fail under parallel-exec.
#+nil
(test-util:with-test (:name :fail-to-rehash :fails-on :sbcl)
  (run-biggybiggy))
