(in-package "SB-LFL")

#-(and sb-thread (or arm64 ppc x86 x86-64)) (sb-ext:exit :code 104)

;;;; These functions are for examining GC behavior.

(defun lfl-nth (n list)
  (let ((node (%node-next (list-head list))))
    (dotimes (i n node)
      (setq node (node-next node)))))

;;; For testing (especially the garbage collector), perform the first CAS
;;; operation but not the second CAS of the deletion algorithm.
(defun logical-delete (n list)
  (let* ((node (lfl-nth n list))
         (succ (%node-next node)))
    (unless (fixnump succ)
      (with-pinned-objects (succ)
        (cas (%node-next node) succ (make-marked-ref succ)))))
  list)

(defvar *lfl*)
(defvar *l*)
(flet ((show (node step when)
         (format t "~a: " when)
         (loop (format t "~x" (get-lisp-obj-address node))
               (unless (setq node (funcall step node)) (return))
               (format t " - "))
         (terpri)))
  (defun makelist (n)
    (setq *l* nil)
    (dotimes (i n) (push (cons i (format nil "~r" i)) *l*))
    (show *l* #'cdr "init")
    (setq *l* (nreverse *l*))
    (show *l* #'cdr "rev ")
    (gc)
    (show *l* #'cdr "GCed"))
  (defun makelflist (n &optional show)
    (setq *lfl* (new-lockfree-list))
    (dotimes (i n) (lfl-insert/fixnum *lfl* (* i n) (format nil "~r" i)))
    (when show (show (list-head *lfl*) #'node-next "init"))
    (gc)
    (when show (show (list-head *lfl*) #'node-next "GCed"))
    (logical-delete 1 *lfl*)
    (logical-delete 4 *lfl*)
    (gc :gen 2)
    (when show (show (list-head *lfl*) #'node-next "del "))))

(test-util:with-test (:name :lockfree-list-gc-correctness)
  ;; Enable heap validity tester
  (setf (sb-alien:extern-alien "verify_gens" char) 0)
  ;; Create a small list and perform logical deletion of 2 nodes
  (makelflist 10)
  (gc)) ; Verify no post-gc crash

(test-util:with-test (:name :lockfree-list-finalize-deletion)
  ;; Check that save-lisp-and-die can remove deleted nodes
  (let ((list (new-lockfree-list)))
    (lfl-insert/fixnum list 4 "four")
    (lfl-insert/fixnum list 5 "five")
    (logical-delete 0 list)
    (logical-delete 1 list)
    (finalize-deletion list)
    (let* ((node (list-head list))
           (next (node-next node)))
      (assert (eq next (list-tail list))))))

;;; These functions are for comparing the running time of a lock-based
;;; implementation versus lockfree.

(defun new-synchronized-list ()
  (list (sb-thread:make-mutex)))
(defun list-search (list key)
  (let* (left
         (this list)
         (next (cdr this)))
    (loop (setq left this this next)
          (when (null this)
            (return))
          (setq next (cdr this))
          (unless (< (truly-the fixnum (caar this)) key)
            (return)))
    (values this left)))
(defun locked-insert (list key value)
  (sb-thread:with-mutex ((car list))
    (multiple-value-bind (successor predecessor) (list-search list key)
      (let ((new (cons (cons key value) successor)))
        (rplacd predecessor new)))
    list))
(defun locked-delete (list key)
  (sb-thread:with-mutex ((car list))
    (multiple-value-bind (this predecessor) (list-search list key)
      (when (and this (= (caar this) key))
        (rplacd predecessor (cdr this)))))
  list)

;;;

(defglobal *worklist* nil)
(defmacro smoketest-macro (constructor inserter deleter)
  `(let ((list ,constructor)
         (threads))
     (assert (<= n-threads 50))
     (let ((max (* n-threads n-items)))
       (dotimes (i n-threads)
         (push (sb-thread:make-thread
                (lambda (my-items &aux (ct 0))
                  (loop
                   (let ((val (atomic-pop *worklist*)))
                     (unless val (return))
                     (setf (aref my-items ct) val)
                     (,inserter list val (- val))
                     (incf ct)
                     (when (oddp ct)
                       (let ((item-to-delete
                              (aref my-items (floor ct 2))))
                         (,deleter list item-to-delete))))))
                :arguments (make-array max))
               threads)))
     (dolist (thr threads) (sb-thread:join-thread thr))
     list))

(defun smoketest-lockfree (n-threads n-items)
  (smoketest-macro (new-lockfree-list) lfl-insert/fixnum lfl-delete/fixnum))
(defun smoketest-locked (n-threads n-items)
  (smoketest-macro (new-synchronized-list)
                   locked-insert locked-delete))

(defun primitive-benchmark (&key (n-trials 3) (n-threads 10) (n-items 500) print)
  (let ((best 0))
    (dotimes (trial n-trials best)
      (let* ((max (* n-threads n-items))
             (worklist (test-util:shuffle (test-util:integer-sequence max)))
             (random-state (make-random-state)))
        (let ((rt0 (get-internal-real-time))
              (rt1)
              (rt2))
          (setq *worklist* (copy-list worklist)
                *random-state* (make-random-state random-state))
          (smoketest-locked n-threads n-items)
          (setq rt1 (get-internal-real-time))
          (setq *worklist* (copy-list worklist)
                *random-state* (make-random-state random-state))
          (smoketest-lockfree n-threads n-items)
          (setq rt2 (get-internal-real-time))
          (let* ((et-locked (- rt1 rt0))
                 (et-lockfree (- rt2 rt1))
                 (ratio (/ et-locked et-lockfree)))
          (when print
            (format t "elapsed-times: locked=~d and lockfree=~d ratio=~f~%"
                    et-locked et-lockfree ratio))
          (setq best (max ratio best))))))))

(test-util:with-test (:name :lockfree-list-performance)
  (let ((cpus
         (max 1
              #-win32 (sb-alien:alien-funcall
                       (sb-alien:extern-alien "sysconf"
                                              (function sb-alien:long sb-alien:int))
                       sb-unix::sc-nprocessors-onln)
              #+win32 (sb-alien:extern-alien "os_number_of_processors" sb-alien:int))))
    (assert (> (primitive-benchmark) (log cpus 2)))))
