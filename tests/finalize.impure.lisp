#+interpreter (sb-ext:exit :code 104)

(defvar *tmp* 0.0) ; don't remove - used by the setq below
(defglobal *count* 0)
(declaim (fixnum *count*))

(defun foo (_)
  (declare (ignore _))
  nil)

(defglobal *maxdepth* 0)
;; Be gentler on 32-bit platforms
(defglobal *n-finalized-things* (or #+(and 64-bit (not sb-safepoint)) 20000 10000))
(defglobal *weak-pointers* nil)

#+sb-thread ; Check that we do want to start a thread, and it hasn't been started.
(assert (eq sb-impl::*finalizer-thread* t))

(defun makejunk (_)
  (declare (ignore _))
  (let ((x (gensym)))
    (push (make-weak-pointer x) *weak-pointers*)
    (finalize x (lambda ()
                  (setq *maxdepth*
                        (max sb-kernel:*free-interrupt-context-index*
                             *maxdepth*))
                  ;; cons 320K in the finalizer for #+64-bit,
                  ;; or 80K for #-64-bit
                  (setf *tmp* (make-list *n-finalized-things*))
                  (sb-ext:atomic-incf *count*)))
    x))
(compile 'makejunk)

(defun scrubstack ()
  (sb-int:dx-let ((b (make-array 20))) (eval b))
  (sb-sys:scrub-control-stack))

(defun run-consy-thing ()
  (dotimes (iter 10)
    (let ((junk (mapcar #'makejunk
                        (make-list (/ *n-finalized-things* 10)))))
      (setf junk (foo junk))
      (foo junk))
    (scrubstack)
    (gc :full t)))

; no threads - hope for the best with respect to conservative root retention
#-sb-thread (run-consy-thing)

#+sb-thread
(progn
  (sb-thread:join-thread (sb-thread:make-thread #'run-consy-thing))
  (gc :full t)) ; one more time to clean everything up

;;; Verify that the thread was started.
#+sb-thread
(with-test (:name :finalizer-thread-started)
  (assert (typep sb-impl::*finalizer-thread* 'sb-thread::thread))
  ;; We're going to assert on an approximate count of finalizers that ran,
  ;; which is a bit sketchy.
  ;; So call SCAN-FINALIZERS which actually does the work of helping out
  ;; the finalizer thread. RUN-PENDING-FINALIZERS is not enough - it would just kick
  ;; the thread and return right away, failing our requirement for being synchronous.
  ;; Also, there's no reason to pre- or post-check how may finalizer are pending
  ;; right now- it may be all of them, none of them, or anything in between,
  ;; depending on what the thread has gotten to so far.
  ;; This also shows that it works to run finalization actions in two threads -
  ;; the finalizer thread and this. Hence the need for ATOMIC-INCF on *count*.
  (sb-impl::scan-finalizers)

  ;; Make sure the thread is done.
  ;; This JOIN-THREADs it, so we know it's not executing.
  (sb-impl::finalizer-thread-stop))

;;; This was failing with something like:
;;;  The assertion (<= *MAXDEPTH* 1) failed with *MAXDEPTH* = 232.
;;; The test parameters for 64-bit are quite severe, but should not exhaust the heap.
(with-test (:name :finalizers-dont-nest-garbage-collections)
  (assert (<= *maxdepth* 1)))

;;; Regardless of anything else, check representational invariants.
;;; - each ID in the id-recycle-list is not a value in the hash-table
;;; - each value in the hash-table is not in the id-recycle-list
(with-test (:name :finalizer-id-uniqueness)
  (let* ((hash-table (elt sb-impl::**finalizer-store** 1))
         (used-ids (loop for v being each hash-value of hash-table
                         collect v))
         (available-ids (cdr (elt sb-impl::**finalizer-store** 0))))
    (assert (null (intersection used-ids available-ids)))))

(with-test (:name :finalizers-ran)
  ;; expect that 97% of the finalizers ran
  (assert (>= *count* (* *n-finalized-things* 97/100)))
  (unless (= *count* *n-finalized-things*)
    ;; show how the junk was reachable
    (search-roots *weak-pointers* :print :verbose)))

;;; For finalizers that didn't run (if any), we had better find
;;; that the object still exists in *weak-pointers*.
;;; Conversely, for each intact weak pointer, there had better be
;;; a finalizer for that object.
(with-test (:name :finalizer-state)
  (setq *weak-pointers*
        (delete-if (lambda (x) (null (weak-pointer-value x)))
                   *weak-pointers*))
  (let ((hash-table (elt sb-impl::**finalizer-store** 1)))
    (loop for k being each hash-key of hash-table
          when (and (symbolp k) (not (symbol-package k)))
            do (assert (find k *weak-pointers* :key #'weak-pointer-value)))
    (dolist (wp *weak-pointers*)
      (assert (gethash (weak-pointer-value wp) hash-table)))))
