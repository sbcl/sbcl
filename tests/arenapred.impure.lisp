#+(or gc-stress interpreter
      (not (and x86-64 system-tlabs sb-futex (not sb-safepoint))))
(invoke-restart 'run-tests::skip-file)

(defvar *allocation-nbytes*)

(setq sb-vm:*arena-exhaustion-handler*
      (lambda (arena allocation new-size)
        (declare (ignore arena new-size))
        (setq *allocation-nbytes* allocation)))

(defparameter *a* (sb-vm:new-arena 65536))
(defvar *l* nil)

(defun cleanup ()
  (setq *l* nil)
  (sb-vm:rewind-arena *a*)
  (makunbound '*allocation-nbytes*))

(defun accumulate-it (l)
  ;; use the first cons of L to attach the rest to *L*
  (let ((cell l))
    (setf l (cdr l)
          (car cell) l
          (cdr cell) *l*
          *l* cell)))

(defun somefun (&rest rest)
  (accumulate-it rest))

;; There are essentially 4 ways to get into the C fallback code
;; from the lisp fast-path allocator:
;;  - list allocation from &REST
;;  - list allocation from MAKE-LIST
;;  - all other list allocations {CONS, LIST, LIST*, ACONS, ...}
;;  - non-list allocation
;; and there are minor variations in how they pass the object size
;; to the Lisp fallback handler. So we would like to assert that
;; in each case, the user-defined predicate gets the correct size
;; of the allocation request.
(defun try-plain-list (arena)
  (sb-vm:with-arena (arena)
    (dotimes (i 400)
      (accumulate-it (list 1 2 3 4 5 6 7 8 9 10 11 12))))
  (assert (= *allocation-nbytes* (* 12 16)))
  (cleanup))

(defun try-rest-arg (arena)
  (sb-vm:with-arena (arena)
    (dotimes (i 400)
      (somefun 1 2 3 4 5 6 7 8 9 10 11)))
  (assert (= *allocation-nbytes* (* 11 16)))
  (cleanup))

(defun foolz (n e) (accumulate-it (make-list n :initial-element e)))
(defun try-make-list (arena)
  (sb-vm:with-arena (arena)
    (dotimes (i 210)
      (foolz 20 i)))
  (assert (= *allocation-nbytes* (* 20 16))))

(defun try-nonlist (arena)
  (sb-int:dx-let ((v (make-array 10)))
    (sb-vm:with-arena (arena)
      (dotimes (i 9)
        ;; this exceeds the huge object threshold for the arena
        (setf (aref v i) (make-array 987))))
    ;; do something here with V if the compiler ever gets smart enough
    ;; to flush the entire loop above
    )
  (assert (= *allocation-nbytes* (* (+ 988 2) 8)))
  (assert (/= 0 (sb-vm::arena-huge-objects *a*)))
  (cleanup))

(test-util:with-test (:name :arena-handler)
  (try-plain-list *a*)
  (try-rest-arg *a*)
  (try-make-list *a*)
  (try-nonlist *a*))

(sb-vm:destroy-arena *a*)

;;; Here is a fairly small arena that is allowed to expand exactly 1 time
;;; by default, doubling in size from 1 MiB to 2 MiB.
(defparameter *a* (sb-vm:new-arena 1048576 1048576 1))

(setq sb-vm:*arena-exhaustion-handler* #'sb-vm::default-arena-extension-test)
(defun expand-many-times ()
  (let (l)
    (sb-vm:with-arena (*a*)
      (dotimes (i 3000)
        (push (make-array 500) l)))
    (length l)))

(test-util:with-test (:name :expand-and-fail)
  (assert (eq (handler-case (expand-many-times)
                (storage-condition (c) c :ok))
              :ok))
  (assert (eq (handler-case (expand-many-times)
                (storage-condition (c) c :ok))
              :ok)))

;; This predicate allows allocating arbitrarily many small objects
;; to that arena, even beyond the stated total maximum size of 2 MiB.
(defvar *n-expansions* 0)
(setq sb-vm:*arena-exhaustion-handler*
      (lambda (arena nbytes total-size)
        (format t "~&Need ~D bytes on arena ~s (proposed size ~D)~%"
                nbytes arena total-size)
        (cond ((> nbytes 32768)
               (throw 'fail 'oom))
              (t
               (incf *n-expansions*))))) ; non-nil = allow

(test-util:with-test (:name :expand-and-succeed)
  (expand-many-times)
  (assert (> *n-expansions* 10))
  (expand-many-times)
  (assert (> *n-expansions* 20))
  (sb-vm:rewind-arena *a*))

;;; This test shows that we can correctly fail an allocation,
;;; aborting to a nonlocal exit point, but running any intervening
;;; cleanups on the arena.
(defun try-making-bunch-of-arrays (arena howmany)
  (let ((array-size 4)
        (list))
    (sb-vm:with-arena (arena)
      (catch 'fail
        (unwind-protect
             (dotimes (i howmany)
               (let ((a (make-array array-size)))
                 (setq array-size (ash array-size 1))
                 (push a list)))
          (cond ((= (length list) howmany)
                 (format t "Made all arrays~%"))
                (t
                 (assert (sb-vm::arena-exhausted arena))
                 ;; (format t "Did not make them. arena=~s~%" (sb-vm:thread-current-arena))
                 ;; Despite allocation failure, the cleanup path runs on the arena.
                 (assert (sb-vm:arena-p (sb-vm:thread-current-arena)))
                 ;; moreover, we can allocate large objects on the arena
                 ;; (technically in single-object extension blocks)
                 ;; during the "emergency" regime
                 (let ((a1 (make-array 800000 :element-type '(unsigned-byte 8)))
                       (a2 (make-array 800000 :element-type '(unsigned-byte 8))))
                   (assert (not (heap-allocated-p a1)))
                   (assert (not (heap-allocated-p a2)))))))))))

(test-util:with-test (:name :conditionally-fail)
  (assert (eq (try-making-bunch-of-arrays *a* 15) 'oom))
  (sb-vm:destroy-arena *a*))
