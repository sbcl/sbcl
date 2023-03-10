(setq *evaluator-mode* :compile)
(load "src/code/redblack.lisp")
(with-compilation-unit () (load "tests/test-util.lisp"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'sb-int:dovector))

(in-package "SB-RBTREE.WORD")
(defun height (tree)
  (sb-int:named-let recurse ((tree tree))
    (if (not tree)
        0
        (1+ (max (recurse (left tree)) (recurse (right tree)))))))
(compile 'height)

(in-package "SB-BROTHERTREE")
(defmacro binary-node-parts (node)
  `(let ((n ,node))
     (if (fringe-binary-node-p n)
         (values nil (binary-node-key n) nil) ; has only one data slot
         ;; has left + right
         (values (binary-node-%left n) (binary-node-key n) (binary-node-%right n)))))
(defun height (tree &aux (n 0))
  (loop
     (unless tree (return n))
     (incf n)
     ;; We're assuming that the brothertree invariant holds-
     ;; the left and right heights are the same.
     (typecase tree
       (binary-node (setq tree (values (binary-node-parts tree))))
       (unary-node  (setq tree (child tree))))))
(compile 'height)

(in-package "CL-USER")
(defvar *brothertree* nil)
(defvar *rbtree* nil)
(defvar *solist* nil)
(defvar *hash-table* nil)

(defvar *lotta-strings*
  (coerce
   ;; Collect all pseudostatic symbols and all readonly strings
   (mapcar (lambda (x)
             (sb-kernel:%make-lisp-obj
              (logandc2 (sb-kernel:get-lisp-obj-address x)
                        sb-vm:lowtag-mask)))
           (nconc (sb-vm:list-allocated-objects
                   :dynamic
                   :type sb-vm:symbol-widetag
                   :test (lambda (x) (= (sb-kernel:generation-of x)
                                        sb-vm:+pseudo-static-generation+)))
                  (sb-vm:list-allocated-objects
                   :read-only
                   :type sb-vm:simple-base-string-widetag)))
   'vector))
(declaim  (simple-vector *lotta-strings*))

(defun insert-all-brothertree ()
  (let ((tree nil))
    (dovector (str *lotta-strings*)
      (setq tree (sb-brothertree:insert str tree)))
    (setq *brothertree* tree)))

(defun insert-all-redblack ()
  (let ((tree nil))
    (dovector (str *lotta-strings*)
      ;; because OF COURSE the arg orders are opposite
      (setq tree (sb-rbtree.word:insert tree str)))
    (setq *rbtree* tree)))

(defun insert-all-solist ()
  (let ((set (let ((sb-lockless::*desired-elts-per-bin* 2))
               (sb-lockless:make-so-set/addr))))
    (dovector (str *lotta-strings*)
      (sb-lockless:so-insert set str))
    (setq *solist* set)))

(defun insert-all-hash-table ()
  (let ((set (make-hash-table :test 'eq :synchronized t)))
    (dovector (str *lotta-strings*)
      (setf (gethash str set) t))
    (setq *hash-table* set)))

(dolist (test '(insert-all-redblack insert-all-brothertree insert-all-solist insert-all-hash-table))
  (gc)
  (format t "Running ~S~%" test)
  (time (funcall test)))

(let ((n (length *lotta-strings*)))
  (format t "~&Memory:~:{~%  ~8a=~8D ~3,1f~}~%"
          (loop for (name . val) in `(("brother" . ,*brothertree*)
                                      ("redblack" . ,*rbtree*)
                                      ("solist" . ,*solist*)
                                      ("hashtbl " . ,*hash-table*))
                collect
                (let ((mem (test-util:deep-size val)))
                  (list name mem (/ mem n))))))

(format t "~&Tree heights: redblack=~D brother=~D~2%"
        (sb-rbtree.word::height *rbtree*)
        (sb-brothertree::height *brothertree*))

(macrolet ((exercise (find-it)
             `(ecase direction
                (:up (loop for str across *lotta-strings*
                           count ,find-it))
                (:down (let ((v *lotta-strings*))
                         (loop for i downfrom (1- (length v)) to 0
                               count (let ((str (svref v i))) ,find-it)))))))
  (defun find-all-in-brothertree (&optional (direction :up) &aux (tree *brothertree*))
    (exercise (sb-brothertree:find= str tree)))
  (defun find-all-in-redblack-tree (&optional (direction :up) &aux (tree *rbtree*))
    (exercise (sb-rbtree.word:find= str tree)))
  (defun find-all-in-solist (&optional (direction :up) &aux (set *solist*))
    (exercise (sb-lockless:so-find set str)))
  (defun find-all-in-hash-table (&optional (direction :up) &aux (set *hash-table*))
    (exercise (gethash str set))))

;;; Each test will run *nthreads* threads and each thread will find each item.
(defvar *start-sem* (sb-thread:make-semaphore))
(defvar *completion-sem* (sb-thread:make-semaphore))
(defvar *function-to-run* nil)
(defglobal *results* nil)
(defun say (s)
  (declare (simple-base-string s))
  (sb-sys:with-pinned-objects (s)
    ;; avoid interleaved output, usually
    (sb-unix:unix-write 2 s 0 (length s))))

(defmacro with-cycle-counter (form)
  `(multiple-value-bind (hi0 lo0) (sb-vm::%read-cycle-counter)
     (values ,form
             (multiple-value-bind (hi1 lo1) (sb-vm::%read-cycle-counter)
               (+ (ash (- hi1 hi0) 32) (- lo1 lo0))))))

(defun workfun (my-index)
  (let ((direction-to-scan (if (oddp my-index) :up :down)))
    (loop ; (say (format nil "thread ~d waiting~%" my-index))
          (sb-thread:wait-on-semaphore *start-sem*)
          ; (say (format nil "thread ~d starting function under test~%" my-index))
          (let ((test-fun *function-to-run*))
            (when (null test-fun)
              ;(say (format nil "thread ~d exiting~%" my-index))
              (return))
            ;(say (format nil "thread ~d working~%" my-index))
            (multiple-value-bind (answer cycle-time)
                (with-cycle-counter (funcall test-fun direction-to-scan))
              (assert (= answer (length *lotta-strings*)))
              (sb-ext:atomic-push cycle-time *results*))
            (sb-thread:signal-semaphore *completion-sem*)))))

(defun perform-work-in-threads (test nthreads)
  (setf *function-to-run* test
        *results* nil)
  (sb-thread:signal-semaphore *start-sem* nthreads)
  (sb-thread:wait-on-semaphore *completion-sem* :n nthreads))

(defvar *find-tests* '(find-all-in-brothertree find-all-in-redblack-tree
                       find-all-in-solist find-all-in-hash-table))

(defun test-nthreads (&optional (nthreads 6))
  (let ((threads (make-array nthreads)))
    (dotimes (i nthreads)
      (setf (aref threads i) (sb-thread:make-thread #'workfun :arguments (list i))))
    (dolist (test *find-tests*)
      (format t "~&Testing ~S~%" test)
      (time (perform-work-in-threads test nthreads))
      (let* ((cycle-times *results*)
            (min (reduce #'min cycle-times))
            (max (reduce #'max cycle-times))
            (sum (reduce #'+ cycle-times)))
        (format t " ==> min=~E max=~E avg=~E~2%"
                min max (/ sum nthreads))))
    (setq *function-to-run* nil)
    (sb-thread:signal-semaphore *start-sem* nthreads)
    (map nil #'sb-thread:join-thread threads)))

#|
* (load"benchmarks/bbtrees")
Evaluation took:
  0.012 seconds of real time
  0.012138 seconds of total run time (0.012086 user, 0.000052 system)
  100.00% CPU
  29,126,552 processor cycles
  21,916,768 bytes consed

Evaluation took:
  0.007 seconds of real time
  0.007634 seconds of total run time (0.007550 user, 0.000084 system)
  114.29% CPU
  18,334,338 processor cycles
  18,640,080 bytes consed

Tree heights: redblack=25 brother=16
|#
