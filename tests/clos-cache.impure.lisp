;;;; testing clos cache

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(with-test (:name :probe-cache-smoke-test)
  (let ((layout
          (sb-kernel::make-layout (sb-kernel::randomish-layout-clos-hash nil)
                                  (sb-kernel::make-undefined-classoid 'x)))
        (cache (sb-pcl::make-cache :key-count 1 :value t :size 10)))
    (sb-pcl::try-update-cache cache (list layout) 'win)
    (assert (eq (nth-value 1 (sb-pcl::probe-cache cache (list layout))) 'win))
    (assert (eq (nth-value 1 (sb-pcl::probe-cache cache layout)) 'win))))

;;;; Make a GF, populate it with a ton of methods, and then hammer
;;;; it with multiple threads. On 1.0.6 this would have failed with
;;;; "NIL is not an SB-KERNEL:LAYOUT" pretty quickly.

(defgeneric cache-test (x y))

(defvar *cache-test-classes* nil)

(macrolet ((def ()
             `(progn
                (defmethod cache-test (x y)
                  (list t t))
                ,@(loop for i from 0 upto 128
                       collect
                        (let ((c (intern (format nil "CACHE-TEST-CLASS-~S" i))))
                          `(progn
                             (defclass ,c () ())
                             (defmethod cache-test ((x ,c) (y ,c))
                               (list x y))
                             (defmethod cache-test ((x ,c) y)
                               (list x t))
                             (defmethod cache-test (x (y ,c))
                               (list t y))
                             (push (find-class ',c) *cache-test-classes*)))))))
  (def))

(defvar *run-cache-test* nil)

(let* ((instances (map 'vector #'make-instance *cache-test-classes*))
       (limit (length instances)))
  (defun test-cache ()
    (let* ((r (random limit))
           (instance (svref instances r)))
      (if (logbitp 0 r)
          (if (logbitp 1 r)
              (assert (equal (cache-test r r) '(t t)))
              (assert (equal (cache-test r instance) (list t instance))))
          (if (logbitp 1 r)
              (assert (equal (cache-test instance r) (list instance t)))
              (assert (equal (cache-test instance instance) (list instance instance))))))))

(let ((lock (sb-thread:make-mutex)))
  (defun note (control &rest args)
    (let ((string (apply #'format nil control args)))
      (sb-thread:with-mutex (lock)
        (write-line string)))))

(defun test-loop ()
  (loop until *run-cache-test* do (sb-thread:thread-yield))
  (handler-case
      (loop repeat 1024 do (test-cache))
    (error (e)
      (note "~&Error in cache test in ~S:~%~A~%...aborting"
            sb-thread:*current-thread* e)
      (sb-ext:exit :code 1))))

(with-test (:name :clos-cache-test
            :broken-on :sb-safepoint)
  #+sb-thread
  (let ((threads (loop repeat 32
                       collect (sb-thread:make-thread 'test-loop))))
    (setf *run-cache-test* t)
    (mapcar #'sb-thread:join-thread threads))

  #-sb-thread
  (progn
    (setf *run-cache-test* t)
    (loop repeat 4
          do (test-loop)))

  ;; Check that the test tests what it was supposed to test: the cache.
  (assert (sb-pcl::cache-p (sb-pcl::gf-dfun-cache #'cache-test))))
