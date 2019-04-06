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

(enable-test-parallelism)

(import '(sb-c::combination-fun-debug-name
          sb-c::combination-fun-source-name
          sb-c::*compile-component-hook*
          sb-c::basic-combination-p
          sb-c::basic-combination-info
          sb-c::node-tail-p
          sb-c::do-blocks
          sb-c::do-nodes
          sb-c::%check-bound
          sb-kernel:%bit-position/1))

(defun inspect-ir (form fun &rest checked-compile-args)
  (let ((*compile-component-hook* fun))
    (apply #'test-util:checked-compile form checked-compile-args)))

(defun ir-full-calls (form)
  (let (calls)
    (inspect-ir
     form
     (lambda (component)
       (do-blocks (block component)
         (do-nodes (node nil block)
           (when (and (basic-combination-p node)
                      (eq (basic-combination-info node) :full))
             (push node calls))))))
    calls))

(defun ir-calls (form)
  (let (calls)
    (inspect-ir
     form
     (lambda (component)
       (do-blocks (block component)
         (do-nodes (node nil block)
           (when (basic-combination-p node)
             (push node calls))))))
    calls))

(test-util:with-test (:name :%bit-position/1-tail-called)
  (destructuring-bind (combination)
      (ir-full-calls `(lambda (x)
                        (declare (optimize (debug 2)))
                        (position 1 (the simple-bit-vector x))))
    (assert (eql (combination-fun-debug-name combination) '%bit-position/1))
    (assert (node-tail-p combination))))

(test-util:with-test (:name :bounds-check-constants)
  (assert (= (count '%check-bound
                    (ir-calls
                     `(lambda (v)
                        (declare (simple-vector v))
                        (setf (aref v 0) (aref v 1))))
                    :key (lambda (x) (combination-fun-source-name x nil)))
             1)))
