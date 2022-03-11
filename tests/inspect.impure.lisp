;;;; tests for the INSPECT function

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

(defun test-inspect (object &optional (control '("q")))
  (let* ((control (if (listp control)
                      (format nil "~{~A~%~}" control)
                      control))
         (*standard-input* (make-string-input-stream control))
         (output (make-string-output-stream))
         (*standard-output* output))
    (inspect object)
    (get-output-stream-string output)))

(defclass class-with-prototype-print-error ()
  ((will-be-unbound)))

(defmethod print-object ((object class-with-prototype-print-error) stream)
  (print-unreadable-object (object stream :type t)
    (princ (slot-value object 'will-be-unbound) stream)))

(with-test (:name (inspect :no-error print-object :lp-454682))
  (let ((class (find-class 'class-with-prototype-print-error)))
    ;; Prototype may not be initialized at this point.
    (assert (search "PROTOTYPE: " (test-inspect class)))
    ;; Force protocol initialization and test again.
    (sb-mop:class-prototype class)
    (assert (search "PROTOTYPE: " (test-inspect class)))))

(with-test (:name (inspect array :element-type :lp-1835934))
  (let* ((array (make-array '() :initial-element 0))
         (result (test-inspect array)))
    (assert (search "an ARRAY of T" result))
    (assert (search "dimensions are ()" result)))

  (let ((array (make-array '() :element-type 'fixnum :initial-element 0)))
    (assert (search "an ARRAY of FIXNUM" (test-inspect array))))

  (let ((array (let ((a (make-array () :initial-element 0)))
                 (make-array '() :displaced-to a))))
    (assert (search "a displaced ARRAY of T" (test-inspect array)))))

(with-test (:name (inspect vector :*inspect-length*))
  (let* ((array (make-array 100 :initial-element t))
         (result (test-inspect array)))
    (assert (search "VECTOR of length 100." result))
    (assert (search "0. T" result))
    (assert (search "9. T" result))
    (assert (not (search "10. T" result)))))

(with-test (:name (inspect array :*inspect-length*))
  (let* ((array (make-array '(100 100) :initial-element t))
         (result (test-inspect array)))
    (assert (search "dimensions are (100 100)." result))
    (assert (search "0. [0,0] : T" result))
    (assert (search "9. [0,9] : T" result))
    (assert (not (search "10." result)))))

(with-test (:name (inspect vector fill-pointer))
  (let* ((array (make-array 3 :fill-pointer 2 :initial-element 0))
         (result (test-inspect array)))
    (assert (search "VECTOR of length 2" result))))
