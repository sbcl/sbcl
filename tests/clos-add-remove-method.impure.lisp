;;;; testing add/remove-method thread safety

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

(defpackage "CLOS-ADD/REMOVE-METHOD"
  (:use "COMMON-LISP" "SB-THREAD"))

(in-package "CLOS-ADD/REMOVE-METHOD")

(defvar *tons-o-method-count* 0)
;; Test for lp#492851
;; On an x86-64 without the fix for garbage retention,
;; this would use up all memory after ~2400 iterations.
;; Now it can run forever, as far as I know.
(defun make-tons-o-methods (&optional (n 50000))
  (dotimes (i n)
    (let ((object (cons "lottajunk" (make-array 20001))))
      ;; FIXME: the first method we defined gets its OBJECT immortalized,
      ;; because that's the one whose name goes in globaldb
      ;; as the chosen one who arbitrarily created the GF.
      (defmethod blah ((x (eql object))) "what?"
        (when (zerop (mod (incf *tons-o-method-count*) 500))
          (write-char #\.)
          (force-output))
        (if (zerop (mod *tons-o-method-count* 100000))
            (terpri)))
      (funcall 'blah object)) ; late bind to avoid style warning
    (let ((gf (symbol-function 'blah)))
      (remove-method gf (first (sb-mop:generic-function-methods gf))))))

(format t "~&;; Be patient. This test is slow.~%")
(test-util:with-test (:name :exhaust-heap-with-eql-specializers)
  (let ((n-junk 0)
        (starting-eql-spec-count
         (hash-table-count sb-pcl::*eql-specializer-table*)))
    (make-tons-o-methods 5000)
    (sb-int:dx-flet ((visit (object type size)
                       (declare (ignore type size))
                       (when (and (typep object '(cons string (simple-vector 20001)))
                                  (string= (car object) "lottajunk"))
                         (incf n-junk))))
      (sb-vm:map-allocated-objects #'visit :dynamic)
      ;; This is probably not more than a few hundred.
      (format t "~&;; Post-test EQL-spec count: ~S, junk obj count: ~D~%"
              (hash-table-count sb-pcl::*eql-specializer-table*) n-junk)
      (sb-ext:gc :full t)
      (setq n-junk 0)
      (sb-vm:map-allocated-objects #'visit :dynamic)
      (format t "~&;; Post-GC EQL-spec count: ~S, junk obj count: ~D~%"
              (hash-table-count sb-pcl::*eql-specializer-table*) n-junk)
      ;; There should be no stray EQL-specializers left.
      ;; The reasoning is a bit elusive, since the first method's specializer's
      ;; OBJECT gets permanentized. But due to the weakness on :VALUE
      ;; in the specializer table, the table entry is dead even though it's
      ;; still possible to get an EQL-specializer for the (almost) zombie object.
      ;; But if you do that, you'll get a new specializer, and you can't see
      ;; that it wasn't the previously interned one, unless you do something
      ;; like extend EQL-specializer to have more slots. In that case you could
      ;; sense that your data went missing. But then you probably shouldn't
      ;; be relying on the existing interned specializer table.
      ;; Generic programming is not a panacea.
      (assert (<= (hash-table-count sb-pcl::*eql-specializer-table*)
                  (1+ starting-eql-spec-count))))))

;;; We make a generic function, add a bunch of method for it, and
;;; prepare another bunch of method objects for later addition.
;;;
;;; Then we run several threads in parallel, removing all the old
;;; ones and adding all the new ones -- and finally we verify that
;;; the resulting method set is correct.

(defgeneric foo (x))

(defvar *to-remove-a* nil)
(defvar *to-remove-b* nil)
(defvar *to-remove-c* nil)
(defvar *to-add-d* nil)
(defvar *to-add-e* nil)
(defvar *to-add-f* nil)

(defun name (key n)
  (intern (format nil "FOO-~A-~A" key n)))

(defun names (key)
  (loop for i from 0 upto 128
        collect (name key i)))

(defun to-remove (key)
  (loop for s in (names key)
        collect
        `(progn
           (defclass ,s () ())
           (defmethod foo ((x ,s))
             ',s)
           (push (find-method #'foo nil (list (find-class ',s)) t)
                 ,(intern (format nil "*TO-REMOVE-~A*" key))))))

(defun to-add (key)
  (loop for s in (names key)
        collect
        `(progn
           (defclass ,s () ())
           (push (make-instance
                  'standard-method
                  :qualifiers nil
                  :specializers (list (find-class ',s))
                  :function (lambda (args next)
                              (declare (ignore args next))
                              ',s)
                  :lambda-list '(x))
                 ,(intern (format nil "*TO-ADD-~A*" key))))))

(macrolet ((def ()
             `(progn
                ,@(to-remove 'a)
                ,@(to-remove 'b)
                ,@(to-remove 'c)
                ,@(to-add 'd)
                ,@(to-add 'e)
                ,@(to-add 'f))))
  (def))

(defvar *run* (sb-thread:make-semaphore))

(defun remove-methods (list)
  (sb-thread:wait-on-semaphore *run*)
  (dolist (method list)
    (remove-method #'foo method)))

(defun add-methods (list)
  (sb-thread:wait-on-semaphore *run*)
  (dolist (method list)
    (add-method #'foo method)))

#+sb-thread
(let ((threads (list (make-thread (lambda () (remove-methods *to-remove-a*)))
                     (make-thread (lambda () (remove-methods *to-remove-b*)))
                     (make-thread (lambda () (remove-methods *to-remove-c*)))
                     (make-thread (lambda () (add-methods *to-add-d*)))
                     (make-thread (lambda () (add-methods *to-add-e*)))
                     (make-thread (lambda () (add-methods *to-add-f*))))))
  (sb-thread:signal-semaphore *run* 6)
  (mapcar #'join-thread threads))

#-sb-thread
(progn
  (sb-thread:signal-semaphore *run* 6)
  (remove-methods *to-remove-a*)
  (remove-methods *to-remove-b*)
  (remove-methods *to-remove-c*)
  (add-methods *to-add-d*)
  (add-methods *to-add-e*)
  (add-methods *to-add-f*))

(let ((target (append *to-add-d* *to-add-e* *to-add-f*))
      (real (sb-mop:generic-function-methods #'foo)))
  (assert (subsetp target real))
  (assert (subsetp real target)))
