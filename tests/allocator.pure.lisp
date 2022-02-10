#+gencgc
(progn
(defun large-object-p (x)
  (and (eq (sb-ext:heap-allocated-p x) :dynamic)
       (let ((flags
              (sb-sys:with-pinned-objects (x)
                (sb-alien:slot (sb-alien:deref sb-vm::page-table
                                               (sb-vm:find-page-index
                                                (sb-kernel:get-lisp-obj-address x)))
                               'sb-vm::flags))))
         (logbitp 4 ; SINGLE_OBJECT_FLAG
                  (ldb (byte 6 (+ #+big-endian 2)) flags)))))
(compile 'large-object-p)

;;; Pseudo-static large objects should retain the single-object flag
#+gencgc ; PSEUDO-STATIC-GENERATION etc don't exist for cheneygc
(with-test (:name :pseudostatic-large-objects)
  (sb-vm:map-allocated-objects
   (lambda (obj type size)
     (declare (ignore type size))
     (when (>= (sb-ext:primitive-object-size obj) (* 4 sb-vm:gencgc-page-bytes))
       (let* ((addr (sb-kernel:get-lisp-obj-address obj))
              (pte (deref sb-vm:page-table (sb-vm:find-page-index addr))))
         (when (eq (slot pte 'sb-vm::gen) sb-vm:+pseudo-static-generation+)
           (assert (large-object-p obj))))))
   :all))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter large-n-words (/ sb-vm:large-object-size sb-vm:n-word-bytes))
  (defparameter large-n-conses (/ large-n-words 2))))

(with-test (:name :no-&rest-on-large-object-pages :skipped-on (:not :gencgc))
  ;; adding in a 2-word vector header makes it at least large-object-size.
  ;; The threshold in the allocator is exact equality for that.
  (let ((definitely-large-vector (make-array (- large-n-words 2)))
        ;; Decreasing by 1 word isn't enough, because of padding, so decrease by 2 words
        (not-large-vector (make-array (- large-n-words 4))))
    ;; Verify the edge case for LARGE-OBJECT-P
    (assert (large-object-p definitely-large-vector))
    (assert (not (large-object-p not-large-vector)))
    (assert (not (large-object-p (list 1 2)))))
  (let ((fun (checked-compile '(lambda (&rest params) params))))
    (assert (not (large-object-p (apply fun (make-list large-n-conses)))))))

#+gencgc ; can't read-time-eval #.large-n-conses
(with-test (:name :no-list-on-large-object-pages
                  :skipped-on (:or (:not :gencgc) :ppc :ppc64))
  (let* ((fun (checked-compile
               '(lambda ()
                 (macrolet ((expand (n) `(list ,@(loop for i from 1 to n collect i))))
                   (expand #.large-n-conses)))))
         (list (funcall fun)))
    (assert (not (large-object-p list)))))
