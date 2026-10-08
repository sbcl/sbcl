#+(or gc-stress ;; c-find-heap->arena is not gc-safe
      (not system-tlabs) interpreter) (invoke-restart 'run-tests::skip-file)

(defmethod translate ((x (eql :a)) val) (* 1 val))
(defmethod translate ((x (eql :b)) val) (* 2 val))
(defmethod translate ((x (eql :c)) val) (* 3 val))
(defmethod translate ((x (eql :d)) val) (* 4 val))
(defmethod translate ((x (eql :e)) val) (* 5 val))
(defmethod translate ((x (eql :f)) val) (* 6 val))
(defmethod translate ((x (eql :g)) val) (* 7 val))
(defmethod translate ((x (eql :h)) val) (* 8 val))
(defmethod translate ((x (eql :i)) val) (* 9 val))
(defmethod translate ((x (eql :j)) val) (* 10 val))
(defmethod translate ((x (eql :k)) val) (* 11 val))

(defvar *a* (sb-vm:new-arena 1048576))

(defun f (arg) (sb-vm:with-arena (*a*) (translate arg 3)))

(f :c)
(assert (not (sb-vm:c-find-heap->arena)))

(defmethod zook ((x list))
  (format t "is-list~%"))
(defmethod zook ((x null))
  (format t "is-null~%")
  (call-next-method))
(defmethod zook ((x (eql nil)))
  (format t "is-eql-nil~%")
  (call-next-method))
(defvar *a* (sb-vm:new-arena 1048576))
(defun g ()
  (sb-vm:with-arena (*a*)
    (zook nil)))
(g)
(assert (not (sb-vm:c-find-heap->arena)))

(use-package "SB-VM")

(defun allocate-some-live-and-dead (arena)
  (with-arena (arena)
    (let ((live-regular nil)
          (live-huge nil))
      ;; Allocate 100 regular vectors, keeping only 10 (10 vectors + 10 conses = 20 objs)
      (dotimes (i 100)
        (let ((v (make-array 98 :initial-element i)))
          (when (< i 10)
            (push v live-regular))))
      ;; Allocate 4 huge byte vectors (> 131072 block size so each overflows mem->limit
      ;; and is placed on uw_huge_objects), keeping only the last one
      (dotimes (i 4)
        (setq live-huge (make-array 150000
                                    :element-type '(unsigned-byte 8)
                                    :initial-element (ldb (byte 8 0) i))))
      ;; 21st regular object:
      (cons live-regular live-huge))))

(test-util:with-test (:name :arena-live-bytes
                      :skipped-on (:not (:and :x86-64 :gencgc :unix)))
  (let ((arena (new-arena 131072 131072 10)))
    (unwind-protect
         (let ((kept (allocate-some-live-and-dead arena)))
           (multiple-value-bind (n-objs1 n-live1 bytes-alloc1 bytes-live1 n-huge1 n-huge-live1)
               (sb-kernel:arena-live-bytes arena)
             (assert (consp (opaque-identity kept)))
             (assert (>= n-objs1 111))
             (assert (<= 21 n-live1 30))
             (assert (= n-huge1 4))
             (assert (<= 1 n-huge-live1 2))
             (assert (< 0 bytes-live1 (* bytes-alloc1 1/2)))
             ;; Now drop KEPT and re-run
             (setq kept (opaque-identity nil))
             (multiple-value-bind (n-objs2 n-live2 bytes-alloc2 bytes-live2 n-huge2 n-huge-live2)
                 (sb-kernel:arena-live-bytes arena)
               (assert (= n-objs2 n-objs1))
               (assert (= bytes-alloc2 bytes-alloc1))
               (assert (= n-huge2 n-huge1))
               (assert (< n-live2 n-live1))
               (assert (< bytes-live2 bytes-live1))
               (assert (< n-huge-live2 n-huge-live1)))
             ;; Assert that we didn't kill the usable frontier of the active arena tlab
             (with-arena (arena)
               (assert (= (length (make-list 50)) 50)))))
      (destroy-arena arena))))
