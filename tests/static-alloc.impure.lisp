
(dolist (type '(single-float double-float (unsigned-byte 8)
                (unsigned-byte 32) (signed-byte 32)
                base-char character))
  (let* ((vectors (loop
                     for i upto 128
                     collect (sb-int:make-static-vector
                              256 :element-type type)))
         (saps (mapcar #'sb-sys:vector-sap vectors)))
    (gc :full t)
    (assert (every #'sb-sys:sap=
                   saps
                   (mapcar #'sb-sys:vector-sap vectors)))))

;;; Compute the physical size of some vectors and make sure it's right.
;;; Why, you might ask, can't this simply use SB-VM::PRIMITIVE-OBJECT-SIZE
;;; to compare against the size of a non-static vector?
;;; Because PRIMITIVE-OBJECT-SIZE always gives you the _CORRECT_ answer
;;; for the object, not the amount of space the allocator took,
;;; and this test needs to assert correctness of the allocator.
(dolist (type '(base-char character))
  (loop for i from 1 to 20
     do (let* ((before sb-vm:*static-space-free-pointer*)
              (obj (sb-vm::make-static-vector i :element-type type))
              (after sb-vm:*static-space-free-pointer*)
              (used (sb-sys:sap- after before)))
          (assert (= used (sb-vm::primitive-object-size obj))))))
