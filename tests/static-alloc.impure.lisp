
(dolist (type '(single-float double-float (unsigned-byte 8)
                (unsigned-byte 32) (signed-byte 32)))
  (let* ((vectors (loop
                     for i upto 128
                     collect (sb-int:make-static-vector
                              256 :element-type type)))
         (saps (mapcar #'sb-sys:vector-sap vectors)))
    (gc :full t)
    (assert (every #'sb-sys:sap=
                   saps
                   (mapcar #'sb-sys:vector-sap vectors)))))

