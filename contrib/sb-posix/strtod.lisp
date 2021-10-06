(in-package "SB-POSIX")

(defun strtod (string)
  "Parse the string INPUT and return a double-precision float,
and a secondary value, the number of characters consumed."
  (flet ((strtod/base-string (chars offset)
           (declare (simple-base-string chars))
           ;; On x86, dx arrays are quicker to make than aliens.
           (sb-int:dx-let ((end (make-array 1 :element-type 'sb-ext:word)))
             (sb-sys:with-pinned-objects (chars)
               (let* ((base (sb-sys:sap+ (sb-sys:vector-sap chars) offset))
                      (answer
                       (handler-case
                        (alien-funcall
                         (extern-alien "strtod" (function double
                                                          system-area-pointer
                                                          system-area-pointer))
                         base
                         (sb-sys:vector-sap end))
                        (floating-point-overflow () nil))))
                 (values answer
                         (if answer
                             (the sb-int:index
                               (- (aref end 0) (sb-sys:sap-int base))))))))))
    (when (typep string 'simple-base-string)
      (return-from strtod (strtod/base-string string 0)))
    ;; Non-simple base-string with a null terminator in the right place.
    (when (typep string 'base-string)
      (sb-kernel:with-array-data ((data string) (start) (end) :check-fill-pointer t)
        (when (eql (locally
                    (declare (optimize (sb-c:insert-array-bounds-checks 0)))
                    (schar data (1+ end))) #\Nul)
          (return-from strtod (strtod/base-string data start)))))
    ;; Short simple non-base string, or base-string w/o a null in the right place
    (when (typep string '(or (simple-array character (*)) base-string))
      (let ((length (length string)))
        (when (< length 256) ; arbitrary limit to keep stack usage minimal
          (string-dispatch ((simple-array character (*)) base-string) string
            (sb-int:dx-let ((copy (make-array length :element-type 'base-char)))
              (loop for i below length do (setf (schar copy i) (char string i)))
              (return-from strtod (strtod/base-string copy 0)))))))
    (strtod/base-string (coerce string 'simple-base-string) 0))) ; Anything else
