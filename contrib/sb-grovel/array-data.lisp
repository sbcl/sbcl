(in-package :sb-grovel)

;;; borrowed from CMUCL manual, lightly ported

(defun array-data-address (array)
  "Return the physical address of where the actual data of an array is
stored.

ARRAY must be a specialized array type - an array of one of these types:

                  double-float
                  single-float
                  (unsigned-byte 32)
                  (unsigned-byte 16)
                  (unsigned-byte  8)
                  (signed-byte 32)
                  (signed-byte 16)
                  (signed-byte  8)
"
  (declare (type (or (array (signed-byte 8))
		     (array base-char)
		     simple-base-string
                     (array (signed-byte 16))
                     (array (signed-byte 32))
                     (array (unsigned-byte 8))
                     (array (unsigned-byte 16))
                     (array (unsigned-byte 32))
                     (array single-float)
                     (array double-float))
                 array)
           (optimize (speed 0) (debug 3) (safety 3)))
  ;; with-array-data will get us to the actual data.  However, because
  ;; the array could have been displaced, we need to know where the
  ;; data starts.

  (let* ((type (car (multiple-value-list (array-element-type array))))
	 (type-size
	  (cond ((or (equal type '(signed-byte 8))
		     (equal type 'cl::base-char)
		     (equal type '(unsigned-byte 8)))
		 1)
		((or (equal type '(signed-byte 16))
		     (equal type '(unsigned-byte 16)))
		 2)
		((or (equal type '(signed-byte 32))
		     (equal type '(unsigned-byte 32)))
		 4)
		((equal type 'single-float)
		 4)
		((equal type 'double-float)
		 8)
		(t (error "Unknown specialized array element type")))))
    (sb-kernel::with-array-data ((data array)
		      (start)
		      (end))
      (declare (ignore end))
      ;; DATA is a specialized simple-array.  Memory is laid out like this:
      ;;
      ;;   byte offset    Value
      ;;        0         type code (e.g. 70 for double-float vector)
      ;;        4         FIXNUMIZE(number of elements in vector)
      ;;        8         1st element of vector
      ;;      ...         ...
      ;;
      (let* ((addr (+ 8 (logandc1 7 (sb-kernel:get-lisp-obj-address data)))))
	(declare (type (unsigned-byte 32) addr)
		 (optimize (speed 3) (safety 0)))
	(sb-sys:int-sap (the (unsigned-byte 32)
			  (+ addr (* type-size start))))))))



