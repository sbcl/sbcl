(cl:in-package :cl-user)

(funcall (lambda () 
	   (let ((simple-t (make-array 35))
		 (simple-u32 (make-array 50
					 :element-type '(unsigned-byte 32)))
		 (simple-character (make-string 44))
		 (complex-t (make-array 35 :fill-pointer 3))
		 (complex-u32 (make-array 88
					  :element-type '(unsigned-byte 32)))
		 (complex-character (make-array 14
						:element-type 'character
						:fill-pointer t)))
	     (assert (= (length simple-t) 35)))))
