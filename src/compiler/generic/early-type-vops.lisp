(in-package "SB!VM")

(defparameter *immediate-types*
  (list unbound-marker-widetag base-char-widetag))

(defparameter *fun-header-widetags*
  (list funcallable-instance-header-widetag
	simple-fun-header-widetag
	closure-fun-header-widetag
	closure-header-widetag))

(defun canonicalize-headers (headers)
  (collect ((results))
    (let ((start nil)
	  (prev nil)
	  (delta (- other-immediate-1-lowtag other-immediate-0-lowtag)))
      (flet ((emit-test ()
	       (results (if (= start prev)
			    start
			    (cons start prev)))))
	(dolist (header (sort headers #'<))
	  (cond ((null start)
		 (setf start header)
		 (setf prev header))
		((= header (+ prev delta))
		 (setf prev header))
		(t
		 (emit-test)
		 (setf start header)
		 (setf prev header))))
	(emit-test)))
    (results)))

(defmacro test-type (value target not-p &rest type-codes)
  ;; Determine what interesting combinations we need to test for.
  (let* ((type-codes (mapcar #'eval type-codes))
	 (fixnump (and (member even-fixnum-lowtag type-codes)
		       (member odd-fixnum-lowtag type-codes)
		       t))
	 (lowtags (remove lowtag-limit type-codes :test #'<))
	 (extended (remove lowtag-limit type-codes :test #'>))
	 (immediates (intersection extended *immediate-types* :test #'eql))
	 (headers (set-difference extended *immediate-types* :test #'eql))
	 (function-p (if (intersection headers *fun-header-widetags*)
			 (if (subsetp headers *fun-header-widetags*)
			     t
			     (error "can't test for mix of function subtypes ~
				     and normal header types"))
			 nil)))
    (unless type-codes
      (error "At least one type must be supplied for TEST-TYPE."))
    (cond
      (fixnump
       (when (remove-if (lambda (x)
			  (or (= x even-fixnum-lowtag)
			      (= x odd-fixnum-lowtag)))
			lowtags)
	 (error "can't mix fixnum testing with other lowtags"))
       (when function-p
	 (error "can't mix fixnum testing with function subtype testing"))
       (when immediates
	 (error "can't mix fixnum testing with other immediates"))
       (if headers
	   `(%test-fixnum-and-headers ,value ,target ,not-p
	     ',(canonicalize-headers headers))
	   `(%test-fixnum ,value ,target ,not-p)))
      (immediates
       (when headers
	 (error "can't mix testing of immediates with testing of headers"))
       (when lowtags
	 (error "can't mix testing of immediates with testing of lowtags"))
       (when (cdr immediates)
	 (error "can't test multiple immediates at the same time"))
       `(%test-immediate ,value ,target ,not-p ,(car immediates)))
      (lowtags
       (when (cdr lowtags)
	 (error "can't test multiple lowtags at the same time"))
       (if headers
	   `(%test-lowtag-and-headers
	     ,value ,target ,not-p ,(car lowtags)
	     ,function-p ',(canonicalize-headers headers))
	   `(%test-lowtag ,value ,target ,not-p ,(car lowtags))))
      (headers
       `(%test-headers ,value ,target ,not-p ,function-p
	 ',(canonicalize-headers headers)))
      (t
       (error "nothing to test?")))))

