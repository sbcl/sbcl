;;;; This implementation of RANDOM is based on the Mersenne Twister random
;;;; number generator "MT19937" due to Matsumoto and Nishimura. See:
;;;;   Makoto Matsumoto and T. Nishimura, "Mersenne twister: A
;;;;   623-dimensionally equidistributed uniform pseudorandom number
;;;;   generator.", ACM Transactions on Modeling and Computer Simulation,
;;;;   1997, to appear.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;;; RANDOM-STATEs

(def!method make-load-form ((random-state random-state) &optional environment) 
  (make-load-form-saving-slots random-state :environment environment))

;;; The state is stored in a (simple-array (unsigned-byte 32) (627))
;;; wrapped in a random-state structure:
;;;
;;;  0-1:   Constant matrix A. [0, #x9908b0df]
;;;  2:     Index k.
;;;  3-626: State.

;;; Generate and initialize a new random-state array. Index is
;;; initialized to 1 and the states to 32bit integers excluding zero.
;;;
;;; Seed - A 32bit number, not zero.
;;;
;;; Apparently uses the generator Line 25 of Table 1 in
;;; [KNUTH 1981, The Art of Computer Programming, Vol. 2 (2nd Ed.), pp102]
(defun init-random-state (&optional (seed 4357) state)
  (declare (type (integer 1 #xffffffff) seed))
  (let ((state (or state (make-array 627 :element-type '(unsigned-byte 32)))))
    (declare (type (simple-array (unsigned-byte 32) (627)) state))
    (setf (aref state 1) #x9908b0df)
    (setf (aref state 2) 1)
    (setf (aref state 3) seed)
    (do ((k 1 (1+ k)))
	((>= k 624))
      (declare (type (mod 625) k))
      (setf (aref state (+ 3 k))
	    (logand (* 69069 (aref state (+ 3 (1- k)))) #xffffffff)))
    state))

(defvar *random-state*)
(defun !random-cold-init ()
  (/show0 "entering !RANDOM-COLD-INIT")
  (setf *random-state* (%make-random-state))
  (/show0 "returning from !RANDOM-COLD-INIT"))

(defun make-random-state (&optional state)
  #!+sb-doc
  "Make a random state object. If STATE is not supplied, return a copy
  of the default random state. If STATE is a random state, then return a
  copy of it. If STATE is T then return a random state generated from
  the universal time."
  (/show0 "entering !RANDOM-COLD-INIT")
  (flet ((copy-random-state (state)
	   (/show0 "entering COPY-RANDOM-STATE")
	   (let ((state (random-state-state state))
		 (new-state
		  (make-array 627 :element-type '(unsigned-byte 32))))
	     (/show0 "made NEW-STATE, about to DOTIMES")
	     (dotimes (i 627)
	       (setf (aref new-state i) (aref state i)))
	     (/show0 "falling through to %MAKE-RANDOM-STATE")
	     (%make-random-state :state new-state))))
    (/show0 "at head of ETYPECASE in MAKE-RANDOM-STATE")
    (etypecase state
      (null
       (/show0 "NULL case")
       (copy-random-state *random-state*))
      (random-state
       (/show0 "RANDOM-STATE-P clause")
       (copy-random-state state))
      ((member t)
       (/show0 "T clause")
       (%make-random-state :state (init-random-state
				   (logand (get-universal-time)
					   #xffffffff)))))))

;;;; random entries

;;; This function generates a 32bit integer between 0 and #xffffffff
;;; inclusive.
#!-sb-fluid (declaim (inline random-chunk))
;;; portable implementation
(defconstant mt19937-n 624)
(defconstant mt19937-m 397)
(defconstant mt19937-upper-mask #x80000000)
(defconstant mt19937-lower-mask #x7fffffff)
(defconstant mt19937-b #x9D2C5680)
(defconstant mt19937-c #xEFC60000)
#!-x86
(defun random-mt19937-update (state)
  (declare (type (simple-array (unsigned-byte 32) (627)) state)
	   (optimize (speed 3) (safety 0)))
  (let ((y 0))
    (declare (type (unsigned-byte 32) y))
    (do ((kk 3 (1+ kk)))
	((>= kk (+ 3 (- mt19937-n mt19937-m))))
      (declare (type (mod 628) kk))
      (setf y (logior (logand (aref state kk) mt19937-upper-mask)
		      (logand (aref state (1+ kk)) mt19937-lower-mask)))
      (setf (aref state kk) (logxor (aref state (+ kk mt19937-m))
				    (ash y -1) (aref state (logand y 1)))))
    (do ((kk (+ (- mt19937-n mt19937-m) 3) (1+ kk)))
	((>= kk (+ (1- mt19937-n) 3)))
      (declare (type (mod 628) kk))
      (setf y (logior (logand (aref state kk) mt19937-upper-mask)
		      (logand (aref state (1+ kk)) mt19937-lower-mask)))
      (setf (aref state kk) (logxor (aref state (+ kk (- mt19937-m mt19937-n)))
				    (ash y -1) (aref state (logand y 1)))))
    (setf y (logior (logand (aref state (+ 3 (1- mt19937-n)))
			    mt19937-upper-mask)
		    (logand (aref state 3) mt19937-lower-mask)))
    (setf (aref state (+ 3 (1- mt19937-n)))
	  (logxor (aref state (+ 3 (1- mt19937-m)))
		  (ash y -1) (aref state (logand y 1)))))
  (values))
#!-x86
(defun random-chunk (state)
  (declare (type random-state state))
  (let* ((state (random-state-state state))
	 (k (aref state 2)))
    (declare (type (mod 628) k))
    (when (= k mt19937-n)
      (random-mt19937-update state)
      (setf k 0))
    (setf (aref state 2) (1+ k))
    (let ((y (aref state (+ 3 k))))
      (declare (type (unsigned-byte 32) y))
      (setf y (logxor y (ash y -11)))
      (setf y (logxor y (ash (logand y (ash mt19937-b -7)) 7)))
      (setf y (logxor y (ash (logand y (ash mt19937-c -15)) 15)))
      (setf y (logxor y (ash y -18)))
      y)))

;;; Using inline VOP support, only available on the x86 so far.
;;;
;;; FIXME: It would be nice to have some benchmark numbers on this.
;;; My inclination is to get rid of the nonportable implementation
;;; unless the performance difference is just enormous.
#!+x86
(defun random-chunk (state)
  (declare (type random-state state))
  (sb!vm::random-mt19937 (random-state-state state)))

;;; Handle the single or double float case of RANDOM. We generate a
;;; float between 0.0 and 1.0 by clobbering the significand of 1.0
;;; with random bits, then subtracting 1.0. This hides the fact that
;;; we have a hidden bit.
#!-sb-fluid (declaim (inline %random-single-float %random-double-float))
(declaim (ftype (function ((single-float (0f0)) random-state)
			  (single-float 0f0))
		%random-single-float))
(defun %random-single-float (arg state)
  (declare (type (single-float (0f0)) arg)
	   (type random-state state))
  (* arg
     (- (make-single-float
	 (dpb (ash (random-chunk state)
		   (- sb!vm:single-float-digits random-chunk-length))
	      sb!vm:single-float-significand-byte
	      (single-float-bits 1.0)))
	1.0)))
(declaim (ftype (function ((double-float (0d0)) random-state)
			  (double-float 0d0))
		%random-double-float))

;;; 32-bit version
#!+nil
(defun %random-double-float (arg state)
  (declare (type (double-float (0d0)) arg)
	   (type random-state state))
  (* (float (random-chunk state) 1d0) (/ 1d0 (expt 2 32))))

;;; 53-bit version
#!-x86
(defun %random-double-float (arg state)
  (declare (type (double-float (0d0)) arg)
	   (type random-state state))
  (* arg
     (- (sb!impl::make-double-float
	 (dpb (ash (random-chunk state)
		   (- sb!vm:double-float-digits random-chunk-length
		      sb!vm:n-word-bits))
	      sb!vm:double-float-significand-byte
	      (sb!impl::double-float-high-bits 1d0))
	 (random-chunk state))
	1d0)))

;;; using a faster inline VOP
#!+x86
(defun %random-double-float (arg state)
  (declare (type (double-float (0d0)) arg)
	   (type random-state state))
  (let ((state-vector (random-state-state state)))
    (* arg
       (- (sb!impl::make-double-float
	   (dpb (ash (sb!vm::random-mt19937 state-vector)
		     (- sb!vm:double-float-digits random-chunk-length
			sb!vm:n-word-bits))
		sb!vm:double-float-significand-byte
		(sb!impl::double-float-high-bits 1d0))
	   (sb!vm::random-mt19937 state-vector))
	  1d0))))

#!+long-float
(declaim #!-sb-fluid (inline %random-long-float))
#!+long-float
(declaim (ftype (function ((long-float (0l0)) random-state) (long-float 0l0))
		%random-long-float))

;;; using a faster inline VOP
#!+(and long-float x86)
(defun %random-long-float (arg state)
  (declare (type (long-float (0l0)) arg)
	   (type random-state state))
  (let ((state-vector (random-state-state state)))
    (* arg
       (- (sb!impl::make-long-float
	   (sb!impl::long-float-exp-bits 1l0)
	   (logior (sb!vm::random-mt19937 state-vector)
		   sb!vm:long-float-hidden-bit)
	   (sb!vm::random-mt19937 state-vector))
	  1l0))))

#!+(and long-float sparc)
(defun %random-long-float (arg state)
  (declare (type (long-float (0l0)) arg)
	   (type random-state state))
  (* arg
     (- (sb!impl::make-long-float
	 (sb!impl::long-float-exp-bits 1l0)	; X needs more work
	 (random-chunk state) (random-chunk state) (random-chunk state))
	1l0)))

;;;; random integers

(defun %random-integer (arg state)
  (declare (type (integer 1) arg) (type random-state state))
  (let ((shift (- random-chunk-length random-integer-overlap)))
    (do ((bits (random-chunk state)
	       (logxor (ash bits shift) (random-chunk state)))
	 (count (+ (integer-length arg)
		   (- random-integer-extra-bits shift))
		(- count shift)))
	((minusp count)
	 (rem bits arg))
      (declare (fixnum count)))))

(defun random (arg &optional (state *random-state*))
  (declare (inline %random-single-float %random-double-float
		   #!+long-float %long-float))
  (cond
    ((and (fixnump arg) (<= arg random-fixnum-max) (> arg 0))
     (rem (random-chunk state) arg))
    ((and (typep arg 'single-float) (> arg 0.0S0))
     (%random-single-float arg state))
    ((and (typep arg 'double-float) (> arg 0.0D0))
     (%random-double-float arg state))
    #!+long-float
    ((and (typep arg 'long-float) (> arg 0.0L0))
     (%random-long-float arg state))
    ((and (integerp arg) (> arg 0))
     (%random-integer arg state))
    (t
     (error 'simple-type-error
	    :expected-type '(or (integer 1) (float (0))) :datum arg
	    :format-control "~@<Argument is neither a positive integer nor a ~
                             positive float: ~2I~_~S~:>"
	    :format-arguments (list arg)))))
