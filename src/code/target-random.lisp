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

(def!method print-object ((state random-state) stream)
  (if (and *print-readably* (not *read-eval*))
      (error 'print-not-readable :object state)
      (format stream "#S(~S ~S #.~S)"
              'random-state
              ':state
              `(make-array 627
                :element-type
                '(unsigned-byte 32)
                :initial-contents
                ',(coerce (random-state-state state) 'list)))))

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
  "Make a RANDOM-STATE object. If STATE is not supplied, return a copy
  of the default random state. If STATE is a random state, then return a
  copy of it. If STATE is T then return a random state generated from
  the universal time."
  (/show0 "entering MAKE-RANDOM-STATE")
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
                   (- sb!vm:single-float-digits n-random-chunk-bits))
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
                   (- sb!vm:double-float-digits n-random-chunk-bits 32))
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
                     (- sb!vm:double-float-digits n-random-chunk-bits
                        sb!vm:n-word-bits))
                sb!vm:double-float-significand-byte
                (sb!impl::double-float-high-bits 1d0))
           (sb!vm::random-mt19937 state-vector))
          1d0))))


;;;; random integers

;;; a bitmask M wide enough that (= (LOGAND INCLUSIVE-LIMIT M) INCLUSIVE-LIMIT)
(declaim (inline %inclusive-random-integer-mask))
(defun %inclusive-random-integer-mask (inclusive-limit)
  (1- (ash 1 (integer-length inclusive-limit))))

;;; Transform a uniform sample from an at-least-as-large range into a
;;; random sample in [0,INCLUSIVE-LIMIT] range by throwing away
;;; too-big samples.
;;;
;;; Up through sbcl-1.0.4, the (RANDOM INTEGER) worked by taking (MOD
;;; RAW-MERSENNE-OUTPUT INTEGER). That introduces enough bias that it
;;; is unsuitable for some calculations (like the Metropolis Monte
;;; Carlo simulations that I, WHN, worked on in grad school): in the
;;; sbcl-1.0.4, the expectation value of a sample was (in the worst
;;; part of the range) more than 1.0001 times the ideal expectation
;;; value. Perhaps that was even ANSI-conformant, because ANSI says only
;;;   An approximately uniform choice distribution is used. If
;;;   LIMIT is an integer, each of the possible results occurs
;;;   with (approximate) probability 1/LIMIT.
;;; But I (WHN again) said "yuck," so these days we try to get a
;;; truly uniform distribution by translating RAW-MERSENNE-OUTPUT to
;;; our output using the second method recommended by
;;; <http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/efaq.html>:
;;;   In the rare case that the rounding-off error becomes a problem,
;;;   obtain minimum integer n such that N<=2^n, generate integer
;;;   random numbers, take the most significant n bits, and discard
;;;   those more than or equal to N.
;;; (The first method recommended there differs from the sbcl-1.0.4
;;; method: the recommended method gives slightly different weights
;;; to different integers, but distributes the overweighted integers
;;; evenly across the range of outputs so that any skew would be
;;; of order (/ MOST-POSITIVE-MACHINE-WORD), rather than the (/ 10000)
;;; or so achieved by sbcl-1.0.4. That skew would probably be
;;; acceptable --- it seems to be exactly the kind of deviation that
;;; might have been anticipated in the ANSI CL standard. However, that
;;; recommended calculation involves multiplication and division of
;;; two-machine-word bignums, which is hard for us to do efficiently
;;; here. Without special low-level hacking to support such short
;;; bignums without consing, the accept-reject solution is not only
;;; more exact, but also likely more efficient.)
(defmacro %inclusive-random-integer-accept-reject (raw-mersenne-output-expr
                                                   inclusive-limit)
  (with-unique-names (raw-mersenne-output inclusive-limit-once)
  `(loop
    with ,inclusive-limit-once = ,inclusive-limit
    for ,raw-mersenne-output = ,raw-mersenne-output-expr
    until (<= ,raw-mersenne-output ,inclusive-limit-once)
    finally (return ,raw-mersenne-output))))

;;; an UNSIGNED-BYTE of N-CHUNKS chunks sampled from the Mersenne twister
(declaim (inline %random-chunks))
(defun %random-chunks (n-chunks state)
  ;; KLUDGE: This algorithm will cons O(N^2) words when constructing
  ;; an N-word result. To do better should be fundamentally easy, we
  ;; just need to do some low-level hack preallocating the bignum and
  ;; writing its words one by one.
  ;;
  ;; Note: The old sbcl-1.0.4 code did its analogous operation using a
  ;; mysterious RANDOM-INTEGER-OVERLAP parameter, "the amount that we
  ;; overlap chunks by when building a large integer to make up for
  ;; the loss of randomness in the low bits." No such thing is called
  ;; for on
  ;;   <http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/efaq.html>:
  ;; they say there that it's OK just to concatenate words of twister
  ;; output with no overlap. Thus, crossing our fingers and hoping that
  ;; the previous RNG author didn't have some subtle reason to need
  ;; RANDOM-INTEGER-OVERLAP that we know not of, we just concatenate
  ;; chunks.
  (loop repeat n-chunks
        for result = 0 then (logior (ash result n-random-chunk-bits)
                                    (random-chunk state))
        finally (return result)))

;;; an UNSIGNED-BYTE of N-BITS bits sampled from the Mersenne twister
(declaim (inline %random-bits))
(defun %random-bits (n-bits state)
  (multiple-value-bind (n-full-chunks n-extra-bits)
      (floor n-bits n-random-chunk-bits)
    (let ((full-chunks (%random-chunks n-full-chunks state)))
      (if (zerop n-extra-bits)
          full-chunks
          (logior full-chunks
                  (ash (logand (random-chunk state)
                               (1- (ash 1 n-extra-bits)))
                       (* n-full-chunks n-random-chunk-bits)))))))

;;; the guts of (RANDOM (1+ INCLUSIVE-LIMIT))
(defun %inclusive-random-integer (inclusive-limit state)
  (declare (optimize speed (space 1))) ; to ensure DEFTRANSFORM is enabled
  (%inclusive-random-integer inclusive-limit state))

;;; the guts of RANDOM for the known-to-return-FIXNUM case
;;;
;;; We use INCLUSIVE-LIMIT instead of the (exclusive) LIMIT of RANDOM,
;;; because we want it to be a FIXNUM even for the possibly-common
;;; case of (RANDOM (1+ MOST-POSITIVE-FIXNUM)). (That case is what
;;; one might use for generating random hash values, e.g.)
;;; It also turns out to be just what's needed for INTEGER-LENGTH.
(declaim (maybe-inline %inclusive-random-fixnum))
(defun %inclusive-random-fixnum (inclusive-limit state)
  (declare (type (and fixnum unsigned-byte) inclusive-limit))
  (aver (<= inclusive-limit most-positive-random-chunk))
  (let (;; If this calculation needs to be optimized further, a good
        ;; start might be a DEFTRANSFORM which picks off the case of
        ;; constant LIMIT and precomputes the MASK at compile time.
        (mask (%inclusive-random-integer-mask inclusive-limit)))
    (%inclusive-random-integer-accept-reject (logand (random-chunk state) mask)
                                             inclusive-limit)))

;;;; outer, dynamically-typed interface

(defun random (arg &optional (state *random-state*))
  (declare (inline %random-single-float
                   %random-double-float
                   #!+long-float %random-long-float))
  (cond
    ((and (fixnump arg) (plusp arg))
     (locally
         ;; The choice to inline this very common case of
         ;; %INCLUSIVE-RANDOM-FIXNUM and not the less-common call
         ;; below was just a guess (by WHN 2007-03-27), not based on
         ;; benchmarking or anything.
         (declare (inline %inclusive-random-fixnum))
       (%inclusive-random-fixnum (1- arg) state)))
    ((and (typep arg 'single-float) (> arg 0.0f0))
     (%random-single-float arg state))
    ((and (typep arg 'double-float) (> arg 0.0d0))
     (%random-double-float arg state))
    #!+long-float
    ((and (typep arg 'long-float) (> arg 0.0l0))
     (%random-long-float arg state))
    ((and (integerp arg) (plusp arg))
     (if (= arg (1+ most-positive-fixnum))
         (%inclusive-random-fixnum most-positive-fixnum state)
         (%inclusive-random-integer (1- arg) state)))
    (t
     (error 'simple-type-error
            :expected-type '(or (integer 1) (float (0))) :datum arg
            :format-control "~@<Argument is neither a positive integer nor a ~
                             positive float: ~2I~_~S~:>"
            :format-arguments (list arg)))))
