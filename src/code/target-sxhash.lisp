;;;; hashing functions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(defun pointer-hash (key)
  (pointer-hash key))

;;; the depthoid explored when calculating hash values
;;;
;;; "Depthoid" here is a sort of mixture of what Common Lisp ordinarily calls
;;; depth and what Common Lisp ordinarily calls length; it's incremented either
;;; when we descend into a compound object or when we step through elements of
;;; a compound object.
(defconstant +max-hash-depthoid+ 4)

;;;; mixing hash values

;;; a function for mixing hash values
;;;
;;; desiderata:
;;;   * Non-commutativity keeps us from hashing e.g. #(1 5) to the
;;;     same value as #(5 1), and ending up in real trouble in some
;;;     special cases like bit vectors the way that CMUCL 18b SXHASH
;;;     does. (Under CMUCL 18b, SXHASH of any bit vector is 1..)
;;;   * We'd like to scatter our hash values over the entire possible range
;;;     of values instead of hashing small or common key values (like
;;;     2 and NIL and #\a) to small FIXNUMs the way that the CMUCL 18b
;;;     SXHASH function does, again helping to avoid pathologies like
;;;     hashing all bit vectors to 1.
;;;   * We'd like this to be simple and fast, too.
(declaim (ftype (sfunction ((and fixnum unsigned-byte)
                            (and fixnum unsigned-byte))
                           (and fixnum unsigned-byte))
                mix))
(declaim (inline mix))
(defun mix (x y)
  (declare (optimize (speed 3)))
  (declare (type (and fixnum unsigned-byte) x y))
  ;; the ideas here:
  ;;   * Bits diffuse in both directions (shifted arbitrarily left by
  ;;     the multiplication in the calculation of XY, and shifted
  ;;     right by up to 5 places by the ASH).
  ;;   * The #'+ and #'LOGXOR operations don't commute with each other,
  ;;     so different bit patterns are mixed together as they shift
  ;;     past each other.
  ;;   * The arbitrary constant XOR used in the LOGXOR expression is
  ;;     intended to help break up any weird anomalies we might
  ;;     otherwise get when hashing highly regular patterns.
  ;; (These are vaguely like the ideas used in many cryptographic
  ;; algorithms, but we're not pushing them hard enough here for them
  ;; to be cryptographically strong.)
  ;;
  ;; note: 3622009729038463111 is a 62-bit prime such that its low 61
  ;; bits, low 60 bits and low 29 bits are all also primes, thus
  ;; giving decent distributions no matter which of the possible
  ;; values of most-positive-fixnum we have.  It is derived by simple
  ;; search starting from 2^60*pi.  The multiplication should be
  ;; efficient no matter what the platform thanks to modular
  ;; arithmetic.
  (let* ((mul (logand 3622009729038463111 sb!xc:most-positive-fixnum))
         (xor (logand 608948948376289905 sb!xc:most-positive-fixnum))
         (xy (logand (+ (* x mul) y) sb!xc:most-positive-fixnum)))
    (logand (logxor xor xy (ash xy -5)) sb!xc:most-positive-fixnum)))

;; Return a number that increments by 1 for each word-pair allocation,
;; barring complications such as exhaustion of the current page.
;; The result is guaranteed to be a positive fixnum.
(declaim (inline address-based-counter-val))
(defun address-based-counter-val ()
  #!+(and (not sb-thread) cheneygc)
  (ash (sap-int (dynamic-space-free-pointer)) (- (1+ sb!vm:word-shift)))
  ;; dynamic-space-free-pointer increments only when a page is full.
  ;; Using boxed_region directly is finer-grained.
  #!+(and (not sb-thread) gencgc)
  (ash (extern-alien "boxed_region" unsigned-long)
       (- (1+ sb!vm:word-shift)))
  ;; threads imply gencgc. use the per-thread alloc region pointer
  #!+sb-thread
  (ash (sap-int (sb!vm::current-thread-offset-sap
                 sb!vm::thread-alloc-region-slot))
       (- (1+ sb!vm:word-shift))))

;; Return some bits that are dependent on the next address that will be
;; allocated, mixed with the previous state (in case addresses get recycled).
;; This algorithm, used for stuffing a hash-code into instances of CTYPE
;; subtypes, is simpler than RANDOM, and a test of randomness won't
;; measure up as well, but for the intended use, it doesn't matter.
;; CLOS hashes could probably be made to use this.
(defun quasi-random-address-based-hash (state mask)
  (declare (type (simple-array (and fixnum unsigned-byte) (1)) state))
  ;; Ok with multiple threads - No harm, no foul.
  (logand (setf (aref state 0) (mix (address-based-counter-val) (aref state 0)))
          mask))


;;;; hashing strings
;;;;
;;;; Note that this operation is used in compiler symbol table
;;;; lookups, so we'd like it to be fast.
;;;;
;;;; As of 2004-03-10, we implement the one-at-a-time algorithm
;;;; designed by Bob Jenkins (see
;;;; <http://burtleburtle.net/bob/hash/doobs.html> for some more
;;;; information).

(declaim (inline %sxhash-substring))
(defun %sxhash-substring (string &optional (count (length string)))
  ;; FIXME: As in MIX above, we wouldn't need (SAFETY 0) here if the
  ;; cross-compiler were smarter about ASH, but we need it for
  ;; sbcl-0.5.0m.  (probably no longer true?  We might need SAFETY 0
  ;; to elide some type checks, but then again if this is inlined in
  ;; all the critical places, we might not -- CSR, 2004-03-10)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type string string))
  (declare (type index count))
  (macrolet ((set-result (form)
               `(setf result (ldb (byte #.sb!vm:n-word-bits 0) ,form))))
    (let ((result 0))
      (declare (type (unsigned-byte #.sb!vm:n-word-bits) result))
      (unless (typep string '(vector nil))
        (dotimes (i count)
          (declare (type index i))
          (set-result (+ result (char-code (aref string i))))
          (set-result (+ result (ash result 10)))
          (set-result (logxor result (ash result -6)))))
      (set-result (+ result (ash result 3)))
      (set-result (logxor result (ash result -11)))
      (set-result (logxor result (ash result 15)))
      (logand result most-positive-fixnum))))
;;; test:
;;;   (let ((ht (make-hash-table :test 'equal)))
;;;     (do-all-symbols (symbol)
;;;       (let* ((string (symbol-name symbol))
;;;           (hash (%sxhash-substring string)))
;;;      (if (gethash hash ht)
;;;          (unless (string= (gethash hash ht) string)
;;;            (format t "collision: ~S ~S~%" string (gethash hash ht)))
;;;          (setf (gethash hash ht) string))))
;;;     (format t "final count=~W~%" (hash-table-count ht)))

(defun %sxhash-simple-string (x)
  (declare (optimize speed))
  (declare (type simple-string x))
  ;; KLUDGE: this FLET is a workaround (suggested by APD) for presence
  ;; of let conversion in the cross compiler, which otherwise causes
  ;; strongly suboptimal register allocation.
  (flet ((trick (x)
           (%sxhash-substring x)))
    (declare (notinline trick))
    (trick x)))

(defun %sxhash-simple-substring (x count)
  (declare (optimize speed))
  (declare (type simple-string x))
  (declare (type index count))
  ;; see comment in %SXHASH-SIMPLE-STRING
  (flet ((trick (x count)
           (%sxhash-substring x count)))
    (declare (notinline trick))
    (trick x count)))

;;;; the SXHASH function

;; simple cases
(declaim (ftype (sfunction (integer) hash) sxhash-bignum))

(defun new-instance-hash-code ()
  ;; ANSI SXHASH wants us to make a good-faith effort to produce
  ;; hash-codes that are well distributed within the range of
  ;; non-negative fixnums, and this address-based operation does that.
  ;; This is faster than calling RANDOM, and is random enough.
  (loop
   (let ((answer
          (truly-the fixnum
           (quasi-random-address-based-hash
            (load-time-value (make-array 1 :element-type '(and fixnum unsigned-byte))
                             t)
            most-positive-fixnum))))
     (when (plusp answer)
       ;; Make sure we never return 0 (almost no chance of that anyway).
       (return answer)))))

(declaim (inline std-instance-hash))
(defun std-instance-hash (instance)
  (let ((hash (%instance-ref instance sb!pcl::std-instance-hash-slot-index)))
    (if (not (eql hash 0))
        hash
        (let ((new (new-instance-hash-code)))
          ;; At most one thread will compute a random hash.
          ;; %INSTANCE-CAS is a full call if there is no vop for it.
          (let ((old (%instance-cas instance sb!pcl::std-instance-hash-slot-index
                                    0 new)))
            (if (eql old 0) new old))))))

;; These are also random numbers, but not lazily computed.
(declaim (inline fsc-instance-hash))
(defun fsc-instance-hash (fin)
  (%funcallable-instance-info fin sb!pcl::fsc-instance-hash-slot-index))

(defun sxhash (x)
  ;; profiling SXHASH is hard, but we might as well try to make it go
  ;; fast, in case it is the bottleneck somewhere.  -- CSR, 2003-03-14
  (declare (optimize speed))
  (labels ((sxhash-number (x)
             (etypecase x
               (fixnum (sxhash x))      ; through DEFTRANSFORM
               (integer (sb!bignum:sxhash-bignum x))
               (single-float (sxhash x)) ; through DEFTRANSFORM
               (double-float (sxhash x)) ; through DEFTRANSFORM
               #!+long-float (long-float (error "stub: no LONG-FLOAT"))
               (ratio (let ((result 127810327))
                        (declare (type fixnum result))
                        (mixf result (sxhash-number (numerator x)))
                        (mixf result (sxhash-number (denominator x)))
                        result))
               (complex (let ((result 535698211))
                          (declare (type fixnum result))
                          (mixf result (sxhash-number (realpart x)))
                          (mixf result (sxhash-number (imagpart x)))
                          result))))
           (sxhash-recurse (x depthoid)
             (declare (type index depthoid))
             (typecase x
               ;; we test for LIST here, rather than CONS, because the
               ;; type test for CONS is in fact the test for
               ;; LIST-POINTER-LOWTAG followed by a negated test for
               ;; NIL.  If we're going to have to test for NIL anyway,
               ;; we might as well do it explicitly and pick off the
               ;; answer.  -- CSR, 2004-07-14
               (list
                (if (null x)
                    (sxhash x)          ; through DEFTRANSFORM
                    (if (plusp depthoid)
                        (mix (sxhash-recurse (car x) (1- depthoid))
                             (sxhash-recurse (cdr x) (1- depthoid)))
                        261835505)))
               (instance
                (typecase x
                  (pathname
                   ;; Pathnames are EQUAL if all the components are EQUAL, so
                   ;; we hash all of the components of a pathname together.
                   (let ((hash (sxhash-recurse (pathname-host x) depthoid)))
                     (mixf hash (sxhash-recurse (pathname-device x) depthoid))
                     (mixf hash (sxhash-recurse (pathname-directory x) depthoid))
                     (mixf hash (sxhash-recurse (pathname-name x) depthoid))
                     (mixf hash (sxhash-recurse (pathname-type x) depthoid))
                     ;; Hash :NEWEST the same as NIL because EQUAL for
                     ;; pathnames assumes that :newest and nil are equal.
                     (let ((version (%pathname-version x)))
                       (mixf hash (sxhash-recurse (if (eq version :newest)
                                                      nil
                                                      version)
                                                  depthoid)))))
                  (layout
                   ;; LAYOUTs have an easily-accesible hash value: we
                   ;; might as well use it.  It's not actually uniform
                   ;; over the space of hash values (it excludes 0 and
                   ;; some of the larger numbers) but it's better than
                   ;; simply returning the same value for all LAYOUT
                   ;; objects, as the next branch would do.
                   (layout-clos-hash x))
                  ((or structure-object condition)
                   (logxor 422371266
                           ;; FIXME: why not (LAYOUT-CLOS-HASH ...) ?
                           (sxhash      ; through DEFTRANSFORM
                            (classoid-name
                             (layout-classoid (%instance-layout x))))))
                  (t (std-instance-hash x))))
               (symbol (sxhash x))      ; through DEFTRANSFORM
               (array
                (typecase x
                  (simple-string (sxhash x)) ; through DEFTRANSFORM
                  (string (%sxhash-substring x))
                  (simple-bit-vector (sxhash x)) ; through DEFTRANSFORM
                  (bit-vector
                   ;; FIXME: It must surely be possible to do better
                   ;; than this.  The problem is that a non-SIMPLE
                   ;; BIT-VECTOR could be displaced to another, with a
                   ;; non-zero offset -- so that significantly more
                   ;; work needs to be done using the %VECTOR-RAW-BITS
                   ;; approach.  This will probably do for now.
                   (sxhash-recurse (copy-seq x) depthoid))
                  (t (logxor 191020317 (sxhash (array-rank x))))))
               (character
                (logxor 72185131
                        (sxhash (char-code x)))) ; through DEFTRANSFORM
               ;; general, inefficient case of NUMBER
               (number (sxhash-number x))
               (generic-function (fsc-instance-hash x))
               (t 42))))
    (sxhash-recurse x +max-hash-depthoid+)))

;;;; the PSXHASH function

;;;; FIXME: This code does a lot of unnecessary full calls. It could be made
;;;; more efficient (in both time and space) by rewriting it along the lines
;;;; of the SXHASH code above.

;;; like SXHASH, but for EQUALP hashing instead of EQUAL hashing
(defun psxhash (key &optional (depthoid +max-hash-depthoid+))
  (declare (optimize speed))
  (declare (type (integer 0 #.+max-hash-depthoid+) depthoid))
  ;; Note: You might think it would be cleaner to use the ordering given in the
  ;; table from Figure 5-13 in the EQUALP section of the ANSI specification
  ;; here. So did I, but that is a snare for the unwary! Nothing in the ANSI
  ;; spec says that HASH-TABLE can't be a STRUCTURE-OBJECT, and in fact our
  ;; HASH-TABLEs *are* STRUCTURE-OBJECTs, so we need to pick off the special
  ;; HASH-TABLE behavior before we fall through to the generic STRUCTURE-OBJECT
  ;; comparison behavior.
  (typecase key
    (array (array-psxhash key depthoid))
    (hash-table (hash-table-psxhash key))
    (structure-object (structure-object-psxhash key depthoid))
    (cons (list-psxhash key depthoid))
    (number (number-psxhash key))
    (character (char-code (char-upcase key)))
    (t (sxhash key))))

(defun array-psxhash (key depthoid)
  (declare (optimize speed))
  (declare (type array key))
  (declare (type (integer 0 #.+max-hash-depthoid+) depthoid))
  (typecase key
    ;; VECTORs have to be treated specially because ANSI specifies
    ;; that we must respect fill pointers.
    (vector
     (macrolet ((frob ()
                  '(let ((result 572539))
                     (declare (type fixnum result))
                     (mixf result (length key))
                    (when (plusp depthoid)
                      (decf depthoid)
                      (dotimes (i (length key))
                       (declare (type fixnum i))
                       (mixf result
                             (psxhash (aref key i) depthoid))))
                    result))
                (make-dispatch (types)
                  `(typecase key
                     ,@(loop for type in types
                             collect `(,type
                                       (frob))))))
       (make-dispatch (simple-base-string
                       (simple-array character (*))
                       simple-vector
                       (simple-array (unsigned-byte 8) (*))
                       (simple-array fixnum (*))
                       t))))
    ;; Any other array can be hashed by working with its underlying
    ;; one-dimensional physical representation.
    (t
     (let ((result 60828))
       (declare (type fixnum result))
       (dotimes (i (array-rank key))
         (mixf result (array-dimension key i)))
       (when (plusp depthoid)
         (decf depthoid)
         (dotimes (i (array-total-size key))
          (mixf result
                (psxhash (row-major-aref key i) depthoid))))
       result))))

(defun structure-object-psxhash (key depthoid)
  (declare (optimize speed))
  (declare (type structure-object key))
  (declare (type (integer 0 #.+max-hash-depthoid+) depthoid))
  (let* ((layout (%instance-layout key)) ; i.e. slot #0
         ;; Is there some reason the name of the layout's classoid
         ;; should be preferred as the seed, instead of using the CLOS-HASH
         ;; just like SXHASH does?
         (classoid (layout-classoid layout))
         (name (classoid-name classoid))
         (result (mix (sxhash name) (the fixnum 79867))))
    (declare (type fixnum result))
    (when (plusp depthoid)
      (let ((max-iterations depthoid)
            (depthoid (1- depthoid)))
        ;; We don't mix in LAYOUT here because it was already done above.
        (do-instance-tagged-slot (i key :layout layout :pad nil)
          (mixf result (psxhash (%instance-ref key i) depthoid))
          (if (zerop (decf max-iterations)) (return)))))
    ;; [The following comment blurs some issues: indeed it would take
    ;; a second loop in the non-interleaved-slots code; that loop might
    ;; never execute because depthoid "cuts off", although that's an arbitrary
    ;; choice and could be decided otherwise; and efficiency would likely
    ;; demand that we store some additional metadata in the LAYOUT indicating
    ;; how to mix the bits in, because floating-point +/-zeros have to
    ;; be considered EQUALP]
    ;; KLUDGE: Should hash untagged slots, too.  (Although +max-hash-depthoid+
    ;; is pretty low currently, so they might not make it into the hash
    ;; value anyway.)
    result))

(defun list-psxhash (key depthoid)
  (declare (optimize speed))
  (declare (type list key))
  (declare (type (integer 0 #.+max-hash-depthoid+) depthoid))
  (cond ((null key)
         (the fixnum 480929))
        ((zerop depthoid)
         (the fixnum 779578))
        (t
         (mix (psxhash (car key) (1- depthoid))
              (psxhash (cdr key) (1- depthoid))))))

(defun hash-table-psxhash (key)
  (declare (optimize speed))
  (declare (type hash-table key))
  (let ((result 103924836))
    (declare (type fixnum result))
    (mixf result (hash-table-count key))
    (mixf result (sxhash (hash-table-test key)))
    result))

(defun number-psxhash (key)
  (declare (optimize speed))
  (declare (type number key))
  (flet ((sxhash-double-float (val)
           (declare (type double-float val))
           ;; FIXME: Check to make sure that the DEFTRANSFORM kicks in and the
           ;; resulting code works without consing. (In Debian cmucl 2.4.17,
           ;; it didn't.)
           (sxhash val)))
    (etypecase key
      (integer (sxhash key))
      (float (macrolet ((frob (type)
                          (let ((lo (coerce sb!xc:most-negative-fixnum type))
                                (hi (coerce sb!xc:most-positive-fixnum type)))
                            `(cond (;; This clause allows FIXNUM-sized integer
                                    ;; values to be handled without consing.
                                    (<= ,lo key ,hi)
                                    (multiple-value-bind (q r)
                                        (floor (the (,type ,lo ,hi) key))
                                      (if (zerop (the ,type r))
                                          (sxhash q)
                                          (sxhash-double-float
                                           (coerce key 'double-float)))))
                                   (t
                                    (multiple-value-bind (q r) (floor key)
                                      (if (zerop (the ,type r))
                                          (sxhash q)
                                          (sxhash-double-float
                                           (coerce key 'double-float)))))))))
               (etypecase key
                 (single-float (frob single-float))
                 (double-float (frob double-float))
                 #!+long-float
                 (long-float (error "LONG-FLOAT not currently supported")))))
      (rational (if (and (<= most-negative-double-float
                             key
                             most-positive-double-float)
                         (= (coerce key 'double-float) key))
                    (sxhash-double-float (coerce key 'double-float))
                    (sxhash key)))
      (complex (if (zerop (imagpart key))
                   (number-psxhash (realpart key))
                   (let ((result 330231))
                     (declare (type fixnum result))
                     (mixf result (number-psxhash (realpart key)))
                     (mixf result (number-psxhash (imagpart key)))
                     result))))))

;;; Semantic equivalent of SXHASH, but better-behaved for function names.
;;; It performs more work by not cutting off as soon in the CDR direction.
;;; More work here equates to less work in the global hashtable.
;;; To wit: (eq (sxhash '(foo a b c bar)) (sxhash '(foo a b c d))) => T
;;; but the corresponding globaldb-sxhashoids differ.
(defun sb!c::globaldb-sxhashoid (name)
  (locally
      (declare (optimize (safety 0))) ; after the argc check
    ;; TRAVERSE will walk across more cons cells than RECURSE will descend.
    ;; That's why this isn't just one self-recursive function.
    (labels ((traverse (accumulator x length-limit)
               (declare (fixnum length-limit))
               (cond ((atom x) (mix (sxhash x) accumulator))
                     ((zerop length-limit) accumulator)
                     (t (traverse (mix (recurse (car x) 4) accumulator)
                                  (cdr x) (1- length-limit)))))
             (recurse (x depthoid) ; depthoid = a blend of level and length
               (declare (fixnum depthoid))
               (cond ((atom x) (sxhash x))
                     ((zerop depthoid)
                      #.(logand sb!xc:most-positive-fixnum #36Rglobaldbsxhashoid))
                     (t (mix (recurse (car x) (1- depthoid))
                             (recurse (cdr x) (1- depthoid)))))))
      (traverse 0 name 10))))
