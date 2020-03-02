;;;; hashing functions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

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
  (let* ((mul (logand 3622009729038463111 sb-xc:most-positive-fixnum))
         (xor (logand 608948948376289905 sb-xc:most-positive-fixnum))
         (xy (logand (+ (* x mul) y) sb-xc:most-positive-fixnum)))
    (logand (logxor xor xy (ash xy -5)) sb-xc:most-positive-fixnum)))

;;; Same as above, but don't mask computations to n-positive-fixnum-bits.
(declaim (inline word-mix))
(defun word-mix (x y)
  (declare (optimize (speed 3)))
  (declare (type word x y))
  (declare (muffle-conditions compiler-note))
  (let* ((mul (logand 3622009729038463111 most-positive-word))
         (xor (logand 608948948376289905 most-positive-word))
         (xy (logand (+ (* x mul) y) most-positive-word)))
    (logand (logxor xor xy (ash xy -5)) most-positive-word)))

;; Return a number that increments by 1 for each word-pair allocation,
;; barring complications such as exhaustion of the current page.
;; The result is guaranteed to be a positive fixnum.
(declaim (inline address-based-counter-val))
(defun address-based-counter-val ()
  #+(and (not sb-thread) cheneygc)
  (ash (sap-int (dynamic-space-free-pointer)) (- (1+ sb-vm:word-shift)))
  ;; dynamic-space-free-pointer increments only when a page is full.
  ;; Using boxed_region directly is finer-grained.
  #+(and (not sb-thread) gencgc)
  (progn #+(or arm arm64 riscv x86 x86-64 ppc ppc64) ; new way: alloc_region is in static space
         (ash (sb-sys:sap-ref-word (sb-sys:int-sap sb-vm:static-space-start)
                                   (* 2 sb-vm:n-word-bytes))
              (- (1+ sb-vm:word-shift)))
         #-(or arm arm64 riscv x86 x86-64 ppc ppc64) ; old way: alloc_region is in C data
         (ash (extern-alien "gc_alloc_region" unsigned-long)
              (- (1+ sb-vm:word-shift))))
  ;; threads imply gencgc. use the per-thread alloc region pointer
  #+sb-thread
  (ash (sap-int (sb-vm::current-thread-offset-sap
                 #.sb-vm::thread-alloc-region-slot))
       (- (1+ sb-vm:word-shift))))

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


;;; This is an out-of-line callable entrypoint that the compiler can
;;; transform SXHASH into when hashing a non-simple string.
(defun %sxhash-string (x)
  (declare (optimize speed) (type string x))
  (multiple-value-bind (string start end)
      (if (array-header-p x)
          (with-array-data ((string x) (start) (end) :check-fill-pointer t)
            (values string start end))
          (values x 0 (length x)))
    ;; I'm not sure I believe the comment about FLET TRICK being needed.
    ;; The generated code seems tight enough, and the comment is, after all,
    ;; >14 years old.
    (%sxhash-simple-substring string start end)))

;;;; the SXHASH function

;; simple cases
(declaim (ftype (sfunction (integer) hash-code) sxhash-bignum))

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
            sb-xc:most-positive-fixnum))))
     (when (plusp answer)
       ;; Make sure we never return 0 (almost no chance of that anyway).
       (return answer)))))

(declaim (inline !condition-hash))
(defun !condition-hash (instance)
  (let ((hash (sb-kernel::condition-hash instance)))
    (if (not (eql hash 0))
        hash
        (let ((new (new-instance-hash-code)))
          ;; At most one thread will compute a random hash.
          (let ((old (cas (sb-kernel::condition-hash instance) 0 new)))
            (if (eql old 0) new old))))))

#+(and compact-instance-header x86-64)
(progn
  (declaim (inline %std-instance-hash))
  (defun %std-instance-hash (slots) ; return or compute the 32-bit hash
    (let ((stored-hash (sb-vm::get-header-data-high slots)))
      (if (eql stored-hash 0)
          (let ((new (logand (new-instance-hash-code) #xFFFFFFFF)))
            (let ((old (sb-vm::cas-header-data-high slots 0 new)))
              (if (eql old 0) new old)))
          stored-hash))))

(defun std-instance-hash (instance)
  ;; Apparently we care that the object is of primitive type INSTANCE, but not
  ;; whether it is STANDARD-INSTANCE. It had better be, or we're in trouble.
  (declare (instance instance))
  #+(and compact-instance-header x86-64)
  ;; The one logical slot (excluding layout) in the primitive object is index 0.
  ;; That holds a vector of the clos slots, and its header holds the hash.
  (let* ((slots (%instance-ref instance 0))
         (hash (%std-instance-hash slots)))
    ;; Simulate N-POSITIVE-FIXNUM-BITS of output for backward-compatibility,
    ;; in case people use the high order bits.
    ;; (There are only 32 bits of actual randomness, if even that)
    (logxor (ash hash (- sb-vm:n-positive-fixnum-bits 32)) hash))
  #-(and compact-instance-header x86-64)
  (locally
   (declare (optimize (sb-c::type-check 0)))
   (let ((hash (sb-pcl::standard-instance-hash-code instance)))
     (if (not (eql hash 0))
         hash
         (let ((new (new-instance-hash-code)))
          ;; At most one thread will compute a random hash.
          (let ((old (cas (sb-pcl::standard-instance-hash-code instance) 0 new)))
            (if (eql old 0) new old)))))))

;; These are also random numbers, but not lazily computed.
(declaim (inline fsc-instance-hash))
(defun fsc-instance-hash (fin)
  ;; As above, we care that the object is of primitive type FUNCTION, but not
  ;; whether it is STANDARD-FUNCALLABLE-INSTANCE. Let's assume it is.
  (declare (function fin))
  (locally
   (declare (optimize (sb-c::type-check 0)))
   #+compact-instance-header
   (sb-vm::get-header-data-high
    (sb-pcl::standard-funcallable-instance-clos-slots fin))
   #-compact-instance-header
   (sb-pcl::standard-funcallable-instance-hash-code fin)))

(declaim (inline integer-sxhash))
(defun integer-sxhash (x)
  (if (fixnump x) (sxhash (truly-the fixnum x)) (sb-bignum:sxhash-bignum x)))

(defun number-sxhash (x)
  (declare (optimize (sb-c::verify-arg-count 0) speed))
  (labels ((hash-ratio (x)
             (let ((result 127810327))
               (declare (type fixnum result))
               (mixf result (integer-sxhash (numerator x)))
               (mixf result (integer-sxhash (denominator x)))
               result))
           (hash-rational (x)
             (if (ratiop x)
                 (hash-ratio x)
                 (integer-sxhash x))))
    (macrolet ((hash-complex-float (type)
                 `(let ((result 535698211))
                    (declare (type fixnum result))
                    (mixf result (sxhash (truly-the ,type (realpart x))))
                    (mixf result (sxhash (truly-the ,type (imagpart x))))
                    result)))
      (typecase x
        (fixnum (sxhash x)) ; (Should be picked off by main SXHASH)
        (integer (sb-bignum:sxhash-bignum x))
        (single-float (sxhash x)) ; through DEFTRANSFORM
        (double-float (sxhash x)) ; through DEFTRANSFORM
        #+long-float (long-float (error "stub: no LONG-FLOAT"))
        (ratio (hash-ratio x))
        #+long-float
        ((complex long-float) (hash-complex-float long-float))
        ((complex double-float) (hash-complex-float double-float))
        ((complex single-float) (hash-complex-float single-float))
        ((complex rational)
         (let ((result 535698211))
           (declare (type fixnum result))
           (mixf result (hash-rational (imagpart x)))
           (mixf result (hash-rational (realpart x)))
           result))
        (t 0)))))

(clear-info :function :inlinep 'integer-sxhash)

(defun sxhash (x)
  ;; profiling SXHASH is hard, but we might as well try to make it go
  ;; fast, in case it is the bottleneck somewhere.  -- CSR, 2003-03-14
  ;; So, yes, profiling is a little tough but not impossible with some added
  ;; instrumentation in each stanza of the COND, either manually or
  ;; automagically. Based on a manual approach, the order of the tests below
  ;; are now better arranged by approximate descending frequency in terms
  ;; of calls observed in certain test. Regardless of the fact that applications
  ;; will vary by use-cases, this seems like a good order because:
  ;;  * despite that INSTANCE is often the 2nd-most common object type in the heap
  ;;    (right behind CONS), there are probably at least as many heap words
  ;;    that are FIXNUM as instance pointers. So it stands to reason that
  ;;    SXHASH-RECURSE is invoked very often on FIXNUM.
  ;;  * SYMBOLs are extremely common as table keys, more so than INSTANCE,
  ;;    so we should pick off SYMBOL sooner than INSTANCE as well.
  ;;  * INSTANCE (except for PATHNAME) doesn't recurse anyway - in fact
  ;;    it is particularly dumb (by design), so performing that test later
  ;;    doesn't incur much of a penalty. And our users probably know that
  ;;    SXHASH on instance doesn't really do anything.
  ;; Anyway, afaiu, the code below was previously ordered by gut feeling
  ;; rather than than actual measurement, so having any rationale for ordering
  ;; is better than having no rationale. And as a further comment observes,
  ;; we could do away with the question of order if only we had jump tables.
  ;; (Also, could somebody perhaps explain how these magic numbers were chosen?)
  (declare (optimize speed))
  (labels ((sxhash-recurse (x depthoid)
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
               (symbol (sxhash x)) ; through DEFTRANSFORM
               (fixnum (sxhash x)) ; through DEFTRANSFORM
               (instance
                (case (layout-flags (%instance-layout x))
                  (#.(logior +pathname-layout-flag+ +structure-layout-flag+)
                   ;; Pathnames are EQUAL if all the components are EQUAL, so
                   ;; we hash all of the components of a pathname together.
                   (let ((hash (sxhash-recurse (pathname-host x) depthoid)))
                     (mixf hash (sxhash-recurse (pathname-device x) depthoid))
                     (mixf hash (%pathname-dir-hash x))
                     (mixf hash (%pathname-stem-hash x))
                     ;; Hash :NEWEST the same as NIL because EQUAL for
                     ;; pathnames assumes that :newest and nil are equal.
                     (let ((version (%pathname-version x)))
                       (mixf hash (sxhash-recurse (if (eq version :newest)
                                                      nil
                                                      version)
                                                  depthoid)))))
                  (#.+structure-layout-flag+
                   (typecase x
                     (layout (layout-clos-hash x))
                     (t (logxor 422371266
                                (layout-clos-hash (%instance-layout x))))))
                  (#.+condition-layout-flag+ (!condition-hash x))
                  (#.+pcl-object-layout-flag+ (std-instance-hash x))
                  (t 0))) ; can't get here
               (array
                (typecase x
                  ;; If we could do something smart for widetag-based jump tables,
                  ;; then we wouldn't have to think so much about whether to test
                  ;; STRING and BIT-VECTOR inside or outside of the ARRAY stanza.
                  ;; The code is structured now to narrow down in broad strokes with
                  ;; the outer typecase, and then refines the type further.
                  ;; We could equally well move the STRING test into the outer
                  ;; typecase, but that would impart one more test in front of
                  ;; all remaining stanzas.
                  (string (%sxhash-string x))
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
               ;; general, inefficient case of NUMBER
               ;; There's a spurious FIXNUMP test here, as we've already picked it off.
               ;; Maybe the NUMBERP emitter could be informed that X can't be a fixnum,
               ;; because writing this case as (OR BIGNUM RATIO FLOAT COMPLEX)
               ;; produces far worse code.
               (number (number-sxhash x))
               (character
                (logxor 72185131
                        (sxhash (char-code x)))) ; through DEFTRANSFORM
               (funcallable-instance
                (if (layout-for-std-class-p
                     (%funcallable-instance-layout x))
                    (fsc-instance-hash x)
                    ;; funcallable structure, not funcallable-standard-object
                    9550684))
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
    (pathname (sxhash key))
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
                  `(let ((result 572539))
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
         (mixf result (%array-dimension key i)))
       (when (plusp depthoid)
         (decf depthoid)
         (with-array-data ((key key) (start) (end))
           (let ((getter (truly-the function (svref %%data-vector-reffers%%
                                                    (%other-pointer-widetag key)))))
             (loop for i from start below end
                   do (mixf result
                            (psxhash (funcall getter key i) depthoid))))))
       result))))

(defun structure-object-psxhash (key depthoid)
  (declare (optimize speed))
  (declare (type structure-object key))
  (declare (type (integer 0 #.+max-hash-depthoid+) depthoid))
  (let* ((layout (%instance-layout key))
         (result (layout-clos-hash layout)))
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
  (declare (type number key)
           (muffle-conditions compiler-note)
           (optimize speed))
  (flet ((sxhash-double-float (val)
           (declare (type double-float val))
           ;; FIXME: Check to make sure that the DEFTRANSFORM kicks in and the
           ;; resulting code works without consing. (In Debian cmucl 2.4.17,
           ;; it didn't.)
           (sxhash val)))
    (macrolet ((hash-float (type key)
                 (let ((lo (coerce sb-xc:most-negative-fixnum type))
                       (hi (coerce sb-xc:most-positive-fixnum type)))
                   `(let ((key ,key))
                      (cond ( ;; This clause allows FIXNUM-sized integer
                             ;; values to be handled without consing.
                             (<= ,lo key ,hi)
                             (multiple-value-bind (q r)
                                 (floor (the (,type ,lo ,hi) key))
                               (if (zerop (the ,type r))
                                   (sxhash q)
                                   (sxhash-double-float
                                    (coerce key 'double-float)))))
                            ((float-infinity-p key)
                             ;; {single,double}-float infinities are EQUALP
                             (if (minusp key)
                                 (sxhash sb-ext:single-float-negative-infinity)
                                 (sxhash sb-ext:single-float-positive-infinity)))
                            (t
                             (multiple-value-bind (q r) (floor key)
                               (if (zerop (the ,type r))
                                   (sxhash q)
                                   (sxhash-double-float
                                    (coerce key 'double-float)))))))))
               (hash-complex (&optional (hasher '(number-psxhash)))
                 `(if (zerop (imagpart key))
                      (,@hasher (realpart key))
                      (let ((result 330231))
                        (declare (type fixnum result))
                        (mixf result (,@hasher (realpart key)))
                        (mixf result (,@hasher (imagpart key)))
                        result))))
     (etypecase key
       (integer (sxhash key))
       (float (macrolet ()
                (etypecase key
                  (single-float (hash-float single-float key))
                  (double-float (hash-float double-float key))
                  #+long-float
                  (long-float (error "LONG-FLOAT not currently supported")))))
       (rational (if (and (<= sb-xc:most-negative-double-float
                              key
                              sb-xc:most-positive-double-float)
                          (= (coerce key 'double-float) key))
                     (sxhash-double-float (coerce key 'double-float))
                     (sxhash key)))
       ((complex double-float)
        (hash-complex (hash-float double-float)))
       ((complex single-float)
        (hash-complex (hash-float single-float)))
       ((complex rational)
        (hash-complex))))))

;;; Semantic equivalent of SXHASH, but better-behaved for function names.
;;; It performs more work by not cutting off as soon in the CDR direction.
;;; More work here equates to less work in the global hashtable.
;;; To wit: (eq (sxhash '(foo a b c bar)) (sxhash '(foo a b c d))) => T
;;; but the corresponding globaldb-sxhashoids differ.
(defun globaldb-sxhashoid (name)
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
                      #.(logand sb-xc:most-positive-fixnum #36Rglobaldbsxhashoid))
                     (t (mix (recurse (car x) (1- depthoid))
                             (recurse (cdr x) (1- depthoid)))))))
      (traverse 0 name 10))))

;;; Not needed post-build
(clear-info :function :inlining-data '%sxhash-simple-substring)
