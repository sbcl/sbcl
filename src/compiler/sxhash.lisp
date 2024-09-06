;;;; that part of SXHASH logic which runs not only in the target Lisp but
;;;; in the cross-compilation host Lisp

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;; CAUTION: transforms are selected in the *reverse* order of definition,
;;; so define the most general first, followed by more specific.
;;; I once tried to fix that glitch by using APPEND instead of PUSH into
;;; FUN-INFO-TRANSFORMS, and of course it broke things because we depend on such
;;; stupidity. It would be remedied by reversing all definitions whenever
;;; it matters but I didn't feel like figuring out all places where it does.

(deftransform sxhash ((x) (t))
  (let ((type (lvar-type x)))
    ;; It is common for structure slots to have a :TYPE resembling (OR STRING NULL),
    ;; and also common to create custom hash calculations on structures with such slots.
    ;; So it makes sense for the compiler to try to pick off cases where the slot type
    ;; has a specialized hash computation via sxhash after picking off NIL.
    (or (dolist (case '((simple-string . %sxhash-simple-string)
                        (string . %sxhash-string)
                        (simple-bit-vector . %sxhash-simple-bit-vector)
                        (bit-vector . %sxhash-bit-vector)))
          (cond ((csubtypep type (specifier-type (car case)))
                 (return `(,(cdr case) x)))
                ((csubtypep type (specifier-type `(or null ,(car case))))
                 (return `(if x (,(cdr case) x) ,(sb-xc:sxhash nil))))))
        (give-up-ir1-transform))))

(deftransform sxhash ((x) (number)) `(sb-impl::number-sxhash x))
(deftransform sxhash ((x) (integer)) `(sb-impl::integer-sxhash x))

;;; Note about signed zeros with respect to SXHASH (but not PSXHASH!) -

;;; Change b0a51fec91 added some logic to discard the sign of floating-point zeros
;;; before computing SXHASH. Its reasoning was based on the notion of "similarity".
;;; But if +/-0 were truly "similar" as per the definition
;;; at http://www.lispworks.com/documentation/HyperSpec/Body/03_bdbb.htm
;;; then it causes a massive semantic problem: it essentially would mean that
;;; when used as first class objects in code compiled not to memory,
;;; literal floating-point zeros might have their sign bit ignored.
;;;
;;; i.e. if you hang your hat on "similarity", then the literals externalized
;;; in (DEFUN F () (VALUES -0d0 0d0))
;;; might cause F to return two pointers to the identical object, or not,
;;; because, after all, they're similar values, aren't they?
;;;
;;; Well, we know it returns different values, thus they must be non-similar.
;;; Of course, you could also argue that similar values don't *have* to be
;;; collapsed into one object - they simply _may_ be one object,
;;; but this completely misses the point.
;;;
;;; The crux of the question is: if they are similar, then would it be legal
;;; to return the positive zero where you wrote the negative zero? Yes, it would
;;; be. But that's ludicrous. Therefore they must be non-similar.
;;;
;;; Taking it from the top, the reason cited in the prior change is that SXHASH
;;; respects similarity. OK, then, what does the SXHASH writeup actually say?
;;;  The manner in which the hash code is computed is implementation-dependent,
;;;  but subject to certain constraints:
;;;  1. (equal x y) implies (= (sxhash x) (sxhash y)).
;;;  2. For any two objects, x and y, both of which are [...] numbers [...]
;;;     which are similar, (sxhash x) and (sxhash y) yield the same mathematical
;;;     value even if x and y exist in different Lisp images ...
;;;
;;; You have to look at SXHASH bullet points as to intent, and the definition of
;;; similarity as to intent.
;;;
;;; Point (1) says that SXHASH should make a distinction between objects when EQUAL
;;; does, and not more. It's like an "only-if" in that:
;;;  - SXHASH must not make distinctions that EQUAL does not make if EQUAL said T.
;;;    i.e. you must not hash to different things if EQUAL says they're the same.
;;;  - it is always legal to make fewer distinctions than EQUAL makes,
;;;    which is why all structure instances may hash to a constant.
;;;  - SXHASH might make distinctions when EQUAL says NIL, but nobody should care
;;;    about what exactly those distinctions are.
;;;
;;; Point (2) is concerned with predictability of hash values for a certain domain
;;; of objects, such that if you exit an image, and either restart it, or start up
;;; a completely different image, then _if_ two objects would satisfy the
;;; similarity test, they will hash to the same value.

;;; The remaining few points say that the behavior is predictable within a session,
;;; it terminates in the presence of circularity, and is intended for hashing.

;;; But the aforementioned change made two mistakes:
;;; - it assumed that the definition of "similarity" of floating-point zeroes
;;;   implies that +0f0 is similar to -0f0, respectively for double-float.
;;; - it assumed that we care what SXHASH says when EQUAL says NIL for numbers
;;;   when in fact it is overwhelmingly clear that the intent of similarity
;;    regarding numbers is to capture the behavior of EQL, and not EQUALP or =.
;;; A reductio ad absurdum argument shows that the first assumption is false.

;;; The main defining aspect of SXHASH is that it agrees with EQUAL which is like
;;; EQL, not EQUALP (or =) for numbers. The text for "similar" bears some semblance to
;;; the footnote to '=' where it has an adjective "mathematical", but '=' says in its
;;; main body text that it performs "arithmetic" comparison. "Similar" does not say that.
;;; The = footnote is clarifying why it is not the same as EQL with respect to signed
;;; zeroes. You can not infer from it that "similar" entails arithmetic equality
;;; in the exact sense of '='. Mention of "mathematical" value at "similar" is meant to
;;; distinguish itself from EQ comparison. This is obvious because plenty of functions
;;; use a "mathematical" value of a signed zero, and make distinctions between them,
;;; which is to say, just because a definition inserts the adjective "mathematical"
;;; you don't get to drop the sign bit of a zero as we seem to have done.
;;; Pretty much it should have said under "similar" to compare numbers as if by EQL.
;;; Not to mention, externalizing a signaling NaN should not be allowed to compare
;;; on anything except the bits, so EQL is legal to call but = isn't.
;;;
;;; You can't then leap from an already slightly wrong treatment of the similarity
;;; notion for numbers to conclude that +/-0.0 hash the same, when even if they did,
;;; only an EQUALP table would have (eq (gethash +0.0 tbl) (gethash -0.0 tbl)).
;;; So unless someone is seriously going to argue that similarity of numbers implies
;;; that the distinction between +0 and -0 makes no difference *ANYWHERE* that uses
;;; floating-point literals, I think there is no argument here.

;;; SXHASH of FLOAT values is defined directly in terms of DEFTRANSFORM in
;;; order to avoid boxing.
(deftransform sxhash ((x) (single-float)) '#.(sxhash-single-float-xform 'x))

;;; SXHASH of FIXNUM values is defined as a DEFTRANSFORM because it's so
;;; simple.
(deftransform sxhash ((x) (fixnum)) '#.(sxhash-fixnum-xform 'x))

(deftransform sxhash ((x) (double-float)) '#.(sxhash-double-float-xform 'x))

;;; All symbols have a precomputed hash.
;;; Here are the behaviors I plan on implementing:
;;; 32-bit:
;;;   * SYMBOL-NAME-HASH, SYMBOL-HASH, and (SXHASH sym) are all the same.
;;;
;;; x86-64 (initially, then others as I am able):
;;;   * SYMBOL-HASH returns 40 significant bits, the low 8 being pseudorandom
;;;   * SYMBOL-NAME-HASH returns 32 significant bits
;;;   * (SXHASH sym) uses the name hash mixed with itself
;;; Given only 32 bits of name-based hash, we will have call MIX to make it appear that
;;; the values of SXHASH are "well distributed within the range of non-negative fixnums"
;;; (per CLHS) even though it doesn't add any more entropy to do that.
;;;
;;; not-x86-64 (until I do them):
;;;   * SYMBOL-HASH returns all hash bits, but no bits are pseudorandom
;;;   * SYMBOL-NAME-HASH returns the low 32 bits
;;;   * (SXHASH sym) returns symbol-hash
;;;
;;; The use-cases are as follows:
;;; - SYMBOL-HASH is for hash-tables, for XSET membership testing (which abhors collisions),
;;;   for mapping slot names to slot indices in PCL, and/or anything else wanting an opaque
;;;   pseudorandom value. There is literally no reason that symbols spelled the same must
;;;   hash to the same numerical value in most situations.
;;;
;;; - SYMBOL-NAME-HASH is for compile-time computation of hash-based lookup tables since it
;;;   deterministic, and meets the CLHS requirement for SXHASH, and has at most 32 significant
;;;   bits for feeding into the Jenkins perfect hash generator.
;;;
;;; - SXHASH is required by the language to have behavior that precludes randomizing the
;;;   hash, and encourages using all the range of positive fixnums.
;;;
(deftransform sxhash ((x) (symbol)) '#.(sxhash-symbol-xform 'x))

(deftransform hash-as-if-symbol-name ((object) (symbol) * :important nil)
  `(symbol-name-hash object))

(intern "SCRAMBLE" "SB-C")
(intern "TAB" "SB-C")

(defun ub32-collection-uniquep (array)
  (declare (type (simple-array (unsigned-byte 32) (*)) array))
  (let ((dedup (alloc-xset)))
    (dovector (x array t)
      (when (xset-member-p x dedup)
        (return-from ub32-collection-uniquep nil))
      (add-to-xset x dedup))))

;;; This cache is not used for cross-compiling because there's already
;;; another cache-like layer (the journal files)
(defglobal *phash-lambda-cache* nil)

;;; MINIMAL (the default) returns a a function that returns an output
;;; in the range 0..N-1 for N possible symbols. If non-minimal, the output
;;; range is the power-of-2-ceiling of N.
;;; It could be worth using a non-minimal perfect hash if it avoids needing
;;; a lookup table and can be done entirely with arithmetic and logical ops.
;;; That seldom seems to be the case.
;;; FAST should make the generator try less hard to do a good job.
;;; Practically speaking it does not often make the generator run faster,
;;; and it might run slower! In no way does it say anything about the speed
;;; of the generated function. So you pretty much don't want to supply it.
;;; Indeed one should avoid passing either optional arg to this function.
(defun make-perfect-hash-lambda (array &optional objects
                                       (minimal t) (fast nil)
                                       (cacheable #-sb-xc-host t))
  (declare (type (simple-array (unsigned-byte 32) (*)) array))
  (declare (ignorable objects minimal fast))
  (when (or (< (length array) 3) ; one or two keys - why are you doing this?
            (>= (length array) (ash 1 31))) ; insanity if this many
    (return-from make-perfect-hash-lambda))
  (unless (ub32-collection-uniquep array)
    (return-from make-perfect-hash-lambda))
  ;; no dups present
  (let* ((cache *phash-lambda-cache*)
         ;; LOGXOR is commutative and associative, which matters to
         ;; EMULATE-GENERATE-PERFECT-HASH-SEXPR. Cross-compiling sorts the key array
         ;; to make the xperfecthash files insensitive to the exact manner by which
         ;; consumers of this function provide the keys. It's not important for the
         ;; target compiler- the cache only helps repeated calls, in contrast
         ;; to the cross-compiler which lives or dies by the journal file.
         (digest (reduce #'logxor array))
         (invert-keys)
         ;; The cross-compiler records the string directly. Regression tests look for
         ;; certain comments in the string to assert coverage of the generator.
         (string)
         (expr))
    #+sb-xc-host
    (setq string (sb-cold::emulate-generate-perfect-hash-sexpr array objects digest)
            ;; don't rebind anything except *PACKAGE* for read-from-string,
            ;; especially as we need to keep our #\A charmacro
          expr (let ((*package* #.(find-package "SB-C"))) (read-from-string string)))
    #-sb-xc-host
    (flet ((generate-ph (keys)
             (sb-unix::newcharstar-string
              (sb-sys:with-pinned-objects (array)
                (alien-funcall
                 (extern-alien
                  "lisp_perfhash_with_options"
                  (function (* char) int system-area-pointer int))
                 (logior (if minimal 1 0) (if fast 2 0))
                 (sb-sys:vector-sap keys) (length keys))))))
      (unless cache
        ;; A race that clobbers the global is benign. At worst it discards
        ;; work done some in another thread to cache something.
        (setf cache (make-hash-table :test 'equalp :synchronized t)
              *phash-lambda-cache* cache))
      (dx-let ((cache-key (cons digest array)))
        ;; Purposely return empty-string if we hit the cache
        (awhen (gethash cache-key cache)
          (return-from make-perfect-hash-lambda (values it ""))))
      (setq string (generate-ph array))
      ;; generator might have trouble with 0 key
      (when (and (not string) (position 0 array))
        (setq string (generate-ph (map '(simple-array (unsigned-byte 32) (*))
                                       (lambda (x) (logxor x #xFFFFFFFF))
                                       array))
              invert-keys t))
      (unless string
        (return-from make-perfect-hash-lambda (values nil nil)))
      ;; string won't account for inverted keys but that's fine
      ;; as the resulting expression will
      (setq expr (with-standard-io-syntax
                     (let ((*package* #.(find-package "SB-C")))
                       (read-from-string string)))))
    (let ((tables))
      ;; Change array constants into symbol-macrolets
      (dotimes (i 2)
        (unless (typep expr '(cons (cons (eql let))))
          (return))
        (destructuring-bind (bindings . body) (cdar expr)
          (aver (singleton-p bindings))
          (unless (member (caar bindings) '(tab scramble)) (return))
          (setf tables (append tables bindings))
          (setq expr body)))
      ;; {>>,<<} can appear with a 2nd arg of 0. Remove them because
      ;; the 32-bit expression evaluator vop won't.
      (setq expr
            (named-let recurse ((expr expr))
              (cond ((listp expr)
                     (if (and (member (first expr) '(>> <<)) (eql (third expr) 0))
                         (recurse (second expr))
                         (mapcar #'recurse expr)))
                    (t
                     expr))))
      ;; Inject 32-bit LOGNOT if keys need to be bitwise inverted
      (when invert-keys
        (setq expr `((^= val #xFFFFFFFF) ,@expr)))
      (let* ((result-type `(mod ,(power-of-two-ceiling (length array))))
             (calc `(the ,result-type (uint32-modularly val ,@expr)))
             (lambda
        ;; Noting that the output of Bob's perfect hash generator was originally
        ;; intended to be compiled into C, and we've adapted it to Lisp,
        ;; it is not necessary to insert array-bounds-checks
        ;; even if users declaim that optimization quality to be 3.
                `(lambda (val)
                   (declare (optimize (safety 0) (speed 3) (debug 0)
                                      (sb-c:insert-array-bounds-checks 0)
                                      (sb-c:store-source-form 0)))
                   (declare (type (unsigned-byte 32) val))
                   ,(if tables `(symbol-macrolet ,tables ,calc) calc))))
        (when cacheable
          (setf (gethash (cons digest array) cache) lambda))
        (values lambda string)))))

(sb-xc:defmacro uint32-modularly (input &body exprs &environment env)
  (declare (ignore input env)) ; might use ENV to detect compiled vs interpreted code
  (let ((uint-max #xFFFFFFFF))
    ;; Replace C-ish operators with Lisp functions.
    (labels
        ((u32+ (a b) `(logand (+ ,a ,b) ,UINT-MAX))
         (u32- (a) `(logand (- ,a) ,UINT-MAX))
         (<< (n c) `(logand (ash ,n ,c) ,UINT-MAX))
         (>> (n c) `(ash ,n ,(- c)))
         (rewrite (expr)
           ;; Naive code-walking is OK here. Forms shaped like calls are calls.
           (when (atom expr)
             (return-from rewrite expr))
           (let ((subst
                  (case (car expr)
                    (^=   '(logxor))
                    (+=   '(u32+))
                    (&    'logand)
                    (^    'logxor)
                    (u32+ #'u32+)
                    (u32- #'u32-)
                    (<<   #'<<)
                    (>>   #'>>)
                    (t (return-from rewrite (mapcar #'rewrite expr))))))
             (rewrite
              (cond ((functionp subst) (apply subst (cdr expr)))
                    ((listp subst) ; becomes SETQ
                     `(setq ,(cadr expr) (,(car subst) ,(cadr expr) ,(caddr expr))))
                    (t (cons subst (cdr expr)))))))) ; simply a name change
      `(progn ,@(rewrite exprs)))))
;;; Neither sb-xc:define-compiler-macro nor (setf sb-xc:compiler-macro-function) exists
#+x86-64
(setf (info :function :compiler-macro-function 'uint32-modularly)
      #'sb-c::optimize-for-calc-phash)

;;; The CASE macro can use this predicate to decide whether to expand in a way
;;; that selects a clause via a perfect hash versus the customary expansion
;;; as a sequence of IFs.
(defun perfectly-hashable (objects)
  (flet ((hash (x)
           (cond ((fixnump x) (ldb (byte 32 0) x))
                 ((symbolp x) (symbol-name-hash x))
                 ((characterp x) (char-code x)))))
    (let* ((n (length objects))
           (hashes (make-array n :element-type '(unsigned-byte 32))))
      (loop for o in objects
            for i from 0
            do (let ((h (hash o)))
                 (if h
                     (setf (aref hashes i) h)
                     (return-from perfectly-hashable nil))))
      (make-perfect-hash-lambda hashes objects))))
