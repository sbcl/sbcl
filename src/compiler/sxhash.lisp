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
(deftransform sxhash ((x) (single-float)) '#.(sb-impl::sxhash-single-float-xform 'x))

;;; SXHASH of FIXNUM values is defined as a DEFTRANSFORM because it's so
;;; simple.
(deftransform sxhash ((x) (fixnum)) '#.(sb-impl::sxhash-fixnum-xform 'x))

(deftransform sxhash ((x) (double-float)) '#.(sb-impl::sxhash-double-float-xform 'x))

;; All symbols have a precomputed hash.
(deftransform sxhash ((x) (symbol)) `(symbol-hash x))

(deftransform hash-as-if-symbol-name ((object) (symbol) * :important nil)
  `(symbol-hash object))

;;; To use this macro during cross-compilation we will have to emulate
;;; generate_perfhash_sexpr using a file, similar to xfloat-math.
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
(defun make-perfect-hash-lambda (array &optional (minimal t) (fast nil))
  (declare (type (simple-array (unsigned-byte 32) (*)) array))
  (declare (ignorable minimal fast))
  (let* ((string
          #+sb-xc-host (sb-cold::emulate-generate-perfect-hash-sexpr array)
          #-sb-xc-host
          (sb-unix::newcharstar-string
           (sb-sys:with-pinned-objects (array)
             (alien-funcall (extern-alien
                             "generate_perfhash_sexpr"
                             (function (* char) int system-area-pointer int))
                            (logior (if minimal 1 0) (if fast 2 0))
                            (sb-sys:vector-sap array) (length array)))))
         (expr #+sb-xc-host ; don't rebind anything except *PACKAGE*
               ;; especially as we need to keep our #\A charmacro
               (let ((*package* #.(find-package "SB-C"))) (read-from-string string))
               #-sb-xc-host
               (with-standard-io-syntax
                 (let ((*package* #.(find-package "SB-C")))
                   (read-from-string string)))))
    (labels ((containsp (e op)
               (if (consp e)
                   (or (containsp (car e) op) (containsp (cdr e) op))
                   (eq e op))))
      `(lambda (val)
         (declare (optimize (safety 0) (debug 0) (sb-c:store-source-form 0)))
         (declare (type (unsigned-byte 32) val))
         ;; Remove the macros that aren't used, it helps with visual inspection
         ;; of the result. However, one macro can't call another since CONTAINSP
         ;; doesn't understand macros.
         (macrolet ,(remove-if-not
                     (lambda (m) (containsp expr (car m)))
                     '((& (a b) `(logand ,a ,b)) ; purposely look more C-like
                       (^ (a b) `(logxor ,a ,b)) ;  (for debugging)
                       (u32+ (a b) `(logand (+ ,a ,b) #xFFFFFFFF))
                       (^= (a b) `(setq ,a (logxor ,a ,b)))
                       (+= (a b) `(setq ,a (logand (+ ,a ,b) #xFFFFFFFF)))
                       (<< (n c) `(logand (ash ,n ,c) #xFFFFFFFF))
                       (>> (n c) `(ash ,n (- ,c)))))
           ;; We generate _really_ crappy code for 32-bit math on 64-bit machines.
           ;; I think the steps are sufficiently trivial that a single vop could choose
           ;; how to translate the arbitrary s-expression
           ,@expr)))))
(intern "TAB")
(intern "SCRAMBLE")
