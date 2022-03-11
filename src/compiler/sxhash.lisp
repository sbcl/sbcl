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
(defglobal +sxhash-single-float-expr+
  `(let ((bits (logand (single-float-bits x) ,(1- (ash 1 32)))))
     (logxor 66194023
             (sxhash (the sb-xc:fixnum
                          (logand most-positive-fixnum
                                  (logxor bits (ash bits -7))))))))
(deftransform sxhash ((x) (single-float)) '#.+sxhash-single-float-expr+)

#-64-bit
(defglobal +sxhash-double-float-expr+
  `(let* ((hi (logand (double-float-high-bits x) ,(1- (ash 1 32))))
          (lo (double-float-low-bits x))
          (hilo (logxor hi lo)))
     (logxor 475038542
             (sxhash (the fixnum
                          (logand most-positive-fixnum
                                  (logxor hilo
                                          (ash hilo -7))))))))

;;; SXHASH of FIXNUM values is defined as a DEFTRANSFORM because it's so
;;; simple.
(defglobal +sxhash-fixnum-expr+
  (let ((c (logand 1193941380939624010 most-positive-fixnum)))
    ;; shift by -1 to get sign bit into hash
    `(logand (logxor (ash x 4) (ash x -1) ,c) most-positive-fixnum)))
(deftransform sxhash ((x) (fixnum)) '#.+sxhash-fixnum-expr+)

;;; Treat double-float essentially the same as a fixnum if words are 64 bits.
#+64-bit
(defglobal +sxhash-double-float-expr+
  ;; logical negation of magic constant ensures that 0.0d0 hashes to something
  ;; other than what the fixnum 0 hashes to (as tested in hash.impure.lisp)
  (let ((c (logandc1 1193941380939624010 most-positive-fixnum)))
    `(let ((x (double-float-bits x)))
       ;; ensure we mix the sign bit into the hash
       (logand (logxor (ash x 4)
                       (ash x (- (1+ sb-vm:n-fixnum-tag-bits)))
                       ,c)
               most-positive-fixnum))))

(deftransform sxhash ((x) (double-float)) '#.+sxhash-double-float-expr+)

(deftransform sxhash ((x) (symbol))
  (cond ((csubtypep (lvar-type x) (specifier-type 'keyword))
         ;; All interned symbols have a precomputed hash.
         ;; There's no way to ask the type system whether a symbol is known to
         ;; be interned, but we *can* test for the specific case of keywords.
         ;; Even if it gets uninterned, this shortcut remains valid.
         `(symbol-hash x)) ; Never need to lazily compute and memoize
        ((gethash 'ensure-symbol-hash *backend-parsed-vops*)
         ;; A vop might emit slightly better code than the expression below
         `(ensure-symbol-hash x))
        (t
         ;; Cache the value of the symbol's sxhash in the symbol-hash
         ;; slot.
         '(let ((result (symbol-hash x)))
            ;; 0 marks uninitialized slot. We can't use negative
            ;; values for the uninitialized slots since NIL might be
            ;; located so high in memory on some platforms that its
            ;; SYMBOL-HASH (which contains NIL itself) is a negative
            ;; fixnum.
            (if (= 0 result)
                (ensure-symbol-hash x)
                result)))))

(deftransform symbol-hash* ((object predicate) (symbol null) * :important nil)
  `(symbol-hash* object 'symbolp)) ; annotate that object satisfies SYMBOLP
(deftransform symbol-hash* ((object predicate)
                            ((and (not null) symbol)
                             (constant-arg (member nil symbolp)))
                            * :important nil)
  `(symbol-hash* object 'non-null-symbol-p)) ; etc

;;; To define SXHASH compatibly without repeating the logic in the transforms
;;; that define the numeric cases, we do some monkey business involving #. to paste
;;; in the expressions that the transforms would return.
#+sb-xc-host
(progn
  (defvar *sxhash-crosscheck* nil)
  (defun symbol-name-hash (x)
    (cond ((string= x "NIL") ; :NIL must hash the same as NIL
           (ash sb-vm:nil-value (- sb-vm:n-fixnum-tag-bits)))
          (t
           ;; (STRING X) could be a non-simple string, it's OK.
           (let ((hash (logxor (sb-impl::%sxhash-simple-string (string x))
                               most-positive-fixnum)))
             (aver (ldb-test (byte (- 32 sb-vm:n-fixnum-tag-bits) 0) hash))
             hash))))
  (defun sxhash (x)
    (let ((answer
           (etypecase x ; croak on anything but these
            (symbol (symbol-name-hash x))
            (sb-xc:fixnum #.+sxhash-fixnum-expr+)
            (single-float #.+sxhash-single-float-expr+)
            (double-float #.+sxhash-double-float-expr+))))
      ;; Symbol hashes are cross-checked during cold-init
      (unless (symbolp x)
        (push (cons x answer) *sxhash-crosscheck*))
      answer)))
