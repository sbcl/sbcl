;;;; Provide the host's mock for the target's SXHASH, as well as a few
;;;; helper functions that construct DEFTRANSFORMS for the target
;;;; but also comprise the host's function.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;; We need to avoid dependence on the host's SXHASH function when producing
;;; hash values for hashset lookup, so that elements end up in an order
;;; that is host-lisp-insensitive. But the logic for our SXHASH is used here
;;; and also in the compiler transforms (which defines the ordinary function).
;;; Spewing it all over would lead invariably to getting it wrong,
;;; so we define the expression that the compiler will use, and then
;;; we paste the expressions into the cross-compilers emulation of our SXHASH.

(in-package "SB-C")

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun sxhash-fixnum-xform (x)
  (let ((c (logand 1193941380939624010 most-positive-fixnum)))
    ;; shift by -1 to get sign bit into hash
    `(logand (logxor (ash ,x 4) (ash ,x -1) ,c) most-positive-fixnum)))

(defun sxhash-single-float-xform (x)
  `(let ((bits (logand (single-float-bits ,x) ,(1- (ash 1 32)))))
     (logxor 66194023
             (sxhash (the sb-xc:fixnum
                          (logand most-positive-fixnum
                                  (logxor bits (ash bits -7))))))))

(defun sxhash-double-float-xform (x)
  #-64-bit
  `(let* ((hi (logand (double-float-high-bits ,x) ,(1- (ash 1 32))))
          (lo (double-float-low-bits ,x))
          (hilo (logxor hi lo)))
     (logxor 475038542
             (sxhash (the fixnum
                          (logand most-positive-fixnum
                                  (logxor hilo
                                          (ash hilo -7)))))))
  ;; Treat double-float essentially the same as a fixnum if words are 64 bits.
  #+64-bit
  `(let ((x (double-float-bits ,x)))
     ;; ensure we mix the sign bit into the hash
     (logand (logxor (ash x 4)
                     (ash x (- (1+ sb-vm:n-fixnum-tag-bits)))
                     ;; logical negation of magic constant ensures
                     ;; that 0.0d0 hashes to something other than what
                     ;; the fixnum 0 hashes to (as tested in
                     ;; hash.impure.lisp)
                     #.(logandc1 1193941380939624010 most-positive-fixnum))
             most-positive-fixnum)))

(defun sxhash-symbol-xform (s)
  #+64-bit `(let ((h (symbol-name-hash ,s))) (sb-int:mix h h)) ; get 60ish bits from 32
  #-64-bit `(symbol-name-hash ,s))
) ; end EVAL-WHEN

(defun calc-symbol-name-hash (string length)
  ;; The reader passes a string buffer and length to avoid consing a new string
  ;; for each symbol read. That does not occur in cross-compilation.
  (assert (= length (length string)))
  (cond #+(and 64-bit (not relocatable-static-space))
        ((string= string "NIL") ; :NIL must hash the same as NIL
         ;; Return the high 4 bytes in NIL's car slot.
         ;; out-of-order with defconstant nil-value
         (ldb (byte 32 32) (sb-vm::get-nil-taggedptr)))
        (t
         (ldb (byte 32 0) ; discard high bits for 64-bit builds
              (logxor (%sxhash-simple-string string) most-positive-fixnum)))))

;;; This is merely a slot-reader in real life, but since cross-compiling
;;; doesn't have a slot, simply recompute the answer as if it were stored.
;;; This hash contains *MORE* significant bits than SYMBOL-NAME-HASH.
;;; %MAKE-SYMBOL will (eventually) stuff in some pseudo-random bits.
;;; However, for cross-compiling, it's OK to forgo randomizing, because
;;; we don't have many (if any) STRING= symbols that would benefit from
;;; having distinct hashes.
(defun symbol-hash (symbol)
  (let ((name (symbol-name symbol)))
    (calc-symbol-name-hash name (length name))))

;;; This is also theoretically a slot-reader which for 32-bit is identical
;;; to symbol-hash, or 64-bit is a zero-extending 4-byte load that grabs
;;; only 32 bits.
(defun symbol-name-hash (symbol)
  (ldb (byte 32 0) (symbol-hash symbol)))

(defun hash-as-if-symbol-name (x)
  (aver (symbolp x))
  (symbol-name-hash x))

(defvar *sxhash-crosscheck* nil)
(defun sb-xc:sxhash (obj)
  (when (symbolp obj)
    ;; Allowing a few symbols (NIL,*,:UNSPECIFIED) here avoids a headache elsewhere:
    ;; 1. The ARRAY-TYPE hash mixer hashes the array dimensions, each
    ;;    being * or an integer.  I do not like an IF which decides to hash
    ;;    one way or another based on the type of the dimension.
    ;; 2. Exactly the same thing occurs with alien-type :BITS and :ALIGNMENT
    ;;    which are either NIL or int, as is alien-fun-type :VARARGS
    ;; But in stark contrast, XSET-ELTS-HASH avoids calling SXHASH on symbols
    ;; because SXHASH is not the best hasher for that purpose, like say an XSET
    ;; that contains 100 symbols all of whose print names are the same.
    (let ((s (string obj)))
      (unless (member s '("*" "NIL" "UNSPECIFIED") :test 'string=)
        (error "don't call SB-XC:SXHASH on ~S" obj))
      ;; !PACKAGE-COLD-INIT will cross-check this hash
      (return-from sb-xc:sxhash #.(sxhash-symbol-xform 'obj))))
  (let ((answer
         (etypecase obj ; croak on anything but these
           (sb-xc:fixnum #.(sxhash-fixnum-xform 'obj))
           ;; CALC-MEMBER-TYPE-HASH seems to want to invoke SB-XC:SXHASH on
           ;; signed zeros. I'm not sure if the answer has to match SBCL's
           ;; answer, but this makes it so it does.
           (single-float #.(sxhash-single-float-xform 'obj))
           (double-float #.(sxhash-double-float-xform 'obj)))))
    (push (cons obj answer) *sxhash-crosscheck*)
    answer))

;;; The fallback hashing functions might rely on the host's SXHASH but always
;;; return an SB-XC:FIXNUM result so that we can call our MIX on the value.
;;; It turns out that we have to largely reimplement numeric hashing because
;;; some hosts produce sufficiently poor hashes that our CTYPE hashsets would explode
;;; in size due to so many collisions, were we to use the host's SXHASH.
(defun number-hash (x) ; this does *not* have to match SB-IMPL::NUMBER-SXHASH
  (etypecase x
    (integer
     (if (typep x 'sb-xc:fixnum)
         (sb-xc:sxhash x)
         ;; mix target-fixnum-sized chunks
         (let ((nbits (1+ (integer-length x))) (h #xbbbb) (pos 0))
           (dotimes (i (ceiling nbits sb-vm:n-positive-fixnum-bits) h)
             (let ((chunk (ldb (byte sb-vm:n-positive-fixnum-bits pos) x)))
               (setf h (mix (number-hash chunk) h)))
             (incf pos sb-vm:n-positive-fixnum-bits)))))
    (ratio (mix (number-hash (numerator x)) (number-hash (denominator x))))
    (single-float (number-hash (single-float-bits x)))
    (double-float (mix (number-hash (double-float-low-bits x))
                       (number-hash (double-float-high-bits x))))))

(defun fallback-hash (x) ; only for HAIRY type specifier
  (etypecase x
    (symbol (symbol-name-hash x))
    (cons (if (eq (car x) 'satisfies)
              (fallback-hash (cadr x)) ; it's good enough
              (error "please no: ~S" x)))))

;;; This is only for XSETs.
(defun sb-impl::eql-hash (obj)
  (cond ((symbolp obj)
         ;; The target uses SYMBOL-HASH which is better (on 64-bit at least)
         ;; but symbol-name-hash is good enough for cross-compiling.
         (values (symbol-name-hash obj) nil))
        ((sb-xc:typep obj '(or number character))
         ;; Numbers parse into NUMERIC-TYPE (except for signed zeros).
         ;; Character parse into CHARACTER-SET-TYPE.
         (bug "Huh - Why does an XSET contain ~S?" obj))
        (t ; use the algorithm of https://xkcd.com/221/
         ;; This case is hit maybe a few dozen times in cross-compiling.
         (values 4 t))))

(defun %make-lisp-obj (bits) ; Cast BITS (a representation) to fixnum
  (declare (type sb-vm:word bits))
  (flet ((sign-extend (int size) ; borrowed from disassem.lisp
           (if (logbitp (1- size) int)
               (dpb int (byte size 0) -1)
               int)))
    (if (= (logand bits sb-vm:fixnum-tag-mask) 0)
        (sign-extend (ash bits (- sb-vm:n-fixnum-tag-bits)) sb-vm:n-fixnum-bits)
        (bug "%MAKE-LISP-OBJ can only make fixnums"))))

;;; Every architecture needs this portable replacement for +-modfx.
;;; Some, but not all, define +-modfx in cross-modular.
;;; We need this available sooner than that because type-classes needs it
;;; to compute a hash of a list of things order-insensitively.
(defun plus-mod-fixnum (a b)
  (declare (type sb-xc:fixnum a b))
  (labels ((representation (x) (mask-to-word (ash x sb-vm:n-fixnum-tag-bits)))
           (mask-to-word (x) (ldb (byte sb-vm:n-word-bits 0) x)))
    (%make-lisp-obj (mask-to-word (+ (representation a) (representation b))))))

;; Assume 2 fixnum tag bits:
;;      significant bits      tag
;;   #b01111...11111111111111  00
;; + #b0                    1  00
;;   ------------------------
;; = #b10000...00000000000000  00
(assert (= (plus-mod-fixnum sb-xc:most-positive-fixnum 1)
           sb-xc:most-negative-fixnum))
;; etc
(assert (= (plus-mod-fixnum sb-xc:most-negative-fixnum sb-xc:most-negative-fixnum)
           0))
(assert (= (plus-mod-fixnum -1 most-negative-fixnum)
           sb-xc:most-positive-fixnum))
