;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;;; hashing strings
;;;;
;;;; Note that this operation is used in compiler symbol table
;;;; lookups, so we'd like it to be fast.
;;;;
;;;; As of 2004-03-10, we implement the one-at-a-time algorithm
;;;; designed by Bob Jenkins (see
;;;; <http://burtleburtle.net/bob/hash/doobs.html> for some more
;;;; information).

#-sb-xc-host (declaim (inline %sxhash-simple-substring))
(defun %sxhash-simple-substring (string start end)
  ;; FIXME: As in MIX above, we wouldn't need (SAFETY 0) here if the
  ;; cross-compiler were smarter about ASH, but we need it for
  ;; sbcl-0.5.0m.  (probably no longer true?  We might need SAFETY 0
  ;; to elide some type checks, but then again if this is inlined in
  ;; all the critical places, we might not -- CSR, 2004-03-10)

  ;; Never decrease safety in the cross-compiler. It's not worth the headache
  ;; of tracking down insidious host/target compatibility bugs.
  #-sb-xc-host (declare (optimize (speed 3) (safety 0)))
  (macrolet ((guts ()
               `(loop for i of-type index from start below end do
                  (set-result (+ result (char-code (aref string i))))
                  (set-result (+ result (ash result 10)))
                  (set-result (logxor result (ash result -6)))))
             (set-result (form)
               `(setf result (ldb (byte #.sb-vm:n-word-bits 0) ,form))))
    (let ((result 238625159)) ; (logandc2 most-positive-fixnum (sxhash #\S)) on 32 bits
      (declare (type word result))
      ;; Avoid accessing elements of a (simple-array nil (*)).
      ;; The expansion of STRING-DISPATCH involves ETYPECASE,
      ;; so we can't simply omit one case. Therefore that macro
      ;; is unusable here.
      #-sb-xc-host (typecase string
                     (simple-base-string (guts))
                     ((simple-array character (*)) (guts)))

      ;; just do it, don't care about loop unswitching or simple-ness of the string.
      #+sb-xc-host (guts)

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
  ;; Don't care if the host uses non-simple strings where SBCL would always
  ;; have had a simple-string, notably SYMBOL-NAME and PACKAGE-NAME.
  (declare (type #+sb-xc-host string #-sb-xc-host simple-string x))
  ;; KLUDGE: this FLET is a workaround (suggested by APD) for presence
  ;; of let conversion in the cross compiler, which otherwise causes
  ;; strongly suboptimal register allocation.
  (flet ((trick (x)
           (%sxhash-simple-substring x 0 (length x))))
    (declare (notinline trick))
    (trick x)))

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
(declaim (ftype (sfunction ((and sb-xc:fixnum unsigned-byte)
                            (and sb-xc:fixnum unsigned-byte))
                           (and sb-xc:fixnum unsigned-byte))
                mix))
(declaim (inline mix))
(defun mix (x y)
  #-sb-xc-host (declare (optimize (speed 3)))
  (declare (type (and sb-xc:fixnum unsigned-byte) x y))
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
  (let* ((mul (logand 3622009729038463111 most-positive-fixnum))
         (xor (logand 608948948376289905 most-positive-fixnum))
         (xy (logand (+ (* x mul) y) most-positive-fixnum)))
    (logand (logxor xor xy (ash xy -5)) most-positive-fixnum)))

;;; Same as above, but don't mask computations to n-positive-fixnum-bits.
(declaim (inline word-mix))
(defun word-mix (x y)
  #-sb-xc-host (declare (optimize (speed 3)) (muffle-conditions compiler-note))
  (declare (type word x y))
  (let* ((mul (logand 3622009729038463111 most-positive-word))
         (xor (logand 608948948376289905 most-positive-word))
         (xy (logand (+ (* x mul) y) most-positive-word)))
    (logand (logxor xor xy (ash xy -5)) most-positive-word)))

;;; These are Lisp implementations of
;;; https://github.com/aappleby/smhasher/blob/master/src/MurmurHash3.cpp
;;; Please excuse the C-like syle.
#-64-bit
(progn
(declaim (maybe-inline murmur3-fmix32))
(defun murmur3-fmix32 (h)
  (declare (type (unsigned-byte 32) h))
  (setq h (logxor h (ash h -16)))
  (setq h (logand (* h #x85ebca6b) most-positive-word))
  (setq h (logxor h  (ash h -13)))
  (setq h (logand (* h #xc2b2ae35) most-positive-word))
  (logxor h (ash h -16))))

#+64-bit
(progn
(declaim (maybe-inline murmur3-fmix64))
(defun murmur3-fmix64 (k)
  (setq k (logxor k (ash k -33)))
  (setq k (logand (* k #xff51afd7ed558ccd) most-positive-word))
  (setq k (logxor k (ash k -33)))
  (setq k (logand (* k #xc4ceb9fe1a85ec53) most-positive-word))
  (logxor k (ash k -33))))

;;;; support for the hash values used by CLOS when working with LAYOUTs

;;; Return a quasi-random number up to and including MOST-POSITIVE-FIXNUM.
;;; The top bit of the fixnum is forced to 1, i.e. it is not random at all,
;;; and signifies a valid layout.  If the entire hash is 0, the layout is
;;; is invalid. TO ensure that a bitwise AND of hashes produces a nonzero bit,
;;; the effective hash is half that of positive fixnum.
;;; For further reference:
;;; See paper by Kiczales and Rodriguez, "Efficient Method Dispatch in PCL", 1990
;;;
;;; Also note:
;;; - the layout invalidation mechanism does not depend on a "new"
;;;   layout (obsoleting an "old" layout) having a different hash,
;;;   because hash collisions are are always possible.
;;;   Therefore a hash based solely on name is perfectly fine, and to be preferred.
;;; - And unlike SXHASH of a symbol, we *can* mix the package name into the hash.
;;;   SBCL doesn't use a lot of "looklike" symbols but users might.
;;; - As further work (not done) we can reserve a bit to signify satisfying STREAMP
;;;   or similar predicates. Then a valid stream would have #b11 in 2 bits,
;;;   an invalid stream #b10, and a non-stream or invalid layout anything else.
;;; - Low bits would have been preferable for the ancillary bits, but that would entail
;;;   an extra right-shift to remove bits that lack any randomness. CACHE-MASK can be
;;;   reworked to examine bits other than at the low end.
(defun hash-layout-name (name)
  (let ((limit (1+ (ash most-positive-fixnum -1))))
    (declare (notinline random))
    (logior (if (typep name '(and symbol (not null)))
                (flet ((improve-hash (x)
                         (#+64-bit murmur3-fmix64 #-64-bit murmur3-fmix32 x)))
                  (mix (logand
                        (improve-hash (sb-impl::%sxhash-simple-string (symbol-name name)))
                        most-positive-fixnum)
                       (let ((package (sb-xc:symbol-package name)))
                         (sb-impl::%sxhash-simple-string
                          ;; Must specifically look for CL package when cross-compiling
                          ;; because we might have remapped a symbol from its "actual"
                          ;; package of the host lisp, to being logically in CL if the
                          ;; host happens to have standard symbols homed elsewhere.
                          (cond #+sb-xc ((eq package *cl-package*) "COMMON-LISP")
                                ((not package) "uninterned")
                                (t (package-name package)))))))
                ;; This L-T-V form has to remain out of the common path,
                ;; or else cheneygc will crash in cold-init.
                ;; Cold-init calls HASH-LAYOUT-NAME many times *before* the L-T-V
                ;; is actually executed and stuffed in as a constant.
                ;; That's harmless - the loader put an unbound marker there, and we don't care.
                ;; *HOWEVER* there is an interesting issue that arises if the value-cell
                ;; indirection is present (see the IR1 translator for L-T-V and the
                ;; conditional code for cheneygc): when can the compiler dereference the
                ;; value-cell? If the binding were earlier (before the IF), then the dereference
                ;; would happen earlier, and crash, because unbound-marker isn't a pointer.
                (let ((random-state (load-time-value (make-random-state))))
                  (random (ash limit -1) random-state)))
            limit)))
