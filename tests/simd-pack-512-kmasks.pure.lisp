;;;; Potentially side-effectful tests of the AVX-512 mask register infrastructure.


;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

;; Skip the file if the feature is missing or hardware does not support AVX-512.
#-sb-simd-pack-512 (invoke-restart 'run-tests::skip-file)
(when (zerop (sb-alien:extern-alien "avx512_supported" int))
  (format t "~&INFO: AVX-512 (and thus masks) not supported on this hardware")
  (invoke-restart 'run-tests::skip-file))

;; I like this first because it clobbers the terminal with some ugly printouts
(with-test (:name :load-simd-pack-512-mask-literal)
  (let ((file (scratch-file-name))
        (fasl nil)
        (var '*loaded-mask-literal*))
    (unwind-protect
         (progn
           ;; Force the compiler to dump a real SIMD-PACK-512-MASK object
           ;; as a literal constant, not compile a call to %MAKE-MASK.
           (with-open-file (s file
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
             (let ((*print-readably* t)
                   (*read-eval* t))
               (prin1
                `(defparameter ,var
                   #.(sb-ext:%make-simd-pack-512-mask #x123456789ABCDEF0))
                s)))

           (multiple-value-bind (fasl-path warnings-p failure-p)
               (compile-file file)
             (declare (ignore warnings-p))
             (assert (not failure-p))
             (setq fasl fasl-path))

           (makunbound var)
           (load fasl)

           (assert (boundp var))
           (let ((mask (symbol-value var)))
             (assert (sb-ext:simd-pack-512-mask-p mask))
             (assert (= #x123456789ABCDEF0
                        (sb-kernel:%simd-pack-512-mask-value mask)))))
      (when fasl (delete-file fasl))
      (when file (delete-file file)))))

(defun make-constant-masks ()
  (values (sb-ext:%make-simd-pack-512-mask 0)
          (sb-ext:%make-simd-pack-512-mask (ldb (byte 64 0) -1))
          (sb-ext:%make-simd-pack-512-mask #x123456789ABCDEF0)))

(with-test (:name :compile-simd-pack-512-mask-identity)
  (multiple-value-bind (x y z) (make-constant-masks)
    (declare (type sb-ext:simd-pack-512-mask x))
    (assert (= 0 (sb-kernel:%simd-pack-512-mask-value x)))
    (assert (= (ldb (byte 64 0) -1) (sb-kernel:%simd-pack-512-mask-value y)))
    (assert (= #x123456789ABCDEF0 (sb-kernel:%simd-pack-512-mask-value z)))))

(with-test (:name (simd-pack-512-mask print :smoke))
  (let ((masks (multiple-value-list (make-constant-masks))))
    (dolist (mask masks)
      (with-output-to-string (stream)
        (write mask :stream stream :pretty t :escape nil)))))

(defvar *tmp-filename* (scratch-file-name))
(defvar *mask*)

(with-test (:name :load-simd-pack-512-mask-fasl)
  (with-open-file (s *tmp-filename* :direction :output :if-exists :supersede :if-does-not-exist :create)
    (print '(setq *mask* (sb-ext:%make-simd-pack-512-mask #xDEADBEEFCAFEBABE)) s))
  (let (tmp-fasl)
    (unwind-protect
         (progn
           (setq tmp-fasl (compile-file *tmp-filename*))
           (let ((*mask* nil))
             (load tmp-fasl)
             (assert (sb-ext:simd-pack-512-mask-p *mask*))
             (assert (= #xDEADBEEFCAFEBABE (sb-kernel:%simd-pack-512-mask-value *mask*)))))
      (when tmp-fasl (delete-file tmp-fasl))
      (delete-file *tmp-filename*))))

(with-test (:name :mask-spilling)
  (checked-compile-and-assert ()
                              `(lambda (x y)
                                 (declare (type sb-ext:simd-pack-512-mask x))
                                 (eval y)
                                 (sb-kernel:%simd-pack-512-mask-value x))
                              (((sb-ext:%make-simd-pack-512-mask #x1337) 0) #x1337)))

(with-test (:name (simd-pack-512-mask subtypep :smoke))
  (assert-tri-eq t t (subtypep 'sb-ext:simd-pack-512-mask 'sb-ext:simd-pack-512-mask))
  (assert-tri-eq t t (subtypep 'sb-ext:simd-pack-512-mask 't))
  (assert-tri-eq nil t (subtypep 't 'sb-ext:simd-pack-512-mask)))

(with-test (:name (simd-pack-512-mask :ctype-unparse :smoke))
  (flet ((unparsed (s) (sb-kernel:type-specifier (sb-kernel:specifier-type s))))
    (assert (equal (unparsed 'sb-ext:simd-pack-512-mask) 'sb-ext:simd-pack-512-mask))))

(with-test (:name :simd-pack-512-mask-type-errors)
  (locally
      (declare (muffle-conditions warning))
    (assert-error (sb-ext:%make-simd-pack-512-mask (ash 1 64)) type-error)
    (assert-error (sb-ext:%make-simd-pack-512-mask -1) type-error)))

(cl:in-package "SB-VM")

(sb-c::defknown sb-vm::%mask-identity
    (sb-ext:simd-pack-512-mask)
    sb-ext:simd-pack-512-mask
    (sb-c::flushable sb-c::movable))

(sb-c::defknown sb-vm::%make-mask-from-unsigned
    ((unsigned-byte 64))
    sb-ext:simd-pack-512-mask
    (sb-c::flushable sb-c::movable))

(sb-c::defknown sb-vm::%mask-to-unsigned
    (sb-ext:simd-pack-512-mask)
    (unsigned-byte 64)
    (sb-c::flushable sb-c::movable))

(sb-c::defknown sb-vm::%mask-kandq
    (sb-ext:simd-pack-512-mask sb-ext:simd-pack-512-mask)
    sb-ext:simd-pack-512-mask
    (sb-c::flushable sb-c::movable))

(sb-c::defknown sb-vm::%mask-kshiftrq
    (sb-ext:simd-pack-512-mask (integer 0 63))
    sb-ext:simd-pack-512-mask
    (sb-c::flushable sb-c::movable))

(defun %mask-identity (x)
  (declare (type simd-pack-512-mask x)
           (ignore x))
  (error "%mask-identity stub"))

(defun %make-mask-from-unsigned (x)
  (declare (type (unsigned-byte 64) x)
           (ignore x))
  (error "%mask-from-unsigned stub"))

(defun %mask-to-unsigned (x)
  (declare (type simd-pack-512-mask x)
           (ignore x))
  (error "%mask-to-unsigned stub"))

(defun %mask-kandq (x y)
  (declare (ignore x y))
  (error "%mask-kandq stub"))

(defun %mask-kshiftrq (x count)
  (declare (ignore x count))
  (error "%mask-kshiftrq stub"))

(define-vop (%mask-identity)
  (:translate %mask-identity)
  (:policy :fast-safe)
  (:args (x :scs (mask-reg)))
  (:arg-types simd-pack-512-mask-type)
  (:results (y :scs (mask-reg)))
  (:result-types simd-pack-512-mask-type)
  (:generator 1
    (inst kmovq y x)))

(define-vop (%make-mask-from-unsigned)
  (:translate %make-mask-from-unsigned)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (y :scs (mask-reg)))
  (:result-types simd-pack-512-mask-type)
  (:generator 1
    (inst kmovq y x)))

(define-vop (%mask-to-unsigned)
  (:translate %mask-to-unsigned)
  (:policy :fast-safe)
  (:args (x :scs (mask-reg)))
  (:arg-types simd-pack-512-mask-type)
  (:results (y :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (inst kmovq y x)))

(define-vop (%mask-kandq)
  (:translate %mask-kandq)
  (:policy :fast-safe)
  (:args (x :scs (mask-reg))
         (y :scs (mask-reg)))
  (:arg-types simd-pack-512-mask-type simd-pack-512-mask-type)
  (:results (z :scs (mask-reg)))
  (:result-types simd-pack-512-mask-type)
  (:generator 1
    (inst kandq z x y)))

(define-vop (%mask-kshiftrq)
  (:translate %mask-kshiftrq)
  (:policy :fast-safe)
  (:args (x :scs (mask-reg)))
  (:info count)
  (:arg-types simd-pack-512-mask-type (:constant t))
  (:results (z :scs (mask-reg)))
  (:result-types simd-pack-512-mask-type)
  (:generator 1
    (inst kshiftrq z x count)))

(cl:in-package :test-util)

(with-test (:name :mask-raw-spilling)
  (checked-compile-and-assert ()
    `(lambda (x y)
       (declare (type (unsigned-byte 64) x))
       (let ((tmp (sb-vm::%make-mask-from-unsigned x)))
         (eval y)
         (sb-vm::%mask-to-unsigned tmp)))
    ((#x1234 0) #x1234)))

(with-test (:name :mask-gpr-kmask-gpr-roundtrip)
  (checked-compile-and-assert ()
    `(lambda (x)
       (declare (type (unsigned-byte 64) x))
       (sb-vm::%mask-to-unsigned (sb-vm::%make-mask-from-unsigned x)))
    ((#xDEADBEEFCAFEBABE) #xDEADBEEFCAFEBABE)))

(with-test (:name :mask-gpr-to-kmask-to-boxed)
  (checked-compile-and-assert ()
    `(lambda (x)
       (declare (type (unsigned-byte 64) x))
       (sb-kernel:%simd-pack-512-mask-value
        (sb-vm::%make-mask-from-unsigned x)))
    ((#x1337) #x1337)))

(with-test (:name :mask-identity-kmask-kmask)
  (checked-compile-and-assert ()
    `(lambda (x)
       (declare (type (unsigned-byte 64) x))
       (sb-vm::%mask-to-unsigned
        (sb-vm::%mask-identity
         (sb-vm::%make-mask-from-unsigned x))))
    ((#x1234) #x1234)))

(with-test (:name :mask-constant-folding)
  (let* ((fun (compile nil
                       '(lambda ()
                          (sb-kernel:%simd-pack-512-mask-value
                           (sb-ext:%make-simd-pack-512-mask
                            #x123456789ABCDEF0)))))
         (text (with-output-to-string (s)
                 (disassemble fun :stream s))))
    (assert (= (funcall fun) #x123456789ABCDEF0))
    ;; if constant folding works, the compiled body should not need
    ;; to allocate a mask object or move values through K registers.
    (assert (not (search "ALLOC" text)))
    (assert (not (search "KMOVQ" text)))))

(with-test (:name :kandq-disassembly)
  (let* ((fun (compile nil
    '(lambda (x y)
      (declare (type (unsigned-byte 64) x y))
      (sb-vm::%mask-kandq
       (sb-vm::%make-mask-from-unsigned x)
       (sb-vm::%make-mask-from-unsigned y)))))
         (text (with-output-to-string (s)
                 (disassemble fun :stream s))))
    (assert (search "KANDQ" text))
    (assert (not (search "BYTE #XC4" text)))))

(with-test (:name :kshiftrq-disassembly)
  (let* ((fun (compile nil
    '(lambda (x)
      (declare (type (unsigned-byte 64) x))
      (sb-vm::%mask-kshiftrq
       (sb-vm::%make-mask-from-unsigned x)
       1))))
         (text (with-output-to-string (s)
                 (disassemble fun :stream s))))
    (assert (search "KSHIFTRQ" text))
    (assert (not (search "BYTE #XC4" text)))))

(with-test (:name :location-print-name)
  (let* ((vm (find-package "SB-VM"))
         (c (find-package "SB-C"))
         (location-print-name (and vm (find-symbol "LOCATION-PRINT-NAME" vm)))
         (mask-reg-name (and vm (find-symbol "MASK-REG" vm)))
         (sc-or-lose (and c (find-symbol "SC-OR-LOSE" c)))
         (make-random-tn (and c (find-symbol "MAKE-RANDOM-TN" c)))
         (sc (and sc-or-lose mask-reg-name
                  (funcall sc-or-lose mask-reg-name)))
         (tn (and make-random-tn sc
                  (funcall make-random-tn sc 1))))
    (when (and location-print-name tn)
      (let ((name (funcall location-print-name tn)))
        (assert (stringp name))
        (assert (string= name "K1"))))))

(with-test (:name :mask-reg-sc-locations)
  (let* ((vm (find-package "SB-VM"))
         (c (find-package "SB-C"))
         (mask-reg-name (and vm (find-symbol "MASK-REG" vm)))
         (sc-or-lose (and c (find-symbol "SC-OR-LOSE" c)))
         (sc-locations (and c (find-symbol "SC-LOCATIONS" c)))
         (sc (and sc-or-lose mask-reg-name
                  (funcall sc-or-lose mask-reg-name)))
         (locs (and sc-locations sc
                    (funcall sc-locations sc))))
    (assert locs)
    ;; #xFE = #b11111110 -> K1-K7 only, K0 excluded.
    (assert (= locs #xFE))))

(with-test (:name :simd-pack-512-mask-print-readably)
  (let* ((value #x123456789ABCDEF0)
         (mask (sb-ext:%make-simd-pack-512-mask value))
         (*print-readably* t)
         (*read-eval* t)
         (printed (prin1-to-string mask))
         (read-back (read-from-string printed)))
    (assert (sb-ext:simd-pack-512-mask-p read-back))
    (assert (= value
               (sb-kernel:%simd-pack-512-mask-value read-back)))))

(with-test (:name :mask-reg-sc-locations)
  (let* ((vm (find-package "SB-VM"))
         (c (find-package "SB-C"))
         (mask-reg-name (and vm (find-symbol "MASK-REG" vm)))
         (sc-or-lose (and c (find-symbol "SC-OR-LOSE" c)))
         (sc-locations (and c (find-symbol "SC-LOCATIONS" c)))
         (sc (and sc-or-lose mask-reg-name
                  (funcall sc-or-lose mask-reg-name)))
         (locs (and sc-locations sc
                    (funcall sc-locations sc))))
    (assert locs)
    ;; 254 = #b11111110, i.e. K1-K7 only.
    (assert (= locs #xFE))))

;; This particular test does not test for avx512 feature per se
;; cpu-has-zmm-registers has a low constant number, 2, so
;; check if there is a collision, just to be on the safe side.
;; It checks acutally all cpu feature bits, but I guess it is OK.
(with-test (:name :cpu-feature-bit-no-collisions)
  (let* ((vm (find-package "SB-VM"))
         (seen (make-hash-table))
         (collisions nil))
    (do-symbols (sym vm)
      (when (and (boundp sym)
                 (let ((name (symbol-name sym)))
                   (and (<= 8 (length name))
                        (string= name "CPU-HAS-" :end1 8 :end2 8))))
        (let* ((sym (find-symbol (symbol-name sym) vm))
               (value (symbol-value sym)))
          (when (gethash value seen)
            (push (list sym (gethash value seen) value) collisions))
          (setf (gethash value seen) sym))))
    (assert (null collisions) nil
            "CPU feature bits collide: ~S" collisions)))

;; assembly printer
(with-test (:name :kmovq-disassembly)
  (let* ((fun (compile nil
                       '(lambda (x)
                         (declare (type (unsigned-byte 64) x))
                         (sb-vm::%mask-to-unsigned
                          (sb-vm::%mask-identity
                           (sb-vm::%make-mask-from-unsigned x))))))
         (text (with-output-to-string (s)
                 (disassemble fun :stream s))))
    (assert (search "KMOVQ" text))
    ;; Ensure we are not seeing raw VEX bytes instead of decoded KMOVQ.
    (assert (not (search "BYTE #XC4" text)))))

(with-test (:name :kmovq-memory-disassembly)
  (let* ((fun (compile nil
                       '(lambda (x y)
                         (declare (type (unsigned-byte 64) x))
                         (let ((tmp (sb-vm::%make-mask-from-unsigned x)))
                           (eval y)
                           (sb-vm::%mask-to-unsigned tmp)))))
         (text (with-output-to-string (s)
                 (disassemble fun :stream s))))
    (assert (search "KMOVQ" text))
    (assert (search "[RBP" text))   ; memory store/load somewhere
    (assert (not (search "BYTE #XC4" text)))))
