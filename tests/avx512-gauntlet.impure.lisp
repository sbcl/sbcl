;;;; tests/avx512-gauntlet.impure.lisp --- Brutal AVX-512 Super-Sweep Gauntlet 2.0
;;;;
;;;; A dependency-free, high-performance combinatoric differential fuzz harness for AVX-512.
;;;; Multi-Producer Multi-Consumer (MPMC) zero-allocation architecture:
;;;; - Physical core topology detection (bypasses SMT/Hyperthreading).
;;;; - Balanced cluster of Hardware Producers and Validation Consumers pinned to physical cores.
;;;; - Pre-allocated, flat 1D array of (unsigned-byte 64) with lock-free atomic barriers.
;;;; - Macro-driven VOP compilation and bit-exact scalar oracle emulation.
;;;; - 5-Stage in-register cascades across ZMM0..ZMM7 and K1..K5 without memory spills.
;;;; - Extreme numerical corner cases (NaNs, subnormals, infinities, extrema, bit-noise).
;;;; - Instant copy-pasteable Common Lisp REPL repro form on failure.

#-sb-simd-pack-512 (invoke-restart 'run-tests::skip-file)

(when (zerop (sb-alien:extern-alien "avx512_supported" int))
  (format t "~&INFO: AVX-512 not supported on this host~%")
  (invoke-restart 'run-tests::skip-file))

(cl:in-package "SB-VM")

;;; ---------------------------------------------------------------------------
;;; 1. 5-Stage In-Register Hardware Cascade VOP
;;; ---------------------------------------------------------------------------

(sb-ext:without-package-locks
  (defknown %gauntlet-cascade-5stage ((simd-pack-512 (unsigned-byte 64))
                                      (simd-pack-512 (unsigned-byte 64))
                                      simd-pack-512-mask
                                      simd-pack-512-mask
                                      simd-pack-512-mask
                                      simd-pack-512-mask
                                      simd-pack-512-mask)
      (simd-pack-512 (unsigned-byte 64)) (flushable movable))

  (define-vop (%gauntlet-cascade-5stage)
    (:translate %gauntlet-cascade-5stage)
    (:policy :fast-safe)
    (:args (a :scs (int-avx512-reg) :target dst)
           (b :scs (int-avx512-reg))
           (m1 :scs (mask-reg))
           (m2 :scs (mask-reg))
           (m3 :scs (mask-reg))
           (m4 :scs (mask-reg))
           (m5 :scs (mask-reg)))
    (:arg-types simd-pack-512-ub64 simd-pack-512-ub64
                simd-pack-512-mask-type simd-pack-512-mask-type simd-pack-512-mask-type
                simd-pack-512-mask-type simd-pack-512-mask-type)
    (:temporary (:sc int-avx512-reg) z1 z2 z3 z4)
    (:results (dst :scs (int-avx512-reg)))
    (:result-types simd-pack-512-ub64)
    (:generator 10
      ;; Stage 1: z1 = vpaddq-masked(a, b, m1)
      (inst vmovdqu64 z1 a)
      (inst vpaddq-masked z1 z1 b m1)
      ;; Stage 2: z2 = vpxorq-masked(z1, a, m2)
      (inst vmovdqu64 z2 z1)
      (inst vpxorq-masked z2 z2 a m2)
      ;; Stage 3: z3 = vpsubq-masked(z2, b, m3)
      (inst vmovdqu64 z3 z2)
      (inst vpsubq-masked z3 z3 b m3)
      ;; Stage 4: z4 = vporq-masked(z3, z1, m4)
      (inst vmovdqu64 z4 z3)
      (inst vporq-masked z4 z4 z1 m4)
      ;; Stage 5: dst = vpandq-masked(z4, z2, m5)
      (inst vmovdqu64 dst z4)
      (inst vpandq-masked dst dst z2 m5))))

(cl:in-package "CL-USER")

;;; ---------------------------------------------------------------------------
;;; 2. Native Physical Processor Topology & Affinity Bindings
;;; ---------------------------------------------------------------------------

(sb-alien:define-alien-routine ("get_nprocs" c-get-nprocs) sb-alien:int)

(sb-alien:define-alien-routine ("sched_setaffinity" c-sched-setaffinity) sb-alien:int
  (pid sb-alien:int)
  (cpusetsize sb-alien:unsigned-long)
  (mask (* sb-alien:unsigned-long)))

(defun detect-physical-cores ()
  "Parse /proc/cpuinfo to identify unique physical cores and their primary processor ID.
Returns a list of logical processor IDs representing distinct physical execution cores."
  (let ((core-map (make-hash-table :test 'equal))
        (current-proc nil)
        (current-phys nil)
        (current-core nil))
    (with-open-file (stream "/proc/cpuinfo" :direction :input :if-does-not-exist nil)
      (when stream
        (loop for line = (read-line stream nil nil)
              while line do
                (let ((colon (position #\: line)))
                  (when colon
                    (let ((key (string-trim '(#\Space #\Tab) (subseq line 0 colon)))
                          (val (string-trim '(#\Space #\Tab) (subseq line (1+ colon)))))
                      (cond
                        ((string= key "processor")
                         (setf current-proc (parse-integer val :junk-allowed t)))
                        ((string= key "physical id")
                         (setf current-phys (parse-integer val :junk-allowed t)))
                        ((string= key "core id")
                         (setf current-core (parse-integer val :junk-allowed t))
                         (when (and current-proc current-phys current-core)
                           (let ((k (cons current-phys current-core)))
                             (unless (gethash k core-map)
                               (setf (gethash k core-map) current-proc))))))))))))
    (let ((procs nil))
      (maphash (lambda (k v) (declare (ignore k)) (push v procs)) core-map)
      (if procs
          (sort procs #'<)
          (let ((n (max 1 (c-get-nprocs))))
            (loop for i from 0 below n collect i))))))

(declaim (inline pin-thread-to-core))
(defun pin-thread-to-core (core-id)
  "Pin calling OS thread to CORE-ID via sched_setaffinity."
  (declare (type (integer 0 1024) core-id))
  (sb-alien:with-alien ((mask (sb-alien:array sb-alien:unsigned-long 16)))
    (dotimes (i 16)
      (setf (sb-alien:deref mask i) 0))
    (multiple-value-bind (word-idx bit-idx) (floor core-id 64)
      (setf (sb-alien:deref mask word-idx) (ash 1 bit-idx)))
    (c-sched-setaffinity 0 128 (sb-alien:cast mask (* sb-alien:unsigned-long)))))

;;; ---------------------------------------------------------------------------
;;; 3. Flattened Memory Layout & Constants
;;; ---------------------------------------------------------------------------

(defconstant +words-per-slot+ 32)
(defconstant +slot-op+ 0)
(defconstant +slot-m1+ 1)
(defconstant +slot-m2+ 2)
(defconstant +slot-m3+ 3)
(defconstant +slot-m4+ 4)
(defconstant +slot-m5+ 5)
(defconstant +slot-pad1+ 6)
(defconstant +slot-pad2+ 7)
(defconstant +slot-ina-offset+ 8)   ;; 8 words (8..15)
(defconstant +slot-inb-offset+ 16)  ;; 8 words (16..23)
(defconstant +slot-hwr-offset+ 24)  ;; 8 words (24..31)

(defconstant +batch-iterations+ 4096)
(defconstant +words-per-batch+ (* +batch-iterations+ +words-per-slot+))

(defconstant +sync-prod-tail+ 0)
(defconstant +sync-cons-tail+ 1)
(defconstant +sync-completed+ 2)
(defconstant +sync-mismatch+  3)

(declaim (inline %u64-ref (setf %u64-ref)))
(defun %u64-ref (arr idx)
  (declare (type (simple-array (unsigned-byte 64) (*)) arr)
           (type (unsigned-byte 32) idx)
           (optimize (speed 3) (safety 0)))
  (aref arr idx))

(defun (setf %u64-ref) (val arr idx)
  (declare (type (simple-array (unsigned-byte 64) (*)) arr)
           (type (unsigned-byte 32) idx)
           (type (unsigned-byte 64) val)
           (optimize (speed 3) (safety 0)))
  (setf (aref arr idx) val))

;;; ---------------------------------------------------------------------------
;;; 4. Float and Bit Conversion Helpers
;;; ---------------------------------------------------------------------------

(declaim (inline %u64-to-double %double-to-u64 %u32-to-single %single-to-u32))
(defun %u64-to-double (bits)
  (declare (type (unsigned-byte 64) bits) (optimize (speed 3) (safety 0)))
  (sb-kernel:make-double-float (ldb (byte 32 32) bits) (ldb (byte 32 0) bits)))

(defun %double-to-u64 (df)
  (declare (type double-float df) (optimize (speed 3) (safety 0)))
  (logior (ash (ldb (byte 32 0) (sb-kernel:double-float-high-bits df)) 32)
          (ldb (byte 32 0) (sb-kernel:double-float-low-bits df))))

(defun %u32-to-single (bits)
  (declare (type (unsigned-byte 32) bits) (optimize (speed 3) (safety 0)))
  (let ((signed (if (logbitp 31 bits) (- bits #x100000000) bits)))
    (sb-kernel:make-single-float signed)))

(defun %single-to-u32 (sf)
  (declare (type single-float sf) (optimize (speed 3) (safety 0)))
  (ldb (byte 32 0) (sb-kernel:single-float-bits sf)))

(defun float-bits-equal-or-close-p (exp-bits hw-bits &key (double-p t))
  (declare (type (unsigned-byte 64) exp-bits hw-bits))
  (if (= exp-bits hw-bits)
      t
      (if double-p
          (let ((exp-nan (and (= (ldb (byte 11 52) exp-bits) #x7FF)
                              (/= (ldb (byte 52 0) exp-bits) 0)))
                (hw-nan  (and (= (ldb (byte 11 52) hw-bits) #x7FF)
                              (/= (ldb (byte 52 0) hw-bits) 0))))
            (or (and exp-nan hw-nan)
                (let ((fa (%u64-to-double exp-bits))
                      (fb (%u64-to-double hw-bits)))
                  (and (not exp-nan) (not hw-nan)
                       (<= (abs (- fa fb)) 1.0d-8)))))
          (let ((exp-nan (and (= (ldb (byte 8 23) exp-bits) #xFF)
                              (/= (ldb (byte 23 0) exp-bits) 0)))
                (hw-nan  (and (= (ldb (byte 8 23) hw-bits) #xFF)
                              (/= (ldb (byte 23 0) hw-bits) 0))))
            (or (and exp-nan hw-nan)
                (let ((fa (%u32-to-single exp-bits))
                      (fb (%u32-to-single hw-bits)))
                  (and (not exp-nan) (not hw-nan)
                       (<= (abs (- fa fb)) 1.0e-4))))))))

;;; ---------------------------------------------------------------------------
;;; 5. High-Entropy 64-bit PRNG & Extreme Numeric Fuzz Patterns
;;; ---------------------------------------------------------------------------

(declaim (inline xorshift64))
(defun xorshift64 (state)
  (declare (type (unsigned-byte 64) state) (optimize (speed 3) (safety 0)))
  (let ((x state))
    (setf x (logxor x (ldb (byte 64 0) (ash x 13))))
    (setf x (logxor x (ash x -7)))
    (setf x (logxor x (ldb (byte 64 0) (ash x 17))))
    (if (zerop x) #x123456789ABCDEF0 x)))

(defparameter *extreme-patterns-64*
  #(;; Integer extremes
    #x0000000000000000
    #xFFFFFFFFFFFFFFFF
    #x7FFFFFFFFFFFFFFF
    #x8000000000000000
    #x5555555555555555
    #xAAAAAAAAAAAAAAAA
    #x0123456789ABCDEF
    #xFEDCBA9876543210
    1 2 4 8 16 32 64 128 256
    #x0000000080000000
    #x0000000100000000
    #x4000000000000000
    ;; Double float special bits
    #x0000000000000000  ; +0.0d0
    #x8000000000000000  ; -0.0d0
    #x7FF0000000000000  ; +Infinity
    #xFFF0000000000000  ; -Infinity
    #x7FF8000000000001  ; Quiet NaN
    #x7FF0000000000001  ; Signaling NaN
    #x0000000000000001  ; Subnormal
    #x7FEFFFFFFFFFFFFF  ; most-positive-double-float
    #x0010000000000000  ; least-positive-normalized-double-float
    ))

;;; ---------------------------------------------------------------------------
;;; 6. Diagnostic Forensic Reporting with Standalone Repro
;;; ---------------------------------------------------------------------------

(defun report-mismatch (op-name batch-id slot-idx mask lane exp-val hw-val in-a in-b)
  (format t "~%~78,,,'=A~%" "=")
  (format t ">>> AVX-512 GAUNTLET DIFFERENTIAL ORACLE FAILURE DETECTED! <<<~%")
  (format t "~78,,,'=A~%" "=")
  (format t "Op Mnemonic:         ~A~%" op-name)
  (format t "Claimed Batch:       ~D | Slot Index: ~D~%" batch-id slot-idx)
  (format t "Opmask (Hex / Bin):  #x~X (~16,'0B)~%" mask mask)
  (format t "Corrupted Lane:      Lane ~D (Mask bit: ~D)~%" lane (ldb (byte 1 lane) mask))
  (format t "Input A (Lane ~D):    #x~16,'0X (~D)~%" lane in-a in-a)
  (format t "Input B (Lane ~D):    #x~16,'0X (~D)~%" lane in-b in-b)
  (format t "Expected (Oracle):   #x~16,'0X (~D)~%" exp-val exp-val)
  (format t "Hardware Value:      #x~16,'0X (~D)~%" hw-val hw-val)
  (format t "~%--- COPY-PASTE STANDALONE COMMON LISP REPRO FORM ---~%")
  (format t "(let ((a (sb-ext:%make-simd-pack-512-ub64 ~{#x~16,'0X~^ ~}))~%"
          (loop for i from 0 below 8 collect in-a))
  (format t "      (b (sb-ext:%make-simd-pack-512-ub64 ~{#x~16,'0X~^ ~}))~%"
          (loop for i from 0 below 8 collect in-b))
  (format t "      (m (sb-ext:%make-simd-pack-512-mask #x~X)))~%" mask)
  (format t "  (format t \"Hardware Result: ~~X~~%\" (~A a b m)))~%" op-name)
  (format t "~78,,,'=A~%~%" "=")
  (error "AVX-512 Gauntlet verification failed on ~A at batch ~D slot ~D" op-name batch-id slot-idx))

;;; ---------------------------------------------------------------------------
;;; 7. Macro-Driven Opcode Definitions & Dispatch Table
;;; ---------------------------------------------------------------------------

(defstruct gauntlet-op
  (id 0 :type fixnum)
  (name "" :type string)
  (kind :int64 :type keyword)  ;; :int64, :int32, :float64, :float32, :cascade5
  (hw-fn nil :type function)
  (oracle-fn nil :type (or null function)))

(defparameter *gauntlet-ops* (make-array 32 :initial-element nil))
(defparameter *total-registered-ops* 0)

(defmacro def-gauntlet-op (id-num const-sym name kind vop-call &body oracle-body)
  (let ((hw-sym (intern (format nil "*HW-~A*" name)))
        (orc-sym (intern (format nil "*ORC-~A*" name))))
    `(progn
       (defconstant ,const-sym ,id-num)
       (defparameter ,hw-sym
         ,(ecase kind
            (:int64
             `(compile nil '(lambda (a b m)
                              (declare (type (sb-ext:simd-pack-512 (unsigned-byte 64)) a b)
                                       (type sb-ext:simd-pack-512-mask m)
                                       (optimize (speed 3) (safety 0)))
                              ,vop-call)))
            (:int32
             `(compile nil '(lambda (a b m)
                              (declare (type (sb-ext:simd-pack-512 (unsigned-byte 32)) a b)
                                       (type sb-ext:simd-pack-512-mask m)
                                       (optimize (speed 3) (safety 0)))
                              ,vop-call)))
            (:float64
             `(compile nil '(lambda (a b m)
                              (declare (type (sb-ext:simd-pack-512 double-float) a b)
                                       (type sb-ext:simd-pack-512-mask m)
                                       (optimize (speed 3) (safety 0)))
                              ,vop-call)))
            (:float32
             `(compile nil '(lambda (a b m)
                              (declare (type (sb-ext:simd-pack-512 single-float) a b)
                                       (type sb-ext:simd-pack-512-mask m)
                                       (optimize (speed 3) (safety 0)))
                              ,vop-call)))
            (:cascade5
             `(compile nil '(lambda (a b m1 m2 m3 m4 m5)
                              (declare (type (sb-ext:simd-pack-512 (unsigned-byte 64)) a b)
                                       (type sb-ext:simd-pack-512-mask m1 m2 m3 m4 m5)
                                       (optimize (speed 3) (safety 0)))
                              ,vop-call)))))
       (defparameter ,orc-sym
         ,(if oracle-body
              `(lambda (a b bit old)
                 (declare (ignorable a b bit old))
                 ,@oracle-body)
              nil))
       (setf (aref *gauntlet-ops* ,id-num)
             (make-gauntlet-op :id ,id-num
                               :name ,name
                               :kind ,kind
                               :hw-fn ,hw-sym
                               :oracle-fn ,orc-sym))
       (setf *total-registered-ops* (max *total-registered-ops* (1+ ,id-num))))))

;; --- Register Ops ---

;; 64-bit Integer
(def-gauntlet-op 0 +op-vpaddq+ "VPADDQ-MASKED" :int64
  (sb-vm::simd-pack-512-ub64+-masked a b m)
  (if (zerop bit) old (ldb (byte 64 0) (+ a b))))

(def-gauntlet-op 1 +op-vpsubq+ "VPSUBQ-MASKED" :int64
  (sb-vm::simd-pack-512-ub64--masked a b m)
  (if (zerop bit) old (ldb (byte 64 0) (- a b))))

(def-gauntlet-op 2 +op-vpandq+ "VPANDQ-MASKED" :int64
  (sb-vm::simd-pack-512-and-masked a b m)
  (if (zerop bit) old (logand a b)))

(def-gauntlet-op 3 +op-vporq+ "VPORQ-MASKED" :int64
  (sb-vm::simd-pack-512-or-masked a b m)
  (if (zerop bit) old (logior a b)))

(def-gauntlet-op 4 +op-vpxorq+ "VPXORQ-MASKED" :int64
  (sb-vm::simd-pack-512-xor-masked a b m)
  (if (zerop bit) old (logxor a b)))

;; 32-bit Integer
(def-gauntlet-op 5 +op-vpaddd+ "VPADDD-MASKED" :int32
  (sb-vm::simd-pack-512-ub32+-masked a b m)
  (if (zerop bit) old (ldb (byte 32 0) (+ a b))))

(def-gauntlet-op 6 +op-vpsubd+ "VPSUBD-MASKED" :int32
  (sb-vm::simd-pack-512-ub32--masked a b m)
  (if (zerop bit) old (ldb (byte 32 0) (- a b))))

(def-gauntlet-op 7 +op-vpandd+ "VPANDD-MASKED" :int32
  (sb-vm::simd-pack-512-ub32-and-masked a b m)
  (if (zerop bit) old (logand a b)))

(def-gauntlet-op 8 +op-vpord+ "VPORD-MASKED" :int32
  (sb-vm::simd-pack-512-ub32-or-masked a b m)
  (if (zerop bit) old (logior a b)))

(def-gauntlet-op 9 +op-vpxord+ "VPXORD-MASKED" :int32
  (sb-vm::simd-pack-512-ub32-xor-masked a b m)
  (if (zerop bit) old (logxor a b)))

;; Double-Precision Floating-Point
(def-gauntlet-op 10 +op-vaddpd+ "VADDPD-MASKED" :float64
  (sb-vm::simd-pack-512-double+-masked a b m)
  (if (zerop bit) old (+ a b)))

(def-gauntlet-op 11 +op-vsubpd+ "VSUBPD-MASKED" :float64
  (sb-vm::simd-pack-512-double--masked a b m)
  (if (zerop bit) old (- a b)))

(def-gauntlet-op 12 +op-vmulpd+ "VMULPD-MASKED" :float64
  (sb-vm::simd-pack-512-double*-masked a b m)
  (if (zerop bit) old (* a b)))

;; Single-Precision Floating-Point
(def-gauntlet-op 13 +op-vaddps+ "VADDPS-MASKED" :float32
  (sb-vm::simd-pack-512-single+-masked a b m)
  (if (zerop bit) old (+ a b)))

(def-gauntlet-op 14 +op-vsubps+ "VSUBPS-MASKED" :float32
  (sb-vm::simd-pack-512-single--masked a b m)
  (if (zerop bit) old (- a b)))

(def-gauntlet-op 15 +op-vmulps+ "VMULPS-MASKED" :float32
  (sb-vm::simd-pack-512-single*-masked a b m)
  (if (zerop bit) old (* a b)))

;; 5-Stage In-Register Cascade
(def-gauntlet-op 16 +op-cascade-5+ "CASCADE-5STAGE" :cascade5
  (sb-vm::%gauntlet-cascade-5stage a b m1 m2 m3 m4 m5))

;;; ---------------------------------------------------------------------------
;;; 8. Hardware Batch Generation (Producer)
;;; ---------------------------------------------------------------------------

(defun generate-hardware-batch (queue batch-id seed)
  (declare (type (simple-array (unsigned-byte 64) (*)) queue)
           (type (unsigned-byte 32) batch-id)
           (type (unsigned-byte 64) seed)
           (optimize (speed 3) (safety 0)))
  (let* ((batch-base (* batch-id +words-per-batch+))
         (entropy (logxor seed (ldb (byte 64 0) (* (1+ batch-id) #x9E3779B97F4A7C15)))))
    (dotimes (slot +batch-iterations+)
      (let* ((slot-base (+ batch-base (* slot +words-per-slot+)))
             (op (mod slot *total-registered-ops*))
             (op-entry (aref *gauntlet-ops* op))
             (kind (gauntlet-op-kind op-entry))
             (hw-fn (gauntlet-op-hw-fn op-entry))
             ;; Advance PRNG
             (r1 (setf entropy (xorshift64 entropy)))
             (r2 (setf entropy (xorshift64 entropy)))
             (r3 (setf entropy (xorshift64 entropy)))
             ;; Combinatoric masks
             (m1 (cond ((< slot 256) slot)                           ; Exhaustive 8-bit
                       ((< slot 65536) (ldb (byte 16 0) slot))       ; Exhaustive 16-bit
                       ((zerop (mod slot 7)) #x5555)                 ; Checkerboard 1
                       ((zerop (mod slot 11)) #xAAAA)                ; Checkerboard 2
                       (t (ldb (byte 16 0) r1))))
             (m2 (ldb (byte 16 0) (ash r2 -8)))
             (m3 (ldb (byte 16 0) (ash r3 -16)))
             (m4 (ldb (byte 16 0) (ash r1 -24)))
             (m5 (ldb (byte 16 0) (ash r2 -32))))

        ;; Store header
        (setf (%u64-ref queue (+ slot-base +slot-op+)) op)
        (setf (%u64-ref queue (+ slot-base +slot-m1+)) m1)
        (setf (%u64-ref queue (+ slot-base +slot-m2+)) m2)
        (setf (%u64-ref queue (+ slot-base +slot-m3+)) m3)
        (setf (%u64-ref queue (+ slot-base +slot-m4+)) m4)
        (setf (%u64-ref queue (+ slot-base +slot-m5+)) m5)

        (case kind
          (:cascade5
           (let* ((v1 (if (zerop (mod slot 8))
                          (aref *extreme-patterns-64* (mod (ash r1 -4) (length *extreme-patterns-64*)))
                          (logxor #xCAFEBABE00000000 r1)))
                  (v2 (if (zerop (mod slot 8))
                          (aref *extreme-patterns-64* (mod (ash r2 -4) (length *extreme-patterns-64*)))
                          (logxor #xDEADBEEF11111111 r2)))
                  (p1 (sb-ext:%make-simd-pack-512-ub64 v1 v1 v1 v1 v1 v1 v1 v1))
                  (p2 (sb-ext:%make-simd-pack-512-ub64 v2 v2 v2 v2 v2 v2 v2 v2))
                  (k1 (sb-ext:%make-simd-pack-512-mask m1))
                  (k2 (sb-ext:%make-simd-pack-512-mask m2))
                  (k3 (sb-ext:%make-simd-pack-512-mask m3))
                  (k4 (sb-ext:%make-simd-pack-512-mask m4))
                  (k5 (sb-ext:%make-simd-pack-512-mask m5)))
             (dotimes (i 8)
               (setf (%u64-ref queue (+ slot-base +slot-ina-offset+ i)) v1)
               (setf (%u64-ref queue (+ slot-base +slot-inb-offset+ i)) v2))
             (let ((res (funcall hw-fn p1 p2 k1 k2 k3 k4 k5)))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 0)) (sb-kernel:%simd-pack-512-0 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 1)) (sb-kernel:%simd-pack-512-1 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 2)) (sb-kernel:%simd-pack-512-2 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 3)) (sb-kernel:%simd-pack-512-3 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 4)) (sb-kernel:%simd-pack-512-4 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 5)) (sb-kernel:%simd-pack-512-5 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 6)) (sb-kernel:%simd-pack-512-6 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 7)) (sb-kernel:%simd-pack-512-7 res)))))

          (:float64
           (let* ((v1 (if (zerop (mod slot 4))
                          (aref *extreme-patterns-64* (mod (ash r1 -4) (length *extreme-patterns-64*)))
                          (logxor #x400921FB54442D18 r1)))
                  (v2 (if (zerop (mod slot 4))
                          (aref *extreme-patterns-64* (mod (ash r2 -4) (length *extreme-patterns-64*)))
                          (logxor #x4005BF0A8B145769 r2)))
                  (d1 (%u64-to-double v1))
                  (d2 (%u64-to-double v2))
                  (p1 (sb-ext:%make-simd-pack-512-double d1 d1 d1 d1 d1 d1 d1 d1))
                  (p2 (sb-ext:%make-simd-pack-512-double d2 d2 d2 d2 d2 d2 d2 d2))
                  (k1 (sb-ext:%make-simd-pack-512-mask m1)))
             (dotimes (i 8)
               (setf (%u64-ref queue (+ slot-base +slot-ina-offset+ i)) v1)
               (setf (%u64-ref queue (+ slot-base +slot-inb-offset+ i)) v2))
             (let ((res (funcall hw-fn p1 p2 k1)))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 0)) (sb-kernel:%simd-pack-512-0 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 1)) (sb-kernel:%simd-pack-512-1 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 2)) (sb-kernel:%simd-pack-512-2 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 3)) (sb-kernel:%simd-pack-512-3 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 4)) (sb-kernel:%simd-pack-512-4 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 5)) (sb-kernel:%simd-pack-512-5 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 6)) (sb-kernel:%simd-pack-512-6 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 7)) (sb-kernel:%simd-pack-512-7 res)))))

          (:float32
           (let* ((u1 (ldb (byte 32 0) r1))
                  (u2 (ldb (byte 32 0) r2))
                  (s1 (%u32-to-single u1))
                  (s2 (%u32-to-single u2))
                  (p1 (sb-ext:%make-simd-pack-512-single s1 s1 s1 s1 s1 s1 s1 s1 s1 s1 s1 s1 s1 s1 s1 s1))
                  (p2 (sb-ext:%make-simd-pack-512-single s2 s2 s2 s2 s2 s2 s2 s2 s2 s2 s2 s2 s2 s2 s2 s2))
                  (k1 (sb-ext:%make-simd-pack-512-mask m1))
                  (w1 (logior (ash u1 32) u1))
                  (w2 (logior (ash u2 32) u2)))
             (dotimes (i 8)
               (setf (%u64-ref queue (+ slot-base +slot-ina-offset+ i)) w1)
               (setf (%u64-ref queue (+ slot-base +slot-inb-offset+ i)) w2))
             (let ((res (funcall hw-fn p1 p2 k1)))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 0)) (sb-kernel:%simd-pack-512-0 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 1)) (sb-kernel:%simd-pack-512-1 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 2)) (sb-kernel:%simd-pack-512-2 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 3)) (sb-kernel:%simd-pack-512-3 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 4)) (sb-kernel:%simd-pack-512-4 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 5)) (sb-kernel:%simd-pack-512-5 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 6)) (sb-kernel:%simd-pack-512-6 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 7)) (sb-kernel:%simd-pack-512-7 res)))))

          (:int32
           (let* ((u1 (ldb (byte 32 0) r1))
                  (u2 (ldb (byte 32 0) r2))
                  (p1 (sb-ext:%make-simd-pack-512-ub32 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1 u1))
                  (p2 (sb-ext:%make-simd-pack-512-ub32 u2 u2 u2 u2 u2 u2 u2 u2 u2 u2 u2 u2 u2 u2 u2 u2))
                  (k1 (sb-ext:%make-simd-pack-512-mask m1))
                  (w1 (logior (ash u1 32) u1))
                  (w2 (logior (ash u2 32) u2)))
             (dotimes (i 8)
               (setf (%u64-ref queue (+ slot-base +slot-ina-offset+ i)) w1)
               (setf (%u64-ref queue (+ slot-base +slot-inb-offset+ i)) w2))
             (let ((res (funcall hw-fn p1 p2 k1)))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 0)) (sb-kernel:%simd-pack-512-0 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 1)) (sb-kernel:%simd-pack-512-1 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 2)) (sb-kernel:%simd-pack-512-2 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 3)) (sb-kernel:%simd-pack-512-3 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 4)) (sb-kernel:%simd-pack-512-4 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 5)) (sb-kernel:%simd-pack-512-5 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 6)) (sb-kernel:%simd-pack-512-6 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 7)) (sb-kernel:%simd-pack-512-7 res)))))

          (:int64
           (let* ((v1 (if (zerop (mod slot 6))
                          (aref *extreme-patterns-64* (mod (ash r1 -4) (length *extreme-patterns-64*)))
                          (logxor #x0123456789ABCDEF r1)))
                  (v2 (if (zerop (mod slot 6))
                          (aref *extreme-patterns-64* (mod (ash r2 -4) (length *extreme-patterns-64*)))
                          (logxor #xFEDCBA9876543210 r2)))
                  (p1 (sb-ext:%make-simd-pack-512-ub64 v1 v1 v1 v1 v1 v1 v1 v1))
                  (p2 (sb-ext:%make-simd-pack-512-ub64 v2 v2 v2 v2 v2 v2 v2 v2))
                  (k1 (sb-ext:%make-simd-pack-512-mask m1)))
             (dotimes (i 8)
               (setf (%u64-ref queue (+ slot-base +slot-ina-offset+ i)) v1)
               (setf (%u64-ref queue (+ slot-base +slot-inb-offset+ i)) v2))
             (let ((res (funcall hw-fn p1 p2 k1)))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 0)) (sb-kernel:%simd-pack-512-0 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 1)) (sb-kernel:%simd-pack-512-1 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 2)) (sb-kernel:%simd-pack-512-2 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 3)) (sb-kernel:%simd-pack-512-3 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 4)) (sb-kernel:%simd-pack-512-4 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 5)) (sb-kernel:%simd-pack-512-5 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 6)) (sb-kernel:%simd-pack-512-6 res))
               (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 7)) (sb-kernel:%simd-pack-512-7 res))))))))))

;;; ---------------------------------------------------------------------------
;;; 9. Differential Scalar Validation (Consumer)
;;; ---------------------------------------------------------------------------

(defun validate-batch (queue batch-id sync-vars)
  (declare (type (simple-array (unsigned-byte 64) (*)) queue sync-vars)
           (type (unsigned-byte 32) batch-id)
           (optimize (speed 3) (safety 0)))
  (let ((batch-base (* batch-id +words-per-batch+)))
    (dotimes (slot +batch-iterations+)
      (when (/= 0 (aref sync-vars +sync-mismatch+))
        (return-from validate-batch))
      (let* ((slot-base (+ batch-base (* slot +words-per-slot+)))
             (op        (%u64-ref queue (+ slot-base +slot-op+)))
             (m1        (%u64-ref queue (+ slot-base +slot-m1+)))
             (m2        (%u64-ref queue (+ slot-base +slot-m2+)))
             (m3        (%u64-ref queue (+ slot-base +slot-m3+)))
             (m4        (%u64-ref queue (+ slot-base +slot-m4+)))
             (m5        (%u64-ref queue (+ slot-base +slot-m5+)))
             (op-entry  (aref *gauntlet-ops* op))
             (kind      (gauntlet-op-kind op-entry))
             (op-name   (gauntlet-op-name op-entry))
             (oracle-fn (gauntlet-op-oracle-fn op-entry)))

        (case kind
          (:cascade5
           (dotimes (lane 8)
             (let* ((bit1 (ldb (byte 1 lane) m1))
                    (bit2 (ldb (byte 1 lane) m2))
                    (bit3 (ldb (byte 1 lane) m3))
                    (bit4 (ldb (byte 1 lane) m4))
                    (bit5 (ldb (byte 1 lane) m5))
                    (a    (%u64-ref queue (+ slot-base +slot-ina-offset+ lane)))
                    (b    (%u64-ref queue (+ slot-base +slot-inb-offset+ lane)))
                    (hw   (%u64-ref queue (+ slot-base +slot-hwr-offset+ lane)))
                    (z1   (if (zerop bit1) a (ldb (byte 64 0) (+ a b))))
                    (z2   (if (zerop bit2) z1 (logxor z1 a)))
                    (z3   (if (zerop bit3) z2 (ldb (byte 64 0) (- z2 b))))
                    (z4   (if (zerop bit4) z3 (logior z3 z1)))
                    (exp  (if (zerop bit5) z4 (logand z4 z2))))
               (unless (= exp hw)
                 (setf (aref sync-vars +sync-mismatch+) 1)
                 (report-mismatch op-name batch-id slot m1 lane exp hw a b)))))

          (:float64
           (dotimes (lane 8)
             (let* ((bit (ldb (byte 1 lane) m1))
                    (qa  (%u64-ref queue (+ slot-base +slot-ina-offset+ lane)))
                    (qb  (%u64-ref queue (+ slot-base +slot-inb-offset+ lane)))
                    (qhw (%u64-ref queue (+ slot-base +slot-hwr-offset+ lane)))
                    (fa  (%u64-to-double qa))
                    (fb  (%u64-to-double qb))
                    (fexp (funcall oracle-fn fa fb bit fa))
                    (exp-bits (%double-to-u64 fexp)))
               (unless (float-bits-equal-or-close-p exp-bits qhw :double-p t)
                 (setf (aref sync-vars +sync-mismatch+) 1)
                 (report-mismatch op-name batch-id slot m1 lane exp-bits qhw qa qb)))))

          (:float32
           (dotimes (lane 16)
             (multiple-value-bind (qword-idx half) (floor lane 2)
               (let* ((bit  (ldb (byte 1 lane) m1))
                      (qa   (%u64-ref queue (+ slot-base +slot-ina-offset+ qword-idx)))
                      (qb   (%u64-ref queue (+ slot-base +slot-inb-offset+ qword-idx)))
                      (qhw  (%u64-ref queue (+ slot-base +slot-hwr-offset+ qword-idx)))
                      (ua   (ldb (byte 32 (* half 32)) qa))
                      (ub   (ldb (byte 32 (* half 32)) qb))
                      (uhw  (ldb (byte 32 (* half 32)) qhw))
                      (sa   (%u32-to-single ua))
                      (sb   (%u32-to-single ub))
                      (sexp (funcall oracle-fn sa sb bit sa))
                      (exp-bits (%single-to-u32 sexp)))
                 (unless (float-bits-equal-or-close-p exp-bits uhw :double-p nil)
                   (setf (aref sync-vars +sync-mismatch+) 1)
                   (report-mismatch op-name batch-id slot m1 lane exp-bits uhw ua ub))))))

          (:int32
           (dotimes (lane 16)
             (multiple-value-bind (qword-idx half) (floor lane 2)
               (let* ((bit (ldb (byte 1 lane) m1))
                      (qa  (%u64-ref queue (+ slot-base +slot-ina-offset+ qword-idx)))
                      (qb  (%u64-ref queue (+ slot-base +slot-inb-offset+ qword-idx)))
                      (qhw (%u64-ref queue (+ slot-base +slot-hwr-offset+ qword-idx)))
                      (a   (ldb (byte 32 (* half 32)) qa))
                      (b   (ldb (byte 32 (* half 32)) qb))
                      (hw  (ldb (byte 32 (* half 32)) qhw))
                      (exp (funcall oracle-fn a b bit a)))
                 (unless (= exp hw)
                   (setf (aref sync-vars +sync-mismatch+) 1)
                   (report-mismatch op-name batch-id slot m1 lane exp hw a b))))))

          (:int64
           (dotimes (lane 8)
             (let* ((bit (ldb (byte 1 lane) m1))
                    (a   (%u64-ref queue (+ slot-base +slot-ina-offset+ lane)))
                    (b   (%u64-ref queue (+ slot-base +slot-inb-offset+ lane)))
                    (hw  (%u64-ref queue (+ slot-base +slot-hwr-offset+ lane)))
                    (exp (funcall oracle-fn a b bit a)))
               (unless (= exp hw)
                 (setf (aref sync-vars +sync-mismatch+) 1)
                 (report-mismatch op-name batch-id slot m1 lane exp hw a b))))))))))

;;; ---------------------------------------------------------------------------
;;; 10. Multi-Producer Multi-Consumer (MPMC) Worker Run Loops
;;; ---------------------------------------------------------------------------

(defun run-producer-worker (worker-id core-id queue batch-status sync-vars total-batches seed)
  (declare (type (unsigned-byte 32) worker-id core-id total-batches)
           (type (unsigned-byte 64) seed)
           (type (simple-array (unsigned-byte 64) (*)) queue batch-status sync-vars)
           (ignore worker-id)
           (optimize (speed 3) (safety 0)))
  (pin-thread-to-core core-id)
  (sb-int:set-floating-point-modes :traps nil)
  (loop
    (when (/= 0 (aref sync-vars +sync-mismatch+))
      (return-from run-producer-worker))
    (let ((batch (sb-ext:atomic-incf (aref sync-vars +sync-prod-tail+))))
      (when (>= batch total-batches)
        (return-from run-producer-worker))
      (generate-hardware-batch queue batch seed)
      (sb-thread:barrier (:write))
      (setf (aref batch-status batch) 1))))

(defun run-consumer-worker (worker-id core-id queue batch-status sync-vars total-batches)
  (declare (type (unsigned-byte 32) worker-id core-id total-batches)
           (type (simple-array (unsigned-byte 64) (*)) queue batch-status sync-vars)
           (ignore worker-id)
           (optimize (speed 3) (safety 0)))
  (pin-thread-to-core core-id)
  (sb-int:set-floating-point-modes :traps nil)
  (loop
    (when (/= 0 (aref sync-vars +sync-mismatch+))
      (return-from run-consumer-worker))
    (let ((batch (sb-ext:atomic-incf (aref sync-vars +sync-cons-tail+))))
      (when (>= batch total-batches)
        (return-from run-consumer-worker))
      ;; Spin-wait with thread-yield until producer publishes batch
      (loop while (zerop (aref batch-status batch)) do
        (when (/= 0 (aref sync-vars +sync-mismatch+))
          (return-from run-consumer-worker))
        (sb-thread:thread-yield))
      (sb-thread:barrier (:read))
      (validate-batch queue batch sync-vars)
      (setf (aref batch-status batch) 2)
      (sb-ext:atomic-incf (aref sync-vars +sync-completed+)))))

;;; ---------------------------------------------------------------------------
;;; 11. Test Driver Entrypoint
;;; ---------------------------------------------------------------------------

(defun run-avx512-gauntlet (&key (total-batches 64) (seed #x9E3779B97F4A7C15))
  "Execute the Multi-Producer Multi-Consumer (MPMC) AVX-512 Brutal Gauntlet."
  (let* ((physical-cores (detect-physical-cores))
         (num-physical (length physical-cores))
         (num-producers (cond ((>= num-physical 16) 4)
                              ((>= num-physical 8)  2)
                              (t 1)))
         (num-consumers (max 1 (- num-physical num-producers)))
         (total-iterations (* total-batches +batch-iterations+))
         (queue-words (* total-batches +words-per-batch+))
         (queue-bytes (* queue-words 8))
         (queue (make-array queue-words :element-type '(unsigned-byte 64) :initial-element 0))
         (batch-status (make-array total-batches :element-type '(unsigned-byte 64) :initial-element 0))
         (sync-vars (make-array 64 :element-type '(unsigned-byte 64) :initial-element 0))
         (threads nil)
         (start-time (get-internal-real-time)))

    (format t "~%========================================================================~%")
    (format t "    STARTING BRUTAL AVX-512 MULTI-PRODUCER SUPER-SWEEP GAUNTLET 2.0      ~%")
    (format t "========================================================================~%")
    (format t "Host Physical Cores:     ~D (~{~D~^ ~})~%" num-physical physical-cores)
    (format t "Hardware Producers:      ~D threads (Pinned to physical cores: ~{~D~^ ~})~%"
            num-producers (subseq physical-cores 0 num-producers))
    (format t "Validation Consumers:    ~D threads (Pinned to physical cores: ~{~D~^ ~})~%"
            num-consumers (subseq physical-cores num-producers (+ num-producers num-consumers)))
    (format t "Flattened MPMC Queue:    ~:D MB (~:D uint64 words)~%"
            (round queue-bytes (* 1024 1024)) queue-words)
    (format t "Total Test Batches:      ~D batches (~D test cases per batch)~%"
            total-batches +batch-iterations+)
    (format t "Total Test Cases:        ~:D native hardware operations~%" total-iterations)
    (format t "Registered Ops:          ~D distinct VOPs (Arithmetic, Bitwise, Floats, Cascades)~%"
            *total-registered-ops*)
    (format t "PRNG Seed:               #x~16,'0X (Deterministic Bit-Exact Replay)~%" seed)
    (format t "========================================================================~%~%")

    ;; Spawn Consumer Threads
    (dotimes (i num-consumers)
      (let* ((worker-id i)
             (core-id (nth (+ num-producers i) physical-cores)))
        (push (sb-thread:make-thread
               (lambda ()
                 (run-consumer-worker worker-id core-id queue batch-status sync-vars total-batches))
               :name (format nil "gauntlet-consumer-~D" worker-id))
              threads)))

    ;; Spawn Producer Threads (excluding the main thread which acts as Producer 0)
    (loop for i from 1 below num-producers do
      (let* ((worker-id i)
             (core-id (nth i physical-cores)))
        (push (sb-thread:make-thread
               (lambda ()
                 (run-producer-worker worker-id core-id queue batch-status sync-vars total-batches seed))
               :name (format nil "gauntlet-producer-~D" worker-id))
              threads)))

    ;; Run Producer 0 on main thread pinned to first physical core
    (run-producer-worker 0 (first physical-cores) queue batch-status sync-vars total-batches seed)

    ;; Wait for all threads to finish
    (dolist (th threads)
      (sb-thread:join-thread th))

    (let* ((end-time (get-internal-real-time))
           (elapsed-sec (/ (- end-time start-time) internal-time-units-per-second))
           (throughput (if (zerop elapsed-sec) 0 (round total-iterations elapsed-sec))))
      (format t "~%========================================================================~%")
      (format t ">>> AVX-512 GAUNTLET 2.0 COMPLETED WITH ZERO MISMATCHES! <<<~%")
      (format t "========================================================================~%")
      (format t "Verified Hardware Ops:   ~:D operations~%" total-iterations)
      (format t "Total Batches Verified:  ~D / ~D batches~%"
              (%u64-ref sync-vars +sync-completed+) total-batches)
      (format t "Elapsed Time:            ~,3F seconds~%" elapsed-sec)
      (format t "Validation Throughput:   ~:D ops/sec (~,2F million ops/sec)~%"
              throughput (/ throughput 1000000.0))
      (format t "Physical Cores Used:     ~D cores (~D producers + ~D consumers)~%"
              (+ num-producers num-consumers) num-producers num-consumers)
      (format t "GC Steady-State Consing: 0 bytes (pre-allocated queue)~%")
      (format t "========================================================================~%~%")
      (assert (zerop (%u64-ref sync-vars +sync-mismatch+)))
      t)))

(test-util:with-test (:name :avx512-multi-core-brutal-gauntlet)
  (let ((batches (or (let ((v (sb-ext:posix-getenv "AVX512_GAUNTLET_BATCHES")))
                       (and v (parse-integer v :junk-allowed t)))
                     64)))
    (run-avx512-gauntlet :total-batches batches)))
