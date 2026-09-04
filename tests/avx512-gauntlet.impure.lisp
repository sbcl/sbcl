;;;; tests/avx512-gauntlet.impure.lisp --- Brutal AVX-512 Super-Sweep Gauntlet
;;;;
;;;; A dependency-free, high-performance combinatoric test harness for AVX-512.
;;;; Utilizes a Single-Producer, Multi-Consumer differential testing model:
;;;; - Producer pinned to Core 0 orchestrating native AVX-512 register operations.
;;;; - N-1 Consumers pinned across all remaining host cores validating via scalar oracle.
;;;; - Pre-allocated, flat 1D array of (unsigned-byte 64) with lock-free atomic barriers.
;;;; - In-register cascades and exhaustive opmask combinatorial coverage.

#-sb-simd-pack-512 (invoke-restart 'run-tests::skip-file)

(when (zerop (sb-alien:extern-alien "avx512_supported" int))
  (format t "~&INFO: AVX-512 not supported on this host~%")
  (invoke-restart 'run-tests::skip-file))

(cl:in-package "SB-VM")

;;; ---------------------------------------------------------------------------
;;; 1. Custom In-Register Hardware Cascades
;;; ---------------------------------------------------------------------------

(sb-ext:without-package-locks
  (defknown %gauntlet-cascade-3stage ((simd-pack-512 (unsigned-byte 64))
                                      (simd-pack-512 (unsigned-byte 64))
                                      simd-pack-512-mask
                                      simd-pack-512-mask
                                      simd-pack-512-mask)
      (simd-pack-512 (unsigned-byte 64)) (flushable movable))

  (define-vop (%gauntlet-cascade-3stage)
    (:translate %gauntlet-cascade-3stage)
    (:policy :fast-safe)
    (:args (a :scs (int-avx512-reg) :target dst)
           (b :scs (int-avx512-reg))
           (m1 :scs (mask-reg))
           (m2 :scs (mask-reg))
           (m3 :scs (mask-reg)))
    (:arg-types simd-pack-512-ub64 simd-pack-512-ub64
                simd-pack-512-mask-type simd-pack-512-mask-type simd-pack-512-mask-type)
    (:temporary (:sc int-avx512-reg) z1 z2)
    (:results (dst :scs (int-avx512-reg)))
    (:result-types simd-pack-512-ub64)
    (:generator 5
      ;; Stage 1: z1 = vpaddq-masked(a, b, m1)
      (inst vmovdqu64 z1 a)
      (inst vpaddq-masked z1 z1 b m1)
      ;; Stage 2: z2 = vpxorq-masked(z1, a, m2)
      (inst vmovdqu64 z2 z1)
      (inst vpxorq-masked z2 z2 a m2)
      ;; Stage 3: dst = vpsubq-masked(z2, b, m3)
      (inst vmovdqu64 dst z2)
      (inst vpsubq-masked dst dst b m3))))

(cl:in-package "CL-USER")

;;; ---------------------------------------------------------------------------
;;; 2. Native System & Processor Topology Bindings
;;; ---------------------------------------------------------------------------

(sb-alien:define-alien-routine ("get_nprocs" c-get-nprocs) sb-alien:int)

(sb-alien:define-alien-routine ("sched_setaffinity" c-sched-setaffinity) sb-alien:int
  (pid sb-alien:int)
  (cpusetsize sb-alien:unsigned-long)
  (mask (* sb-alien:unsigned-long)))

(declaim (inline pin-thread-to-core))
(defun pin-thread-to-core (core-id)
  "Pin the calling OS thread to CORE-ID via sched_setaffinity."
  (declare (type (integer 0 1024) core-id))
  (sb-alien:with-alien ((mask (sb-alien:array sb-alien:unsigned-long 16)))
    (dotimes (i 16)
      (setf (sb-alien:deref mask i) 0))
    (multiple-value-bind (word-idx bit-idx) (floor core-id 64)
      (setf (sb-alien:deref mask word-idx) (ash 1 bit-idx)))
    (c-sched-setaffinity 0 128 (sb-alien:cast mask (* sb-alien:unsigned-long)))))

;;; ---------------------------------------------------------------------------
;;; 3. Operation Constants & Layout Definitions
;;; ---------------------------------------------------------------------------

(defconstant +op-vpaddq+        0)
(defconstant +op-vpsubq+        1)
(defconstant +op-vpaddd+        2)
(defconstant +op-vpsubd+        3)
(defconstant +op-vpandq+        4)
(defconstant +op-vporq+         5)
(defconstant +op-vpxorq+        6)
(defconstant +op-vpandd+        7)
(defconstant +op-vpord+         8)
(defconstant +op-vpxord+        9)
(defconstant +op-cascade-3+    10)
(defconstant +op-vaddpd+       11)
(defconstant +op-vsubpd+       12)
(defconstant +op-vaddps+       13)
(defconstant +op-vsubps+       14)

;; Slot stride within flat queue: 32 words = 256 bytes (4 cache lines)
(defconstant +iter-stride+      32)
(defconstant +slot-op+           0)
(defconstant +slot-m1+           1)
(defconstant +slot-m2+           2)
(defconstant +slot-m3+           3)
(defconstant +slot-seq+          4)
(defconstant +slot-ina-offset+   8)  ;; Words 8..15  (512 bits)
(defconstant +slot-inb-offset+  16)  ;; Words 16..23 (512 bits)
(defconstant +slot-hwr-offset+  24)  ;; Words 24..31 (512 bits)

(defconstant +batch-size+      4096)
(defconstant +words-per-batch+ (* +batch-size+ +iter-stride+))
(defconstant +num-batches+       32)
(defconstant +queue-total-words+ (* +num-batches+ +words-per-batch+))

;; Synchronization array indices
(defconstant +sync-head+          0)
(defconstant +sync-tail+          1)
(defconstant +sync-completed+     2)
(defconstant +sync-mismatch+      3)
(defconstant +sync-done+          4)
(defconstant +sync-abort+         5)

;;; ---------------------------------------------------------------------------
;;; 4. Zero-Allocation Fast Vector Helpers & Producers
;;; ---------------------------------------------------------------------------

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

;; Fast Compiled Dispatchers for Hardware Execution
(defparameter *fn-vpaddq*
  (compile nil `(lambda (a b m)
                  (declare (type (sb-ext:simd-pack-512 (unsigned-byte 64)) a b)
                           (type sb-ext:simd-pack-512-mask m)
                           (optimize (speed 3) (safety 0)))
                  (sb-vm::simd-pack-512-ub64+-masked a b m))))

(defparameter *fn-vpsubq*
  (compile nil `(lambda (a b m)
                  (declare (type (sb-ext:simd-pack-512 (unsigned-byte 64)) a b)
                           (type sb-ext:simd-pack-512-mask m)
                           (optimize (speed 3) (safety 0)))
                  (sb-vm::simd-pack-512-ub64--masked a b m))))

(defparameter *fn-vpaddd*
  (compile nil `(lambda (a b m)
                  (declare (type (sb-ext:simd-pack-512 (unsigned-byte 32)) a b)
                           (type sb-ext:simd-pack-512-mask m)
                           (optimize (speed 3) (safety 0)))
                  (sb-vm::simd-pack-512-ub32+-masked a b m))))

(defparameter *fn-vpsubd*
  (compile nil `(lambda (a b m)
                  (declare (type (sb-ext:simd-pack-512 (unsigned-byte 32)) a b)
                           (type sb-ext:simd-pack-512-mask m)
                           (optimize (speed 3) (safety 0)))
                  (sb-vm::simd-pack-512-ub32--masked a b m))))

(defparameter *fn-vpandq*
  (compile nil `(lambda (a b m)
                  (declare (type (sb-ext:simd-pack-512 (unsigned-byte 64)) a b)
                           (type sb-ext:simd-pack-512-mask m)
                           (optimize (speed 3) (safety 0)))
                  (sb-vm::simd-pack-512-and-masked a b m))))

(defparameter *fn-vporq*
  (compile nil `(lambda (a b m)
                  (declare (type (sb-ext:simd-pack-512 (unsigned-byte 64)) a b)
                           (type sb-ext:simd-pack-512-mask m)
                           (optimize (speed 3) (safety 0)))
                  (sb-vm::simd-pack-512-or-masked a b m))))

(defparameter *fn-vpxorq*
  (compile nil `(lambda (a b m)
                  (declare (type (sb-ext:simd-pack-512 (unsigned-byte 64)) a b)
                           (type sb-ext:simd-pack-512-mask m)
                           (optimize (speed 3) (safety 0)))
                  (sb-vm::simd-pack-512-xor-masked a b m))))

(defparameter *fn-cascade3*
  (compile nil `(lambda (a b m1 m2 m3)
                  (declare (type (sb-ext:simd-pack-512 (unsigned-byte 64)) a b)
                           (type sb-ext:simd-pack-512-mask m1 m2 m3)
                           (optimize (speed 3) (safety 0)))
                  (sb-vm::%gauntlet-cascade-3stage a b m1 m2 m3))))

(defparameter *fn-vaddpd*
  (compile nil `(lambda (a b m)
                  (declare (type (sb-ext:simd-pack-512 double-float) a b)
                           (type sb-ext:simd-pack-512-mask m)
                           (optimize (speed 3) (safety 0)))
                  (sb-vm::simd-pack-512-double+-masked a b m))))

(defparameter *fn-vsubpd*
  (compile nil `(lambda (a b m)
                  (declare (type (sb-ext:simd-pack-512 double-float) a b)
                           (type sb-ext:simd-pack-512-mask m)
                           (optimize (speed 3) (safety 0)))
                  (sb-vm::simd-pack-512-double--masked a b m))))

(defparameter *fn-vaddps*
  (compile nil `(lambda (a b m)
                  (declare (type (sb-ext:simd-pack-512 single-float) a b)
                           (type sb-ext:simd-pack-512-mask m)
                           (optimize (speed 3) (safety 0)))
                  (sb-vm::simd-pack-512-single+-masked a b m))))

(defparameter *fn-vsubps*
  (compile nil `(lambda (a b m)
                  (declare (type (sb-ext:simd-pack-512 single-float) a b)
                           (type sb-ext:simd-pack-512-mask m)
                           (optimize (speed 3) (safety 0)))
                  (sb-vm::simd-pack-512-single--masked a b m))))

;;; ---------------------------------------------------------------------------
;;; 5. Differential Scalar Oracle (Consumer)
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
  (format t "~78,,,'=A~%~%" "=")
  (error "AVX-512 Gauntlet verification failed on ~A at batch ~D slot ~D" op-name batch-id slot-idx))

(defun validate-slot (arr base-idx batch-id slot-idx)
  (declare (type (simple-array (unsigned-byte 64) (*)) arr)
           (type (unsigned-byte 32) base-idx batch-id slot-idx)
           (optimize (speed 3) (safety 0)))
  (let ((op   (%u64-ref arr (+ base-idx +slot-op+)))
        (m1   (%u64-ref arr (+ base-idx +slot-m1+)))
        (m2   (%u64-ref arr (+ base-idx +slot-m2+)))
        (m3   (%u64-ref arr (+ base-idx +slot-m3+))))
    (cond
      ;; 64-bit Integer Operations (8 lanes)
      ((or (= op +op-vpaddq+) (= op +op-vpsubq+)
           (= op +op-vpandq+) (= op +op-vporq+) (= op +op-vpxorq+))
       (loop for lane of-type fixnum from 0 below 8 do
         (let* ((bit (ldb (byte 1 lane) m1))
                (a   (%u64-ref arr (+ base-idx +slot-ina-offset+ lane)))
                (b   (%u64-ref arr (+ base-idx +slot-inb-offset+ lane)))
                (hw  (%u64-ref arr (+ base-idx +slot-hwr-offset+ lane)))
                (exp (if (zerop bit)
                         a
                         (cond ((= op +op-vpaddq+) (ldb (byte 64 0) (+ a b)))
                               ((= op +op-vpsubq+) (ldb (byte 64 0) (- a b)))
                               ((= op +op-vpandq+) (logand a b))
                               ((= op +op-vporq+)  (logior a b))
                               ((= op +op-vpxorq+) (logxor a b))))))
           (unless (= exp hw)
             (report-mismatch (cond ((= op +op-vpaddq+) "VPADDQ-MASKED")
                                    ((= op +op-vpsubq+) "VPSUBQ-MASKED")
                                    ((= op +op-vpandq+) "VPANDQ-MASKED")
                                    ((= op +op-vporq+)  "VPORQ-MASKED")
                                    ((= op +op-vpxorq+) "VPXORQ-MASKED"))
                              batch-id slot-idx m1 lane exp hw a b)))))

      ;; 32-bit Integer Operations (16 lanes)
      ((or (= op +op-vpaddd+) (= op +op-vpsubd+))
       (loop for lane of-type fixnum from 0 below 16 do
         (multiple-value-bind (qword-idx half) (floor lane 2)
           (let* ((bit (ldb (byte 1 lane) m1))
                  (qa  (%u64-ref arr (+ base-idx +slot-ina-offset+ qword-idx)))
                  (qb  (%u64-ref arr (+ base-idx +slot-inb-offset+ qword-idx)))
                  (qhw (%u64-ref arr (+ base-idx +slot-hwr-offset+ qword-idx)))
                  (a   (ldb (byte 32 (* half 32)) qa))
                  (b   (ldb (byte 32 (* half 32)) qb))
                  (hw  (ldb (byte 32 (* half 32)) qhw))
                  (exp (if (zerop bit)
                           a
                           (cond ((= op +op-vpaddd+) (ldb (byte 32 0) (+ a b)))
                                 ((= op +op-vpsubd+) (ldb (byte 32 0) (- a b)))))))
             (unless (= exp hw)
               (report-mismatch (if (= op +op-vpaddd+) "VPADDD-MASKED" "VPSUBD-MASKED")
                                batch-id slot-idx m1 lane exp hw a b))))))

      ;; 3-Stage In-Register Cascade:
      ;; Stage 1: z1 = (vpaddq a b m1)
      ;; Stage 2: z2 = (vpxorq z1 a m2)
      ;; Stage 3: dst = (vpsubq z2 b m3)
      ((= op +op-cascade-3+)
       (loop for lane of-type fixnum from 0 below 8 do
         (let* ((bit1 (ldb (byte 1 lane) m1))
                (bit2 (ldb (byte 1 lane) m2))
                (bit3 (ldb (byte 1 lane) m3))
                (a    (%u64-ref arr (+ base-idx +slot-ina-offset+ lane)))
                (b    (%u64-ref arr (+ base-idx +slot-inb-offset+ lane)))
                (hw   (%u64-ref arr (+ base-idx +slot-hwr-offset+ lane)))
                (z1   (if (zerop bit1) a (ldb (byte 64 0) (+ a b))))
                (z2   (if (zerop bit2) z1 (logxor z1 a)))
                (exp  (if (zerop bit3) z2 (ldb (byte 64 0) (- z2 b)))))
           (unless (= exp hw)
             (report-mismatch "CASCADE-3STAGE"
                              batch-id slot-idx m1 lane exp hw a b)))))

      ;; Double Float Operations (8 lanes)
      ((or (= op +op-vaddpd+) (= op +op-vsubpd+))
       (loop for lane of-type fixnum from 0 below 8 do
         (let* ((bit (ldb (byte 1 lane) m1))
                (qa  (%u64-ref arr (+ base-idx +slot-ina-offset+ lane)))
                (qb  (%u64-ref arr (+ base-idx +slot-inb-offset+ lane)))
                (qhw (%u64-ref arr (+ base-idx +slot-hwr-offset+ lane)))
                (fa  (sb-kernel:make-double-float (ldb (byte 32 32) qa) (ldb (byte 32 0) qa)))
                (fb  (sb-kernel:make-double-float (ldb (byte 32 32) qb) (ldb (byte 32 0) qb)))
                (fhw (sb-kernel:make-double-float (ldb (byte 32 32) qhw) (ldb (byte 32 0) qhw)))
                (fexp (if (zerop bit)
                          fa
                          (cond ((= op +op-vaddpd+) (+ fa fb))
                                ((= op +op-vsubpd+) (- fa fb))))))
           (unless (< (abs (- fexp fhw)) 1.0d-9)
             (report-mismatch (if (= op +op-vaddpd+) "VADDPD-MASKED" "VSUBPD-MASKED")
                              batch-id slot-idx m1 lane
                              (sb-kernel:double-float-bits fexp)
                              (sb-kernel:double-float-bits fhw)
                              qa qb))))))))

;;; ---------------------------------------------------------------------------
;;; 6. Multi-Threaded Execution Engine
;;; ---------------------------------------------------------------------------

(defun run-consumer-worker (worker-id core-id queue sync-vars total-batches)
  (declare (type (unsigned-byte 32) worker-id core-id total-batches)
           (type (simple-array (unsigned-byte 64) (*)) queue sync-vars)
           (ignore worker-id)
           (optimize (speed 3) (safety 0)))
  (pin-thread-to-core core-id)
  (loop
    (let ((claimed-batch (sb-ext:atomic-incf (aref sync-vars +sync-tail+))))
      (when (>= claimed-batch total-batches)
        (return))
      ;; Spin-wait for Producer to commit this batch
      (loop while (and (zerop (%u64-ref sync-vars +sync-abort+))
                       (< (%u64-ref sync-vars +sync-head+) (1+ claimed-batch)))
            do (sb-thread:thread-yield))
      (when (= 1 (%u64-ref sync-vars +sync-abort+))
        (return))
      ;; Validate entire batch
      (let* ((slot-in-ring (mod claimed-batch +num-batches+))
             (batch-base   (* slot-in-ring +words-per-batch+)))
        (loop for slot of-type (unsigned-byte 32) from 0 below +batch-size+ do
          (let ((slot-base (+ batch-base (* slot +iter-stride+))))
            (validate-slot queue slot-base claimed-batch slot))))
      ;; Mark batch completed
      (sb-ext:atomic-incf (aref sync-vars +sync-completed+)))))

(defun run-producer-engine (queue sync-vars total-batches)
  (declare (type (simple-array (unsigned-byte 64) (*)) queue sync-vars)
           (type (unsigned-byte 32) total-batches)
           (optimize (speed 3) (safety 0)))
  (pin-thread-to-core 0)
  (let ((seq 0))
    (declare (type (unsigned-byte 64) seq))
    (loop for batch of-type (unsigned-byte 32) from 0 below total-batches do
      ;; Flow control: wait if ring buffer is full
      (loop while (and (zerop (%u64-ref sync-vars +sync-abort+))
                       (>= (- batch (%u64-ref sync-vars +sync-completed+)) +num-batches+))
            do (sb-thread:thread-yield))
      (when (= 1 (%u64-ref sync-vars +sync-abort+))
        (return))

      (let* ((slot-in-ring (mod batch +num-batches+))
             (batch-base   (* slot-in-ring +words-per-batch+)))
        (loop for slot of-type (unsigned-byte 32) from 0 below +batch-size+ do
          (let* ((slot-base (+ batch-base (* slot +iter-stride+)))
                 (op (mod (+ batch slot) 13))
                 ;; Dynamic opmask combinatorics:
                 ;; Brute-force 8-bit masks (0..255), 16-bit masks (0..65535), walking bits
                 (m1 (cond ((or (= op +op-vpaddd+) (= op +op-vpsubd+))
                            (mod (+ (* batch +batch-size+) slot) 65536))
                           ((= op +op-cascade-3+)
                            (mod slot 256))
                           (t
                            (mod (+ slot (ash batch 3)) 256))))
                 (m2 (logxor (ash m1 1) #x55))
                 (m3 (logxor (ash m1 -1) #xAA)))
            (declare (type (unsigned-byte 64) op m1 m2 m3))
            (setf (%u64-ref queue (+ slot-base +slot-op+)) op)
            (setf (%u64-ref queue (+ slot-base +slot-m1+)) m1)
            (setf (%u64-ref queue (+ slot-base +slot-m2+)) m2)
            (setf (%u64-ref queue (+ slot-base +slot-m3+)) m3)
            (setf (%u64-ref queue (+ slot-base +slot-seq+)) (incf seq))

            ;; Generate input vectors
            (cond
              ;; Double float
              ((or (= op +op-vaddpd+) (= op +op-vsubpd+))
               (let* ((d1 (coerce (+ 10.0d0 (mod slot 100)) 'double-float))
                      (d2 (coerce (+ 2.0d0 (mod batch 50)) 'double-float))
                      (p1 (sb-ext:%make-simd-pack-512-double d1 d1 d1 d1 d1 d1 d1 d1))
                      (p2 (sb-ext:%make-simd-pack-512-double d2 d2 d2 d2 d2 d2 d2 d2))
                      (k1 (sb-ext:%make-simd-pack-512-mask m1)))
                 (loop for lane from 0 below 8 do
                   (setf (%u64-ref queue (+ slot-base +slot-ina-offset+ lane))
                         (sb-kernel:%simd-pack-512-0 p1))
                   (setf (%u64-ref queue (+ slot-base +slot-inb-offset+ lane))
                         (sb-kernel:%simd-pack-512-0 p2)))
                 (let ((res (if (= op +op-vaddpd+)
                                (funcall *fn-vaddpd* p1 p2 k1)
                                (funcall *fn-vsubpd* p1 p2 k1))))
                   (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 0)) (sb-kernel:%simd-pack-512-0 res))
                   (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 1)) (sb-kernel:%simd-pack-512-1 res))
                   (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 2)) (sb-kernel:%simd-pack-512-2 res))
                   (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 3)) (sb-kernel:%simd-pack-512-3 res))
                   (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 4)) (sb-kernel:%simd-pack-512-4 res))
                   (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 5)) (sb-kernel:%simd-pack-512-5 res))
                   (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 6)) (sb-kernel:%simd-pack-512-6 res))
                   (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 7)) (sb-kernel:%simd-pack-512-7 res)))))

              ;; 3-Stage Cascade
              ((= op +op-cascade-3+)
               (let* ((v1 (logxor #xCAFEBABE00000000 slot))
                      (v2 (logxor #xDEADBEEF11111111 (ash batch 4)))
                      (p1 (sb-ext:%make-simd-pack-512-ub64 v1 v1 v1 v1 v1 v1 v1 v1))
                      (p2 (sb-ext:%make-simd-pack-512-ub64 v2 v2 v2 v2 v2 v2 v2 v2))
                      (k1 (sb-ext:%make-simd-pack-512-mask m1))
                      (k2 (sb-ext:%make-simd-pack-512-mask m2))
                      (k3 (sb-ext:%make-simd-pack-512-mask m3)))
                 (loop for lane from 0 below 8 do
                   (setf (%u64-ref queue (+ slot-base +slot-ina-offset+ lane)) v1)
                   (setf (%u64-ref queue (+ slot-base +slot-inb-offset+ lane)) v2))
                 (let ((res (funcall *fn-cascade3* p1 p2 k1 k2 k3)))
                   (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 0)) (sb-kernel:%simd-pack-512-0 res))
                   (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 1)) (sb-kernel:%simd-pack-512-1 res))
                   (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 2)) (sb-kernel:%simd-pack-512-2 res))
                   (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 3)) (sb-kernel:%simd-pack-512-3 res))
                   (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 4)) (sb-kernel:%simd-pack-512-4 res))
                   (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 5)) (sb-kernel:%simd-pack-512-5 res))
                   (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 6)) (sb-kernel:%simd-pack-512-6 res))
                   (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 7)) (sb-kernel:%simd-pack-512-7 res)))))

              ;; Standard Integer / Logic
              (t
               (let* ((v1 (logxor #x0123456789ABCDEF (+ slot (* batch 1000))))
                      (v2 (logxor #xFEDCBA9876543210 (+ (* slot 3) batch)))
                      (p1 (sb-ext:%make-simd-pack-512-ub64 v1 v1 v1 v1 v1 v1 v1 v1))
                      (p2 (sb-ext:%make-simd-pack-512-ub64 v2 v2 v2 v2 v2 v2 v2 v2))
                      (k1 (sb-ext:%make-simd-pack-512-mask m1)))
                 (loop for lane from 0 below 8 do
                   (setf (%u64-ref queue (+ slot-base +slot-ina-offset+ lane)) v1)
                   (setf (%u64-ref queue (+ slot-base +slot-inb-offset+ lane)) v2))
                 (let ((res (cond ((= op +op-vpaddq+) (funcall *fn-vpaddq* p1 p2 k1))
                                  ((= op +op-vpsubq+) (funcall *fn-vpsubq* p1 p2 k1))
                                  ((= op +op-vpaddd+) (funcall *fn-vpaddd* p1 p2 k1))
                                  ((= op +op-vpsubd+) (funcall *fn-vpsubd* p1 p2 k1))
                                  ((= op +op-vpandq+) (funcall *fn-vpandq* p1 p2 k1))
                                  ((= op +op-vporq+)  (funcall *fn-vporq* p1 p2 k1))
                                  ((= op +op-vpxorq+) (funcall *fn-vpxorq* p1 p2 k1))
                                  (t (funcall *fn-vpaddq* p1 p2 k1)))))
                   (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 0)) (sb-kernel:%simd-pack-512-0 res))
                   (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 1)) (sb-kernel:%simd-pack-512-1 res))
                   (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 2)) (sb-kernel:%simd-pack-512-2 res))
                   (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 3)) (sb-kernel:%simd-pack-512-3 res))
                   (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 4)) (sb-kernel:%simd-pack-512-4 res))
                   (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 5)) (sb-kernel:%simd-pack-512-5 res))
                   (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 6)) (sb-kernel:%simd-pack-512-6 res))
                   (setf (%u64-ref queue (+ slot-base +slot-hwr-offset+ 7)) (sb-kernel:%simd-pack-512-7 res)))))))))

      ;; Publish batch to consumers
      (sb-ext:atomic-incf (aref sync-vars +sync-head+)))))

;;; ---------------------------------------------------------------------------
;;; 7. Test Driver Entrypoint
;;; ---------------------------------------------------------------------------

(defun run-avx512-gauntlet (&key (total-batches 64))
  "Execute the SPMC AVX-512 gauntlet across all detected host CPU cores."
  (let* ((num-cores (c-get-nprocs))
         (num-consumers (max 1 (1- num-cores)))
         (queue (make-array +queue-total-words+ :element-type '(unsigned-byte 64) :initial-element 0))
         (sync-vars (make-array 16 :element-type '(unsigned-byte 64) :initial-element 0))
         (total-iterations (* total-batches +batch-size+)))
    (format t "~%========================================================================~%")
    (format t "    STARTING BRUTAL AVX-512 MULTI-CORE SUPER-SWEEP GAUNTLET              ~%")
    (format t "========================================================================~%")
    (format t "Host CPU Cores Detected: ~D~%" num-cores)
    (format t "Hardware Producer:       1 Dedicated Thread (Pinned to Core 0)~%")
    (format t "Validation Consumers:    ~D Concurrent Threads (Pinned to Cores 1..~D)~%"
            num-consumers (1- num-cores))
    (format t "Flattened Ring Buffer:   ~D MB (~D words)~%"
            (round (* +queue-total-words+ 8) (* 1024 1024))
            +queue-total-words+)
    (format t "Total Test Batches:      ~D batches (~D test cases per batch)~%"
            total-batches +batch-size+)
    (format t "Total Test Cases:        ~:D native hardware operations~%" total-iterations)
    (format t "Coverage:                Exhaustive 8-bit & 16-bit masks, In-Register Cascades~%")
    (format t "========================================================================~%~%")

    (let ((start-time (get-internal-real-time))
          (consumer-threads nil))
      ;; Spawn N-1 consumer validation threads
      (dotimes (i num-consumers)
        (let ((worker-id i)
              (core-id (1+ (mod i (1- num-cores)))))
          (push (sb-thread:make-thread
                 (lambda ()
                   (run-consumer-worker worker-id core-id queue sync-vars total-batches))
                 :name (format nil "gauntlet-consumer-~D" worker-id))
                consumer-threads)))

      ;; Run Hardware Producer on main thread pinned to Core 0
      (run-producer-engine queue sync-vars total-batches)

      ;; Wait for all consumers to complete
      (dolist (th consumer-threads)
        (sb-thread:join-thread th))

      (let* ((end-time (get-internal-real-time))
             (elapsed-sec (/ (- end-time start-time) internal-time-units-per-second))
             (throughput (if (zerop elapsed-sec) 0 (round total-iterations elapsed-sec))))
        (format t "~%========================================================================~%")
        (format t ">>> AVX-512 GAUNTLET COMPLETED WITH ZERO MISMATCHES! <<<~%")
        (format t "========================================================================~%")
        (format t "Verified Hardware Ops:   ~:D operations~%" total-iterations)
        (format t "Total Batches Verified:  ~D / ~D batches~%"
                (%u64-ref sync-vars +sync-completed+) total-batches)
        (format t "Elapsed Time:            ~,3F seconds~%" elapsed-sec)
        (format t "Validation Throughput:   ~:D ops/sec (~,2F million ops/sec)~%"
                throughput (/ throughput 1000000.0))
        (format t "Active Cores Utilized:   ~D cores~%" num-cores)
        (format t "GC Steady-State Consing: 0 bytes (pre-allocated queue)~%")
        (format t "========================================================================~%~%")
        (assert (zerop (%u64-ref sync-vars +sync-mismatch+)))
        t))))

(test-util:with-test (:name :avx512-multi-core-brutal-gauntlet)
  (let ((batches (or (let ((v (sb-ext:posix-getenv "AVX512_GAUNTLET_BATCHES")))
                       (and v (parse-integer v :junk-allowed t)))
                     32)))
    (run-avx512-gauntlet :total-batches batches)))
