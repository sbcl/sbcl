;;;; writer-preferring reader/writer locks

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMUCL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-THREAD")

(export '(make-rwlock rwlock-rdlock rwlock-wrlock rwlock-unlock
          make-rw-spinlock rwspinlock-rdlock rwspinlock-rdunlock
          rwspinlock-wrlock rwspinlock-wrunlock))

;;; This design is inspired by that of Bionic libc with adjustments for the fact that SBCL
;;; lacks 32-bit integer raw slots on 64-bit machines, and removal of timeouts
;;; and the choice of reader vs writer preference.
(defstruct (rwlock
            (:constructor make-rwlock ())
            (:copier nil)
            (:predicate nil))
  (mutex (sb-thread:make-mutex) :type sb-thread:mutex :read-only t)
  ;; State in high 4 bytes, writer thread ID in low 4 bytes.
  ;; Sign bit on means locked by writer, however access is done using SAP-REF-32,
  ;; and not SIGNED-SAP-32, so it reads as unsigned because I haven't implemented
  ;; (CAS signed-sap-ref-32) on any architecture other than x86-64.
  (state+writer 0 :type sb-vm:signed-word) ; *not* guarded by MUTEX
  ;; PENDING slots are guarded by MUTEX.
  ;; futex word in high bytes, count in low bytes
  (pending-writers 0 :type sb-vm:word)
  (pending-readers 0 :type sb-vm:word))

(declaim (sb-ext:freeze-type rwlock))

(eval-when (:compile-toplevel :execute)
  (defun rwl-offsetof (slot-name)
    (let ((slot (find slot-name (dd-slots (find-defstruct-description 'rwlock))
                      :key #'dsd-name)))
      (+ (ash (+ (dsd-index slot) sb-vm:instance-slots-offset) sb-vm:word-shift)
         (- sb-vm:instance-pointer-lowtag))))

  (defmacro rwlock-slot (l slot-name half)
    `(sap-ref-32 (int-sap (get-lisp-obj-address ,l))
                 ,(+ (rwl-offsetof slot-name) (ecase half (:low 0) (:high 4)))))

  (defmacro &rwlock-futex-word (l slot-name)
    `(sap-int (sap+ (int-sap (get-lisp-obj-address ,l))
                    ,(+ (rwl-offsetof slot-name) #+little-endian 4)))))

(defmacro rwlock-state (l) `(rwlock-slot ,l state+writer :high))
(defmacro rwlock-writer-tid (l) `(rwlock-slot ,l state+writer :low))
(defmacro rwlock-n-pending-readers (l) `(rwlock-slot ,l pending-readers :low))
(defmacro rwlock-n-pending-writers (l) `(rwlock-slot ,l pending-writers :low))
(defmacro rwlock-reader-wake-word (l) `(rwlock-slot ,l pending-readers :high))
(defmacro rwlock-writer-wake-word (l) `(rwlock-slot ,l pending-writers :high))
(defmacro &rwlock-reader-wake-word (l) `(&rwlock-futex-word ,l pending-readers))
(defmacro &rwlock-writer-wake-word (l) `(&rwlock-futex-word ,l pending-writers))

;; Stats bits 2 through 30 inclusive count the number of outstanding rdlock invocations.
;; State bits 0, 1, and 31 are flags.
(defconstant OWNED-BY-WRITER      (ash 1 31))
(defconstant PENDING-READERS-FLAG 1)
(defconstant PENDING-WRITERS-FLAG 2)
(defconstant READER-COUNT-SHIFT   2)

(declaim (inline rwl-locked-p rwl-owned-by-writer-p))
(declaim (inline can-acquire-for-read can-acquire-for-write))
(declaim (inline rwlock-tryrdlock rwlock-trywrlock))

(defun rwl-locked-p (state) (ldb-test (byte 30 2) state)) ; writer, or readers > 0
(defun rwl-owned-by-writer-p (state) (logbitp 31 state))

(defun can-acquire-for-read (state) ; if neither waited on by nor owned by a writer
  (not (logtest state (logior OWNED-BY-WRITER PENDING-WRITERS-FLAG))))

(defun can-acquire-for-write (state) (not (rwl-locked-p state)))

(defmacro atomic-op ((function place rhs))
  `(let ((.old. ,place))
     (loop (let* ((new (sb-ext:truly-the (unsigned-byte 32) (,function .old. ,rhs)))
                  (actual (sb-ext:cas ,place .old. new)))
             (if (= actual .old.) (return .old.) (setq .old. actual))))))

(defun rwlock-tryrdlock (lock)
  (do ((old (rwlock-state lock)))
      ((not (can-acquire-for-read old)))
    (let ((new (sb-ext:truly-the (unsigned-byte 32)
                                 (+ old (ash 1 READER-COUNT-SHIFT)))))
      (when (logbitp 31 new) ; overflow of the reader count
        (error "Too many readers"))
      (when (= old (setq old (sb-ext:cas (rwlock-state lock) old new)))
        (return t)))))

(defun %rwlock-rdlock (lock)
  (when (= (rwlock-writer-tid lock) (my-kernel-thread-id))
    (error "Can't acquire lock for read when locked for write"))
  (loop
    (when (rwlock-tryrdlock lock) (return t))
    (let ((old-state (rwlock-state lock))
          (old-serial 0))
      (unless (can-acquire-for-read old-state)
        (with-system-mutex ((rwlock-mutex lock))
          (incf (rwlock-n-pending-readers lock))
          (setq old-state (atomic-op (logior (rwlock-state lock) PENDING-READERS-FLAG))
                old-serial (rwlock-reader-wake-word lock)))
        (unless (can-acquire-for-read old-state)
          (futex-wait (&rwlock-reader-wake-word lock) old-serial -1 0))
        (with-system-mutex ((rwlock-mutex lock))
          (when (zerop (decf (rwlock-n-pending-readers lock)))
            (atomic-op (logand (rwlock-state lock) (lognot PENDING-READERS-FLAG)))))))))

(defun rwlock-rdlock (lock)
  (with-pinned-objects (lock)
    (or (rwlock-tryrdlock lock) (%rwlock-rdlock lock)))
  t)

(defun rwlock-trywrlock (lock)
  (do ((old (rwlock-state lock)))
      ((not (can-acquire-for-write old)))
    (when (= old (setq old (sb-ext:cas (rwlock-state lock) old (logior old OWNED-BY-WRITER))))
      (setf (rwlock-writer-tid lock) (my-kernel-thread-id))
      (return t))))

(defun %rwlock-wrlock (lock)
  (when (= (rwlock-writer-tid lock) (my-kernel-thread-id))
    (error "Can't recursively acquire write lock"))
  (loop
    (when (rwlock-trywrlock lock) (return t))
    (let ((old-state (rwlock-state lock))
          (old-serial 0))
      (unless (can-acquire-for-write old-state)
        (with-system-mutex ((rwlock-mutex lock))
          (incf (rwlock-n-pending-writers lock))
          (setq old-state (atomic-op (logior (rwlock-state lock) PENDING-WRITERS-FLAG))
                old-serial (rwlock-writer-wake-word lock)))
        (unless (can-acquire-for-write old-state)
          (futex-wait (&rwlock-writer-wake-word lock) old-serial -1 0))
        (with-system-mutex ((rwlock-mutex lock))
          (when (zerop (decf (rwlock-n-pending-writers lock)))
            (atomic-op (logand (rwlock-state lock) (lognot PENDING-WRITERS-FLAG)))))))))

(defun rwlock-wrlock (lock)
  (with-pinned-objects (lock)
    (or (rwlock-trywrlock lock) (%rwlock-wrlock lock)))
  t)

(defun rwlock-unlock (lock)
  (declare (type rwlock lock))
  (with-pinned-objects (lock)
    (let ((state (rwlock-state lock)))
      (cond ((rwl-owned-by-writer-p state)
             (aver (= (rwlock-writer-tid lock) (my-kernel-thread-id)))
             (setf (rwlock-writer-tid lock) 0)
             (setq state (atomic-op (logand (rwlock-state lock) (lognot OWNED-BY-WRITER)))))
            ((rwl-locked-p state)
             (setq state (atomic-op (- (rwlock-state lock) (ash 1 READER-COUNT-SHIFT))))
             (when (>= state (ash 2 READER-COUNT-SHIFT)) ; still locked by some reader
               (return-from rwlock-unlock)))
            (t (error "unlock without lock")))
      (when (logtest state #b11) ; at least 1 waiting writer or reader
        (macrolet ((add-mod-u32 (field)
                     `(setf (,field lock) (ldb (byte 32 0) (1+ (,field lock))))))
          (let ((wake (with-system-mutex ((rwlock-mutex lock))
                        (cond ((plusp (rwlock-n-pending-writers lock))
                               (add-mod-u32 rwlock-writer-wake-word)
                               PENDING-WRITERS-FLAG)
                              ((plusp (rwlock-n-pending-readers lock))
                               (add-mod-u32 rwlock-reader-wake-word)
                               PENDING-READERS-FLAG)))))
          (case wake
            (#.PENDING-READERS-FLAG
             (futex-wake (&rwlock-reader-wake-word lock) #x7fffffff))
            (#.PENDING-WRITERS-FLAG
             (futex-wake (&rwlock-writer-wake-word lock) 1))))))))
  t)

(defstruct (rw-spinlock (:constructor make-rw-spinlock))
  ;; Bits 0-31: Reader count, or #xFFFFFFFF if a writer holds the lock
  ;; Bits 32-63: Writers waiting to acquire the lock
  (state 0 :type sb-ext:word))

(defconstant +reader-mask+ #xFFFFFFFF)
(defconstant +writer-shift+ 32)

(defun rwspinlock-rdlock (lock)
  (declare (optimize (speed 3) (safety 0)))
  (loop
    (let ((state (rw-spinlock-state lock)))
      ;; The max reader count is capped to #xFFFFFFFE,
      ;; because exactly #xFFFFFFFF equals one writer.
      (cond ((< state (1- +reader-mask+))
             ;; No writer holds the lock, and no writers are waiting for the lock.
             (when (eql state (sb-ext:cas (rw-spinlock-state lock) state (1+ state)))
               (return t)))
            ((and (= (logand state +reader-mask+) (1- +reader-mask+))
                  (zerop (ash state (- +writer-shift+))))
             (bug "Too many readers"))))
   (thread-yield)))

(declaim (inline rwspinlock-rdunlock))
(defun rwspinlock-rdunlock (lock)
  (declare (optimize (speed 3) (safety 0)))
  (sb-ext:atomic-decf (rw-spinlock-state lock))
  t)

(defun rwspinlock-wrlock (lock)
  (declare (optimize (speed 3) (safety 0)))
  (symbol-macrolet ((writer-increment (ash 1 +writer-shift+)))
    ;; Step 1: Announce a new writer by incrementing the high half of the state bits.
    ;; After doing this, no additional readers can take the lock.
    (sb-ext:atomic-incf (rw-spinlock-state lock) writer-increment)
    ;; Step 2: Wait for 0 contenders and then try to take the lock as a writer
    (loop
     (let ((current (rw-spinlock-state lock)))
       (when (zerop (logand current +reader-mask+))
         ;; Try to set active reader count to #xFFFFFFFF which means 1 active writer,
         ;; and also decrement the waiting writer count.
         (let ((new-state (logior +reader-mask+ (- current writer-increment))))
           (when (eql current (sb-ext:cas (rw-spinlock-state lock) current new-state))
             (return t)))))
     (thread-yield))))

(declaim (inline rwspinlock-wrunlock))
(defun rwspinlock-wrunlock (lock)
  (declare (optimize (speed 3) (safety 0)))
  (let ((disp (ash (+ sb-vm:instance-slots-offset sb-vm:instance-data-start) sb-vm:word-shift)))
    ;; Clear the the low 32 state bits (assuming little-endian)
    (with-pinned-objects (lock)
      (setf (sap-ref-32 (int-sap (get-lisp-obj-address lock))
                        (- disp sb-vm:instance-pointer-lowtag))
            0))))
