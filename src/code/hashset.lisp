;;;; Robinhood-hashing weak hashset
;;;; based on https://cs.uwaterloo.ca/research/tr/1986/CS-86-14.pdf

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;;; See also https://dspace.mit.edu/bitstream/handle/1721.1/130693/1251799942-MIT.pdf
;;;; which has a concurrent implementation

(in-package "SB-IMPL")

(export '(make-hashset hashset-remove hashset-statistics))

(eval-when ()
  (pushnew :hashset-debug sb-xc:*features*))

(define-load-time-global *hashset-print-statistics* nil)

;;; TODO: teach GC about these hashsets so that we can do something
;;; with address-sensitive (EQ and EQL) hashing.
;;; Then we can implement XSET in terms of this hashset.

(defstruct (robinhood-hashset-storage
            (:constructor !make-hs-storage (cells psl-vector hash-vector hv-inexact))
            (:conc-name hss-)
            (:copier nil)
            (:predicate nil))
  (cells #() :type (or simple-vector weak-vector) :read-only t)
  ;; We always store a hash-vector even if inexact (see below)
  ;; so that we can avoid calling the comparator on definite mismatches.
  (hash-vector (make-array 0 :element-type '(unsigned-byte 16))
              :type (simple-array (unsigned-byte 16) (*))
              :read-only t)
  ;; Inexact hashes occur when the size of the hash vector exceeds 2^16
  ;; in which case the stored hash has lost precision in terms of how
  ;; many storage bins there are. So we need to call the hash function
  ;; when moving keys around during insert, and when rehashing.
  (hv-inexact nil :type boolean :read-only t)
  (psl-vector (make-array 0 :element-type '(unsigned-byte 8))
              :type (simple-array (unsigned-byte 8) (*))
              :read-only t))

(defstruct (robinhood-hashset (:conc-name hashset-))
  ;; STORAGE can be swapped atomically so that readers can threadsafely read
  ;; all the relevant vectors even if there is a writer.
  ;; It _doesn't_ mean that FIND returns the right answer if writes occur while reading.
  ;; It _does_ mean that the algorithm won't crash.
  (storage (missing-arg) :type robinhood-hashset-storage)
  ;; need to allow maximum number of bits in either host or target fixnum
  ;; because the code runs on either. Will be efficient in the target at least.
  (hash-function #'error :type (sfunction (t) (or fixnum sb-xc:fixnum)))
  (test-function #'error :type function)
  #+hashset-metrics (count-find-hits 0 :type sb-vm:word)
  #+hashset-metrics (count-finds 0 :type sb-vm:word)
  (mutex nil :type (or null sb-thread:mutex)))

(defmacro hs-cells-len (v)
  #-weak-vector-readbarrier `(length ,v)
  #+weak-vector-readbarrier
  `(let ((v (truly-the (or simple-vector weak-vector) ,v)))
     (if (simple-vector-p v) (length v) (weak-vector-len v))))

(defun (setf hs-cell-ref) (newval cells index)
  #-weak-vector-readbarrier (setf (svref cells index) newval)
  ;; Even though this access could be punned (because the effective address
  ;; of weak-pointer + displacement or vector + displacement is the same)
  ;; that would be wrong because the store barriers are different.
  #+weak-vector-readbarrier
  (if (simple-vector-p cells)
      (setf (svref cells index) newval)
      (setf (weak-vector-ref cells index) newval)))

(declaim (inline hs-cell-ref))
(defun hs-cell-ref (v i)
  #-weak-vector-readbarrier (svref v i)
  ;; As above, there is a read barrier needed for access to weak objects
  ;; but not for simple-vector. Would it be both correct and faster
  ;; to always _assume_ the vector is weak? I don't think so.
  #+weak-vector-readbarrier
  (let ((v (truly-the (or simple-vector weak-vector) v)))
     (if (simple-vector-p v) (svref v i) (weak-vector-ref v i))))

;;; The last few elements in the cell vector are metadata.
(defconstant hs-storage-trailer-cells 3)
(defmacro hs-cells-capacity (v)
  `(truly-the index (- (hs-cells-len ,v) ,hs-storage-trailer-cells)))
(defmacro hs-cells-mask (v)
  `(truly-the index (- (hs-cells-len ,v) ,(1+ hs-storage-trailer-cells))))
(defmacro hs-cells-gc-epoch (v)
  `(hs-cell-ref ,v (- (hs-cells-len ,v) 3)))
;;; max probe sequence length for these cells
;;; TODO: if #+weak-vector-readbarrier add an accessor which bypasses
;;; the barrier given that the indexed element is a non-pointer.
(defmacro hs-cells-max-psl (v)
  `(truly-the fixnum (hs-cell-ref ,v (- (hs-cells-len ,v) 2))))
(defmacro hs-cells-n-avail (v)
  `(truly-the fixnum (hs-cell-ref ,v (- (hs-cells-len ,v) 1))))

(defun hashset-cells-load-factor (cells)
  (let* ((cap (hs-cells-capacity cells))
         (used (- cap (hs-cells-n-avail cells))))
    (/ used cap)))
(defconstant +hashset-unused-cell+ 0)
(defmacro hs-chain-terminator-p (val) `(eq ,val 0))

(defun allocate-hashset-storage (capacity weakp)
  (declare (type (unsigned-byte 28) capacity)) ; 256M cells maximum
  (declare (ignorable weakp))
  (declare (sb-c::tlab :system) (inline !make-hs-storage))
  (let* ((len (+ capacity hs-storage-trailer-cells))
         ;; This *MUST* use allocate-weak-vector and not MAKE-WEAK-VECTOR.
         ;; The latter can not be open-coded and would allocate to an arena
         ;; if so chosen by the the current thread, thereby ignoring
         ;; the SB-C::TLAB declaration above. ALLOCATE-WEAK-VECTOR
         ;; will respect the local declaration.
         (cells (cond (weakp (sb-c::allocate-weak-vector len))
                      (t (make-array len :initial-element 0))))
         (psl-vector (make-array capacity :element-type '(unsigned-byte 8)
                                          :initial-element 0))
         (hash-vector (make-array capacity :element-type '(unsigned-byte 16))))
    (setf (hs-cells-gc-epoch cells) sb-kernel::*gc-epoch*)
    (setf (hs-cells-max-psl cells) 0)
    (setf (hs-cells-n-avail cells) capacity)
    ;; Capacity 65536 is the max for which stored hashes can represent all indices
    ;; into the cell vector. Beyond that, the hashes don't have the required precision.
    ;; I might instead want the hash-vector's type to
    ;;   (OR (SIMPLE-ARRAY (UNSIGNED-BYTE 16) (*)) (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (*)))
    ;; but I'm not going there yet.
    (!make-hs-storage cells psl-vector hash-vector (> capacity 65536))))

(defun hashset-weakp (hashset)
  (weak-vector-p (hss-cells (hashset-storage hashset))))

;;; The hash-function provided to this constructor has to be reasonably strong.
;;; You could probably get away with SXHASH for instance types since we have stable
;;; addressed-based hashing on those. However, if the intent is to lookup keys
;;; using a content-based hash, then it's not useful to rely on SXHASH. When in doubt
;;; as to whether your hash is strong enough, call MURMUR3-FMIX-WORD somewhere in it.
(defun make-hashset (estimated-count test-function hash-function &key weakness synchronized)
  (declare (boolean weakness synchronized))
  (let* ((capacity (power-of-two-ceiling (* estimated-count 2)))
         (storage (allocate-hashset-storage capacity weakness)))
    (make-robinhood-hashset :hash-function hash-function
                            :test-function test-function
                            :mutex (if synchronized (sb-thread:make-mutex :name "hashset"))
                            :storage storage)))

;;; The following terms are synonyms: "probe sequence position", "probe sequence index".
;;; Probe sequence length is the maximum index attained for a given probe sequence.
;;; (Note that probe sequence indices are 1-origin, not 0)
;;; The table's overall max-PSL is the maximum of any PSL for any key in the table.
(defmacro hs-aver-probe-sequence-len (storage index length)
  (declare (ignorable storage index length))
  ;; This assertion is not threadsafe, because an inserter could cause the
  ;; value in the hash-vector to change by backshifting while we're reading.
  ;; Readers are otherwise threadsafe. So this is only for debugging.
  #+nil `(aver (= (aref (hss-psl-vector ,storage) ,index) ,length))
  )

;;; This definition is probably off-by-1 for what you think of as the triangular numbers,
;;; but it does what I want for the hashset probing algorithm:
;;;   f(1)=0, f(2)=1, f(3)=3, f(4)=6, etc
;;; So the first probe occurs at index+0, then "skip 1", "skip 2", "skip 3", ...
(declaim (inline triangular-number))
(defun triangular-number (n)
  (declare (fixnum n))
  (/ (* n (1- n)) 2))

#+nil
(defun hss-backshift (hashset storage current-index desired-psp key hash)
  ;; If we can grab the mutex, then shift this item backwards in its probe sequence.
  ;; If mutex is already owned, or if after acquiring the mutex a change is detected,
  ;; just skip this. If no mutex then back-shift is always OK.
  (let* ((cells (hss-cells storage))
         (desired-index (logand (+ hash (triangular-number desired-psp))
                                (hs-cells-mask cells)))
         (mutex (hashset-mutex hashset)))
    ;; this should probably be WITH-SYSTEM-MUTEX :WAIT NIL
    ;; but it's hard to structure this exceptional case that way
    (when (or (not mutex) (sb-thread::%try-mutex mutex))
      (when (and (eq (hashset-storage hashset) storage)
                 (eq (aref cells current-index) key) ; key is still here
                 (null (aref cells desired-index))) ; tombstone is still there
        #+nil
        (format t "~&Hashset moved ~S from psp ~D to ~D~%"
                key (aref (hss-psl-vector storage) current-index) desired-psp)
        ;; Put this item into the tombstone's location and make this a tombstone
        ;; and *not* +hashset-unused-cell+. Any other probing sequence for a different key
        ;; might have previously tried to claim this cell and could not, so
        ;; went to a later cell in its sequence. Therefore we can't interrupt that
        ;; other (unknown) sequence of probes with a chain-terminating marker.
        ;; Change the key's stored hash to indicate its new, shorter, probe sequence length
        (setf (aref (hss-hash-vector storage) desired-index) (ldb (byte 16 0) hash)
              (aref (hss-psl-vector storage) desired-index) desired-psp
              (aref cells desired-index) key
              (aref cells current-index) nil))
      ;; (hashset-check-invariants hashset)
      (when mutex (sb-thread:release-mutex mutex)))))

;;; Algorithm from Figure 2.1 of the paper.
;;; TODO: there's a simple optimization to avoid calling TRIANGULAR-NUMBER
;;; but I want (for the time being) to be a little closer to the structure
;;; of the reference algorithm, though already this deviates somewhat
;;; substantially by:
;;; - not abstracting out the family of hash functions
;;; - not depending on a 'findposition'
;;; - storing hashes explicitly
(defun hashset-%insert (hashset storage key hash)
  (flet ((triang (n)
           (triangular-number (the (unsigned-byte 8) n))))
    (declare (inline triang))
    (let* ((probe-sequence-pos 0) ; called 'probeposition' in the paper
           (cells (hss-cells storage))
           (max-psl (hs-cells-max-psl cells))
           (psl-vector (hss-psl-vector storage))
           (hash-vector (hss-hash-vector storage))
           (mask (hs-cells-mask cells))
           (original-key key))
      (loop
        (incf probe-sequence-pos)
        (let* ((location (logand (+ hash (triang probe-sequence-pos)) mask))
               (probed-key (hs-cell-ref cells location))
               ;; Get the position in its probe sequence of the key (if any)
               ;; in the slot we're considering taking.
               (probed-key-psp ; named 'recordposition' in the paper
                ;; GC-smashed cells might have a nonzero value in the psl-vector
                ;; which must be disregarded.
                (if (null probed-key) 0 (aref psl-vector location))))
          (when (> probe-sequence-pos probed-key-psp) ; KEY should get the slot
            ;; Get the hash for the key we're stomping on (if any)
            ;; before storing our hash
            (let ((probed-key-hash (aref hash-vector location)))
              (setf (aref hash-vector location) (ldb (byte 16 0) hash)
                    (aref psl-vector location) probe-sequence-pos
                    (hs-cell-ref cells location) key)
              #+hashset-debug
              (when (> probe-sequence-pos max-psl)
                (format *error-output* "hashset-insert(~x): max_psl was ~D is ~D, LF=~F~%"
                        (get-lisp-obj-address cells)
                        max-psl probe-sequence-pos (hashset-cells-load-factor cells)))
              (setf max-psl (max max-psl probe-sequence-pos))
              (when (or (null probed-key) (hs-chain-terminator-p probed-key)) (return))
              (setq probe-sequence-pos probed-key-psp
                    key probed-key
                    hash (if (hss-hv-inexact storage)
                             (funcall (hashset-hash-function hashset) key)
                             probed-key-hash))))))
      (setf (hs-cells-max-psl cells) max-psl)
      original-key)))

(defun hashset-statistics (storage &aux (cells (hss-cells storage))
                                        (psl-vector (hss-psl-vector storage)))
  (flet ((validp (x) (and (not (hs-chain-terminator-p x)) x)))
    (declare (inline validp))
    (do ((i (hs-cells-mask cells) (1- i))
         (histo (make-array (hs-cells-max-psl cells) :initial-element 0))
         (sum-psl 0) ; sum of probe sequence lengths
         (n-keys 0))
        ((< i 0)
         (values (if (plusp n-keys) (/ (float sum-psl) n-keys)) ; avg PSL
                 histo
                 (/ (float n-keys) (hs-cells-capacity cells)))) ; load factor
      (let ((x (hs-cell-ref cells i)))
        (when (validp x)
          (let ((psl (aref psl-vector i)))
            (incf (aref histo (1- psl)))
            (incf sum-psl psl)
            (incf n-keys)))))))

;;; Return count of occupied cells, NILs, 0s, and unbound markers.
;;; [Sidebar: I may need to change the tombstone and/or unassigned value.
;;; Currently these hashsets can't represent 0 or NIL as a key per se]
(defun hs-cells-occupancy (cells limit)
  (declare (type (or simple-vector weak-vector) cells)
           (type index limit))
  (let ((count-live 0)
        (count-nil 0) ; GC smashes to NIL, so these are tombstones
        (count-0 0) ; cells are 0-initialized, so this indicates never used
        (count-ubm 0))
    (declare (fixnum count-live count-0 count-NIL count-ubm))
    (dotimes (i limit)
      (let ((val (hs-cell-ref cells i)))
        (cond ((eql val nil) (incf count-nil))
              ((eql val 0) (incf count-0))
              ((unbound-marker-p val) (incf count-ubm))
              (t (incf count-live)))))
    (values count-live count-nil count-0 count-ubm)))

(defun hashset-rehash (hashset count)
  (declare (type (or index null) count))
  ;; (hashset-check-invariants hashset "begin rehash")
  (flet ((validp (x) (and (not (hs-chain-terminator-p x)) x)))
    (declare (inline validp))
    (let* ((old-storage (hashset-storage hashset))
           (old-cells (hss-cells old-storage))
           (old-capacity (hs-cells-capacity old-cells))
           (count (or count ; count is already known if just GC'ed
                      (hs-cells-occupancy old-cells old-capacity)))
           (new-capacity (max 64 (power-of-two-ceiling (* count 2))))
           (new-storage
            (allocate-hashset-storage new-capacity (hashset-weakp hashset))))

      ;; This can be removed
      (when *hashset-print-statistics*
        (multiple-value-bind (mean-psl histo) (hashset-statistics old-storage)
          (let ((*print-length* nil)
                (*print-pretty* nil))
            (format *error-output* "~&rehash: size=(~D -> ~D), ~D avg=~f~%"
                    old-capacity new-capacity histo mean-psl))))

      (do ((i (1- old-capacity) (1- i))
           (n-inserted 0)
           (old-hash-vector (hss-hash-vector old-storage)))
          ((< i 0)
           (decf (hs-cells-n-avail (hss-cells new-storage)) n-inserted))
        (declare (type index-or-minus-1 i) (fixnum n-inserted))
        (let ((key (hs-cell-ref old-cells i)))
          (when (validp key)
            (incf n-inserted)
            ;; Test whether 16-bit hashes are good for the _new_ storage, not the old.
            (hashset-%insert hashset new-storage key
                             (if (hss-hv-inexact new-storage)
                                 (funcall (hashset-hash-function hashset) key)
                                 (aref old-hash-vector i))))))
      ;; Assign the new vectors
      (setf (hashset-storage hashset) new-storage)
      (when (simple-vector-p old-cells)
        ;; Zap the old key vector
        (fill old-cells 0)
        ;; old vector becomes non-weak, eliminating some GC overhead
        #-sb-xc-host (assign-vector-flags old-cells 0))
      ;; (hashset-check-invariants hashset "end rehash")
      new-storage)))

(defun hashset-insert (hashset key)
  (let* ((storage (hashset-storage hashset))
         (cells (hss-cells storage))
         (capacity (hs-cells-capacity cells))
         (min-avail (ash capacity -2)))
    (cond ((hashset-weakp hashset)
           (flet ((validp (x) (and (not (hs-chain-terminator-p x)) x)))
             (declare (inline validp))
             (let ((current-epoch sb-kernel::*gc-epoch*)
                   (n-live))
               ;; First decide if the table occupancy needs to be recomputed after GC
               (unless (eq (hs-cells-gc-epoch cells) current-epoch)
                 (setf n-live (hs-cells-occupancy cells capacity)
                       (hs-cells-n-avail cells) (- capacity n-live)
                       (hs-cells-gc-epoch cells) current-epoch))
               ;; Next decide if rehash should occur (LF exceeds 75%)
               ;; TODO: also do the rehash if 50% of cells are NULL
               (when (< (hs-cells-n-avail cells) min-avail)
                 ;; No big deal if GC culled some more after the counting-
                 ;; REHASH will only copy valid items.
                 (setf storage (hashset-rehash hashset n-live)
                       (hs-cells-gc-epoch (hss-cells storage))
                       current-epoch)))))
          (t
           ;; Just look at the occupancy, which has to be accurate
           (let ((n-avail (hs-cells-n-avail cells)))
             (when (< n-avail min-avail)
               (setf storage (hashset-rehash hashset (- capacity n-avail)))))))
    ;; Finally, insert
    (decf (hs-cells-n-avail cells))
    (hashset-%insert hashset storage key (funcall (hashset-hash-function hashset) key))))

;;; This is the standard open-addressing algorithm using triangular numbers for successive
;;; probes, with early termination based on the observed maximum probe sequence length
;;; as maintained by the insertion algorithm.
(defun hashset-find (hashset key)
  #-sb-xc-host (declare (optimize (sb-c::insert-array-bounds-checks 0)))
  (let* ((storage (hashset-storage hashset))
         (cells (hss-cells storage))
         (hash (funcall (hashset-hash-function hashset) key))
         (mask (hs-cells-mask cells))
         (hash-vector (hss-hash-vector storage))
         (test (hashset-test-function hashset))
         (max-psl-1 (1- (hs-cells-max-psl cells)))
         ;; Filtering on LOWTAG rejects the unused cell marker as well as NIL
         ;; stuffed in by GC, except for keys which are lists. It is assumed
         ;; that the comparator can accept NIL if it accepts lists.
         (lowtag (lowtag-of key))
         (clipped-hash (ldb (byte 16 0) hash))
         (index (logand hash mask))
         (iteration 1))
    (declare (fixnum iteration))
    #+hashset-metrics (incf (hashset-count-finds hashset))
    ;; Unroll by always fetching a pair of keys and hashes.
    ;; Theory suggests that first probing the 2nd choice location should perform better
    ;; than probing the 1st choice first, because the probability density function
    ;; for key K mapping to its Nth-choice probe-sequence-position is more highly
    ;; concentrated at 2 than 1. Despite that I have not observed that to be always true,
    ;; in the unrolled loop, this tactic is performed by checking K2 before K1.
    ;; (It's also not better for subsequent iterations, but it's good enough)
    (loop
      (let* ((next-index (logand (+ index iteration) mask))
             (k1 (hs-cell-ref cells index))
             (k2 (hs-cell-ref cells next-index))
             (h1 (aref hash-vector index))
             (h2 (aref hash-vector next-index)))
        (when (and (= (lowtag-of k2) lowtag) (= h2 clipped-hash) (funcall test k2 key))
          (hs-aver-probe-sequence-len storage next-index (1+ iteration))
          #+hashset-metrics (incf (hashset-count-find-hits hashset))
          (return k2))
        (when (and (= (lowtag-of k1) lowtag) (= h1 clipped-hash) (funcall test k1 key))
          (hs-aver-probe-sequence-len storage index iteration)
          #+hashset-metrics (incf (hashset-count-find-hits hashset))
          (return k1))
        (when (or (hs-chain-terminator-p k1)
                  (hs-chain-terminator-p k2)
                  ;; We've tested through ITERATION+1. If that is >= MAX-PSL we're done.
                  ;; That's the same as checking ITERATION >= (1- MAX-PSL)
                  (>= iteration max-psl-1))
          (return nil))
        ;; this visits every cell.
        ;; Proof at https://fgiesen.wordpress.com/2015/02/22/triangular-numbers-mod-2n/
        (setq index (logand (+ next-index iteration 1) mask))
        (incf (truly-the fixnum iteration) 2)))))

;;; This is basically FIND, storing NIL in the cell if found.
;;; Caller is responsible for guarding with the hashset-mutex if applicable.
;;; Return T if KEY was present, NIL otherwise.
(defun hashset-remove (hashset key &aux (storage (hashset-storage hashset))
                                        (cells (hss-cells storage))
                                        (test (hashset-test-function hashset)))
  #-sb-xc-host (declare (optimize (sb-c::insert-array-bounds-checks 0)))
  (let* ((mask (hs-cells-mask cells))
         (index (logand (funcall (hashset-hash-function hashset) key) mask))
         (max-psl (hs-cells-max-psl cells))
         (iteration 1))
    (declare (fixnum iteration))
    (loop
      (let ((probed-value (hs-cell-ref cells index)))
        (when (hs-chain-terminator-p probed-value) ; end of probe sequence
          (return nil))
        (when (and probed-value (funcall test probed-value key))
          (hs-aver-probe-sequence-len storage index iteration)
          (setf (hs-cell-ref cells index) nil) ; It's that simple
          (return t))
        (if (>= iteration max-psl) (return nil))
        (setq index (logand (+ index iteration) mask))
        (incf iteration)))))

;;; Search for KEY in HASHSET and if found return the matching entry.
;;; If not found, call COPIER on KEY and insert that.
;;; This operation allows the supplied key to be dynamic-extent or possibly
;;; not in GC-managed memory.
;;; The hashset is single-reader safe without the mutex, but you might or might not
;;; get a hit even if KEY is logically present, because a concurrent INSERT is
;;; allowed to reorder the physical storage. So we rely on the double-check pattern.
(declaim (ftype (sfunction (robinhood-hashset t function) t) hashset-insert-if-absent))
(defun hashset-insert-if-absent (hashset key copier)
  (or (hashset-find hashset key)
      (if (not (hashset-mutex hashset))
          (hashset-insert hashset (funcall copier key))
          (with-system-mutex ((hashset-mutex hashset))
            (or (hashset-find hashset key)
                (hashset-insert hashset (funcall copier key)))))))

(defun map-hashset (function hashset)
  (declare (dynamic-extent function))
  (macrolet ((dovect ()
               `(let ((vec (hss-cells (hashset-storage hashset))))
                  (do ((index 0 (1+ index))
                       ;; FIXME: isn't this just HS-CELLS-CAPACITY ?
                       (length (- (hs-cells-len vec) hs-storage-trailer-cells)))
                      ((>= index length))
                    (let ((elt (hs-cell-ref vec index)))
                      (unless (or (null elt) (eql elt 0))
                        (funcall function elt)))))))
    (if (hashset-mutex hashset)
        (with-system-mutex ((hashset-mutex hashset)) (dovect))
        (dovect))))

(defun hashset-count (hashset &aux (cells (hss-cells (hashset-storage hashset))))
  (let ((n 0))
    (dotimes (i (hs-cells-capacity cells) n)
      (let ((x (hs-cell-ref cells i)))
        (when (and x (not (hs-chain-terminator-p x)))
          (incf n))))))
(defmethod print-object ((self robinhood-hashset) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (let ((cells (hss-cells (hashset-storage self))))
      (format stream "~S ~D/~D keys, psl=~D"
              (%fun-name (hashset-test-function self))
              (hashset-count self)
              (hs-cells-capacity cells)
              (hs-cells-max-psl cells)))))

#-sb-xc-host
(progn
(defun weak-hashset-linear-find/eq (key hashset)
  (let ((cells (hss-cells hashset)))
    (if (simple-vector-p cells)
        (find key cells :test #'eq)
        (weak-vector-find/eq key cells))))
(defun weak-vector-find/eq (key vector)
  (dotimes (i (weak-vector-len vector))
    (when (eq (weak-vector-ref vector i) key)
      (return key)))))
