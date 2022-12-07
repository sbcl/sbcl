
(import '(sb-impl::hashset-storage
          sb-impl::hashset-hash-function
          sb-impl::hashset-test-function
          sb-impl::hss-cells
          sb-impl::hss-hash-vector
          sb-impl::hss-psl-vector))

(defun hs-cells-mask (v) (- (length v) 4))
(defun hs-chain-terminator-p (x) (eq x 0))

(defun hashset-probing-sequence (hashset key)
  (let* ((storage (hashset-storage hashset))
         (cells (hss-cells storage))
         (mask (hs-cells-mask cells))
         (index (logand (funcall (hashset-hash-function hashset) key) mask))
         (interval 1)
         (sequence))
    (loop
     (push index sequence)
     (let ((probed-key (aref cells index)))
       (assert (not (hs-chain-terminator-p probed-key)))
       (when (and probed-key (funcall (hashset-test-function hashset) probed-key key))
         (return (nreverse sequence)))
       (setq index (logand (+ index interval) mask))
       (incf interval)))))

(defun hashset-check-invariants (hashset)
  (let* ((storage (hashset-storage hashset))
         (cells (hss-cells storage))
         (hashes (hss-hash-vector storage))
         (psl (hss-psl-vector storage)))
    (assert (= (length hashes) (length psl)))
    (when (> (length hashes) 65536)
      (assert (sb-impl::hss-hv-inexact (hashset-storage hashset))))
    (dotimes (i (length hashes))
      (let ((key (aref cells i)))
        (when (and key (not (hs-chain-terminator-p key)))
          ;; For each key, the stored hash should be correct
          (assert (= (ldb (byte 16 0) (funcall (hashset-hash-function hashset) key))
                     (aref hashes i)))
          (let ((sequence (hashset-probing-sequence hashset key)))
            ;; And the stored probing-sequence-length should match the actual
            (unless (= (aref psl i) (length sequence))
              (error "Wrong at ~S: sequence=~S stored=~S"
                      key sequence (aref psl i)))))))))

(defun make-string-hashset (case-sensitive-p)
  (if case-sensitive-p
      (sb-int:make-hashset 4 #'string= #'sb-kernel:%sxhash-simple-string)
      (sb-int:make-hashset 4 #'string-equal #'sb-impl::psxhash)))

;; HASHSET DOES NOT ALLOW INSERTING AN EXISTING KEY.
;; IT WILL VIOLATE INVARIANTS, BUT IT DOES NOT CHECK FOR IT.
(defun insert-all-into-hashset (hashset strings existsp-check)
  (let ((n 0)
        (worst-max-probes 0))
    (dolist (string strings)
      (when (or (not existsp-check)
                (not (sb-int:hashset-find hashset string)))
        (sb-int:hashset-insert hashset string)
        (when (zerop (mod (incf n) 1000))
          (hashset-check-invariants hashset)
          (multiple-value-bind (mean-psl histogram load-factor)
              (sb-impl::hashset-statistics (hashset-storage hashset))
            ;; (format t "~,4f ~7,4f  ~s~%" load-factor mean-psl histogram)
            (assert (<= load-factor .75))
            (assert (< mean-psl 3))
            ;; this is a bit of a "change detector" but I hope it remains correct for a while
            (setf worst-max-probes (max worst-max-probes (length histogram)))
            #+nil (assert (< (length histogram) 10))))))
    (hashset-check-invariants hashset)
    (values hashset worst-max-probes)))

(defparameter *lottastrings*
  (let ((h (make-hash-table :test #'equal)))
    (dolist (p (list-all-packages))
      (flet ((add-symbols (table)
               (sb-int:dovector (symbol (sb-impl::symtbl-cells table))
                 (when (symbolp symbol)
                   (setf (gethash (string symbol) h) t)
                   (setf (gethash (string-downcase symbol) h) t)
                   (setf (gethash (reverse (string symbol)) h) t)))))
        (add-symbols (sb-impl::package-internal-symbols p))
        (add-symbols (sb-impl::package-external-symbols p))))
    (loop for k being each hash-key of h collect k)))

(defun insert-all-into-hash-table (strings weakness &optional (test 'equal))
  (let ((hash-table (make-hash-table :test test :weakness weakness)))
    (dolist (string strings hash-table)
      (setf (gethash string hash-table) t))))

(defparameter *ht0* (insert-all-into-hash-table *lottastrings* nil))
;(defparameter *ht1* (insert-all-into-hash-table *lottastrings* :key))
;(defparameter *ht2* (insert-all-into-hash-table *lottastrings* :value))

(defun read-all-from-hash-table (strings hash-table ntimes &aux (result 0))
  (declare (fixnum ntimes result))
  (dotimes (i ntimes result)
    (dolist (string strings)
      (when (gethash string hash-table) (incf result)))))
(defun read-all-from-hashset (strings hashset ntimes &aux (result 0))
  (declare (fixnum ntimes result))
  (dotimes (i ntimes result)
    (dolist (string strings)
      (when (sb-int:hashset-find hashset string) (incf result)))))

(with-test (:name :string-hashset)
  (sb-int:binding* (((hs worst-max-probes)
                     (insert-all-into-hashset (make-string-hashset t) *lottastrings* nil)))
    (format t "~&Worst max probes: ~D~%" worst-max-probes)
    (assert (= (read-all-from-hash-table *lottastrings* *ht0* 1)
               (read-all-from-hashset *lottastrings* hs 1)))))

(with-test (:name :case-insensitive-string-hashset)
  (sb-int:binding*
      ((ht (insert-all-into-hash-table *lottastrings* nil 'equalp))
       ((hs worst-max-probes)
        (insert-all-into-hashset (make-string-hashset nil)
                                 *lottastrings* t))) ; check existence before inserting
    (format t "~&Worst max probes: ~D~%" worst-max-probes)
    (assert (= (read-all-from-hash-table *lottastrings* ht 1)
               (read-all-from-hashset *lottastrings* hs 1)))))

;(format t "~&Timing weak hash-table, KEY weak:~%")
;(time (read-all-from-hash-table *lottastrings* *ht1* 20))
;(format t "~&Timing weak hash-table, VALUE weak:~%")
;(time (read-all-from-hash-table *lottastrings* *ht2* 20))
;(format t "~&Timing ordinary hash-table:~%")
;(time (read-all-from-hash-table *lottastrings* *ht3* 20))
;
;(format t "~&Timing hash-set:~%")
;(time (read-all-from-hashset *lottastrings* *hs* 20))


#|
(defun hs-check-loop-unroll (&optional (start-index 0) (mask #xff) (count 10))
  (format t "~&regular way:~%")
  (let ((index start-index) (iteration 1))
    (dotimes (i count (terpri))
      (format t " ~D" index)
      (setq index (logand (+ index iteration) mask))
      (incf iteration)))
  (format t "~&Unrolled 2x:~%")
  (let ((index start-index) (iteration 1))
    (dotimes (i (ceiling count 2) (terpri))
      (let ((next-index (logand (+ index iteration) mask)))
        (format t " ~D ~D" index next-index)
        (setq index (logand (+ next-index iteration 1) mask))
        (incf iteration 2)))))
|#
