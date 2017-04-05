;;;; Tests of the hashtable routines used by gencgc

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

;;; All these tests have to be in without-gcing because the GC expects
;;; exclusive use of the "cached" memory allocator.
;;; Were it not for that, these tests would have nothing to do with GC.

(defun new-hh-table ()
  (let ((ht (sb-alien::%make-alien 100))) ; overestimate the C structure size
    (sb-sys:without-gcing
        (alien-funcall (extern-alien "hopscotch_create"
                                     (function void system-area-pointer int int int))
                       ht
                       1 ; values
                       32 ; size
                       8)) ; hop range
    ht))

(defun hhput (table key value)
  (sb-sys:without-gcing
      (alien-funcall (extern-alien "hopscotch_put"
                                   (function int system-area-pointer int unsigned))
                     table
                     (the fixnum key)
                     (sb-kernel:get-lisp-obj-address
                      (the (or fixnum character) value)))))

(defun hhget (table key)
  ;; GET doesn't need to inhibit GC
  (let ((result
         (alien-funcall (extern-alien "hopscotch_get"
                                      (function int system-area-pointer unsigned))
                        table
                        (the fixnum key))))
    (unless (eql result -1)
      (sb-kernel:make-lisp-obj
       (logand result sb-ext:most-positive-word)))))

(defun fill-table (table n-things)
  (let ((lisp-ht (make-hash-table :test 'eq)))
    (dotimes (i n-things)
      ;; Pick a random small integer key, but don't pick the empty marker (0).
      ;; Also shift left left by N-LOWTAG-BITS, because the hash function
      ;; is optimized specifically for object addresses.
      ;; In particular it discards that many bits from the hash, and the table
      ;; is unable to handle collisions to such a degree that the max hop
      ;; is exceeded. Resize will keep happening until mmap() fails.
      ;; This is not something worth "fixing" unless someone reports a real
      ;; problem.  GC is much faster with a dumb hash but fast table.
      (let ((k (ash (max 1 (random (ash 1 20))) sb-vm:n-lowtag-bits))
            (v (random (ash 1 29))))
        ;; The C code can't deal with a key already present
        (loop while (gethash k lisp-ht)
              do (setq k (max 1 (random (ash 1 20)))))
        (setf (gethash k lisp-ht) v)
        (hhput table k v)))
    (maphash (lambda (k v)
               (assert (eq (hhget table k) v)))
             lisp-ht)
    lisp-ht))

(defun randomly-bang-on-table (&optional (n-iter 10))
  (setq *random-state* (sb-ext:seed-random-state t))
  (dotimes (i n-iter)
    (let ((c-table (new-hh-table))
          (sizes-list '(10 20 50 100 1000
                        2000 5000 10000 15000 20000
                        99999)))
      ;; The table has a heuristic for selecting a hop range
      ;; and resizing based on the size at last reset.
      ;; Make sure these work with the table count
      ;; not strictly increasing on each test.
      (loop for n-items in (test-util:shuffle sizes-list)
            do
         ; (format t "~&Filling table with ~D things" n-items)
         (alien-funcall (extern-alien "hopscotch_reset"
                                      (function void system-area-pointer))
                        c-table)
         (let ((lisp-table (fill-table c-table n-items)))
           ;; We already checked that everything that should be found
           ;; in both is found. Also check that things that should not
           ;; be found are not found.
           ;; This also acts to increase the miss rate when gathering stats.
           (loop for i from 1 to (ash 1 15)
                 do (assert (eq (hhget c-table i)
                                (gethash i lisp-table))))
           ;; If statistics collection is enabled, show the performance
           (alien-funcall (extern-alien "hopscotch_log_stats"
                                        (function void system-area-pointer))
                          c-table)))
      (alien-funcall (extern-alien "hopscotch_delete"
                                   (function void system-area-pointer))
                     c-table))))

(with-test (:name :hopscotch-hash)
  (randomly-bang-on-table 1))
