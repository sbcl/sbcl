#-system-tlabs (invoke-restart 'run-tests::skip-file)

(in-package sb-vm)

(defvar *arena* (new-arena 1048576 1048576))
;;; This REWIND is strictly unnecessary. It simply should not crash
(rewind-arena *arena*)

(defun f (x y z)
  (with-arena (*arena*) (list x y z)))

(test-util:with-test (:name :arena-alloc-waste-reduction)
  (let* ((list1 (f 'foo 'bar'baz))
         (list1-addr (get-lisp-obj-address list1))
         (prev list1-addr))
    (dotimes (i 40)
      (let* ((list2 (f 'baz 'quux 'glerp))
             (this (get-lisp-obj-address list2)))
        ;; Thread should have picked up where it  left off in the arena
        ;; on the previous allocation.
        ;; The list is 3 conses.
        (assert (= (- this prev) (* 3 cons-size n-word-bytes)))
        (setq prev this)))
    (rewind-arena *arena*)
    (let* ((list3 (f 'zot nil 'bork))
           (this (get-lisp-obj-address list3)))
      (assert (= this list1-addr)))))

;;;

(defun test-with-open-file ()
  (with-open-file (stream (format nil "/proc/~A/stat" (sb-unix:unix-getpid))
                          :if-does-not-exist nil)
    (if stream
        (let ((pn (pathname stream)))
          (values pn (namestring pn) (read-line stream nil)))
        (values nil nil nil))))
(defun pathname-parts-heap-p (pathname)
  (labels ((scan (piece)
             (etypecase piece
               (fixnum t) ; pathname-version
               ((or string symbol bignum) (heap-allocated-p piece))
               (sb-impl::pattern (every #'scan (sb-impl::pattern-pieces piece)))
               (cons (and (scan (car piece)) (scan (cdr piece)))))))
    ;; just access the slots, don't "coerce" the arg to a pathname
    (and (scan (sb-impl::%pathname-namestring pathname))
         (scan (sb-impl::%pathname-device pathname))
         (scan (sb-impl::%pathname-dir+hash pathname))
         (scan (sb-impl::%pathname-name pathname))
         (scan (sb-impl::%pathname-type pathname))
         (scan (sb-impl::%pathname-version pathname)))))

(defvar *answerstring*)
(test-util:with-test (:name :with-open-stream :skipped-on (:not :linux))
  (multiple-value-bind (pathname namestring answer)
      (sb-vm:with-arena (*arena*) (test-with-open-file))
    (when pathname
      (assert (heap-allocated-p pathname))
      (assert (heap-allocated-p namestring))
      (assert (pathname-parts-heap-p pathname))
      (assert (not (heap-allocated-p answer)))
      ;; FIXME: special bindings aren't found- a regression or new finder just never did it?
      ;;    (declare (special *answerstring*))
      (unwind-protect
           (progn
             (setq *answerstring* answer) ; simulate special binding I guess
             ;; user's string went to the arena, and detector finds the source object
             (let ((finder-result (sb-vm:c-find-heap->arena)))
               (assert (equal finder-result '(*answerstring*)))))
        (makunbound '*answerstring*)))))

;;;

(defun test-vpe-heap-vector (vector count &aux grown)
  (with-arena (*arena*)
    (assert (not (heap-allocated-p (cons 1 2)))) ; assert arena is in use
    (dotimes (i count)
      (let ((old-data (%array-data vector)))
        (vector-push-extend i vector)
        (let ((new-data (%array-data vector)))
          (unless (eq new-data old-data)
            (assert (heap-allocated-p new-data))
            (setq grown t))))))
  (assert grown)) ; make sure the test proved something

(defun test-vpe-arena-vector (count &aux grown)
  (with-arena (*arena*)
    (let ((v (make-array 4 :fill-pointer 0 :adjustable t)))
      (assert (not (heap-allocated-p v)))
      (assert (not (heap-allocated-p (%array-data v))))
      (dotimes (i count)
        (let ((old-data (%array-data v)))
          (vector-push-extend i v)
          (let ((new-data (%array-data v)))
            (unless (eq new-data old-data)
              (assert (not (heap-allocated-p new-data)))
              (setq grown t)))))))
  (assert grown))

(defun test-puthash-heap-table (table count &aux grown)
  (assert (sb-impl::hash-table-hash-vector table)) ; require a hash vector
  (with-arena (*arena*)
    (assert (not (heap-allocated-p (cons 1 2))))
    (dotimes (i count)
      (let ((old-data (sb-impl::hash-table-pairs table)))
        (setf (gethash i table) i)
        (let ((new-data (sb-impl::hash-table-pairs table)))
          (unless (eq new-data old-data)
            (assert (heap-allocated-p new-data))
            (assert (heap-allocated-p (sb-impl::hash-table-hash-vector table)))
            (assert (heap-allocated-p (sb-impl::hash-table-index-vector table)))
            (assert (heap-allocated-p (sb-impl::hash-table-next-vector table)))
            (setq grown t))))))
  (assert grown))

(defun test-puthash-arena-table (count &aux grown)
  (with-arena (*arena*)
    (let ((table (make-hash-table :test 'equal)))
      (assert (sb-impl::hash-table-hash-vector table)) ; require a hash vector
      (assert (not (heap-allocated-p table)))
      (dotimes (i count)
        (let ((old-data (sb-impl::hash-table-pairs table)))
          (setf (gethash i table) i)
          (let ((new-data (sb-impl::hash-table-pairs table)))
            (unless (eq new-data old-data)
              (assert (not (heap-allocated-p new-data)))
              (assert (not (heap-allocated-p (sb-impl::hash-table-hash-vector table))))
              (assert (not (heap-allocated-p (sb-impl::hash-table-index-vector table))))
              (assert (not (heap-allocated-p (sb-impl::hash-table-next-vector table))))
              (setq grown t)))))))
  (assert grown))

;;; There is a case that this doesn't assert anything about, which is that
;;; an arena-allocated table or vector which grows while *not* in the scope of
;;; a WITH-ARENA (or inside a nested WITHOUT-ARENA) will go to the dynamic space.
;;; I think that is the right behavior: you can't force an object to be arena-allocated
;;; within a dynamic controls that asks for no arena allocation.
;;; I can't see how such a situation would legitimately arise,
;;; and it's probably only through application programmer error.

(test-util:with-test (:name :vector-push-extend-heap-vector)
  (test-vpe-heap-vector (make-array 4 :fill-pointer 0 :adjustable t) 100))

(test-util:with-test (:name :vector-push-extend-arena-vector)
  (test-vpe-arena-vector 100))

(test-util:with-test (:name :puthash-heap-table)
  (test-puthash-heap-table (make-hash-table :test 'equal) 100))

(test-util:with-test (:name :puthash-arena-table)
  (test-puthash-arena-table 100))

(defvar arena1 (new-arena 65536))
(defvar arena2 (new-arena 65536))

(defun f (a) (with-arena (a) (make-array 1000)))
(defun g (a) (with-arena (a) (list 'x 'y 'z)))

(defvar ptr1 (cons (f arena1) 'foo))
(defvar ptr2 (g arena2))

(test-util:with-test (:name :find-ptrs-all-arenas)
  (let ((result (c-find-heap->arena)))
    ;; There should be a cons pointing to ARENA1,
    ;; the cons which happens to be in PTR1
    (assert (member ptr1 result))
    ;; The symbol PTR2 points directly to ARENA2.
    (assert (member 'ptr2 result))
    ;; There should not be anything else
    (assert (= (length result) 2))))

(test-util:with-test (:name :find-ptrs-specific-arena)
  (let ((result (c-find-heap->arena arena1)))
    (assert (equal result (list ptr1))))
  (let ((result (c-find-heap->arena arena2)))
    (assert (equal result '(ptr2)))))
