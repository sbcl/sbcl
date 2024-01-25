
(defun list-all-layouts ()
  #+permgen
  (let (list)
    ;; I should probably incorporate permgen into the set of spaces
    ;; that can be listed, but until then ...
    (sb-vm::map-objects-in-range
     (lambda (obj widetag size)
       (declare (ignore widetag size))
       (when (sb-kernel::layout-p obj)
         (push obj list)))
     (sb-kernel:%make-lisp-obj sb-vm:permgen-space-start)
     (sb-kernel:%make-lisp-obj (sb-sys:sap-int sb-vm:*permgen-space-free-pointer*)))
    list)
  #-permgen
  (sb-vm:list-allocated-objects :all :type sb-vm:instance-widetag
                                     :test #'sb-kernel::layout-p))

(with-test (:name :funinstance-layout-bitmaps-all-same)
  (let* ((list (list-all-layouts))
         (fun-layouts
          (remove-if-not (lambda (x) (find (sb-kernel:find-layout 'function)
                                           (sb-kernel:layout-inherits x)))
                         list))
         (bitmaps (mapcar 'sb-kernel::%layout-bitmap fun-layouts)))
    (assert (= (length (remove-duplicates bitmaps)) 1))))

(with-test (:name :stream-layout-bits)
  (loop for wrapper being each hash-value
        of (sb-kernel:classoid-subclasses (sb-kernel:find-classoid 't))
        do (flet ((check-bit (bit ancestor-type)
                    (let ((ancestor (sb-kernel:find-layout ancestor-type)))
                      (when (or (eq wrapper ancestor)
                                (find ancestor (sb-kernel:layout-inherits wrapper)))
                        (assert (logtest bit (sb-kernel:layout-flags wrapper)))))))
              (check-bit sb-kernel:+stream-layout-flag+ 'stream)
              (check-bit sb-kernel:+string-stream-layout-flag+ 'string-stream)
              (check-bit sb-kernel:+file-stream-layout-flag+ 'file-stream))))

(with-test (:name :boxed-layout-bits)
  ;; Negative test
  (dolist (name '(hash-table sb-thread:thread sb-thread::avlnode))
    (let ((layout (sb-kernel:find-layout name)))
      (assert (not (logtest (sb-kernel:layout-flags layout)
                            sb-kernel:+strictly-boxed-flag+)))))
  ;; Positive test, just a small sampling
  (dolist (name '(condition warning error
                  pathname logical-pathname
                  sb-impl::string-output-stream
                  structure-object sb-c::node
                  fundamental-stream))
    (let ((layout (sb-kernel:find-layout name)))
      (assert (logtest (sb-kernel:layout-flags layout)
                       sb-kernel:+strictly-boxed-flag+)))))

;;; Test some aspects of bitmaps, and the iterator.

;;; A layout-bitmap has the same representation as a BIGNUM-
;;; least-significant word first, native endian within the word.
;;; Like a bignum, all but the last word are unsigned, and the last is signed.
;;; This representation allows trailing slots to be either all tagged
;;; or all untagged.

(defstruct d2)
(defstruct (d3 (:include d2)))
(defstruct (d4 (:include d3)))
(defstruct (d5 (:include d4)))
(defstruct (d6 (:include d5)))
(defstruct (d7 (:include d6)))
(defstruct (d8 (:include d7)))
(defstruct (d9 (:include d8)))
(defstruct (d10 (:include d9)))
(defstruct (d11 (:include d10)))
(defstruct (d12 (:include d11)))
(defstruct (d13 (:include d12)))
(defstruct (d14 (:include d13)))
(defstruct (d15 (:include d14)))

(defparameter *test-layouts*
  (coerce (list* (sb-kernel:find-layout 't)
                 (sb-kernel:find-layout 'structure-object)
                 (loop for i from 2 to 15
                    collect (sb-kernel:find-layout (intern (format nil "D~D" i)))))
          'vector))

;;; Assert that BITMAP-NWORDS is insensitive to depthoid
(with-test (:name :bitmap-nwords-1)
  (loop for depthoid from 3 to 16
     do
       (let ((layout (sb-kernel:make-layout
                      1 ; random hash
                      (sb-kernel:make-undefined-classoid 'blah)
                      :depthoid depthoid
                      :bitmap #+64-bit #x6fffffffeeeeff02 ; 1-word bignum
                              #-64-bit #x70ffe123
                      :inherits (subseq *test-layouts* 0 depthoid)
                      :flags sb-kernel:+structure-layout-flag+)))
         (assert (= (sb-kernel:bitmap-nwords layout) 1)))))
(with-test (:name :bitmap-nwords-2)
  (loop for depthoid from 3 to 16
     do
       (let ((layout (sb-kernel:make-layout
                      1 ; random hash
                      (sb-kernel:make-undefined-classoid 'blah)
                      :depthoid depthoid
                      :bitmap #+64-bit #xffffffffeeeeff02 ; 2-word bignum
                              #-64-bit #x80ffe123
                      :inherits (subseq *test-layouts* 0 depthoid)
                      :flags sb-kernel:+structure-layout-flag+)))
         (assert (= (sb-kernel:bitmap-nwords layout) 2)))))

(defun layout-id-vector-sap (layout)
  (sb-sys:sap+ (sb-sys:int-sap (sb-kernel:get-lisp-obj-address layout))
               (- (ash (+ sb-vm:instance-slots-offset
                          (sb-kernel:get-dsd-index sb-kernel:layout sb-kernel::id-word0))
                       sb-vm:word-shift)
                  sb-vm:instance-pointer-lowtag)))

;;;; Ensure ID uniqueness and that layout ID words match the ID's in the INHERITS vector.
(defparameter *all-wrappers*
  (delete-if
   ;; temporary layouts (created for parsing DEFSTRUCT)
   ;; must be be culled out.
   (lambda (x)
     (and (typep (sb-kernel:layout-classoid x)
                 'sb-kernel:structure-classoid)
          (eq (sb-kernel:layout-equalp-impl x)
              #'sb-kernel::equalp-err)))
   (sb-vm::list-allocated-objects :all
                                  :type sb-vm:instance-widetag
                                  :test #'sb-kernel::layout-p)))

;;; Assert no overlaps on ID
(with-test (:name :id-uniqueness)
  (let ((hash (make-hash-table)))
    (dolist (wrapper *all-wrappers*)
      (let ((id (sb-kernel:layout-id wrapper)))
        (sb-int:awhen (gethash id hash)
          (error "ID ~D is ~A and ~A" id sb-int:it wrapper))
        (setf (gethash id hash) wrapper)))))

;;; Assert that IDs are right
(with-test (:name :id-versus-inherits)
  (let ((structure-object (sb-kernel:find-layout 'structure-object)))
    (dolist (wrapper *all-wrappers*)
      (when (find structure-object (sb-kernel:layout-inherits wrapper))
        (let* ((layout wrapper)
               (ids
                (sb-sys:with-pinned-objects (layout)
                  (let ((sap (layout-id-vector-sap layout)))
                    (loop for depthoid from 2 to (sb-kernel:layout-depthoid wrapper)
                       collect (sb-sys:signed-sap-ref-32 sap (ash (- depthoid 2) 2))))))
               (expected
                (map 'list 'sb-kernel:layout-id (sb-kernel:layout-inherits wrapper))))
          (unless (equal (list* (sb-kernel:layout-id (sb-kernel:find-layout 't))
                                (sb-kernel:layout-id (sb-kernel:find-layout 'structure-object))
                                ids)
                         (append expected (list (sb-kernel:layout-id wrapper))))
            (error "Wrong IDs for ~A: expect ~D actual ~D~%"
                   wrapper expected ids)))))))

(makunbound '*all-wrappers*)

(defun random-bitmap (nwords random-state sign-bit)
  (let ((integer 0)
        (position 0))
    ;; Deposit N-WORD-BITS bits into INTEGER NWORDS times,
    ;; then make sure the sign bit is as requested.
    (dotimes (i nwords)
      (setf (ldb (byte sb-vm:n-word-bits position) integer)
            ;; If the PRNG generates a 0 word, change it to 1.
            (max 1 (random (ash 1 sb-vm:n-word-bits) random-state)))
      (incf position sb-vm:n-word-bits))
    ;; If INSTANCE-DATA-START is 1, then the 0th bitmap bit must be 0
    ;; because we don't want LAYOUT to be lumped in with tagged slots
    ;; (even though it's of course tagged)
    (when (and (= sb-vm:instance-data-start 1) (oddp integer))
      (setq integer (logxor integer 1)))
    (ecase sign-bit
      (:positive
       (ldb (byte (1- (* nwords sb-vm:n-word-bits)) 0) integer))
      (:negative
       (dpb integer (byte (1- (* nwords sb-vm:n-word-bits)) 0) -1)))))
(compile'random-bitmap)

;;; Check the random bitmap generator a little.
(with-test (:name :check-random-bitmaps)
  (loop for nwords from 2 to 8
     do (dolist (sign '(:positive :negative))
          (dotimes (i 100)
            (let ((b (random-bitmap nwords *random-state* sign)))
              (assert (= (sb-bignum:%bignum-length b) nwords)))))))

(defun make-layout-for-test (depthoid bitmap)
  (sb-kernel:make-layout 1 ; random hash
                         (sb-kernel:make-undefined-classoid 'blah)
                         :depthoid depthoid
                         :bitmap bitmap
                         :inherits (subseq *test-layouts* 0 depthoid)
                         :flags sb-kernel:+structure-layout-flag+))
(compile 'make-layout-for-test)

(defun test-bitmap-iterator (layout instance-length reference-bitmap)
  (let ((count 0))
    (declare (fixnum count))
    (sb-kernel:do-layout-bitmap (slot-index taggedp layout instance-length)
      (incf count)
      (sb-int:aver (eq (logbitp slot-index reference-bitmap) taggedp)))
    (sb-int:aver (= count (- instance-length sb-vm:instance-data-start)))))
(compile 'test-bitmap-iterator)

;;; Now randomly test bitmaps of varying length in words
;;; and for both values of the sign bit in the last word.
;;; Test with instances that are longer than the bitmap's significant bit count
;;; so that we can verify infinite sign-extension.
;;; And test with shorter to make sure the loop is properly bounded
;;; by the instance length.
(with-test (:name :random-bitmaps)
  (let ((rs (make-random-state t)))
    ;; Modulate the depthoid so that BITMAP-START is at different indices.
    (loop for depthoid from 6 to 10
       do (loop for n-bitmap-words from 1 to 6
                do
             (dolist (sign '(:positive :negative))
               (let* ((bitmap (random-bitmap n-bitmap-words rs sign))
                      (layout (make-layout-for-test depthoid bitmap)))
                 (loop for instance-length from 5 to (* (+ n-bitmap-words 2)
                                                        sb-vm:n-word-bits)
                       do (test-bitmap-iterator layout instance-length bitmap))))))))
