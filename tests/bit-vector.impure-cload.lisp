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

;;; the bitvector transforms were buggy prior to sbcl-0.7.3.4 under
;;; speed-optimizing regimes; in particular, they would fail if the
;;; vector length were near ARRAY-DIMENSION-LIMIT. Testing this takes
;;; up a certain amount of time...

(declaim (optimize (speed 3) (safety 1) (space 0) (compilation-speed 0)))

(defun test-small-bit-vectors ()
  ;; deal with the potential length 0 special case
  (let ((a (make-array 0 :element-type 'bit))
        (b (make-array 0 :element-type 'bit)))
    (assert (equal (bit-not a) #*))
    (assert (equal (bit-xor a b a) #*))
    (assert (equal (bit-and a a b) #*)))
  ;; also test some return values for sanity
  (let ((a (make-array 33 :element-type 'bit :initial-element 0))
        (b (make-array 33 :element-type 'bit :initial-element 0)))
    (assert (equal (bit-not a a) #*111111111111111111111111111111111))
    (setf (aref a 0) 0) ; a = #*011..1
    (setf (aref b 1) 1) ; b = #*010..0
    (assert (equal (bit-xor a b) #*001111111111111111111111111111111))
    (assert (equal (bit-and a b) #*010000000000000000000000000000000)))
  ;; a special COUNT transform on bitvectors; triggers on (>= SPEED SPACE)
  (locally
      (declare (optimize (speed 3) (space 1)))
    (let ((bv1 (make-array 5 :element-type 'bit))
          (bv2 (make-array 0 :element-type 'bit))
          (bv3 (make-array 68 :element-type 'bit)))
      (declare (type simple-bit-vector bv1 bv2 bv3))
      (setf (sbit bv3 42) 1)
      ;; bitvector smaller than the word size
      (assert (= 0 (count 1 bv1)))
      (assert (= 5 (count 0 bv1)))
      ;; special case of 0-length bitvectors
      (assert (= 0 (count 1 bv2)))
      (assert (= 0 (count 0 bv2)))
      ;; bitvector larger than the word size
      (assert (= 1 (count 1 bv3)))
      (assert (= 67 (count 0 bv3))))))

(defun inform (msg)
  (print msg)
  (force-output))

(defun test-big-bit-vectors ()
  ;; now test the biggy, mostly that it works...
  (let ((a (progn
             (inform :make-array-1)
             (make-array (1- array-dimension-limit)
                         :element-type 'bit :initial-element 0)))
        (b (progn
             (inform :make-array-2)
             (make-array (1- array-dimension-limit)
                         :element-type 'bit :initial-element 0))))
    (inform :bit-not)
    (bit-not a a)
    (inform :aref-1)
    (assert (= (aref a 0) 1))
    (inform :aref-2)
    (assert (= (aref a (- array-dimension-limit 2)) 1))
    (inform :bit-and)
    (bit-and a b a)
    (inform :aref-3)
    (assert (= (aref a 0) 0))
    (inform :aref-4)
    (assert (= (aref a (- array-dimension-limit 2)) 0))))

(test-small-bit-vectors)

;; except on machines where the arrays won't fit into the dynamic space.
#+#.(cl:if (cl:> (sb-ext:dynamic-space-size)
                 (cl:truncate (cl:1- cl:array-dimension-limit)
                              sb-vm:n-word-bits))
           '(and)
           '(or))
(test-big-bit-vectors)

(with-test (:name :find-non-bit-from-bit-vector)
  (assert (not (find #\a #*0101)))
  (assert (not (position #\a #*0101)))
  (let ((f1 (compile nil
                     `(lambda (b)
                        (find b #*0101))))
        (f2 (compile nil
                     `(lambda (b)
                        (position b #*0101)))))
    (assert (not (funcall f1 t)))
    (assert (not (funcall f2 t))))
  (let ((f1 (compile nil
                     `(lambda (b)
                        (declare (bit-vector b))
                        (find t b))))
        (f2 (compile nil
                     `(lambda (b)
                        (declare (bit-vector b))
                        (position t b)))))
    (assert (not (funcall f1 #*010101)))
    (assert (not (funcall f2 #*101010)))))

;;; BIT-POSITION would access 1 word beyond a bit-vector's final word
;;; which could crash if the next page of memory was not readable. To
;;; produce such a sitution, mmap two pages the second one read
;;; protected, allocated the vector at the end of the first one and
;;; see if it touches the second pages.
(with-test (:name :bit-position-overrun
                  :skipped-on :win32)
  (let* ((n-bytes (* 4 sb-vm:n-word-bytes))
         (first (sb-posix:mmap nil (* sb-c:+backend-page-bytes+ 2)
                               (logior sb-posix:prot-read
                                       sb-posix:prot-write)
                               (logior sb-posix:map-private sb-posix:map-anon) -1 0))
         (second (sb-sys:sap+ first sb-c:+backend-page-bytes+))
         (addr (sb-sys:sap+ second (- n-bytes)))
         (n-bits (* 2 sb-vm:n-word-bits)))
    (assert (sb-sys:sap=
             second
             (sb-posix:mmap second sb-c:+backend-page-bytes+
                            sb-posix:prot-none
                            (logior sb-posix:map-private sb-posix:map-anon sb-posix:map-fixed)
                            -1 0)))
    (setf (sb-sys:sap-ref-word addr 0) sb-vm:simple-bit-vector-widetag)
    (setf (sb-sys:sap-ref-word addr sb-vm:n-word-bytes)
          (ash n-bits sb-vm:n-fixnum-tag-bits))
    (multiple-value-bind (object widetag size)
        (sb-vm::reconstitute-object (ash (sb-sys:sap-int addr) (- sb-vm:n-fixnum-tag-bits)))
      (declare (ignore widetag))
      (assert (simple-bit-vector-p object))
      (assert (= size n-bytes))
      (assert (not (sb-kernel:%bit-position/1 object nil 0 n-bits))))))
