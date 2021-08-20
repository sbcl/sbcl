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

(with-test (:name (bit-vector bit-not bit-xor bit-and equal :small))
  ;; deal with the potential length 0 special case
  (locally
      (declare (optimize (speed 3) (safety 1) (space 0) (compilation-speed 0)))
    (let ((a (make-array 0 :element-type 'bit))
          (b (make-array 0 :element-type 'bit)))
      (assert (equal (bit-not a) #*))
      (assert (equal (bit-xor a b a) #*))
      (assert (equal (bit-and a a b) #*)))))

(with-test (:name (bit-vector bit-not bit-xor bit-and equal :modification))
  ;; also test some return values for sanity
  (locally
      (declare (optimize (speed 3) (safety 1) (space 0) (compilation-speed 0)))
    (let ((a (make-array 33 :element-type 'bit :initial-element 0))
          (b (make-array 33 :element-type 'bit :initial-element 0)))
      (assert (equal (bit-not a a) #*111111111111111111111111111111111))
      (setf (aref a 0) 0)               ; a = #*011..1
      (setf (aref b 1) 1)               ; b = #*010..0
      (assert (equal (bit-xor a b) #*001111111111111111111111111111111))
      (assert (equal (bit-and a b) #*010000000000000000000000000000000)))))

(with-test (:name (bit-vector count))
  ;; a special COUNT transform on bitvectors; triggers on (>= SPEED
  ;; SPACE)
  (locally
      (declare (optimize (speed 3) (space 1)))
    (let ((bv1 (make-array 5 :element-type 'bit :initial-element 0))
          (bv2 (make-array 0 :element-type 'bit :initial-element 0))
          (bv3 (make-array 68 :element-type 'bit :initial-element 0)))
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

;;; now test the biggy, mostly that it works...
;;;
;;; except on machines where the arrays won't fit into the dynamic
;;; space.
(when (> (sb-ext:dynamic-space-size)
         (truncate (1- array-dimension-limit)
                   sb-vm:n-word-bits))
  (push :sufficient-dynamic-space *features*))
(with-test (:name (bit-vector bit-not bit-and :big)
                  :skipped-on (:not :sufficient-dynamic-space))
  (locally
      (declare (optimize (speed 3) (safety 1) (space 0) (compilation-speed 0)))
    (let ((a (make-array (1- array-dimension-limit)
                         :element-type 'bit :initial-element 0))
          (b (make-array (1- array-dimension-limit)
                         :element-type 'bit :initial-element 0)))
      (bit-not a a)
      (assert (= (aref a 0) 1))
      (assert (= (aref a (- array-dimension-limit 2)) 1))
      (bit-and a b a)
      (assert (= (aref a 0) 0))
      (assert (= (aref a (- array-dimension-limit 2)) 0)))))

(with-test (:name (bit-vector find :non-bit-from-bit-vector))
  (assert (not (find #\a #*0101)))
  (assert (not (position #\a #*0101)))
  (checked-compile-and-assert ()
      `(lambda (b)
         (find b #*0101))
    ((t) nil))
  (checked-compile-and-assert ()
      `(lambda (b)
         (position b #*0101))
    ((t) nil))
  (checked-compile-and-assert ()
      `(lambda (b)
         (declare (bit-vector b))
         (find t b))
    ((#*010101) nil))
  (checked-compile-and-assert ()
      `(lambda (b)
         (declare (bit-vector b))
         (position t b))
    ((#*101010) nil)))

#-win32 (require :sb-posix)
;;; BIT-POSITION would access 1 word beyond a bit-vector's final word
;;; which could crash if the next page of memory was not readable. To
;;; produce such a sitution, mmap two pages the second one read
;;; protected, allocated the vector at the end of the first one and
;;; see if it touches the second pages.
#-win32 ;; no sb-posix:mmap
(with-test (:name :bit-position-overrun)
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
    (setf (sb-kernel:%array-fill-pointer
           (sb-kernel:%make-lisp-obj (logior (sb-sys:sap-int addr)
                                             sb-vm:other-pointer-lowtag)))
          n-bits)
    (let* ((object
             (sb-vm::reconstitute-object
              (sb-c::mask-signed-field
                                     sb-vm:n-fixnum-bits
                                     (ash (sb-sys:sap-int addr)
                                          (- sb-vm:n-fixnum-tag-bits)))))
           (size (sb-ext:primitive-object-size object)))
      (assert (simple-bit-vector-p object))
      (assert (= size n-bytes))
      (assert (not (sb-kernel:%bit-position/1 object nil 0 n-bits)))
      (assert (not (sb-kernel:%bit-position/1 object nil n-bits n-bits))))))

;;; Shamelessly piggybacking on the approach above to grab a page
;;; which adjoins an unreadable page for testing the disassembler.
#-win32 ;; no sb-posix:mmap
(with-test (:name :disassembler-overrun :skipped-on (not (or :x86 :x86-64)))
  (let* ((n-bytes 7)
         (first (sb-posix:mmap nil (* sb-c:+backend-page-bytes+ 2)
                               (logior sb-posix:prot-read
                                       sb-posix:prot-write)
                               (logior sb-posix:map-private sb-posix:map-anon) -1 0))
         (second (sb-sys:sap+ first sb-c:+backend-page-bytes+))
         (addr (sb-sys:sap+ second (- n-bytes))))
    (assert (sb-sys:sap=
             second
             (sb-posix:mmap second sb-c:+backend-page-bytes+
                            sb-posix:prot-none
                            (logior sb-posix:map-private sb-posix:map-anon sb-posix:map-fixed)
                            -1 0)))
    (loop for byte in '(#x8B #x50 #xFD #x8B #xE5 #xF8 #x5D)
          for i from 0
          do (setf (sb-sys:sap-ref-8 addr i) byte))
    (sb-disassem:disassemble-memory addr 7 :stream (make-broadcast-stream))))
