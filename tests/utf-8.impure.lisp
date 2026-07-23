;;;; This file is for testing external-format functionality for UTF-8,
;;;; using test machinery which does not have side effects.  Note that
;;;; the tests here reach into unexported functionality, and should
;;;; not be used as a guide for users.

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

#+(or (not sb-unicode)
      win32)
(invoke-restart 'run-tests::skip-file)

(require :sb-posix)

(defconstant +page-size+ (extern-alien "os_reported_page_size" int))

(defun free-protected-array (vector)
  (let* ((addr (sb-sys:sap-int (sb-sys:vector-sap vector)))
         (rw (logand addr (- +page-size+))))
    (sb-posix:munmap (sb-sys:int-sap (- rw +page-size+))
                     (* +page-size+ 3))))

(defun make-protected-array (length type align-to-start)
  (multiple-value-bind (widetag shift) (sb-vm::%vector-widetag-and-n-bits-shift type)
    (let* ((full-length (+ length (if (= widetag sb-vm::simple-base-string-widetag) 1 0)))
           (bytes (sb-vm:pad-data-block
                   (+ sb-vm:vector-data-offset
                      (sb-vm::vector-length-in-words full-length shift))))
           (whole (sb-posix:mmap nil (* +page-size+ 3)
                                 (logior sb-posix:prot-read
                                         sb-posix:prot-write)
                                 (logior sb-posix:map-private sb-posix:map-anon) -1 0))
           (rw (sb-sys:sap+ whole +page-size+))
           (addr (sb-sys:sap+ rw (if align-to-start
                                     0
                                     (- +page-size+ bytes))))
           (vector (sb-kernel:%make-lisp-obj (logior (sb-sys:sap-int addr)
                                                     sb-vm:other-pointer-lowtag))))
      (sb-posix:mprotect whole +page-size+ sb-posix:prot-none)
      (sb-posix:mprotect (sb-sys:sap+ whole (* +page-size+ 2))
                         +page-size+ sb-posix:prot-none)
      (setf (sb-sys:sap-ref-word addr 0) widetag)
      (setf (sb-kernel:%array-fill-pointer vector) length)
      vector)))

(defun decode-test (vector string-length)
  (sb-sys:with-pinned-objects (vector)
    (let* ((length (length vector))
           (string (make-protected-array string-length 'character nil)))
      (unwind-protect
           (progn (sb-vm::utf8-sap-to-character-string (sb-sys:vector-sap vector)
                                                       string
                                                       length)
                  (copy-seq string))

        (free-protected-array string)))))

(compile 'decode-test)

(defun fill-random-string (string &optional ascii)
  (loop with i = 0
        for width = (if ascii
                        0
                        (random 4))
        while (< i (length string))
        do (loop repeat (1+ (random (- (length string) i)))
                 for char = (case width
                              (0 (random 128))
                              (1 (+ 128 (random (- 2048 128))))
                              (2 (+ 2048 (random (- 50000 2048))))
                              (3 (+ 65536 (random (- char-code-limit 65536)))))
                 do (setf (aref string i) (code-char char))
                 (incf i)))
  string)

(defun strlen (bytes)
  (sb-vm::utf8-strlen (sb-sys:vector-sap bytes)))
(compile 'strlen)

(with-test (:name :decode-test)
  (loop for length from 1 to 32
        for string = (make-string length)
        do
        (loop repeat (* 500 #+slow 10)
              do (fill-random-string string)
                 (let* ((octets (sb-ext:string-to-octets string))
                        (bytes (make-protected-array (length octets) '(unsigned-byte 8) nil)))
                   (unwind-protect
                        (progn (replace bytes octets)
                               (assert (equal (decode-test bytes length)
                                              string)))
                     (free-protected-array bytes))))))

(with-test (:name :strlen-test)
  (loop for length from 1 to 32
        for string = (make-protected-array length 'character nil)
        do
        (loop repeat (* 500 #+slow 10)
              do (fill-random-string string)
                 (let* ((octets (sb-ext:string-to-octets string :null-terminate t))
                        (octet-length (1- (length octets)))
                        (bytes (make-protected-array (length octets) '(unsigned-byte 8) nil)))
                   (unwind-protect
                        (progn
                          (replace bytes octets)
                          (multiple-value-bind (strlen-chars strlen-bytes) (strlen bytes)
                            (unless (or (and (= strlen-chars length)
                                             (= strlen-bytes octet-length))
                                        (find #\Nul string))
                              (error "(strlen ~s) => ~a, ~a /= ~a, ~a" string strlen-chars strlen-bytes length octet-length)))
                          (let ((utf-length (sb-impl::character-string-utf8-length string)))
                            (unless (= utf-length octet-length)
                              (error "(sb-impl::character-string-utf8-length ~s) => ~a /= ~a"
                                     string utf-length octet-length))))
                     (free-protected-array bytes))))))

(defun encode-test (string byte-length)
  (let ((byte-array (make-protected-array byte-length '(unsigned-byte 8) nil)))
    (unwind-protect
         (progn (sb-vm::character-string-to-utf8-byte-array byte-array
                                                            string
                                                            byte-length)
                (copy-seq byte-array))
      (free-protected-array byte-array))))

(compile 'encode-test)

(with-test (:name :encode-test)
  (loop for length from 1 to 32
        for string = (make-protected-array length 'character nil)
        do
        (unwind-protect
             (loop repeat (* 500 #+slow 10)
                   do (fill-random-string string)
                      (let* ((octets (sb-ext:string-to-octets string))
                             (result (encode-test string (length octets))))
                        (unless (equalp result octets)
                          (error "(encode-test ~s ~a) => ~a /= ~a" string (length octets)
                                 result octets))))
          (free-protected-array string))))

(defun encode-test.ascii (string byte-length)
  (let ((byte-array (make-protected-array byte-length '(unsigned-byte 8) nil)))
    (unwind-protect
         (progn (sb-vm::character-string-to-ascii-byte-array byte-array
                                                             string
                                                             byte-length)
                (copy-seq byte-array))
      (free-protected-array byte-array))))

(defun decode-test.ascii (vector string-length)
  (sb-sys:with-pinned-objects (vector)
    (let* ((length (length vector))
           (string (make-protected-array string-length 'character nil)))
      (unwind-protect
           (progn (sb-vm::ascii-sap-to-character-string (sb-sys:vector-sap vector)
                                                        string
                                                        length)
                  (copy-seq string))

        (free-protected-array string)))))

(compile 'encode-test.ascii)
(compile 'decode-test.ascii)

(with-test (:name :decode-test.ascii)
  (loop for length from 1 to 256
        for string = (make-string length)
        do
        (fill-random-string string t)
        (let* ((octets (sb-ext:string-to-octets string))
               (bytes (make-protected-array (length octets) '(unsigned-byte 8) nil)))
          (unwind-protect
               (progn (replace bytes octets)
                      (assert (equal (decode-test.ascii bytes length)
                                     string)))
            (free-protected-array bytes)))))

(with-test (:name :encode-test.ascii)
  (loop for length from 1 to 256
        for string = (make-protected-array length 'character nil)
        do
        (unwind-protect
           (progn
             (fill-random-string string t)
             (let* ((octets (sb-ext:string-to-octets string))
                    (result (encode-test.ascii string (length octets))))
               (unless (equalp result octets)
                 (error "(encode-test ~s ~a) => ~a /= ~a" string (length octets)
                        result octets))))
          (free-protected-array string))))

(defun validate-utf8 (vector)
  (macrolet ((return-if-not-cont (x)
               `(let ((x ,x))
                  (unless (<= #x80 x #xBF)
                    (return index))
                  x)))
    (let ((index 0)
          (length (length vector)))
      (loop while (< index length)
            do
            (let ((b0 (aref vector index)))
              (cond
                ;; ASCII
                ((< b0 #x80)
                 (incf index 1))
                ;; 2 bytes
                ((<= #xC2 b0 #xDF)
                 (when (>= (+ index 1) length)
                   (return index))
                 (return-if-not-cont (aref vector (+ index 1)))
                 (incf index 2))

                ;; 3 bytes
                ((<= #xE0 b0 #xEF)
                 (when (>= (+ index 2) length)
                   (return index))
                 (let ((b1 (return-if-not-cont (aref vector (+ index 1))))
                       (b2 (return-if-not-cont (aref vector (+ index 2)))))
                   (declare (ignore b2 ))
                   (unless (if (= b0 #xE0)
                               (<= #xA0 b1 #xBF) ; Overlong
                               (if (= b0 #xED)
                                   (<= #x80 b1 #x9F) ; Surrogate halves
                                   t))
                     (return index)))
                 (incf index 3))
                ;; 4 bytes
                ((<= #xF0 b0 #xF4)
                 (when (>= (+ index 3) length)
                   (return index))
                 (let ((b1 (return-if-not-cont (aref vector (+ index 1))))
                       (b2 (return-if-not-cont (aref vector (+ index 2))))
                       (b3 (return-if-not-cont (aref vector (+ index 3)))))
                   (declare (ignore b2 b3))
                   (unless (if (= b0 #xF0)
                               (<= #x90 b1 #xBF) ; Overlong
                               (if (= b0 #xF4)
                                   (<= #x80 b1 #x8F) ; Too Large
                                   t))
                     (return index)))
                 (incf index 4))
                (t (return index))))))))


(with-test (:name :utf8-to-character-string)
  (let ((vector (make-array sb-impl::+bytes-per-buffer+ :element-type '(unsigned-byte 8)))
        (ibuf (sb-impl::alloc-buffer))
        (string (make-protected-array 512 'character nil)))
    (setf (sb-impl::buffer-tail ibuf) sb-impl::+bytes-per-buffer+)
    (loop repeat (* 512 #+slow 10)
          do
          (map-into vector (lambda () (random 256)))
          (setf (sb-impl::buffer-head ibuf) 0
                (sb-impl::buffer-tail ibuf) sb-impl::+bytes-per-buffer+)
          (sb-kernel:copy-ub8-to-system-area vector 0 (sb-impl::buffer-sap ibuf) 0 (length vector))
          (let* ((bad (validate-utf8 vector))
                 (chars (sb-vm::utf8-to-character-string 0 (length string) string ibuf))
                 (bytes (sb-impl::buffer-head ibuf))
                 (decoded (octets-to-string vector :end bad)))
            (when bad
              (assert (>= bad bytes)))
            (assert (string= string decoded :end1 chars :end2 chars))))))
