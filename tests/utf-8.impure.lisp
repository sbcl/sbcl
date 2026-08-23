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
(setf *random-state* (make-random-state t))

(defun free-protected-array (vector)
  (let* ((addr (sb-sys:sap-int (sb-sys:vector-sap vector)))
         (rw (logand addr (- +page-size+))))
    (sb-posix:munmap (sb-sys:int-sap (- rw +page-size+))
                     (* +page-size+ 3))))

(defun free-protected-sap (sap)
  (let* ((addr (sb-sys:sap-int sap))
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

(defun make-protected-sap (bytes align-to-start)
  (let* ((whole (sb-posix:mmap nil (* +page-size+ 3)
                               (logior sb-posix:prot-read
                                       sb-posix:prot-write)
                               (logior sb-posix:map-private sb-posix:map-anon) -1 0))
         (rw (sb-sys:sap+ whole +page-size+))
         (sap (sb-sys:sap+ rw (if align-to-start
                                   0
                                   (- +page-size+ bytes)))))
    (sb-posix:mprotect whole +page-size+ sb-posix:prot-none)
    (sb-posix:mprotect (sb-sys:sap+ whole (* +page-size+ 2))
                       +page-size+ sb-posix:prot-none)
    sap))

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
        do (loop repeat (1+ (random (min (- (length string) i)
                                         80)))
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
                     (free-protected-array bytes))))
        (free-protected-array string)))

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

(defun random-element (seq)
  (elt seq (random (length seq))))

#+(or arm64 x86-64)
(defun test-utf8-to-string (crlf)
  (let* ((buffer-length 1024)
         (vector (make-array buffer-length :element-type '(unsigned-byte 8)))
         (sap (make-protected-sap buffer-length nil))
         (ibuf (sb-impl::alloc-buffer buffer-length))
         (string (make-protected-array 512 'character nil)))
    (setf (slot-value ibuf (opaque-identity 'sb-impl::sap)) sap)
    (unwind-protect
         (progn
           (setf (sb-impl::buffer-tail ibuf) buffer-length)
           (let ((random-string (make-string buffer-length)))
             (loop repeat (* 512 #+slow 10)
                   do
                   (fill-random-string random-string)
                   (replace vector (string-to-octets random-string))
                   (let ((crlf (when crlf
                                 (random (1- (length vector)))))
                         (error (random (- (length vector) 4))))
                     (when crlf
                       (setf (aref vector crlf) #xd)
                       (setf (aref vector (1+ crlf)) #xa))
                     (when (zerop (random 2))
                       (replace vector
                                (if (zerop (random 2))
                                    (loop repeat (1+ (random 4))
                                          collect (+ 128 (random 128)))
                                    (random-element '((#xc0 #xaf)
                                                      (#xc1 #xaf)
                                                      (#xe0 #x80 #x80)
                                                      (#xf0 #x80 #x80 #x80)
                                                      (#xed #xa0 #x80)
                                                      (#xf4 #x90 #x80 #x80)
                                                      (#x41 #xff #x42)
                                                      (#x80 #x80 #x80)
                                                      (#xe2 #x82 #x41)
                                                      (#xf0 #x9f #x92 #x41))))
                                :start1 error)))

                   (setf (sb-impl::buffer-head ibuf) 0
                         (sb-impl::buffer-tail ibuf) buffer-length)
                   (sb-kernel:copy-ub8-to-system-area vector 0 sap 0 (length vector))
                   (let* ((bad (validate-utf8 vector))
                          (chars (sb-vm::utf8-crlf-to-character-string 0 (length string) string ibuf))
                          (bytes (sb-impl::buffer-head ibuf))
                          (decoded (octets-to-string vector :end bad
                                                            :external-format '(:utf8 :newline :crlf))))
                     (when bad
                       (assert (>= bad bytes)))
                     (unless (string= string decoded :end1 chars :end2 chars)
                       (error "~s" vector))))))
      (free-protected-sap sap)
      (free-protected-array string))))

(with-test (:name :utf8-to-character-string
            :implemented-on (or :arm64 :x86-64))
  (test-utf8-to-string nil))

(with-test (:name :utf8-crlf-to-character-string
            :implemented-on (or :arm64 :x86-64))
  (test-utf8-to-string t))

#+(or arm64 x86-64)
(with-test (:name :character-string-to-utf8)
  (flet ((run-test (string obuf vector)
           (loop repeat (* 256 #+slow 10)
                 do
                 (if (zerop (random 4))
                     (fill-random-string string t)
                     (fill-random-string string))

                 (dotimes (i (random 5))
                   (setf (char string (random (length string))) #\Newline))

                 ;; Surrogates
                 (dotimes (i (random 3))
                   (setf (char string (random (length string)))
                         (code-char (+ #xD800 (random 2048)))))

                 (let* ((start (random 64))
                        (end (max start (- (length string) (random 64))))
                        (initial-tail (random 64)))

                   (setf (sb-impl::buffer-head obuf) 0
                         (sb-impl::buffer-tail obuf) initial-tail)

                   (multiple-value-bind (read last-newline)
                       (sb-vm::character-string-to-utf8 start end string obuf)

                     (let* ((new-tail (sb-impl::buffer-tail obuf))
                            (bytes-written (- new-tail initial-tail))
                            (expected-octets (string-to-octets string :start start :end read))
                            (expected-newline (position #\Newline string :start start :end read :from-end t)))

                       (sb-kernel:copy-ub8-from-system-area (sb-impl::buffer-sap obuf) initial-tail vector 0 bytes-written)

                       (assert (equalp (subseq vector 0 bytes-written) expected-octets) ()
                               "Mismatch in octets: start ~a end ~a read ~a bytes ~a~% expected ~s~% got ~s"
                               start end read bytes-written expected-octets (subseq vector 0 bytes-written))
                       (assert (eql last-newline (or expected-newline -1)) ()
                               "Mismatch in last-newline: expected ~a, got ~a" expected-newline last-newline)))))))

    (let ((vector (make-array sb-impl::+bytes-per-buffer+ :element-type '(unsigned-byte 8)))
          (obuf (sb-impl::alloc-buffer))
          (string (make-protected-array 512 'character nil)))
      (unwind-protect
           (run-test string obuf vector)
        (free-protected-array string)))

    (let ((vector (make-array sb-impl::+bytes-per-buffer+ :element-type '(unsigned-byte 8)))
          (obuf (sb-impl::alloc-buffer))
          (random-string (make-string sb-impl::+bytes-per-buffer+ :element-type 'character)))
      (run-test random-string obuf vector))))

(with-test (:name :utf8-strlen
            :skipped-on :interpreter)
  (flet ((test (bytes &optional (offset 0))
           (loop for prefix from 0 to (if (> offset 0)
                                          0
                                          128)
                 for string = (fill-random-string (make-string prefix))
                 do
                 (loop with bytes = (concatenate '(vector (mod 256))
                                                 (string-to-octets string)
                                                 bytes)
                       for align-to-start in '(t nil)
                       do
                       (let* ((nul (position 0 bytes :start offset))
                              (sub (subseq bytes offset nul))
                              (bad (validate-utf8 sub))
                              (expected-byte-length (length sub))
                              (expected-length (unless bad
                                                 (length (octets-to-string sub))))
                              (expected-ascii-p (every (lambda (c)
                                                         (< c 128)) sub))
                              (sap (make-protected-sap (length bytes) align-to-start)))
                         (when align-to-start
                           (setf sap (sb-sys:sap+ sap (random 128))))
                         (sb-kernel:copy-ub8-to-system-area bytes 0 sap 0 (length bytes))
                         (unwind-protect
                              (multiple-value-bind (length byte-length ascii-p)
                                  (sb-vm::utf8-strlen (sb-sys:sap+ sap offset))
                                (unless (and (eql expected-length length)
                                             (eql expected-ascii-p (and ascii-p t))
                                             (or (not expected-length)
                                                 (eql expected-byte-length byte-length)))
                                  (error "(sb-vm::utf8-strlen (sb-sys:sap+ (sb-sys:vector-sap ~s) ~s)) => ~a, ~a, ~a; but ~a, ~a, ~a expected"
                                         bytes offset length byte-length ascii-p
                                         expected-length expected-byte-length expected-ascii-p)))
                           (free-protected-sap sap)))))))
    (test '(1 2 0 255 255))
    (test (append (loop for i from 1 to 64 collect i) '(0 1 255)))
    (test '(1 2 0 1 1 1))
    (test '(0 0 1 2 0) 2)
    (test '(1 1 1 2 0) 2)
    (test '(255 255 1 127 0) 2)
    (test '(255 255 1 2 0))
    (test '(255 255 1 2 0))
    (test '(240 159 152 130 0))
    (test '(1 240 159 152 130 0))
    (test '(1 240 159 152 130 2 0))
    (test '(65 195 132 226 130 172 240 159 152 130 0))
    (test '(#xf4 #x8f #xbf #xbf 0))
    (test '(#xe0 #xa0 #x80 0))
    (test (append (loop for i from 1 to 61 collect i) '(240 159 152 130 0 255)))
    (test (append (loop for i from 1 to 62 collect i) '(240 159 152 130 0 240)))
    (test (append (loop for i from 1 to 63 collect i) '(240 159 152 130 0 250)))
    (test (append (loop for i from 1 to 63 collect i) '(195 169 0 128)))

    (test '(#xc0 #xaf 0))
    (test '(#xe0 #x80 #x80 0))
    (test '(#xf0 #x80 #x80 #x80 0))
    (test '(#xed #xa0 #x80 0))
    (test '(#xf4 #x90 #x80 #x80 0))
    (test '(#x41 #xff #x42 0))
    (test '(#x80 #x80 #x80 0))
    (test '(#xe2 #x82 #x41 0))
    (test '(#xf0 #x9f #x92 0))))
