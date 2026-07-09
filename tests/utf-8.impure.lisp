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
           (progn (sb-vm::simd-copy-utf8-sap-to-character-string (sb-sys:vector-sap vector)
                                                                 string
                                                                 length)
                  (copy-seq string))

        (free-protected-array string)))))

(compile 'decode-test)

(with-test (:name :decode-test)
  (loop for length from 1 to 32
        for string = (make-string length)
        do
        (loop repeat (* 500 #+slow 10)
              do (map-into string (lambda ()
                                    (code-char (random 4096))))
                 (let* ((octets (sb-ext:string-to-octets string))
                        (bytes (make-protected-array (length octets) '(unsigned-byte 8) nil)))
                   (unwind-protect
                        (progn (replace bytes octets)
                               (assert (equal (decode-test bytes length)
                                              string)))
                     (free-protected-array bytes))))))

(defun encode-test (string byte-length)
  (let ((byte-array (make-protected-array byte-length '(unsigned-byte 8) nil)))
    (unwind-protect
         (progn (sb-vm::simd-copy-character-string-to-utf8-byte-array byte-array
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
                   do (map-into string (lambda ()
                                         (code-char (random 4096))))
                      (let* ((octets (sb-ext:string-to-octets string))
                             (result (encode-test string (length octets))))
                        (unless (equalp result octets)
                          (error "(encode-test ~s ~a) => ~a /= ~a" string (length octets)
                                 result octets))))
          (free-protected-array string))))
