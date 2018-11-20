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

#-x86-64 (sb-ext:exit :code 104) ;; not implemented elsewhere

(with-test (:name :aprof-smoketest-struct
            ;; reverse-engineering the allocation instructions fails but should not
            :fails-on (not :immobile-space))
  (let ((nbytes
         (let ((*standard-output* (make-broadcast-stream)))
           (sb-aprof:aprof-run
            (checked-compile
             '(sb-int:named-lambda "test" ()
                (declare (inline sb-thread:make-mutex)
                         (optimize sb-c::instrument-consing))
                (loop repeat 50 collect (sb-thread:make-mutex))))))))
    (assert (= nbytes
                (+ (* 51 2 sb-vm:n-word-bytes) ; list (extra for dummy head)
                   (* 50 (sb-vm::primitive-object-size
                          (sb-thread:make-mutex))))))))

(with-test (:name :aprof-smoketest-non-constant-size-vector
            :broken-on :win32)
  (let ((nbytes
         (let ((*standard-output* (make-broadcast-stream)))
           (sb-aprof:aprof-run
            (checked-compile
             '(sb-int:named-lambda "test" (&optional (n 10))
                (declare (optimize sb-c::instrument-consing))
                (make-array (the (mod 64) n))))))))
    (assert (= nbytes (* 12 sb-vm:n-word-bytes)))))

;;; The profiler's disassembler expected to see a store at alloc-ptr
;;; or that + n-word-bytes, when in fact the code might write to 1 byte
;;; positioned anywhere in the word after the object header.
(with-test (:name :aprof-smoketest-bit-vector
            :fails-on :win32)
  (let ((nbytes
         (let ((*standard-output* (make-broadcast-stream)))
           (sb-aprof:aprof-run
            (checked-compile
             '(sb-int:named-lambda "test" ()
                (declare (optimize sb-c::instrument-consing))
                (make-array (* 128 16) :element-type 'bit)))))))
    (assert (= nbytes (sb-vm::primitive-object-size
                       (make-array (* 128 16) :element-type 'bit))))))

(with-test (:name :aprof-smoketest-large-vector
            :fails-on :win32)
  (let ((nbytes
          (let ((*standard-output* (make-broadcast-stream)))
            (sb-aprof:aprof-run
             (checked-compile
              '(sb-int:named-lambda "test" ()
                (declare (optimize sb-c::instrument-consing))
                (make-array 45000)))))))
    (assert (= nbytes (* (+ 45000 sb-vm:vector-data-offset)
                         8)))))
sb-vm::
(define-vop (cl-user::alloc-to-r8)
  (:temporary (:sc any-reg :offset r8-offset :from :eval) result)
  (:node-var node)
  (:generator 1
    (let* ((bytes large-object-size) ; payload + header total
           (words (- (/ bytes n-word-bytes) vector-data-offset)))
      (instrument-alloc bytes node)
      (pseudo-atomic ()
       (allocation result bytes node nil other-pointer-lowtag)
       (storew* simple-array-unsigned-byte-64-widetag result 0
                other-pointer-lowtag t)
       (storew* (fixnumize words) result vector-length-slot
                other-pointer-lowtag t)))))

(with-test (:name :aprof-smoketest-large-vector-to-upper-register
            :fails-on :win32)
  (let ((nbytes
          (let ((*standard-output* (make-broadcast-stream)))
            (sb-aprof:aprof-run
             (checked-compile
              '(sb-int:named-lambda "test" ()
                (declare (optimize sb-c::instrument-consing))
                (sb-sys:%primitive cl-user::alloc-to-r8)
                nil))))))
    (assert (= nbytes sb-vm:large-object-size))))
