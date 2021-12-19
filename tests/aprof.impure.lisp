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

#-(and x86-64 sb-thread (not win32)) (sb-ext:exit :code 104) ;; not implemented elsewhere

(defstruct fruitbasket x y z)
(with-test (:name :aprof-smoketest-struct
                  :skipped-on :darwin
            ;; reverse-engineering the allocation instructions fails but should not
            :fails-on (not :immobile-space))
  (let ((nbytes
         (sb-aprof:aprof-run
            (checked-compile
             '(sb-int:named-lambda "test" ()
                (declare (inline make-fruitbasket)
                         (optimize sb-c::instrument-consing))
                (loop repeat 50 collect (make-fruitbasket))))
            :stream nil)))
    (assert (= nbytes
               (* 50 (+ (sb-ext:primitive-object-size (make-fruitbasket))
                        (* 2 sb-vm:n-word-bytes))))))) ; cons cells

(with-test (:name :aprof-smoketest-non-constant-size-vector)
  (let ((nbytes
         (sb-aprof:aprof-run
            (checked-compile
             '(sb-int:named-lambda "test" (&optional (n 10))
                (declare (optimize sb-c::instrument-consing))
                (make-array (the (mod 64) n))))
            :stream nil)))
    (assert (= nbytes (* 12 sb-vm:n-word-bytes)))))

;;; The profiler's disassembler expected to see a store at alloc-ptr
;;; or that + n-word-bytes, when in fact the code might write to 1 byte
;;; positioned anywhere in the word after the object header.
(with-test (:name :aprof-smoketest-bit-vector)
  (let ((nbytes
         (sb-aprof:aprof-run
            (checked-compile
             '(sb-int:named-lambda "test" ()
                (declare (optimize sb-c::instrument-consing))
                (make-array (* 128 16) :element-type 'bit)))
            :stream nil)))
    (assert (= nbytes (sb-ext:primitive-object-size
                       (make-array (* 128 16) :element-type 'bit))))))

(with-test (:name :aprof-smoketest-large-vector)
  (let ((nbytes
         (sb-aprof:aprof-run
             (checked-compile
              '(sb-int:named-lambda "test" ()
                (declare (optimize sb-c::instrument-consing))
                (make-array 45000)))
             :stream nil)))
    (assert (= nbytes (* (+ 45000 sb-vm:vector-data-offset)
                         8)))))
sb-vm::
(define-vop (cl-user::alloc-to-r8)
  (:temporary (:sc any-reg :offset r8-offset :from :eval) result)
  (:node-var node)
  (:generator 1
    (let* ((bytes large-object-size) ; payload + header total
           (temp sb-vm::r11-tn)
           (words (- (/ bytes n-word-bytes) vector-data-offset)))
      (instrument-alloc nil bytes node temp)
      (pseudo-atomic ()
       (allocation nil bytes 0 result node temp nil)
       (storew* simple-array-unsigned-byte-64-widetag result 0 0 t)
       (storew* (fixnumize words) result vector-length-slot 0 t)
       (inst or :byte result other-pointer-lowtag)))))

(with-test (:name :aprof-smoketest-large-vector-to-upper-register)
  (let ((nbytes
         (sb-aprof:aprof-run
             (checked-compile
              '(sb-int:named-lambda "test" ()
                (declare (optimize sb-c::instrument-consing))
                (sb-sys:%primitive cl-user::alloc-to-r8)
                nil))
             :stream nil)))
    (assert (= nbytes sb-vm:large-object-size))))

;; this moves an immediate-to-memory, then a load + store, then a store
(defun f1 (&optional x)
  (declare (optimize sb-c::instrument-consing))
  (list* :if-exists (load-time-value(gensym)) x))
;; this reverses the first two operations relative to the preceding
(defun f2 (&optional x)
  (declare (optimize sb-c::instrument-consing))
  (list* (load-time-value(gensym)) :if-exists x))

(import '(sb-vm::thread-mixed-tlab-slot sb-vm::thread-unboxed-tlab-slot
          sb-vm::rcx-tn sb-vm::rbp-tn sb-vm::r9-tn sb-vm::r10-tn sb-vm::rsi-tn
          sb-vm:cons-size sb-vm:n-word-bytes
          sb-vm::ea sb-vm:nil-value
          sb-vm:list-pointer-lowtag sb-vm:bignum-widetag))

(defstruct this-struct)
(defstruct that-struct)
(declaim (inline make-this-struct make-that-struct))
(defun make-structs ()
  (declare (optimize sb-c::instrument-consing))
  (values (make-this-struct) (make-that-struct)))
(compile 'make-structs)
(with-test (:name :aprof-instance :skipped-on (not :immobile-space))
  (let (seen-this seen-that)
    (dolist (line (split-string
                   (with-output-to-string (s)
                     (sb-aprof:aprof-run #'make-structs :stream s))
                   #\newline))
      (when (search "THIS-STRUCT" line) (setq seen-this t))
      (when (search "THAT-STRUCT" line) (setq seen-that t)))
    (assert (and seen-this seen-that))))

(defun my-list (&rest x)
  (declare (optimize sb-c::instrument-consing))
  x)
(compile 'my-list)

(with-test (:name :listify-rest-arg)
  (let ((nbytes (let ((*standard-output* (make-broadcast-stream)))
                  (sb-aprof:aprof-run #'my-list :arguments '(a b c)))))
    (assert (= nbytes (* sb-vm:n-word-bytes 6)))))

(defun make-new-code (n)
  (let ((f
         (compile nil
                   '(lambda (n)
                     (declare (optimize sb-c::instrument-consing))
                     (make-array (the fixnum n))))))
    (sb-aprof:aprof-reset) ; In case the counts are already nonzero
    (loop for i below n
       do (funcall f i))
    f))
(with-test (:name :make-new-code)
  (let* ((n 20)
         (nbytes (sb-aprof:aprof-run #'make-new-code :arguments n
                                     :stream (make-broadcast-stream)
                                     ;; FIXME: need to return nbytes with or without a report
                                     #|:report nil|#
                                     )))
    ;; If the lisp image was compiled with cons profiling, then the COMPILE
    ;; inside MAKE-NEW-CODE was also instrumented, messing up the result.
    ;; (The compiler conses about ~128Kb but there's no way to predict how much)
    (unless (sb-c:policy nil (> sb-c:instrument-consing 0))
      (assert (= (loop for i below n
                       sum (sb-ext:primitive-object-size (make-array i)))
                 nbytes)))))

(with-test (:name :aprof-brutal-test)
  (with-scratch-file (fasl "fasl")
    ;; Just compile anything that exercises the compiler.
    ;; This is only useful if the compiler was compiled with cons profiling.
    (sb-aprof:aprof-run #'compile-file
                        :stream (make-broadcast-stream)
                        :arguments `("../src/code/shaketree"
                                     :output-file ,fasl
                                     :print nil :verbose nil))))
