;;;; gc tests

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;
;;;; This software is in the public domain and is provided with
;;;; absoluely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(in-package :cl-user)

#-interleaved-raw-slots (invoke-restart 'run-tests::skip-file)

;;; More tests of raw slots can be found in 'defstruct.impure.lisp'
;;; Since those are all passing, it's fair to say that interleaving works.
;;; But we want also to test what happens in a very specific case that
;;; is difficult to provoke, when a structure contains enough slots that
;;; its raw bitmap is a bignum and the bignum is moved during GC.

(macrolet ((defbiggy ()
             `(defstruct biggy
                ,@(loop for i from 1 to 62
                        collect `(,(sb-int:symbolicate "SLOT" (write-to-string i))
                                  0 :type ,(if (> i 60) 'sb-ext:word t))))))
  (defbiggy))

(assert (typep (sb-kernel:layout-raw-slot-metadata
                (sb-kernel::find-layout 'biggy)) 'bignum))

(defvar *x* nil)
(defvar *y* nil)

;; This test offers "anecdotal evidence" that it works to have
;; a bignum for raw slot metadata, *and* that the bignum could be
;; transported by GC, leaving a forwarding pointer,
;; before transporting an instance of an object whose layout
;; sees the bignum.

;; Without extra augmentation of the GC code [such as printf("got here!")]
;; there is no visible means of determining that this works,
;; aside from GC not crashing.
;; Additionally, the test does not work - which is to say, the GC behavior
;; is different and the desired effect can't be observed - when placed in
;; a WITH-TEST or any other toplevel "noise"; but even without that,
;; the test is brittle.
;; With some extra annotation (printf of otherwise), the line
;; of code in positive_bignum_logbitp() is seen to be reached 63 times
;; in each test run, corresponding to the 63 slots (counting the layout)
;; in each structure instance, times two structure instances.

;; Run it twice to make sure things really worked.

(let ((*y* (make-biggy))
      (*x* (sb-kernel:layout-raw-slot-metadata
            (sb-kernel::find-layout 'biggy))))
  (sb-ext:gc :gen 1))
(princ 'did-pass-1) (terpri)
(force-output)

(let ((*y* (make-biggy))
      (*x* (sb-kernel:layout-raw-slot-metadata
            (sb-kernel::find-layout 'biggy))))
  (sb-ext:gc :gen 1))
(princ 'did-pass-2) (terpri)
(force-output)

;; Test the C bignum bit extractor.
;; Surprisingly, there was a bug in it, unrelated to forwarding
;; pointers that remained dormant until the randomized
;; HUGE-MANYRAW test in 'defstruct.impure.lisp' found it.
(defun c-bignum-logbitp (index bignum)
  (assert (typep bignum 'bignum))
  (sb-sys:with-pinned-objects (bignum)
    (alien-funcall (extern-alien "positive_bignum_logbitp"
                                 (function boolean int system-area-pointer))
                   index
                   (sb-sys:int-sap
                    (- (sb-kernel:get-lisp-obj-address bignum)
                       sb-vm:other-pointer-lowtag)))))

(with-test (:name :c-bignum-logbitp)
  ;; walking 1 bit
  (dotimes (i 256)
    (let ((num (ash 1 i)))
      (when (typep num 'bignum)
        (dotimes (j 257)
          (assert (eq (c-bignum-logbitp j num)
                     (logbitp j num)))))))
  ;; random bits
  (let ((max (ash 1 768)))
    (dotimes (i 100)
      (let ((num (random max)))
        (when (typep num 'bignum)
          (dotimes (j (* (sb-bignum:%bignum-length num)
                         sb-vm:n-word-bits))
            (assert (eq (c-bignum-logbitp j num)
                       (logbitp j num)))))))))

;; for testing the comparator
(defstruct foo1
  ;;                                  INDICES:    32-bit  64-bit
  ;;                                  ========   =======  ======
  (df 1d0 :type double-float)                    ;   1,2       1
  (a 'aaay)                                      ;     3       2
  (sf 1f0 :type single-float)                    ;     4       3
  (cdf #c(1d0 1d0) :type (complex double-float)) ;  5..8     4,5
  (b 'bee)                                       ;     9       6
  (csf #c(2f0 2f0) :type (complex single-float)) ; 10,11       7
  (w 0 :type sb-ext:word)                        ;    12       8
  (c 'cee))                                      ;    13       9

(defvar *afoo* (make-foo1))
(with-test (:name :tagged-slot-iterator-macro)
  ;; on 32-bit, slots 1 through 14 exist, keeping the total length even.
  #-64-bit (setf (sb-kernel:%instance-ref *afoo* 14) 'magic)
  ;; on 64-bit, slots 1 through 10 exist, keeping the total length even.
  #+64-bit (setf (sb-kernel:%instance-ref *afoo* 10) 'magic)
  (let (l)
    (sb-kernel:do-instance-tagged-slot (i *afoo*)
      (push `(,i ,(sb-kernel:%instance-ref *afoo* i)) l))
    (assert (oddp (sb-kernel:%instance-length *afoo*)))
    (assert (= (sb-kernel:layout-length (sb-kernel:layout-of *afoo*))
               (1- (sb-kernel:%instance-length *afoo*))))
    (assert (equalp (nreverse l)
                    #-64-bit
                    `((3 aaay) (9 bee) (13 cee) (14 magic))
                    #+64-bit
                    `((2 aaay) (6 bee) (9 cee) (10 magic))))))

(defvar *anotherfoo* (make-foo1))

(with-test (:name :structure-obj-equalp-raw-slots)
  ;; these structures are EQUALP even though one of them
  ;; has a word of junk in its padding slot, as could happen
  ;; if the structure was stack-allocated (I think)
  (assert (equalp *anotherfoo* *afoo*)))

(defstruct foo
  a
  (w 0 :type sb-ext:word)
  b
  (cdf #c(0d0 0d0) :type (complex double-float))
  c)
(sb-kernel:define-structure-slot-addressor
 foo-w-ptr :structure foo :slot w)
(sb-kernel:define-structure-slot-addressor
 foo-cdf-ptr :structure foo :slot cdf)

(with-test (:name :define-structure-slot-addressor)
  (let* ((word (logand sb-ext:most-positive-word #xfeedbad))
         (re 4.2d58)
         (im 8.93d-10)
         (thing (make-foo :cdf (complex re im) :w word)))
     (sb-sys:with-pinned-objects (thing)
      (assert (= word (sb-sys:sap-ref-word
                       (sb-sys:int-sap (foo-w-ptr thing)) 0)))
      (assert (= re (sb-sys:sap-ref-double
                     (sb-sys:int-sap (foo-cdf-ptr thing)) 0)))
      (assert (= im (sb-sys:sap-ref-double
                     (sb-sys:int-sap (foo-cdf-ptr thing)) 8))))))

(load "compiler-test-util.lisp")
(with-test (:name :copy-structure-efficient-case)
  (assert (not (ctu:find-named-callees #'copy-structure :name 'ash))))
