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

;;; More tests of raw slots can be found in 'defstruct.impure.lisp'
;;; Since those are all passing, it's fair to say that interleaving works.
;;; But we want also to test what happens in a very specific case that
;;; is difficult to provoke, when a structure contains enough slots that
;;; its raw bitmap is a bignum and the bignum is moved during GC.

(macrolet ((defbiggy ()
             `(defstruct biggy
                ,@(loop for i from 1 to 64
                        collect `(,(sb-int:symbolicate "SLOT" (write-to-string i))
                                  0 :type ,(if (= i 64) 'sb-ext:word t))))))
  (defbiggy))

(macrolet ((def-100slots ()
             `(defstruct 100slots
               ,@(loop for i from 0 repeat 100
                    collect `(,(sb-int:symbolicate "SLOT" (write-to-string i))
                              ,(format nil "This is ~D" i))))))
  (def-100slots))

(assert (typep (sb-kernel:wrapper-bitmap
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
      (*x* (sb-kernel:wrapper-bitmap
            (sb-kernel::find-layout 'biggy))))
  (sb-ext:gc :gen 1))
(princ 'did-pass-1) (terpri)
(force-output)

(let ((*y* (make-biggy))
      (*x* (sb-kernel:wrapper-bitmap
            (sb-kernel::find-layout 'biggy))))
  (sb-ext:gc :gen 1))
(princ 'did-pass-2) (terpri)
(force-output)

(defun collect-slot-values (struct &aux result)
  (sb-kernel:do-instance-tagged-slot (i struct)
    (push (sb-kernel:%instance-ref struct i) result))
  (nreverse result))

(with-test (:name :sign-extended-bitmap
            :fails-on :interpreter)
  ;; could have 100 or 101 physical payload slots depending on
  ;; presence of a padding word
  (assert (>= (length (collect-slot-values (make-100slots))) 100)))

;; for testing the comparator
(defstruct foo1
  ;;                                  INDICES:    32-bit  64-bit
  ;;                                  ========   =======  ======
  #+compact-instance-header
  (fluff 0 :type sb-ext:word)                    ;             0
  (df 1d0 :type double-float)                    ;   1,2       1
  (a 'aaay)                                      ;     3       2
  (sf 1f0 :type single-float)                    ;     4       3
  (cdf #c(1d0 1d0) :type (complex double-float)) ;  5..8     4,5
  (b 'bee)                                       ;     9       6
  (csf #c(2f0 2f0) :type (complex single-float)) ; 10,11       7
  (w 0 :type sb-ext:word)                        ;    12       8
  (c 'cee))                                      ;    13       9

(defvar *afoo* (make-foo1))
(assert (= (sb-kernel:wrapper-length (sb-kernel:wrapper-of *afoo*))
           (sb-kernel:%instance-length *afoo*)))
(with-test (:name :tagged-slot-iterator-macro
            :fails-on :interpreter)
  ;; on 32-bit, the logical length is 14, which means 15 words (with header),
  ;; but slot index 14 (word index 15) exists after padding to 16 memory words.
  ;; It is allowed to hold a fixnum (or any non-pointer) but naught else.
  #-64-bit (progn (assert (= (sb-kernel:%instance-length *afoo*) 14))
                  (setf (sb-kernel:%instance-ref *afoo* 14) #xdead))
  ;; on 64-bit, the logical length is 10, which means 11 words (with header),
  ;; but slot index 10 (word index 11) exists after padding to 12 memory words.
  #+64-bit (progn (assert (= (sb-kernel:%instance-length *afoo*) 10))
                  (setf (sb-kernel:%instance-ref *afoo* 10) #xdead))

  (let (l)
    (sb-kernel:do-instance-tagged-slot (i *afoo*)
      (push `(,i ,(sb-kernel:%instance-ref *afoo* i)) l))
    (assert (equalp (nreverse l)
                    #-64-bit `((3 aaay) (9 bee) (13 cee) (14 #xdead))
                    #+64-bit `((2 aaay) (6 bee) (9 cee) (10 #xdead))))))

(defvar *anotherfoo* (make-foo1))

(with-test (:name :structure-obj-equalp-raw-slots)
  ;; these structures are EQUALP even though one of them
  ;; has a word of junk in its padding slot, as could happen
  ;; if the structure was stack-allocated
  (assert (equalp *anotherfoo* *afoo*)))

(defstruct foo
  a
  (w 0 :type sb-ext:word)
  b
  (cdf #c(0d0 0d0) :type (complex double-float))
  c
  (sword -1 :type (integer #.(1- most-negative-fixnum) 100)))
(sb-kernel:define-structure-slot-addressor
 foo-w-ptr :structure foo :slot w)
(sb-kernel:define-structure-slot-addressor
 foo-cdf-ptr :structure foo :slot cdf)
(sb-kernel:define-structure-slot-addressor
 foo-sword-ptr :structure foo :slot sword)

(with-test (:name :define-structure-slot-addressor)
  (let* ((word (logand sb-ext:most-positive-word #xfeedbad))
         (re 4.2d58)
         (im 8.93d-10)
         (thing (make-foo :cdf (complex re im) :w word :sword -9)))
     (sb-sys:with-pinned-objects (thing)
      (assert (= word (sb-sys:sap-ref-word
                       (sb-sys:int-sap (foo-w-ptr thing)) 0)))
      (assert (= re (sb-sys:sap-ref-double
                     (sb-sys:int-sap (foo-cdf-ptr thing)) 0)))
      (assert (= im (sb-sys:sap-ref-double
                     (sb-sys:int-sap (foo-cdf-ptr thing)) 8)))
       (let* ((sap (sb-sys:int-sap (foo-sword-ptr thing)))
              (slots (sb-kernel:dd-slots (sb-kernel:find-defstruct-description 'foo)))
              (valtype (sb-kernel:dsd-raw-type
                        (find 'sword slots :key #'sb-kernel:dsd-name))))
         (assert (eq valtype 'sb-vm:signed-word))
         (assert (= -9 (if (eq valtype 'sb-vm:signed-word)
                           (sb-sys:signed-sap-ref-word sap 0)
                           (sb-sys:sap-ref-lispobj sap 0))))))))

(macrolet ((def ()
             `(defstruct foo-lotsaslots
                ,@(loop for i below 100 collect
                        `(,(sb-int:symbolicate "S" (write-to-string i))
                          0 :type ,(if (oddp i) 'sb-ext:word 't))))))
  (def))

(with-test (:name :copy-structure-bignum-bitmap)
  (assert (zerop (foo-lotsaslots-s0
                  (copy-structure (make-foo-lotsaslots))))))

(load "compiler-test-util.lisp")
(with-test (:name :copy-structure-efficient-case)
  (assert (not (ctu:find-named-callees #'copy-structure :name 'ash))))
