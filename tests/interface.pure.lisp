;;;; tests for problems in the interface presented to the user/programmer

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


;;;; properties of symbols, e.g. presence of doc strings for public symbols

(enable-test-parallelism)

(with-test (:name (documentation :cl) :skipped-on (:not :sb-doc))
  (let ((n 0))
    (do-symbols (s 'cl)
      (if (fboundp s)
          (when (documentation s 'function)
            (incf n))))
    (assert (= n 597))))

;;;; tests of interface machinery

(with-test (:name :defthingy-fail-early)
  (dolist (form '((defun "hi" 3)
                  (defconstant "hi" 3)
                  (defvar "hi" 3)
                  (define-modify-macro "hi" :operator 'wat)))
    (multiple-value-bind (exp err) (ignore-errors (macroexpand-1 form))
      (assert (and (not exp)
                   (search "is not"
                           (write-to-string err :escape nil)))))))

;; Regression from R/O space change
(with-test (:name (apropos-list :smoke-test))
  (assert (plusp (length (apropos-list "required-foreign")))))

;;; APROPOS should accept a package designator, not just a package, and
;;; furthermore do the right thing when it gets a package designator.
;;; (bug reported and fixed by Alexey Dejneka sbcl-devel 2001-10-17)
(with-test (:name (apropos-list :package-designator))
  (assert (< 0
             (length (apropos-list "PRINT" :cl))
             (length (apropos-list "PRINT")))))
;;; Further, it should correctly deal with the external-only flag (bug
;;; reported by cliini on #lisp IRC 2003-05-30, fixed in sbcl-0.8.0.1x
;;; by CSR)
(with-test (:name (apropos-list :external-only))
  (assert (= (length (apropos-list "" "CL"))
             (length (apropos-list "" "CL" t))))
  (assert (< 0
             (length (apropos-list "" "SB-VM" t))
             (length (apropos-list "" "SB-VM")))))

(with-test (:name :apropos-symbol-values)
  (let ((string
         (with-output-to-string (*standard-output*)
           (apropos "*print-"))))
    (assert (search "=" string))
    (assert (search "PPRINT-DISPATCH" string))))

;;; TYPEP, SUBTYPEP, UPGRADED-ARRAY-ELEMENT-TYPE and
;;; UPGRADED-COMPLEX-PART-TYPE should be able to deal with NIL as an
;;; environment argument
(with-test (:name (typep :environment nil))
  (typep 1 'fixnum nil))

(with-test (:name (subtypep :environment nil))
  (subtypep 'fixnum 'integer nil))

(with-test (:name (upgraded-array-element-type :environment nil))
  (upgraded-array-element-type '(mod 5) nil))

(with-test (:name (upgraded-complex-part-type :environment nil))
  (upgraded-complex-part-type '(single-float 0.0 1.0) nil))

#+sb-doc
(with-test (:name (documentation :sb-ext))
  ;; We should have documentation for our extension package:
  (assert (documentation (find-package "SB-EXT") t)))

;;; DECLARE should not be a special operator
(with-test (:name (declare :not special-operator-p))
  (assert (not (special-operator-p 'declare))))

;;; DOCUMENTATION should return nil, not signal slot-unbound
(with-test (:name (documentation :return nil))
  (flet ((test (thing doc-type)
           (assert (eq nil (documentation thing doc-type)))))
    (test 'fixnum 'type)
    (test 'class 'type)
    (test (find-class 'class) 'type)
    (test 'foo 'structure)))

;;; DECODE-UNIVERSAL-TIME should accept second-resolution time-zones.
(with-test (:name (decode-universal-time :second-resolution :time-zone))
  (macrolet ((test (ut time-zone list)
               (destructuring-bind (sec min hr date mon yr day tz)
                   list
                 `(multiple-value-bind (sec min hr date mon yr day dst tz)
                      (decode-universal-time ,ut ,time-zone)
                    (declare (ignore dst))
                    (assert (= sec ,sec))
                    (assert (= min ,min))
                    (assert (= hr ,hr))
                    (assert (= date ,date))
                    (assert (= mon ,mon))
                    (assert (= yr ,yr))
                    (assert (= day ,day))
                    (assert (= tz ,tz))))))
    (test (* 86400 365) -1/3600 (1 0 0 1 1 1901 1 -1/3600))
    (test (* 86400 365) 0 (0 0 0 1 1 1901 1 0))
    (test (* 86400 365) 1/3600 (59 59 23 31 12 1900 0 1/3600))))

;;; DECODE-UNIVERSAL-TIME shouldn't fail when the time is outside UNIX
;;; 32-bit time_t and a timezone wasn't passed
(with-test (:name (decode-universal-time :decode 0))
  (decode-universal-time 0 nil))

;;; ENCODE-UNIVERSAL-TIME should be able to encode the universal time
;;; 0 when passed a representation in a timezone where the
;;; representation of 0 as a decoded time is in 1899.
(with-test (:name (encode-universal-time :encode 0))
  (encode-universal-time 0 0 23 31 12 1899 1))

;;; DISASSEMBLE shouldn't fail on purified functions
(with-test (:name (disassemble :purified))
  (disassemble 'cl:+ :stream (make-broadcast-stream))
  (disassemble 'sb-ext:run-program :stream (make-broadcast-stream)))

;;; minimal test of GC: see stress-gc.{sh,lisp} for a more
;;; comprehensive test.
(with-test (:name (sb-ext:gc :minimal :stress))
  (loop repeat 2
     do (checked-compile '(lambda (x) x))
     do (sb-ext:gc :full t)))

;;; On x86-64, the instruction definitions for CMP*[PS][SD] were broken
;;; so that the disassembler threw an error when they were used with
;;; one operand in memory.
(with-test (:name (disassemble :bug-814702))
  ;; Quote the lambdas, because WITH-TEST produces a hairy lexical environment
  ;; which make an interpreted lambda uncompilable.
  (disassemble '(lambda (x)
                 (= #C(2.0f0 3.0f0)
                    (the (complex single-float) x)))
               :stream (make-broadcast-stream))
  (disassemble '(lambda (x y)
                 (= (the (complex single-float) x)
                    (the (complex single-float) y)))
               :stream (make-broadcast-stream)))

;;; Data in the high bits of a fun header caused CODE-N-UNBOXED-DATA-BYTES
;;; to return a ridiculously huge value.
(with-test (:name (disassemble :unboxed-data))
  (assert (< (sb-kernel:code-n-unboxed-data-bytes
              (sb-kernel:fun-code-header #'expt))
             150))) ; The exact value is irrelevant.

#+x86-64
;; The labeler for LEA would choke on an illegal encoding
;; instead of showing what it illegally encodes, such as LEA RAX, RSP
(with-test (:name (disassemble :x86-lea :illegal-op))
  (let ((a (coerce '(#x48 #x8D #xC4) '(array (unsigned-byte 8) (3)))))
    (sb-sys:with-pinned-objects (a)
      (sb-disassem::disassemble-memory (sb-sys:sap-int (sb-sys:vector-sap a)) 3
                                       :stream (make-broadcast-stream)))))

;; Assert that disassemblies of identically-acting functions are identical
;; if address printing is turned off. Should work on any backend, I think.
(with-test (:name (disassemble :without-addresses))
  (flet ((disassembly-text (lambda-expr)
           (let ((string
                   (let ((sb-disassem::*disassem-location-column-width* 0)
                         (sb-ext:*disassemble-annotate* nil)
                         (*print-pretty* nil)) ; prevent function name wraparound
                     (with-output-to-string (s)
                       (disassemble lambda-expr :stream s)))))
             ;; Return all except the first two lines. This is subject to change
             ;; any time we muck with the layout unfortunately.
             (subseq string
                     (1+ (position #\Newline string
                                   :start (1+ (position #\Newline string))))))))
    (let ((string1 (disassembly-text '(lambda (x) (car x))))
          (string2 (disassembly-text '(lambda (y) (car y)))))
      (assert (string= string1 string2)))))

(with-test (:name :disassemble-assembly-routine)
  (disassemble sb-fasl:*assembler-routines* :stream (make-broadcast-stream)))

(with-test (:name (sb-ext:assert-version->= :ok))
  (sb-ext:assert-version->= 1 1 13))

(with-test (:name (sb-ext:assert-version->= :fails))
  (assert-error
   (sb-ext:assert-version->= most-positive-fixnum)))

(with-test (:name :bug-1095483)
  (assert-error (fboundp '(cas "foo"))))

(with-test (:name (time :no *print-length* :abbreviation))
  (let ((s (make-string-output-stream)))
    (let ((*trace-output* s))
      (time (progn)))
    (let ((str (get-output-stream-string s)))
      (assert (and (>= (count #\newline str) 4)
                   (search "bytes consed" str))))))

(with-test (:name :split-seconds-for-sleep)
  (assert (< (nth-value 1 (sb-impl::split-seconds-for-sleep 7.2993028420866d7))
             1000000000)))

#+(or x86-64 arm64)
(with-test (:name :restart-invalid-arg-counts.1)
  (handler-bind ((error (lambda (c)
                          (invoke-restart (find-restart 'sb-kernel::replace-function c) 'list))))
    (assert (equal (eval '(cons 324)) '(324)))))

#+(or x86-64 arm64)
(with-test (:name :restart-invalid-arg-counts.2)
  (handler-bind ((error (lambda (c)
                          (invoke-restart (find-restart 'sb-kernel::call-form c) 123))))
    (assert (= (eval '(cons 1)) 123))))

(with-test (:name :restart-bogus-arg-to-values-list-error
                  :broken-on (not (or :x86 :x86-64 :arm :arm64)))
  (let ((fun (checked-compile `(lambda (x) (values-list x)))))
    (assert (equal (handler-bind ((sb-kernel::values-list-argument-error
                                   #'continue))
                     (multiple-value-list
                      (funcall fun '(1 2 3 4 5 6 7 8 . 10))))
                   '(1 2 3 4 5 6 7 8)))))

(with-test (:name :compile--install-named-definition)
  ;; (compiled-function-p #'make-load-form) => NIL because there could be
  ;; interpreted MAKE-LOAD-FORM methods which COMPILE does not compile,
  ;; though of course in the baseline image there aren't any.
  (compile 'blech #'make-load-form)
  ;; FUNCALL just avoids a style-warning on unknown names.
  (assert (eq (car (funcall 'blech #p"foo.bar")) 'sb-impl::intern-pathname))
  (compile 'flem (compile nil '(lambda (x) (+ x 3))))
  (assert (= (funcall 'flem 9) 12))
  (compile 'flem (compile nil '(lambda (x) (* x 3))))
  (assert (= (funcall 'flem 9) 27)))

(with-test (:name (documentation (not sb-impl::legal-fun-name-p)))
  (assert (typep (documentation '(xxx yyy) 'function)
                 '(or null string))))
