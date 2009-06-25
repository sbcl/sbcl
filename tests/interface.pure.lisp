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

(in-package :cl-user)

;;;; properties of symbols, e.g. presence of doc strings for public symbols

;;; FIXME: It would probably be good to require here that every
;;; external symbol either has a doc string or has some good excuse
;;; (like being an accessor for a structure which has a doc string).

;;;; tests of interface machinery

;;; APROPOS should accept a package designator, not just a package, and
;;; furthermore do the right thing when it gets a package designator.
;;; (bug reported and fixed by Alexey Dejneka sbcl-devel 2001-10-17)
(assert (< 0
           (length (apropos-list "PRINT" :cl))
           (length (apropos-list "PRINT"))))
;;; Further, it should correctly deal with the external-only flag (bug
;;; reported by cliini on #lisp IRC 2003-05-30, fixed in sbcl-0.8.0.1x
;;; by CSR)
(assert (= (length (apropos-list "" "CL"))
           (length (apropos-list "" "CL" t))))
(assert (< 0
           (length (apropos-list "" "SB-VM" t))
           (length (apropos-list "" "SB-VM"))))

;;; DESCRIBE shouldn't fail on rank-0 arrays (bug reported and fixed
;;; by Lutz Euler sbcl-devel 2002-12-03)
(describe #0a0)
(describe #(1 2 3))
(describe #2a((1 2) (3 4)))

;;; TYPEP, SUBTYPEP, UPGRADED-ARRAY-ELEMENT-TYPE and
;;; UPGRADED-COMPLEX-PART-TYPE should be able to deal with NIL as an
;;; environment argument
(typep 1 'fixnum nil)
(subtypep 'fixnum 'integer nil)
(upgraded-array-element-type '(mod 5) nil)
(upgraded-complex-part-type '(single-float 0.0 1.0) nil)

;;; We should have documentation for our extension package:
(assert (documentation (find-package "SB-EXT") t))

;;; DECLARE should not be a special operator
(assert (not (special-operator-p 'declare)))

;;; WITH-TIMEOUT should accept more than one form in its body.
(with-test (:name :with-timeout-forms)
  (handler-bind ((sb-ext:timeout #'continue))
    (sb-ext:with-timeout 3
      (sleep 2)
      (sleep 2))))

;;; SLEEP should work with large integers as well -- no timers
;;; on win32, so don't test there.
#-win32
(with-test (:name (sleep pretty-much-forever))
  (assert (eq :timeout
              (handler-case
                  (sb-ext:with-timeout 1
                    (sleep (ash 1 (* 2 sb-vm:n-word-bits))))
                (sb-ext:timeout ()
                  :timeout)))))

;;; DOCUMENTATION should return nil, not signal slot-unbound
(documentation 'fixnum 'type)
(documentation 'class 'type)
(documentation (find-class 'class) 'type)
(documentation 'foo 'structure)

;;; DECODE-UNIVERSAL-TIME should accept second-resolution time-zones.
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
  (test (* 86400 365) 1/3600 (59 59 23 31 12 1900 0 1/3600)))

;;; DECODE-UNIVERSAL-TIME shouldn't fail when the time is outside UNIX
;;; 32-bit time_t and a timezone wasn't passed
(decode-universal-time 0 nil)

;;; ENCODE-UNIVERSAL-TIME should be able to encode the universal time
;;; 0 when passed a representation in a timezone where the
;;; representation of 0 as a decoded time is in 1899.
(encode-universal-time 0 0 23 31 12 1899 1)

;;; DISASSEMBLE shouldn't fail on purified functions
(disassemble 'cl:+)
(disassemble 'sb-ext:run-program)

;;; minimal test of GC: see stress-gc.{sh,lisp} for a more
;;; comprehensive test.
(loop repeat 2
      do (compile nil '(lambda (x) x))
      do (sb-ext:gc :full t))