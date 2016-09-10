;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package :sb-introspect-test/xref)

(defmacro define-xref-test (name form result)
  `(deftest ,name
       (sort (mapcar #'first ,form) #'string< :key #'princ-to-string)
     ,(sort (copy-list result) #'string< :key #'princ-to-string)))

(define-xref-test who-calls.1
    (who-calls 'foo)
  nil)
(define-xref-test who-calls.2
    (who-calls 'bar)
  (xref/1 xref/3))

(define-xref-test who-calls.3
    (who-calls 'xref/1)
  (xref/2))

(define-xref-test who-calls.4
    (who-calls 'xref/2)
  (xref/5
   xref/6
   xref/8
   xref/8
   xref/12
   (sb-pcl::fast-method xref/10 (t t t t t t t t fixnum))
   (sb-pcl::fast-method xref/11 (fixnum))
   (sb-pcl::fast-method xref/11 ((eql z)))))

(define-xref-test who-calls.5
    (who-calls 'xref/3)
  (inline/1 (sb-pcl::fast-method xref/11 (float))))

(define-xref-test who-calls.6
    (who-calls 'xref/4)
  nil)

(define-xref-test who-calls.7
    (who-calls 'xref/5)
  nil)

(define-xref-test who-calls.8
    (who-calls 'xref/6)
  (xref/7))

(define-xref-test who-calls.9
    (who-calls 'xref/7)
  nil)

(define-xref-test who-calls.10
    (who-calls 'xref/8)
  nil)

(define-xref-test who-calls.11
    (who-calls 'xref/10)
  nil)
(define-xref-test who-calls.12
    (who-calls 'xref/11)
  nil)

(define-xref-test who-calls.13
    (who-calls 'inline/1)
  (xref/12))

(define-xref-test who-calls.14
    (who-calls 'xref/12)
  (macro/1))

(define-xref-test who-calls.15
    (who-calls 'inline/3)
  (inline/3-user/1
   inline/3-user/2
   inline/3-user/3
   inline/3-user/4))

(define-xref-test who-calls.16
    (who-calls 'inline/4)
  (inline/4-user))

(define-xref-test who-calls.17
    (who-calls 'called-by-traced-fun)
  (traced-fun))

#+sb-eval
(define-xref-test who-calls.18
    (who-calls 'called-by-interpreted-fun)
  nil)


(define-xref-test who-macroexpands.1
    (who-macroexpands 'macro/1)
  (macro-use/1
   macro-use/2
   macro-use/3
   macro-use/4
   inline/2))


(define-xref-test who-binds.1
    (who-binds '*a*)
  (xref/2))


(define-xref-test who-sets.1
    (who-sets '*a*)
  (xref/2 xref/13))

(define-xref-test who-sets.2
    (who-sets '**global**)
  (xref/16))

(define-xref-test who-references.1
    (who-references '*a*)
  (xref/1 xref/2 xref/4 inline/1 xref/14))

(define-xref-test who-references.2
    (who-references '+z+)
  (inline/1))

(define-xref-test who-references.3
    (who-references '**global**)
  (xref/15))

(define-xref-test who-calls.struct-slot.1
    (who-calls 'struct-slot)
  (source-user))

(define-xref-test who-calls.cmacro.1
    (who-calls 'cmacro)
  (source-user))


(define-xref-test who-specializes-directly.1
    (who-specializes-directly 'a-class)
  ((method a-gf-1)
   (method a-gf-2)))

(define-xref-test who-specializes-directly.2
    (who-specializes-directly 'a-structure)
  ((method a-gf-1)
   (method a-gf-2)))

(define-xref-test who-specializes-generally.1
    (who-specializes-generally 'a-class)
  ((method a-gf-1)
   (method a-gf-2)
   (method a-gf-3)))

(define-xref-test who-specializes-generally.2
    (who-specializes-generally 'a-structure)
  ((method a-gf-1)
   (method a-gf-2)
   (method a-gf-3)))
