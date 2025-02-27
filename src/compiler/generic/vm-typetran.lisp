;;;; This file contains the implementation specific type
;;;; transformation magic. Basically, the various non-standard
;;;; predicates that can be used in TYPEP transformations.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;;; internal predicates

;;; These type predicates are used to implement simple cases of TYPEP.
;;; They shouldn't be used explicitly.
(define-type-predicate base-string-p base-string)
(define-type-predicate bignump bignum)
#+sb-unicode (define-type-predicate character-string-p (vector character))
(define-type-predicate complex-double-float-p (complex double-float))
(define-type-predicate complex-single-float-p (complex single-float))
#+long-float
(define-type-predicate complex-long-float-p (complex long-float))
;;; (COMPLEX-VECTOR-P isn't here because it's not so much a Lisp-level
;;; type predicate as just a hack to get at the type code so that we
;;; can implement some primitive stuff in Lisp.)
(define-type-predicate double-float-p double-float)
(define-type-predicate fixnump fixnum)
#+long-float
(define-type-predicate long-float-p long-float)
(define-type-predicate ratiop ratio)
(define-type-predicate single-float-p single-float)
(define-type-predicate simple-array-p simple-array)
(define-type-predicate simple-array-nil-p (simple-array nil (*)))
(define-type-predicate simple-array-unsigned-byte-2-p
                       (simple-array (unsigned-byte 2) (*)))
(define-type-predicate simple-array-unsigned-byte-4-p
                       (simple-array (unsigned-byte 4) (*)))
(define-type-predicate simple-array-unsigned-byte-7-p
                       (simple-array (unsigned-byte 7) (*)))
(define-type-predicate simple-array-unsigned-byte-8-p
                       (simple-array (unsigned-byte 8) (*)))
(define-type-predicate simple-array-unsigned-byte-15-p
                       (simple-array (unsigned-byte 15) (*)))
(define-type-predicate simple-array-unsigned-byte-16-p
                       (simple-array (unsigned-byte 16) (*)))

(define-type-predicate simple-array-unsigned-fixnum-p
    (simple-array
     (unsigned-byte #.sb-vm:n-positive-fixnum-bits) (*)))

(define-type-predicate simple-array-unsigned-byte-31-p
    (simple-array (unsigned-byte 31) (*)))
(define-type-predicate simple-array-unsigned-byte-32-p
                       (simple-array (unsigned-byte 32) (*)))

#+64-bit
(define-type-predicate simple-array-unsigned-byte-63-p
                       (simple-array (unsigned-byte 63) (*)))
#+64-bit
(define-type-predicate simple-array-unsigned-byte-64-p
                       (simple-array (unsigned-byte 64) (*)))
(define-type-predicate simple-array-signed-byte-8-p
                       (simple-array (signed-byte 8) (*)))
(define-type-predicate simple-array-signed-byte-16-p
                       (simple-array (signed-byte 16) (*)))

(define-type-predicate simple-array-fixnum-p
    (simple-array (signed-byte #.sb-vm:n-fixnum-bits)
                  (*)))

(define-type-predicate simple-array-signed-byte-32-p
    (simple-array (signed-byte 32) (*)))

#+64-bit
(define-type-predicate simple-array-signed-byte-64-p
                       (simple-array (signed-byte 64) (*)))
(define-type-predicate simple-array-single-float-p
                       (simple-array single-float (*)))
(define-type-predicate simple-array-double-float-p
                       (simple-array double-float (*)))
#+long-float
(define-type-predicate simple-array-long-float-p
                       (simple-array long-float (*)))
(define-type-predicate simple-array-complex-single-float-p
                       (simple-array (complex single-float) (*)))
(define-type-predicate simple-array-complex-double-float-p
                       (simple-array (complex double-float) (*)))
#+long-float
(define-type-predicate simple-array-complex-long-float-p
                       (simple-array (complex long-float) (*)))
(define-type-predicate simple-base-string-p simple-base-string)
#+sb-unicode (define-type-predicate simple-character-string-p
                  (simple-array character (*)))
(define-type-predicate simple-rank-1-array-*-p (simple-array * (*)))

(define-type-predicate system-area-pointer-p system-area-pointer)

(when-vop-existsp (:translate signed-byte-8-p)
  (define-type-predicate signed-byte-8-p (signed-byte 8)))
(when-vop-existsp (:translate signed-byte-16-p)
  (define-type-predicate signed-byte-16-p (signed-byte 16)))

#-64-bit
(define-type-predicate unsigned-byte-32-p (unsigned-byte 32))
(when-vop-existsp (:translate signed-byte-32-p)
  (define-type-predicate signed-byte-32-p (signed-byte 32)))
#+64-bit
(define-type-predicate unsigned-byte-64-p (unsigned-byte 64))
#+64-bit
(define-type-predicate signed-byte-64-p (signed-byte 64))
#+sb-simd-pack
(define-type-predicate simd-pack-p simd-pack)
#+sb-simd-pack-256
(define-type-predicate simd-pack-256-p simd-pack-256)
(define-type-predicate weak-pointer-p weak-pointer)
(define-type-predicate code-component-p code-component)
#-(or x86 x86-64 arm64) (define-type-predicate lra-p lra)
(define-type-predicate fdefn-p fdefn)
;;; Unlike the un-%'ed versions, these are true type predicates,
;;; accepting any type object.
(define-type-predicate %standard-char-p standard-char)
(define-type-predicate non-null-symbol-p (and symbol (not null)))
(define-type-predicate string-designator-p string-designator)


(defglobal *backend-type-predicates-grouped*
  (let ((classes (make-array (count-if #'identity sb-kernel::*type-classes*)
                             :initial-element nil)))
    (dolist (cell *backend-type-predicates*)
      (let ((index (sb-kernel::type-class-id (car cell))))
        (push cell (aref classes index))))
    (dotimes (i (length classes) classes)
      (let ((elements (nreverse (aref classes i))))
        (setf (aref classes i)
              (when elements
                (let ((types (coerce (mapcar 'car elements) 'vector))
                      (preds (coerce (mapcar 'cdr elements) 'vector)))
                  (cons types preds))))))))
(declaim (simple-vector *backend-type-predicates-grouped*))

(defun backend-type-predicate (type)
  #-sb-xc-host
  (declare (optimize (insert-array-bounds-checks 0)))
  (let* ((choices (aref *backend-type-predicates-grouped*
                        (sb-kernel::type-class-id type)))
         (ctypes (car choices)))
    ;; Other than array types, there is no way to make a type
    ;; which is TYPE= to one of the choices but not EQ to it.
    ;; But often with array types, EQ works, so always try it first.
    (dotimes (i (length ctypes))
      (when (eq type (aref ctypes i))
        (return-from backend-type-predicate (aref (cdr choices) i))))
    ;; "if at first you don't succeed ..."
    (when (array-type-p type)
      (dotimes (i (length ctypes))
        (when (type= type (aref ctypes i))
          (return (aref (cdr choices) i)))))))

(defglobal *backend-union-type-predicates*
    (let ((unions (sort
                   (loop for (type . pred) in *backend-type-predicates*
                         when (union-type-p type)
                         collect (cons type pred))
                   #'>
                   :key (lambda (x)
                          (length (union-type-types (car x)))))))
      (coerce (loop for (key . value) in unions
                    collect key
                    collect value)
              'vector)))
(declaim (simple-vector *backend-union-type-predicates*))

(defun split-union-type-tests (types)
  (let ((predicates *backend-union-type-predicates*))
    (loop for x below (length predicates) by 2
          for union-types = (union-type-types (aref predicates x))
          when (subsetp union-types types :test #'type=)
          return (values (aref predicates (1+ x))
                         (remove-if
                          (lambda (x) (member x union-types :test #'type=))
                          types)))))

(unless-vop-existsp (:translate keywordp)
(define-source-transform keywordp (x)
  `(let ((object ,x))
     (and (non-null-symbol-p object)
          (= (symbol-package-id object) ,sb-impl::+package-id-keyword+)))))
