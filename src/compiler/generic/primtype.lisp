;;;; machine-independent aspects of the object representation and
;;;; primitive types

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;;; primitive type definitions

(/show0 "primtype.lisp 17")

(!def-primitive-type t (descriptor-reg))
(/show0 "primtype.lisp 20")

;;; primitive integer types that fit in registers
(/show0 "primtype.lisp 24")
(!def-primitive-type positive-fixnum (any-reg signed-reg unsigned-reg)
  :type (unsigned-byte #.sb-vm:n-positive-fixnum-bits))
(/show0 "primtype.lisp 27")
#-(or 64-bit 64-bit-registers)
(!def-primitive-type unsigned-byte-31 (signed-reg unsigned-reg descriptor-reg)
  :type (unsigned-byte 31))
(/show0 "primtype.lisp 31")
#-(or 64-bit 64-bit-registers)
(!def-primitive-type unsigned-byte-32 (unsigned-reg descriptor-reg)
  :type (unsigned-byte 32))
(/show0 "primtype.lisp 35")
#+(or 64-bit 64-bit-registers)
(!def-primitive-type unsigned-byte-63 (signed-reg unsigned-reg descriptor-reg)
  :type (unsigned-byte 63))
#+(or 64-bit 64-bit-registers)
(!def-primitive-type unsigned-byte-64 (unsigned-reg descriptor-reg)
  :type (unsigned-byte 64))
(!def-primitive-type fixnum (any-reg signed-reg)
  :type (signed-byte #.(1+ n-positive-fixnum-bits)))
#-(or 64-bit 64-bit-registers)
(!def-primitive-type signed-byte-32 (signed-reg descriptor-reg)
  :type (signed-byte 32))
#+(or 64-bit 64-bit-registers)
(!def-primitive-type signed-byte-64 (signed-reg descriptor-reg)
  :type (signed-byte 64))

(define-load-time-global *fixnum-primitive-type* (primitive-type-or-lose 'fixnum))

(/show0 "primtype.lisp 53")
(!def-primitive-type-alias tagged-num '(:or positive-fixnum fixnum))
(multiple-value-bind (unsigned signed untagged)
    (case sb-vm:n-machine-word-bits
      (64 (values '(unsigned-byte-64 unsigned-byte-63 positive-fixnum)
                  '(signed-byte-64 fixnum unsigned-byte-63 positive-fixnum)
                  '(signed-byte-64 unsigned-byte-64 unsigned-byte-63 fixnum positive-fixnum)))
      (32 (values '(unsigned-byte-32 unsigned-byte-31 positive-fixnum)
                  '(signed-byte-32 fixnum unsigned-byte-31 positive-fixnum)
                  '(signed-byte-32 unsigned-byte-32 unsigned-byte-31 fixnum positive-fixnum))))
  (!def-primitive-type-alias unsigned-num `(:or ,@unsigned))
  (!def-primitive-type-alias signed-num `(:or ,@signed))
  (!def-primitive-type-alias untagged-num `(:or ,@untagged)))

;;; other primitive immediate types
(/show0 "primtype.lisp 68")
(!def-primitive-type character (character-reg any-reg))

;;; primitive pointer types
(/show0 "primtype.lisp 73")
(!def-primitive-type function (descriptor-reg))
(!def-primitive-type list (descriptor-reg))
(!def-primitive-type instance (descriptor-reg))

(/show0 "primtype.lisp 77")
(!def-primitive-type funcallable-instance (descriptor-reg))

;;; primitive other-pointer number types
(/show0 "primtype.lisp 81")
(!def-primitive-type integer (descriptor-reg))
(!def-primitive-type bignum (descriptor-reg))
(!def-primitive-type ratio (descriptor-reg))
(!def-primitive-type complex (descriptor-reg))
(/show0 "about to !DEF-PRIMITIVE-TYPE SINGLE-FLOAT")
(!def-primitive-type single-float (single-reg descriptor-reg))
(/show0 "about to !DEF-PRIMITIVE-TYPE DOUBLE-FLOAT")
(!def-primitive-type double-float (double-reg descriptor-reg))

(/show0 "about to !DEF-PRIMITIVE-TYPE COMPLEX-SINGLE-FLOAT")
(!def-primitive-type complex-single-float (complex-single-reg descriptor-reg)
  :type (complex single-float))
(/show0 "about to !DEF-PRIMITIVE-TYPE COMPLEX-DOUBLE-FLOAT")
(!def-primitive-type complex-double-float (complex-double-reg descriptor-reg)
  :type (complex double-float))
#+sb-simd-pack
(progn
  (/show0 "about to !DEF-PRIMITIVE-TYPE SIMD-PACK")
  (!def-primitive-type simd-pack-single (single-sse-reg descriptor-reg)
    :type (simd-pack single-float))
  (!def-primitive-type simd-pack-double (double-sse-reg descriptor-reg)
    :type (simd-pack double-float))
  (!def-primitive-type simd-pack-ub8 (int-sse-reg descriptor-reg)
    :type (simd-pack (unsigned-byte 8)))
  (!def-primitive-type simd-pack-ub16 (int-sse-reg descriptor-reg)
    :type (simd-pack (unsigned-byte 16)))
  (!def-primitive-type simd-pack-ub32 (int-sse-reg descriptor-reg)
    :type (simd-pack (unsigned-byte 32)))
  (!def-primitive-type simd-pack-ub64 (int-sse-reg descriptor-reg)
    :type (simd-pack (unsigned-byte 64)))
  (!def-primitive-type simd-pack-sb8 (int-sse-reg descriptor-reg)
    :type (simd-pack (signed-byte 8)))
  (!def-primitive-type simd-pack-sb16 (int-sse-reg descriptor-reg)
    :type (simd-pack (signed-byte 16)))
  (!def-primitive-type simd-pack-sb32 (int-sse-reg descriptor-reg)
    :type (simd-pack (signed-byte 32)))
  (!def-primitive-type simd-pack-sb64 (int-sse-reg descriptor-reg)
    :type (simd-pack (signed-byte 64)))
  (!def-primitive-type-alias simd-pack
   '(:or simd-pack-single
         simd-pack-double
         simd-pack-ub8
         simd-pack-ub16
         simd-pack-ub32
         simd-pack-ub64
         simd-pack-sb8
         simd-pack-sb16
         simd-pack-sb32
         simd-pack-sb64)))
#+sb-simd-pack-256
(progn
  (!def-primitive-type simd-pack-256-single (single-avx2-reg descriptor-reg)
    :type (simd-pack-256 single-float))
  (!def-primitive-type simd-pack-256-double (double-avx2-reg descriptor-reg)
    :type (simd-pack-256 double-float))
  (!def-primitive-type simd-pack-256-ub8 (int-avx2-reg descriptor-reg)
    :type (simd-pack-256 (unsigned-byte 8)))
  (!def-primitive-type simd-pack-256-ub16 (int-avx2-reg descriptor-reg)
    :type (simd-pack-256 (unsigned-byte 16)))
  (!def-primitive-type simd-pack-256-ub32 (int-avx2-reg descriptor-reg)
    :type (simd-pack-256 (unsigned-byte 32)))
  (!def-primitive-type simd-pack-256-ub64 (int-avx2-reg descriptor-reg)
    :type (simd-pack-256 (unsigned-byte 64)))
  (!def-primitive-type simd-pack-256-sb8 (int-avx2-reg descriptor-reg)
    :type (simd-pack-256 (signed-byte 8)))
  (!def-primitive-type simd-pack-256-sb16 (int-avx2-reg descriptor-reg)
    :type (simd-pack-256 (signed-byte 16)))
  (!def-primitive-type simd-pack-256-sb32 (int-avx2-reg descriptor-reg)
    :type (simd-pack-256 (signed-byte 32)))
  (!def-primitive-type simd-pack-256-sb64 (int-avx2-reg descriptor-reg)
    :type (simd-pack-256 (signed-byte 64)))
  (!def-primitive-type-alias simd-pack-256
   '(:or simd-pack-256-single
         simd-pack-256-double
         simd-pack-256-ub8
         simd-pack-256-ub16
         simd-pack-256-ub32
         simd-pack-256-ub64
         simd-pack-256-sb8
         simd-pack-256-sb16
         simd-pack-256-sb32
         simd-pack-256-sb64)))

;;; primitive other-pointer array types
(/show0 "primtype.lisp 96")
(macrolet ((define-simple-array-primitive-types ()
               `(progn
                 ,@(map 'list
                        (lambda (saetp)
                          `(!def-primitive-type
                            ,(saetp-primitive-type-name saetp)
                            (descriptor-reg)
                            :type (simple-array ,(saetp-specifier saetp) (*))))
                        *specialized-array-element-type-properties*))))
  (define-simple-array-primitive-types))
;;; Note: The complex array types are not included, 'cause it is
;;; pointless to restrict VOPs to them.

;;; other primitive other-pointer types
(!def-primitive-type system-area-pointer (sap-reg descriptor-reg))
(!def-primitive-type weak-pointer (descriptor-reg))

;;; miscellaneous primitive types that don't exist at the LISP level
(!def-primitive-type catch-block (catch-block) :type nil)
(!def-primitive-type unwind-block (unwind-block) :type nil)

;;;; PRIMITIVE-TYPE-OF and friends

;;; Return the most restrictive primitive type that contains OBJECT.
(/show0 "primtype.lisp 147")
(defun primitive-type-of (object)
  (if (null object)
      (load-time-value (primitive-type-or-lose 'list) t)
      (let ((type (ctype-of object)))
        (if (member-type-p type)
            *backend-t-primitive-type*
            (primitive-type type)))))

#+sb-simd-pack
(defun simd-pack-mask->tag (mask)
  (aver (= (logcount mask) 1))
  (the (integer 0 (#.(length +simd-pack-element-types+)))
       (1- (integer-length mask))))

;;; Return the primitive type corresponding to a type descriptor
;;; structure. The second value is true when the primitive type is
;;; exactly equivalent to the argument Lisp type.
;;;
;;; In a bootstrapping situation, we should be careful to use the
;;; correct values for the system parameters.
;;;
;;; Meta: the following comment is not true. Should remove the AUX fn.
;;; We need an aux function because we need to use both
;;; !DEF-VM-SUPPORT-ROUTINE and DEFUN-CACHED.
(/show0 "primtype.lisp 188")
(defun primitive-type (type)
  (sb-kernel::maybe-reparse-specifier! type)
  (primitive-type-aux type))
(/show0 "primtype.lisp 191")
(defun-cached (primitive-type-aux
               :hash-function #'sb-kernel::type-%bits
               :hash-bits 9
               :values 2)
              ((type eq))
  (declare (type ctype type))
  (macrolet ((any () '(values *backend-t-primitive-type* nil))
             (exactly (type)
               `(values (primitive-type-or-lose ',type) t))
             (part-of (type)
               `(values (primitive-type-or-lose ',type) nil)))
    (flet ((maybe-numeric-type-union (t1 t2)
             (let ((t1-name (primitive-type-name t1))
                   (t2-name (primitive-type-name t2)))
               (case t1-name
                 (positive-fixnum
                  (if (or (eq t2-name 'fixnum)
                          (eq t2-name
                              (ecase n-machine-word-bits
                                (32 'signed-byte-32)
                                (64 'signed-byte-64)))
                          (eq t2-name
                              (ecase n-machine-word-bits
                                (32 'unsigned-byte-31)
                                (64 'unsigned-byte-63)))
                          (eq t2-name
                              (ecase n-machine-word-bits
                                (32 'unsigned-byte-32)
                                (64 'unsigned-byte-64))))
                      t2))
                 (fixnum
                  (case t2-name
                    (#.(ecase n-machine-word-bits
                         (32 'signed-byte-32)
                         (64 'signed-byte-64))
                       t2)
                    (#.(ecase n-machine-word-bits
                         (32 'unsigned-byte-31)
                         (64 'unsigned-byte-63))
                       (primitive-type-or-lose
                        (ecase n-machine-word-bits
                          (32 'signed-byte-32)
                          (64 'signed-byte-64))))))
                 (#.(ecase n-machine-word-bits
                      (32 'signed-byte-32)
                      (64 'signed-byte-64))
                  (if (eq t2-name
                          (ecase n-machine-word-bits
                            (32 'unsigned-byte-31)
                            (64 'unsigned-byte-63)))
                      t1))
                 (#.(ecase n-machine-word-bits
                      (32 'unsigned-byte-31)
                      (64 'unsigned-byte-63))
                    (if (eq t2-name
                            (ecase n-machine-word-bits
                              (32 'unsigned-byte-32)
                              (64 'unsigned-byte-64)))
                        t2))
                 ((bignum integer)
                  (cond ((and (eq t1-name 'bignum)
                              (eq t2-name 'bignum))
                         t1)
                        ((memq t2-name '(positive-fixnum fixnum
                                         integer bignum
                                         . #.(ecase n-machine-word-bits
                                               (32 '(unsigned-byte-31 unsigned-byte-32 signed-byte-32))
                                               (64 '(unsigned-byte-63 unsigned-byte-64 signed-byte-64)))))
                         (primitive-type-or-lose 'integer))))))))
      (etypecase type
        (numeric-type
         (let ((lo (numeric-type-low type))
               (hi (numeric-type-high type)))
           (case (numeric-type-complexp type)
             (:real
              (case (numeric-type-class type)
                (integer
                 (cond ((and hi lo)
                        (dolist (spec
                                  `((positive-fixnum 0 ,most-positive-fixnum)
                                    ,@(ecase n-machine-word-bits
                                        (32
                                         `((unsigned-byte-31
                                            0 ,(1- (ash 1 31)))
                                           (unsigned-byte-32
                                            0 ,(1- (ash 1 32)))))
                                        (64
                                         `((unsigned-byte-63
                                            0 ,(1- (ash 1 63)))
                                           (unsigned-byte-64
                                            0 ,(1- (ash 1 64))))))
                                    (fixnum ,most-negative-fixnum
                                            ,most-positive-fixnum)
                                    ,(ecase n-machine-word-bits
                                       (32
                                        `(signed-byte-32 ,(ash -1 31)
                                                         ,(1- (ash 1 31))))
                                       (64
                                        `(signed-byte-64 ,(ash -1 63)
                                                         ,(1- (ash 1 63))))))
                                 (if (or (< hi most-negative-fixnum)
                                         (> lo most-positive-fixnum))
                                     (part-of bignum)
                                     (part-of integer)))
                          (let ((type (car spec))
                                (min (cadr spec))
                                (max (caddr spec)))
                            (when (<= min lo hi max)
                              (return (values
                                       (primitive-type-or-lose type)
                                       (and (= lo min) (= hi max))))))))
                       ((or (and hi (< hi most-negative-fixnum))
                            (and lo (> lo most-positive-fixnum)))
                        (part-of bignum))
                       (t
                        (part-of integer))))
                (float
                 (let ((exact (and (null lo) (null hi))))
                   (case (numeric-type-format type)
                     ((short-float single-float)
                      (values (primitive-type-or-lose 'single-float)
                              exact))
                     ((double-float)
                      (values (primitive-type-or-lose 'double-float)
                              exact))
                     (t
                      (any)))))
                (t
                 (any))))
             (:complex
              (if (eq (numeric-type-class type) 'float)
                  (let ((exact (and (null lo) (null hi))))
                    (case (numeric-type-format type)
                      ((short-float single-float)
                       (values (primitive-type-or-lose 'complex-single-float)
                               exact))
                      ((double-float long-float)
                       (values (primitive-type-or-lose 'complex-double-float)
                               exact))
                      (t
                       (part-of complex))))
                  (part-of complex)))
             (t
              (any)))))
        (array-type
         (if (or (array-type-complexp type)
                 (not (singleton-p (array-type-dimensions type))))
             (any)
             ;; EQ is ok to compare by because all CTYPEs representing
             ;; array specializations are interned objects.
             (let ((saetp (find (array-type-specialized-element-type type)
                                *specialized-array-element-type-properties*
                                :key #'saetp-ctype :test #'eq)))
               (if saetp
                   (values (primitive-type-or-lose
                            (saetp-primitive-type-name saetp))
                           (eq (first (array-type-dimensions type)) '*))
                   (any)))))
        ((or union-type numeric-union-type)
         (if (type= type (specifier-type 'list))
             (exactly list)
             (let ((types (sb-kernel::flatten-numeric-union-types type)))
               (multiple-value-bind (res exact) (primitive-type (first types))
                 (dolist (type (rest types) (values res exact))
                   (multiple-value-bind (ptype ptype-exact)
                       (primitive-type type)
                     (unless ptype-exact (setq exact nil))
                     (unless (eq ptype res)
                       (let ((new-ptype
                              (or (maybe-numeric-type-union res ptype)
                                  (maybe-numeric-type-union ptype res))))
                         (if new-ptype
                             (setq res new-ptype)
                             (return (any)))))))))))
        (intersection-type
         (let ((types (intersection-type-types type))
               (res (any)))
           ;; why NIL for the exact?  Well, we assume that the
           ;; intersection type is in fact doing something for us:
           ;; that is, that each of the types in the intersection is
           ;; in fact cutting off some of the type lattice.  Since no
           ;; intersection type is represented by a primitive type and
           ;; primitive types are mutually exclusive, it follows that
           ;; no intersection type can represent the entirety of the
           ;; primitive type.  (And NIL is the conservative answer,
           ;; anyway).  -- CSR, 2006-09-14
           (dolist (type types (values res nil))
             (when (csubtypep type (specifier-type 'function))
               ;; Things like (AND STANDARD-OBJECT FUNCTION) are callable as functions.
               (part-of function))
             (multiple-value-bind (ptype)
                 (primitive-type type)
               (cond
                 ;; if the result so far is (any), any improvement on
                 ;; the specificity of the primitive type is valid.
                 ((eq res (any))
                  (setq res ptype))
                 ;; if the primitive type returned is (any), the
                 ;; result so far is valid.  Likewise, if the
                 ;; primitive type is the same as the result so far,
                 ;; everything is fine.
                 ((or (eq ptype (any)) (eq ptype res)))
                 ;; otherwise, we have something hairy and confusing,
                 ;; such as (and condition funcallable-instance).
                 ;; Punt.
                 (t (return (any))))))))
        (member-type
         (let (res)
           (block nil
             (mapc-member-type-members
              (lambda (member)
                (let ((ptype (primitive-type-of member)))
                  (if res
                      (unless (eq ptype res)
                        (let ((new-ptype (or (maybe-numeric-type-union res ptype)
                                             (maybe-numeric-type-union ptype res))))
                          (if new-ptype
                              (setq res new-ptype)
                              (return (any)))))
                      (setf res ptype))))
              type)
             res)))
        (named-type
         (ecase (named-type-name type)
           ((t *) (values *backend-t-primitive-type* t))
           ((instance) (exactly instance))
           ((funcallable-instance) (part-of function))
           ((extended-sequence) (any))
           ((nil) (any))))
        (character-set-type
         (if (eq type (specifier-type 'character))
             (exactly character)
             (part-of character)))
        #+sb-simd-pack
        (simd-pack-type
         (let ((mask (simd-pack-type-tag-mask type)))
           (if (= (logcount mask) 1)
               (values (primitive-type-or-lose
                        (svref +simd-pack-128-primtypes+ (simd-pack-mask->tag mask)))
                       t)
               (any))))
        #+sb-simd-pack-256
        (simd-pack-256-type
         (let ((mask (simd-pack-256-type-tag-mask type)))
           (if (= (logcount mask) 1)
               (values (primitive-type-or-lose
                        (svref +simd-pack-256-primtypes+ (simd-pack-mask->tag mask)))
                       t)
               (any))))
        (cons-type
         (part-of list))
        (built-in-classoid
         (case (classoid-name type)
           ((complex function system-area-pointer weak-pointer)
            (values (primitive-type-or-lose (classoid-name type)) t))
           ((pathname logical-pathname)
            (part-of instance))
           (t
            (any))))
        (fun-designator-type
         (any))
        (fun-type
         (exactly function))
        (classoid
         (if (csubtypep type (specifier-type 'function))
             (part-of function)
             (part-of instance)))
        (ctype
         (if (csubtypep type (specifier-type 'function))
             (part-of function)
             (any)))))))

(/show0 "primtype.lisp end of file")
