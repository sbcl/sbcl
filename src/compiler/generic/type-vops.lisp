;;;; generic type testing and checking VOPs

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.
(in-package "SB-VM")


(defconstant-eqx +immediate-types+
  `(,unbound-marker-widetag ,character-widetag #+64-bit ,single-float-widetag)
  #'equal)

;; Given a list of widetags in HEADERS, compress into a minimal list of ranges
;; and/or singletons that should be tested.
;; FIXME: At present the "is it effectively a one-sided test" is re-implemented
;;        in an ad-hoc way by each backend. The range convention should be
;;        changed to indicate explicitly when either limit needn't be checked.
;;        (Use NIL or * as a bound perhaps)
(eval-when (:compile-toplevel :load-toplevel :execute)

(defun canonicalize-widetags (headers)
  (collect ((results))
    (let ((start nil)
          (prev nil)
          (delta widetag-spacing))
      (flet ((emit-test ()
               (results (if (= start prev)
                            start
                            (cons start prev)))))
        ;; COPY-LIST because the argument may come from immutable source code
        (dolist (header (sort (copy-list headers) #'<))
          (cond ((null start)
                 (setf start header)
                 (setf prev header))
                ((= header (+ prev delta))
                 (setf prev header))
                (t
                 (emit-test)
                 (setf start header)
                 (setf prev header))))
        (emit-test)))
    (results)))

;; If WIDETAGS is comprised of two ranges that are nearly adjacent,
;; return a single range spanning both original ranges,
;; and as a second value the widetag(s) to exclude;
;; or return the unmodified ranges and NIL.
;; This could be generalized: three ranges that collapse to one with at most
;; two exceptions, or three collapsing to two with one exception, etc.
(defun canonicalize-widetags+exceptions (widetags)
  (let ((ranges (canonicalize-widetags widetags)))
    (flet ((begin (x) (if (listp x) (car x) x))
           (end (x) (if (listp x) (cdr x) x)))
      (when (and (cdr ranges) (endp (cddr ranges))) ; 2 ranges
        (let* ((range-1 (first ranges))
               (range-2 (second ranges))
               (begin-1 (begin range-1))
               (end-1 (end range-1))
               (begin-2 (begin range-2))
               (end-2 (end range-2))
               (delta widetag-spacing))
          (when (and (= (+ end-1 (* 2 delta)) begin-2)
                     ;; Don't return {X} - {Y} if {X} spans only 3 widetags,
                     ;; because clearly we can just test the 2 members of X.
                     ;; fencepost: 3 delta is 4 widetags.
                     (>= (- end-2 begin-1) (* 3 delta)))
            (return-from canonicalize-widetags+exceptions
              (values `((,begin-1 . ,end-2))
                      `(,(+ end-1 delta)))))))) ; the excluded value
    (values ranges nil)))
) ; EVAL-WHEN

(defmacro test-type (value temp target not-p
                     (&rest type-codes)
                     &rest other-args
                     &key &allow-other-keys)
  ;; Determine what interesting combinations we need to test for.
  (let* ((type-codes (mapcar #'eval type-codes))
         (fixnump (and (every (lambda (lowtag)
                                (member lowtag type-codes))
                              '#.(mapcar #'symbol-value fixnum-lowtags))
                       t))
         ;; On 64-bit, UNBOUND-MARKER-WIDETAG may be smaller than LOWTAG-LIMIT
         ;; but it is not a lowtag.
         (lowtags (remove unbound-marker-widetag
                          (remove lowtag-limit type-codes :test #'<)))
         (extended (remove-if (lambda (x)
                                (and (< x lowtag-limit)
                                     (/= x unbound-marker-widetag)))
                              type-codes))
         (immediates (intersection extended +immediate-types+ :test #'eql))
         ;; To collapse the range of widetags comprising real numbers on 64-bit
         ;; machines, consider SHORT-FLOAT-WIDETAG both a header and immediate.
         ;; No OTHER-POINTER-LOWTAG object can ever have that header tag.
         ;; But only do so if there would otherwise be a discontinuity
         ;; in the set of headers.
         ;; Another approach would have been to flip DOUBLE- and SINGLE- float,
         ;; but that would not help NUMBERP, only REALP. Putting SINGLE-
         ;; after the complex widetags would work but harm 32-bit machines.
         (headers (set-difference
                   extended
                   (if (and (= n-word-bits 64)
                            (member (- single-float-widetag 4) extended)
                            (member (+ single-float-widetag 4) extended))
                       (remove single-float-widetag +immediate-types+)
                       +immediate-types+)
                   :test #'eql))
         (function-p (if (intersection headers +function-widetags+)
                         (if (subsetp headers +function-widetags+)
                             t
                             (error "can't test for mix of function subtypes ~
                                     and other header types"))
                         nil)))
    (unless type-codes
      (error "At least one type must be supplied for TEST-TYPE."))
    (unless headers
      (remf other-args :value-tn-ref))
    (cond
      (fixnump
       (when (remove-if (lambda (x)
                          (member x '#.(mapcar #'symbol-value fixnum-lowtags)))
                        lowtags)
         (error "can't mix fixnum testing with other lowtags"))
       (when function-p
         (error "can't mix fixnum testing with function subtype testing"))
       (cond
         ((and (= n-word-bits 64) immediates headers)
          `(%test-fixnum-immediate-and-headers ,value ,temp ,target ,not-p
                                               ,(car immediates)
                                               ',(canonicalize-widetags
                                                  headers)
                                               ,@other-args))
         (immediates
          (if (= n-word-bits 64)
              `(%test-fixnum-and-immediate ,value ,temp ,target ,not-p
                                           ,(car immediates)
                                           ,@other-args)
              (error "can't mix fixnum testing with other immediates")))
         (headers
          `(%test-fixnum-and-headers ,value ,temp ,target ,not-p
                                     ',(canonicalize-widetags headers)
                                     ,@other-args))
         (t
          `(%test-fixnum ,value ,temp ,target ,not-p
                         ,@other-args))))
      (immediates
       (cond
         (headers
          (if (= n-word-bits 64)
              `(%test-immediate-and-headers ,value ,temp ,target ,not-p
                                            ,(car immediates)
                                            ',(canonicalize-widetags headers)
                                            ,@other-args)
              (error "can't mix testing of immediates with testing of headers")))
         (lowtags
          (error "can't mix testing of immediates with testing of lowtags"))
         ((cdr immediates)
          (error "can't test multiple immediates at the same time"))
         (t
          `(%test-immediate ,value ,temp ,target ,not-p ,(car immediates)
                            ,@other-args))))
      (lowtags
       (when (cdr lowtags)
         (error "can't test multiple lowtags at the same time"))
       (when headers
         (error "can't test non-fixnum lowtags and headers at the same time"))
       `(%test-lowtag ,value ,temp ,target ,not-p ,(car lowtags) ,@other-args))
      (headers
       `(%test-headers ,value ,temp ,target ,not-p ,function-p
                       ',(canonicalize-widetags headers)
                       ,@other-args))
      (t
       (error "nothing to test?")))))

;;; FIXME: backend-specific junk doesn't belong in compiler/generic.

#+(or x86 x86-64)
(progn
(define-vop (type-predicate)
  (:args (value :scs (any-reg descriptor-reg)))
  ;; x86 code has to avoid 'esi' and 'edi' for the temp
  ;; since they can't be accessed as an 8-bit byte.
  ;; x86-64 being more regular, any reg can serve as the temp.
  ;; In all likelihood, it'll get rax anyway just because.
  (:temporary (:sc unsigned-reg #+x86 :offset #+x86 eax-offset) temp)
  (:conditional)
  (:info target not-p)
  (:args-var args)
  (:policy :fast-safe))
(define-vop (simple-type-predicate)
  (:args (value :scs (any-reg descriptor-reg control-stack)))
  (:conditional)
  (:info target not-p)
  (:args-var args)
  (:policy :fast-safe))
;; A vop that accepts a computed set of widetags.
(define-vop (%other-pointer-subtype-p type-predicate)
  (:translate %other-pointer-subtype-p)
  (:info target not-p widetags)
  (:arg-types * (:constant t)) ; voodoo - 'target' and 'not-p' are absent
  (:generator 15 ; arbitrary
    (multiple-value-bind (headers except) (canonicalize-widetags+exceptions widetags)
      (%test-headers value temp target not-p nil headers :except except
                                                         :value-tn-ref args)))))

#-(or x86 x86-64)
(progn
(define-vop (type-predicate)
  (:args (value :scs (any-reg descriptor-reg)))
  (:temporary (:sc non-descriptor-reg) temp)
  (:conditional)
  (:info target not-p)
  (:args-var args)
  (:policy :fast-safe))
;; A vop that accepts a computed set of widetags.
(define-vop (%other-pointer-subtype-p type-predicate)
  (:translate %other-pointer-subtype-p)
  (:info target not-p widetags)
  (:arg-types * (:constant t)) ; voodoo - 'target' and 'not-p' are absent
  (:args-var args)
  (:generator 15 ; arbitrary
    (%test-headers value temp target not-p nil (canonicalize-widetags widetags)
                   :value-tn-ref args))))

(defmacro define-type-vop (pred-name type-codes
                             &optional (inherit 'type-predicate))
  (let ((cost (if (> (reduce #'max type-codes :key #'eval) lowtag-limit)
                  7
                  4)))
    `(define-vop (,pred-name ,inherit)
       (:translate ,pred-name)
       (:generator ,cost
         (test-type value
                    ,(if (eq inherit 'simple-type-predicate) nil 'temp)
                    target not-p ,type-codes
                    :value-tn-ref args)))))

#-x86-64 ; defined in compiler/x86-64/type-vops for x86-64
(progn
(define-type-vop unbound-marker-p (unbound-marker-widetag))

(define-type-vop characterp (character-widetag))

#-arm-64
(define-type-vop single-float-p (single-float-widetag))

(define-type-vop fixnump
  #.fixnum-lowtags
  #+(or x86) simple-type-predicate) ;; save a register

(define-type-vop functionp (fun-pointer-lowtag))

(define-type-vop listp (list-pointer-lowtag))

(define-type-vop %instancep (instance-pointer-lowtag))

(define-type-vop %other-pointer-p (other-pointer-lowtag))

(define-type-vop non-null-symbol-p (symbol-widetag))

(define-type-vop closurep (closure-widetag))

(define-type-vop simple-fun-p (simple-fun-widetag))

(define-type-vop funcallable-instance-p (funcallable-instance-widetag))
) ; end PROGN

(define-type-vop bignump (bignum-widetag))

(define-type-vop ratiop (ratio-widetag))

(define-type-vop complexp
  (complex-widetag complex-single-float-widetag complex-double-float-widetag
                   #+long-float complex-long-float-widetag))

(define-type-vop complex-rational-p (complex-widetag))

(define-type-vop complex-float-p
  (complex-single-float-widetag complex-double-float-widetag
                                #+long-float complex-long-float-widetag))

(define-type-vop complex-single-float-p (complex-single-float-widetag))

(define-type-vop complex-double-float-p (complex-double-float-widetag))

(define-type-vop double-float-p (double-float-widetag))

(define-type-vop simple-string-p
  (#+sb-unicode simple-character-string-widetag
   simple-base-string-widetag))

#-x86-64
(macrolet
    ((define-simple-array-type-vops ()
         `(progn
           ,@(map 'list
                  (lambda (saetp)
                    (let ((primtype (saetp-primitive-type-name saetp)))
                    `(define-type-vop
                      ,(symbolicate primtype "-P")
                      (,(saetp-typecode saetp)))))
                  *specialized-array-element-type-properties*))))
  (define-simple-array-type-vops))

(macrolet
    ((def ()
       `(define-type-vop simple-rank-1-array-*-p
         ,(map 'list #'saetp-typecode
               *specialized-array-element-type-properties*))))
  (def)) ; simple-rank-1-array-*-p

(define-type-vop system-area-pointer-p (sap-widetag))

(define-type-vop weak-pointer-p (weak-pointer-widetag))

(define-type-vop code-component-p (code-header-widetag))

#-(or x86 x86-64 arm64) (define-type-vop lra-p (return-pc-widetag))

(define-type-vop fdefn-p (fdefn-widetag))

(define-type-vop array-header-p
  (simple-array-widetag
   #+sb-unicode complex-character-string-widetag
   complex-base-string-widetag complex-bit-vector-widetag
   complex-vector-widetag complex-array-widetag))

(define-type-vop simple-array-header-p
  (simple-array-widetag))

(define-type-vop stringp
  (#+sb-unicode simple-character-string-widetag
   #+sb-unicode complex-character-string-widetag
   simple-base-string-widetag complex-base-string-widetag))

(define-type-vop base-string-p
  (simple-base-string-widetag complex-base-string-widetag))

(define-type-vop bit-vector-p
  (simple-bit-vector-widetag complex-bit-vector-widetag))

#+sb-unicode
(define-type-vop character-string-p
  (simple-character-string-widetag complex-character-string-widetag))

(define-type-vop vectorp
  (complex-vector-widetag .
   #.(append
      (map 'list
           #'saetp-typecode
           *specialized-array-element-type-properties*)
      (mapcan (lambda (saetp)
                (when (saetp-complex-typecode saetp)
                  (list (saetp-complex-typecode saetp))))
              (coerce *specialized-array-element-type-properties* 'list)))))

;;; Note that this "type VOP" is sort of an oddball; it doesn't so
;;; much test for a Lisp-level type as just expose a low-level type
;;; code at the Lisp level. It is used as a building block to help us
;;; to express things like the test for (TYPEP FOO '(VECTOR T))
;;; efficiently in Lisp code, but it doesn't correspond to any type
;;; expression which would actually occur in reasonable application
;;; code. (Common Lisp doesn't have any natural way of expressing this
;;; type.) Thus, there's no point in building up the full machinery of
;;; associated backend type predicates and so forth as we do for
;;; ordinary type VOPs.
(define-type-vop complex-vector-p (complex-vector-widetag))

(define-type-vop simple-array-p
  (simple-array-widetag .
   #.(map 'list
          #'saetp-typecode
          *specialized-array-element-type-properties*)))

(define-type-vop arrayp
  (simple-array-widetag
   complex-array-widetag
   complex-vector-widetag .
   #.(append
      (map 'list
           #'saetp-typecode
           *specialized-array-element-type-properties*)
      (mapcan (lambda (saetp)
                (when (saetp-complex-typecode saetp)
                  (list (saetp-complex-typecode saetp))))
              (coerce *specialized-array-element-type-properties* 'list)))))

(define-type-vop numberp
  (bignum-widetag
   ratio-widetag
   single-float-widetag
   double-float-widetag
   #+long-float long-float-widetag
   complex-widetag
   complex-single-float-widetag
   complex-double-float-widetag
   #+long-float complex-long-float-widetag
   . #.fixnum-lowtags))

(define-type-vop rationalp
  (ratio-widetag bignum-widetag . #.fixnum-lowtags))

(define-type-vop integerp
  (bignum-widetag . #.fixnum-lowtags))

(define-type-vop floatp
  (single-float-widetag double-float-widetag #+long-float long-float-widetag))

(define-type-vop realp
  (ratio-widetag
   bignum-widetag
   single-float-widetag
   double-float-widetag
   #+long-float long-float-widetag
   . #.fixnum-lowtags))

#+sb-simd-pack
(define-type-vop simd-pack-p (simd-pack-widetag))
#+sb-simd-pack-256
(define-type-vop simd-pack-256-p (simd-pack-256-widetag))

;;; Not type vops, but generic over all backends
(macrolet ((def (name lowtag)
             `(define-vop ()
                (:translate ,name)
                (:policy :fast-safe)
                (:args (x :scs (descriptor-reg)))
                (:results (res :scs (unsigned-reg)))
                (:result-types unsigned-num)
                (:generator 2 (loadw res x 0 ,lowtag)))))
  (def function-header-word fun-pointer-lowtag)
  (def instance-header-word instance-pointer-lowtag))
