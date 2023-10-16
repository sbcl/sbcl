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

(defconstant-eqx +simple-rank-1-array-widetags+
  (map 'list #'saetp-typecode *specialized-array-element-type-properties*)
  #'equal)

(defconstant-eqx +vector-widetags+
  `(,complex-vector-widetag
    #-sb-unicode ,unused-simple-char-string ; because of the contiguity assertion
    #-sb-unicode ,unused-complex-char-string
    ,@(append
       (map 'list #'saetp-typecode *specialized-array-element-type-properties*)
       (mapcan (lambda (saetp)
                 (when (saetp-complex-typecode saetp)
                   (list (saetp-complex-typecode saetp))))
               (coerce *specialized-array-element-type-properties* 'list))))
  #'equal)

(defconstant-eqx +simple-array-widetags+
  `(,simple-array-widetag ,@+simple-rank-1-array-widetags+)
  #'equal)

(defconstant-eqx +array-widetags+
  `(,simple-array-widetag ,complex-array-widetag ,@+vector-widetags+)
  #'equal)

(defconstant-eqx +string-widetags+
  `(#+sb-unicode ,simple-character-string-widetag
    #+sb-unicode ,complex-character-string-widetag
    ,simple-base-string-widetag ,complex-base-string-widetag)
  #'equal)

#+sb-xc-host
(flet ((check (list)
         ;; Assert that LIST is a contiguous range of widetags
         (let* ((sorted (sort (copy-list list) #'<))
                (min (first sorted))
                (max (car (last sorted))))
           (assert (equal sorted (loop for w from min to max by 4 collect w))))))
  (check +simple-rank-1-array-widetags+)
  (check +vector-widetags+)
  (check +simple-array-widetags+)
  (check +array-widetags+)
  #+sb-unicode (check +string-widetags+)) ; they're discontiguous if #-sb-unicode

;; Given a list of widetags in HEADERS, compress into a minimal list of ranges
;; and/or singletons that should be tested.
;; FIXME: At present the "is it effectively a one-sided test" is re-implemented
;;        in an ad-hoc way by each backend. The range convention should be
;;        changed to indicate explicitly when either limit needn't be checked.
;;        (Use NIL or * as a bound perhaps)
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

(defmacro test-type (value temp target not-p
                     (&rest type-codes)
                     &rest other-args
                     &key &allow-other-keys)
  ;; Determine what interesting combinations we need to test for.
  (let* ((type-codes (mapcar #'eval type-codes))
         ;; FIXNUMP is true if every one of the fixnum lowtags is present
         ;; in your specified list of type-codes (NOT the other way 'round)
         (fixnump (subsetp fixnum-lowtags type-codes))
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
    (cond
      (fixnump
       (when (set-difference lowtags fixnum-lowtags)
         (error "can't mix fixnum testing with other lowtags"))
       (when function-p
         (error "can't mix fixnum testing with function subtype testing"))
       (cond
         ((and (= n-word-bits 64) immediates headers)
          `(%test-fixnum-immediate-and-headers ,value ,temp ,target ,not-p
                                               ,(car immediates)
                                               ',(canonicalize-widetags
                                                  headers)
                                               :immediate-tested '(fixnum ,(car immediates))
                                               ,@other-args))
         (immediates
          (error "can't mix fixnum testing with other immediates"))
         (headers
          `(%test-fixnum-and-headers ,value ,temp ,target ,not-p
                                     ',(canonicalize-widetags headers)
                                     :immediate-tested '(fixnum)
                                     ,@other-args))
         (t
          (remf other-args :value-tn-ref)
          `(%test-fixnum ,value ,temp ,target ,not-p
                         ,@other-args))))
      (immediates
       (cond
         (headers
          (if (= n-word-bits 64)
              `(%test-immediate-and-headers ,value ,temp ,target ,not-p
                                            ,(car immediates)
                                            ',(canonicalize-widetags headers)
                                            :immediate-tested '(,(car immediates))
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
  (:arg-refs args)
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
  (:arg-refs args)
  (:policy :fast-safe))
;; A vop that accepts a computed set of widetags.
(define-vop (%other-pointer-subtype-p type-predicate)
  (:translate %other-pointer-subtype-p)
  (:info target not-p widetags)
  (:arg-types * (:constant t)) ; voodoo - 'target' and 'not-p' are absent
  (:arg-refs args)
  (:generator 15 ; arbitrary
    (%test-headers value temp target not-p nil (canonicalize-widetags widetags)
                   :value-tn-ref args))))
