;;;; the VM definition arithmetic VOPs for the ARM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; unary operations.

(define-vop (fast-safe-arith-op)
  (:policy :fast-safe)
  (:effects)
  (:affected))

;;;; Binary fixnum operations.

;;; Assume that any constant operand is the second arg...

(define-vop (fast-fixnum-binop fast-safe-arith-op)
  (:args (x :target r :scs (any-reg))
         (y :target r :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic"))

(define-vop (fast-unsigned-binop fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg))
         (y :target r :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic"))

(define-vop (fast-signed-binop fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg))
         (y :target r :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline (signed-byte 32) arithmetic"))


#+(or) ;; FIXME: Sort out the :constant ranges for these
(define-vop (fast-fixnum-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (any-reg)))
  (:info y)
  (:arg-types tagged-num
              (:constant (and (signed-byte 11) (not (integer 0 0)))))
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic"))

#+(or)
(define-vop (fast-unsigned-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg)))
  (:info y)
  (:arg-types unsigned-num
              (:constant (and (signed-byte 13) (not (integer 0 0)))))
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic"))

#+(or)
(define-vop (fast-signed-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg)))
  (:info y)
  (:arg-types signed-num
              (:constant (and (signed-byte 13) (not (integer 0 0)))))
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline (signed-byte 32) arithmetic"))

(defmacro define-binop (translate untagged-penalty op
                        &optional arg-swap)
  `(progn
     (define-vop (,(symbolicate 'fast translate '/fixnum=>fixnum)
                  fast-fixnum-binop)
       (:translate ,translate)
       (:generator 2
         ,(if arg-swap
              `(inst ,op r y x)
              `(inst ,op r x y))))
     #+(or)
     ,@(unless arg-swap
         `((define-vop (,(symbolicate 'fast- translate '-c/fixnum=>fixnum)
                        fast-fixnum-binop-c)
             (:translate ,translate)
             (:generator 1
               (inst ,op r x (fixnumize y))))))
     (define-vop (,(symbolicate 'fast- translate '/signed=>signed)
                  fast-signed-binop)
       (:translate ,translate)
       (:generator ,(1+ untagged-penalty)
         ,(if arg-swap
              `(inst ,op r y x)
              `(inst ,op r x y))))
     #+(or)
     ,@(unless arg-swap
         `((define-vop (,(symbolicate 'fast- translate '-c/signed=>signed)
                        fast-signed-binop-c)
             (:translate ,translate)
             (:generator ,untagged-penalty
               (inst ,op r x y)))))
     (define-vop (,(symbolicate 'fast- translate '/unsigned=>unsigned)
                  fast-unsigned-binop)
       (:translate ,translate)
       (:generator ,(1+ untagged-penalty)
         ,(if arg-swap
              `(inst ,op r y x)
              `(inst ,op r x y))))
     #+(or)
     ,@(unless arg-swap
         `((define-vop (,(symbolicate 'fast- translate '-c/unsigned=>unsigned)
                        fast-unsigned-binop-c)
             (:translate ,translate)
             (:generator ,untagged-penalty
               (inst ,op r x y)))))))

(define-binop + 4 add)
(define-binop - 4 sub)
(define-binop logand 2 and)
(define-binop logandc1 2 bic t)
(define-binop logandc2 2 bic)
(define-binop logior 2 orr)
(define-binop logxor 2 eor)

(define-vop (fast-logand/signed-unsigned=>unsigned fast-logand/unsigned=>unsigned)
  (:args (x :scs (signed-reg) :target r)
         (y :scs (unsigned-reg) :target r))
  (:arg-types signed-num unsigned-num)
  (:translate logand))

(define-source-transform logeqv (&rest args)
  (if (oddp (length args))
      `(logxor ,@args)
      `(lognot (logxor ,@args))))
(define-source-transform logorc1 (x y)
  `(logior (lognot ,x) ,y))
(define-source-transform logorc2 (x y)
  `(logior ,x (lognot ,y)))

;;;; Binary conditional VOPs:

(define-vop (fast-conditional)
  (:conditional)
  (:info target not-p)
  (:effects)
  (:affected)
  (:policy :fast-safe))

(define-vop (fast-conditional/fixnum fast-conditional)
  (:args (x :scs (any-reg))
         (y :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison"))

(define-vop (fast-conditional-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg)))
  (:arg-types tagged-num (:constant (unsigned-byte 8)))
  (:info target not-p y))

(define-vop (fast-conditional/signed fast-conditional)
  (:args (x :scs (signed-reg))
         (y :scs (signed-reg)))
  (:arg-types signed-num signed-num)
  (:note "inline (signed-byte 32) comparison"))

(define-vop (fast-conditional-c/signed fast-conditional/signed)
  (:args (x :scs (signed-reg)))
  (:arg-types signed-num (:constant (unsigned-byte 8)))
  (:info target not-p y))

(define-vop (fast-conditional/unsigned fast-conditional)
  (:args (x :scs (unsigned-reg))
         (y :scs (unsigned-reg)))
  (:arg-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 32) comparison"))

(define-vop (fast-conditional-c/unsigned fast-conditional/unsigned)
  (:args (x :scs (unsigned-reg)))
  (:arg-types unsigned-num (:constant (unsigned-byte 8)))
  (:info target not-p y))

(defmacro define-conditional-vop (tran cond unsigned not-cond not-unsigned)
  `(progn
     ,@(mapcar (lambda (suffix cost signed)
                 (unless (and (member suffix '(/fixnum -c/fixnum))
                              (eq tran 'eql))
                   `(define-vop (,(intern (format nil "~:@(FAST-IF-~A~A~)"
                                                  tran suffix))
                                 ,(intern
                                   (format nil "~:@(FAST-CONDITIONAL~A~)"
                                           suffix)))
                     (:translate ,tran)
                     (:generator ,cost
                      (inst cmp x
                       ,(if (eq suffix '-c/fixnum) '(fixnumize y) 'y))
                      (inst b (if not-p
                                  ,(if signed not-cond not-unsigned)
                                  ,(if signed cond unsigned))
                       target)))))
               '(/fixnum -c/fixnum /signed -c/signed /unsigned -c/unsigned)
               '(4 3 6 5 6 5)
               '(t t t t nil nil))))

(define-conditional-vop < :lt :lo :ge :hs)
(define-conditional-vop > :gt :hi :le :ls)
(define-conditional-vop eql :eq :eq :ne :ne)

;;; EQL/FIXNUM is funny because the first arg can be of any type, not
;;; just a known fixnum.

;;; These versions specify a fixnum restriction on their first arg.
;;; We have also generic-eql/fixnum VOPs which are the same, but have
;;; no restriction on the first arg and a higher cost.  The reason for
;;; doing this is to prevent fixnum specific operations from being
;;; used on word integers, spuriously consing the argument.

(define-vop (fast-eql/fixnum fast-conditional)
  (:args (x :scs (any-reg))
         (y :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison")
  (:translate eql)
  (:generator 4
    (inst cmp x y)
    (inst b (if not-p :ne :eq) target)))
(define-vop (generic-eql/fixnum fast-eql/fixnum)
  (:args (x :scs (any-reg descriptor-reg))
         (y :scs (any-reg)))
  (:arg-types * tagged-num)
  (:variant-cost 7))

(define-vop (fast-eql-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg)))
  (:arg-types tagged-num (:constant (unsigned-byte 8)))
  (:info target not-p y)
  (:translate eql)
  (:generator 2
    (inst cmp x (fixnumize y))
    (inst b (if not-p :ne :eq) target)))
(define-vop (generic-eql-c/fixnum fast-eql-c/fixnum)
  (:args (x :scs (any-reg descriptor-reg)))
  (:arg-types * (:constant (unsigned-byte 8)))
  (:variant-cost 6))
