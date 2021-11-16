;;;; the PPC definitions of some general purpose memory reference VOPs
;;;; inherited by basic memory reference operations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;; Cell-Ref and Cell-Set are used to define VOPs like CAR, where the offset to
;;; be read or written is a property of the VOP used.
;;;
(define-vop (cell-ref)
  (:args (object :scs (descriptor-reg)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 4
    (loadw value object offset lowtag)))
;;;
(define-vop (cell-set)
  (:args (object :scs (descriptor-reg))
         (value :scs (descriptor-reg any-reg)))
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 4
    (storew value object offset lowtag)))

;;;; Indexed references:

;;; Define some VOPs for indexed memory reference.
(defmacro define-indexer (name write-p ri-op rr-op shift &key sign-extend-byte)
  `(define-vop (,name)
     (:args (object :scs (descriptor-reg))
            (index :scs (any-reg zero immediate))
            ,@(when write-p '((value :scs (any-reg descriptor-reg)))))
     (:arg-types * tagged-num ,@(when write-p '(*)))
     (:temporary (:scs (non-descriptor-reg)) temp)
     ,@(unless write-p
         `((:results (value :scs (any-reg descriptor-reg)))
           (:result-types *)))
     (:variant-vars offset lowtag)
     (:policy :fast-safe)
     (:generator 5
       (sc-case index
         ((immediate zero)
          (let ((offset (- (+ (if (sc-is index zero)
                                  0
                                  (ash (tn-value index)
                                       (- word-shift ,shift)))
                              (ash offset word-shift))
                           lowtag)))
            (etypecase offset
              ((signed-byte 16)
               (inst ,ri-op value object offset))
              ((or (unsigned-byte 32) (signed-byte 32))
               (inst lr temp offset)
               (inst ,rr-op value object temp)))))
         (t
          ,@(unless (zerop shift)
              `((inst srwi temp index ,shift)))
          (inst addi temp ,(if (zerop shift) 'index 'temp)
                (- (ash offset word-shift) lowtag))
          (inst ,rr-op value object temp)))
       ,@(when sign-extend-byte
           `((inst extsb value value))))))

(define-indexer word-index-ref            nil lwz lwzx 0)
(define-indexer halfword-index-ref        nil lhz lhzx 1)
(define-indexer signed-halfword-index-ref nil lha lhax 1)
(define-indexer byte-index-ref            nil lbz lbzx 2)
(define-indexer signed-byte-index-ref     nil lbz lbzx 2 :sign-extend-byte t)

(define-indexer word-index-set              t stw stwx 0)
(define-indexer halfword-index-set          t sth sthx 1)
(define-indexer byte-index-set              t stb stbx 2)

(define-vop (word-index-cas)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg zero immediate))
         (old-value :scs (any-reg descriptor-reg))
         (new-value :scs (any-reg descriptor-reg)))
  (:arg-types * tagged-num * *)
  (:temporary (:sc non-descriptor-reg) temp)
  (:results (result :scs (any-reg descriptor-reg) :from :load))
  (:result-types *)
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 5
    (sc-case index
      ((immediate zero)
       (let ((offset (- (+ (if (sc-is index zero)
                               0
                             (ash (tn-value index) word-shift))
                           (ash offset word-shift))
                        lowtag)))
         (inst lr temp offset)))
      (t
       ;; KLUDGE: This relies on N-FIXNUM-TAG-BITS being the same as
       ;; WORD-SHIFT.  I know better than to do this.  --AB, 2010-Jun-16
       (inst addi temp index
             (- (ash offset word-shift) lowtag))))

    (inst sync)
    LOOP
    (inst lwarx result temp object)
    (inst cmpw result old-value)
    (inst bne EXIT)
    (inst stwcx. new-value temp object)
    (inst bne LOOP)
    EXIT
    (inst isync)))

(define-vop (set-instance-hashed)
  (:args (object :scs (descriptor-reg)))
  (:temporary (:sc non-descriptor-reg) baseptr header)
  (:generator 5
    (inst addi baseptr object (- instance-pointer-lowtag))
    (inst sync)
    LOOP
    (inst lwarx header 0 baseptr)
    (inst ori header header (ash 1 stable-hash-required-flag))
    (inst stwcx. header 0 baseptr)
    (inst bne LOOP)
    (inst isync)))
