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

(defun emit-gengc-barrier (object cell-address temps &optional value-tn-ref allocator)
  (aver (neq (car temps) cell-address)) ; LD would clobber the cell-address
  (when (require-gengc-barrier-p object value-tn-ref allocator)
    ;; (inst ld (car temps) thread-base-tn (ash thread-card-table-slot word-shift))
    ;; RLIDCL dest, source, (64-rightshift), (64-indexbits)
    (inst rldicl (car temps) (or cell-address object) (- 64 gencgc-card-shift)
          (make-fixup nil :card-table-index-mask))
    ;; THREAD-TN's low byte is 0.
    (inst stbx thread-base-tn card-table-base-tn (car temps))))


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
  (:vop-var vop)
  (:temporary (:sc non-descriptor-reg) t1)
  (:generator 4
    (emit-gengc-barrier object nil (list t1) (vop-nth-arg 1 vop))
    (storew value object offset lowtag)))

;;;; Indexed references:

;;; Define some VOPs for indexed memory reference.

(define-vop (descriptor-word-index-set)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg immediate))
         (value :scs (any-reg descriptor-reg)))
  (:arg-types * tagged-num *)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
   (emit-gengc-barrier object nil (list temp) (vop-nth-arg 2 vop))
   (sc-case index
    ((immediate)
     (let ((offset (- (ash (+ (tn-value index) offset) word-shift) lowtag)))
       (cond ((and (typep offset '(signed-byte 16)) (not (logtest offset #b11)))
              (inst std value object offset))
             (t
              (inst lr temp offset)
              (inst stdx value object temp)))))
    (t
     (inst addi temp index (- (ash offset word-shift) lowtag))
     (inst stdx value object temp)))))

;;; Due to the encoding restrictione that doubleword accesses can not displace
;;; from the base register by an arbitrarily aligned value, but only an even
;;; multiple of 4. By using a certain arrangement of lowtags we can get two of
;;; the pointer types to get lowtag subtraction for free:
;;;    #b0100 list - good
;;;    #b0110 function
;;;    #b1100 instance - good
;;;    #b1110 other
;;; The misalignment of OTHER is not a huge problem, because array access requires
;;; calculating the displacement anyway, and the remaining OTHER pointer types
;;; like VALUE-CELL are not as prevalent. SYMBOL remains a bit of a problem.

;;; NB: this macro uses an inverse of the 'shift' convention in the 32-bit file.
;;; Here is the convention is that 'scale' how much to left-shift a natural
;;; integer (an unsigned-reg) to produce a byte offset for the element size.
;;; For 32-bits, the convention is that 'shift' is how much to right-shift
;;; a tagged fixnum to produce a natural index into the data vector for the
;;; element size. It works only because n-fixnum-tag-bits = word-shift,
;;; and there is no case in which left-shift is required.
(defmacro define-indexer (name shift write-p ri-op rr-op &key sign-extend-byte
                                                              multiple-of-four
                          &aux (net-shift (- shift n-fixnum-tag-bits)))
  `(define-vop (,name)
     (:args (object :scs (descriptor-reg))
            (index :scs (any-reg immediate))
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
         ((immediate)
          (let ((offset (- (+ (ash (tn-value index) ,shift)
                              (ash offset word-shift))
                           lowtag)))
            (if (and (typep offset '(signed-byte 16))
                     (or ,@(and (not multiple-of-four)
                                `((< ,shift 3))) ;; If it's not word-index
                            (not (logtest offset #b11)))) ;; Or the displacement is a multiple of 4
                (inst ,ri-op value object offset)
                (progn
                  (inst lr temp offset)
                  (inst ,rr-op value object temp)))))
         (t
          ,@(cond ((plusp net-shift)
                   `((inst sldi temp index ,net-shift)))
                  ((minusp net-shift)
                   `((inst srdi temp index (- ,net-shift)))))
          (inst addi temp ,(if (zerop net-shift) 'index 'temp)
                (- (ash offset word-shift) lowtag))
          (inst ,rr-op value object temp)))
       ,@(when sign-extend-byte
           `((inst extsb value value))))))

(define-indexer word-index-ref           3 nil ld  ldx) ;; Word means Lisp Word
(define-indexer 32-bits-index-ref        2 nil lwz lwzx)
(define-indexer signed-32-bits-index-ref 2 nil lwa lwax :multiple-of-four t)
(define-indexer 16-bits-index-ref        1 nil lhz lhzx)
(define-indexer signed-16-bits-index-ref 1 nil lha lhax)
(define-indexer byte-index-ref           0 nil lbz lbzx)
(define-indexer signed-byte-index-ref    0 nil lbz lbzx :sign-extend-byte t)

(define-indexer word-index-set           3 t   std stdx)
(define-indexer 32-bits-index-set        2 t   stw stwx)
(define-indexer 16-bits-index-set        1 t   sth sthx)
(define-indexer byte-index-set           0 t   stb stbx)

(define-vop (word-index-cas)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg immediate))
         (old-value :scs (any-reg descriptor-reg))
         (new-value :scs (any-reg descriptor-reg)))
  (:arg-types * tagged-num * *)
  (:temporary (:sc non-descriptor-reg) temp)
  (:results (result :scs (any-reg descriptor-reg) :from :load))
  (:result-types *)
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (let ((ea
           (ecase lowtag
             (#.instance-pointer-lowtag nil)
             (#.other-pointer-lowtag ; has to be (SETF SVREF)
              (cond ((sc-is index immediate)
                     (let ((offset (- (ash (+ (tn-value index) offset) word-shift) lowtag)))
                       (inst lr temp offset)))
                    (t
                     (inst addi temp index (- (ash offset word-shift) lowtag))))
              (inst add temp object temp)
              temp))))
      (emit-gengc-barrier object ea (list result temp) (vop-nth-arg 3 vop)))
    (sc-case index
      ((immediate)
       (let ((offset (- (ash (+ (tn-value index) offset) word-shift) lowtag)))
         (inst lr temp offset)))
      (t
       (inst sldi temp index (- word-shift n-fixnum-tag-bits))
       (inst addi temp temp (- (ash offset word-shift) lowtag))))
    (inst sync)
    LOOP
    (inst ldarx result temp object)
    (inst cmpd result old-value)
    (inst bne EXIT)
    (inst stdcx. new-value temp object)
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
    (inst ldarx header 0 baseptr)
    (inst ori header header (ash 1 stable-hash-required-flag))
    (inst stdcx. header 0 baseptr)
    (inst bne LOOP)
    (inst isync)))
