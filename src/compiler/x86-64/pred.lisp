;;;; predicate VOPs for the x86 VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; the branch VOP

;;; The unconditional branch, emitted when we can't drop through to the desired
;;; destination. Dest is the continuation we transfer control to.
(define-vop (branch)
  (:info dest)
  (:generator 5
    (inst jmp dest)))


;;;; Generic conditional VOPs

;;; The generic conditional branch, emitted immediately after test
;;; VOPs that only set flags.
;;;
;;; FLAGS is a list of condition descriptors. If the first descriptor
;;; is CL:NOT, the test was true if all the remaining conditions are
;;; false. Otherwise, the test was true if any of the conditions is.
;;;
;;; NOT-P flips the meaning of the test, as with regular :CONDITIONAL
;;; VOP. If NOT-P is true, the code must branch to dest if the test was
;;; false. Otherwise, the code must branch to dest if the test was true.

(define-vop (branch-if)
  (:info dest flags not-p)
  (:generator 0
     (when (eq (car flags) 'not)
       (pop flags)
       (setf not-p (not not-p)))
     (flet ((negate-condition (name)
              (let ((code (logxor 1 (conditional-opcode name))))
                (aref *condition-name-vec* code))))
       (cond ((null (rest flags))
              (inst jmp
                    (if not-p
                        (negate-condition (first flags))
                        (first flags))
                    dest))
             (not-p
              (let ((not-lab (gen-label))
                    (last    (car (last flags))))
                (dolist (flag (butlast flags))
                  (inst jmp flag not-lab))
                (inst jmp (negate-condition last) dest)
                (emit-label not-lab)))
             (t
              (dolist (flag flags)
                (inst jmp flag dest)))))))

(defvar *cmov-ptype-representation-vop*
  (mapcan (lambda (entry)
            (destructuring-bind (ptypes &optional sc vop)
                entry
              (mapcar (if (and vop sc)
                          (lambda (ptype)
                            (list ptype sc vop))
                          #'list)
                      (ensure-list ptypes))))
          '((t descriptor-reg move-if/t)

            ((fixnum positive-fixnum)
             any-reg move-if/fx)
            ((unsigned-byte-64 unsigned-byte-63)
             unsigned-reg move-if/unsigned)
            (signed-byte-64 signed-reg move-if/signed)
            ;; FIXME: Can't use CMOV with byte registers, and characters live
            ;; in such outside of unicode builds. A better solution then just
            ;; disabling MOVE-IF/CHAR should be possible, though.
            #!+sb-unicode
            (character character-reg move-if/char)

            ((single-float complex-single-float
              double-float complex-double-float))

            (system-area-pointer sap-reg move-if/sap)))
  #!+sb-doc
  "Alist of primitive type -> (storage-class-name VOP-name)
   if values of such a type should be cmoved, and NIL otherwise.

   storage-class-name is the name of the storage class to use for
   the values, and VOP-name the name of the VOP that will be used
   to execute the conditional move.")

(defun convert-conditional-move-p (node dst-tn x-tn y-tn)
  (declare (ignore node))
  (let* ((ptype (sb!c::tn-primitive-type dst-tn))
         (name  (sb!c::primitive-type-name ptype))
         (param (cdr (or (assoc name *cmov-ptype-representation-vop*)
                         '(t descriptor-reg move-if/t)))))
    (when param
      (destructuring-bind (representation vop) param
        (let ((scn (sc-number-or-lose representation)))
          (labels ((make-tn ()
                     (make-representation-tn ptype scn))
                   (frob-tn (tn)
                     (if (immediate-tn-p tn)
                         tn
                         (make-tn))))
            (values vop
                    (frob-tn x-tn) (frob-tn y-tn)
                    (make-tn)
                    nil)))))))

(define-vop (move-if)
  (:args (then) (else))
  (:results (res))
  (:info flags)
  (:generator 0
     (let ((not-p (eq (first flags) 'not)))
       (when not-p (pop flags))
       (flet ((negate-condition (name)
                (let ((code (logxor 1 (conditional-opcode name))))
                  (aref *condition-name-vec* code)))
              (load-immediate (dst constant-tn
                                   &optional (sc (sc-name (tn-sc dst))))
                (let ((val (tn-value constant-tn)))
                  (etypecase val
                    (integer
                     ;; Can't use ZEROIZE here, since XOR will affect
                     ;; the flags.
                     (if (memq sc '(any-reg descriptor-reg))
                         (inst mov dst (fixnumize val))
                         (inst mov dst val)))
                    (symbol
                     (aver (eq sc 'descriptor-reg))
                     (load-symbol dst val))
                    (character
                     (if (eq sc 'descriptor-reg)
                         (inst mov dst (logior (ash (char-code val) n-widetag-bits)
                                               character-widetag))
                         (inst mov dst (char-code val))))))))
         (cond ((null (rest flags))
                (if (sc-is else immediate)
                    (load-immediate res else)
                    (move res else))
                (when (sc-is then immediate)
                  (load-immediate temp-reg-tn then (sc-name (tn-sc res)))
                  (setf then temp-reg-tn))
                (inst cmov (if not-p
                               (negate-condition (first flags))
                               (first flags))
                      res
                      then))
               (not-p
                (cond ((sc-is then immediate)
                       (when (location= else res)
                         (inst mov temp-reg-tn else)
                         (setf else temp-reg-tn))
                       (load-immediate res then))
                      ((location= else res)
                       (inst xchg else then)
                       (rotatef else then))
                      (t
                       (move res then)))
                (when (sc-is else immediate)
                  (load-immediate temp-reg-tn else (sc-name (tn-sc res)))
                  (setf else temp-reg-tn))
                (dolist (flag flags)
                  (inst cmov flag res else)))
               (t
                (if (sc-is else immediate)
                    (load-immediate res else)
                    (move res else))
                (when (sc-is then immediate)
                  (load-immediate temp-reg-tn then (sc-name (tn-sc res)))
                  (setf then temp-reg-tn))
                (dolist (flag flags)
                  (inst cmov flag res then))))))))

(macrolet ((def-move-if (name type reg stack)
             `(define-vop (,name move-if)
                (:args (then :scs (immediate ,reg ,stack) :to :eval
                             :load-if (not (or (sc-is then immediate)
                                               (and (sc-is then ,stack)
                                                    (not (location= else res))))))
                       (else :scs (immediate ,reg ,stack) :target res
                             :load-if (not (sc-is else immediate ,stack))))
                (:arg-types ,type ,type)
                (:results (res :scs (,reg)
                               :from (:argument 1)))
                (:result-types ,type))))
  (def-move-if move-if/t t descriptor-reg control-stack)
  (def-move-if move-if/fx tagged-num any-reg control-stack)
  (def-move-if move-if/unsigned unsigned-num unsigned-reg unsigned-stack)
  (def-move-if move-if/signed signed-num signed-reg signed-stack)
  ;; FIXME: See *CMOV-PTYPE-REPRESENTATION-VOP* above.
  #!+sb-unicode
  (def-move-if move-if/char character character-reg character-stack)
  (def-move-if move-if/sap system-area-pointer sap-reg sap-stack))

;;;; conditional VOPs

;;; Note: a constant-tn is allowed in CMP; it uses an EA displacement,
;;; not immediate data.
(define-vop (if-eq)
  (:args (x :scs (any-reg descriptor-reg control-stack constant)
            :load-if (not (and (sc-is x immediate)
                               (sc-is y any-reg descriptor-reg
                                      control-stack constant))))
         (y :scs (any-reg descriptor-reg immediate)
            :load-if (not (and (sc-is x any-reg descriptor-reg immediate)
                               (sc-is y control-stack constant)))))
  (:temporary (:sc descriptor-reg) temp)
  (:conditional :e)
  (:policy :fast-safe)
  (:translate eq)
  (:generator 6
    (cond
     ((sc-is y immediate)
      (let ((val (tn-value y)))
        (etypecase val
          (integer
           (if (and (zerop val) (sc-is x any-reg descriptor-reg))
               (inst test x x) ; smaller
             (let ((fixnumized (fixnumize val)))
               (if (typep fixnumized
                          '(or (signed-byte 32) (unsigned-byte 31)))
                   (inst cmp x fixnumized)
                 (progn
                   (inst mov temp fixnumized)
                   (inst cmp x temp))))))
          (symbol
           (inst cmp x (+ nil-value (static-symbol-offset val))))
          (character
           (inst cmp x (logior (ash (char-code val) n-widetag-bits)
                               character-widetag))))))
     ((sc-is x immediate) ; and y not immediate
      ;; Swap the order to fit the compare instruction.
      (let ((val (tn-value x)))
        (etypecase val
          (integer
           (if (and (zerop val) (sc-is y any-reg descriptor-reg))
               (inst test y y) ; smaller
             (let ((fixnumized (fixnumize val)))
               (if (typep fixnumized
                          '(or (signed-byte 32) (unsigned-byte 31)))
                   (inst cmp y fixnumized)
                 (progn
                   (inst mov temp fixnumized)
                   (inst cmp y temp))))))
          (symbol
           (inst cmp y (+ nil-value (static-symbol-offset val))))
          (character
           (inst cmp y (logior (ash (char-code val) n-widetag-bits)
                               character-widetag))))))
      (t
       (inst cmp x y)))))

;; The template above is a very good fallback for the generic
;; case.  However, it is sometimes possible to perform unboxed
;; comparisons.  Repurpose char= and eql templates here, instead
;; of forcing values to be boxed and then compared.
;;
;; We only weaken EQL => EQ for characters and fixnums, and detect
;; when types definitely mismatch.  No need to import other EQL
;; VOPs (e.g. floats).
(macrolet ((def (eq-name eql-name cost)
             `(define-vop (,eq-name ,eql-name)
                (:translate eq)
                (:variant-cost ,cost))))
  (def fast-if-eq-character fast-char=/character 3)
  (def fast-if-eq-character/c fast-char=/character/c 2)
  (def fast-if-eq-fixnum fast-eql/fixnum 3)
  (def fast-if-eq-fixnum/c fast-eql-c/fixnum 2)
  (def fast-if-eq-signed fast-if-eql/signed 5)
  (def fast-if-eq-signed/c fast-if-eql-c/signed 4)
  (def fast-if-eq-unsigned fast-if-eql/unsigned 5)
  (def fast-if-eq-unsigned/c fast-if-eql-c/unsigned 4))
