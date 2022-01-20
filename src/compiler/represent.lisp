;;;; This file contains the implementation-independent code for the
;;;; representation selection phase in the compiler. Representation
;;;; selection decides whether to use non-descriptor representations
;;;; for objects and emits the appropriate representation-specific move
;;;; and coerce vops.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;;; error routines
;;;;
;;;; Problems in the VM definition often show up here, so we try to be
;;;; as implementor-friendly as possible.

;;; Given a TN ref for a VOP argument or result, return these values:
;;; 1. True if the operand is an argument, false otherwise.
;;; 2. The ordinal position of the operand.
;;; 3. True if the operand is a more operand, false otherwise.
;;; 4. The costs for this operand.
;;; 5. The load-scs vector for this operand (NIL if more-p.)
;;; 6. True if the costs or SCs in the VOP-INFO are inconsistent with
;;;    the currently recorded ones.
(defun get-operand-info (ref)
  (declare (type tn-ref ref))
  (let* ((arg-p (not (tn-ref-write-p ref)))
         (vop (tn-ref-vop ref))
         (info (vop-info vop)))
    (flet ((frob (refs costs load more-cost)
             (do ((refs refs (tn-ref-across refs))
                  (costs costs (cdr costs))
                  (load load (cdr load))
                  (n 0 (1+ n)))
                 ((null costs)
                  (aver more-cost)
                  (values arg-p
                          (+ n
                             (or (position-in #'tn-ref-across ref refs)
                                 (error "Couldn't find REF?"))
                             1)
                          t
                          more-cost
                          nil
                          nil))
               (when (eq refs ref)
                 (let ((parse (vop-parse-or-lose (vop-info-name info))))
                   (multiple-value-bind (ccosts cscs)
                       (compute-loading-costs
                        (elt (if arg-p
                                 (vop-parse-args parse)
                                 (vop-parse-results parse))
                             n)
                        arg-p)

                     (return
                      (values arg-p
                              (1+ n)
                              nil
                              (car costs)
                              (car load)
                              (not (and (equalp ccosts (car costs))
                                        (equalp cscs (car load))))))))))))
      (if arg-p
          (frob (vop-args vop) (vop-info-arg-costs info)
                (vop-info-arg-load-scs info)
                (vop-info-more-arg-costs info))
          (frob (vop-results vop) (vop-info-result-costs info)
                (vop-info-result-load-scs info)
                (vop-info-more-result-costs info))))))

;;; Convert a load-costs vector to the list of SCs allowed by the
;;; operand restriction.
(defun listify-restrictions (restr)
  (declare (type sc-vector restr))
  (collect ((res))
    (dotimes (i sb-vm:sc-number-limit)
      (when (eq (svref restr i) t)
        (res (svref *backend-sc-numbers* i))))
    (res)))

;;; Try to give a helpful error message when REF has no cost specified
;;; for some SC allowed by the TN's PRIMITIVE-TYPE.
(defun bad-costs-error (ref)
  (declare (type tn-ref ref))
  (let* ((tn (tn-ref-tn ref))
         (ptype (tn-primitive-type tn)))
    (multiple-value-bind (arg-p pos more-p costs load-scs incon)
        (get-operand-info ref)
      (collect ((losers))
        (dolist (scn (primitive-type-scs ptype))
          (unless (svref costs scn)
            (losers (svref *backend-sc-numbers* scn))))

        (unless (losers)
          (error "Representation selection flamed out for no obvious reason.~@
                  Try again after recompiling the VM definition."))

        (error "~S is not valid as the ~:R ~:[result~;argument~] to the~@
                ~S VOP, since the TN's primitive type ~S allows SCs:~%  ~S~@
                ~:[which cannot be coerced or loaded into the allowed SCs:~
                ~%  ~S~;~*~]~:[~;~@
                Current cost info inconsistent with that in effect at compile ~
                time. Recompile.~%Compilation order may be incorrect.~]"
               tn pos arg-p
               (template-name (vop-info (tn-ref-vop ref)))
               (primitive-type-name ptype)
               (mapcar #'sc-name (losers))
               more-p
               (unless more-p
                 (mapcar #'sc-name (listify-restrictions load-scs)))
               incon)))))

;;; Try to give a helpful error message when we fail to do a coercion
;;; for some reason.
(defun bad-coerce-error (op)
  (declare (type tn-ref op))
  (let* ((op-tn (tn-ref-tn op))
         (op-sc (tn-sc op-tn))
         (op-scn (sc-number op-sc))
         (ptype (tn-primitive-type op-tn))
         (write-p (tn-ref-write-p op)))
    (multiple-value-bind (arg-p pos more-p costs load-scs incon)
        (get-operand-info op)
      (declare (ignore costs more-p))
      (collect ((load-lose)
                (no-move-scs)
                (move-lose))
        (dotimes (i sb-vm:sc-number-limit)
          (let ((i-sc (svref *backend-sc-numbers* i)))
            (when (eq (svref load-scs i) t)
              (cond ((not (sc-allowed-by-primitive-type i-sc ptype))
                     (load-lose i-sc))
                    ((not (find-move-vop op-tn write-p i-sc ptype
                                         #'sc-move-vops))
                     (let ((vops (if write-p
                                     (svref (sc-move-vops op-sc) i)
                                     (svref (sc-move-vops i-sc) op-scn))))
                       (if vops
                           (dolist (vop vops) (move-lose (template-name vop)))
                           (no-move-scs i-sc))))
                    (t
                     (error "Representation selection flamed out for no ~
                             obvious reason."))))))

        (unless (or (load-lose) (no-move-scs) (move-lose))
          (error "Representation selection flamed out for no obvious reason.~@
                  Try again after recompiling the VM definition."))

        (error "~S is not valid as the ~:R ~:[result~;argument~] to VOP:~
                ~%  ~S~%Primitive type: ~S~@
                SC restrictions:~%  ~S~@
                ~@[The primitive type disallows these loadable SCs:~%  ~S~%~]~
                ~@[No move VOPs are defined to coerce to these allowed SCs:~
                ~%  ~S~%~]~
                ~@[These move VOPs couldn't be used due to operand type ~
                restrictions:~%  ~S~%~]~
                ~:[~;~@
                Current cost info inconsistent with that in effect at compile ~
                time. Recompile.~%Compilation order may be incorrect.~]"
               op-tn pos arg-p
               (template-name (vop-info (tn-ref-vop op)))
               (primitive-type-name ptype)
               (mapcar #'sc-name (listify-restrictions load-scs))
               (mapcar #'sc-name (load-lose))
               (mapcar #'sc-name (no-move-scs))
               (move-lose)
               incon)))))

(defun bad-move-arg-error (val pass)
  (declare (type tn val pass))
  (error "no :MOVE-ARG VOP defined to move ~S (SC ~S) to ~
          ~S (SC ~S)"
         val (sc-name (tn-sc val))
         pass (sc-name (tn-sc pass))))

;;;; VM consistency checking
;;;;
;;;; We do some checking of the consistency of the VM definition at
;;;; load time.

;;; FIXME: should probably be conditional on #+SB-SHOW
(defun check-move-fun-consistency ()
  (dotimes (i sb-vm:sc-number-limit)
    (let ((sc (svref *backend-sc-numbers* i)))
      (when sc
        (let ((moves (sc-move-funs sc)))
          (dolist (const (sc-constant-scs sc))
            (unless (svref moves (sc-number const))
              (warn "no move function defined to load SC ~S from constant ~
                     SC ~S"
                    (sc-name sc) (sc-name const))))

          (dolist (alt (sc-alternate-scs sc))
            (unless (svref moves (sc-number alt))
              (warn "no move function defined to load SC ~S from alternate ~
                     SC ~S"
                    (sc-name sc) (sc-name alt)))
            (unless (svref (sc-move-funs alt) i)
              (warn "no move function defined to save SC ~S to alternate ~
                     SC ~S"
                    (sc-name sc) (sc-name alt)))))))))

;;;; representation selection

;;; VOPs that we ignore in initial cost computation. We ignore SET in
;;; the hopes that nobody is setting specials inside of loops. We
;;; ignore TYPE-CHECK-ERROR because we don't want the possibility of
;;; error to bias the result. Notes are suppressed for T-C-E as well,
;;; since we don't need to worry about the efficiency of that case.
(defconstant-eqx ignore-cost-vops '(set type-check-error) #'equal)
(defconstant-eqx suppress-note-vops '(type-check-error) #'equal)

(declaim (start-block select-tn-representation))

;;; We special-case the move VOP, since using this costs for the
;;; normal MOVE would spuriously encourage descriptor representations.
;;; We won't actually need to coerce to descriptor and back, since we
;;; will replace the MOVE with a specialized move VOP. What we do is
;;; look at the other operand. If its representation has already been
;;; chosen (e.g. if it is wired), then we use the appropriate move
;;; costs, otherwise we just ignore the references.
(defun add-representation-costs (tn refs scs costs
                                 ops-slot costs-slot more-costs-slot
                                 write-p)
  (declare (type function ops-slot costs-slot more-costs-slot))
  (do ((ref refs (tn-ref-next ref)))
      ((null ref))
    (flet ((add-costs (cost)
             (dolist (scn scs)
               (let ((res (svref cost scn)))
                 (unless res
                   (bad-costs-error ref))
                 (incf (svref costs scn) res)))))
      (let* ((vop (tn-ref-vop ref))
             (info (vop-info vop)))
        (unless (and (neq (tn-kind tn) :constant)
                     (find (vop-info-name info) ignore-cost-vops))
          (case (vop-info-name info)
            (move
             (let ((rep (tn-sc
                         (tn-ref-tn
                          (if write-p
                              (vop-args vop)
                              (vop-results vop))))))
               (when rep
                 (if write-p
                     (dolist (scn scs)
                       (let ((res (svref (sc-move-costs
                                          (svref *backend-sc-numbers* scn))
                                         (sc-number rep))))
                         (when res
                           (incf (svref costs scn) res))))
                     (dolist (scn scs)
                       (let ((res (svref (sc-move-costs rep) scn)))
                         (when res
                           (incf (svref costs scn) res))))))))
            (t
             (do ((cost (funcall costs-slot info) (cdr cost))
                  (op (funcall ops-slot vop) (tn-ref-across op)))
                 ((null cost)
                  (add-costs (funcall more-costs-slot info)))
               (when (eq op ref)
                 (add-costs (car cost))
                 (return)))))))))
  (values))

;;; Return the best representation for a normal TN. SCs is a list
;;; of the SC numbers of the SCs to select from. Costs is a scratch
;;; vector.
;;;
;;; What we do is sum the costs for each reference to TN in each of
;;; the SCs, and then return the SC having the lowest cost. A second
;;; value is returned which is true when the selection is unique which
;;; is often not the case for the MOVE VOP.
(defun select-tn-representation (tn scs costs)
  (declare (type tn tn) (type sc-vector costs))
  (dolist (scn scs)
    (setf (svref costs scn) 0))

  (add-representation-costs tn (tn-reads tn) scs costs
                            #'vop-args #'vop-info-arg-costs
                            #'vop-info-more-arg-costs
                            nil)
  (add-representation-costs tn (tn-writes tn) scs costs
                            #'vop-results #'vop-info-result-costs
                            #'vop-info-more-result-costs
                            t)


  (let ((min most-positive-fixnum)
        (min-scn nil)
        (unique nil))
    (dolist (scn scs)
      (let ((cost (svref costs scn)))
        (cond ((= cost min)
               (setf unique nil))
              ((< cost min)
               (setq min cost)
               (setq min-scn scn)
               (setq unique t)))))
    (values min-scn unique)))

(declaim (end-block))

;;; Prepare for the possibility of a TN being allocated on the number
;;; stack by setting NUMBER-STACK-P in all functions that TN is
;;; referenced in and in all the functions in their tail sets. REFS is
;;; a TN-REFS list of references to the TN.
(defun note-number-stack-tn (refs)
  (declare (type (or tn-ref null) refs))

  (do ((ref refs (tn-ref-next ref)))
      ((null ref))
    (let* ((lambda (block-home-lambda
                    (ir2-block-block
                     (vop-block (tn-ref-vop ref)))))
           (tails (lambda-tail-set lambda)))
      (flet ((frob (fun)
               (setf (ir2-environment-number-stack-p
                      (environment-info
                       (lambda-environment fun)))
                     t)))
        (frob lambda)
        (when tails
          (dolist (fun (tail-set-funs tails))
            (frob fun))))))

  (values))

;;; If TN is a variable, return the name. If TN is used by a VOP
;;; emitted for a return, then return a string indicating this.
;;; Otherwise, return NIL.
(defun get-operand-name (tn arg-p)
  (declare (type tn tn))
  (let* ((actual (if (eq (tn-kind tn) :alias) (tn-save-tn tn) tn))
         (reads (tn-reads tn))
         (leaf (tn-leaf actual)))
    (cond ((lambda-var-p leaf) (leaf-source-name leaf))
          ((and (not arg-p) reads
                (return-p (vop-node (tn-ref-vop reads))))
           "<return value>")
          (t
           nil))))

;;; If policy indicates, give an efficiency note for doing the
;;; coercion VOP, where OP is the operand we are coercing for and
;;; DEST-TN is the distinct destination in a move.
(defun maybe-emit-coerce-efficiency-note (vop op dest-tn)
  (declare (type vop-info vop) (type tn-ref op) (type (or tn null) dest-tn))
  (let* ((note (or (template-note vop) (template-name vop)))
         (cost (template-cost vop))
         (op-vop (tn-ref-vop op))
         (op-node (vop-node op-vop))
         (op-tn (tn-ref-tn op))
         (*compiler-error-context* op-node))
    (cond ((eq (tn-kind op-tn) :constant))
          ((policy op-node (and (<= speed inhibit-warnings)
                                (<= space inhibit-warnings))))
          ((member (template-name (vop-info op-vop)) suppress-note-vops))
          ((null dest-tn)
           (let* ((op-info (vop-info op-vop))
                  (op-note (or (template-note op-info)
                               (template-name op-info)))
                  (arg-p (not (tn-ref-write-p op)))
                  (name (get-operand-name op-tn arg-p))
                  (pos (1+ (or (position-in #'tn-ref-across op
                                            (if arg-p
                                                (vop-args op-vop)
                                                (vop-results op-vop)))
                               (error "couldn't find op? bug!")))))
             (compiler-notify
              "doing ~A (cost ~W)~:[~2*~; ~:[to~;from~] ~S~], for:~%~6T~
               the ~:R ~:[result~;argument~] of ~A"
              note cost name arg-p name
              pos arg-p op-note)))
          (t
           (compiler-notify "doing ~A (cost ~W)~@[ from ~S~]~@[ to ~S~]"
                            note cost (get-operand-name op-tn t)
                            (get-operand-name dest-tn nil)))))
  (values))

;;; Find a move VOP to move from the operand OP-TN to some other
;;; representation corresponding to OTHER-SC and OTHER-PTYPE. SLOT is
;;; the SC slot that we grab from (move or move-arg). WRITE-P
;;; indicates that OP is a VOP result, so OP is the move result and
;;; other is the arg, otherwise OP is the arg and other is the result.
;;;
;;; If an operand is of primitive type T, then we use the type of the
;;; other operand instead, effectively intersecting the argument and
;;; result type assertions. This way, a move VOP can restrict
;;; whichever operand makes more sense, without worrying about which
;;; operand has the type info.
(defun find-move-vop (op-tn write-p other-sc other-ptype slot)
  (declare (type tn op-tn) (type storage-class other-sc)
           (type primitive-type other-ptype)
           (type function slot))
  (let* ((op-sc (tn-sc op-tn))
         (op-scn (sc-number op-sc))
         (other-scn (sc-number other-sc))
         (any-ptype *backend-t-primitive-type*)
         (op-ptype (tn-primitive-type op-tn)))
    (let ((other-ptype (if (eq other-ptype any-ptype) op-ptype other-ptype))
          (op-ptype (if (eq op-ptype any-ptype) other-ptype op-ptype)))
      (dolist (info (if write-p
                        (svref (funcall slot op-sc) other-scn)
                        (svref (funcall slot other-sc) op-scn))
                    nil)
        (when (and (operand-restriction-ok
                    (first (template-arg-types info))
                    (if write-p other-ptype op-ptype)
                    :tn op-tn :t-ok nil)
                   (operand-restriction-ok
                    (first (template-result-types info))
                    (if write-p op-ptype other-ptype)
                    :t-ok nil))
          (return info))))))

;;; Emit a coercion VOP for OP BEFORE the specified VOP or die trying.
;;; SCS is the operand's LOAD-SCS vector, which we use to determine
;;; what SCs the VOP will accept. We pick any acceptable coerce VOP,
;;; since it practice it seems uninteresting to have more than one
;;; applicable.
;;;
;;; On the X86 port, stack SCs may be placed in the list of operand
;;; preferred SCs, and to prevent these stack SCs being selected when
;;; a register SC is available the non-stack SCs are searched first.
;;;
;;; What we do is look at each SC allowed by both the operand
;;; restriction and the operand primitive-type, and see whether there
;;; is a move VOP which moves between the operand's SC and load SC. If
;;; we find such a VOP, then we make a TN having the load SC as the
;;; representation.
;;;
;;; DEST-TN is the TN that we are moving to, for a move or move-arg.
;;; This is only for efficiency notes.
;;;
;;; If the TN is an unused result TN, then we don't actually emit the
;;; move; we just change to the right kind of TN.
(defun emit-coerce-vop (op dest-tn scs before)
  (declare (type tn-ref op) (type sc-vector scs) (type (or vop null) before)
           (type (or tn null) dest-tn))
  (let* ((op-tn (tn-ref-tn op))
         (ptype (tn-primitive-type op-tn))
         (write-p (tn-ref-write-p op))
         (vop (tn-ref-vop op))
         (node (vop-node vop))
         (block (vop-block vop)))
    (labels ((emit-move (vop x y)
               (when (>= (vop-info-cost vop)
                         *efficiency-note-cost-threshold*)
                 (maybe-emit-coerce-efficiency-note vop op dest-tn))
               (emit-move-template node block vop x y before))
             (check-sc (scn sc)
               (when (sc-allowed-by-primitive-type sc ptype)
                 (let ((res (find-move-vop op-tn write-p sc ptype
                                           #'sc-move-vops)))
                   (when res
                     (let ((temp (make-representation-tn ptype scn)))
                       (change-tn-ref-tn op temp)
                       (cond
                         ((not write-p)
                          (or
                           (coerce-from-constant op temp)
                           (emit-move (or (maybe-move-from-fixnum+-1 op-tn temp
                                                                     op)
                                          res)
                                      op-tn temp)))
                         ((and (null (tn-reads op-tn))
                               (eq (tn-kind op-tn) :normal)))
                         (t
                          (emit-move (or (maybe-move-from-fixnum+-1 temp op-tn op)
                                         res)
                                     temp op-tn))))
                     t)))))
      ;; Search the non-stack load SCs first.
      (dotimes (scn sb-vm:sc-number-limit)
        (let ((sc (svref *backend-sc-numbers* scn)))
          (when (and (eq (svref scs scn) t)
                     (not (eq (sb-kind (sc-sb sc)) :unbounded))
                     (check-sc scn sc))
            (return-from emit-coerce-vop))))
      ;; Search the stack SCs if the above failed.
      (dotimes (scn sb-vm:sc-number-limit (bad-coerce-error op))
        (let ((sc (svref *backend-sc-numbers* scn)))
          (when (and (eq (svref scs scn) t)
                     (eq (sb-kind (sc-sb sc)) :unbounded)
                     (check-sc scn sc))
            (return)))))))

;;; Scan some operands and call EMIT-COERCE-VOP on any for which we
;;; can't load the operand. The coerce VOP is inserted Before the
;;; specified VOP. Dest-TN is the destination TN if we are doing a
;;; move or move-arg, and is NIL otherwise. This is only used for
;;; efficiency notes.
(defun coerce-some-operands (ops dest-tn load-scs before)
  (declare (type (or tn-ref null) ops) (list load-scs)
           (type (or tn null) dest-tn) (type (or vop null) before))
  (do ((op ops (tn-ref-across op))
       (scs load-scs (cdr scs)))
      ((null scs))
    (let ((tn (tn-ref-tn op)))
      (unless (or (eq (tn-kind tn) :unused)
                  (svref (car scs)
                         (sc-number (tn-sc tn))))
        (emit-coerce-vop op dest-tn (car scs) before))))
  (values))

;;; Emit coerce VOPs for the args and results, as needed.
(defun coerce-vop-operands (vop)
  (declare (type vop vop))
  (let ((info (vop-info vop)))
    (coerce-some-operands (vop-args vop) nil (vop-info-arg-load-scs info) vop)
    (coerce-some-operands (vop-results vop) nil (vop-info-result-load-scs info)
                          (vop-next vop)))
  (values))

;;; Iterate over the more operands to a call VOP, emitting move-arg
;;; VOPs and any necessary coercions. We determine which FP to use by
;;; looking at the MOVE-ARGS annotation. If the vop is a :LOCAL-CALL,
;;; we insert any needed coercions before the ALLOCATE-FRAME so that
;;; lifetime analysis doesn't get confused (since otherwise, only
;;; passing locations are written between A-F and call.)
(defun emit-arg-moves (vop)
  (let* ((info (vop-info vop))
         (node (vop-node vop))
         (block (vop-block vop))
         (how (vop-info-move-args info))
         (args (vop-args vop))
         (fp-tn (tn-ref-tn args))
         (nfp-tn (if (eq how :local-call)
                     (tn-ref-tn (tn-ref-across args))
                     nil))
         (pass-locs (first (vop-codegen-info vop)))
         (prev (vop-prev vop)))
    (do ((val (do ((arg args (tn-ref-across arg))
                   (req (template-arg-types info) (cdr req)))
                  ((null req) arg))
              (tn-ref-across val))
         (pass pass-locs (cdr pass)))
        ((null val)
         (aver (null pass)))
      (let* ((val-tn (tn-ref-tn val))
             (pass-tn (first pass))
             (pass-sc (tn-sc pass-tn))
             (res (find-move-vop val-tn nil pass-sc
                                 (tn-primitive-type pass-tn)
                                 #'sc-move-arg-vops)))
        (unless res
          (bad-move-arg-error val-tn pass-tn))

        (change-tn-ref-tn val pass-tn)
        (let* ((this-fp
                (cond ((not (sc-number-stack-p pass-sc)) fp-tn)
                      (nfp-tn)
                      (t
                       (aver (eq how :known-return))
                       (setq nfp-tn (make-number-stack-pointer-tn))
                       (setf (tn-sc nfp-tn)
                             (svref *backend-sc-numbers*
                                    (first (primitive-type-scs
                                            (tn-primitive-type nfp-tn)))))
                       (emit-context-template
                        node block
                        (template-or-lose 'compute-old-nfp)
                        nfp-tn vop)
                       (aver (not (sc-number-stack-p (tn-sc nfp-tn))))
                       nfp-tn)))
               (new (emit-move-arg-template node block res val-tn this-fp
                                            pass-tn vop))
               (after
                (cond ((eq how :local-call)
                       (aver (eq (vop-name prev) 'allocate-frame))
                       prev)
                      (prev (vop-next prev))
                      (t
                       (ir2-block-start-vop block)))))
          (coerce-some-operands (vop-args new) pass-tn
                                (vop-info-arg-load-scs res)
                                after)))))
  (values))

(defun maybe-move-from-fixnum+-1 (x y &optional x-tn-ref)
  (when (and (sc-is y sb-vm::descriptor-reg)
             (sc-is x sb-vm::signed-reg sb-vm::unsigned-reg))
    (let ((type (tn-ref-type x-tn-ref)))
      (cond ((not type)
             nil)
            ((csubtypep type (specifier-type 'fixnum))
             (template-or-lose 'sb-vm::move-from-word/fixnum))
            ((csubtypep type
                        (specifier-type `(integer ,most-negative-fixnum
                                                  ,(1+ most-positive-fixnum))))
             (template-or-lose 'sb-vm::move-from-fixnum+1))
            ((csubtypep type
                        (specifier-type `(integer ,(1- most-negative-fixnum)
                                                  ,most-positive-fixnum)))
             (template-or-lose 'sb-vm::move-from-fixnum-1))))))

(defun coerce-from-constant (x-tn-ref y)
  (when (and (sc-is y sb-vm::descriptor-reg sb-vm::control-stack)
             (tn-ref-type x-tn-ref))
    (multiple-value-bind (constantp value) (type-singleton-p (tn-ref-type x-tn-ref))
      (when constantp
        (change-tn-ref-tn x-tn-ref
                          (make-constant-tn (find-constant value) t))
        t))))

(defun split-ir2-block (vop)
  (cond ((vop-next vop)
         (with-ir1-environment-from-node (vop-node vop)
           (let* ((2block (vop-block vop))
                  (succ (ir2-block-block 2block))
                  (start (make-ctran))
                  (block (ctran-starts-block start))
                  (no-op-node (make-exit))
                  (new-2block (make-ir2-block block))
                  (vop-next (vop-next vop)))
             (link-node-to-previous-ctran no-op-node start)
             (setf (block-info block) new-2block)
             (add-to-emit-order new-2block (ir2-block-prev 2block))
             (loop for pred-block in (block-pred succ)
                   do
                   (change-block-successor pred-block succ block))
             (setf (block-last block) no-op-node)
             (setf (ir2-block-start-vop new-2block) vop
                   (ir2-block-last-vop new-2block) vop
                   (ir2-block-start-vop 2block) vop-next)

             (shiftf (ir2-block-%label new-2block)
                     (ir2-block-%label 2block)
                     (gen-label))
             (shiftf (ir2-block-%trampoline-label new-2block)
                     (ir2-block-%trampoline-label 2block)
                     nil)
             (setf (vop-block vop) new-2block
                   (vop-next vop) nil
                   (vop-prev vop-next) nil)
             (link-blocks block succ)
             2block)))
        (t
         (let* ((2block (vop-block vop))
                (next (ir2-block-next 2block)))
           (unless (ir2-block-%label next)
             (setf (ir2-block-%label next) (gen-label)))
           next))))

;;; If a MOVE about to be coerced is going to another MOVE, the
;;; result of which is compatible with the original TN, jump directly
;;; after that move without performing any coercions.
(defun jump-over-move-coercion (vop x y block)
  (let* ((reads (tn-reads y))
         (dest-vop (and reads
                        (not (tn-ref-next reads))
                        (tn-ref-vop reads)))
         branch)
    (when (and dest-vop
               (vop-info-move-vop-p (vop-info dest-vop))
               (eq (ir2-block-start-vop (vop-block dest-vop)) dest-vop)
               (let ((last (ir2-block-last-vop block))
                     (dest-block (vop-block dest-vop)))
                 (or (and (eq last vop)
                          (eq (ir2-block-next block) dest-block))
                     (and last
                          (eq (vop-next vop) last)
                          (eq (vop-name last) 'branch)
                          (eq (car (vop-codegen-info last)) (ir2-block-%label dest-block))
                          (setf branch last)))))
      (let ((dest (tn-ref-tn (vop-results dest-vop))))
        (when (and (eq (tn-sc x) (tn-sc dest))
                   (eq (find-move-vop x nil (tn-sc dest) (tn-primitive-type dest) #'sc-move-vops)
                       (vop-info vop)))
          (let ((new-block (split-ir2-block dest-vop))
                (1block (ir2-block-block block)))
            (if branch
                (setf (vop-codegen-info branch) (list (ir2-block-%label new-block)))
                (emit-and-insert-vop (vop-node vop)
                                     block
                                     (template-or-lose 'branch)
                                     nil
                                     nil
                                     nil
                                     (list (ir2-block-%label new-block))))
            (change-tn-ref-tn (vop-results vop) dest)
            (change-block-successor 1block (car (block-succ 1block))
                                    (ir2-block-block new-block))
            t))))))

;;; Scan the IR2 looking for move operations that need to be replaced
;;; with special-case VOPs and emitting coercion VOPs for operands of
;;; normal VOPs. We delete moves to TNs that are never read at this
;;; point, rather than possibly converting them to some expensive move
;;; operation.
(defun emit-moves-and-coercions (block)
  (declare (type ir2-block block))
  (do ((vop (ir2-block-start-vop block)
            (vop-next vop)))
      ((null vop))
    (let ((info (vop-info vop))
          (node (vop-node vop)))
      (cond
        ((eq (vop-info-name info) 'move)
         (let* ((args (vop-args vop))
                (x (tn-ref-tn args))
                (y (tn-ref-tn (vop-results vop)))
                (res (find-move-vop x nil (tn-sc y) (tn-primitive-type y)
                                    #'sc-move-vops)))

           (cond ((and (null (tn-reads y))
                       (eq (tn-kind y) :normal))
                  (delete-vop vop))
                 ((eq res info))
                 ((coerce-from-constant args y))
                 (res
                  (or
                   (jump-over-move-coercion vop x y block)
                   (let ((res (or (maybe-move-from-fixnum+-1 x y
                                                             args)
                                  res)))
                     (when (>= (vop-info-cost res)
                               *efficiency-note-cost-threshold*)
                       (maybe-emit-coerce-efficiency-note res args y))
                     (emit-move-template node (vop-block vop) res x y vop)
                     (delete-vop vop))))
                 (t
                  (coerce-vop-operands vop)))))
        ((vop-info-move-args info)
         (emit-arg-moves vop))
        (t
         (coerce-vop-operands vop)))
      (when (vop-info-after-sc-selection info)
        (funcall (vop-info-after-sc-selection info) vop)))))

;;; If TN is in a number stack SC, make all the right annotations.
;;; Note that this should be called after TN has been referenced,
;;; since it must iterate over the referencing environments.
(declaim (inline note-if-number-stack))
(defun note-if-number-stack (tn 2comp restricted)
  (declare (type tn tn) (type ir2-component 2comp))
  (when (if restricted
            (eq (sb-name (sc-sb (tn-sc tn))) 'non-descriptor-stack)
            (sc-number-stack-p (tn-sc tn)))
    (unless (ir2-component-nfp 2comp)
      (setf (ir2-component-nfp 2comp) (make-nfp-tn)))
    (note-number-stack-tn (tn-reads tn))
    (note-number-stack-tn (tn-writes tn)))
  (values))

;;; Arrange boxed constants so that all :NAMED-CALL constants are first,
;;; then constant leaves, and finally LOAD-TIME-VALUE constants.
;;; There exist a few reasons for placing all the FDEFNs first:
;;;  * FDEFNs which are referenced for lisp call - as opposed to referenced
;;;    in #'FUN syntax - could be stored as untagged pointers which would
;;;    benefit the PPC64 architecture by removing a few instructions from each
;;;    use of such fdefn by not having to subtract its lowtag prior to loading
;;;    from both the fun and raw-fun slots. GC would need to be aware of the
;;;    untagged pointer convention.
;;;  * In the current approach for so-called "static" linking of immobile code,
;;;    we change code instruction bytes so that they call into a simple-fun
;;;    directly rather than through an fdefn, but the approach is subject to a
;;;    data race when redefining an fdefn. It's conceivable that the race can be
;;;    eliminated by substituting placeholders in the code headers of functions
;;;    that had static linking performed - so that they see a reference to the
;;;    callee rather than an fdefn - but in order for that to work, we must
;;;    distinguish between fdefns that are needed for FDEFN-FUN
;;;    (via IR2-CONVERT-GLOBAL-VAR) versus those which are present to satisfy
;;;    a GC invariant and are not otherwise actually used.
;;;  * Even without the preceding change, remove-static-links can avoid
;;;    scanning code constants that are not FDEFNs.
(defun sort-boxed-constants (2comp)
  (let* ((sorted (ir2-component-constants 2comp))
         (unsorted (subseq sorted 1))
         (renumbering)) ; alist of (old . new) indices into constant vector
    (setf (fill-pointer sorted) 0)
    ;; add in fixed overhead
    (let ((n-entries (length (ir2-component-entries 2comp))))
      (dotimes (i (+ (* sb-vm:code-slots-per-simple-fun n-entries)
                     sb-vm:code-constants-offset))
        (vector-push-extend nil sorted)))
    (flet ((scan (pass &aux (old-offset 0))
             (dovector (constant unsorted)
               (incf old-offset)
               (when (eql pass (cond ((constant-p constant) 2)
                                     ((eq (car constant) :named-call) 1)
                                     (t 3)))
                 (let ((new-offset (vector-push-extend constant sorted)))
                   (push (cons old-offset new-offset) renumbering))))))
      (scan 1)  ; first all the called fdefinitions
      (scan 2)  ; then IR1 constants
      (scan 3)) ; then various flavors of load-time magic
    ;; Update the TN-OFFSET slot.
    ;; There can be more than one TN with the same index into the constants
    ;; because of how MAKE-LOAD-TIME-CONSTANT-TN works.
    (do ((tn (ir2-component-constant-tns 2comp) (tn-next tn)))
        ((null tn))
      (let ((old-offset (tn-offset tn)))
        (when old-offset
          (setf (tn-offset tn)
                (cdr (the (not null) (assoc old-offset renumbering)))))))
    sorted))

#+arm64
(defun choose-zero-tn (tn)
  (let (zero-tn)
    (flet ((zero-tn ()
             (or zero-tn
                 (setf zero-tn
                       (component-live-tn
                        (make-wired-tn nil
                                       sb-vm:any-reg-sc-number
                                       sb-vm::zr-offset))))))
      (do ((tn tn (tn-next tn)))
          ((null tn))
        (when (and (constant-tn-p tn)
                   (eql (tn-value tn) 0))
          (loop with next
                for read = (tn-reads tn) then next
                while read
                do
                (setf next (tn-ref-next read))
                (do* ((vop (tn-ref-vop read))
                      (info (vop-info vop))
                      (cost (vop-info-arg-costs info) (cdr cost))
                      (op (vop-args vop) (tn-ref-across op)))
                     ((null cost))
                  (when (eq op read)
                    (when (eql (svref (car cost) sb-vm:zero-sc-number) 0)
                      (change-tn-ref-tn read (zero-tn)))
                    (return)))))))))

;;; The call VOPs allocate a temporary register wired to
;;; nfp-save-offset, but don't use it if there's no nfp-tn in the
;;; current frame, but the stack space is still allocated.
#-c-stack-is-control-stack
(defun unwire-nfp-save-tn (2comp)
  (unless (ir2-component-nfp 2comp)
    (do ((prev)
         (tn (ir2-component-wired-tns 2comp) (tn-next tn)))
        ((null tn))
      (cond ((and (sc-is tn sb-vm::control-stack)
                  (eql (tn-offset tn) sb-vm:nfp-save-offset))
             (setf (tn-kind tn) :unused)
             (if prev
                 (setf (tn-next prev) (tn-next tn))
                 (setf (ir2-component-wired-tns 2comp) (tn-next tn))))
            (t
             (setf prev tn))))))

;;; This is the entry to representation selection. First we select the
;;; representation for all normal TNs, setting the TN-SC. After
;;; selecting the TN representations, we set the SC for all :ALIAS TNs
;;; to be the representation chosen for the original TN. We then scan
;;; all the IR2, emitting any necessary coerce and move-arg VOPs.
;;; Finally, we scan all TNs looking for ones that might be placed on
;;; the number stack, noting this so that the number-FP can be
;;; allocated. This must be done last, since references in new
;;; environments may be introduced by MOVE-ARG insertion.
(defun select-representations (component)
  (let ((costs (make-array sb-vm:sc-number-limit))
        (2comp (component-info component)))
    (sort-boxed-constants 2comp)
    (labels ((set-sc (tn sc)
               (cond ((neq (tn-kind tn) :constant)
                      (setf (tn-sc tn)
                            (svref *backend-sc-numbers* sc)))
                     ;; Translate primitive type scs into constant scs
                     ((= sc sb-vm:descriptor-reg-sc-number)
                      (setf (tn-sc tn)
                            (svref *backend-sc-numbers* sb-vm:constant-sc-number)
                            (tn-offset tn)
                            (or (position (tn-leaf tn)
                                          (ir2-component-constants 2comp))
                                (vector-push-extend (tn-leaf tn)
                                                    (ir2-component-constants 2comp)))))
                     (t
                      (setf (tn-sc tn)
                            (svref *backend-sc-numbers*
                                   (immediate-constant-sc (constant-value (tn-leaf tn))))))))
             (possible-scs (tn)
               (primitive-type-scs (tn-primitive-type tn)))
             (pass (tn &key unique)
               (do ((tn tn (tn-next tn)))
                   ((null tn))
                 (aver (tn-primitive-type tn))
                 (unless (tn-sc tn)
                   (let ((scs (possible-scs tn)))
                     (cond ((rest scs)
                            (multiple-value-bind (sc uniquep)
                                (select-tn-representation tn scs costs)
                              (when (or (not unique)
                                        uniquep)
                                (set-sc tn sc))))
                           (t
                            (set-sc tn (first scs)))))))))

      ;; First pass; only allocate SCs where there is a distinct choice.
      (pass (ir2-component-normal-tns 2comp) :unique t)
      (pass (ir2-component-normal-tns 2comp))
      (pass (ir2-component-constant-tns 2comp)))

    (do ((alias (ir2-component-alias-tns 2comp)
                (tn-next alias)))
        ((null alias))
      (setf (tn-sc alias) (tn-sc (tn-save-tn alias))))

    (do-ir2-blocks (block component)
      (emit-moves-and-coercions block))

    #+arm64
    (choose-zero-tn (ir2-component-constant-tns 2comp))

    ;; Give the optimizers a second opportunity to alter newly inserted vops
    ;; by looking for patterns that have a shorter expression as a single vop.
    (run-vop-optimizers component)

    (macrolet ((frob (slot restricted)
                 `(do ((tn (,slot 2comp) (tn-next tn)))
                      ((null tn))
                    (note-if-number-stack tn 2comp ,restricted))))
      (frob ir2-component-normal-tns nil)
      (frob ir2-component-wired-tns t)
      (frob ir2-component-restricted-tns t)
      #-c-stack-is-control-stack
      (unwire-nfp-save-tn 2comp)))
  (values))
