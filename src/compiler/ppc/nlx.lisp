;;; Written by Rob MacLachlan
;;;
(in-package "SB!VM")

;;; MAKE-NLX-SP-TN  --  Interface
;;;
;;;    Make an environment-live stack TN for saving the SP for NLX entry.
;;;
(!def-vm-support-routine make-nlx-sp-tn (env)
  (physenv-live-tn
   (make-representation-tn *fixnum-primitive-type* immediate-arg-scn)
   env))

;;; Make-NLX-Entry-Arg-Start-Location  --  Interface
;;;
;;;    Make a TN for the argument count passing location for a
;;; non-local entry.
;;;
(!def-vm-support-routine make-nlx-entry-arg-start-location ()
  (make-wired-tn *fixnum-primitive-type* immediate-arg-scn ocfp-offset))


;;; Save and restore dynamic environment.
;;;
;;;    These VOPs are used in the reentered function to restore the appropriate
;;; dynamic environment.  Currently we only save the Current-Catch and binding
;;; stack pointer.  We don't need to save/restore the current unwind-protect,
;;; since unwind-protects are implicitly processed during unwinding.  If there
;;; were any additional stacks, then this would be the place to restore the top
;;; pointers.


;;; Make-Dynamic-State-TNs  --  Interface
;;;
;;;    Return a list of TNs that can be used to snapshot the dynamic state for
;;; use with the Save/Restore-Dynamic-Environment VOPs.
;;;
(!def-vm-support-routine make-dynamic-state-tns ()
  (make-n-tns 4 *backend-t-primitive-type*))

(define-vop (save-dynamic-state)
  (:results (catch :scs (descriptor-reg))
	    (nfp :scs (descriptor-reg))
	    (nsp :scs (descriptor-reg))
	    (eval :scs (descriptor-reg)))
  (:vop-var vop)
  (:generator 13
    (load-symbol-value catch *current-catch-block*)
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(move nfp cur-nfp)))
    (move nsp nsp-tn)
    (load-symbol-value eval *eval-stack-top*)))

(define-vop (restore-dynamic-state)
  (:args (catch :scs (descriptor-reg))
	 (nfp :scs (descriptor-reg))
	 (nsp :scs (descriptor-reg))
	 (eval :scs (descriptor-reg)))
  (:vop-var vop)
  (:generator 10
    (store-symbol-value catch *current-catch-block*)
    (store-symbol-value eval *eval-stack-top*)
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(move cur-nfp nfp)))
    (move nsp-tn nsp)))

(define-vop (current-stack-pointer)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    (move res csp-tn)))

(define-vop (current-binding-pointer)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    (move res bsp-tn)))



;;;; Unwind block hackery:

;;; Compute the address of the catch block from its TN, then store into the
;;; block the current Fp, Env, Unwind-Protect, and the entry PC.
;;;
(define-vop (make-unwind-block)
  (:args (tn))
  (:info entry-label)
  (:results (block :scs (any-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 22
    (inst addi block cfp-tn (* (tn-offset tn) sb!vm:n-word-bytes))
    (load-symbol-value temp *current-unwind-protect-block*)
    (storew temp block sb!vm:unwind-block-current-uwp-slot)
    (storew cfp-tn block sb!vm:unwind-block-current-cont-slot)
    (storew code-tn block sb!vm:unwind-block-current-code-slot)
    (inst compute-lra-from-code temp code-tn entry-label ndescr)
    (storew temp block sb!vm:catch-block-entry-pc-slot)))


;;; Like Make-Unwind-Block, except that we also store in the specified tag, and
;;; link the block into the Current-Catch list.
;;;
(define-vop (make-catch-block)
  (:args (tn)
	 (tag :scs (any-reg descriptor-reg)))
  (:info entry-label)
  (:results (block :scs (any-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg) :target block :to (:result 0)) result)
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 44
    (inst addi result cfp-tn (* (tn-offset tn) sb!vm:n-word-bytes))
    (load-symbol-value temp *current-unwind-protect-block*)
    (storew temp result sb!vm:catch-block-current-uwp-slot)
    (storew cfp-tn result sb!vm:catch-block-current-cont-slot)
    (storew code-tn result sb!vm:catch-block-current-code-slot)
    (inst compute-lra-from-code temp code-tn entry-label ndescr)
    (storew temp result sb!vm:catch-block-entry-pc-slot)

    (storew tag result sb!vm:catch-block-tag-slot)
    (load-symbol-value temp *current-catch-block*)
    (storew temp result sb!vm:catch-block-previous-catch-slot)
    (store-symbol-value result *current-catch-block*)

    (move block result)))


;;; Just set the current unwind-protect to TN's address.  This instantiates an
;;; unwind block as an unwind-protect.
;;;
(define-vop (set-unwind-protect)
  (:args (tn))
  (:temporary (:scs (descriptor-reg)) new-uwp)
  (:generator 7
    (inst addi new-uwp cfp-tn (* (tn-offset tn) sb!vm:n-word-bytes))
    (store-symbol-value new-uwp *current-unwind-protect-block*)))


(define-vop (unlink-catch-block)
  (:temporary (:scs (any-reg)) block)
  (:policy :fast-safe)
  (:translate %catch-breakup)
  (:generator 17
    (load-symbol-value block *current-catch-block*)
    (loadw block block sb!vm:catch-block-previous-catch-slot)
    (store-symbol-value block *current-catch-block*)))

(define-vop (unlink-unwind-protect)
  (:temporary (:scs (any-reg)) block)
  (:policy :fast-safe)
  (:translate %unwind-protect-breakup)
  (:generator 17
    (load-symbol-value block *current-unwind-protect-block*)
    (loadw block block sb!vm:unwind-block-current-uwp-slot)
    (store-symbol-value block *current-unwind-protect-block*)))


;;;; NLX entry VOPs:


(define-vop (nlx-entry)
  (:args (sp) ; Note: we can't list an sc-restriction, 'cause any load vops
	      ; would be inserted before the LRA.
	 (start)
	 (count))
  (:results (values :more t))
  (:temporary (:scs (descriptor-reg)) move-temp)
  (:info label nvals)
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:generator 30
    (emit-return-pc label)
    (note-this-location vop :non-local-entry)
    (cond ((zerop nvals))
	  ((= nvals 1)
	   (let ((no-values (gen-label)))
	     (inst cmpwi count 0)
	     (move (tn-ref-tn values) null-tn)
	     (inst beq no-values)
	     (loadw (tn-ref-tn values) start)
	     (emit-label no-values)))
	  (t
	   (collect ((defaults))
	     (inst addic. count count (- (fixnumize 1)))
	     (do ((i 0 (1+ i))
		  (tn-ref values (tn-ref-across tn-ref)))
		 ((null tn-ref))
	       (let ((default-lab (gen-label))
		     (tn (tn-ref-tn tn-ref)))
		 (defaults (cons default-lab tn))
		 
		 (inst subi count count (fixnumize 1))
		 (inst blt default-lab)
		 (sc-case tn
			  ((descriptor-reg any-reg)
			   (loadw tn start i))
			  (control-stack
			   (loadw move-temp start i)
			   (store-stack-tn tn move-temp)))
                 (inst cmpwi count 0)))
	     
	     (let ((defaulting-done (gen-label)))
	       
	       (emit-label defaulting-done)
	       
	       (assemble (*elsewhere*)
		 (dolist (def (defaults))
		   (emit-label (car def))
		   (let ((tn (cdr def)))
		     (sc-case tn
			      ((descriptor-reg any-reg)
			       (move tn null-tn))
			      (control-stack
			       (store-stack-tn tn null-tn)))))
		 (inst b defaulting-done))))))
    (load-stack-tn csp-tn sp)))


(define-vop (nlx-entry-multiple)
  (:args (top :target result) (src) (count))
  ;; Again, no SC restrictions for the args, 'cause the loading would
  ;; happen before the entry label.
  (:info label)
  (:temporary (:scs (any-reg)) dst)
  (:temporary (:scs (descriptor-reg)) temp)
  (:results (result :scs (any-reg) :from (:argument 0))
	    (num :scs (any-reg) :from (:argument 0)))
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:generator 30
    (emit-return-pc label)
    (note-this-location vop :non-local-entry)
    (let ((loop (gen-label))
	  (done (gen-label)))

      ;; Setup results, and test for the zero value case.
      (load-stack-tn result top)
      (inst cmpwi count 0)
      (inst li num 0)
      (inst beq done)

      ;; Compute dst as one slot down from result, because we inc the index
      ;; before we use it.
      (inst subi dst result 4)

      ;; Copy stuff down the stack.
      (emit-label loop)
      (inst lwzx temp src num)
      (inst addi num num (fixnumize 1))
      (inst cmpw num count)
      (inst stwx temp dst num)
      (inst bne loop)

      ;; Reset the CSP.
      (emit-label done)
      (inst add csp-tn result num))))


;;; This VOP is just to force the TNs used in the cleanup onto the stack.
;;;
(define-vop (uwp-entry)
  (:info label)
  (:save-p :force-to-stack)
  (:results (block) (start) (count))
  (:ignore block start count)
  (:vop-var vop)
  (:generator 0
    (emit-return-pc label)
    (note-this-location vop :non-local-entry)))

