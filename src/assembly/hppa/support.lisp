(in-package "SB!VM")


(!def-vm-support-routine generate-call-sequence (name style vop)
  (ecase style
    (:raw
     (let ((fixup (gensym "FIXUP-")))
       (values
	`((let ((fixup (make-fixup ',name :assembly-routine)))
	    (inst ldil fixup ,fixup)
	    (inst ble fixup lisp-heap-space ,fixup :nullify t))
	  (inst nop))
	`((:temporary (:scs (any-reg) :from (:eval 0) :to (:eval 1))
		      ,fixup)))))
    (:full-call
     (let ((temp (make-symbol "TEMP"))
	   (nfp-save (make-symbol "NFP-SAVE"))
	   (lra (make-symbol "LRA")))
       (values
	`((let ((lra-label (gen-label))
		(cur-nfp (current-nfp-tn ,vop)))
	    (when cur-nfp
	      (store-stack-tn ,nfp-save cur-nfp))
	    (inst compute-lra-from-code code-tn lra-label ,temp ,lra)
	    (note-this-location ,vop :call-site)
	    (let ((fixup (make-fixup ',name :assembly-routine)))
	      (inst ldil fixup ,temp)
	      (inst be fixup lisp-heap-space ,temp :nullify t))
	    (emit-return-pc lra-label)
	    (note-this-location ,vop :single-value-return)
	    (move ocfp-tn csp-tn)
	    (inst compute-code-from-lra code-tn lra-label ,temp code-tn)
	    (when cur-nfp
	      (load-stack-tn cur-nfp ,nfp-save))))
	`((:temporary (:scs (non-descriptor-reg) :from (:eval 0) :to (:eval 1))
		      ,temp)
	  (:temporary (:sc descriptor-reg :offset lra-offset
			   :from (:eval 0) :to (:eval 1))
		      ,lra)
	  (:temporary (:scs (control-stack) :offset nfp-save-offset)
		      ,nfp-save)
	  (:save-p :compute-only)))))
    (:none
     (let ((fixup (gensym "FIXUP-")))
       (values
	`((let ((fixup (make-fixup ',name :assembly-routine)))
	    (inst ldil fixup ,fixup)
	    (inst be fixup lisp-heap-space ,fixup :nullify t)))
	`((:temporary (:scs (any-reg) :from (:eval 0) :to (:eval 1))
		      ,fixup)))))))


(!def-vm-support-routine generate-return-sequence (style)
  (ecase style
    (:raw
     `((inst bv lip-tn :nullify t)))
    (:full-call
     `((lisp-return (make-random-tn :kind :normal
				    :sc (sc-or-lose 'descriptor-reg)
				    :offset lra-offset)
		    :offset 1)))
    (:none)))
