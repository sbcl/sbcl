;;;; various primitive memory access VOPs for the x86 VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; data object ref/set stuff

(define-vop (slot)
  (:args (object :scs (descriptor-reg)))
  (:info name offset lowtag)
  (:ignore name)
  (:results (result :scs (descriptor-reg any-reg)))
  (:generator 1
    (loadw result object offset lowtag)))

(define-vop (set-slot)
  (:args (object :scs (descriptor-reg))
	 (value :scs (descriptor-reg any-reg immediate)))
  (:info name offset lowtag)
  (:ignore name)
  (:results)
  (:generator 1
     (if (sc-is value immediate)
	(let ((val (tn-value value)))
	   (etypecase val
	      (integer
	       (inst mov
		     (make-ea :dword :base object
			      :disp (- (* offset n-word-bytes) lowtag))
		     (fixnumize val)))
	      (symbol
	       (inst mov
		     (make-ea :dword :base object
			      :disp (- (* offset n-word-bytes) lowtag))
		     (+ nil-value (static-symbol-offset val))))
	      (character
	       (inst mov
		     (make-ea :dword :base object
			      :disp (- (* offset n-word-bytes) lowtag))
		     (logior (ash (char-code val) n-widetag-bits)
			     base-char-widetag)))))
       ;; Else, value not immediate.
       (storew value object offset lowtag))))



;;;; symbol hacking VOPs

;;; these next two cf the sparc version, by jrd.
;;; FIXME: Deref this ^ reference.


;;; The compiler likes to be able to directly SET symbols.
#!+sb-thread
(define-vop (set)
  (:args (symbol :scs (descriptor-reg))
         (value :scs (descriptor-reg any-reg)))
  (:translate sb!kernel:%set-symbol-value)
  (:temporary (:sc descriptor-reg ) tls)
  ;;(:policy :fast-safe)
  (:generator 4
    (let ((global-val (gen-label))
	  (done (gen-label)))
      (loadw tls symbol symbol-tls-index-slot other-pointer-lowtag)
      (inst or tls tls)
      (inst jmp :z global-val)
      (inst fs-segment-prefix)
      (inst cmp (make-ea :dword :scale 1 :index tls) unbound-marker-widetag)
      (inst jmp :z global-val)
      (inst fs-segment-prefix)
      (inst mov (make-ea :dword :scale 1 :index tls) value)
      (inst jmp done)
      (emit-label global-val)
      (storew value symbol symbol-value-slot other-pointer-lowtag)
      (emit-label done))))

;; unithreaded it's a lot simpler ...
#!-sb-thread 
(define-vop (set cell-set)
  (:variant symbol-value-slot other-pointer-lowtag))

;;; Do a cell ref with an error check for being unbound.
;;; XXX stil used? I can't see where -dan
(define-vop (checked-cell-ref)
  (:args (object :scs (descriptor-reg) :target obj-temp))
  (:results (value :scs (descriptor-reg any-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:save-p :compute-only)
  (:temporary (:sc descriptor-reg :from (:argument 0)) obj-temp))

;;; With Symbol-Value, we check that the value isn't the trap object. So
;;; Symbol-Value of NIL is NIL.
#!+sb-thread
(define-vop (symbol-value)
  (:translate symbol-value)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:result 1)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 9
    (let* ((err-lab (generate-error-code vop unbound-symbol-error object))
	   (ret-lab (gen-label)))
      (loadw value object symbol-tls-index-slot other-pointer-lowtag)
      (inst fs-segment-prefix)
      (inst mov value (make-ea :dword :index value :scale 1))
      (inst cmp value unbound-marker-widetag)
      (inst jmp :ne ret-lab)
      (loadw value object symbol-value-slot other-pointer-lowtag)
      (inst cmp value unbound-marker-widetag)
      (inst jmp :e err-lab)
      (emit-label ret-lab))))

#!+sb-thread
(define-vop (fast-symbol-value symbol-value)
  ;; KLUDGE: not really fast, in fact, because we're going to have to
  ;; do a full lookup of the thread-local area anyway.  But half of
  ;; the meaning of FAST-SYMBOL-VALUE is "do not signal an error if
  ;; unbound", which is used in the implementation of COPY-SYMBOL.  --
  ;; CSR, 2003-04-22
  (:policy :fast)
  (:translate symbol-value)
  (:generator 8
    (let ((ret-lab (gen-label)))
      (loadw value object symbol-tls-index-slot other-pointer-lowtag)
      (inst fs-segment-prefix)
      (inst mov value (make-ea :dword :index value :scale 1))
      (inst cmp value unbound-marker-widetag)
      (inst jmp :ne ret-lab)
      (loadw value object symbol-value-slot other-pointer-lowtag)
      (emit-label ret-lab))))

#!-sb-thread
(define-vop (symbol-value)
  (:translate symbol-value)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:result 1)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 9
    (let ((err-lab (generate-error-code vop unbound-symbol-error object)))
      (loadw value object symbol-value-slot other-pointer-lowtag)
      (inst cmp value unbound-marker-widetag)
      (inst jmp :e err-lab))))

#!-sb-thread
(define-vop (fast-symbol-value cell-ref)
  (:variant symbol-value-slot other-pointer-lowtag)
  (:policy :fast)
  (:translate symbol-value))

(defknown locked-symbol-global-value-add (symbol fixnum) fixnum ())

(define-vop (locked-symbol-global-value-add)
    (:args (object :scs (descriptor-reg) :to :result)
	   (value :scs (any-reg) :target result))
  (:arg-types * tagged-num)
  (:results (result :scs (any-reg) :from (:argument 1)))
  (:policy :fast)
  (:translate locked-symbol-global-value-add)
  (:result-types tagged-num)
  (:policy :fast-safe)
  (:generator 4
    (move result value)
    (inst lock)
    (inst add (make-ea :dword :base object
		       :disp (- (* symbol-value-slot n-word-bytes)
				other-pointer-lowtag))
	  value)))

#!+sb-thread
(define-vop (boundp)
  (:translate boundp)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:conditional)
  (:info target not-p)
  (:temporary (:sc descriptor-reg #+nil(:from (:argument 0))) value)
  (:generator 9
    (if not-p
	(let ((not-target (gen-label)))
	  (loadw value object symbol-value-slot other-pointer-lowtag)
	  (inst cmp value unbound-marker-widetag)
	  (inst jmp :ne not-target)
	  (loadw value object symbol-tls-index-slot other-pointer-lowtag)
	  (inst fs-segment-prefix)
	  (inst cmp (make-ea :dword :index value :scale 1) unbound-marker-widetag)
	  (inst jmp  :e  target)
	  (emit-label not-target))
	(progn
	  (loadw value object symbol-value-slot other-pointer-lowtag)
	  (inst cmp value unbound-marker-widetag)
	  (inst jmp :ne target)
	  (loadw value object symbol-tls-index-slot other-pointer-lowtag)
	  (inst fs-segment-prefix)
	  (inst cmp (make-ea :dword :index value :scale 1) unbound-marker-widetag)
	  (inst jmp  :ne  target)))))

#!-sb-thread
(define-vop (boundp)
  (:translate boundp)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:conditional)
  (:info target not-p)
  (:temporary (:sc descriptor-reg :from (:argument 0)) value)
  (:generator 9
    (loadw value object symbol-value-slot other-pointer-lowtag)
    (inst cmp value unbound-marker-widetag)
    (inst jmp (if not-p :e :ne) target)))


(define-vop (symbol-hash)
  (:policy :fast-safe)
  (:translate symbol-hash)
  (:args (symbol :scs (descriptor-reg)))
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 2
    ;; The symbol-hash slot of NIL holds NIL because it is also the
    ;; cdr slot, so we have to strip off the two low bits to make sure
    ;; it is a fixnum.  The lowtag selection magic that is required to
    ;; ensure this is explained in the comment in objdef.lisp
    (loadw res symbol symbol-hash-slot other-pointer-lowtag)
    (inst and res (lognot #b11))))

;;;; fdefinition (FDEFN) objects

(define-vop (fdefn-fun cell-ref)	; /pfw - alpha
  (:variant fdefn-fun-slot other-pointer-lowtag))

(define-vop (safe-fdefn-fun)
  (:args (object :scs (descriptor-reg) :to (:result 1)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 10
    (loadw value object fdefn-fun-slot other-pointer-lowtag)
    (inst cmp value nil-value)
    (let ((err-lab (generate-error-code vop undefined-fun-error object)))
      (inst jmp :e err-lab))))

(define-vop (set-fdefn-fun)
  (:policy :fast-safe)
  (:translate (setf fdefn-fun))
  (:args (function :scs (descriptor-reg) :target result)
	 (fdefn :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg) raw)
  (:temporary (:sc byte-reg) type)
  (:results (result :scs (descriptor-reg)))
  (:generator 38
    (load-type type function (- fun-pointer-lowtag))
    (inst lea raw
	  (make-ea :byte :base function
		   :disp (- (* simple-fun-code-offset n-word-bytes)
			    fun-pointer-lowtag)))
    (inst cmp type simple-fun-header-widetag)
    (inst jmp :e normal-fn)
    (inst lea raw (make-fixup (extern-alien-name "closure_tramp") :foreign))
    NORMAL-FN
    (storew function fdefn fdefn-fun-slot other-pointer-lowtag)
    (storew raw fdefn fdefn-raw-addr-slot other-pointer-lowtag)
    (move result function)))

(define-vop (fdefn-makunbound)
  (:policy :fast-safe)
  (:translate fdefn-makunbound)
  (:args (fdefn :scs (descriptor-reg) :target result))
  (:results (result :scs (descriptor-reg)))
  (:generator 38
    (storew nil-value fdefn fdefn-fun-slot other-pointer-lowtag)
    (storew (make-fixup (extern-alien-name "undefined_tramp") :foreign)
	    fdefn fdefn-raw-addr-slot other-pointer-lowtag)
    (move result fdefn)))

;;;; binding and unbinding

;;; BIND -- Establish VAL as a binding for SYMBOL. Save the old value and
;;; the symbol on the binding stack and stuff the new value into the
;;; symbol.

#!+sb-thread
(define-vop (bind)
  (:args (val :scs (any-reg descriptor-reg))
	 (symbol :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg) tls-index temp bsp)
  (:generator 5
    (let ((tls-index-valid (gen-label)))
      (load-tl-symbol-value bsp *binding-stack-pointer*)
      (loadw tls-index symbol symbol-tls-index-slot other-pointer-lowtag)
      (inst add bsp (* binding-size n-word-bytes))
      (store-tl-symbol-value bsp *binding-stack-pointer* temp)
      
      (inst or tls-index tls-index)
      (inst jmp :ne tls-index-valid)
      ;; allocate a new tls-index
      (load-symbol-value tls-index *free-tls-index*)
      (inst add tls-index 4)		;XXX surely we can do this more
      (store-symbol-value tls-index *free-tls-index*) ;succintly
      (inst sub tls-index 4)
      (storew tls-index symbol symbol-tls-index-slot other-pointer-lowtag)
      (emit-label tls-index-valid)
      (inst fs-segment-prefix) 
      (inst mov temp (make-ea :dword :scale 1 :index tls-index))
      (storew temp bsp (- binding-value-slot binding-size))
      (storew symbol bsp (- binding-symbol-slot binding-size))
      (inst fs-segment-prefix)
      (inst mov (make-ea :dword :scale 1 :index tls-index) val))))

#!-sb-thread
(define-vop (bind)
  (:args (val :scs (any-reg descriptor-reg))
         (symbol :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg) temp bsp)
  (:generator 5
    (load-symbol-value bsp *binding-stack-pointer*)
    (loadw temp symbol symbol-value-slot other-pointer-lowtag)
    (inst add bsp (* binding-size n-word-bytes))
    (store-symbol-value bsp *binding-stack-pointer*)
    (storew temp bsp (- binding-value-slot binding-size))
    (storew symbol bsp (- binding-symbol-slot binding-size))
    (storew val symbol symbol-value-slot other-pointer-lowtag)))


#!+sb-thread
(define-vop (unbind)
    ;; four temporaries?   
  (:temporary (:sc unsigned-reg) symbol value bsp tls-index)
  (:generator 0
    (load-tl-symbol-value bsp *binding-stack-pointer*)
    (loadw symbol bsp (- binding-symbol-slot binding-size))
    (loadw value bsp (- binding-value-slot binding-size))

    (loadw tls-index symbol symbol-tls-index-slot other-pointer-lowtag)    
    (inst fs-segment-prefix)
    (inst mov (make-ea :dword :scale 1 :index tls-index) value)

    (storew 0 bsp (- binding-symbol-slot binding-size))
    (inst sub bsp (* binding-size n-word-bytes))
    ;; we're done with value, so we can use it as a temp here
    (store-tl-symbol-value bsp *binding-stack-pointer* value)))

#!-sb-thread
(define-vop (unbind)
  (:temporary (:sc unsigned-reg) symbol value bsp)
  (:generator 0
    (load-symbol-value bsp *binding-stack-pointer*)
    (loadw symbol bsp (- binding-symbol-slot binding-size))
    (loadw value bsp (- binding-value-slot binding-size))
    (storew value symbol symbol-value-slot other-pointer-lowtag)
    (storew 0 bsp (- binding-symbol-slot binding-size))
    (inst sub bsp (* binding-size n-word-bytes))
    (store-symbol-value bsp *binding-stack-pointer*)))


(define-vop (unbind-to-here)
  (:args (where :scs (descriptor-reg any-reg)))
  (:temporary (:sc unsigned-reg) symbol value bsp #!+sb-thread tls-index)
  (:generator 0
    (load-tl-symbol-value bsp *binding-stack-pointer*)
    (inst cmp where bsp)
    (inst jmp :e done)

    LOOP
    (loadw symbol bsp (- binding-symbol-slot binding-size))
    (inst or symbol symbol)
    (inst jmp :z skip)
    (loadw value bsp (- binding-value-slot binding-size))
    #!-sb-thread (storew value symbol symbol-value-slot other-pointer-lowtag)

    #!+sb-thread (loadw
		  tls-index symbol symbol-tls-index-slot other-pointer-lowtag)
    #!+sb-thread (inst fs-segment-prefix)
    #!+sb-thread (inst mov (make-ea :dword :scale 1 :index tls-index) value)
    (storew 0 bsp (- binding-symbol-slot binding-size))

    SKIP
    (inst sub bsp (* binding-size n-word-bytes))
    (inst cmp where bsp)
    (inst jmp :ne loop)
    ;; we're done with value, so can use it as a temporary
    (store-tl-symbol-value bsp *binding-stack-pointer* value)

    DONE))



;;;; closure indexing

(define-full-reffer closure-index-ref *
  closure-info-offset fun-pointer-lowtag
  (any-reg descriptor-reg) * %closure-index-ref)

(define-full-setter set-funcallable-instance-info *
  funcallable-instance-info-offset fun-pointer-lowtag
  (any-reg descriptor-reg) * %set-funcallable-instance-info)

(define-full-reffer funcallable-instance-info *
  funcallable-instance-info-offset fun-pointer-lowtag
  (descriptor-reg any-reg) * %funcallable-instance-info)

(define-vop (funcallable-instance-lexenv cell-ref)
  (:variant funcallable-instance-lexenv-slot fun-pointer-lowtag))

(define-vop (closure-ref slot-ref)
  (:variant closure-info-offset fun-pointer-lowtag))

(define-vop (closure-init slot-set)
  (:variant closure-info-offset fun-pointer-lowtag))

;;;; value cell hackery

(define-vop (value-cell-ref cell-ref)
  (:variant value-cell-value-slot other-pointer-lowtag))

(define-vop (value-cell-set cell-set)
  (:variant value-cell-value-slot other-pointer-lowtag))

;;;; structure hackery

(define-vop (instance-length)
  (:policy :fast-safe)
  (:translate %instance-length)
  (:args (struct :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 4
    (loadw res struct 0 instance-pointer-lowtag)
    (inst shr res n-widetag-bits)))

(define-vop (instance-ref slot-ref)
  (:variant instance-slots-offset instance-pointer-lowtag)
  (:policy :fast-safe)
  (:translate %instance-ref)
  (:arg-types instance (:constant index)))

(define-vop (instance-set slot-set)
  (:policy :fast-safe)
  (:translate %instance-set)
  (:variant instance-slots-offset instance-pointer-lowtag)
  (:arg-types instance (:constant index) *))

(define-full-reffer instance-index-ref * instance-slots-offset
  instance-pointer-lowtag (any-reg descriptor-reg) * %instance-ref)

(define-full-setter instance-index-set * instance-slots-offset
  instance-pointer-lowtag (any-reg descriptor-reg) * %instance-set)


(defknown %instance-set-conditional (instance index t t) t
	  (unsafe))

(define-vop (instance-set-conditional)
  (:translate %instance-set-conditional)
  (:args (object :scs (descriptor-reg) :to :eval)
	 (slot :scs (any-reg) :to :result)
	 (old-value :scs (descriptor-reg any-reg) :target eax)
	 (new-value :scs (descriptor-reg any-reg)))
  (:arg-types instance positive-fixnum * *)
  (:temporary (:sc descriptor-reg :offset eax-offset
		   :from (:argument 2) :to :result :target result)  eax)
  (:results (result :scs (descriptor-reg any-reg)))
  ;(:guard (backend-featurep :i486))
  (:policy :fast-safe)
  (:generator 5
    (move eax old-value)
    (inst lock)
    (inst cmpxchg (make-ea :dword :base object :index slot :scale 1
			   :disp (- (* instance-slots-offset n-word-bytes)
				    instance-pointer-lowtag))
	  new-value)
    (move result eax)))



;;;; code object frobbing

(define-full-reffer code-header-ref * 0 other-pointer-lowtag
  (any-reg descriptor-reg) * code-header-ref)

(define-full-setter code-header-set * 0 other-pointer-lowtag
  (any-reg descriptor-reg) * code-header-set)
