;;;; the extra code necessary to feed an entire file of assembly code
;;;; to the assembler

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

(defvar *do-assembly* nil
  #!+sb-doc "If non-NIL, emit assembly code. If NIL, emit VOP templates.")

(defvar *lap-output-file* nil
  #!+sb-doc "the FASL file currently being output to")

(defvar *entry-points* nil
  #!+sb-doc "a list of (name . label) for every entry point")

(defvar *assembly-optimize* t
  #!+sb-doc
  "Set this to NIL to inhibit assembly-level optimization. For compiler
  debugging, rather than policy control.")

;;; Note: You might think from the name that this would act like COMPILE-FILE,
;;; but in fact it's arguably more like LOAD, even down to the return
;;; convention. It LOADs a file, then writes out any assembly code created
;;; by the process.
(defun assemble-file (name
		      &key
		      (output-file (make-pathname :defaults name
						  :type "assem")))
  ;; FIXME: Consider nuking the filename defaulting logic here.
  (let* ((*do-assembly* t)
	 (name (pathname name))
	 (*lap-output-file* (open-fasl-file (pathname output-file) name))
	 (*entry-points* nil)
	 (won nil)
	 (*code-segment* nil)
	 (*elsewhere* nil)
	 (*assembly-optimize* nil)
	 (*fixups* nil))
    (unwind-protect
	(let ((*features* (cons :sb-assembling *features*)))
	  (init-assembler)
	  (load (merge-pathnames name (make-pathname :type "lisp")))
	  (fasl-dump-cold-load-form `(in-package ,(package-name
						   (sane-package)))
				    *lap-output-file*)
	  (sb!assem:append-segment *code-segment* *elsewhere*)
	  (setf *elsewhere* nil)
	  (let ((length (sb!assem:finalize-segment *code-segment*)))
	    (dump-assembler-routines *code-segment*
				     length
				     *fixups*
				     *entry-points*
				     *lap-output-file*))
	  (setq won t))
      (close-fasl-file *lap-output-file* (not won)))
    won))

(defstruct reg-spec
  (kind :temp :type (member :arg :temp :res))
  (name nil :type symbol)
  (temp nil :type symbol)
  (scs nil :type (or list symbol))
  (offset nil))
(def!method print-object ((spec reg-spec) stream)
  (print-unreadable-object (spec stream :type t)
    (format stream
	    ":KIND ~S :NAME ~S :SCS ~S :OFFSET ~S"
	    (reg-spec-kind spec)
	    (reg-spec-name spec)
	    (reg-spec-scs spec)
	    (reg-spec-offset spec))))

(defun reg-spec-sc (spec)
  (if (atom (reg-spec-scs spec))
      (reg-spec-scs spec)
      (car (reg-spec-scs spec))))

(defun parse-reg-spec (kind name sc offset)
  (let ((reg (make-reg-spec :kind kind :name name :scs sc :offset offset)))
    (ecase kind
      (:temp)
      ((:arg :res)
       (setf (reg-spec-temp reg) (make-symbol (symbol-name name)))))
    reg))

(defun emit-assemble (name options regs code)
  (collect ((decls))
    (loop
      (if (and (consp code) (consp (car code)) (eq (caar code) 'declare))
	  (decls (pop code))
	  (return)))
    `(let (,@(mapcar
	      #'(lambda (reg)
		  `(,(reg-spec-name reg)
		    (make-random-tn
		     :kind :normal
		     :sc (sc-or-lose ',(reg-spec-sc reg))
		     :offset ,(reg-spec-offset reg))))
	      regs))
       ,@(decls)
       (sb!assem:assemble (*code-segment* ',name)
	 ,name
	 (push (cons ',name ,name) *entry-points*)
	 ,@code
	 ,@(generate-return-sequence
	    (or (cadr (assoc :return-style options)) :raw)))
       (when sb!xc:*compile-print*
	 (format *error-output* "~S assembled~%" ',name)))))

(defun arg-or-res-spec (reg)
  `(,(reg-spec-name reg)
    :scs ,(if (atom (reg-spec-scs reg))
	      (list (reg-spec-scs reg))
	      (reg-spec-scs reg))
    ,@(unless (eq (reg-spec-kind reg) :res)
	`(:target ,(reg-spec-temp reg)))))

(defun emit-vop (name options vars)
  (let* ((args (remove :arg vars :key #'reg-spec-kind :test-not #'eq))
	 (temps (remove :temp vars :key #'reg-spec-kind :test-not #'eq))
	 (results (remove :res vars :key #'reg-spec-kind :test-not #'eq))
	 (return-style (or (cadr (assoc :return-style options)) :raw))
	 (cost (or (cadr (assoc :cost options)) 247))
	 (vop (make-symbol "VOP")))
    (unless (member return-style '(:raw :full-call :none))
      (error "unknown return-style for ~S: ~S" name return-style))
    (multiple-value-bind
	(call-sequence call-temps)
	(generate-call-sequence name return-style vop)
      `(define-vop ,(if (atom name) (list name) name)
	 (:args ,@(mapcar #'arg-or-res-spec args))
	 ,@(let ((index -1))
	     (mapcar #'(lambda (arg)
			 `(:temporary (:sc ,(reg-spec-sc arg)
					   :offset ,(reg-spec-offset arg)
					   :from (:argument ,(incf index))
					   :to (:eval 2))
				      ,(reg-spec-temp arg)))
		     args))
	 ,@(mapcar #'(lambda (temp)
		       `(:temporary (:sc ,(reg-spec-sc temp)
					 :offset ,(reg-spec-offset temp)
					 :from (:eval 1)
					 :to (:eval 3))
				    ,(reg-spec-name temp)))
		   temps)
	 ,@call-temps
	 (:vop-var ,vop)
	 ,@(let ((index -1))
	     (mapcar #'(lambda (res)
			 `(:temporary (:sc ,(reg-spec-sc res)
					   :offset ,(reg-spec-offset res)
					   :from (:eval 2)
					   :to (:result ,(incf index))
					   :target ,(reg-spec-name res))
				      ,(reg-spec-temp res)))
		     results))
	 (:results ,@(mapcar #'arg-or-res-spec results))
	 (:ignore ,@(mapcar #'reg-spec-name temps)
		  ,@(apply #'append
			   (mapcar #'cdr
				   (remove :ignore call-temps
					   :test-not #'eq :key #'car))))
	 ,@(remove-if #'(lambda (x)
			  (member x '(:return-style :cost)))
		      options
		      :key #'car)
	 (:generator ,cost
	   ,@(mapcar #'(lambda (arg)
			 #!+(or hppa alpha) `(move ,(reg-spec-name arg)
						   ,(reg-spec-temp arg))
			 #!-(or hppa alpha) `(move ,(reg-spec-temp arg)
						   ,(reg-spec-name arg)))
		     args)
	   ,@call-sequence
	   ,@(mapcar #'(lambda (res)
			 #!+(or hppa alpha) `(move ,(reg-spec-temp res)
						   ,(reg-spec-name res))
			 #!-(or hppa alpha) `(move ,(reg-spec-name res)
						   ,(reg-spec-temp res)))
		     results))))))

(def!macro define-assembly-routine (name&options vars &body code)
  (multiple-value-bind (name options)
      (if (atom name&options)
	  (values name&options nil)
	(values (car name&options)
		(cdr name&options)))
    (let ((regs (mapcar #'(lambda (var) (apply #'parse-reg-spec var)) vars)))
      (if *do-assembly*
	  (emit-assemble name options regs code)
	  (emit-vop name options regs)))))
