;;;; stuff that creates debugger information from the compiler's
;;;; internal data structures

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

(deftype byte-buffer () '(vector (unsigned-byte 8)))
(defvar *byte-buffer*)
(declaim (type byte-buffer *byte-buffer*))

;;;; debug blocks

(deftype location-kind ()
  '(member :unknown-return :known-return :internal-error :non-local-exit
	   :block-start :call-site :single-value-return :non-local-entry))

;;; The LOCATION-INFO structure holds the information what we need
;;; about locations which code generation decided were "interesting".
(defstruct (location-info
	    (:constructor make-location-info (kind label vop))
	    (:copier nil))
  ;; The kind of location noted.
  (kind nil :type location-kind)
  ;; The label pointing to the interesting code location.
  (label nil :type (or label index null))
  ;; The VOP that emitted this location (for node, save-set, ir2-block, etc.)
  (vop nil :type vop))

;;; This is called during code generation in places where there is an
;;; "interesting" location: someplace where we are likely to end up
;;; in the debugger, and thus want debug info.
(defun note-debug-location (vop label kind)
  (declare (type vop vop) (type (or label null) label)
	   (type location-kind kind))
  (let ((location (make-location-info kind label vop)))
    (setf (ir2-block-locations (vop-block vop))
	  (nconc (ir2-block-locations (vop-block vop))
		 (list location)))
    location))

#!-sb-fluid (declaim (inline ir2-block-environment))
(defun ir2-block-environment (2block)
  (declare (type ir2-block 2block))
  (block-environment (ir2-block-block 2block)))

;;; Given a local conflicts vector and an IR2 block to represent the
;;; set of live TNs, and the VAR-LOCS hash-table representing the
;;; variables dumped, compute a bit-vector representing the set of
;;; live variables. If the TN is environment-live, we only mark it as
;;; live when it is in scope at NODE.
(defun compute-live-vars (live node block var-locs vop)
  (declare (type ir2-block block) (type local-tn-bit-vector live)
	   (type hash-table var-locs) (type node node)
	   (type (or vop null) vop))
  (let ((res (make-array (logandc2 (+ (hash-table-count var-locs) 7) 7)
			 :element-type 'bit
			 :initial-element 0))
	(spilled (gethash vop
			  (ir2-component-spilled-vops
			   (component-info *component-being-compiled*)))))
    (do-live-tns (tn live block)
      (let ((leaf (tn-leaf tn)))
	(when (and (lambda-var-p leaf)
		   (or (not (member (tn-kind tn)
				    '(:environment :debug-environment)))
		       (rassoc leaf (lexenv-variables (node-lexenv node))))
		   (or (null spilled)
		       (not (member tn spilled))))
	  (let ((num (gethash leaf var-locs)))
	    (when num
	      (setf (sbit res num) 1))))))
    res))

;;; The PC for the location most recently dumped.
(defvar *previous-location*)
(declaim (type index *previous-location*))

;;; Dump a compiled debug-location into *BYTE-BUFFER* that describes
;;; the code/source map and live info. If true, VOP is the VOP
;;; associated with this location, for use in determining whether TNs
;;; are spilled.
(defun dump-1-location (node block kind tlf-num label live var-locs vop)
  (declare (type node node) (type ir2-block block)
	   (type local-tn-bit-vector live)
	   (type (or label index) label)
	   (type location-kind kind) (type (or index null) tlf-num)
	   (type hash-table var-locs) (type (or vop null) vop))

  (vector-push-extend
   (dpb (position-or-lose kind *compiled-code-location-kinds*)
	compiled-code-location-kind-byte
	0)
   *byte-buffer*)

  (let ((loc (if (fixnump label) label (label-position label))))
    (write-var-integer (- loc *previous-location*) *byte-buffer*)
    (setq *previous-location* loc))

  (let ((path (node-source-path node)))
    (unless tlf-num
      (write-var-integer (source-path-tlf-number path) *byte-buffer*))
    (write-var-integer (source-path-form-number path) *byte-buffer*))

  (write-packed-bit-vector (compute-live-vars live node block var-locs vop)
			   *byte-buffer*)

  (values))

;;; Extract context info from a Location-Info structure and use it to
;;; dump a compiled code-location.
(defun dump-location-from-info (loc tlf-num var-locs)
  (declare (type location-info loc) (type (or index null) tlf-num)
	   (type hash-table var-locs))
  (let ((vop (location-info-vop loc)))
    (dump-1-location (vop-node vop)
		     (vop-block vop)
		     (location-info-kind loc)
		     tlf-num
		     (location-info-label loc)
		     (vop-save-set vop)
		     var-locs
		     vop))
  (values))

;;; Scan all the blocks, determining if all locations are in the same
;;; TLF, and returning it or NIL.
(defun find-tlf-number (fun)
  (declare (type clambda fun))
  (let ((res (source-path-tlf-number (node-source-path (lambda-bind fun)))))
    (declare (type (or index null) res))
    (do-environment-ir2-blocks (2block (lambda-environment fun))
      (let ((block (ir2-block-block 2block)))
	(when (eq (block-info block) 2block)
	  (unless (eql (source-path-tlf-number
			(node-source-path
			 (continuation-next
			  (block-start block))))
		       res)
	    (setq res nil)))
	
	(dolist (loc (ir2-block-locations 2block))
	  (unless (eql (source-path-tlf-number
			(node-source-path
			 (vop-node (location-info-vop loc))))
		       res)
	    (setq res nil)))))
    res))

;;; Dump out the number of locations and the locations for Block.
(defun dump-block-locations (block locations tlf-num var-locs)
  (declare (type cblock block) (list locations))
  (if (and locations
	   (eq (location-info-kind (first locations))
	       :non-local-entry))
      (write-var-integer (length locations) *byte-buffer*)
      (let ((2block (block-info block)))
	(write-var-integer (+ (length locations) 1) *byte-buffer*)
	(dump-1-location (continuation-next (block-start block))
			 2block :block-start tlf-num
			 (ir2-block-%label 2block)
			 (ir2-block-live-out 2block)
			 var-locs
			 nil)))
  (dolist (loc locations)
    (dump-location-from-info loc tlf-num var-locs))
  (values))

;;; Dump the successors of Block, being careful not to fly into space on
;;; weird successors.
(defun dump-block-successors (block env)
  (declare (type cblock block) (type environment env))
  (let* ((tail (component-tail (block-component block)))
	 (succ (block-succ block))
	 (valid-succ
	  (if (and succ
		   (or (eq (car succ) tail)
		       (not (eq (block-environment (car succ)) env))))
	      ()
	      succ)))
    (vector-push-extend
     (dpb (length valid-succ) compiled-debug-block-nsucc-byte 0)
     *byte-buffer*)
    (let ((base (block-number
		 (node-block
		  (lambda-bind (environment-function env))))))
      (dolist (b valid-succ)
	(write-var-integer
	 (the index (- (block-number b) base))
	 *byte-buffer*))))
  (values))

;;; Return a vector and an integer (or null) suitable for use as the
;;; BLOCKS and TLF-NUMBER in FUN's debug-function. This requires two
;;; passes to compute:
;;; -- Scan all blocks, dumping the header and successors followed
;;;    by all the non-elsewhere locations.
;;; -- Dump the elsewhere block header and all the elsewhere 
;;;    locations (if any.)
(defun compute-debug-blocks (fun var-locs)
  (declare (type clambda fun) (type hash-table var-locs))
  (setf (fill-pointer *byte-buffer*) 0)
  (let ((*previous-location* 0)
	(tlf-num (find-tlf-number fun))
	(env (lambda-environment fun))
	(prev-locs nil)
	(prev-block nil))
    (collect ((elsewhere))
      (do-environment-ir2-blocks (2block env)
	(let ((block (ir2-block-block 2block)))
	  (when (eq (block-info block) 2block)
	    (when prev-block
	      (dump-block-locations prev-block prev-locs tlf-num var-locs))
	    (setq prev-block block  prev-locs ())
	    (dump-block-successors block env)))
	
	(collect ((here prev-locs))
	  (dolist (loc (ir2-block-locations 2block))
	    (if (label-elsewhere-p (location-info-label loc))
		(elsewhere loc)
		(here loc)))
	  (setq prev-locs (here))))

      (dump-block-locations prev-block prev-locs tlf-num var-locs)

      (when (elsewhere)
	(vector-push-extend compiled-debug-block-elsewhere-p *byte-buffer*)
	(write-var-integer (length (elsewhere)) *byte-buffer*)
	(dolist (loc (elsewhere))
	  (dump-location-from-info loc tlf-num var-locs))))

    (values (copy-seq *byte-buffer*) tlf-num)))

;;; Return a list of DEBUG-SOURCE structures containing information
;;; derived from INFO. Unless :BYTE-COMPILE T was specified, we always
;;; dump the Start-Positions, since it is too hard figure out whether
;;; we need them or not.
(defun debug-source-for-info (info)
  (declare (type source-info info))
  (let* ((file-info (source-info-file-info info))
	 (res (make-debug-source
	       :from :file
	       :created (file-info-write-date file-info)
	       :compiled (source-info-start-time info)
	       :source-root (file-info-source-root file-info)
	       :start-positions
	       (unless (eq *byte-compile* t)
		 (coerce-to-smallest-eltype
		  (file-info-positions file-info)))))
	 (name (file-info-name file-info)))
    (etypecase name
      ((member :lisp)
       (setf (debug-source-from res) name)
       (setf (debug-source-name res)
	     (coerce (file-info-forms file-info) 'simple-vector)))
      (pathname
       (let* ((untruename (file-info-untruename file-info))
	      (dir (pathname-directory untruename)))
	 (setf (debug-source-name res)
	       (namestring
		(if (and dir (eq (first dir) :absolute))
		    untruename
		    name))))))
    (list res)))


;;; Given an arbitrary sequence, coerce it to an unsigned vector if
;;; possible. Ordinarily we coerce it to the smallest specialized
;;; vector we can. However, we also have a special hack for
;;; cross-compiling at bootstrap time, when arbitrarily-specialized
;;; vectors aren't fully supported: in that case, we coerce it only to
;;; a vector whose element size is an integer multiple of output byte
;;; size.
(defun coerce-to-smallest-eltype (seq)
  (let ((maxoid #-sb-xc-host 0
		;; An initial value of 255 prevents us from
		;; specializing the array to anything smaller than
		;; (UNSIGNED-BYTE 8), which keeps the cross-compiler's
		;; portable specialized array output functions happy.
		#+sb-xc-host 255))
    (flet ((frob (x)
	     (if (typep x 'unsigned-byte)
		 (when (>= x maxoid)
		   (setf maxoid x))
		 (return-from coerce-to-smallest-eltype
		   (coerce seq 'simple-vector)))))
      (if (listp seq)
	  (dolist (i seq)
	    (frob i))
	  (dovector (i seq)
	    (frob i)))
      (coerce seq `(simple-array (integer 0 ,maxoid) (*))))))

;;;; variables

;;; Return a SC-OFFSET describing TN's location.
(defun tn-sc-offset (tn)
  (declare (type tn tn))
  (make-sc-offset (sc-number (tn-sc tn))
		  (tn-offset tn)))

;;; Dump info to represent Var's location being TN. ID is an integer
;;; that makes Var's name unique in the function. Buffer is the vector
;;; we stick the result in. If Minimal is true, we suppress name
;;; dumping, and set the minimal flag.
;;;
;;; The debug-var is only marked as always-live if the TN is
;;; environment live and is an argument. If a :debug-environment TN,
;;; then we also exclude set variables, since the variable is not
;;; guaranteed to be live everywhere in that case.
(defun dump-1-variable (fun var tn id minimal buffer)
  (declare (type lambda-var var) (type (or tn null) tn) (type index id)
	   (type clambda fun))
  (let* ((name (leaf-name var))
	 (save-tn (and tn (tn-save-tn tn)))
	 (kind (and tn (tn-kind tn)))
	 (flags 0))
    (declare (type index flags))
    (when minimal
      (setq flags (logior flags compiled-debug-var-minimal-p))
      (unless tn
	(setq flags (logior flags compiled-debug-var-deleted-p))))
    (when (and (or (eq kind :environment)
		   (and (eq kind :debug-environment)
			(null (basic-var-sets var))))
	       (not (gethash tn (ir2-component-spilled-tns
				 (component-info *component-being-compiled*))))
	       (eq (lambda-var-home var) fun))
      (setq flags (logior flags compiled-debug-var-environment-live)))
    (when save-tn
      (setq flags (logior flags compiled-debug-var-save-loc-p)))
    (unless (or (zerop id) minimal)
      (setq flags (logior flags compiled-debug-var-id-p)))
    (vector-push-extend flags buffer)
    (unless minimal
      (vector-push-extend name buffer)
      (unless (zerop id)
	(vector-push-extend id buffer)))
    (if tn
	(vector-push-extend (tn-sc-offset tn) buffer)
	(aver minimal))
    (when save-tn
      (vector-push-extend (tn-sc-offset save-tn) buffer)))
  (values))

;;; Return a vector suitable for use as the DEBUG-FUNCTION-VARIABLES
;;; of FUN. LEVEL is the current DEBUG-INFO quality. VAR-LOCS is a
;;; hashtable in which we enter the translation from LAMBDA-VARS to
;;; the relative position of that variable's location in the resulting
;;; vector.
(defun compute-variables (fun level var-locs)
  (declare (type clambda fun) (type hash-table var-locs))
  (collect ((vars))
    (labels ((frob-leaf (leaf tn gensym-p)
	       (let ((name (leaf-name leaf)))
		 (when (and name (leaf-refs leaf) (tn-offset tn)
			    (or gensym-p (symbol-package name)))
		   (vars (cons leaf tn)))))
	     (frob-lambda (x gensym-p)
	       (dolist (leaf (lambda-vars x))
		 (frob-leaf leaf (leaf-info leaf) gensym-p))))
      (frob-lambda fun t)
      (when (>= level 2)
	(dolist (x (ir2-environment-environment
		    (environment-info (lambda-environment fun))))
	  (let ((thing (car x)))
	    (when (lambda-var-p thing)
	      (frob-leaf thing (cdr x) (= level 3)))))
	
	(dolist (let (lambda-lets fun))
	  (frob-lambda let (= level 3)))))

    (let ((sorted (sort (vars) #'string<
			:key #'(lambda (x)
				 (symbol-name (leaf-name (car x))))))
	  (prev-name nil)
	  (id 0)
	  (i 0)
	  (buffer (make-array 0 :fill-pointer 0 :adjustable t)))
      (declare (type (or simple-string null) prev-name)
	       (type index id i))
      (dolist (x sorted)
	(let* ((var (car x))
	       (name (symbol-name (leaf-name var))))
	  (cond ((and prev-name (string= prev-name name))
		 (incf id))
		(t
		 (setq id 0  prev-name name)))
	  (dump-1-variable fun var (cdr x) id nil buffer)
	  (setf (gethash var var-locs) i))
	(incf i))
      (coerce buffer 'simple-vector))))

;;; Return a vector suitable for use as the DEBUG-FUNCTION-VARIABLES of
;;; FUN, representing the arguments to FUN in minimal variable format.
(defun compute-minimal-variables (fun)
  (declare (type clambda fun))
  (let ((buffer (make-array 0 :fill-pointer 0 :adjustable t)))
    (dolist (var (lambda-vars fun))
      (dump-1-variable fun var (leaf-info var) 0 t buffer))
    (coerce buffer 'simple-vector)))

;;; Return Var's relative position in the function's variables (determined
;;; from the Var-Locs hashtable.)  If Var is deleted, then return DELETED.
(defun debug-location-for (var var-locs)
  (declare (type lambda-var var) (type hash-table var-locs))
  (let ((res (gethash var var-locs)))
    (cond (res)
	  (t
	   (aver (or (null (leaf-refs var))
		     (not (tn-offset (leaf-info var)))))
	   'deleted))))

;;;; arguments/returns

;;; Return a vector to be used as the
;;; COMPILED-DEBUG-FUNCTION-ARGUMENTS for Fun. If fun is the
;;; MAIN-ENTRY for an optional dispatch, then look at the ARGLIST to
;;; determine the syntax, otherwise pretend all arguments are fixed.
;;;
;;; ### This assumption breaks down in EPs other than the main-entry,
;;; since they may or may not have supplied-p vars, etc.
(defun compute-arguments (fun var-locs)
  (declare (type clambda fun) (type hash-table var-locs))
  (collect ((res))
    (let ((od (lambda-optional-dispatch fun)))
      (if (and od (eq (optional-dispatch-main-entry od) fun))
	  (let ((actual-vars (lambda-vars fun))
		(saw-optional nil))
	    (dolist (arg (optional-dispatch-arglist od))
	      (let ((info (lambda-var-arg-info arg))
		    (actual (pop actual-vars)))
		(cond (info
		       (case (arg-info-kind info)
			 (:keyword
			  (res (arg-info-key info)))
			 (:rest
			  (res 'rest-arg))
			 (:more-context
			  (res 'more-arg))
			 (:optional
			  (unless saw-optional
			    (res 'optional-args)
			    (setq saw-optional t))))
		       (res (debug-location-for actual var-locs))
		       (when (arg-info-supplied-p info)
			 (res 'supplied-p)
			 (res (debug-location-for (pop actual-vars) var-locs))))
		      (t
		       (res (debug-location-for actual var-locs)))))))
	  (dolist (var (lambda-vars fun))
	    (res (debug-location-for var var-locs)))))

    (coerce-to-smallest-eltype (res))))

;;; Return a vector of SC offsets describing FUN's return locations.
;;; (Must be known values return...)
(defun compute-debug-returns (fun)
  (coerce-to-smallest-eltype
   (mapcar #'(lambda (loc)
	       (tn-sc-offset loc))
	   (return-info-locations (tail-set-info (lambda-tail-set fun))))))

;;;; debug functions

;;; Return a C-D-F structure with all the mandatory slots filled in.
(defun dfun-from-fun (fun)
  (declare (type clambda fun))
  (let* ((2env (environment-info (lambda-environment fun)))
	 (dispatch (lambda-optional-dispatch fun))
	 (main-p (and dispatch
		      (eq fun (optional-dispatch-main-entry dispatch)))))
    (make-compiled-debug-function
     :name (cond ((leaf-name fun))
		 ((let ((ef (functional-entry-function
			     fun)))
		    (and ef (leaf-name ef))))
		 ((and main-p (leaf-name dispatch)))
		 (t
		  (component-name
		   (block-component (node-block (lambda-bind fun))))))
     :kind (if main-p nil (functional-kind fun))
     :return-pc (tn-sc-offset (ir2-environment-return-pc 2env))
     :old-fp (tn-sc-offset (ir2-environment-old-fp 2env))
     :start-pc (label-position (ir2-environment-environment-start 2env))
     :elsewhere-pc (label-position (ir2-environment-elsewhere-start 2env)))))

;;; Return a complete C-D-F structure for Fun. This involves
;;; determining the DEBUG-INFO level and filling in optional slots as
;;; appropriate.
(defun compute-1-debug-function (fun var-locs)
  (declare (type clambda fun) (type hash-table var-locs))
  (let* ((dfun (dfun-from-fun fun))
	 (actual-level (policy (lambda-bind fun) debug))
	 (level (if #!+sb-dyncount *collect-dynamic-statistics*
		    #!-sb-dyncount nil
		    (max actual-level 2)
		    actual-level)))
    (cond ((zerop level))
	  ((and (<= level 1)
		(let ((od (lambda-optional-dispatch fun)))
		  (or (not od)
		      (not (eq (optional-dispatch-main-entry od) fun)))))
	   (setf (compiled-debug-function-variables dfun)
		 (compute-minimal-variables fun))
	   (setf (compiled-debug-function-arguments dfun) :minimal))
	  (t
	   (setf (compiled-debug-function-variables dfun)
		 (compute-variables fun level var-locs))
	   (setf (compiled-debug-function-arguments dfun)
		 (compute-arguments fun var-locs))))

    (when (>= level 2)
      (multiple-value-bind (blocks tlf-num) (compute-debug-blocks fun var-locs)
	(setf (compiled-debug-function-tlf-number dfun) tlf-num)
	(setf (compiled-debug-function-blocks dfun) blocks)))

    (if (external-entry-point-p fun)
	(setf (compiled-debug-function-returns dfun) :standard)
	(let ((info (tail-set-info (lambda-tail-set fun))))
	  (when info
	    (cond ((eq (return-info-kind info) :unknown)
		   (setf (compiled-debug-function-returns dfun)
			 :standard))
		  ((/= level 0)
		   (setf (compiled-debug-function-returns dfun)
			 (compute-debug-returns fun)))))))
    dfun))

;;;; minimal debug functions

;;; Return true if DFUN can be represented as a minimal debug
;;; function. DFUN is a cons (<start offset> . C-D-F).
(defun debug-function-minimal-p (dfun)
  (declare (type cons dfun))
  (let ((dfun (cdr dfun)))
    (and (member (compiled-debug-function-arguments dfun) '(:minimal nil))
	 (null (compiled-debug-function-blocks dfun)))))

;;; Dump a packed binary representation of a DFUN into *BYTE-BUFFER*.
;;; PREV-START and START are the byte offsets in the code where the
;;; previous function started and where this one starts.
;;; PREV-ELSEWHERE is the previous function's elsewhere PC.
(defun dump-1-minimal-dfun (dfun prev-start start prev-elsewhere)
  (declare (type compiled-debug-function dfun)
	   (type index prev-start start prev-elsewhere))
  (let* ((name (compiled-debug-function-name dfun))
	 (setf-p (and (consp name) (eq (car name) 'setf)
		      (consp (cdr name)) (symbolp (cadr name))))
	 (base-name (if setf-p (cadr name) name))
	 (pkg (when (symbolp base-name)
		(symbol-package base-name)))
	 (name-rep
	  (cond ((stringp base-name)
		 minimal-debug-function-name-component)
		((not pkg)
		 minimal-debug-function-name-uninterned)
		((eq pkg (sane-package))
		 minimal-debug-function-name-symbol)
		(t
		 minimal-debug-function-name-packaged))))
    (aver (or (atom name) setf-p))
    (let ((options 0))
      (setf (ldb minimal-debug-function-name-style-byte options) name-rep)
      (setf (ldb minimal-debug-function-kind-byte options)
	    (position-or-lose (compiled-debug-function-kind dfun)
			      *minimal-debug-function-kinds*))
      (setf (ldb minimal-debug-function-returns-byte options)
	    (etypecase (compiled-debug-function-returns dfun)
	      ((member :standard) minimal-debug-function-returns-standard)
	      ((member :fixed) minimal-debug-function-returns-fixed)
	      (vector minimal-debug-function-returns-specified)))
      (vector-push-extend options *byte-buffer*))

    (let ((flags 0))
      (when setf-p
	(setq flags (logior flags minimal-debug-function-setf-bit)))
      (when (compiled-debug-function-nfp dfun)
	(setq flags (logior flags minimal-debug-function-nfp-bit)))
      (when (compiled-debug-function-variables dfun)
	(setq flags (logior flags minimal-debug-function-variables-bit)))
      (vector-push-extend flags *byte-buffer*))

    (when (eql name-rep minimal-debug-function-name-packaged)
      (write-var-string (package-name pkg) *byte-buffer*))
    (unless (stringp base-name)
      (write-var-string (symbol-name base-name) *byte-buffer*))

    (let ((vars (compiled-debug-function-variables dfun)))
      (when vars
	(let ((len (length vars)))
	  (write-var-integer len *byte-buffer*)
	  (dotimes (i len)
	    (vector-push-extend (aref vars i) *byte-buffer*)))))

    (let ((returns (compiled-debug-function-returns dfun)))
      (when (vectorp returns)
	(let ((len (length returns)))
	  (write-var-integer len *byte-buffer*)
	  (dotimes (i len)
	    (write-var-integer (aref returns i) *byte-buffer*)))))

    (write-var-integer (compiled-debug-function-return-pc dfun)
		       *byte-buffer*)
    (write-var-integer (compiled-debug-function-old-fp dfun)
		       *byte-buffer*)
    (when (compiled-debug-function-nfp dfun)
      (write-var-integer (compiled-debug-function-nfp dfun)
			 *byte-buffer*))
    (write-var-integer (- start prev-start) *byte-buffer*)
    (write-var-integer (- (compiled-debug-function-start-pc dfun) start)
		       *byte-buffer*)
    (write-var-integer (- (compiled-debug-function-elsewhere-pc dfun)
			  prev-elsewhere)
		       *byte-buffer*)))

;;; Return a byte-vector holding all the debug functions for a
;;; component in the packed binary minimal-debug-function format.
(defun compute-minimal-debug-functions (dfuns)
  (declare (list dfuns))
  (setf (fill-pointer *byte-buffer*) 0)
  (let ((prev-start 0)
	(prev-elsewhere 0))
    (dolist (dfun dfuns)
      (let ((start (car dfun))
	    (elsewhere (compiled-debug-function-elsewhere-pc (cdr dfun))))
	(dump-1-minimal-dfun (cdr dfun) prev-start start prev-elsewhere)
	(setq prev-start start  prev-elsewhere elsewhere))))
  (copy-seq *byte-buffer*))

;;;; full component dumping

;;; Compute the full form (simple-vector) function map.
(defun compute-debug-function-map (sorted)
  (declare (list sorted))
  (let* ((len (1- (* (length sorted) 2)))
	 (funs-vec (make-array len)))
    (do ((i -1 (+ i 2))
	 (sorted sorted (cdr sorted)))
	((= i len))
      (declare (fixnum i))
      (let ((dfun (car sorted)))
	(unless (minusp i)
	  (setf (svref funs-vec i) (car dfun)))
	(setf (svref funs-vec (1+ i)) (cdr dfun))))
    funs-vec))

;;; Return a DEBUG-INFO structure describing COMPONENT. This has to be
;;; called after assembly so that source map information is available.
(defun debug-info-for-component (component)
  (declare (type component component))
  (collect ((dfuns))
    (let ((var-locs (make-hash-table :test 'eq))
	  ;; FIXME: What is *BYTE-BUFFER* for? Has it become dead code
	  ;; now that we no longer use minimal-debug-function
	  ;; representation?
	  (*byte-buffer* (make-array 10
				     :element-type '(unsigned-byte 8)
				     :fill-pointer 0
				     :adjustable t)))
      (dolist (fun (component-lambdas component))
	(clrhash var-locs)
	(dfuns (cons (label-position
		      (block-label (node-block (lambda-bind fun))))
		     (compute-1-debug-function fun var-locs))))
      (let* ((sorted (sort (dfuns) #'< :key #'car))
	     ;; FIXME: CMU CL had
	     ;;    (IF (EVERY #'DEBUG-FUNCTION-MINIMAL-P SORTED)
	     ;;	       (COMPUTE-MINIMAL-DEBUG-FUNCTIONS SORTED)
	     ;;	       (COMPUTE-DEBUG-FUNCTION-MAP SORTED))
	     ;; here. We've gotten rid of the minimal-debug-function
	     ;; case in SBCL because the minimal representation
	     ;; couldn't be made to transform properly under package
	     ;; renaming. Now that that case is gone, a lot of code is
	     ;; dead, and once everything is known to work, the dead
	     ;; code should be deleted.
	     (function-map (compute-debug-function-map sorted)))
	(make-compiled-debug-info :name (component-name component)
				  :function-map function-map)))))

;;; Write BITS out to BYTE-BUFFER in backend byte order. The length of
;;; BITS must be evenly divisible by eight.
(defun write-packed-bit-vector (bits byte-buffer)
  (declare (type simple-bit-vector bits) (type byte-buffer byte-buffer))
  (multiple-value-bind (initial step done)
      (ecase *backend-byte-order*
	(:little-endian (values 0  1  8))
	(:big-endian    (values 7 -1 -1)))
    (let ((shift initial)
	  (byte 0))
      (dotimes (i (length bits))
	(let ((int (aref bits i)))
	  (setf byte (logior byte (ash int shift)))
	  (incf shift step))
	(when (= shift done)
	  (vector-push-extend byte byte-buffer)
	  (setf shift initial
		byte 0)))
      (unless (= shift initial)
	(vector-push-extend byte byte-buffer))))
  (values))
