;;;; that part of the byte compiler which exists not only in the
;;;; target Lisp, but also in the cross-compilation host Lisp

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;; ### remaining work:
;;;
;;; - add more inline operations.
;;; - Breakpoints/debugging info.

;;;; stuff to emit noise

;;; Note: We use the regular assembler, but we don't use any
;;; ``instructions'' because there is no way to keep our byte-code
;;; instructions separate from the instructions used by the native
;;; backend. Besides, we don't want to do any scheduling or anything
;;; like that, anyway.

#!-sb-fluid (declaim (inline output-byte))
(defun output-byte (segment byte)
  (declare (type sb!assem:segment segment)
	   (type (unsigned-byte 8) byte))
  (sb!assem:emit-byte segment byte))

;;; Output OPERAND as 1 or 4 bytes, using #xFF as the extend code.
(defun output-extended-operand (segment operand)
  (declare (type (unsigned-byte 24) operand))
  (cond ((<= operand 254)
	 (output-byte segment operand))
	(t
	 (output-byte segment #xFF)
	 (output-byte segment (ldb (byte 8 16) operand))
	 (output-byte segment (ldb (byte 8 8) operand))
	 (output-byte segment (ldb (byte 8 0) operand)))))

;;; Output a byte, logior'ing in a 4 bit immediate constant. If that
;;; immediate won't fit, then emit it as the next 1-4 bytes.
(defun output-byte-with-operand (segment byte operand)
  (declare (type sb!assem:segment segment)
	   (type (unsigned-byte 8) byte)
	   (type (unsigned-byte 24) operand))
  (cond ((<= operand 14)
	 (output-byte segment (logior byte operand)))
	(t
	 (output-byte segment (logior byte 15))
	 (output-extended-operand segment operand)))
  (values))

(defun output-label (segment label)
  (declare (type sb!assem:segment segment)
	   (type sb!assem:label label))
  (sb!assem:assemble (segment)
    (sb!assem:emit-label label)))

;;; Output a reference to LABEL.
(defun output-reference (segment label)
  (declare (type sb!assem:segment segment)
	   (type sb!assem:label label))
  (sb!assem:emit-back-patch
   segment
   3
   #'(lambda (segment posn)
       (declare (type sb!assem:segment segment)
		(ignore posn))
       (let ((target (sb!assem:label-position label)))
	 (aver (<= 0 target (1- (ash 1 24))))
	 (output-byte segment (ldb (byte 8 16) target))
	 (output-byte segment (ldb (byte 8 8) target))
	 (output-byte segment (ldb (byte 8 0) target))))))

;;; Output some branch byte-sequence.
(defun output-branch (segment kind label)
  (declare (type sb!assem:segment segment)
	   (type (unsigned-byte 8) kind)
	   (type sb!assem:label label))
  (sb!assem:emit-chooser
   segment 4 1
   #'(lambda (segment posn delta)
       (when (<= (- (ash 1 7))
		 (- (sb!assem:label-position label posn delta) posn 2)
		 (1- (ash 1 7)))
	 (sb!assem:emit-chooser
	  segment 2 1
	  #'(lambda (segment posn delta)
	      (declare (ignore segment) (type index posn delta))
	      (when (zerop (- (sb!assem:label-position label posn delta)
			      posn 2))
		;; Don't emit anything, because the branch is to the following
		;; instruction.
		t))
	  #'(lambda (segment posn)
	      ;; We know that we fit in one byte.
	      (declare (type sb!assem:segment segment)
		       (type index posn))
	      (output-byte segment (logior kind 1))
	      (output-byte segment
			   (ldb (byte 8 0)
				(- (sb!assem:label-position label) posn 2)))))
	 t))
   #'(lambda (segment posn)
       (declare (type sb!assem:segment segment)
		(ignore posn))
       (let ((target (sb!assem:label-position label)))
	 (aver (<= 0 target (1- (ash 1 24))))
	 (output-byte segment kind)
	 (output-byte segment (ldb (byte 8 16) target))
	 (output-byte segment (ldb (byte 8 8) target))
	 (output-byte segment (ldb (byte 8 0) target))))))

;;;; system constants, Xops, and inline functions

;;; If (%FDEFINITION-MARKER% . NAME) is a key in the table, then the
;;; corresponding value is the byte code fdefinition.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *system-constant-codes* (make-hash-table :test 'equal)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((def-system-constant (index form)
	   (setf (gethash form *system-constant-codes*) index)))
    (def-system-constant 0 nil)
    (def-system-constant 1 t)
    (def-system-constant 2 :start)
    (def-system-constant 3 :end)
    (def-system-constant 4 :test)
    (def-system-constant 5 :count)
    (def-system-constant 6 :test-not)
    (def-system-constant 7 :key)
    (def-system-constant 8 :from-end)
    (def-system-constant 9 :type)
    (def-system-constant 10 '(%fdefinition-marker% . error))
    (def-system-constant 11 '(%fdefinition-marker% . format))
    (def-system-constant 12 '(%fdefinition-marker% . %typep))
    (def-system-constant 13 '(%fdefinition-marker% . eql))
    (def-system-constant 14 '(%fdefinition-marker% . %negate))
    (def-system-constant 15 '(%fdefinition-marker% . %%defun))
    (def-system-constant 16 '(%fdefinition-marker% . %%defmacro))
    (def-system-constant 17 '(%fdefinition-marker% . %%defconstant))
    (def-system-constant 18 '(%fdefinition-marker% . length))
    (def-system-constant 19 '(%fdefinition-marker% . equal))
    (def-system-constant 20 '(%fdefinition-marker% . append))
    (def-system-constant 21 '(%fdefinition-marker% . reverse))
    (def-system-constant 22 '(%fdefinition-marker% . nreverse))
    (def-system-constant 23 '(%fdefinition-marker% . nconc))
    (def-system-constant 24 '(%fdefinition-marker% . list))
    (def-system-constant 25 '(%fdefinition-marker% . list*))
    (def-system-constant 26 '(%fdefinition-marker% . %coerce-name-to-function))
    (def-system-constant 27 '(%fdefinition-marker% . values-list))))

(eval-when (#+sb-xc :compile-toplevel :load-toplevel :execute)

(defparameter *xop-names*
  '(breakpoint; 0
    dup; 1
    type-check; 2
    fdefn-function-or-lose; 3
    default-unknown-values; 4
    push-n-under; 5
    xop6
    xop7
    merge-unknown-values
    make-closure
    throw
    catch
    breakup
    return-from
    tagbody
    go
    unwind-protect))

(defun xop-index-or-lose (name)
  (or (position name *xop-names* :test #'eq)
      (error "unknown XOP ~S" name)))

) ; EVAL-WHEN

;;; FIXME: The hardwired 32 here (found also in (MOD 32) above, and in
;;; the number of bits tested in EXPAND-INTO-INLINES, and perhaps
;;; elsewhere) is ugly. There should be some symbolic constant for the
;;; number of bits devoted to coding byte-inline functions.
(eval-when (:compile-toplevel :load-toplevel :execute)

  (defstruct (inline-function-info (:copier nil))
    ;; the name of the function that we convert into calls to this
    (function (required-argument) :type symbol)
    ;; the name of the function that the interpreter should call to
    ;; implement this. This may not be the same as the FUNCTION slot
    ;; value if extra safety checks are required.
    (interpreter-function (required-argument) :type symbol)
    ;; the inline operation number, i.e. the byte value actually
    ;; written into byte-compiled code
    (number (required-argument) :type (mod 32))
    ;; the type that calls must satisfy
    (type (required-argument) :type function-type)
    ;; Can we skip type checking of the arguments?
    (safe (required-argument) :type boolean))

  (defparameter *inline-functions* (make-array 32 :initial-element nil))
  (defparameter *inline-function-table* (make-hash-table :test 'eq))
  (let ((number 0))
    (dolist (stuff
	     '((+ (fixnum fixnum) fixnum)
	       (- (fixnum fixnum) fixnum)
	       (make-value-cell (t) t)
	       (value-cell-ref (t) t)
	       (value-cell-setf (t t) (values))
	       (symbol-value (symbol) t
			     :interpreter-function %byte-symbol-value)
	       (setf-symbol-value (t symbol) (values))
	       (%byte-special-bind (t symbol) (values))
	       (%byte-special-unbind () (values))
	       (%negate (fixnum) fixnum)
	       (< (fixnum fixnum) t)
	       (> (fixnum fixnum) t)
	       (car (t) t :interpreter-function %byte-car :safe t)
	       (cdr (t) t :interpreter-function %byte-cdr :safe t)
	       (length (list) t)
	       (cons (t t) t)
	       (list (t t) t)
	       (list* (t t t) t)
	       (%instance-ref (t t) t)
	       (%setf-instance-ref (t t t) (values))))
      (destructuring-bind
	  (name arg-types result-type
		&key (interpreter-function name) alias safe)
	  stuff
	(let ((info
	       (make-inline-function-info
		:function name
		:number number
		:interpreter-function interpreter-function
		:type (specifier-type `(function ,arg-types ,result-type))
		:safe safe)))
	  (setf (svref *inline-functions* number) info)
	  (setf (gethash name *inline-function-table*) info))
	(unless alias (incf number))))))

(defun inline-function-number-or-lose (function)
  (let ((info (gethash function *inline-function-table*)))
    (if info
	(inline-function-info-number info)
	(error "unknown inline function: ~S" function))))

;;;; transforms which are specific to byte code

;;; It appears that the idea here is that in byte code, EQ is more
;;; efficient than CHAR=. -- WHN 199910

(deftransform eql ((x y) ((or fixnum character) (or fixnum character))
		   * :when :byte)
  '(eq x y))

(deftransform char= ((x y) * * :when :byte)
  '(eq x y))

;;;; annotations hung off the IR1 while compiling

(defstruct (byte-component-info (:copier nil))
  (constants (make-array 10 :adjustable t :fill-pointer 0)))

(defstruct (byte-lambda-info (:copier nil))
  (label nil :type (or null label))
  (stack-size 0 :type index)
  ;; FIXME: should be INTERESTING-P T :TYPE BOOLEAN
  (interesting t :type (member t nil)))

(defun block-interesting (block)
  (byte-lambda-info-interesting (lambda-info (block-home-lambda block))))

(defstruct (byte-lambda-var-info (:copier nil))
  (argp nil :type (member t nil))
  (offset 0 :type index))

(defstruct (byte-nlx-info (:copier nil))
  (stack-slot nil :type (or null index))
  (label (sb!assem:gen-label) :type sb!assem:label)
  (duplicate nil :type (member t nil)))

(defstruct (byte-block-info
	    (:copier nil)
	    (:include block-annotation)
	    (:constructor make-byte-block-info
			  (block &key produces produces-sset consumes
			    total-consumes nlx-entries nlx-entry-p)))
  (label (sb!assem:gen-label) :type sb!assem:label)
  ;; A list of the CONTINUATIONs describing values that this block
  ;; pushes onto the stack. Note: PRODUCES and CONSUMES can contain
  ;; the keyword :NLX-ENTRY marking the place on the stack where a
  ;; non-local-exit frame is added or removed. Since breaking up a NLX
  ;; restores the stack, we don't have to about (and in fact must not)
  ;; discard values underneath a :NLX-ENTRY marker evern though they
  ;; appear to be dead (since they might not be.)
  (produces nil :type list)
  ;; An SSET of the produces for faster set manipulations. The
  ;; elements are the BYTE-CONTINUATION-INFO objects. :NLX-ENTRY
  ;; markers are not represented.
  (produces-sset (make-sset) :type sset)
  ;; A list of the continuations that this block pops from the stack.
  ;; See PRODUCES.
  (consumes nil :type list)
  ;; The transitive closure of what this block and all its successors
  ;; consume. After stack-analysis, that is.
  (total-consumes (make-sset) :type sset)
  ;; Set to T whenever the consumes lists of a successor changes and
  ;; the block is queued for re-analysis so we can easily avoid
  ;; queueing the same block several times.
  (already-queued nil :type (member t nil))
  ;; The continuations and :NLX-ENTRY markers on the stack (in order)
  ;; when this block starts.
  (start-stack :unknown :type (or (member :unknown) list))
  ;; The continuations and :NLX-ENTRY markers on the stack (in order)
  ;; when this block ends.
  (end-stack nil :type list)
  ;; List of ((nlx-info*) produces consumes) for each ENTRY in this
  ;; block that is a NLX target.
  (nlx-entries nil :type list)
  ;; T if this is an %nlx-entry point, and we shouldn't just assume we
  ;; know what is going to be on the stack.
  (nlx-entry-p nil :type (member t nil)))

(defprinter (byte-block-info)
  block)

(defstruct (byte-continuation-info
	    (:include sset-element)
	    (:constructor make-byte-continuation-info
			  (continuation results placeholders))
	    (:copier nil))
  (continuation (required-argument) :type continuation)
  (results (required-argument)
	   :type (or (member :fdefinition :eq-test :unknown) index))
  ;; If the DEST is a local non-MV call, then we may need to push some
  ;; number of placeholder args corresponding to deleted
  ;; (unreferenced) args. If PLACEHOLDERS /= 0, then RESULTS is
  ;; PLACEHOLDERS + 1.
  (placeholders (required-argument) :type index))

(defprinter (byte-continuation-info)
  continuation
  results
  (placeholders :test (/= placeholders 0)))

;;;; Annotate the IR1.

(defun annotate-continuation (cont results &optional (placeholders 0))
  ;; For some reason, DO-NODES does the same return node multiple
  ;; times, which causes ANNOTATE-CONTINUATION to be called multiple
  ;; times on the same continuation. So we can't assert that we
  ;; haven't done it.
  #+nil
  (aver (null (continuation-info cont)))
  (setf (continuation-info cont)
	(make-byte-continuation-info cont results placeholders))
  (values))

(defun annotate-set (set)
  ;; Annotate the value for one value.
  (annotate-continuation (set-value set) 1))

;;; We do different stack magic for non-MV and MV calls to figure out
;;; how many values should be pushed during compilation of each arg.
;;;
;;; Since byte functions are directly caller by the interpreter (there
;;; is no XEP), and it doesn't know which args are actually used, byte
;;; functions must allow unused args to be passed. But this creates a
;;; problem with local calls, because these unused args would not
;;; otherwise be pushed (since the continuation has been deleted.) So,
;;; in this function, we count up placeholders for any unused args
;;; contiguously preceding this one. These placeholders are inserted
;;; under the referenced arg by CHECKED-CANONICALIZE-VALUES.
;;;
;;; With MV calls, we try to figure out how many values are actually
;;; generated. We allow initial args to supply a fixed number of
;;; values, but everything after the first :unknown arg must also be
;;; unknown. This picks off most of the standard uses (i.e. calls to
;;; apply), but still is easy to implement.
(defun annotate-basic-combination-args (call)
  (declare (type basic-combination call))
  (etypecase call
    (combination
     (if (and (eq (basic-combination-kind call) :local)
	      (member (functional-kind (combination-lambda call))
		      '(nil :optional :cleanup)))
	 (let ((placeholders 0))
	   (declare (type index placeholders))
	   (dolist (arg (combination-args call))
	     (cond (arg
		    (annotate-continuation arg (1+ placeholders) placeholders)
		    (setq placeholders 0))
		   (t
		    (incf placeholders)))))
	 (dolist (arg (combination-args call))
	   (when arg
	     (annotate-continuation arg 1)))))
    (mv-combination
     (labels
	 ((allow-fixed (remaining)
	    (when remaining
	      (let* ((cont (car remaining))
		     (values (nth-value 1
					(values-types
					 (continuation-derived-type cont)))))
		(cond ((eq values :unknown)
		       (force-to-unknown remaining))
		      (t
		       (annotate-continuation cont values)
		       (allow-fixed (cdr remaining)))))))
	  (force-to-unknown (remaining)
	    (when remaining
	      (let ((cont (car remaining)))
		(when cont
		  (annotate-continuation cont :unknown)))
	      (force-to-unknown (cdr remaining)))))
       (allow-fixed (mv-combination-args call)))))
  (values))

(defun annotate-local-call (call)
  (cond ((mv-combination-p call)
	 (annotate-continuation
	  (first (basic-combination-args call))
	  (length (lambda-vars (combination-lambda call)))))
	(t
	 (annotate-basic-combination-args call)
	 (when (member (functional-kind (combination-lambda call))
		       '(nil :optional :cleanup))
	   (dolist (arg (basic-combination-args call))
	     (when arg
	       (setf (continuation-%type-check arg) nil))))))
  (annotate-continuation (basic-combination-fun call) 0)
  (when (node-tail-p call)
    (set-tail-local-call-successor call)))

;;; Annotate the values for any :full combination. This includes
;;; inline functions, multiple value calls & throw. If a real full
;;; call or a safe inline operation, then clear any type-check
;;; annotations. When we are done, remove jump to return for tail
;;; calls.
;;;
;;; Also, we annotate slot accessors as inline if no type check is
;;; needed and (for setters) no value needs to be left on the stack.
(defun annotate-full-call (call)
  (let* ((fun (basic-combination-fun call))
	 (args (basic-combination-args call))
	 (name (continuation-function-name fun))
	 (info (gethash name *inline-function-table*)))
    (flet ((annotate-args ()
	     (annotate-basic-combination-args call)
	     (dolist (arg args)
	       (when (continuation-type-check arg)
		 (setf (continuation-%type-check arg) :deleted)))
	     (annotate-continuation
	      fun
	      (if (continuation-function-name fun) :fdefinition 1))))
      (cond ((mv-combination-p call)
	     (cond ((eq name '%throw)
		    (aver (= (length args) 2))
		    (annotate-continuation (first args) 1)
		    (annotate-continuation (second args) :unknown)
		    (setf (node-tail-p call) nil)
		    (annotate-continuation fun 0))
		   (t
		    (annotate-args))))
	    ((and info
		  (valid-function-use call (inline-function-info-type info)))
	     (annotate-basic-combination-args call)
	     (setf (node-tail-p call) nil)
	     (setf (basic-combination-info call) info)
	     (annotate-continuation fun 0)
	     (when (inline-function-info-safe info)
	       (dolist (arg args)
		 (when (continuation-type-check arg)
		   (setf (continuation-%type-check arg) :deleted)))))
	    ((and name
		  (let ((leaf (ref-leaf (continuation-use fun))))
		    (and (slot-accessor-p leaf)
			 (or (policy call (zerop safety))
			     (not (find t args
					:key #'continuation-type-check)))
			 (if (consp name)
			     (not (continuation-dest (node-cont call)))
			     t))))
	     (setf (basic-combination-info call)
		   (gethash (if (consp name) '%setf-instance-ref '%instance-ref)
			    *inline-function-table*))
	     (setf (node-tail-p call) nil)
	     (annotate-continuation fun 0)
	     (annotate-basic-combination-args call))
	    (t
	     (annotate-args)))))

  ;; If this is (still) a tail-call, then blow away the return.
  (when (node-tail-p call)
    (node-ends-block call)
    (let ((block (node-block call)))
      (unlink-blocks block (first (block-succ block)))
      (link-blocks block (component-tail (block-component block)))))

  (values))

(defun annotate-known-call (call)
  (annotate-basic-combination-args call)
  (setf (node-tail-p call) nil)
  (annotate-continuation (basic-combination-fun call) 0)
  t)

(defun annotate-basic-combination (call)
  ;; Annotate the function.
  (let ((kind (basic-combination-kind call)))
    (case kind
      (:local
       (annotate-local-call call))
      (:full
       (annotate-full-call call))
      (:error
       (setf (basic-combination-kind call) :full)
       (annotate-full-call call))
      (t
       (unless (and (function-info-byte-compile kind)
		    (funcall (or (function-info-byte-annotate kind)
				 #'annotate-known-call)
			     call))
	 (setf (basic-combination-kind call) :full)
	 (annotate-full-call call)))))

  (values))

(defun annotate-if (if)
  ;; Annotate the test.
  (let* ((cont (if-test if))
	 (use (continuation-use cont)))
    (annotate-continuation
     cont
     (if (and (combination-p use)
	      (eq (continuation-function-name (combination-fun use)) 'eq)
	      (= (length (combination-args use)) 2))
	 ;; If the test is a call to EQ, then we can use branch-if-eq
	 ;; so don't need to actually funcall the test.
	 :eq-test
	 ;; Otherwise, funcall the test for 1 value.
	 1))))

(defun annotate-return (return)
  (let ((cont (return-result return)))
    (annotate-continuation
     cont
     (nth-value 1 (values-types (continuation-derived-type cont))))))

(defun annotate-exit (exit)
  (let ((cont (exit-value exit)))
    (when cont
      (annotate-continuation cont :unknown))))

(defun annotate-block (block)
  (do-nodes (node cont block)
    (etypecase node
      (bind)
      (ref)
      (cset (annotate-set node))
      (basic-combination (annotate-basic-combination node))
      (cif (annotate-if node))
      (creturn (annotate-return node))
      (entry)
      (exit (annotate-exit node))))
  (values))

(defun annotate-ir1 (component)
  (do-blocks (block component)
    (when (block-interesting block)
      (annotate-block block)))
  (values))

;;;; stack analysis

(defvar *byte-continuation-counter*)

;;; Scan the nodes in BLOCK and compute the information that we will
;;; need to do flow analysis and our stack simulation walk. We simulate
;;; the stack within the block, reducing it to ordered lists
;;; representing the values we remove from the top of the stack and
;;; place on the stack (not considering values that are produced and
;;; consumed within the block.) A NLX entry point is considered to
;;; push a :NLX-ENTRY marker (can be though of as the run-time catch
;;; frame.)
(defun compute-produces-and-consumes (block)
  (let ((stack nil)
	(consumes nil)
	(total-consumes (make-sset))
	(nlx-entries nil)
	(nlx-entry-p nil))
    (labels ((interesting (cont)
	       (and cont
		    (let ((info (continuation-info cont)))
		      (and info
			   (not (member (byte-continuation-info-results info)
					'(0 :eq-test)))))))
	     (consume (cont)
	       (cond ((not (or (eq cont :nlx-entry) (interesting cont))))
		     (stack
		      (aver (eq (car stack) cont))
		      (pop stack))
		     (t
		      (adjoin-cont cont total-consumes)
		      (push cont consumes))))
	     (adjoin-cont (cont sset)
	       (unless (eq cont :nlx-entry)
		 (let ((info (continuation-info cont)))
		   (unless (byte-continuation-info-number info)
		     (setf (byte-continuation-info-number info)
			   (incf *byte-continuation-counter*)))
		   (sset-adjoin info sset)))))
      (do-nodes (node cont block)
	(etypecase node
	  (bind)
	  (ref)
	  (cset
	   (consume (set-value node)))
	  (basic-combination
	   (dolist (arg (reverse (basic-combination-args node)))
	     (when arg
	       (consume arg)))
	   (consume (basic-combination-fun node))
	   (case (continuation-function-name (basic-combination-fun node))
	     (%nlx-entry
	      (let ((nlx-info (continuation-value
			       (first (basic-combination-args node)))))
		(ecase (cleanup-kind (nlx-info-cleanup nlx-info))
		  ((:catch :unwind-protect)
		   (consume :nlx-entry))
		  ;; If for a lexical exit, we will see a breakup later, so
		  ;; don't consume :NLX-ENTRY now.
		  (:tagbody)
		  (:block
		   (let ((cont (nlx-info-continuation nlx-info)))
		     (when (interesting cont)
		       (push cont stack))))))
	      (setf nlx-entry-p t))
	     (%lexical-exit-breakup
	      (unless (byte-nlx-info-duplicate
		       (nlx-info-info
			(continuation-value
			 (first (basic-combination-args node)))))
		(consume :nlx-entry)))
	     ((%catch-breakup %unwind-protect-breakup)
	      (consume :nlx-entry))))
	  (cif
	   (consume (if-test node)))
	  (creturn
	   (consume (return-result node)))
	  (entry
	   (let* ((cup (entry-cleanup node))
		  (nlx-info (cleanup-nlx-info cup)))
	     (when nlx-info
	       (push :nlx-entry stack)
	       (push (list nlx-info stack (reverse consumes))
		     nlx-entries))))
	  (exit
	   (when (exit-value node)
	     (consume (exit-value node)))))
	(when (and (not (exit-p node)) (interesting cont))
	  (push cont stack)))

      (setf (block-info block)
	    (make-byte-block-info
	     block
	     :produces stack
	     :produces-sset (let ((res (make-sset)))
			      (dolist (product stack)
				(adjoin-cont product res))
			      res)
	     :consumes (reverse consumes)
	     :total-consumes total-consumes
	     :nlx-entries nlx-entries
	     :nlx-entry-p nlx-entry-p))))

  (values))

(defun walk-successors (block stack)
  (let ((tail (component-tail (block-component block))))
    (dolist (succ (block-succ block))
      (unless (or (eq succ tail)
		  (not (block-interesting succ))
		  (byte-block-info-nlx-entry-p (block-info succ)))
	(walk-block succ block stack)))))

;;; Take a stack and a consumes list, and remove the appropriate
;;; stuff. When we consume a :NLX-ENTRY, we just remove the top
;;; marker, and leave any values on top intact. This represents the
;;; desired effect of %CATCH-BREAKUP, etc., which don't affect any
;;; values on the stack.
(defun consume-stuff (stack stuff)
  (let ((new-stack stack))
    (dolist (cont stuff)
      (cond ((eq cont :nlx-entry)
	     (aver (find :nlx-entry new-stack))
	     (setq new-stack (remove :nlx-entry new-stack :count 1)))
	    (t
	     (aver (eq (car new-stack) cont))
	     (pop new-stack))))
    new-stack))

;;; NLX-INFOS is the list of NLX-INFO structures for this ENTRY note.
;;; CONSUME and PRODUCE are the values from outside this block that
;;; were consumed and produced by this block before the ENTRY node.
;;; STACK is the globally simulated stack at the start of this block.
(defun walk-nlx-entry (nlx-infos stack produce consume)
  (let ((stack (consume-stuff stack consume)))
    (dolist (nlx-info nlx-infos)
      (walk-block (nlx-info-target nlx-info) nil (append produce stack))))
  (values))

;;; Simulate the stack across block boundaries, discarding any values
;;; that are dead. A :NLX-ENTRY marker prevents values live at a NLX
;;; entry point from being discarded prematurely.
(defun walk-block (block pred stack)
  ;; Pop everything off of stack that isn't live.
  (let* ((info (block-info block))
	 (live (byte-block-info-total-consumes info)))
    (collect ((pops))
      (let ((fixed 0))
	(flet ((flush-fixed ()
		 (unless (zerop fixed)
		   (pops `(%byte-pop-stack ,fixed))
		   (setf fixed 0))))
	  (loop
	    (unless stack
	      (return))
	    (let ((cont (car stack)))
	      (when (or (eq cont :nlx-entry)
			(sset-member (continuation-info cont) live))
		(return))
	      (pop stack)
	      (let ((results
		     (byte-continuation-info-results
		      (continuation-info cont))))
		(case results
		  (:unknown
		   (flush-fixed)
		   (pops `(%byte-pop-stack 0)))
		  (:fdefinition
		   (incf fixed))
		  (t
		   (incf fixed results))))))
	  (flush-fixed)))
      (when (pops)
	(aver pred)
	(let ((cleanup-block
	       (insert-cleanup-code pred block
				    (continuation-next (block-start block))
				    `(progn ,@(pops)))))
	  (annotate-block cleanup-block))))

    (cond ((eq (byte-block-info-start-stack info) :unknown)
	   ;; Record what the stack looked like at the start of this block.
	   (setf (byte-block-info-start-stack info) stack)
	   ;; Process any nlx entries that build off of our stack.
	   (dolist (stuff (byte-block-info-nlx-entries info))
	     (walk-nlx-entry (first stuff) stack (second stuff) (third stuff)))
	   ;; Remove whatever we consume.
	   (setq stack (consume-stuff stack (byte-block-info-consumes info)))
	   ;; Add whatever we produce.
	   (setf stack (append (byte-block-info-produces info) stack))
	   (setf (byte-block-info-end-stack info) stack)
	   ;; Pass that on to all our successors.
	   (walk-successors block stack))
	  (t
	   ;; We have already processed the successors of this block. Just
	   ;; make sure we thing the stack is the same now as before.
	   (aver (equal (byte-block-info-start-stack info) stack)))))
  (values))

;;; Do lifetime flow analysis on values pushed on the stack, then call
;;; do the stack simulation walk to discard dead values. In addition
;;; to considering the obvious inputs from a block's successors, we
;;; must also consider %NLX-ENTRY targets to be successors in order to
;;; ensure that any values only used in the NLX entry stay alive until
;;; we reach the mess-up node. After then, we can keep the values from
;;; being discarded by placing a marker on the simulated stack.
(defun byte-stack-analyze (component)
  (let ((head nil))
    (let ((*byte-continuation-counter* 0))
      (do-blocks (block component)
	(when (block-interesting block)
	  (compute-produces-and-consumes block)
	  (push block head)
	  (setf (byte-block-info-already-queued (block-info block)) t))))
    (let ((tail (last head)))
      (labels ((maybe-enqueue (block)
		 (when (block-interesting block)
		   (let ((info (block-info block)))
		     (unless (byte-block-info-already-queued info)
		       (setf (byte-block-info-already-queued info) t)
		       (let ((new (list block)))
			 (if head
			     (setf (cdr tail) new)
			     (setf head new))
			 (setf tail new))))))
	       (maybe-enqueue-predecessors (block)
		 (when (byte-block-info-nlx-entry-p (block-info block))
		   (maybe-enqueue
		    (node-block
		     (cleanup-mess-up
		      (nlx-info-cleanup
		       (find block
			     (environment-nlx-info (block-environment block))
			     :key #'nlx-info-target))))))

		 (dolist (pred (block-pred block))
		   (unless (eq pred (component-head (block-component block)))
		     (maybe-enqueue pred)))))
	(loop
	  (unless head
	    (return))
	  (let* ((block (pop head))
		 (info (block-info block))
		 (total-consumes (byte-block-info-total-consumes info))
		 (produces-sset (byte-block-info-produces-sset info))
		 (did-anything nil))
	    (setf (byte-block-info-already-queued info) nil)
	    (dolist (succ (block-succ block))
	      (unless (eq succ (component-tail component))
		(let ((succ-info (block-info succ)))
		  (when (sset-union-of-difference
			 total-consumes
			 (byte-block-info-total-consumes succ-info)
			 produces-sset)
		    (setf did-anything t)))))
	    (dolist (nlx-list (byte-block-info-nlx-entries info))
	      (dolist (nlx-info (first nlx-list))
		(when (sset-union-of-difference
		       total-consumes
		       (byte-block-info-total-consumes
			(block-info
			 (nlx-info-target nlx-info)))
		       produces-sset)
		  (setf did-anything t))))
	    (when did-anything
	      (maybe-enqueue-predecessors block)))))))

  (walk-successors (component-head component) nil)
  (values))

;;;; Actually generate the byte code.

(defvar *byte-component-info*)

;;; FIXME: These might as well be generated with DEFENUM, right?
;;; It would also be nice to give them less ambiguous names, perhaps
;;; with a "BYTEOP-" prefix instead of "BYTE-".
(defconstant byte-push-local	       #b00000000)
(defconstant byte-push-arg	       #b00010000)
(defconstant byte-push-constant	       #b00100000)
(defconstant byte-push-system-constant #b00110000)
(defconstant byte-push-int	       #b01000000)
(defconstant byte-push-neg-int	       #b01010000)
(defconstant byte-pop-local	       #b01100000)
(defconstant byte-pop-n		       #b01110000)
(defconstant byte-call		       #b10000000)
(defconstant byte-tail-call	       #b10010000)
(defconstant byte-multiple-call	       #b10100000)
(defconstant byte-named		       #b00001000)
(defconstant byte-local-call	       #b10110000)
(defconstant byte-local-tail-call      #b10111000)
(defconstant byte-local-multiple-call  #b11000000)
(defconstant byte-return	       #b11001000)
(defconstant byte-branch-always	       #b11010000)
(defconstant byte-branch-if-true       #b11010010)
(defconstant byte-branch-if-false      #b11010100)
(defconstant byte-branch-if-eq	       #b11010110)
(defconstant byte-xop		       #b11011000)
(defconstant byte-inline-function      #b11100000)

(defun output-push-int (segment int)
  (declare (type sb!assem:segment segment)
	   (type (integer #.(- (ash 1 24)) #.(1- (ash 1 24)))))
  (if (minusp int)
      (output-byte-with-operand segment byte-push-neg-int (- (1+ int)))
      (output-byte-with-operand segment byte-push-int int)))

(defun output-push-constant-leaf (segment constant)
  (declare (type sb!assem:segment segment)
	   (type constant constant))
  (let ((info (constant-info constant)))
    (if info
	(output-byte-with-operand segment
				  (ecase (car info)
				    (:system-constant
				     byte-push-system-constant)
				    (:local-constant
				     byte-push-constant))
				  (cdr info))
	(let ((const (constant-value constant)))
	  (if (and (integerp const) (< (- (ash 1 24)) const (ash 1 24)))
	      ;; It can be represented as an immediate.
	      (output-push-int segment const)
	      ;; We need to store it in the constants pool.
	      (let* ((posn
		      (unless (and (consp const)
				   (eq (car const) '%fdefinition-marker%))
			(gethash const *system-constant-codes*)))
		     (new-info (if posn
				   (cons :system-constant posn)
				   (cons :local-constant
					 (vector-push-extend
					  constant
					  (byte-component-info-constants
					   *byte-component-info*))))))
		(setf (constant-info constant) new-info)
		(output-push-constant-leaf segment constant)))))))

(defun output-push-constant (segment value)
  (if (and (integerp value)
	   (< (- (ash 1 24)) value (ash 1 24)))
      (output-push-int segment value)
      (output-push-constant-leaf segment (find-constant value))))

;;; Return the offset of a load-time constant in the constant pool,
;;; adding it if absent.
(defun byte-load-time-constant-index (kind datum)
  (let ((constants (byte-component-info-constants *byte-component-info*)))
    (or (position-if #'(lambda (x)
			 (and (consp x)
			      (eq (car x) kind)
			      (typecase datum
				(cons (equal (cdr x) datum))
				(ctype (type= (cdr x) datum))
				(t
				 (eq (cdr x) datum)))))
		     constants)
	(vector-push-extend (cons kind datum) constants))))

(defun output-push-load-time-constant (segment kind datum)
  (output-byte-with-operand segment byte-push-constant
			    (byte-load-time-constant-index kind datum))
  (values))

(defun output-do-inline-function (segment function)
  ;; Note: we don't annotate this as a call site, because it is used
  ;; for internal stuff. Functions that get inlined have code
  ;; locations added byte generate-byte-code-for-full-call below.
  (output-byte segment
	       (logior byte-inline-function
		       (inline-function-number-or-lose function))))

(defun output-do-xop (segment xop)
  (let ((index (xop-index-or-lose xop)))
    (cond ((< index 7)
	   (output-byte segment (logior byte-xop index)))
	  (t
	   (output-byte segment (logior byte-xop 7))
	   (output-byte segment index)))))

(defun closure-position (var env)
  (or (position var (environment-closure env))
      (error "Can't find ~S" var)))

(defun output-ref-lambda-var (segment var env
				     &optional (indirect-value-cells t))
  (declare (type sb!assem:segment segment)
	   (type lambda-var var)
	   (type environment env))
  (if (eq (lambda-environment (lambda-var-home var)) env)
      (let ((info (leaf-info var)))
	(output-byte-with-operand segment
				  (if (byte-lambda-var-info-argp info)
				      byte-push-arg
				      byte-push-local)
				  (byte-lambda-var-info-offset info)))
      (output-byte-with-operand segment
				byte-push-arg
				(closure-position var env)))
  (when (and indirect-value-cells (lambda-var-indirect var))
    (output-do-inline-function segment 'value-cell-ref)))

(defun output-ref-nlx-info (segment info env)
  (if (eq (node-environment (cleanup-mess-up (nlx-info-cleanup info))) env)
      (output-byte-with-operand segment
				byte-push-local
				(byte-nlx-info-stack-slot
				 (nlx-info-info info)))
      (output-byte-with-operand segment
				byte-push-arg
				(closure-position info env))))

(defun output-set-lambda-var (segment var env &optional make-value-cells)
  (declare (type sb!assem:segment segment)
	   (type lambda-var var)
	   (type environment env))
  (let ((indirect (lambda-var-indirect var)))
    (cond ((not (eq (lambda-environment (lambda-var-home var)) env))
	   ;; This is not this guy's home environment. So we need to
	   ;; get it the value cell out of the closure, and fill it in.
	   (aver indirect)
	   (aver (not make-value-cells))
	   (output-byte-with-operand segment byte-push-arg
				     (closure-position var env))
	   (output-do-inline-function segment 'value-cell-setf))
	  (t
	   (let* ((pushp (and indirect (not make-value-cells)))
		  (byte-code (if pushp byte-push-local byte-pop-local))
		  (info (leaf-info var)))
	     (aver (not (byte-lambda-var-info-argp info)))
	     (when (and indirect make-value-cells)
	       ;; Replace the stack top with a value cell holding the
	       ;; stack top.
	       (output-do-inline-function segment 'make-value-cell))
	     (output-byte-with-operand segment byte-code
				       (byte-lambda-var-info-offset info))
	     (when pushp
	       (output-do-inline-function segment 'value-cell-setf)))))))

;;; Output whatever noise is necessary to canonicalize the values on
;;; the top of the stack. DESIRED is the number we want, and SUPPLIED
;;; is the number we have. Either push NIL or pop-n to make them
;;; balanced. Note: either desired or supplied can be :unknown, in
;;; which case it means use the ``unknown-values'' convention (which
;;; is the stack values followed by the number of values).
(defun canonicalize-values (segment desired supplied)
  (declare (type sb!assem:segment segment)
	   (type (or (member :unknown) index) desired supplied))
  (cond ((eq desired :unknown)
	 (unless (eq supplied :unknown)
	   (output-byte-with-operand segment byte-push-int supplied)))
	((eq supplied :unknown)
	 (unless (eq desired :unknown)
	   (output-push-int segment desired)
	   (output-do-xop segment 'default-unknown-values)))
	((< supplied desired)
	 (dotimes (i (- desired supplied))
	   (output-push-constant segment nil)))
	((> supplied desired)
	 (output-byte-with-operand segment byte-pop-n (- supplied desired))))
  (values))

(defparameter *byte-type-weakenings*
  (mapcar #'specifier-type
	  '(fixnum single-float double-float simple-vector simple-bit-vector
		   bit-vector)))

;;; Emit byte code to check that the value on top of the stack is of
;;; the specified TYPE. NODE is used for policy information. We weaken
;;; or entirely omit the type check whether speed is more important
;;; than safety.
(defun byte-generate-type-check (segment type node)
  (declare (type ctype type) (type node node))
  (unless (or (policy node (zerop safety))
	      (csubtypep *universal-type* type))
    (let ((type (if (policy node (> speed safety))
		    (dolist (super *byte-type-weakenings* type)
		      (when (csubtypep type super) (return super)))
		    type)))
      (output-do-xop segment 'type-check)
      (output-extended-operand
       segment
       (byte-load-time-constant-index :type-predicate type)))))

;;; This function is used when we are generating code which delivers
;;; values to a continuation. If this continuation needs a type check,
;;; and has a single value, then we do a type check. We also
;;; CANONICALIZE-VALUES for the continuation's desired number of
;;; values (without the placeholders.)
;;;
;;; Somewhat unrelatedly, we also push placeholders for deleted
;;; arguments to local calls. Although we check first, the actual
;;; PUSH-N-UNDER is done afterward, since then the single value we
;;; want is stack top.
(defun checked-canonicalize-values (segment cont supplied)
  (let ((info (continuation-info cont)))
    (if info
	(let ((desired (byte-continuation-info-results info))
	      (placeholders (byte-continuation-info-placeholders info)))
	  (unless (zerop placeholders)
	    (aver (eql desired (1+ placeholders)))
	    (setq desired 1))

	  (flet ((do-check ()
		   (byte-generate-type-check
		    segment
		    (single-value-type (continuation-asserted-type cont))
		    (continuation-dest cont))))
	    (cond
	     ((member (continuation-type-check cont) '(nil :deleted))
	      (canonicalize-values segment desired supplied))
	     ((eql supplied 1)
	      (do-check)
	      (canonicalize-values segment desired supplied))
	     ((eql desired 1)
	      (canonicalize-values segment desired supplied)
	      (do-check))
	     (t
	      (canonicalize-values segment desired supplied))))

	  (unless (zerop placeholders)
	    (output-do-xop segment 'push-n-under)
	    (output-extended-operand segment placeholders)))

	(canonicalize-values segment 0 supplied))))

;;; Emit prologue for non-LET functions. Assigned arguments must be
;;; copied into locals, and argument type checking may need to be done.
(defun generate-byte-code-for-bind (segment bind cont)
  (declare (type sb!assem:segment segment) (type bind bind)
	   (ignore cont))
  (let ((lambda (bind-lambda bind))
	(env (node-environment bind)))
    (ecase (lambda-kind lambda)
      ((nil :top-level :escape :cleanup :optional)
       (let* ((info (lambda-info lambda))
	      (type-check (policy (lambda-bind lambda) (not (zerop safety))))
	      (frame-size (byte-lambda-info-stack-size info)))
	 (cond ((< frame-size (* 255 2))
		(output-byte segment (ceiling frame-size 2)))
	       (t
		(output-byte segment 255)
		(output-byte segment (ldb (byte 8 16) frame-size))
		(output-byte segment (ldb (byte 8 8) frame-size))
		(output-byte segment (ldb (byte 8 0) frame-size))))

	 (do ((argnum (1- (+ (length (lambda-vars lambda))
			     (length (environment-closure
				      (lambda-environment lambda)))))
		      (1- argnum))
	      (vars (lambda-vars lambda) (cdr vars))
	      (pops 0))
	     ((null vars)
	      (unless (zerop pops)
		(output-byte-with-operand segment byte-pop-n pops)))
	   (declare (fixnum argnum pops))
	   (let* ((var (car vars))
		  (info (lambda-var-info var))
		  (type (leaf-type var)))
	     (cond ((not info))
		   ((byte-lambda-var-info-argp info)
		    (when (and type-check
			       (not (csubtypep *universal-type* type)))
		      (output-byte-with-operand segment byte-push-arg argnum)
		      (byte-generate-type-check segment type bind)
		      (incf pops)))
		   (t
		    (output-byte-with-operand segment byte-push-arg argnum)
		    (when type-check
		      (byte-generate-type-check segment type bind))
		    (output-set-lambda-var segment var env t)))))))

      ;; Everything has been taken care of in the combination node.
      ((:let :mv-let :assignment))))
  (values))

;;; This hashtable translates from n-ary function names to the
;;; two-arg-specific versions which we call to avoid &REST-arg consing.
(defvar *two-arg-functions* (make-hash-table :test 'eq))

(dolist (fun '((sb!kernel:two-arg-ior  logior)
	       (sb!kernel:two-arg-*  *)
	       (sb!kernel:two-arg-+  +)
	       (sb!kernel:two-arg-/  /)
	       (sb!kernel:two-arg--  -)
	       (sb!kernel:two-arg->  >)
	       (sb!kernel:two-arg-<  <)
	       (sb!kernel:two-arg-=  =)
	       (sb!kernel:two-arg-lcm  lcm)
	       (sb!kernel:two-arg-and  logand)
	       (sb!kernel:two-arg-gcd  gcd)
	       (sb!kernel:two-arg-xor  logxor)

	       (two-arg-char= char=)
	       (two-arg-char< char<)
	       (two-arg-char> char>)
	       (two-arg-char-equal char-equal)
	       (two-arg-char-lessp char-lessp)
	       (two-arg-char-greaterp char-greaterp)
	       (two-arg-string= string=)
	       (two-arg-string< string<)
	       (two-arg-string> string>)))

  (setf (gethash (second fun) *two-arg-functions*) (first fun)))

;;; If a system constant, push that, otherwise use a load-time constant.
(defun output-push-fdefinition (segment name)
  (let ((offset (gethash `(%fdefinition-marker% . ,name)
			 *system-constant-codes*)))
    (if offset
	(output-byte-with-operand segment byte-push-system-constant
				  offset)
	(output-push-load-time-constant segment :fdefinition name))))

(defun generate-byte-code-for-ref (segment ref cont)
  (declare (type sb!assem:segment segment) (type ref ref)
	   (type continuation cont))
  (let ((info (continuation-info cont)))
    ;; If there is no info, then nobody wants the result.
    (when info
      (let ((values (byte-continuation-info-results info))
	    (leaf (ref-leaf ref)))
	(cond
	 ((eq values :fdefinition)
	  (aver (and (global-var-p leaf)
		     (eq (global-var-kind leaf)
			 :global-function)))
	  (let* ((name (global-var-name leaf))
		 (found (gethash name *two-arg-functions*)))
	    (output-push-fdefinition
	     segment
	     (if (and found
		      (= (length (combination-args (continuation-dest cont)))
			 2))
		 found
		 name))))
	 ((eql values 0)
	  ;; really easy!
	  nil)
	 (t
	  (etypecase leaf
	    (constant
             (cond ((legal-immediate-constant-p leaf)
                     (output-push-constant-leaf segment leaf))
                   (t
                     (output-push-constant segment (leaf-name leaf))
                     (output-do-inline-function segment 'symbol-value))))
	    (clambda
	     (let* ((referred-env (lambda-environment leaf))
		    (closure (environment-closure referred-env)))
	       (if (null closure)
		   (output-push-load-time-constant segment :entry leaf)
		   (let ((my-env (node-environment ref)))
		     (output-push-load-time-constant segment :entry leaf)
		     (dolist (thing closure)
		       (etypecase thing
			 (lambda-var
			  (output-ref-lambda-var segment thing my-env nil))
			 (nlx-info
			  (output-ref-nlx-info segment thing my-env))))
		     (output-push-int segment (length closure))
		     (output-do-xop segment 'make-closure)))))
	    (functional
	     (output-push-load-time-constant segment :entry leaf))
	    (lambda-var
	     (output-ref-lambda-var segment leaf (node-environment ref)))
	    (global-var
	     (ecase (global-var-kind leaf)
	       ((:special :global :constant)
		(output-push-constant segment (global-var-name leaf))
		(output-do-inline-function segment 'symbol-value))
	       (:global-function
		(output-push-fdefinition segment (global-var-name leaf))
		(output-do-xop segment 'fdefn-function-or-lose)))))
	  (checked-canonicalize-values segment cont 1))))))
  (values))

(defun generate-byte-code-for-set (segment set cont)
  (declare (type sb!assem:segment segment) (type cset set)
	   (type continuation cont))
  (let* ((leaf (set-var set))
	 (info (continuation-info cont))
	 (values (if info
		     (byte-continuation-info-results info)
		     0)))
    (unless (eql values 0)
      ;; Someone wants the value, so copy it.
      (output-do-xop segment 'dup))
    (etypecase leaf
      (global-var        
       (ecase (global-var-kind leaf)
	 ((:special :global)
	  (output-push-constant segment (global-var-name leaf))
	  (output-do-inline-function segment 'setf-symbol-value))))
      (lambda-var
        ;; Note: It's important to test for whether there are any
        ;; references to the variable before we actually try to set it.
        ;; (Setting a lexical variable with no refs caused bugs ca. CMU
        ;; CL 18c, because the compiler deletes such variables.)
        (cond ((leaf-refs leaf)                
                (output-set-lambda-var segment leaf (node-environment set)))
              ;; If no one wants the value, then pop it, else leave it
              ;; for them.
              ((eql values 0)
                (output-byte-with-operand segment byte-pop-n 1)))))
    (unless (eql values 0)
      (checked-canonicalize-values segment cont 1)))
  (values))

(defun generate-byte-code-for-local-call (segment call cont num-args)
  (let* ((lambda (combination-lambda call))
	 (vars (lambda-vars lambda))
	 (env (lambda-environment lambda)))
    (ecase (functional-kind lambda)
      ((:let :assignment)
       (dolist (var (reverse vars))
	 (when (lambda-var-refs var)
	   (output-set-lambda-var segment var env t))))
      (:mv-let
       (let ((do-check (member (continuation-type-check
				(first (basic-combination-args call)))
			       '(t :error))))
	 (dolist (var (reverse vars))
	   (when do-check
	     (byte-generate-type-check segment (leaf-type var) call))
	   (output-set-lambda-var segment var env t))))
      ((nil :optional :cleanup)
       ;; We got us a local call.
       (aver (not (eq num-args :unknown)))
       ;; Push any trailing placeholder args...
       (dolist (x (reverse (basic-combination-args call)))
	 (when x (return))
	 (output-push-int segment 0))
       ;; Then push closure vars.
       (let ((closure (environment-closure env)))
	 (when closure
	   (let ((my-env (node-environment call)))
	     (dolist (thing (reverse closure))
	       (etypecase thing
		 (lambda-var
		  (output-ref-lambda-var segment thing my-env nil))
		 (nlx-info
		  (output-ref-nlx-info segment thing my-env)))))
	   (incf num-args (length closure))))
       (let ((results
	      (let ((info (continuation-info cont)))
		(if info
		    (byte-continuation-info-results info)
		    0))))
	 ;; Emit the op for whatever flavor of call we are using.
	 (let ((operand
		(cond ((> num-args 6)
		       (output-push-int segment num-args)
		       7)
		      (t
		       num-args))))
	   (multiple-value-bind (opcode ret-vals)
	       (cond ((node-tail-p call)
		      (values byte-local-tail-call 0))
		     ((member results '(0 1))
		      (values byte-local-call 1))
		     (t
		      (values byte-local-multiple-call :unknown)))
	     ;; ### :call-site
	     (output-byte segment (logior opcode operand))
	     ;; Emit a reference to the label.
	     (output-reference segment
			       (byte-lambda-info-label (lambda-info lambda)))
	     ;; ### :unknown-return
	     ;; Fix up the results.
	     (unless (node-tail-p call)
	       (checked-canonicalize-values segment cont ret-vals))))))))
  (values))

(defun generate-byte-code-for-full-call (segment call cont num-args)
  (let ((info (basic-combination-info call))
	(results
	 (let ((info (continuation-info cont)))
	   (if info
	       (byte-continuation-info-results info)
	       0))))
    (cond
     (info
      ;; It's an inline function.
      (aver (not (node-tail-p call)))
      (let* ((type (inline-function-info-type info))
	     (desired-args (function-type-nargs type))
	     (supplied-results
	      (nth-value 1
			 (values-types (function-type-returns type))))
	     (leaf (ref-leaf (continuation-use (basic-combination-fun call)))))
	(cond ((slot-accessor-p leaf)
	       (aver (= num-args (1- desired-args)))
	       (output-push-int segment (dsd-index (slot-accessor-slot leaf))))
	      (t
	       (canonicalize-values segment desired-args num-args)))
	;; ### :call-site
	(output-byte segment (logior byte-inline-function
				     (inline-function-info-number info)))
	;; ### :known-return
	(checked-canonicalize-values segment cont supplied-results)))
     (t
      (let ((operand
	     (cond ((eq num-args :unknown)
		    7)
		   ((> num-args 6)
		    (output-push-int segment num-args)
		    7)
		   (t
		    num-args))))
	(when (eq (byte-continuation-info-results
		   (continuation-info
		    (basic-combination-fun call)))
		  :fdefinition)
	  (setf operand (logior operand byte-named)))
	;; ### :call-site
	(cond
	 ((node-tail-p call)
	  (output-byte segment (logior byte-tail-call operand)))
	 (t
	  (multiple-value-bind (opcode ret-vals)
	      (case results
		(:unknown (values byte-multiple-call :unknown))
		((0 1) (values byte-call 1))
		(t (values byte-multiple-call :unknown)))
	  (output-byte segment (logior opcode operand))
	  ;; ### :unknown-return
	  (checked-canonicalize-values segment cont ret-vals)))))))))

(defun generate-byte-code-for-known-call (segment call cont num-args)
  (block nil
    (catch 'give-up-ir1-transform
      (funcall (function-info-byte-compile (basic-combination-kind call)) call
	       (let ((info (continuation-info cont)))
		 (if info
		     (byte-continuation-info-results info)
		     0))
	       num-args segment)
      (return))
    (aver (member (byte-continuation-info-results
		   (continuation-info
		    (basic-combination-fun call)))
		  '(1 :fdefinition)))
    (generate-byte-code-for-full-call segment call cont num-args))
  (values))

(defun generate-byte-code-for-generic-combination (segment call cont)
  (declare (type sb!assem:segment segment) (type basic-combination call)
	   (type continuation cont))
  (labels ((examine (args num-fixed)
	     (cond
	      ((null args)
	       ;; None of the arugments supply :UNKNOWN values, so
	       ;; we know exactly how many there are.
	       num-fixed)
	      (t
	       (let* ((vals
		       (byte-continuation-info-results
			(continuation-info (car args)))))
		 (cond
		  ((eq vals :unknown)
		   (unless (null (cdr args))
		     ;; There are (LENGTH ARGS) :UNKNOWN value blocks on
		     ;; the top of the stack. We need to combine them.
		     (output-push-int segment (length args))
		     (output-do-xop segment 'merge-unknown-values))
		   (unless (zerop num-fixed)
		     ;; There are num-fixed fixed args above the unknown
		     ;; values block that want in on the action also.
		     ;; So add num-fixed to the count.
		     (output-push-int segment num-fixed)
		     (output-do-inline-function segment '+))
		   :unknown)
		  (t
		   (examine (cdr args) (+ num-fixed vals)))))))))
    (let* ((args (basic-combination-args call))
	   (kind (basic-combination-kind call))
	   (num-args (if (and (eq kind :local)
			      (combination-p call))
			 (length args)
			 (examine args 0))))
      (case kind
	(:local
	 (generate-byte-code-for-local-call segment call cont num-args))
	(:full
	 (generate-byte-code-for-full-call segment call cont num-args))
	(t
	 (generate-byte-code-for-known-call segment call cont num-args))))))

(defun generate-byte-code-for-basic-combination (segment call cont)
  (cond ((and (mv-combination-p call)
	      (eq (continuation-function-name (basic-combination-fun call))
		  '%throw))
	 ;; ### :internal-error
	 (output-do-xop segment 'throw))
	(t
	 (generate-byte-code-for-generic-combination segment call cont))))

(defun generate-byte-code-for-if (segment if cont)
  (declare (type sb!assem:segment segment) (type cif if)
	   (ignore cont))
  (let* ((next-info (byte-block-info-next (block-info (node-block if))))
	 (consequent-info (block-info (if-consequent if)))
	 (alternate-info (block-info (if-alternative if))))
    (cond ((eq (byte-continuation-info-results
		(continuation-info (if-test if)))
	       :eq-test)
	   (output-branch segment
			  byte-branch-if-eq
			  (byte-block-info-label consequent-info))
	   (unless (eq next-info alternate-info)
	     (output-branch segment
			    byte-branch-always
			    (byte-block-info-label alternate-info))))
	  ((eq next-info consequent-info)
	   (output-branch segment
			  byte-branch-if-false
			  (byte-block-info-label alternate-info)))
	  (t
	   (output-branch segment
			  byte-branch-if-true
			  (byte-block-info-label consequent-info))
	   (unless (eq next-info alternate-info)
	     (output-branch segment
			    byte-branch-always
			    (byte-block-info-label alternate-info)))))))

(defun generate-byte-code-for-return (segment return cont)
  (declare (type sb!assem:segment segment) (type creturn return)
	   (ignore cont))
  (let* ((result (return-result return))
	 (info (continuation-info result))
	 (results (byte-continuation-info-results info)))
    (cond ((eq results :unknown)
	   (setf results 7))
	  ((> results 6)
	   (output-byte-with-operand segment byte-push-int results)
	   (setf results 7)))
    (output-byte segment (logior byte-return results)))
  (values))

(defun generate-byte-code-for-entry (segment entry cont)
  (declare (type sb!assem:segment segment) (type entry entry)
	   (ignore cont))
  (dolist (exit (entry-exits entry))
    (let ((nlx-info (find-nlx-info entry (node-cont exit))))
      (when nlx-info
	(let ((kind (cleanup-kind (nlx-info-cleanup nlx-info))))
	  (when (member kind '(:block :tagbody))
	    ;; Generate a unique tag.
	    (output-push-constant
	     segment
	     (format nil
		     "tag for ~A"
		     (component-name *component-being-compiled*)))
	    (output-push-constant segment nil)
	    (output-do-inline-function segment 'cons)
	    ;; Save it so people can close over it.
	    (output-do-xop segment 'dup)
	    (output-byte-with-operand segment
				      byte-pop-local
				      (byte-nlx-info-stack-slot
				       (nlx-info-info nlx-info)))
	    ;; Now do the actual XOP.
	    (ecase kind
	      (:block
	       (output-do-xop segment 'catch)
	       (output-reference segment
				 (byte-nlx-info-label
				  (nlx-info-info nlx-info))))
	      (:tagbody
	       (output-do-xop segment 'tagbody)))
	    (return))))))
  (values))

(defun generate-byte-code-for-exit (segment exit cont)
  (declare (ignore cont))
  (let ((nlx-info (find-nlx-info (exit-entry exit) (node-cont exit))))
    (output-byte-with-operand segment
			      byte-push-arg
			      (closure-position nlx-info
						(node-environment exit)))
    (ecase (cleanup-kind (nlx-info-cleanup nlx-info))
      (:block
       ;; ### :internal-error
       (output-do-xop segment 'return-from))
      (:tagbody
       ;; ### :internal-error
       (output-do-xop segment 'go)
       (output-reference segment
			 (byte-nlx-info-label (nlx-info-info nlx-info)))))))

(defun generate-byte-code (segment component)
  (let ((*byte-component-info* (component-info component)))
    (do* ((info (byte-block-info-next (block-info (component-head component)))
		next)
	  (block (byte-block-info-block info) (byte-block-info-block info))
	  (next (byte-block-info-next info) (byte-block-info-next info)))
	 ((eq block (component-tail component)))
      (when (block-interesting block)
	(output-label segment (byte-block-info-label info))
	(do-nodes (node cont block)
	  (etypecase node
	    (bind (generate-byte-code-for-bind segment node cont))
	    (ref (generate-byte-code-for-ref segment node cont))
	    (cset (generate-byte-code-for-set segment node cont))
	    (basic-combination
	     (generate-byte-code-for-basic-combination
	      segment node cont))
	    (cif (generate-byte-code-for-if segment node cont))
	    (creturn (generate-byte-code-for-return segment node cont))
	    (entry (generate-byte-code-for-entry segment node cont))
	    (exit
	     (when (exit-entry node)
	       (generate-byte-code-for-exit segment node cont)))))
	(let* ((succ (block-succ block))
	       (first-succ (car succ))
	       (last (block-last block)))
	  (unless (or (cdr succ)
		      (eq (byte-block-info-block next) first-succ)
		      (eq (component-tail component) first-succ)
		      (and (basic-combination-p last)
			   (node-tail-p last)
			   ;; Tail local calls that have been
			   ;; converted to an assignment need the
			   ;; branch.
			   (not (and (eq (basic-combination-kind last) :local)
				     (member (functional-kind
					      (combination-lambda last))
					     '(:let :assignment))))))
	    (output-branch segment
			   byte-branch-always
			   (byte-block-info-label
			    (block-info first-succ))))))))
  (values))

;;;; special purpose annotate/compile optimizers

(defoptimizer (eq byte-annotate) ((this that) node)
  (declare (ignore this that))
  (when (if-p (continuation-dest (node-cont node)))
    (annotate-known-call node)
    t))

(defoptimizer (eq byte-compile) ((this that) call results num-args segment)
  (progn segment) ; ignorable.
  ;; We don't have to do anything, because everything is handled by
  ;; the IF byte-generator.
  (aver (eq results :eq-test))
  (aver (eql num-args 2))
  (values))

(defoptimizer (values byte-compile)
	      ((&rest values) node results num-args segment)
  (canonicalize-values segment results num-args))

(defknown %byte-pop-stack (index) (values))

(defoptimizer (%byte-pop-stack byte-annotate) ((count) node)
  (aver (constant-continuation-p count))
  (annotate-continuation count 0)
  (annotate-continuation (basic-combination-fun node) 0)
  (setf (node-tail-p node) nil)
  t)

(defoptimizer (%byte-pop-stack byte-compile)
	      ((count) node results num-args segment)
  (aver (and (zerop num-args) (zerop results)))
  (output-byte-with-operand segment byte-pop-n (continuation-value count)))

(defoptimizer (%special-bind byte-annotate) ((var value) node)
  (annotate-continuation var 0)
  (annotate-continuation value 1)
  (annotate-continuation (basic-combination-fun node) 0)
  (setf (node-tail-p node) nil)
  t)

(defoptimizer (%special-bind byte-compile)
	      ((var value) node results num-args segment)
  (aver (and (eql num-args 1) (zerop results)))
  (output-push-constant segment (leaf-name (continuation-value var)))
  (output-do-inline-function segment '%byte-special-bind))

(defoptimizer (%special-unbind byte-annotate) ((var) node)
  (annotate-continuation var 0)
  (annotate-continuation (basic-combination-fun node) 0)
  (setf (node-tail-p node) nil)
  t)

(defoptimizer (%special-unbind byte-compile)
	      ((var) node results num-args segment)
  (aver (and (zerop num-args) (zerop results)))
  (output-do-inline-function segment '%byte-special-unbind))

(defoptimizer (%catch byte-annotate) ((nlx-info tag) node)
  (annotate-continuation nlx-info 0)
  (annotate-continuation tag 1)
  (annotate-continuation (basic-combination-fun node) 0)
  (setf (node-tail-p node) nil)
  t)

(defoptimizer (%catch byte-compile)
	      ((nlx-info tag) node results num-args segment)
  (progn node) ; ignore
  (aver (and (= num-args 1) (zerop results)))
  (output-do-xop segment 'catch)
  (let ((info (nlx-info-info (continuation-value nlx-info))))
    (output-reference segment (byte-nlx-info-label info))))

(defoptimizer (%cleanup-point byte-compile) (() node results num-args segment)
  (progn node segment) ; ignore
  (aver (and (zerop num-args) (zerop results))))

(defoptimizer (%catch-breakup byte-compile) (() node results num-args segment)
  (progn node) ; ignore
  (aver (and (zerop num-args) (zerop results)))
  (output-do-xop segment 'breakup))

(defoptimizer (%lexical-exit-breakup byte-annotate) ((nlx-info) node)
  (annotate-continuation nlx-info 0)
  (annotate-continuation (basic-combination-fun node) 0)
  (setf (node-tail-p node) nil)
  t)

(defoptimizer (%lexical-exit-breakup byte-compile)
	      ((nlx-info) node results num-args segment)
  (aver (and (zerop num-args) (zerop results)))
  (let ((nlx-info (continuation-value nlx-info)))
    (when (ecase (cleanup-kind (nlx-info-cleanup nlx-info))
	    (:block
	     ;; We only want to do this for the fall-though case.
	     (not (eq (car (block-pred (node-block node)))
		      (nlx-info-target nlx-info))))
	    (:tagbody
	     ;; Only want to do it once per tagbody.
	     (not (byte-nlx-info-duplicate (nlx-info-info nlx-info)))))
      (output-do-xop segment 'breakup))))

(defoptimizer (%nlx-entry byte-annotate) ((nlx-info) node)
  (annotate-continuation nlx-info 0)
  (annotate-continuation (basic-combination-fun node) 0)
  (setf (node-tail-p node) nil)
  t)

(defoptimizer (%nlx-entry byte-compile)
	      ((nlx-info) node results num-args segment)
  (progn node results) ; ignore
  (aver (eql num-args 0))
  (let* ((info (continuation-value nlx-info))
	 (byte-info (nlx-info-info info)))
    (output-label segment (byte-nlx-info-label byte-info))
    ;; ### :non-local-entry
    (ecase (cleanup-kind (nlx-info-cleanup info))
      ((:catch :block)
       (checked-canonicalize-values segment
				    (nlx-info-continuation info)
				    :unknown))
      ((:tagbody :unwind-protect)))))

(defoptimizer (%unwind-protect byte-annotate)
	      ((nlx-info cleanup-fun) node)
  (annotate-continuation nlx-info 0)
  (annotate-continuation cleanup-fun 0)
  (annotate-continuation (basic-combination-fun node) 0)
  (setf (node-tail-p node) nil)
  t)

(defoptimizer (%unwind-protect byte-compile)
	      ((nlx-info cleanup-fun) node results num-args segment)
  (aver (and (zerop num-args) (zerop results)))
  (output-do-xop segment 'unwind-protect)
  (output-reference segment
		    (byte-nlx-info-label
		     (nlx-info-info
		      (continuation-value nlx-info)))))

(defoptimizer (%unwind-protect-breakup byte-compile)
	      (() node results num-args segment)
  (progn node) ; ignore
  (aver (and (zerop num-args) (zerop results)))
  (output-do-xop segment 'breakup))

(defoptimizer (%continue-unwind byte-annotate) ((a b c) node)
  (annotate-continuation a 0)
  (annotate-continuation b 0)
  (annotate-continuation c 0)
  (annotate-continuation (basic-combination-fun node) 0)
  (setf (node-tail-p node) nil)
  t)

(defoptimizer (%continue-unwind byte-compile)
	      ((a b c) node results num-args segment)
  (progn node) ; ignore
  (aver (member results '(0 nil)))
  (aver (eql num-args 0))
  (output-do-xop segment 'breakup))

(defoptimizer (%load-time-value byte-annotate) ((handle) node)
  (annotate-continuation handle 0)
  (annotate-continuation (basic-combination-fun node) 0)
  (setf (node-tail-p node) nil)
  t)

(defoptimizer (%load-time-value byte-compile)
	      ((handle) node results num-args segment)
  (progn node) ; ignore
  (aver (zerop num-args))
  (output-push-load-time-constant segment :load-time-value
				  (continuation-value handle))
  (canonicalize-values segment results 1))

;;; Make a byte-function for LAMBDA.
(defun make-xep-for (lambda)
  (flet ((entry-point-for (entry)
	   (let ((info (lambda-info entry)))
	     (aver (byte-lambda-info-interesting info))
	     (sb!assem:label-position (byte-lambda-info-label info)))))
    (let ((entry (lambda-entry-function lambda)))
      (etypecase entry
	(optional-dispatch
	 (let ((rest-arg-p nil)
	       (num-more 0))
	   (declare (type index num-more))
	   (collect ((keywords))
	     (dolist (var (nthcdr (optional-dispatch-max-args entry)
				  (optional-dispatch-arglist entry)))
	       (let ((arg-info (lambda-var-arg-info var)))
		 (aver arg-info)
		 (ecase (arg-info-kind arg-info)
		   (:rest
		    (aver (not rest-arg-p))
		    (incf num-more)
		    (setf rest-arg-p t))
		   (:keyword
		    ;; FIXME: Since ANSI specifies that &KEY arguments
		    ;; needn't actually be keywords, :KEY would be a
		    ;; better label for this behavior than :KEYWORD is,
		    ;; and (KEY-ARGS) would be a better name for the
		    ;; accumulator than (KEYWORDS) is.
		    (let ((s-p (arg-info-supplied-p arg-info))
			  (default (arg-info-default arg-info)))
		      (incf num-more (if s-p 2 1))
		      (keywords (list (arg-info-key arg-info)
				      (if (constantp default)
					  (eval default)
					  nil)
				      (if s-p t nil))))))))
	     (make-hairy-byte-function
	      :name (leaf-name entry)
	      :min-args (optional-dispatch-min-args entry)
	      :max-args (optional-dispatch-max-args entry)
	      :entry-points
	      (mapcar #'entry-point-for (optional-dispatch-entry-points entry))
	      :more-args-entry-point
	      (entry-point-for (optional-dispatch-main-entry entry))
	      :num-more-args num-more
	      :rest-arg-p rest-arg-p
	      :keywords-p
	      (if (optional-dispatch-keyp entry)
		  (if (optional-dispatch-allowp entry)
		      :allow-others t))
	      :keywords (keywords)))))
	(clambda
	 (let ((args (length (lambda-vars entry))))
	   (make-simple-byte-function
	    :name (leaf-name entry)
	    :num-args args
	    :entry-point (entry-point-for entry))))))))

(defun generate-xeps (component)
  (let ((xeps nil))
    (dolist (lambda (component-lambdas component))
      (when (member (lambda-kind lambda) '(:external :top-level))
	(push (cons lambda (make-xep-for lambda)) xeps)))
    xeps))

;;;; noise to actually do the compile

(defun assign-locals (component)
  ;; Process all of the lambdas in component, and assign stack frame
  ;; locations for all the locals.
  (dolist (lambda (component-lambdas component))
    ;; We don't generate any code for :external lambdas, so we don't need
    ;; to allocate stack space. Also, we don't use the ``more'' entry,
    ;; so we don't need code for it.
    (cond
     ((or (eq (lambda-kind lambda) :external)
	  (and (eq (lambda-kind lambda) :optional)
	       (eq (optional-dispatch-more-entry
		    (lambda-optional-dispatch lambda))
		   lambda)))
      (setf (lambda-info lambda)
	    (make-byte-lambda-info :interesting nil)))
     (t
      (let ((num-locals 0))
	(let* ((vars (lambda-vars lambda))
	       (arg-num (+ (length vars)
			   (length (environment-closure
				    (lambda-environment lambda))))))
	  (dolist (var vars)
	    (decf arg-num)
	    (cond ((or (lambda-var-sets var) (lambda-var-indirect var))
		   (setf (leaf-info var)
			 (make-byte-lambda-var-info :offset num-locals))
		   (incf num-locals))
		  ((leaf-refs var)
		   (setf (leaf-info var)
			 (make-byte-lambda-var-info :argp t
						    :offset arg-num))))))
	(dolist (let (lambda-lets lambda))
	  (dolist (var (lambda-vars let))
	    (setf (leaf-info var)
		  (make-byte-lambda-var-info :offset num-locals))
	    (incf num-locals)))
	(let ((entry-nodes-already-done nil))
	  (dolist (nlx-info (environment-nlx-info (lambda-environment lambda)))
	    (ecase (cleanup-kind (nlx-info-cleanup nlx-info))
	      (:block
	       (setf (nlx-info-info nlx-info)
		     (make-byte-nlx-info :stack-slot num-locals))
	       (incf num-locals))
	      (:tagbody
	       (let* ((entry (cleanup-mess-up (nlx-info-cleanup nlx-info)))
		      (cruft (assoc entry entry-nodes-already-done)))
		 (cond (cruft
			(setf (nlx-info-info nlx-info)
			      (make-byte-nlx-info :stack-slot (cdr cruft)
						  :duplicate t)))
		       (t
			(push (cons entry num-locals) entry-nodes-already-done)
			(setf (nlx-info-info nlx-info)
			      (make-byte-nlx-info :stack-slot num-locals))
			(incf num-locals)))))
	      ((:catch :unwind-protect)
	       (setf (nlx-info-info nlx-info) (make-byte-nlx-info))))))
	(setf (lambda-info lambda)
	      (make-byte-lambda-info :stack-size num-locals))))))

  (values))

(defun byte-compile-component (component)
  (setf (component-info component) (make-byte-component-info))
  (maybe-mumble "ByteAnn ")

  ;; Assign offsets for all the locals, and figure out which args can
  ;; stay in the argument area and which need to be moved into locals.
  (assign-locals component)

  ;; Annotate every continuation with information about how we want the
  ;; values.
  (annotate-ir1 component)

  ;; Determine what stack values are dead, and emit cleanup code to pop
  ;; them.
  (byte-stack-analyze component)

  ;; Make sure any newly added blocks have a block-number.
  (dfo-as-needed component)

  ;; Assign an ordering of the blocks.
  (control-analyze component #'make-byte-block-info)

  ;; Find the start labels for the lambdas.
  (dolist (lambda (component-lambdas component))
    (let ((info (lambda-info lambda)))
      (when (byte-lambda-info-interesting info)
	(setf (byte-lambda-info-label info)
	      (byte-block-info-label
	       (block-info (node-block (lambda-bind lambda))))))))

  ;; Delete any blocks that we are not going to emit from the emit order.
  (do-blocks (block component)
    (unless (block-interesting block)
      (let* ((info (block-info block))
	     (prev (byte-block-info-prev info))
	     (next (byte-block-info-next info)))
	(setf (byte-block-info-next prev) next)
	(setf (byte-block-info-prev next) prev))))

  (maybe-mumble "ByteGen ")
  (let ((segment nil))
    (unwind-protect
	(progn
	  (setf segment (sb!assem:make-segment :name "Byte Output"))
	  (generate-byte-code segment component)
	  (let ((code-length (sb!assem:finalize-segment segment))
		(xeps (generate-xeps component))
		(constants (byte-component-info-constants
			    (component-info component))))
	    (when *compiler-trace-output*
	      (describe-component component *compiler-trace-output*)
	      (describe-byte-component component xeps segment
				       *compiler-trace-output*))
	    (etypecase *compile-object*
	      (fasl-output
	       (maybe-mumble "FASL")
	       (fasl-dump-byte-component segment code-length constants xeps
					 *compile-object*))
	      (core-object
	       (maybe-mumble "Core")
	       (make-core-byte-component segment code-length constants xeps
					 *compile-object*))
	      (null))))))
  (values))

;;;; extra stuff for debugging

#!+sb-show
(defun dump-stack-info (component)
  (do-blocks (block component)
     (when (block-interesting block)
       (print-nodes block)
       (let ((info (block-info block)))
	 (cond
	  (info
	   (format t
	   "start-stack ~S~%consume ~S~%produce ~S~%end-stack ~S~%~
	    total-consume ~S~%~@[nlx-entries ~S~%~]~@[nlx-entry-p ~S~%~]"
	   (byte-block-info-start-stack info)
	   (byte-block-info-consumes info)
	   (byte-block-info-produces info)
	   (byte-block-info-end-stack info)
	   (byte-block-info-total-consumes info)
	   (byte-block-info-nlx-entries info)
	   (byte-block-info-nlx-entry-p info)))
	  (t
	   (format t "no info~%")))))))
