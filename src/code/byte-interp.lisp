;;;; the byte code interpreter

(in-package "SB!C")

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;; We need at least this level of DEBUGness in order for the local
;;; declaration in WITH-DEBUGGER-INFO to take effect.
;;;
;;; FIXME: This will cause source code location information to be
;;; compiled into the executable, which will probably cause problems
;;; for users running without the sources and/or without the
;;; build-the-system readtable.
(declaim (optimize (debug 2)))

;;; Return a function type approximating the type of a byte-compiled
;;; function. We really only capture the arg signature.
(defun byte-function-type (x)
  (specifier-type
   (etypecase x
     (simple-byte-function
      `(function ,(make-list (simple-byte-function-num-args x)
			     :initial-element t)
		 *))
     (hairy-byte-function
      (collect ((res))
	(let ((min (hairy-byte-function-min-args x))
	      (max (hairy-byte-function-max-args x)))
	  (dotimes (i min) (res t))
	  (when (> max min)
	    (res '&optional)
	    (dotimes (i (- max min))
	      (res t))))
	(when (hairy-byte-function-rest-arg-p x)
	  (res '&rest t))
	(ecase (hairy-byte-function-keywords-p x)
	  ((t :allow-others)
	   (res '&key)
	   (dolist (key (hairy-byte-function-keywords x))
		   (res `(,(car key) t)))
	   (when (eql (hairy-byte-function-keywords-p x) :allow-others)
	     (res '&allow-other-keys)))
	  ((nil)))
	`(function ,(res) *))))))

;;;; the evaluation stack

;;; the interpreter's evaluation stack
(defvar *eval-stack* (make-array 100)) ; will grow as needed
;;; FIXME: This seems to be used by the ordinary (non-byte) interpreter
;;; too, judging from a crash I had when I removed byte-interp.lisp from
;;; the cold build sequence. It would probably be clearer to pull the
;;; shared interpreter machinery out of the byte interpreter and ordinary
;;; interpreter files and put them into their own file shared-interp.lisp
;;; or something.

;;; the index of the next free element of the interpreter's evaluation stack
(defvar *eval-stack-top* 0)

#!-sb-fluid (declaim (inline eval-stack-ref))
(defun eval-stack-ref (offset)
  (declare (type stack-pointer offset))
  (svref sb!eval::*eval-stack* offset))

#!-sb-fluid (declaim (inline (setf eval-stack-ref)))
(defun (setf eval-stack-ref) (new-value offset)
  (declare (type stack-pointer offset))
  (setf (svref sb!eval::*eval-stack* offset) new-value))

(defun push-eval-stack (value)
  (let ((len (length (the simple-vector sb!eval::*eval-stack*)))
	(sp *eval-stack-top*))
    (when (= len sp)
      (let ((new-stack (make-array (ash len 1))))
	(replace new-stack sb!eval::*eval-stack* :end1 len :end2 len)
	(setf sb!eval::*eval-stack* new-stack)))
    (setf *eval-stack-top* (1+ sp))
    (setf (eval-stack-ref sp) value)))

(defun allocate-eval-stack (amount)
  (let* ((len (length (the simple-vector sb!eval::*eval-stack*)))
	 (sp *eval-stack-top*)
	 (new-sp (+ sp amount)))
    (declare (type index sp new-sp))
    (when (>= new-sp len)
      (let ((new-stack (make-array (ash new-sp 1))))
	(replace new-stack sb!eval::*eval-stack* :end1 len :end2 len)
	(setf sb!eval::*eval-stack* new-stack)))
    (setf *eval-stack-top* new-sp)
    (let ((stack sb!eval::*eval-stack*))
      (do ((i sp (1+ i))) ; FIXME: DOTIMES? or just :INITIAL-ELEMENT in MAKE-ARRAY?
	  ((= i new-sp))
	(setf (svref stack i) '#:uninitialized))))
  (values))

(defun pop-eval-stack ()
  (let* ((new-sp (1- *eval-stack-top*))
	 (value (eval-stack-ref new-sp)))
    (setf *eval-stack-top* new-sp)
    value))

(defmacro multiple-value-pop-eval-stack ((&rest vars) &body body)
  #+nil (declare (optimize (inhibit-warnings 3)))
  (let ((num-vars (length vars))
	(index -1)
	(new-sp-var (gensym "NEW-SP-"))
	(decls nil))
    (loop
      (unless (and (consp body)
		   (consp (car body))
		   (eq (caar body) 'declare))
	(return))
      (push (pop body) decls))
    `(let ((,new-sp-var (- *eval-stack-top* ,num-vars)))
       (declare (type stack-pointer ,new-sp-var))
       (let ,(mapcar #'(lambda (var)
			 `(,var (eval-stack-ref
				 (+ ,new-sp-var ,(incf index)))))
		     vars)
	 ,@(nreverse decls)
	 (setf *eval-stack-top* ,new-sp-var)
	 ,@body))))

(defun eval-stack-copy (dest src count)
  (declare (type stack-pointer dest src count))
  (let ((stack *eval-stack*))
    (if (< dest src)
	(dotimes (i count)
	  (setf (svref stack dest) (svref stack src))
	  (incf dest)
	  (incf src))
	(do ((si (1- (+ src count))
		 (1- si))
	     (di (1- (+ dest count))
		 (1- di)))
	    ((< si src))
	  (declare (fixnum si di))
	  (setf (svref stack di) (svref stack si)))))
  (values))

;;;; component access magic

#!-sb-fluid (declaim (inline component-ref))
(defun component-ref (component pc)
  (declare (type code-component component)
	   (type pc pc))
  (sap-ref-8 (code-instructions component) pc))

#!-sb-fluid (declaim (inline (setf component-ref)))
(defun (setf component-ref) (value component pc)
  (declare (type (unsigned-byte 8) value)
	   (type code-component component)
	   (type pc pc))
  (setf (sap-ref-8 (code-instructions component) pc) value))

#!-sb-fluid (declaim (inline component-ref-signed))
(defun component-ref-signed (component pc)
  (let ((byte (component-ref component pc)))
    (if (logbitp 7 byte)
	(logior (ash -1 8) byte)
	byte)))

#!-sb-fluid (declaim (inline component-ref-24))
(defun component-ref-24 (component pc)
  (logior (ash (component-ref component pc) 16)
	  (ash (component-ref component (1+ pc)) 8)
	  (component-ref component (+ pc 2))))

;;;; debugging support

;;; This macro binds three magic variables. When the debugger notices that
;;; these three variables are bound, it makes a byte-code frame out of the
;;; supplied information instead of a compiled frame. We set each var in
;;; addition to binding it so the compiler doens't optimize away the binding.
(defmacro with-debugger-info ((component pc fp) &body body)
  `(let ((%byte-interp-component ,component)
	 (%byte-interp-pc ,pc)
	 (%byte-interp-fp ,fp))
     ;; FIXME: This will cause source code location information to be compiled
     ;; into the executable, which will probably cause problems for users
     ;; running without the sources and/or without the build-the-system
     ;; readtable.
     (declare (optimize (debug 3)))
     (setf %byte-interp-component %byte-interp-component)
     (setf %byte-interp-pc %byte-interp-pc)
     (setf %byte-interp-fp %byte-interp-fp)
     ,@body))

(defun byte-install-breakpoint (component pc)
  (declare (type code-component component)
	   (type pc pc)
	   (values (unsigned-byte 8)))
  (let ((orig (component-ref component pc)))
    (setf (component-ref component pc)
	  #.(logior byte-xop
		    (xop-index-or-lose 'breakpoint)))
    orig))

(defun byte-remove-breakpoint (component pc orig)
  (declare (type code-component component)
	   (type pc pc)
	   (type (unsigned-byte 8) orig)
	   (values (unsigned-byte 8)))
  (setf (component-ref component pc) orig))

(defun byte-skip-breakpoint (component pc fp orig)
  (declare (type code-component component)
	   (type pc pc)
	   (type stack-pointer fp)
	   (type (unsigned-byte 8) orig))
  (byte-interpret-byte component fp pc orig))

;;;; system constants

;;; a table mapping system constant indices to run-time values. We don't
;;; reference the compiler variable at load time, since the interpreter is
;;; loaded first.
(defparameter *system-constants*
  (let ((res (make-array 256)))
    (dolist (x '#.(collect ((res))
		    (dohash (key value *system-constant-codes*)
		      (res (cons key value)))
		    (res)))
      (let ((key (car x))
	    (value (cdr x)))
	(setf (svref res value)
	      (if (and (consp key) (eq (car key) '%fdefinition-marker%))
		  (fdefinition-object (cdr key) t)
		  key))))
    res))

;;;; byte compiled function constructors/extractors

(defun initialize-byte-compiled-function (xep)
  (declare (type byte-function xep))
  (push xep (code-header-ref (byte-function-component xep)
			     sb!vm:code-trace-table-offset-slot))
  (setf (funcallable-instance-function xep)
	#'(instance-lambda (&more context count)
	    (let ((old-sp *eval-stack-top*))
	      (declare (type stack-pointer old-sp))
	      (dotimes (i count)
		(push-eval-stack (%more-arg context i)))
	      (invoke-xep nil 0 old-sp 0 count xep))))
  xep)

(defun make-byte-compiled-closure (xep closure-vars)
  (declare (type byte-function xep)
	   (type simple-vector closure-vars))
  (let ((res (make-byte-closure xep closure-vars)))
    (setf (funcallable-instance-function res)
	  #'(instance-lambda (&more context count)
	      (let ((old-sp *eval-stack-top*))
		(declare (type stack-pointer old-sp))
		(dotimes (i count)
		  (push-eval-stack (%more-arg context i)))
		(invoke-xep nil 0 old-sp 0 count
			    (byte-closure-function res)
			    (byte-closure-data res)))))
    res))

;;;; INLINEs

;;; (The idea here seems to be to make sure it's at least 100,
;;; in order to be able to compile the 32+ inline functions
;;; in EXPAND-INTO-INLINES as intended. -- WHN 19991206)
(eval-when (:compile-toplevel :execute)
  (setq sb!ext:*inline-expansion-limit* 100))

;;; FIXME: This doesn't seem to be needed in the target Lisp, only
;;; at build-the-system time.
;;;
;;; KLUDGE: This expands into code like
;;; (IF (ZEROP (LOGAND BYTE 16))
;;;     (IF (ZEROP (LOGAND BYTE 8))
;;;	 (IF (ZEROP (LOGAND BYTE 4))
;;;	     (IF (ZEROP (LOGAND BYTE 2))
;;;		 (IF (ZEROP (LOGAND BYTE 1))
;;;		     (ERROR "Unknown inline function, id=~D" 0)
;;;		     (ERROR "Unknown inline function, id=~D" 1))
;;;		 (IF (ZEROP (LOGAND BYTE 1))
;;;		     (ERROR "Unknown inline function, id=~D" 2)
;;;		     (ERROR "Unknown inline function, id=~D" 3)))
;;;	     (IF (ZEROP (LOGAND BYTE 2))
;;;	 ..) ..) ..)
;;; That's probably more efficient than doing a function call (even a
;;; local function call) for every byte interpreted, but I doubt it's
;;; as fast as doing a jump through a table of sixteen addresses.
;;; Perhaps it would be good to recode this as a straightforward
;;; CASE statement and redirect the cleverness previously devoted to
;;; this code to an optimizer for CASE which is smart enough to
;;; implement suitable code as jump tables.
(defmacro expand-into-inlines ()
  #+nil (declare (optimize (inhibit-warnings 3)))
  (named-let build-dispatch ((bit 4)
			     (base 0))
    (if (minusp bit)
	(let ((info (svref *inline-functions* base)))
	  (if info
	      (let* ((spec (type-specifier
			    (inline-function-info-type info)))
		     (arg-types (second spec))
		     (result-type (third spec))
		     (args (make-gensym-list (length arg-types)))
		     (func
		      `(the ,result-type
			    (,(inline-function-info-interpreter-function info)
			     ,@args))))
		`(multiple-value-pop-eval-stack ,args
		   (declare ,@(mapcar #'(lambda (type var)
					  `(type ,type ,var))
				      arg-types args))
		   ,(if (and (consp result-type)
			     (eq (car result-type) 'values))
			(let ((results (make-gensym-list
					(length (cdr result-type)))))
			  `(multiple-value-bind ,results ,func
			     ,@(mapcar #'(lambda (res)
					   `(push-eval-stack ,res))
				       results)))
			`(push-eval-stack ,func))))
	      `(error "unknown inline function, id=~D" ,base)))
	`(if (zerop (logand byte ,(ash 1 bit)))
	     ,(build-dispatch (1- bit) base)
	     ,(build-dispatch (1- bit) (+ base (ash 1 bit)))))))

#!-sb-fluid (declaim (inline value-cell-setf))
(defun value-cell-setf (value cell)
  (value-cell-set cell value)
  value)

#!-sb-fluid (declaim (inline setf-symbol-value))
(defun setf-symbol-value (value symbol)
  (setf (symbol-value symbol) value))

#!-sb-fluid (declaim (inline %setf-instance-ref))
(defun %setf-instance-ref (new-value instance index)
  (setf (%instance-ref instance index) new-value))

(eval-when (:compile-toplevel)

(sb!xc:defmacro %byte-symbol-value (x)
  `(let ((x ,x))
     (unless (boundp x)
       (with-debugger-info (component pc fp)
	 (error "unbound variable: ~S" x)))
     (symbol-value x)))

(sb!xc:defmacro %byte-car (x)
  `(let ((x ,x))
     (unless (listp x)
       (with-debugger-info (component pc fp)
	 (error 'simple-type-error :item x :expected-type 'list
		:format-control "non-list argument to CAR: ~S"
		:format-arguments (list x))))
     (car x)))

(sb!xc:defmacro %byte-cdr (x)
  `(let ((x ,x))
     (unless (listp x)
       (with-debugger-info (component pc fp)
	 (error 'simple-type-error :item x :expected-type 'list
		:format-control "non-list argument to CDR: ~S"
		:format-arguments (list x))))
     (cdr x)))

) ; EVAL-WHEN

#!-sb-fluid (declaim (inline %byte-special-bind))
(defun %byte-special-bind (value symbol)
  (sb!sys:%primitive bind value symbol)
  (values))

#!-sb-fluid (declaim (inline %byte-special-unbind))
(defun %byte-special-unbind ()
  (sb!sys:%primitive unbind)
  (values))

;;;; two-arg function stubs
;;;;
;;;; We have two-arg versions of some n-ary functions that are normally
;;;; open-coded.

(defun two-arg-char= (x y) (char= x y))
(defun two-arg-char< (x y) (char< x y))
(defun two-arg-char> (x y) (char> x y))
(defun two-arg-char-equal (x y) (char-equal x y))
(defun two-arg-char-lessp (x y) (char-lessp x y))
(defun two-arg-char-greaterp (x y) (char-greaterp x y))
(defun two-arg-string= (x y) (string= x y))
(defun two-arg-string< (x y) (string= x y))
(defun two-arg-string> (x y) (string= x y))

;;;; miscellaneous primitive stubs

(macrolet ((def-frob (name &optional (args '(x)))
	     `(defun ,name ,args (,name ,@args))))
  (def-frob %code-code-size)
  (def-frob %code-debug-info)
  (def-frob %code-entry-points)
  (def-frob %funcallable-instance-function)
  (def-frob %funcallable-instance-layout)
  (def-frob %funcallable-instance-lexenv)
  (def-frob %function-next)
  (def-frob %function-self)
  (def-frob %set-funcallable-instance-function (fin new-val)))

;;;; funny functions

;;; (used both by the byte interpreter and by the IR1 interpreter)
(defun %progv (vars vals fun)
  (progv vars vals
    (funcall fun)))

;;;; XOPs

;;; Extension operations (XOPs) are various magic things that the byte
;;; interpreter needs to do, but can't be represented as a function call.
;;; When the byte interpreter encounters an XOP in the byte stream, it
;;; tail-calls the corresponding XOP routine extracted from *byte-xops*.
;;; The XOP routine can do whatever it wants, probably re-invoking the
;;; byte interpreter.

;;; Fetch an 8/24 bit operand out of the code stream.
(eval-when (:compile-toplevel :execute)
  (sb!xc:defmacro with-extended-operand ((component pc operand new-pc)
					 &body body)
    (once-only ((n-component component)
		(n-pc pc))
      `(multiple-value-bind (,operand ,new-pc)
	   (let ((,operand (component-ref ,n-component ,n-pc)))
	     (if (= ,operand #xff)
		 (values (component-ref-24 ,n-component (1+ ,n-pc))
			 (+ ,n-pc 4))
		 (values ,operand (1+ ,n-pc))))
	 (declare (type index ,operand ,new-pc))
	 ,@body))))

;;; If a real XOP hasn't been defined, this gets invoked and signals an
;;; error. This shouldn't happen in normal operation.
(defun undefined-xop (component old-pc pc fp)
  (declare (ignore component old-pc pc fp))
  (error "undefined XOP"))

;;; a simple vector of the XOP functions
(declaim (type (simple-vector 256) *byte-xops*))
(defvar *byte-xops*
  (make-array 256 :initial-element #'undefined-xop))

;;; Define a XOP function and install it in *BYTE-XOPS*.
(eval-when (:compile-toplevel :execute)
  (sb!xc:defmacro define-xop (name lambda-list &body body)
    (let ((defun-name (symbolicate "BYTE-" name "-XOP")))
      `(progn
	 (defun ,defun-name ,lambda-list
	   ,@body)
	 (setf (aref *byte-xops* ,(xop-index-or-lose name)) #',defun-name)
	 ',defun-name))))

;;; This is spliced in by the debugger in order to implement breakpoints.
(define-xop breakpoint (component old-pc pc fp)
  (declare (type code-component component)
	   (type pc old-pc)
	   (ignore pc)
	   (type stack-pointer fp))
  ;; Invoke the debugger.
  (with-debugger-info (component old-pc fp)
    (sb!di::handle-breakpoint component old-pc fp))
  ;; Retry the breakpoint XOP in case it was replaced with the original
  ;; displaced byte-code.
  (byte-interpret component old-pc fp))

;;; This just duplicates whatever is on the top of the stack.
(define-xop dup (component old-pc pc fp)
  (declare (type code-component component)
	   (ignore old-pc)
	   (type pc pc)
	   (type stack-pointer fp))
  (let ((value (eval-stack-ref (1- *eval-stack-top*))))
    (push-eval-stack value))
  (byte-interpret component pc fp))

(define-xop make-closure (component old-pc pc fp)
  (declare (type code-component component)
	   (ignore old-pc)
	   (type pc pc)
	   (type stack-pointer fp))
  (let* ((num-closure-vars (pop-eval-stack))
	 (closure-vars (make-array num-closure-vars)))
    (declare (type index num-closure-vars)
	     (type simple-vector closure-vars))
    (named-let frob ((index (1- num-closure-vars)))
      (unless (minusp index)
	(setf (svref closure-vars index) (pop-eval-stack))
	(frob (1- index))))
    (push-eval-stack (make-byte-compiled-closure (pop-eval-stack)
						 closure-vars)))
  (byte-interpret component pc fp))

(define-xop merge-unknown-values (component old-pc pc fp)
  (declare (type code-component component)
	   (ignore old-pc)
	   (type pc pc)
	   (type stack-pointer fp))
  (labels ((grovel (remaining-blocks block-count-ptr)
	     (declare (type index remaining-blocks)
		      (type stack-pointer block-count-ptr))
	     (declare (values index stack-pointer))
	     (let ((block-count (eval-stack-ref block-count-ptr)))
	       (declare (type index block-count))
	       (if (= remaining-blocks 1)
		   (values block-count block-count-ptr)
		   (let ((src (- block-count-ptr block-count)))
		     (declare (type index src))
		     (multiple-value-bind (values-above dst)
			 (grovel (1- remaining-blocks) (1- src))
		       (eval-stack-copy dst src block-count)
		       (values (+ values-above block-count)
			       (+ dst block-count))))))))
    (multiple-value-bind (total-count end-ptr)
	(grovel (pop-eval-stack) (1- *eval-stack-top*))
      (setf (eval-stack-ref end-ptr) total-count)
      (setf *eval-stack-top* (1+ end-ptr))))
  (byte-interpret component pc fp))

(define-xop default-unknown-values (component old-pc pc fp)
  (declare (type code-component component)
	   (ignore old-pc)
	   (type pc pc)
	   (type stack-pointer fp))
  (let* ((desired (pop-eval-stack))
	 (supplied (pop-eval-stack))
	 (delta (- desired supplied)))
    (declare (type index desired supplied)
	     (type fixnum delta))
    (cond ((minusp delta)
	   (incf *eval-stack-top* delta))
	  ((plusp delta)
	   (dotimes (i delta)
	     (push-eval-stack nil)))))
  (byte-interpret component pc fp))

;;; %THROW is compiled down into this xop. The stack contains the tag, the
;;; values, and then a count of the values. We special case various small
;;; numbers of values to keep from consing if we can help it.
;;;
;;; Basically, we just extract the values and the tag and then do a throw.
;;; The native compiler will convert this throw into whatever is necessary
;;; to throw, so we don't have to duplicate all that cruft.
(define-xop throw (component old-pc pc fp)
  (declare (type code-component component)
	   (type pc old-pc)
	   (ignore pc)
	   (type stack-pointer fp))
  (let ((num-results (pop-eval-stack)))
    (declare (type index num-results))
    (case num-results
      (0
       (let ((tag (pop-eval-stack)))
	 (with-debugger-info (component old-pc fp)
	   (throw tag (values)))))
      (1
       (multiple-value-pop-eval-stack
	   (tag result)
	 (with-debugger-info (component old-pc fp)
	   (throw tag result))))
      (2
       (multiple-value-pop-eval-stack
	   (tag result0 result1)
	 (with-debugger-info (component old-pc fp)
	   (throw tag (values result0 result1)))))
      (t
       (let ((results nil))
	 (dotimes (i num-results)
	   (push (pop-eval-stack) results))
	 (let ((tag (pop-eval-stack)))
	   (with-debugger-info (component old-pc fp)
	     (throw tag (values-list results)))))))))

;;; This is used for both CATCHes and BLOCKs that are closed over. We
;;; establish a catcher for the supplied tag (from the stack top), and
;;; recursivly enter the byte interpreter. If the byte interpreter exits,
;;; it must have been because of a BREAKUP (see below), so we branch (by
;;; tail-calling the byte interpreter) to the pc returned by BREAKUP.
;;; If we are thrown to, then we branch to the address encoded in the 3 bytes
;;; following the catch XOP.
(define-xop catch (component old-pc pc fp)
  (declare (type code-component component)
	   (ignore old-pc)
	   (type pc pc)
	   (type stack-pointer fp))
  (let ((new-pc (block nil
		  (let ((results
			 (multiple-value-list
			  (catch (pop-eval-stack)
			    (return (byte-interpret component (+ pc 3) fp))))))
		    (let ((num-results 0))
		      (declare (type index num-results))
		      (dolist (result results)
			(push-eval-stack result)
			(incf num-results))
		      (push-eval-stack num-results))
		    (component-ref-24 component pc)))))
    (byte-interpret component new-pc fp)))

;;; Blow out of the dynamically nested CATCH or TAGBODY. We just return the
;;; pc following the BREAKUP XOP and the drop-through code in CATCH or
;;; TAGBODY will do the correct thing.
(define-xop breakup (component old-pc pc fp)
  (declare (ignore component old-pc fp)
	   (type pc pc))
  pc)

;;; This is exactly like THROW, except that the tag is the last thing on
;;; the stack instead of the first. This is used for RETURN-FROM (hence the
;;; name).
(define-xop return-from (component old-pc pc fp)
  (declare (type code-component component)
	   (type pc old-pc)
	   (ignore pc)
	   (type stack-pointer fp))
  (let ((tag (pop-eval-stack))
	(num-results (pop-eval-stack)))
    (declare (type index num-results))
    (case num-results
      (0
       (with-debugger-info (component old-pc fp)
	 (throw tag (values))))
      (1
       (let ((value (pop-eval-stack)))
	 (with-debugger-info (component old-pc fp)
	   (throw tag value))))
      (2
       (multiple-value-pop-eval-stack
	   (result0 result1)
	 (with-debugger-info (component old-pc fp)
	   (throw tag (values result0 result1)))))
      (t
       (let ((results nil))
	 (dotimes (i num-results)
	   (push (pop-eval-stack) results))
	 (with-debugger-info (component old-pc fp)
	   (throw tag (values-list results))))))))

;;; Similar to CATCH, except for TAGBODY. One significant difference is that
;;; when thrown to, we don't want to leave the dynamic extent of the tagbody
;;; so we loop around and re-enter the catcher. We keep looping until BREAKUP
;;; is used to blow out. When that happens, we just branch to the pc supplied
;;; by BREAKUP.
(define-xop tagbody (component old-pc pc fp)
  (declare (type code-component component)
	   (ignore old-pc)
	   (type pc pc)
	   (type stack-pointer fp))
  (let* ((tag (pop-eval-stack))
	 (new-pc (block nil
		   (loop
		     (setf pc
			   (catch tag
			     (return (byte-interpret component pc fp))))))))
    (byte-interpret component new-pc fp)))

;;; Yup, you guessed it. This XOP implements GO. There are no values to
;;; pass, so we don't have to mess with them, and multiple exits can all be
;;; using the same tag so we have to pass the pc we want to go to.
(define-xop go (component old-pc pc fp)
  (declare (type code-component component)
	   (type pc old-pc pc)
	   (type stack-pointer fp))
  (let ((tag (pop-eval-stack))
	(new-pc (component-ref-24 component pc)))
    (with-debugger-info (component old-pc fp)
      (throw tag new-pc))))

;;; UNWIND-PROTECTs are handled significantly different in the byte
;;; compiler and the native compiler. Basically, we just use the
;;; native compiler's UNWIND-PROTECT, and let it worry about
;;; continuing the unwind.
(define-xop unwind-protect (component old-pc pc fp)
  (declare (type code-component component)
	   (ignore old-pc)
	   (type pc pc)
	   (type stack-pointer fp))
  (let ((new-pc nil))
    (unwind-protect
	(setf new-pc (byte-interpret component (+ pc 3) fp))
      (unless new-pc
	;; The cleanup function expects 3 values to be one the stack, so
	;; we have to put something there.
	(push-eval-stack nil)
	(push-eval-stack nil)
	(push-eval-stack nil)
	;; Now run the cleanup code.
	(byte-interpret component (component-ref-24 component pc) fp)))
    (byte-interpret component new-pc fp)))

(define-xop fdefn-function-or-lose (component old-pc pc fp)
  (let* ((fdefn (pop-eval-stack))
	 (fun (fdefn-function fdefn)))
    (declare (type fdefn fdefn))
    (cond (fun
	   (push-eval-stack fun)
	   (byte-interpret component pc fp))
	  (t
	   (with-debugger-info (component old-pc fp)
	     (error 'undefined-function :name (fdefn-name fdefn)))))))

;;; This is used to insert placeholder arguments for unused arguments
;;; to local calls.
(define-xop push-n-under (component old-pc pc fp)
  (declare (ignore old-pc))
  (with-extended-operand (component pc howmany new-pc)
    (let ((val (pop-eval-stack)))
      (allocate-eval-stack howmany)
      (push-eval-stack val))
    (byte-interpret component new-pc fp)))

;;;; type checking

;;; These two hashtables map between type specifiers and type
;;; predicate functions that test those types. They are initialized
;;; according to the standard type predicates of the target system.
(defvar *byte-type-predicates* (make-hash-table :test 'equal))
(defvar *byte-predicate-types* (make-hash-table :test 'eq))

(loop for (type predicate) in
	  '#.(loop for (type . predicate) in
		   *backend-type-predicates*
	       collect `(,(type-specifier type) ,predicate))
      do
  (let ((fun (fdefinition predicate)))
    (setf (gethash type *byte-type-predicates*) fun)
    (setf (gethash fun *byte-predicate-types*) type)))

;;; This is called by the loader to convert a type specifier into a
;;; type predicate (as used by the TYPE-CHECK XOP.) If it is a
;;; structure type with a predicate or has a predefined predicate,
;;; then return the predicate function, otherwise return the CTYPE
;;; structure for the type.
(defun load-type-predicate (desc)
  (or (gethash desc *byte-type-predicates*)
      (let ((type (specifier-type desc)))
	(if (typep type 'structure-class)
	    (let ((info (layout-info (class-layout type))))
	      (if (and info (eq (dd-type info) 'structure))
		  (let ((pred (dd-predicate info)))
		    (if (and pred (fboundp pred))
			(fdefinition pred)
			type))
		  type))
	    type))))

;;; Check the type of the value on the top of the stack. The type is
;;; designated by an entry in the constants. If the value is a
;;; function, then it is called as a type predicate. Otherwise, the
;;; value is a CTYPE object, and we call %TYPEP on it.
(define-xop type-check (component old-pc pc fp)
  (declare (type code-component component)
	   (type pc old-pc pc)
	   (type stack-pointer fp))
  (with-extended-operand (component pc operand new-pc)
    (let ((value (eval-stack-ref (1- *eval-stack-top*)))
	  (type (code-header-ref component
				 (+ operand sb!vm:code-constants-offset))))
      (unless (if (functionp type)
		  (funcall type value)
		  (%typep value type))
	(with-debugger-info (component old-pc fp)
	  (error 'type-error
		 :datum value
		 :expected-type (if (functionp type)
				    (gethash type *byte-predicate-types*)
				    (type-specifier type))))))

    (byte-interpret component new-pc fp)))

;;;; the actual byte-interpreter

;;; The various operations are encoded as follows.
;;;
;;; 0000xxxx push-local op
;;; 0001xxxx push-arg op   [push-local, but negative]
;;; 0010xxxx push-constant op
;;; 0011xxxx push-system-constant op
;;; 0100xxxx push-int op
;;; 0101xxxx push-neg-int op
;;; 0110xxxx pop-local op
;;; 0111xxxx pop-n op
;;; 1000nxxx call op
;;; 1001nxxx tail-call op
;;; 1010nxxx multiple-call op
;;; 10110xxx local-call
;;; 10111xxx local-tail-call
;;; 11000xxx local-multiple-call
;;; 11001xxx return
;;; 1101000r branch
;;; 1101001r if-true
;;; 1101010r if-false
;;; 1101011r if-eq
;;; 11011xxx Xop
;;; 11100000
;;;    to    various inline functions.
;;; 11111111
;;;
;;; This encoding is rather hard wired into BYTE-INTERPRET due to the
;;; binary dispatch tree.

(defvar *byte-trace* nil)

;;; the main entry point to the byte interpreter
(defun byte-interpret (component pc fp)
  (declare (type code-component component)
	   (type pc pc)
	   (type stack-pointer fp))
  (byte-interpret-byte component pc fp (component-ref component pc)))

;;; This is separated from BYTE-INTERPRET in order to let us continue
;;; from a breakpoint without having to replace the breakpoint with
;;; the original instruction and arrange to somehow put the breakpoint
;;; back after executing the instruction. We just leave the breakpoint
;;; there, and call this function with the byte that the breakpoint
;;; displaced.
(defun byte-interpret-byte (component pc fp byte)
  (declare (type code-component component)
	   (type pc pc)
	   (type stack-pointer fp)
	   (type (unsigned-byte 8) byte))
  (locally
    #+nil (declare (optimize (inhibit-warnings 3)))
    (when *byte-trace*
      (let ((*byte-trace* nil))
	(format *trace-output*
		"pc=~D, fp=~D, sp=~D, byte=#b~,'0X, frame:~%    ~S~%"
		pc fp *eval-stack-top* byte
		(subseq sb!eval::*eval-stack* fp *eval-stack-top*)))))
  (if (zerop (logand byte #x80))
      ;; Some stack operation. No matter what, we need the operand,
      ;; so compute it.
      (multiple-value-bind (operand new-pc)
	  (let ((operand (logand byte #xf)))
	    (if (= operand #xf)
		(let ((operand (component-ref component (1+ pc))))
		  (if (= operand #xff)
		      (values (component-ref-24 component (+ pc 2))
			      (+ pc 5))
		      (values operand (+ pc 2))))
		(values operand (1+ pc))))
	(if (zerop (logand byte #x40))
	    (push-eval-stack (if (zerop (logand byte #x20))
				 (if (zerop (logand byte #x10))
				     (eval-stack-ref (+ fp operand))
				     (eval-stack-ref (- fp operand 5)))
				 (if (zerop (logand byte #x10))
				     (code-header-ref
				      component
				      (+ operand sb!vm:code-constants-offset))
				     (svref *system-constants* operand))))
	    (if (zerop (logand byte #x20))
		(push-eval-stack (if (zerop (logand byte #x10))
				     operand
				     (- (1+ operand))))
		(if (zerop (logand byte #x10))
		    (setf (eval-stack-ref (+ fp operand)) (pop-eval-stack))
		    (if (zerop operand)
			(let ((operand (pop-eval-stack)))
			  (declare (type index operand))
			  (decf *eval-stack-top* operand))
			(decf *eval-stack-top* operand)))))
	(byte-interpret component new-pc fp))
      (if (zerop (logand byte #x40))
	  ;; Some kind of call.
	  (let ((args (let ((args (logand byte #x07)))
			(if (= args #x07)
			    (pop-eval-stack)
			    args))))
	    (if (zerop (logand byte #x20))
		(let ((named (not (zerop (logand byte #x08)))))
		  (if (zerop (logand byte #x10))
		      ;; Call for single value.
		      (do-call component pc (1+ pc) fp args named)
		      ;; Tail call.
		      (do-tail-call component pc fp args named)))
		(if (zerop (logand byte #x10))
		    ;; Call for multiple-values.
		    (do-call component pc (- (1+ pc)) fp args
			     (not (zerop (logand byte #x08))))
		    (if (zerop (logand byte #x08))
			;; Local call
			(do-local-call component pc (+ pc 4) fp args)
			;; Local tail-call
			(do-tail-local-call component pc fp args)))))
	  (if (zerop (logand byte #x20))
	      ;; local-multiple-call, Return, branch, or Xop.
	      (if (zerop (logand byte #x10))
		  ;; local-multiple-call or return.
		  (if (zerop (logand byte #x08))
		      ;; Local-multiple-call.
		      (do-local-call component pc (- (+ pc 4)) fp
				     (let ((args (logand byte #x07)))
				       (if (= args #x07)
					   (pop-eval-stack)
					   args)))
		      ;; Return.
		      (let ((num-results
			     (let ((num-results (logand byte #x7)))
			       (if (= num-results 7)
				   (pop-eval-stack)
				   num-results))))
			(do-return fp num-results)))
		  ;; Branch or Xop.
		  (if (zerop (logand byte #x08))
		      ;; Branch.
		      (if (if (zerop (logand byte #x04))
			      (if (zerop (logand byte #x02))
				  t
				  (pop-eval-stack))
			      (if (zerop (logand byte #x02))
				  (not (pop-eval-stack))
				  (multiple-value-pop-eval-stack
				   (val1 val2)
				   (eq val1 val2))))
			  ;; Branch taken.
			  (byte-interpret
			   component
			   (if (zerop (logand byte #x01))
			       (component-ref-24 component (1+ pc))
			       (+ pc 2
				  (component-ref-signed component (1+ pc))))
			   fp)
			  ;; Branch not taken.
			  (byte-interpret component
					  (if (zerop (logand byte #x01))
					      (+ pc 4)
					      (+ pc 2))
					  fp))
		      ;; Xop.
		      (multiple-value-bind (sub-code new-pc)
			  (let ((operand (logand byte #x7)))
			    (if (= operand #x7)
				(values (component-ref component (+ pc 1))
					(+ pc 2))
				(values operand (1+ pc))))
			(funcall (the function (svref *byte-xops* sub-code))
				 component pc new-pc fp))))
	      ;; some miscellaneous inline function
	      (progn
		(expand-into-inlines)
		(byte-interpret component (1+ pc) fp))))))

(defun do-local-call (component pc old-pc old-fp num-args)
  (declare (type pc pc)
	   (type return-pc old-pc)
	   (type stack-pointer old-fp)
	   (type (integer 0 #.call-arguments-limit) num-args))
  (invoke-local-entry-point component (component-ref-24 component (1+ pc))
			    component old-pc
			    (- *eval-stack-top* num-args)
			    old-fp))

(defun do-tail-local-call (component pc fp num-args)
  (declare (type code-component component) (type pc pc)
	   (type stack-pointer fp)
	   (type index num-args))
  (let ((old-fp (eval-stack-ref (- fp 1)))
	(old-sp (eval-stack-ref (- fp 2)))
	(old-pc (eval-stack-ref (- fp 3)))
	(old-component (eval-stack-ref (- fp 4)))
	(start-of-args (- *eval-stack-top* num-args)))
    (eval-stack-copy old-sp start-of-args num-args)
    (setf *eval-stack-top* (+ old-sp num-args))
    (invoke-local-entry-point component (component-ref-24 component (1+ pc))
			      old-component old-pc old-sp old-fp)))

(defun invoke-local-entry-point (component target old-component old-pc old-sp
					   old-fp &optional closure-vars)
  (declare (type pc target)
	   (type return-pc old-pc)
	   (type stack-pointer old-sp old-fp)
	   (type (or null simple-vector) closure-vars))
  (when closure-vars
    (named-let more ((index (1- (length closure-vars))))
      (unless (minusp index)
	(push-eval-stack (svref closure-vars index))
	(more (1- index)))))
  (push-eval-stack old-component)
  (push-eval-stack old-pc)
  (push-eval-stack old-sp)
  (push-eval-stack old-fp)
  (multiple-value-bind (stack-frame-size entry-pc)
      (let ((byte (component-ref component target)))
	(if (= byte 255)
	    (values (component-ref-24 component (1+ target)) (+ target 4))
	    (values (* byte 2) (1+ target))))
    (declare (type pc entry-pc))
    (let ((fp *eval-stack-top*))
      (allocate-eval-stack stack-frame-size)
      (byte-interpret component entry-pc fp))))

;;; Call a function with some arguments popped off of the interpreter
;;; stack, and restore the SP to the specified value.
(defun byte-apply (function num-args restore-sp)
  (declare (type function function) (type index num-args))
  (let ((start (- *eval-stack-top* num-args)))
    (declare (type stack-pointer start))
    (macrolet ((frob ()
		 `(case num-args
		    ,@(loop for n below 8
			collect `(,n (call-1 ,n)))
		    (t
		     (let ((args ())
			   (end (+ start num-args)))
		       (declare (type stack-pointer end))
		       (do ((i (1- end) (1- i)))
			   ((< i start))
			 (declare (fixnum i))
			 (push (eval-stack-ref i) args))
		       (setf *eval-stack-top* restore-sp)
		       (apply function args)))))
	       (call-1 (n)
		 (collect ((binds)
			   (args))
		   (dotimes (i n)
		     (let ((dum (gensym)))
		       (binds `(,dum (eval-stack-ref (+ start ,i))))
		       (args dum)))
		   `(let ,(binds)
		      (setf *eval-stack-top* restore-sp)
		      (funcall function ,@(args))))))
      (frob))))

;;; Note: negative RET-PC is a convention for "we need multiple return
;;; values".
(defun do-call (old-component call-pc ret-pc old-fp num-args named)
  (declare (type code-component old-component)
	   (type pc call-pc)
	   (type return-pc ret-pc)
	   (type stack-pointer old-fp)
	   (type (integer 0 #.call-arguments-limit) num-args)
	   (type (member t nil) named))
  (let* ((old-sp (- *eval-stack-top* num-args 1))
	 (fun-or-fdefn (eval-stack-ref old-sp))
	 (function (if named
		       (or (fdefn-function fun-or-fdefn)
			   (with-debugger-info (old-component call-pc old-fp)
			     (error 'undefined-function
				    :name (fdefn-name fun-or-fdefn))))
		       fun-or-fdefn)))
    (declare (type stack-pointer old-sp)
	     (type (or function fdefn) fun-or-fdefn)
	     (type function function))
    (typecase function
      (byte-function
       (invoke-xep old-component ret-pc old-sp old-fp num-args function))
      (byte-closure
       (invoke-xep old-component ret-pc old-sp old-fp num-args
		   (byte-closure-function function)
		   (byte-closure-data function)))
      (t
       (cond ((minusp ret-pc)
	      (let* ((ret-pc (- ret-pc))
		     (results
		      (multiple-value-list
		       (with-debugger-info
			   (old-component ret-pc old-fp)
			 (byte-apply function num-args old-sp)))))
		(dolist (result results)
		  (push-eval-stack result))
		(push-eval-stack (length results))
		(byte-interpret old-component ret-pc old-fp)))
	     (t
	      (push-eval-stack
	       (with-debugger-info
		   (old-component ret-pc old-fp)
		 (byte-apply function num-args old-sp)))
	      (byte-interpret old-component ret-pc old-fp)))))))

(defun do-tail-call (component pc fp num-args named)
  (declare (type code-component component)
	   (type pc pc)
	   (type stack-pointer fp)
	   (type (integer 0 #.call-arguments-limit) num-args)
	   (type (member t nil) named))
  (let* ((start-of-args (- *eval-stack-top* num-args))
	 (fun-or-fdefn (eval-stack-ref (1- start-of-args)))
	 (function (if named
		       (or (fdefn-function fun-or-fdefn)
			   (with-debugger-info (component pc fp)
			     (error 'undefined-function
				    :name (fdefn-name fun-or-fdefn))))
		       fun-or-fdefn))
	 (old-fp (eval-stack-ref (- fp 1)))
	 (old-sp (eval-stack-ref (- fp 2)))
	 (old-pc (eval-stack-ref (- fp 3)))
	 (old-component (eval-stack-ref (- fp 4))))
    (declare (type stack-pointer old-fp old-sp start-of-args)
	     (type return-pc old-pc)
	     (type (or fdefn function) fun-or-fdefn)
	     (type function function))
    (typecase function
      (byte-function
       (eval-stack-copy old-sp start-of-args num-args)
       (setf *eval-stack-top* (+ old-sp num-args))
       (invoke-xep old-component old-pc old-sp old-fp num-args function))
      (byte-closure
       (eval-stack-copy old-sp start-of-args num-args)
       (setf *eval-stack-top* (+ old-sp num-args))
       (invoke-xep old-component old-pc old-sp old-fp num-args
		   (byte-closure-function function)
		   (byte-closure-data function)))
      (t
       ;; We are tail-calling native code.
       (cond ((null old-component)
	      ;; We were called by native code.
	      (byte-apply function num-args old-sp))
	     ((minusp old-pc)
	      ;; We were called for multiple values. So return multiple
	      ;; values.
	      (let* ((old-pc (- old-pc))
		     (results
		      (multiple-value-list
		       (with-debugger-info
			(old-component old-pc old-fp)
			(byte-apply function num-args old-sp)))))
		(dolist (result results)
		  (push-eval-stack result))
		(push-eval-stack (length results))
		(byte-interpret old-component old-pc old-fp)))
	     (t
	      ;; We were called for one value. So return one value.
	      (push-eval-stack
	       (with-debugger-info
		   (old-component old-pc old-fp)
		 (byte-apply function num-args old-sp)))
	      (byte-interpret old-component old-pc old-fp)))))))

(defvar *byte-trace-calls* nil)

(defun invoke-xep (old-component ret-pc old-sp old-fp num-args xep
				 &optional closure-vars)
  (declare (type (or null code-component) old-component)
	   (type index num-args)
	   (type return-pc ret-pc)
	   (type stack-pointer old-sp old-fp)
	   (type byte-function xep)
	   (type (or null simple-vector) closure-vars))
  ;; FIXME: Perhaps BYTE-TRACE-CALLS stuff should be conditional on SB-SHOW.
  (when *byte-trace-calls*
    (let ((*byte-trace-calls* nil)
	  (*byte-trace* nil)
	  (*print-level* sb!debug:*debug-print-level*)
	  (*print-length* sb!debug:*debug-print-length*)
	  (sp *eval-stack-top*))
      (format *trace-output*
	      "~&INVOKE-XEP: ocode= ~S[~D]~%  ~
	       osp= ~D, ofp= ~D, nargs= ~D, SP= ~D:~%  ~
	       Fun= ~S ~@[~S~]~%  Args= ~S~%"
	      old-component ret-pc old-sp old-fp num-args sp
	      xep closure-vars (subseq *eval-stack* (- sp num-args) sp))
      (force-output *trace-output*)))

  (let ((entry-point
	 (cond
	  ((typep xep 'simple-byte-function)
	   (unless (eql (simple-byte-function-num-args xep) num-args)
	     (with-debugger-info (old-component ret-pc old-fp)
	       (error "wrong number of arguments")))
	   (simple-byte-function-entry-point xep))
	  (t
	   (let ((min (hairy-byte-function-min-args xep))
		 (max (hairy-byte-function-max-args xep)))
	     (cond
	      ((< num-args min)
	       (with-debugger-info (old-component ret-pc old-fp)
		 (error "not enough arguments")))
	      ((<= num-args max)
	       (nth (- num-args min) (hairy-byte-function-entry-points xep)))
	      ((null (hairy-byte-function-more-args-entry-point xep))
	       (with-debugger-info (old-component ret-pc old-fp)
		 (error "too many arguments")))
	      (t
	       (let* ((more-args-supplied (- num-args max))
		      (sp *eval-stack-top*)
		      (more-args-start (- sp more-args-supplied))
		      (restp (hairy-byte-function-rest-arg-p xep))
		      (rest (and restp
				 (do ((index (1- sp) (1- index))
				      (result nil
					      (cons (eval-stack-ref index)
						    result)))
				     ((< index more-args-start) result)
				   (declare (fixnum index))))))
		 (declare (type index more-args-supplied)
			  (type stack-pointer more-args-start))
		 (cond
		  ((not (hairy-byte-function-keywords-p xep))
		   (aver restp)
		   (setf *eval-stack-top* (1+ more-args-start))
		   (setf (eval-stack-ref more-args-start) rest))
		  (t
		   (unless (evenp more-args-supplied)
		     (with-debugger-info (old-component ret-pc old-fp)
		       (error "odd number of &KEY arguments")))
		   ;; If there are &KEY args, then we need to leave
		   ;; the defaulted and supplied-p values where the
		   ;; more args currently are. There might be more or
		   ;; fewer. And also, we need to flatten the parsed
		   ;; args with the defaults before we scan the
		   ;; keywords. So we copy all the &MORE args to a
		   ;; temporary area at the end of the stack.
		   (let* ((num-more-args
			   (hairy-byte-function-num-more-args xep))
			  (new-sp (+ more-args-start num-more-args))
			  (temp (max sp new-sp))
			  (temp-sp (+ temp more-args-supplied))
			  (keywords (hairy-byte-function-keywords xep)))
		     (declare (type index temp)
			      (type stack-pointer new-sp temp-sp))
		     (allocate-eval-stack (- temp-sp sp))
		     (eval-stack-copy temp more-args-start more-args-supplied)
		     (when restp
		       (setf (eval-stack-ref more-args-start) rest)
		       (incf more-args-start))
		     (let ((index more-args-start))
		       (dolist (keyword keywords)
			 (setf (eval-stack-ref index) (cadr keyword))
			 (incf index)
			 (when (caddr keyword)
			   (setf (eval-stack-ref index) nil)
			   (incf index))))
		     (let ((index temp-sp)
			   (allow (eq (hairy-byte-function-keywords-p xep)
				      :allow-others))
			   (bogus-key nil)
			   (bogus-key-p nil))
		       (declare (type fixnum index))
		       (loop
			 (decf index 2)
			 (when (< index temp)
			   (return))
			 (let ((key (eval-stack-ref index))
			       (value (eval-stack-ref (1+ index))))
			   (if (eq key :allow-other-keys)
			       (setf allow value)
			       (let ((target more-args-start))
				 (declare (type stack-pointer target))
				 (dolist (keyword keywords
						  (setf bogus-key key
							bogus-key-p t))
				   (cond ((eq (car keyword) key)
					  (setf (eval-stack-ref target) value)
					  (when (caddr keyword)
					    (setf (eval-stack-ref (1+ target))
						  t))
					  (return))
					 ((caddr keyword)
					  (incf target 2))
					 (t
					  (incf target))))))))
		       (when (and bogus-key-p (not allow))
			 (with-debugger-info (old-component ret-pc old-fp)
			   (error "unknown keyword: ~S" bogus-key))))
		     (setf *eval-stack-top* new-sp)))))
	       (hairy-byte-function-more-args-entry-point xep))))))))
    (declare (type pc entry-point))
    (invoke-local-entry-point (byte-function-component xep) entry-point
			      old-component ret-pc old-sp old-fp
			      closure-vars)))

(defun do-return (fp num-results)
  (declare (type stack-pointer fp) (type index num-results))
  (let ((old-component (eval-stack-ref (- fp 4))))
    (typecase old-component
      (code-component
       ;; returning to more byte-interpreted code
       (do-local-return old-component fp num-results))
      (null
       ;; returning to native code
       (let ((old-sp (eval-stack-ref (- fp 2))))
	 (case num-results
	   (0
	    (setf *eval-stack-top* old-sp)
	    (values))
	   (1
	    (let ((result (pop-eval-stack)))
	      (setf *eval-stack-top* old-sp)
	      result))
	   (t
	    (let ((results nil))
	      (dotimes (i num-results)
		(push (pop-eval-stack) results))
	      (setf *eval-stack-top* old-sp)
	      (values-list results))))))
      (t
       ;; ### function end breakpoint?
       (error "Function-end breakpoints are not supported.")))))

(defun do-local-return (old-component fp num-results)
  (declare (type stack-pointer fp) (type index num-results))
  (let ((old-fp (eval-stack-ref (- fp 1)))
	(old-sp (eval-stack-ref (- fp 2)))
	(old-pc (eval-stack-ref (- fp 3))))
    (declare (type (signed-byte 25) old-pc))
    (if (plusp old-pc)
	;; wants single value
	(let ((result (if (zerop num-results)
			  nil
			  (eval-stack-ref (- *eval-stack-top*
					     num-results)))))
	  (setf *eval-stack-top* old-sp)
	  (push-eval-stack result)
	  (byte-interpret old-component old-pc old-fp))
	;; wants multiple values
	(progn
	  (eval-stack-copy old-sp
			   (- *eval-stack-top* num-results)
			   num-results)
	  (setf *eval-stack-top* (+ old-sp num-results))
	  (push-eval-stack num-results)
	  (byte-interpret old-component (- old-pc) old-fp)))))

