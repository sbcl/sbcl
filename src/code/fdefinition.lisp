;;;; This file contains functions that hack on the global function
;;;; namespace (primarily concerned with SETF functions here). Also,
;;;; function encapsulation and routines that set and return
;;;; definitions disregarding whether they might be encapsulated.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(sb!int::/show0 "fdefinition.lisp 22")

;;;; fdefinition (fdefn) objects

(defun make-fdefn (name)
  (make-fdefn name))

(defun fdefn-name (fdefn)
  (declare (type fdefn fdefn))
  (fdefn-name fdefn))

(defun fdefn-fun (fdefn)
  (declare (type fdefn fdefn)
	   (values (or function null)))
  (fdefn-fun fdefn))

(defun (setf fdefn-fun) (fun fdefn)
  (declare (type function fun)
	   (type fdefn fdefn)
	   (values function))
  (setf (fdefn-fun fdefn) fun))

(defun fdefn-makunbound (fdefn)
  (declare (type fdefn fdefn))
  (fdefn-makunbound fdefn))

;;; This function is called by !COLD-INIT after the globaldb has been
;;; initialized, but before anything else. We need to install these
;;; fdefn objects into the globaldb before any top level forms run, or
;;; we will end up with two different fdefn objects being used for the
;;; same function name. *!INITIAL-FDEFN-OBJECTS* is set up by GENESIS.
(defvar *!initial-fdefn-objects*)
(defun !fdefn-cold-init ()
  (dolist (fdefn *!initial-fdefn-objects*)
    (setf (info :function :definition (fdefn-name fdefn)) fdefn)))

(defun fdefinition-object (name create)
  #!+sb-doc
  "Return the fdefn object for NAME. If it doesn't already exist and CREATE
   is non-NIL, create a new (unbound) one."
  (declare (values (or fdefn null)))
  (unless (legal-function-name-p name)
    (error 'simple-type-error
	   :datum name
	   :expected-type '(or symbol list)
	   :format-control "invalid function name: ~S"
	   :format-arguments (list name)))
  (let ((fdefn (info :function :definition name)))
    (if (and (null fdefn) create)
	(setf (info :function :definition name) (make-fdefn name))
	fdefn)))

;;; FIXME: If the fundamental operation performed when
;;; funcalling a symbol is %COERCE-NAME-TO-FUNCTION, which expands into
;;; FDEFINITION-OBJECT, which does (INFO :FUNCTION :DEFINITION NAME),
;;; that's a horrendously heavyweight way to implement SYMBOL-FUNCTION.
;;; What compelling reason is there for all this hairiness? The only
;;; thing I can think of is that it does give a place to store
;;; SETF functions, but I don't think that's a good enough reason.
;;; It might even be that the FDEFINITION arrangement saves a little
;;; space, if the proportion of function-less symbols is high enough,
;;; but I don't think that's a good enough reason, either.
;;; I'd really like to wipe out FDEFN stuff root and branch, and
;;; just store SETF functions in the symbol property list.
;;;
;;; One problem with just doing the simple thing: What happens when
;;; people call symbols which have no function definitions?
;;;   1. Just hit "undefined function" error -- with no clue as to
;;;      what undefined function it was. (This might actually not be
;;;      too horrible, since the compiler warns you about undefined
;;;      functions and the debugger aims, with incomplete success,
;;;      to show you what form caused an error.)
;;;   2. various solutions involving closures in the function slot,
;;;      all of which have the drawback of extra memory use and extra
;;;      difficulty in detecting when functions are undefined
;;;   2a. Have every single symbol have an undefined function closure
;;;       which points back to it to tell you which undefined symbol it
;;;       was. (4 extra words per undefined symbol)
;;;   2b. Play tricks with FDEFINITION, where the default SYMBOL-FUNCTION
;;;       for any function is an anonymous "undefined function" error
;;;       which doesn't tell you what the problem was, but if FDEFINITION
;;;       is ever called on an undefined symbol, it helpfully changes the
;;;       function definition to point to a closure which knows which
;;;       symbol caused the problem.
;;;   4. Just don't sweat it except when DEBUG>SPEED, where the calling
;;;      convention gets tweaked to test for the undefined-function
;;;      function at call time and bail out with helpful information
;;;      if it's there.
;;;   5. Require that the function calling convention be stereotyped
;;;      along the lines of
;;;		mov %ebx, local_immediate_3	    ; Point to symbol.
;;;		mov %eax, symbol_fun_offset(%eax)   ; Point to function.
;;;		call *function_code_pointer(%eax)   ; Go.
;;;      That way, it's guaranteed that on entry to a function, %EBX points
;;;      back to the symbol which was used to indirect into the function,
;;;      so the undefined function handler can base its complaint on that.
;;;
;;; Another problem with doing the simple thing: people will want to
;;; indirect through something in order to get to SETF functions, in
;;; order to be able to redefine them. What will they indirect
;;; through? This could be done with a hack, making an anonymous
;;; symbol and linking it to the main symbol's SB!KERNEL:SETF-FUNCTION
;;; property. The anonymous symbol could even point back to the symbol
;;; it's the SETF function for, so that if the SETF function was
;;; undefined at the time a call was made, the debugger could say
;;; which function caused the problem. It'd probably be cleaner,
;;; though, to use a new type of primitive object (SYMBOLOID?)
;;; instead. It could probably be like symbol except that its name
;;; could be any object and its value points back to the symbol which
;;; owns it. Then the setf functions for FOO could be on the list (GET
;;; FOO 'SB!KERNEL:SYMBOLOIDS)
;;;
;;; FIXME: Oh, my. Now that I've started thinking about it, I
;;; appreciate more fully how weird and twisted FDEFNs might be. Look
;;; at the calling sequence for full calls. It goes and reads the
;;; address of a function object from its own table of immediate
;;; values, then jumps into that. Consider how weird that is. Not only
;;; is it not doing indirection through a symbol (which I'd already
;;; realized) but it's not doing indirection through

;;; The compiler emits calls to this when someone tries to funcall a symbol.
(defun %coerce-name-to-function (name)
  #!+sb-doc
  "Return the definition for name, including any encapsulations. Settable
   with SETF."
  (let ((fdefn (fdefinition-object name nil)))
    (or (and fdefn (fdefn-fun fdefn))
	(error 'undefined-function :name name))))

(defun %coerce-callable-to-function (callable)
  (if (functionp callable)
      callable
      (%coerce-name-to-function callable)))

;;; This is just another name for %COERCE-NAME-TO-FUNCTION.
#!-sb-fluid (declaim (inline raw-definition))
(defun raw-definition (name)
  ;; We know that we are calling %COERCE-NAME-TO-FUNCTION, so don't remind us.
  (declare (optimize (inhibit-warnings 3)))
  (%coerce-name-to-function name))
(defun (setf raw-definition) (function name)
  (let ((fdefn (fdefinition-object name t)))
    (setf (fdefn-fun fdefn) function)))

;;; FIXME: There seems to be no good reason to have both
;;; %COERCE-NAME-TO-FUNCTION and RAW-DEFINITION names for the same
;;; thing. And despite what the doc string of %COERCE-NAME-TO-FUNCTION
;;; says, it's doesn't look settable. Perhaps we could collapse
;;; %COERCE-TO-FUNCTION, RAW-DEFINITION, and (SETF RAW-DEFINITION)
;;; into RAW-FDEFINITION and (SETF RAW-FDEFINITION), or
;;; OUTER-FDEFINITION and (SETF OUTER-FDEFINITION).

;;;; definition encapsulation

(defstruct (encapsulation-info (:constructor make-encapsulation-info
					     (type definition))
			       (:copier nil))
  ;; This is definition's encapsulation type. The encapsulated
  ;; definition is in the previous encapsulation-info element or
  ;; installed as the global definition of some function name.
  type
  ;; the previous, encapsulated definition. This used to be installed
  ;; as a global definition for some function name, but it was
  ;; replaced by an encapsulation of type TYPE.
  (definition nil :type function))

;;; Replace the definition of NAME with a function that binds NAME's
;;; arguments a variable named argument-list, binds name's definition
;;; to a variable named basic-definition, and evaluates BODY in that
;;; context. TYPE is whatever you would like to associate with this
;;; encapsulation for identification in case you need multiple
;;; encapsulations of the same name.
(defun encapsulate (name type body)
  (let ((fdefn (fdefinition-object name nil)))
    (unless (and fdefn (fdefn-fun fdefn))
      (error 'undefined-function :name name))
    ;; We must bind and close over INFO. Consider the case where we
    ;; encapsulate (the second) an encapsulated (the first)
    ;; definition, and later someone unencapsulates the encapsulated
    ;; (first) definition. We don't want our encapsulation (second) to
    ;; bind basic-definition to the encapsulated (first) definition
    ;; when it no longer exists. When unencapsulating, we make sure to
    ;; clobber the appropriate INFO structure to allow
    ;; basic-definition to be bound to the next definition instead of
    ;; an encapsulation that no longer exists.
    (let ((info (make-encapsulation-info type (fdefn-fun fdefn))))
      (setf (fdefn-fun fdefn)
	    (lambda (&rest argument-list)
	      (declare (special argument-list))
	      (let ((basic-definition (encapsulation-info-definition info)))
		(declare (special basic-definition))
		(eval body)))))))

;;; This is like FIND-IF, except that we do it on a compiled closure's
;;; environment.
(defun find-if-in-closure (test fun)
  (dotimes (index (1- (get-closure-length fun)))
    (let ((elt (%closure-index-ref fun index)))
      (when (funcall test elt)
	(return elt)))))

;;; Find the encapsulation info that has been closed over.
(defun encapsulation-info (fun)
  (and (functionp fun)
       (= (get-type fun) sb!vm:closure-header-widetag)
       (find-if-in-closure #'encapsulation-info-p fun)))

;;; When removing an encapsulation, we must remember that
;;; encapsulating definitions close over a reference to the
;;; encapsulation-info that describes the encapsulating definition.
;;; When you find an info with the target type, the previous info in
;;; the chain has the ensulating definition of that type. We take the
;;; encapsulated definition from the info with the target type, and we
;;; store it in the previous info structure whose encapsulating
;;; definition it describes looks to this previous info structure for
;;; a definition to bind (see ENCAPSULATE). When removing the first
;;; info structure, we do something conceptually equal, but
;;; mechanically it is different.
(defun unencapsulate (name type)
  #!+sb-doc
  "Removes NAME's most recent encapsulation of the specified TYPE."
  (let* ((fdefn (fdefinition-object name nil))
	 (encap-info (encapsulation-info (fdefn-fun fdefn))))
    (declare (type (or encapsulation-info null) encap-info))
    (cond ((not encap-info)
	   ;; It disappeared on us, so don't worry about it.
	   )
	  ((eq (encapsulation-info-type encap-info) type)
	   ;; It's the first one, so change the fdefn object.
	   (setf (fdefn-fun fdefn)
		 (encapsulation-info-definition encap-info)))
	  (t
	   ;; It must be an interior one, so find it.
	   (loop
	     (let ((next-info (encapsulation-info
			       (encapsulation-info-definition encap-info))))
	       (unless next-info
		 ;; Not there, so don't worry about it.
		 (return))
	       (when (eq (encapsulation-info-type next-info) type)
		 ;; This is it, so unlink us.
		 (setf (encapsulation-info-definition encap-info)
		       (encapsulation-info-definition next-info))
		 (return))
	       (setf encap-info next-info))))))
  t)

;;; Does NAME have an encapsulation of the given TYPE?
(defun encapsulated-p (name type)
  (let ((fdefn (fdefinition-object name nil)))
    (do ((encap-info (encapsulation-info (fdefn-fun fdefn))
		     (encapsulation-info
		      (encapsulation-info-definition encap-info))))
	((null encap-info) nil)
      (declare (type (or encapsulation-info null) encap-info))
      (when (eq (encapsulation-info-type encap-info) type)
	(return t)))))

;;;; FDEFINITION

;;; KLUDGE: Er, it looks as though this means that
;;;    (FUNCALL (FDEFINITION 'FOO))
;;; doesn't do the same thing as
;;;    (FUNCALL 'FOO).
;;; That doesn't look like ANSI behavior to me. Look e.g. at the
;;; ANSI definition of TRACE: "Whenever a traced function is invoked,
;;; information about the call, ..". Try this:
;;;   (DEFUN FOO () (PRINT "foo"))
;;;   (TRACE FOO)
;;;   (FUNCALL 'FOO)
;;;   (FUNCALL (FDEFINITION 'FOO))
;;; What to do? ANSI says TRACE "Might change the definitions of the functions
;;; named by function-names." Might it be OK to just get punt all this
;;; encapsulation stuff and go back to a simple but correct implementation of
;;; TRACE? We'd lose the ability to redefine a TRACEd function and keep the
;;; trace in place, but that seems tolerable to me. (Is the wrapper stuff
;;; needed for anything else besides TRACE?)
;;;
;;; The only problem I can see with not having a wrapper: If tracing
;;; EQ, EQL, EQUAL, or EQUALP causes its function address to change,
;;; it will mess up the MAKE-HASH-TABLE logic which uses EQ tests
;;; on those function values. -- WHN 19990906
(defun fdefinition (name)
  #!+sb-doc
  "Return name's global function definition taking care to respect any
   encapsulations and to return the innermost encapsulated definition.
   This is SETF'able."
  (let ((fun (raw-definition name)))
    (loop
      (let ((encap-info (encapsulation-info fun)))
	(if encap-info
	    (setf fun (encapsulation-info-definition encap-info))
	    (return fun))))))

(defvar *setf-fdefinition-hook* nil
  #!+sb-doc
  "This holds functions that (SETF FDEFINITION) invokes before storing the
   new value. These functions take the function name and the new value.")

(defun %set-fdefinition (name new-value)
  #!+sb-doc
  "Set NAME's global function definition."
  (declare (type function new-value) (optimize (safety 1)))
  (let ((fdefn (fdefinition-object name t)))
    ;; *SETF-FDEFINITION-HOOK* won't be bound when initially running top-level
    ;; forms in the kernel core startup.
    (when (boundp '*setf-fdefinition-hook*)
      (dolist (f *setf-fdefinition-hook*)
	(funcall f name new-value)))

    (let ((encap-info (encapsulation-info (fdefn-fun fdefn))))
      (cond (encap-info
	     (loop
	       (let ((more-info
		      (encapsulation-info
		       (encapsulation-info-definition encap-info))))
		 (if more-info
		     (setf encap-info more-info)
		     (return
		      (setf (encapsulation-info-definition encap-info)
			    new-value))))))
	    (t
	     (setf (fdefn-fun fdefn) new-value))))))

;;;; FBOUNDP and FMAKUNBOUND

(defun fboundp (name)
  #!+sb-doc
  "Return true if name has a global function definition."
  (let ((fdefn (fdefinition-object name nil)))
    (and fdefn (fdefn-fun fdefn) t)))

(defun fmakunbound (name)
  #!+sb-doc
  "Make NAME have no global function definition."
  (let ((fdefn (fdefinition-object name nil)))
    (when fdefn
      (fdefn-makunbound fdefn)))
  name)
