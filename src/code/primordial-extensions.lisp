;;;; various user-level definitions which need to be done particularly
;;;; early

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!INT")

;;;; target constants which need to appear as early as possible

;;; an internal tag for marking empty slots, which needs to be defined
;;; as early as possible because it appears in macroexpansions for
;;; iteration over hash tables
;;;
;;; CMU CL 18b used :EMPTY for this purpose, which was somewhat nasty
;;; since it's easily accessible to the user, so that e.g.
;;;	(DEFVAR *HT* (MAKE-HASH-TABLE))
;;;	(SETF (GETHASH :EMPTY *HT*) :EMPTY)
;;;	(MAPHASH (LAMBDA (K V) (FORMAT T "~&~S ~S~%" K V)))
;;; gives no output -- oops!
;;;
;;; FIXME: It'd probably be good to use the unbound marker for this.
;;; However, there might be some gotchas involving assumptions by
;;; e.g. AREF that they're not going to return the unbound marker,
;;; and there's also the noted-below problem that the C-level code
;;; contains implicit assumptions about this marker.
;;;
;;; KLUDGE: Note that as of version 0.6.6 there's a dependence in the
;;; gencgc.c code on this value being a symbol. (This is only one of
;;; many nasty dependencies between that code and this, alas.)
;;; -- WHN 2001-02-28
(defconstant +empty-ht-slot+ '%empty-ht-slot%)
;;; KLUDGE: Using a private symbol still leaves us vulnerable to users
;;; getting nonconforming behavior by messing around with
;;; DO-ALL-SYMBOLS. That seems like a fairly obscure problem, so for
;;; now we just don't worry about it. If for some reason it becomes
;;; worrisome and the magic value needs replacement:
;;;   * The replacement value needs to be LOADable with EQL preserved,
;;;     so that macroexpansion for WITH-HASH-TABLE-ITERATOR will work
;;;     when compiled into a file and loaded back into SBCL.
;;;     (Thus, just uninterning %EMPTY-HT-SLOT% doesn't work.)
;;;   * The replacement value needs to be acceptable to the
;;;     low-level gencgc.lisp hash table scavenging code. 
;;;   * The change will break binary compatibility, since comparisons
;;;     against the value used at the time of compilation are wired
;;;     into FASL files.
;;; -- WHN 20000622

;;;; DO-related stuff which needs to be visible on the cross-compilation host

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun do-do-body (varlist endlist decls-and-code bind step name block)
    (let* ((r-inits nil) ; accumulator for reversed list
	   (r-steps nil) ; accumulator for reversed list
	   (label-1 (gensym))
	   (label-2 (gensym)))
      ;; Check for illegal old-style DO.
      (when (or (not (listp varlist)) (atom endlist))
	(error "Ill-formed ~S -- possibly illegal old style DO?" name))
      ;; Parse VARLIST to get R-INITS and R-STEPS.
      (dolist (v varlist)
	(flet (;; (We avoid using CL:PUSH here so that CL:PUSH can be defined
	       ;; in terms of CL:SETF, and CL:SETF can be defined in terms of
	       ;; CL:DO, and CL:DO can be defined in terms of the current
	       ;; function.)
	       (push-on-r-inits (x)
		 (setq r-inits (cons x r-inits)))
	       ;; common error-handling
	       (illegal-varlist ()
		 (error "~S is an illegal form for a ~S varlist." v name)))
	  (cond ((symbolp v) (push-on-r-inits v))
		((listp v)
		 (unless (symbolp (first v))
		   (error "~S step variable is not a symbol: ~S"
			  name
			  (first v)))
		 (let ((lv (length v)))
		   ;; (We avoid using CL:CASE here so that CL:CASE can be
		   ;; defined in terms of CL:SETF, and CL:SETF can be defined
		   ;; in terms of CL:DO, and CL:DO can be defined in terms of
		   ;; the current function.)
		   (cond ((= lv 1)
			  (push-on-r-inits (first v)))
			 ((= lv 2)
			  (push-on-r-inits v))
			 ((= lv 3)
			  (push-on-r-inits (list (first v) (second v)))
			  (setq r-steps (list* (third v) (first v) r-steps)))
			 (t (illegal-varlist)))))
		(t (illegal-varlist)))))
      ;; Construct the new form.
      (multiple-value-bind (code decls) (parse-body decls-and-code nil)
	`(block ,block
	   (,bind ,(nreverse r-inits)
		  ,@decls
		  (tagbody
		   (go ,label-2)
		   ,label-1
		   ,@code
		   (,step ,@(nreverse r-steps))
		   ,label-2
		   (unless ,(first endlist) (go ,label-1))
		   (return-from ,block (progn ,@(rest endlist))))))))))

;;; DO-ANONYMOUS ({(Var [Init] [Step])}*) (Test Exit-Form*) Declaration* Form*
;;;
;;; This is like DO, except it has no implicit NIL block. Each VAR is
;;; initialized in parallel to the value of the specified INIT form.
;;; On subsequent iterations, the VARS are assigned the value of the
;;; STEP form (if any) in parallel. The TEST is evaluated before each
;;; evaluation of the body FORMS. When the TEST is true, the
;;; EXIT-FORMS are evaluated as a PROGN, with the result being the
;;; value of the DO.
(defmacro do-anonymous (varlist endlist &rest body)
  (do-do-body varlist endlist body 'let 'psetq 'do-anonymous (gensym)))

;;;; miscellany

;;; Concatenate together the names of some strings and symbols,
;;; producing a symbol in the current package.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symbolicate (&rest things)
    (values (intern (apply #'concatenate
			   'string
			   (mapcar #'string things))))))

;;; like SYMBOLICATE, but producing keywords
(defun keywordicate (&rest things)
  (let ((*package* *keyword-package*))
    (apply #'symbolicate things)))

;;; Access *PACKAGE* in a way which lets us recover when someone has
;;; done something silly like (SETF *PACKAGE* :CL-USER). (Such an
;;; assignment is undefined behavior, so it's sort of reasonable for
;;; it to cause the system to go totally insane afterwards, but it's a
;;; fairly easy mistake to make, so let's try to recover gracefully
;;; instead.)
(defun sane-package ()
  (let ((maybe-package *package*))
    (cond ((and (packagep maybe-package)
		;; For good measure, we also catch the problem of
		;; *PACKAGE* being bound to a deleted package.
		;; Technically, this is not undefined behavior in itself,
		;; but it will immediately lead to undefined to behavior,
		;; since almost any operation on a deleted package is
		;; undefined.
		(package-name maybe-package))
	   maybe-package)
	  (t
	   ;; We're in the undefined behavior zone. First, munge the
	   ;; system back into a defined state.
	   (let ((really-package (find-package :cl-user)))
	     (setf *package* really-package)
	     ;; Then complain.
	     (error 'simple-type-error
		    :datum maybe-package
		    :expected-type 'package
		    :format-control
		    "~@<~S can't be a ~S: ~2I~_~S has been reset to ~S.~:>"
		    :format-arguments (list '*package* (type-of maybe-package)
					    '*package* really-package)))))))

;;; Give names to elements of a numeric sequence.
(defmacro defenum ((&key (prefix "") (suffix "") (start 0) (step 1))
		   &rest identifiers)
  (let ((results nil)
	(index 0)
 	(start (eval start))
	(step (eval step)))
    (dolist (id identifiers)
      (when id
	(multiple-value-bind (root docs)
	    (if (consp id)
		(values (car id) (cdr id))
		(values id nil))
	  (push `(defconstant ,(symbolicate prefix root suffix)
		   ,(+ start (* step index))
		   ,@docs)
		results)))
      (incf index))
    `(progn
       ,@(nreverse results))))

;;; generalization of DEFCONSTANT to values which are the same not
;;; under EQL but under e.g. EQUAL or EQUALP
;;;
;;; DEFCONSTANT-EQX is to be used instead of DEFCONSTANT for values
;;; which are appropriately compared using the function given by the
;;; EQX argument instead of EQL.
;;;
;;; Note: Be careful when using this macro, since it's easy to
;;; unintentionally pessimize your code. A good time to use this macro
;;; is when the values defined will be fed into optimization
;;; transforms and never actually appear in the generated code; this
;;; is especially common when defining BYTE expressions. Unintentional
;;; pessimization can result when the values defined by this macro are
;;; actually used in generated code: because of the way that the
;;; dump/load system works, you'll typically get one copy of consed
;;; structure for each object file which contains code referring to
;;; the value, plus perhaps one more copy bound to the SYMBOL-VALUE of
;;; the constant. If you don't want that to happen, you should
;;; probably use DEFPARAMETER instead.
(defmacro defconstant-eqx (symbol expr eqx &optional doc)
  (let ((expr-tmp (gensym "EXPR-TMP-")))
    `(progn
       ;; When we're building the cross-compiler, and in most
       ;; situations even when we're running the cross-compiler,
       ;; all we need is a nice portable definition in terms of the
       ;; ANSI Common Lisp operations.
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (let ((,expr-tmp ,expr))
	   (cond ((boundp ',symbol)
		  (unless (and (constantp ',symbol)
			       (funcall ,eqx
					(symbol-value ',symbol)
					,expr-tmp))
		    (error "already bound differently: ~S")))
		 (t
		  (defconstant ,symbol
                    ;; KLUDGE: This is a very ugly hack, to be able to
                    ;; build SBCL with CMU CL (2.4.19), because there
                    ;; seems to be some confusion in CMU CL about
                    ;; ,EXPR-TEMP at EVAL-WHEN time ... -- MNA 2000-02-23
                    #-cmu ,expr-tmp
                    #+cmu ,expr
                    ,@(when doc `(,doc)))))))
       ;; The #+SB-XC :COMPILE-TOPLEVEL situation is special, since we
       ;; want to define the symbol not just in the cross-compilation
       ;; host Lisp (which was handled above) but also in the
       ;; cross-compiler (which we will handle now).
       ;;
       ;; KLUDGE: It would probably be possible to do this fairly
       ;; cleanly, in a way parallel to the code above, if we had
       ;; SB!XC:FOO versions of all the primitives CL:FOO used above
       ;; (e.g. SB!XC:BOUNDP, SB!XC:SYMBOL-VALUE, and
       ;; SB!XC:DEFCONSTANT), and took care to call them. But right
       ;; now we just hack around in the guts of the cross-compiler
       ;; instead. -- WHN 2000-11-03
       #+sb-xc
       (eval-when (:compile-toplevel)
	 (let ((,expr-tmp ,symbol))
	   (unless (and (eql (info :variable :kind ',symbol) :constant)
			(funcall ,eqx
				 (info :variable :constant-value ',symbol)
				 ,expr-tmp))
	     (sb!c::%defconstant ',symbol ,expr-tmp ,doc)))))))
