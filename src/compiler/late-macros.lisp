;;;; macros which use GET-SETF-EXPANSION in their macroexpander code,
;;;; and hence need special treatment. Currently (19990806) this
;;;; special treatment involves bare calls to SB!XC:DEFMACRO, and so
;;;; this code can't appear in the build sequence until after
;;;; SB!XC:DEFMACRO has been defined, and so this stuff is separated
;;;; out of the main compiler/macros.lisp file (which has to appear
;;;; earlier)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

#+sb-xc-host
(sb!xc:defmacro def-boolean-attribute (name &rest attribute-names)
  #!+sb-doc
  "Def-Boolean-Attribute Name Attribute-Name*
  Define a new class of boolean attributes, with the attributes having the
  specified Attribute-Names. Name is the name of the class, which is used to
  generate some macros to manipulate sets of the attributes:

    NAME-attributep attributes attribute-name*
      Return true if one of the named attributes is present, false otherwise.
      When set with SETF, updates the place Attributes setting or clearing the
      specified attributes.

    NAME-attributes attribute-name*
      Return a set of the named attributes."

  (let ((const-name (symbolicate name "-ATTRIBUTE-TRANSLATIONS"))
	(test-name (symbolicate name "-ATTRIBUTEP")))
    (collect ((alist))
      (do ((mask 1 (ash mask 1))
	   (names attribute-names (cdr names)))
	  ((null names))
	(alist (cons (car names) mask)))

      `(progn
	 (eval-when (:compile-toplevel :load-toplevel :execute)
	   (defconstant ,const-name ',(alist)))

	 (defmacro ,test-name (attributes &rest attribute-names)
	   "Automagically generated boolean attribute test function. See
	    Def-Boolean-Attribute."
	   `(logtest ,(compute-attribute-mask attribute-names ,const-name)
		     (the attributes ,attributes)))

	 (define-setf-expander ,test-name (place &rest attributes
						 &environment env)
	   "Automagically generated boolean attribute setter. See
	    Def-Boolean-Attribute."
	   (boolean-attribute-setter--target place
					     attributes
					     env
					     (compute-attribute-mask
					      attributes
					      ,const-name
					      )
					     ',test-name))

	 (defmacro ,(symbolicate name "-ATTRIBUTES") (&rest attribute-names)
	   "Automagically generated boolean attribute creation function. See
	    Def-Boolean-Attribute."
	   (compute-attribute-mask attribute-names ,const-name))))))

;;; a helper function for the cross-compilation target Lisp code which
;;; DEF-BOOLEAN-ATTRIBUTE expands into
;;;
;;; KLUDGE: Eventually I'd like to rewrite the mainstream DEF-BOOLEAN-ATTRIBUTE
;;; to use code like this, to factor out some shared functionality for clarity
;;; and for economy. But the motivation for splitting out this code here is
;;; much weirder. In the current version of the code, the cross-compiler calls
;;; UNCROSS on each top-level form before processing it. Ordinarily, UNCROSS
;;; isn't called on macro expansions, but since DEF-BOOLEAN-ATTRIBUTE expands
;;; into a PROGN, the cross-compiler does end up calling UNCROSS on (the
;;; components of) its macroexpansion, since they're separate top-level forms.
;;; In the classic CMU CL macroexpansion, the call to GET-SETF-EXPANSION is in
;;; the macroexpansion, and even when I translate it to
;;; SB!XC:GET-SETF-MACROEXPANSION so that it will work on target code, my
;;; damned, damned UNCROSS kludge unconverts it before processing it. Moving
;;; this shared logic (which includes the troublesome
;;; SB!XC:GET-SETF-EXPANSION code) out of the macroexpansion and into this
;;; helper function works around this problem. -- WHN 19990812
(defun boolean-attribute-setter--target (place attributes env mask test-name)
  (multiple-value-bind (temps values stores set get)
      (sb!xc:get-setf-expansion place env)
    (when (cdr stores)
      (error "multiple store variables for ~S" place))
    (let ((newval (gensym))
	  (n-place (gensym)))
      (values `(,@temps ,n-place)
	      `(,@values ,get)
	      `(,newval)
	      `(let ((,(first stores)
		      (if ,newval
			(logior ,n-place ,mask)
			(logand ,n-place ,(lognot mask)))))
		 ,set
		 ,newval)
	      `(,test-name ,n-place ,@attributes)))))

#+sb-xc-host
(sb!xc:defmacro deletef-in (next place item &environment env)
  (multiple-value-bind (temps vals stores store access)
      (sb!xc:get-setf-expansion place env)
    (when (cdr stores)
      (error "multiple store variables for ~S" place))
    (let ((n-item (gensym))
	  (n-place (gensym))
	  (n-current (gensym))
	  (n-prev (gensym)))
      `(let* (,@(mapcar #'list temps vals)
	      (,n-place ,access)
	      (,n-item ,item))
	 (if (eq ,n-place ,n-item)
	     (let ((,(first stores) (,next ,n-place)))
	       ,store)
	     (do ((,n-prev ,n-place ,n-current)
		  (,n-current (,next ,n-place)
			      (,next ,n-current)))
		 ((eq ,n-current ,n-item)
		  (setf (,next ,n-prev)
			(,next ,n-current)))))
	 (values)))))

#+sb-xc-host
(sb!xc:defmacro push-in (next item place &environment env)
  #!+sb-doc
  "Push Item onto a list linked by the accessor function Next that is stored in
  Place."
  (multiple-value-bind (temps vals stores store access)
      (sb!xc:get-setf-expansion place env)
    (when (cdr stores)
      (error "multiple store variables for ~S" place))
    `(let (,@(mapcar #'list temps vals)
	   (,(first stores) ,item))
       (setf (,next ,(first stores)) ,access)
       ,store
       (values))))
