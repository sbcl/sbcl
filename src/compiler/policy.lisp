;;;; compiler optimization policy stuff

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;; a value for an optimization declaration
(def!type policy-quality () '(rational 0 3))

;;; CMU CL used a special STRUCTURE-OBJECT type POLICY to represent
;;; the state of optimization policy at any point in compilation. This
;;; was a natural choice, but in SBCL it became a little troublesome
;;; because of stupid technicalities involving the cold initialization
;;; of structure LAYOUTs and structure accessors, so now we just use
;;; alists instead.
(def!type policy () 'list)

;;; names of recognized optimization policy qualities
(defvar *policy-qualities*) ; (initialized at cold init)

;;; Is X the name of an optimization quality?
(defun policy-quality-name-p (x)
  (memq x *policy-qualities*))

;;; *POLICY* holds the current global compiler policy information, as
;;; an alist mapping from optimization quality name to quality value.
;;; Inside the scope of declarations, new entries are added at the
;;; head of the alist.
(declaim (type policy *policy*))
(defvar *policy*)	   ; initialized in cold init

;;; This is to be called early in cold init to set things up, and may
;;; also be called again later in cold init in order to reset default
;;; optimization policy back to default values after toplevel PROCLAIM
;;; OPTIMIZE forms have messed with it.
(defun !policy-cold-init-or-resanify ()
  (setf *policy-qualities*
	'(;; ANSI standard qualities
	  compilation-speed
	  debug
	  safety
	  space
	  speed
	  ;; SBCL extensions
	  ;;
	  ;; FIXME: INHIBIT-WARNINGS is a misleading name for this.
	  ;; Perhaps BREVITY would be better. But the ideal name would
	  ;; have connotations of suppressing not warnings but only
	  ;; optimization-related notes, which is already mostly the
	  ;; behavior, and should probably become the exact behavior.
	  ;; Perhaps INHIBIT-NOTES?
	  inhibit-warnings))
  (setf *policy*
	(mapcar (lambda (name)
		  ;; CMU CL didn't use 1 as the default for everything,
		  ;; but since ANSI says 1 is the ordinary value, we do.
		  (cons name 1))
		*policy-qualities*)))
;;; On the cross-compilation host, we initialize immediately (not
;;; waiting for "cold init", since cold init doesn't exist on
;;; cross-compilation host).
#+sb-xc-host (!policy-cold-init-or-resanify)

;;; Look up a named optimization quality in POLICY. This is only
;;; called by compiler code for known-valid QUALITY-NAMEs, e.g. SPEED;
;;; it's an error if it's called for a quality which isn't defined.
;;;
;;; FIXME: After this is debugged, it should get a DEFKNOWN.
#+nil (declaim (ftype (function (policy symbol) policy-quality)))
(defun policy-quality (policy quality-name)
  (let ((acons (assoc quality-name policy)))
    (unless acons
      (error "Argh! no such optimization quality ~S in~%  ~S"
	     quality-name policy))
    (let ((result (cdr acons)))
      (unless (typep result '(rational 0 3))
	(error "Argh! bogus optimization quality ~S" acons))
      result)))

;;; Return a list of symbols naming the optimization qualities which
;;; appear in EXPR.
(defun policy-qualities-used-by (expr)
  (let ((result nil))
    (labels ((recurse (x)
	       (if (listp x)
		   (map nil #'recurse x)
		   (when (policy-quality-name-p x)
		     (pushnew x result)))))
      (recurse expr)
      result)))

;;; syntactic sugar for querying optimization policy qualities
;;;
;;; Evaluate EXPR in terms of the current optimization policy for
;;; NODE, or if NODE is NIL, in terms of the current policy as defined
;;; by *POLICY*. (Using NODE=NIL is only well-defined during
;;; IR1 conversion.)
;;;
;;; EXPR is a form which accesses the policy values by referring to
;;; them by name, e.g. (> SPEED SPACE).
(defmacro policy (node expr)
  (let* ((n-policy (gensym))
	 (used-qualities (policy-qualities-used-by expr))
	 (binds (mapcar (lambda (name)
			  `(,name (policy-quality ,n-policy ',name)))
			used-qualities)))
    `(let* ((,n-policy (lexenv-policy ,(if node
					   `(node-lexenv ,node)
					   '*lexenv*)))
	    ,@binds)
       ,expr)))
