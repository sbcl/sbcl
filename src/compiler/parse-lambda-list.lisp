;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;; Break a lambda-list into its component parts. We return eleven
;;; values:
;;;  1. A list of the required args.
;;;  2. A list of the optional arg specs.
;;;  3. True if a rest arg was specified.
;;;  4. The rest arg.
;;;  5. A boolean indicating whether keywords args are present.
;;;  6. A list of the keyword arg specs.
;;;  7. True if &allow-other-keys was specified.
;;;  8. A list of the &aux specifiers.
;;;  9. True if a more arg was specified.
;;; 10. The &more context var
;;; 11. The &more count var
;;;
;;; The top-level lambda-list syntax is checked for validity, but the
;;; arg specifiers are just passed through untouched. If something is
;;; wrong, we use Compiler-Error, aborting compilation to the last
;;; recovery point.
(declaim (ftype (function (list)
			  (values list list boolean t boolean list boolean
				  list boolean t t))
		parse-lambda-list))
(defun parse-lambda-list (list)
  (collect ((required)
	    (optional)
	    (keys)
	    (aux))
    (let ((restp nil)
	  (rest nil)
	  (morep nil)
	  (more-context nil)
	  (more-count nil)
	  (keyp nil)
	  (allowp nil)
	  (state :required))
      (declare (type (member :allow-other-keys :aux
			     :key
			     :more-context :more-count
			     :optional
			     :post-more :post-rest
			     :required :rest)
		     state))
      (dolist (arg list)
	(if (and (symbolp arg)
		 (let ((name (symbol-name arg)))
		   (and (plusp (length name))
			(char= (char name 0) #\&))))
	    (case arg
	      (&optional
	       (unless (eq state :required)
		 (compiler-error "misplaced &OPTIONAL in lambda list: ~S"
				 list))
	       (setq state :optional))
	      (&rest
	       (unless (member state '(:required :optional))
		 (compiler-error "misplaced &REST in lambda list: ~S" list))
	       (setq state :rest))
	      (sb!c:&more
	       (unless (member state '(:required :optional))
		 (compiler-error "misplaced &MORE in lambda list: ~S" list))
	       (setq morep t
		     state :more-context))
	      (&key
	       (unless (member state
			       '(:required :optional :post-rest :post-more))
		 (compiler-error "misplaced &KEY in lambda list: ~S" list))
	       (setq keyp t
		     state :key))
	      (&allow-other-keys
	       (unless (eq state ':key)
		 (compiler-error "misplaced &ALLOW-OTHER-KEYS in ~
                                  lambda list: ~S"
				 list))
	       (setq allowp t
		     state :allow-other-keys))
	      (&aux
	       (when (member state '(:rest :more-context :more-count))
		 (compiler-error "misplaced &AUX in lambda list: ~S" list))
	       (setq state :aux))
	      ;; FIXME: I don't think ANSI says this is an error. (It
	      ;; should certainly be good for a STYLE-WARNING,
	      ;; though.)
	      (t
	       (compiler-error "unknown &KEYWORD in lambda list: ~S" arg)))
	    (case state
	      (:required (required arg))
	      (:optional (optional arg))
	      (:rest
	       (setq restp t
		     rest arg
		     state :post-rest))
	      (:more-context
	       (setq more-context arg
		     state :more-count))
	      (:more-count
	       (setq more-count arg
		     state :post-more))
	      (:key (keys arg))
	      (:aux (aux arg))
	      (t
	       (compiler-error "found garbage in lambda list when expecting ~
				a keyword: ~S"
			       arg)))))

      (values (required) (optional) restp rest keyp (keys) allowp (aux)
	      morep more-context more-count))))
