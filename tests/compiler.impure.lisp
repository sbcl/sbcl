;;;; This file is for compiler tests which have side effects (e.g.
;;;; executing DEFUN) but which don't need any special side-effecting
;;;; environmental stuff (e.g. DECLAIM of particular optimization
;;;; settings). Similar tests which *do* expect special settings may
;;;; be in files compiler-1.impure.lisp, compiler-2.impure.lisp, etc.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;; 
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(cl:in-package :cl-user)

(load "assertoid.lisp")

;;; Old CMU CL code assumed that the names of "keyword" arguments are
;;; necessarily self-evaluating symbols, but ANSI Common Lisp allows
;;; them to be any symbols, not necessarily keywords, and thus not
;;; necessarily self-evaluating. Make sure that this works.
(defun newfangled-cons (&key ((left-thing x)) ((right-thing y)))
  (cons x y))
(assert (equal (cons 1 2) (newfangled-cons 'right-thing 2 'left-thing 1)))

;;; ANSI specifically says that duplicate keys are OK in lambda lists,
;;; with no special exception for macro lambda lists. (As reported by
;;; Pierre Mai on cmucl-imp 2001-03-30, Python didn't think so. The
;;; rest of the thread had some entertainment value, at least for me
;;; (WHN). The unbelievers were besmote and now even CMU CL will
;;; conform to the spec in this regard. Who needs diplomacy when you
;;; have brimstone?:-)
(defmacro ayup-duplicate-keys-are-ok-i-see-the-lite (&key k)
  k)
(assert (equal (ayup-duplicate-keys-are-ok-i-see-the-lite :k 112) 112))
(assert (equal (ayup-duplicate-keys-are-ok-i-see-the-lite :k 'x :k 'y) 'x))

;;; As reported by Alexey Dejneka (sbcl-devel 2002-01-30), in
;;; sbcl-0.7.1 plus his patch (i.e. essentially sbcl-0.7.1.2), the
;;; compiler barfed on this, blowing up in FIND-IN-PHYSENV looking for
;;; the LAMBDA-VAR named NUM. That was fixed in sbcl-0.7.1.3.
(defun parse-num (index)
  (let (num x)
    (flet ((digs ()
             (setq num index))
	   (z ()
	     (let ()
	       (setq x nil))))
      (when (and (digs) (digs)) x))))

;;; Bug 132: The compiler used to fail to compile INTEGER-valued CATCH
;;; tags. This was fixed by Alexey Dejneka in sbcl-0.7.1.14. (They're
;;; still a bad idea because tags are compared with EQ, but now it's a
;;; compiler warning instead of a failure to compile.)
(defun foo ()
  (catch 0 (print 1331)))

;;;; tests not in the problem domain, but of the consistency of the
;;;; compiler machinery itself

(in-package "SB-C")

;;; Hunt for wrong-looking things in fundamental compiler definitions,
;;; and gripe about them.
;;;
;;; FIXME: It should be possible to (1) repair the things that this
;;; code gripes about, and then (2) make the code signal errors
;;; instead of just printing complaints to standard output, in order
;;; to prevent the code from later falling back into disrepair.
(defun grovel-results (function)
  (dolist (template (fun-info-templates (info :function :info function)))
    (when (template-more-results-type template)
      (format t "~&Template ~A has :MORE results, and translates ~A.~%"
	      (template-name template)
	      function)
      (return nil))
    (when (eq (template-result-types template) :conditional)
      ;; dunno.
      (return t))
    (let ((types (template-result-types template))
	  (result-type (fun-type-returns (info :function :type function))))
      (cond
	((values-type-p result-type)
	 (do ((ltypes (append (args-type-required result-type)
			      (args-type-optional result-type))
		      (rest ltypes))
	      (types types (rest types)))
	     ((null ltypes)
	      (unless (null types)
		(format t "~&More types than ltypes in ~A, translating ~A.~%"
			(template-name template)
			function)
		(return nil)))
	   (when (null types)
	     (unless (null ltypes)
	       (format t "~&More ltypes than types in ~A, translating ~A.~%"
		       (template-name template)
		       function)
	       (return nil)))))
	((eq result-type (specifier-type nil))
	 (unless (null types)
	   (format t "~&Template ~A returns values for function ~A with RESULT-TYPE NIL.~%"
		   (template-name template)
		   function)
	   (return nil)))
	((/= (length types) 1)
	 (format t "~&Template ~A isn't returning 1 value for ~A.~%"
		 (template-name template)
		 function)
	 (return nil))
	(t t)))))
(defun identify-suspect-vops (&optional (env (first
					      (last *info-environment*))))
  (do-info (env :class class :type type :name name :value value)
    (when (and (eq class :function) (eq type :type))
      ;; OK, so we have an entry in the INFO database. Now, if ...
      (let* ((info (info :function :info name))
	     (templates (and info (fun-info-templates info))))
	(when templates
	  ;; ... it has translators
	  (grovel-results name))))))
(identify-suspect-vops)

;;; success
(quit :unix-status 104)
