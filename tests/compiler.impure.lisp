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

;;; success
(quit :unix-status 104)
