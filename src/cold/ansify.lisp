;;;; patches to work around implementation idiosyncrasies in our
;;;; cross-compilation host

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;;; CLISP issues

#+clisp
(locally

  (in-package "COMMON-LISP")

  ;; no longer needed in CLISP 1999-01-08, hurrah!
  #|
  ;; ANSI specifies that package LISP defines the type BOOLEAN, and the CMU CL
  ;; compiler uses it a lot. This should be trivial to patch in CLISP, except
  ;; that CLISP defines FFI:BOOLEAN, which conflicts. Gads.. Here we try to fix
  ;; it with some package hacking. (Please do not take this as an example of
  ;; good package hacking, I just messed with it until it seemed to work well
  ;; enough to bootstrap CMU CL, because I'm highly unmotivated to make elegant
  ;; fixes for nonstandard behavior. -- WHN)
  (shadow 'ffi:boolean "FFI")
  (deftype cl::boolean () '(member t nil))
  (export 'boolean "LISP")
  |#

  ;; apparently fixed sometime in 2001, hurray!
  #| (error "can't use CLISP -- no MAKE-LOAD-FORM") |#

  ;; CLISP is still unsupported as a cross-compilation host because of
  ;; these known problems:
  (flet ((clisp-ouch (s) (error "can't bootstrap with CLISP: ~A" s)))
    ;; These problems don't seem deep, and could probably be worked
    ;; around.
    #+nil (clisp-ouch "no (DOCUMENTATION X) when X is a PACKAGE")
    #+nil (clisp-ouch "no (FUNCTION (SETF SYMBOL-FUNCTION))"))
  
  (ext:without-package-lock ("SYSTEM")
    (setf system::*inhibit-floating-point-underflow* t)))

;;;; CMU CL issues

;;; CMU CL, at least as of 18b, doesn't support PRINT-OBJECT. In
;;; particular, it refuses to compile :PRINT-OBJECT options to
;;; DEFSTRUCT, so we need to conditionalize such options on the
;;; :NO-ANSI-PRINT-OBJECT feature in order to get the code to compile.
;;; (It also fails to do anything useful with DEFMETHOD PRINT-OBJECT,
;;; but that doesn't matter much, since it doesn't stop the
;;; cross-compiler from working.)
#+cmu
(progn
  (warn "CMU CL doesn't support the :PRINT-OBJECT option to DEFSTRUCT.~%")
  (pushnew :no-ansi-print-object *features*))

;;; KLUDGE: In CMU CL, at least as of 18b, READ-SEQUENCE is somewhat
;;; dain-bramaged. Running
;;;   (defvar *buffer* (make-array (expt 10 6) :element-type 'character))
;;;   (with-open-file (s "/tmp/long-file.tmp")
;;;     (/show (read-sequence *buffer* s :start 0 :end 3000))
;;;     (/show (read-sequence *buffer* s :start 0 :end 15000))
;;;     (/show (read-sequence *buffer* s :start 0 :end 15000)))
;;; on a large test file gives
;;; /(READ-SEQUENCE *BUFFER* S :START 0 :END 3000)=3000
;;; /(READ-SEQUENCE *BUFFER* S :START 0 :END 15000)=1096
;;; /(READ-SEQUENCE *BUFFER* S :START 0 :END 15000)=0
#+cmu
(progn
  (warn "CMU CL has a broken implementation of READ-SEQUENCE.")
  (pushnew :no-ansi-read-sequence *features*))

#+(and cmu alpha)
(unless (ignore-errors (read-from-string "1.0l0"))
  (error "CMUCL on Alpha can't read floats in the format \"1.0l0\".  Patch your core file~%~%"))

#+openmcl 
(unless (ignore-errors (funcall (constantly t) 1 2 3)) 
  (error "please find a binary that understands CONSTANTLY to build from"))


;;;; general non-ANSI-ness

(in-package :sb-cold)

(defmacro munging-cl-package (&body body)
  #-clisp `(progn ,@body)
  #+clisp `(ext:without-package-lock ("CL")
	     ,@body))

;;; Do the exports of COMMON-LISP conform to the standard? If not, try
;;; to make them conform. (Of course, ANSI says that bashing symbols
;;; in the COMMON-LISP package like this is undefined, but then if the
;;; host Common Lisp were ANSI, we wouldn't be doing this, now would
;;; we? "One dirty unportable hack deserves another.":-)
(let ((standard-ht (make-hash-table :test 'equal))
      (host-ht     (make-hash-table :test 'equal))
      (cl	  (find-package "COMMON-LISP")))
  (do-external-symbols (i cl)
    (setf (gethash (symbol-name i) host-ht) t))
  (dolist (i (read-from-file "common-lisp-exports.lisp-expr"))
    (setf (gethash i standard-ht) t))
  (maphash (lambda (key value)
	     (declare (ignore value))
	     (unless (gethash key standard-ht)
	       (warn "removing non-ANSI export from package CL: ~S" key)
	       (munging-cl-package
		(unexport (intern key cl) cl))))
	   host-ht)
  (maphash (lambda (key value)
	     (declare (ignore value))
	     (unless (gethash key host-ht)
	       (warn "adding required-by-ANSI export to package CL: ~S" key)
	       (munging-cl-package 
		(export (intern key cl) cl)))
	     
	     ;; FIXME: My righteous indignation below was misplaced. ANSI sez
	     ;; (in 11.1.2.1, "The COMMON-LISP Package") that it's OK for
	     ;; COMMON-LISP things to have their home packages elsewhere.
	     ;; For now, the hack below works, but it's not good to rely
	     ;; on this nonstandardness. Ergo, I should fix things so that even
	     ;; when the cross-compilation host COMMON-LISP package has
	     ;; symbols with home packages elsewhere, genesis dumps out
	     ;; the correct stuff. (For each symbol dumped, check whether it's
	     ;; exported from COMMON-LISP, and if so, dump it as though its
	     ;; home package is COMMON-LISP regardless of whether it actually
	     ;; is. I think..)
	     ;;
	     ;; X CMU CL, at least the Debian versions ca. 2.4.9 that I'm
	     ;; X using as I write this, plays a sneaky trick on us by
	     ;; X putting DEBUG and FLOATING-POINT-INEXACT in the
	     ;; X EXTENSIONS package, then IMPORTing them into
	     ;; X COMMON-LISP, then reEXPORTing them from COMMON-LISP.
	     ;; X This leaves their home packages bogusly set to
	     ;; X EXTENSIONS, which confuses genesis into thinking that
	     ;; X the CMU CL EXTENSIONS package has to be dumped into the
	     ;; X target SBCL. (perhaps a last-ditch survival strategy
	     ;; X for the CMU CL "nooo! don't bootstrap from scratch!"
	     ;; X meme?) As far as I can see, there's no even slightly
	     ;; X portable way to undo the damage, so we'll play the "one
	     ;; X dirty unportable hack deserves another" game, only even
	     ;; X dirtierly and more unportably than before..
	     #+cmu
	     (let ((symbol (intern key cl)))
	       (unless (eq (symbol-package symbol) cl)
		 (warn "using low-level hack to move ~S from ~S to ~S"
		       symbol
		       (symbol-package symbol)
		       cl)
		 (kernel:%set-symbol-package symbol cl))))
	   standard-ht))
