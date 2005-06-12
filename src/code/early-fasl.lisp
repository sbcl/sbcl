;;;; needed-early, or at least meaningful-early, stuff for FASL files

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!FASL")

;;;; various constants and essentially-constants

;;; a string which appears at the start of a fasl file header
;;;
;;; This value is used to identify fasl files. Even though this is not
;;; declared as a constant (because ANSI Common Lisp has no facility
;;; for declaring values which are constant under EQUAL but not EQL),
;;; obviously you shouldn't mess with it lightly. If you do set a new
;;; value for some reason, keep these things in mind:
;;; * To avoid confusion with the similar but incompatible CMU CL
;;;   fasl file format, the value should not be "FASL FILE", which
;;;   is what CMU CL used for the same purpose.
;;; * Since its presence at the head of a file is used by LOAD to
;;;   decide whether a file is to be fasloaded or just loaded
;;;   ordinarily (as source), the value should be something which
;;;   can't legally appear at the head of a Lisp source file.
;;; * The value should not contain any line-terminating characters,
;;;   because they're hard to express portably and because the LOAD
;;;   code might reasonably use READ-LINE to get the value to compare
;;;   against.
(defparameter *fasl-header-string-start-string* "# FASL")

(macrolet ((define-fasl-format-features ()
             (let (;; master value for *F-P-A-F-F*
		   (fpaff '(:sb-thread :sb-package-locks :sb-unicode)))
	       `(progn
		  ;; a list of *(SHEBANG-)FEATURES* flags which affect
		  ;; binary compatibility, i.e. which must be the same
		  ;; between the SBCL which compiles the code and the
		  ;; SBCL which executes the code
		  ;;
		  ;; This is a property of SBCL executables in the
		  ;; abstract, not of this particular SBCL executable, 
		  ;; so any flag in this list may or may not be present
		  ;; in the *FEATURES* list of this particular build.
		  (defparameter *features-potentially-affecting-fasl-format*
		    ',fpaff)
		  ;; a string representing flags of *F-P-A-F-F* which
		  ;; are in this particular build
		  ;;
		  ;; (A list is the natural logical representation for
		  ;; this, but we represent it as a string because
		  ;; that's physically convenient for writing to and
		  ;; reading from fasl files, and because we don't
		  ;; need to do anything sophisticated with its
		  ;; logical structure, just test it for equality.)
		  (defparameter *features-affecting-fasl-format*
		    ,(let ((*print-pretty* nil))
		       (prin1-to-string
			(sort
			 (copy-seq
			  (intersection sb-cold:*shebang-features* fpaff))
			 #'string<
			 :key #'symbol-name))))))))
  (define-fasl-format-features))
	   
;;; the code for a character which terminates a fasl file header
(def!constant +fasl-header-string-stop-char-code+ 255)

;;; This value should be incremented when the system changes in such a
;;; way that it will no longer work reliably with old fasl files. In
;;; practice, I (WHN) have often forgotten to increment it for CVS
;;; versions which break binary compatibility. But it certainly should
;;; be incremented for release versions which break binary
;;; compatibility.
(def!constant +fasl-file-version+ 57)
;;; (record of versions before 2003 deleted in 2003-04-26/0.pre8.107 or so)
;;; 38: (2003-01-05) changed names of internal SORT machinery
;;; 39: (2003-02-20) in 0.7.12.1 a slot was added to
;;;     DEFSTRUCT-SLOT-DESCRIPTION
;;; 40: (2003-03-11) changed value of (SXHASH NIL)
;;; 41: (2003-04-26) enforced binary incompatibility between +SB-THREAD
;;;     and -SB-THREAD builds
;;; 42: (2003-05-22) %NAME slot changed to NAME in
;;;     DEFSTRUCT-SLOT-DESCRIPTION
;;; 43: (2003-07-18) Something could easily have changed incompatibly in
;;;     recent maintenance, e.g. from (VECTOR NIL)-as-string support.
;;;     (And experimental results suggest that compatibility was broken
;;;     between about 0.8.1.29 and 0.8.1.39.)
;;; 44: (2003-08-25) various changes leading up to 0.8.3
;;;     <dan`b> what happened this month to stalate the fasls?
;;;     <Krystof_> I think I renumbered everything again
;;;     <Krystof_> simple-array-unsigned-byte-7, probably
;;;     <Krystof_> (thanks to pfdietz)
;;; 45: (2003-10-02) I (WHN) incremented the version for the 0.8.4 
;;;     release because I couldn't immediately convince myself that
;;;     .fasl files could never possibly ever refer to the SB-C
;;;     CONTINUATION-related data types which were changed 
;;;     incompatibly in 0.8.3.62.
;;; 46: (2003-11-11) Tim Daly, Jr. (and Christophe Rhodes) reported
;;;     .fasl incompatibility on sbcl-devel 2003-11-09.
;;; 47: (2003-11-30) Static variables were rearranged in 0.8.6.11.
;;; 48: (2004-03-01) Renumbered all the widetags to allow for more
;;;     microefficiency in sbcl-0.8.8.10
;;; 49: (2004-05-04) Changed implementation of DEFFOO macros and the
;;;     functions they expand to.
;;; 50: (2004-05-20) Changed %COMPILER-DEFUN signature again.
;;; 51: (2004-07-24) Package locks (SBCL 0.8.12.7) changed signature of
;;;     %DEFPACKAGE.
;;; 52: (2004-11-02) Merge of SB-UNICODE.
;;; 53: (2005-02-22) Something introduced in SBCL 0.8.19.26 (give or take
;;;     a couple of patches) invalidated some FFI-related fasls. Probably
;;;     caused by "lazy alien resolution improvements".
;;; 54: (2005-03-22) At least "0.8.20.6: Make FILE-STREAM and STRING-STREAM
;;;     potential mixins in CLOS" and "0.8.20.21: Add immediate single-floats
;;;     on x86-64."
;;; 55: (2005-04-06) EXTERN-ALIEN-NAME logic moved from fixups to
;;;     FIND-FOREIGN-SYMBOL-IN-TABLE &co.
;;; 56: (2005-05-22) Something between 0.9.0.1 and 0.9.0.14. My money is
;;;     on 0.9.0.6 (MORE CASE CONSISTENCY).
;;; 57: (2005-06-12) Raw slot rearrangement in 0.9.1.38

;;; the conventional file extension for our fasl files
(declaim (type simple-string *fasl-file-type*))
(defvar *fasl-file-type* "fasl")

;;;; information about below-Lisp-level linkage

;;; Note:
;;;   Assembler routines are named by full Lisp symbols: they
;;;     have packages and that sort of native Lisp stuff associated
;;;     with them. We can compare them with EQ.
(declaim (type hash-table *assembler-routines*))
(defvar *assembler-routines* (make-hash-table :test 'eq))


;;;; the FOP database

(declaim (simple-vector *fop-names* *fop-funs*))

;;; a vector indexed by a FaslOP that yields the FOP's name
(defvar *fop-names* (make-array 256 :initial-element nil))

;;; a vector indexed by a FaslOP that yields a function of 0 arguments
;;; which will perform the operation
(defvar *fop-funs*
  (make-array 256
	      :initial-element (lambda ()
				 (error "corrupt fasl file: losing FOP"))))

;;;; variables

(defvar *load-depth* 0
  #!+sb-doc
  "the current number of recursive LOADs")
(declaim (type index *load-depth*))

;;; the FASL file we're reading from
(defvar *fasl-input-stream*)
(declaim (type ansi-stream *fasl-input-stream*))

(defvar *load-code-verbose* nil)
