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

;;; the code for a character which terminates a fasl file header
(def!constant +fasl-header-string-stop-char-code+ 255)

;;; This value should be incremented when the system changes in such a
;;; way that it will no longer work reliably with old fasl files. In
;;; practice, I (WHN) fairly often forget to increment it for CVS
;;; versions which break binary compatibility. But it certainly should
;;; be incremented for release versions which break binary
;;; compatibility.
(def!constant +fasl-file-version+ 35)
;;; (record of versions before 0.7.0 deleted in 0.7.1.41)
;;; 23 = sbcl-0.7.0.1 deleted no-longer-used EVAL-STACK stuff,
;;;      causing changes in *STATIC-SYMBOLS*.
;;; 24 = sbcl-0.7.1.19 changed PCL service routines which might be
;;;      called from macroexpanded code
;;; 25 = sbcl-0.7.1.41 (and immediately preceding versions, actually)
;;;      introduced new functions to check for control stack exhaustion
;;; 26 = sbcl-0.7.2.4 or so added :VARIABLE :MACRO-EXPANSION to INFO codes
;;; 27: (2002-04-08) added MIGHT-CONTAIN-OTHER-TYPES? slot to CTYPE
;;; 28: (2002-05-08) new convention for foreign symbols to support
;;;     dynamic loading in OpenBSD
;;; 29: (2002-06-24) removed *!INITIAL-FDEFN-OBJECTS* from static symbols
;;; 30: (2002-07-26) deleted all references to %DETECT-STACK-EXHAUSTION, 
;;;     which was introduced in version 25, since now control stack
;;;     is checked using mmap() page protection
;;; 31: (2002-08-14) changed encoding of PCL internal MAKE-INSTANCE
;;;     function names so they're insensitive to whether the class name
;;;     is currently external to its package
;;; 32: (2002-09-21) changes in implementation of sequence functions,
;;;     causing old utility functions like COERCE-TO-SIMPLE-VECTOR to go away
;;; 33: (2002-10-02) (again) changes in implementation of sequence functions,
;;;     causing old utility functions like COERCE-TO-SIMPLE-VECTOR to go away
;;; 34: (2002-10-05) changed implementation of DEFMACRO, so %%DEFMACRO
;;;      was deleted
;;; 35: (2002-11-27) (incremented version before 0.7.10 release,
;;;     reflecting changes from a week or more ago) changed layout of
;;;     CLOS objects to support SXHASH returning values other than 42
;;;     for STANDARD-OBJECT

;;; the conventional file extension for our fasl files
(declaim (type simple-string *fasl-file-type*))
(defvar *fasl-file-type* "fasl")

;;;; information about below-Lisp-level linkage

;;; Note:
;;;   Assembler routines are named by full Lisp symbols: they
;;;     have packages and that sort of native Lisp stuff associated
;;;     with them. We can compare them with EQ.
;;;   Foreign symbols are named by Lisp STRINGs: the Lisp package
;;;     system doesn't extend out to symbols in languages like C.
;;;     We want to use EQUAL to compare them.
;;;   *STATIC-FOREIGN-SYMBOLS* are static as opposed to "dynamic" (not
;;;     as opposed to C's "extern"). The table contains symbols known at 
;;;     the time that the program was built, but not symbols defined
;;;     in object files which have been loaded dynamically since then.
(declaim (type hash-table *assembler-routines* *static-foreign-symbols*))
(defvar *assembler-routines* (make-hash-table :test 'eq))
(defvar *static-foreign-symbols* (make-hash-table :test 'equal))

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

;;;; other miscellaneous loading-related stuff


;;;; variables

(defvar *load-depth* 0
  #!+sb-doc
  "the current number of recursive LOADs")
(declaim (type index *load-depth*))

;;; the FASL file we're reading from
(defvar *fasl-input-stream*)
(declaim (type ansi-stream *fasl-input-stream*))

(defvar *load-print* nil
  #!+sb-doc
  "the default for the :PRINT argument to LOAD")
(defvar *load-verbose* nil
  ;; Note that CMU CL's default for this was T, and ANSI says it's
  ;; implementation-dependent. We choose NIL on the theory that it's
  ;; a nicer default behavior for Unix programs.
  #!+sb-doc
  "the default for the :VERBOSE argument to LOAD")

(defvar *load-code-verbose* nil)

