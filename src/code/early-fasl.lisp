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
(defconstant +fasl-header-string-stop-char-code+ 255)

;;; This value should be incremented when the system changes in such
;;; a way that it will no longer work reliably with old fasl files.
(defconstant +fasl-file-version+ 21)
;;; 2 = sbcl-0.6.4 uses COMPILE-OR-LOAD-DEFGENERIC.
;;; 3 = sbcl-0.6.6 uses private symbol, not :EMPTY, for empty HASH-TABLE slot.
;;; 4 = sbcl-0.6.7 uses HAIRY-DATA-VECTOR-REF and HAIRY-DATA-VECTOR-SET
;;;     when array headers or data element type uncertainty exist, and
;;;     uses DATA-VECTOR-REF and DATA-VECTOR-SET only for VOPs. (Thus,
;;;     full calls to DATA-VECTOR-REF and DATA-VECTOR-SET from older
;;;     fasl files would fail, because there are no DEFUNs for these
;;;     operations any more.)
;;; 5 = sbcl-0.6.8 has rearranged static symbols.
;;; 6 = sbcl-0.6.9, got rid of non-ANSI %DEFCONSTANT/%%DEFCONSTANT stuff
;;;     and deleted a slot from DEBUG-SOURCE structure.
;;; 7 = around sbcl-0.6.9.8, merged SB-CONDITIONS package into SB-KERNEL
;;; 8 = sbcl-0.6.10.4 revived Gray stream support, changing stream layouts.
;;; 9 = deleted obsolete CONS-UNIQUE-TAG bytecode in sbcl-0.6.11.8
;;; (somewhere in here also changes to AND and OR CTYPE layouts) 
;;; 10 = new layout for CONDITION in sbcl-0.6.11.38
;;; 11 = (a) new helper functions for MAKE-LOAD-FORM (HASH-TABLE) in
;;;          sbcl-0.6.12.11
;;;      (b) new address space constants for OpenBSD in 0.6.12.17
;;;          (doesn't need separate version from (a) because the
;;;          OpenBSD port was broken from sometime before 0.6.12.11
;;;          until the address space was changed)
;;; 12 = sbcl-0.6.12.22 added new SB-FASL package
;;; 13 = sbcl-0.6.12.28 removed some elements from *STATIC-SYMBOLS* 
;;; 14 = sbcl-0.6.12.29 removed more elements from *STATIC-SYMBOLS* 
;;; 15 = sbcl-0.6.12.33 changed the layout of STREAM
;;; 16 = sbcl-0.pre7.15 changed the layout of PRETTY-STREAM
;;; 17 = sbcl-0.pre7.38 (merging many changes accumulated in
;;;      the sbcl-0.pre7.37.flaky5.* branch back into the main branch)
;;;      got rid of byte compiler, byte interpreter, and IR1
;;;      interpreter, changed %DEFUN and DEFSTRUCT, changed the
;;;      meaning of FOP-FSET, and changed the layouts of various
;;;      internal compiler structures (e.g. DEFSTRUCT CLAMBDA)
;;; 18 = sbcl-0.pre7.39 swapped FUNCTION-POINTER-TYPE and
;;;      INSTANCE-POINTER-LOWTAG low-level type codes to help with
;;;      the PPC port
;;; (In 0.pre7.48, the low-level object layout of SYMBOL on the
;;; non-X86 ports changed. I forgot to bump the fasl version number:
;;; I only have an X86.. -- WHN)
;;; 19 = sbcl-0.pre7.50 deleted byte-compiler-related low-level type codes
;;; 20 = sbcl-0.pre7.51 modified names and layouts of
;;;      physical-environment-related structures in the compiler
;;; 21 = sbcl-0.pre7.62 finally incremented the version after several
;;;      incompatible changes in earlier versions: many many symbols
;;;      renamed, changes in globaldb representation of constants
;;;      and inline functions, and change in the value of
;;;      INTERNAL-TIME-UNITS-PER-SECOND

;;; the conventional file extension for our fasl files
(declaim (type simple-string *fasl-file-type*))
(defvar *fasl-file-type* "fasl")

;;; information about below-Lisp-level linkage
;;;
;;; Note:
;;;   Assembler routines are named by full Lisp symbols: they
;;;     have packages and that sort of native Lisp stuff associated
;;;     with them. We can compare them with EQ.
;;;   Foreign symbols are named by Lisp strings: the Lisp package
;;;     system doesn't extend out to symbols in languages like C.
;;;     We want to use EQUAL to compare them.
;;;   *STATIC-FOREIGN-SYMBOLS* are static as opposed to "dynamic" (not
;;;     as opposed to "extern"). The table contains symbols known at 
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

