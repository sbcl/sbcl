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
                   (fpaff '(:sb-thread :sb-package-locks :sb-unicode :gencgc :ud2-breakpoints)))
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
(defconstant +fasl-header-string-stop-char-code+ 255)

;;; This value should be incremented when the system changes in such a
;;; way that it will no longer work reliably with old fasl files. In
;;; practice, I (WHN) have often forgotten to increment it for CVS
;;; versions which break binary compatibility. But it certainly should
;;; be incremented for release versions which break binary
;;; compatibility.
(defconstant +fasl-file-version+ 78)
;;; (description of versions before 0.9.0.1 deleted in 0.9.17)
;;; 56: (2005-05-22) Something between 0.9.0.1 and 0.9.0.14. My money is
;;;     on 0.9.0.6 (MORE CASE CONSISTENCY).
;;; 57: (2005-06-12) Raw slot rearrangement in 0.9.1.38
;;; 58: (2005-08-16) Multiple incompatible changes between 0.9.3 and 0.9.3.60
;;; 59: (2005-09-18) METAOBJECT implementation, removal of INSTANCE and
;;;     FUNCALLABLE-INSTANCE classes.
;;; 60: (2005-10-24) Bumped for 0.9.6
;;; 61: (2005-11-06) Improved source location recording added extra parameters
;;;     to multiple %DEFMUMBLE functions.
;;; 62: (2005-12-30) Make the count of FASL header counted strings
;;;     a 32-bit value also on 64-bit platforms.
;;; 63: (2006-01-27) Shuffle storage classes around to reduce the error
;;;     trap information size on RISCy platforms.
;;; 64: (2006-03-24) New calling convention for unknown-values on x86 and
;;;     x86-64.  Also (belatedly) PPC/gencgc, including :gencgc on FPAFF.
;;; 65: (2006-04-11) Package locking interface changed.
;;; 66: (2006-05-13) Fopcompiler
;;; 67: (2006-07-25) Reports on #lisp about 0.9.13 fasls being invalid on
;;;     0.9.14.something
;;; 68: (2006-08-14) changed number of arguments of LOAD-DEFMETHOD
;;; 69: (2006-08-17) changed validity of various initargs for methods
;;; 70: (2006-09-13) changes to *PSEUDO-ATOMIC* on x86 and x86-64
;;; 71: (2006-11-19) CLOS calling convention changes
;;; 72: (2006-12-05) Added slot to the primitive function type
;;; 73: (2007-04-13) Changed a hash function
;;; 74: (2007-06-05) UNWIND-TO-FRAME-AND-CALL
;;; 75: (2007-08-06) FD-STREAM layout changes
;;; 76: (2007-10-05) MUTEX layout changes
;;; 77: (2007-11-08) Essentially obsolete fasl-file-version, fasls are now
;;;     considered compatible only when the version numbers of the compiling
;;;     SBCL instance is exactly the same as the one of the loading instance.
;;;     Further fasl-file-version bumps should only be done for real changes
;;;     in the fasl format, not for changes in function/macro signatures or
;;;     lisp data structures.
;;; 78: (2010-04-02) Add FOP-{SMALL-,}NAMED-PACKAGE, remove FOP-NORMAL-LOAD
;;;     and FOP-MAYBE-COLD-LOAD.

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

(declaim (simple-vector **fop-names** **fop-funs**))

;;; a vector indexed by a FaslOP that yields the FOP's name
(defglobal **fop-names** (make-array 256 :initial-element nil))

;;; a vector indexed by a FaslOP that yields a function which performs
;;; the operation. Most functions take 0 arguments - they only manipulate
;;; the fop stack. But if the fop is defined to receive an argument (or two)
;;; then loader's main loop is responsible for supplying it.
(defglobal **fop-funs** (make-array 256 :initial-element 0))

;; Two bitmaps indicate function signature. One tells whether the fop takes
;; operands (other than from the stack). Each consecutive block of 4 opcodes
;; either does or doesn't, so the array is 1/4th the size of the opcode space.
;; The other tells whether the fop wants its result pushed on the stack.
(declaim (type (cons (simple-bit-vector 64) (simple-bit-vector 256))
               **fop-signatures**))
(defglobal **fop-signatures**
    (cons (make-array 64 :element-type 'bit :initial-element 0)
          (make-array 256 :element-type 'bit :initial-element 0)))

;;;; variables

(defvar *load-depth* 0
  #!+sb-doc
  "the current number of recursive LOADs")
(declaim (type index *load-depth*))

(defun make-fop-vector (size)
  (declare (type index size))
  (let ((vector (make-array size)))
    (setf (aref vector 0) 0)
    vector))

;;; a holder for the FASL file we're reading from
(defstruct (fasl-input (:conc-name %fasl-input-)
                       (:constructor make-fasl-input (stream))
                       (:predicate nil)
                       (:copier nil))
  (stream nil :type ansi-stream :read-only t)
  (table (make-fop-vector 1000) :type simple-vector)
  (stack (make-fop-vector 100) :type simple-vector)
  (deprecated-stuff nil :type list)
  ;; Sometimes we want to skip over any FOPs with side-effects (like
  ;; function calls) while executing other FOPs. SKIP-UNTIL will
  ;; either contain the position where the skipping will stop, or
  ;; NIL if we're executing normally.
  (skip-until nil))
(declaim (freeze-type fasl-input))
