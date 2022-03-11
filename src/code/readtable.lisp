;;;; READTABLEs

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

(sb-xc:deftype attribute-table ()
  `(simple-array (unsigned-byte 8) (,base-char-code-limit)))

;;; constants for readtable character attributes. These are all as in
;;; the manual.
;;;
;;; FIXME: wait a minute.  Firstly, I doubt they're in the manual.
;;; Secondly, the numerical order of these constants is coupled with
;;; code in CHAR-CLASS{,2,3} in the reader implementation, so beware
;;; when changing them.
(defconstant +char-attr-whitespace+ 0)
(defconstant +char-attr-terminating-macro+ 1)
(defconstant +char-attr-single-escape+ 2)
(defconstant +char-attr-multiple-escape+ 3)
(defconstant +char-attr-constituent+ 4)
(defconstant +char-attr-constituent-dot+ 5)
(defconstant +char-attr-constituent-expt+ 6)
(defconstant +char-attr-constituent-slash+ 7)
(defconstant +char-attr-constituent-digit+ 8)
(defconstant +char-attr-constituent-sign+ 9)
;;; the following two are not static but depend on *READ-BASE*.
;;; DECIMAL-DIGIT is for characters being digits in base 10 but not in
;;; base *READ-BASE* (which is therefore perforce smaller than 10);
;;; DIGIT-OR-EXPT is for characters being both exponent markers and
;;; digits in base *READ-BASE* (which is therefore perforce larger
;;; than 10).  -- CSR, 2004-03-16
(defconstant +char-attr-constituent-decimal-digit+ 10)
(defconstant +char-attr-constituent-digit-or-expt+ 11)

(defconstant +char-attr-package-delimiter+ 12)
(defconstant +char-attr-invalid+ 13)
;; Meta: there is no such function as READ-UNQUALIFIED-TOKEN. No biggie.
(defconstant +char-attr-delimiter+ 14) ; (a fake for READ-UNQUALIFIED-TOKEN)

(define-load-time-global *empty-extended-char-table* (make-hash-table :rehash-size 1 :test #'eq))

(sb-xc:defstruct (readtable (:conc-name nil)
                            (:constructor make-readtable ())
                            (:predicate readtablep)
                            ;; ANSI requires a CL:COPY-READTABLE to do
                            ;; a deep copy, so the DEFSTRUCT-generated
                            ;; default is not suitable.
                            (:copier nil))
  "A READTABLE is a data structure that maps characters into syntax
types for the Common Lisp expression reader."
  ;; The BASE-CHAR-SYNTAX-ARRAY is a vector of BASE-CHAR-CODE-LIMIT
  ;; integers for describing the character type. Conceptually, there
  ;; are 4 distinct "primary" character attributes:
  ;; +CHAR-ATTR-WHITESPACE+, +CHAR-ATTR-TERMINATING-MACRO+,
  ;; +CHAR-ATTR-ESCAPE+, and +CHAR-ATTR-CONSTITUENT+. Non-terminating
  ;; macros (such as the symbol reader) have the attribute
  ;; +CHAR-ATTR-CONSTITUENT+.
  ;;
  ;; In order to make READ-TOKEN fast, all this information is stored
  ;; in the character attribute table by having different varieties of
  ;; constituents.
  (base-char-syntax-array
   (make-array base-char-code-limit
               :element-type '(unsigned-byte 8)
               :initial-element +char-attr-constituent+)
   :type attribute-table
   :read-only t)
  ;; The BASE-CHAR-MACRO-TABLE is a vector of BASE-CHAR-CODE-LIMIT
  ;; functions. One of these functions called with appropriate
  ;; arguments whenever any non-WHITESPACE character is encountered
  ;; inside READ-PRESERVING-WHITESPACE. These functions are used to
  ;; implement user-defined read-macros, system read-macros, and the
  ;; number-symbol reader.
  (base-char-macro-array
   (make-array base-char-code-limit :initial-element nil)
   :type (simple-vector #.base-char-code-limit)
   :read-only t)
  ;; Characters above the BASE-CHAR range
  (extended-char-table *empty-extended-char-table* :type hash-table)
  (%readtable-case :upcase :type (member :upcase :downcase :preserve :invert))
  ;; Element type to use when reading a string literal with no extended-chars.
  ;; The system itself prefers base-string, but otherwise it is a contentious
  ;; issue. We don't (by default) use base-strings, because people often write:
  ;;   (SETF (CHAR (READ-STRING S) 0) #\PILE_OF_POO),
  ;; or more likely, something the effect of which resembles
  ;;   (SETF (CHAR (ADJUST-ARRAY "" 10) 0) #\SMILE)
  ;; which are each dubious constructs, because they assume READ to produce
  ;; strings capable of holding any char. The latter further assumes something
  ;; about compilation, because in that example, considering that there are no
  ;; characters in the literal, it is unclear whether the array should
  ;; be similar-as-constant to an array of base-char or array of character.
  ;; While indeed SBCL prints base-strings readably (if *PRINT-READABLY* is T)
  ;; using #. syntax, the question is what the writer of the code intended
  ;; if (s)he did not know that the string should have been expressly
  ;; specified via #.(MAKE-STRING ... :ELEMENT-TYPE) or somesuch.
  (%readtable-string-preference 'base-char :type (member character base-char))
  ;; With symbols, it's fairly clear that immutability of print names
  ;; renders the distinction between the kinds of string in the symbol-name
  ;; as being less relevant. If you expect (copy-seq (string asymbol))
  ;; to produce a certain type of string, your code is unportable anyway.
  (%readtable-symbol-preference 'base-char :type (member character base-char))
  (%readtable-normalization #+sb-unicode t #-sb-unicode nil :type boolean))

(declaim (freeze-type readtable))
