;;;; READTABLEs

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(sb!xc:deftype attribute-table ()
  '(simple-array (unsigned-byte 8) (#.sb!xc:char-code-limit)))

;;; constants for readtable character attributes. These are all as in
;;; the manual.
(def!constant +char-attr-whitespace+ 0)
(def!constant +char-attr-terminating-macro+ 1)
(def!constant +char-attr-escape+ 2)
(def!constant +char-attr-constituent+ 3)
(def!constant +char-attr-constituent-dot+ 4)
(def!constant +char-attr-constituent-expt+ 5)
(def!constant +char-attr-constituent-slash+ 6)
(def!constant +char-attr-constituent-digit+ 7)
(def!constant +char-attr-constituent-sign+ 8)
;; the "9" entry intentionally left blank for some reason -- WHN 19990806
;;
;; appropriated by CSR 2004-03-16
(def!constant +char-attr-constituent-decimal-digit+ 9)
(def!constant +char-attr-multiple-escape+ 10)
(def!constant +char-attr-package-delimiter+ 11)
(def!constant +char-attr-delimiter+ 12) ; (a fake for READ-UNQUALIFIED-TOKEN)

(sb!xc:defstruct (readtable (:conc-name nil)
			    (:predicate readtablep)
			    ;; ANSI requires a CL:COPY-READTABLE to do
			    ;; a deep copy, so the DEFSTRUCT-generated
			    ;; default is not suitable.
			    (:copier nil))
  #!+sb-doc
  "A READTABLE is a data structure that maps characters into syntax
   types for the Common Lisp expression reader."
  ;; The CHARACTER-ATTRIBUTE-TABLE is a vector of CHAR-CODE-LIMIT
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
  (character-attribute-table
   (make-array sb!xc:char-code-limit
	       :element-type '(unsigned-byte 8)
	       :initial-element +char-attr-constituent+)
   :type attribute-table)
  ;; The CHARACTER-MACRO-TABLE is a vector of CHAR-CODE-LIMIT
  ;; functions. One of these functions called with appropriate
  ;; arguments whenever any non-WHITESPACE character is encountered
  ;; inside READ-PRESERVING-WHITESPACE. These functions are used to
  ;; implement user-defined read-macros, system read-macros, and the
  ;; number-symbol reader.
  (character-macro-table
   (make-array sb!xc:char-code-limit :initial-element #'undefined-macro-char)
   :type (simple-vector #.sb!xc:char-code-limit))
  ;; an alist from dispatch characters to vectors of CHAR-CODE-LIMIT
  ;; functions, for use in defining dispatching macros (like #-macro)
  (dispatch-tables () :type list)
  (readtable-case :upcase :type (member :upcase :downcase :preserve :invert)))
