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
  '(simple-array (unsigned-byte 8) (#.char-code-limit)))

(sb!xc:defstruct (readtable (:conc-name nil)
			    (:predicate readtablep))
  #!+sb-doc
  "Readtable is a data structure that maps characters into syntax
   types for the Common Lisp expression reader."
  ;; The CHARACTER-ATTRIBUTE-TABLE is a vector of CHAR-CODE-LIMIT
  ;; integers for describing the character type. Conceptually, there
  ;; are 4 distinct "primary" character attributes: WHITESPACE,
  ;; TERMINATING-MACRO, ESCAPE, and CONSTITUENT. Non-terminating
  ;; macros (such as the symbol reader) have the attribute
  ;; CONSTITUENT.
  ;;
  ;; In order to make the READ-TOKEN fast, all this information is
  ;; stored in the character attribute table by having different
  ;; varieties of constituents.
  (character-attribute-table
   (make-array char-code-limit
	       :element-type '(unsigned-byte 8)
	       :initial-element constituent)
   :type attribute-table)
  ;; The CHARACTER-MACRO-TABLE is a vector of CHAR-CODE-LIMIT
  ;; functions. One of these functions called with appropriate
  ;; arguments whenever any non-WHITESPACE character is encountered
  ;; inside READ-PRESERVING-WHITESPACE. These functions are used to
  ;; implement user-defined read-macros, system read-macros, and the
  ;; number-symbol reader.
  (character-macro-table
   (make-array char-code-limit :initial-element #'undefined-macro-char)
   :type (simple-vector #.char-code-limit))
  ;; DISPATCH-TABLES entry, which is an alist from dispatch characters
  ;; to vectors of CHAR-CODE-LIMIT functions, for use in defining
  ;; dispatching macros (like #-macro).
  (dispatch-tables () :type list)
  (readtable-case :upcase :type (member :upcase :downcase :preserve :invert)))
