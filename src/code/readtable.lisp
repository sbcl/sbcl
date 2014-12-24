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
  '(simple-array (unsigned-byte 8) (#.base-char-code-limit)))

;;; constants for readtable character attributes. These are all as in
;;; the manual.
;;;
;;; FIXME: wait a minute.  Firstly, I doubt they're in the manual.
;;; Secondly, the numerical order of these constants is coupled with
;;; code in CHAR-CLASS{,2,3} in the reader implementation, so beware
;;; when changing them.
(def!constant +char-attr-whitespace+ 0)
(def!constant +char-attr-terminating-macro+ 1)
(def!constant +char-attr-single-escape+ 2)
(def!constant +char-attr-multiple-escape+ 3)
(def!constant +char-attr-constituent+ 4)
(def!constant +char-attr-constituent-dot+ 5)
(def!constant +char-attr-constituent-expt+ 6)
(def!constant +char-attr-constituent-slash+ 7)
(def!constant +char-attr-constituent-digit+ 8)
(def!constant +char-attr-constituent-sign+ 9)
;;; the following two are not static but depend on *READ-BASE*.
;;; DECIMAL-DIGIT is for characters being digits in base 10 but not in
;;; base *READ-BASE* (which is therefore perforce smaller than 10);
;;; DIGIT-OR-EXPT is for characters being both exponent markers and
;;; digits in base *READ-BASE* (which is therefore perforce larger
;;; than 10).  -- CSR, 2004-03-16
(def!constant +char-attr-constituent-decimal-digit+ 10)
(def!constant +char-attr-constituent-digit-or-expt+ 11)

(def!constant +char-attr-package-delimiter+ 12)
(def!constant +char-attr-invalid+ 13)
;; Meta: there is no such function as READ-UNQUALIFIED-TOKEN. No biggie.
(def!constant +char-attr-delimiter+ 14) ; (a fake for READ-UNQUALIFIED-TOKEN)

(sb!xc:defstruct (readtable (:conc-name nil)
                            (:constructor make-readtable ())
                            (:predicate readtablep)
                            ;; ANSI requires a CL:COPY-READTABLE to do
                            ;; a deep copy, so the DEFSTRUCT-generated
                            ;; default is not suitable.
                            (:copier nil))
  #!+sb-doc
  "A READTABLE is a data structure that maps characters into syntax
types for the Common Lisp expression reader."
  ;; The CHARACTER-ATTRIBUTE-TABLE is a vector of BASE-CHAR-CODE-LIMIT
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
  (character-attribute-array
   (make-array base-char-code-limit
               :element-type '(unsigned-byte 8)
               :initial-element +char-attr-constituent+)
   :type attribute-table
   :read-only t)
  (character-attribute-hash-table (make-hash-table)
   :type hash-table
   :read-only t)
  ;; The CHARACTER-MACRO-TABLE is a vector of BASE-CHAR-CODE-LIMIT
  ;; functions. One of these functions called with appropriate
  ;; arguments whenever any non-WHITESPACE character is encountered
  ;; inside READ-PRESERVING-WHITESPACE. These functions are used to
  ;; implement user-defined read-macros, system read-macros, and the
  ;; number-symbol reader.
  (character-macro-array
   (make-array base-char-code-limit :initial-element nil)
   :type (simple-vector #.base-char-code-limit)
   :read-only t)
  (character-macro-hash-table (make-hash-table) :type hash-table
   :read-only t)
  (%readtable-case :upcase :type (member :upcase :downcase :preserve :invert))
  (%readtable-normalization #!+sb-unicode t #!-sb-unicode nil :type boolean))

(declaim (freeze-type readtable))
