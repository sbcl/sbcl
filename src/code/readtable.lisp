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
;;; But note that our constants conflate two distinct aspects of character syntax
;;; into one enumerated value which we term "attributes". The aspects are:
;;; (1) "syntax type" of which there are exactly 6 specified:
;;;     constituent, invalid, macro, single-escape, multi-escape, whitespace
;;; (2) "constituent traits"
;;;     a constituent may have one or more traits, but the traits pertain to the
;;;     char _only_ _if_ its syntax type in the readtable is "constituent".
;;;     e.g. #\. has 3 traits: {alphabetic,consing dot,decimal} as a constituent
;;; Refer to the huge block comment at the bottom of this file for specifics.
;;;
;;; When trying to understand our defconstants, bear in mind that we have rearranged
;;; things so that each char code in a readtable has exactly 1 enumerated value for
;;; its attributes, blending the notions of syntax type and traits together.
;;; i.e. you don't keep a set of traits as as LOGIOR of bits, and separately a
;;; syntax type. (I do wonder if the reader's FSM could have fewer states if it
;;; used a set-of-traits representation)
;;;
;;; CAUTION: the numerical order of these constants is coupled with
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

;;; *** No character is directly given this trait. It will be returned by one of
;;;     the {CHAR-CLASS, CHAR-CLASS2, CHAR-CLASS3} macros for any character which
;;;     acts to terminate/delimit the token currently being scanned.
;;;     It is exactly the union of "terminating-macro" and "whitespace"
(defconstant +char-attr-delimiter+ 14)

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
  ;; implement system- and user-defined read-macros.
  (base-char-macro-array
   (make-array base-char-code-limit :initial-element 0)
   :type (simple-vector #.base-char-code-limit)
   :read-only t)
  ;; Characters above the BASE-CHAR range
  ;; TODO: this should probably be changed back to an alist, or else we need to use
  ;; a hash-table that is safe for readers if there is any write in progress.
  ;; Or cons a new hash-table whenever we need to insert a new <k,v> pair.
  ;; (":synchronized t" table would be waaay too much overhead per character)
  ;; Same goes for the secondary dispatch table of dispatching charmacros.
  (extended-char-table *empty-extended-char-table* :type hash-table)
  ;; Function to call which starts construction of an object given the initial
  ;; non-whitespace character. #\? in the name implies optionally an object.
  (%readtable-read #'read-object?
                   :type (sfunction (readtable stream character) (values bit t)))
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

;;; *** The hypertext rendition of CLHS has, I believe, a missing link making Figure 2-8
;;; *** slightly tricky to find. Specifically, in 2.1.4 "Character Syntax Types" it offers
;;; *** to take you to 2.1.4.1 and then 2.1.4.3 through 2.1.4.7, but where's 2.1.4.2?
;;; *** Just use the link to 2.1.4.1, and presto! That links to 2.1.4.2 with Figure 2-8.
;;;
;;; Now with Figure 2-8 in mind, a careful study of the tables below reveals a slight
;;; bug in our reader based on the technical definition of "invalid" characters.
;;; CLHS 2.1.4.3 says
;;;   Characters with the constituent trait invalid cannot ever appear in a token except under the
;;;   control of a single escape character. If an invalid character is encountered while an object
;;;   is being read, an error of type reader-error is signaled. If an invalid character is
;;;   preceded by a single escape character, it is treated as an alphabetic[2] constituent instead.
;;; and the glossary reiterates
;;;   invalid n., adj. 1. n. a possible constituent trait of a character which if present signifies
;;;   that the character cannot ever appear in a token except under the control of a single escape
;;;   character. For details, see Section 2.1.4.1 (Constituent Characters). 2. adj. (of a character)
;;;   being a character that has syntax type constituent in the current readtable and that has
;;;   the constituent trait invalid[1]. See Figure 2-8.
;;; (And by the way, the glossary is the easiest way to discover Figure 2-8)
;;;
;;; So which are the constituents that are invalid and _not_ shadowed by having syntactic
;;; type of "whitespace"? Just char-codes 7 and 127 for #\Backspace and #\Rubout.
;;; And the bug: we consider multi-escape and single-escape equivalent because either one
;;; will allow invalid constituent characters into a token; only single-escape should.

#|
2.1.4 Characters Syntax Types
https://www.lispworks.com/documentation/HyperSpec/Body/02_ad.htm

Figure 2-6. Possible Character Syntax Types
-------------------------------------------
constituent  macro character  single escape
invalid      multiple escape  whitespace[2]

Figure 2-7. Character Syntax Types in Standard Syntax
-----------------------------------------------------
character  syntax type                 character  syntax type
Backspace  constituent                 0--9       constituent
Tab        whitespace[2]               :          constituent
Newline    whitespace[2]               ;          terminating macro char
Linefeed   whitespace[2]               <          constituent
Page       whitespace[2]               =          constituent
Return     whitespace[2]               >          constituent
Space      whitespace[2]               ?          constituent*
!          constituent*                @          constituent
"          terminating macro char      A--Z       constituent
#          non-terminating macro char  [          constituent*
$          constituent                 \          single escape
%          constituent                 ]          constituent*
&          constituent                 ^          constituent
'          terminating macro char      _          constituent
(          terminating macro char      `          terminating macro char
)          terminating macro char      a--z       constituent
*          constituent                 {          constituent*
+          constituent                 |          multiple escape
,          terminating macro char      }          constituent*
-          constituent                 ~          constituent
.          constituent                 Rubout     constituent
/          constituent

The characters marked with an asterisk (*) are initially constituents, but they are not used in
any standard Common Lisp notations. These characters are explicitly reserved to the programmer.
~ is not used in Common Lisp, and reserved to implementors. $ and % are alphabetic[2] characters,
but are not used in the names of any standard Common Lisp defined names.

Whitespace[2] characters serve as separators but are otherwise ignored. Constituent and escape
characters are accumulated to make a token, which is then interpreted as a number or symbol.
Macro characters trigger the invocation of functions (possibly user-supplied) that can perform
arbitrary parsing actions. Macro characters are divided into two kinds, terminating and non-terminating,
depending on whether or not they terminate a token.

Figure 2-8. Constituent Traits of Standard Characters and Semi-Standard Characters
----------------------------------------------------------------------------------
https://www.lispworks.com/documentation/HyperSpec/Body/02_adb.htm

constituent  traits          constituent  traits
character                    character
----------

Backspace    invalid         {            alphabetic[2]
Tab          invalid*        }            alphabetic[2]
Newline      invalid*        +            alphabetic[2], plus sign
Linefeed     invalid*        -            alphabetic[2], minus sign
Page         invalid*        .            alphabetic[2], dot, decimal point
Return       invalid*        /            alphabetic[2], ratio marker
Space        invalid*        A, a         alphadigit
!            alphabetic[2]   B, b         alphadigit
"            alphabetic[2]*  C, c         alphadigit
#            alphabetic[2]*  D, d         alphadigit, double-float exponent marker
$            alphabetic[2]   E, e         alphadigit, float exponent marker
%            alphabetic[2]   F, f         alphadigit, single-float exponent marker
&            alphabetic[2]   G, g         alphadigit
'            alphabetic[2]*  H, h         alphadigit
(            alphabetic[2]*  I, i         alphadigit
)            alphabetic[2]*  J, j         alphadigit
*            alphabetic[2]   K, k         alphadigit
,            alphabetic[2]*  L, l         alphadigit, long-float exponent marker
0-9          alphadigit      M, m         alphadigit
:            package marker  N, n         alphadigit
;            alphabetic[2]*  O, o         alphadigit
<            alphabetic[2]   P, p         alphadigit
=            alphabetic[2]   Q, q         alphadigit
>            alphabetic[2]   R, r         alphadigit
?            alphabetic[2]   S, s         alphadigit, short-float exponent marker
@            alphabetic[2]   T, t         alphadigit
[            alphabetic[2]   U, u         alphadigit
\            alphabetic[2]*  V, v         alphadigit
]            alphabetic[2]   W, w         alphadigit
^            alphabetic[2]   X, x         alphadigit
_            alphabetic[2]   Y, y         alphadigit
`            alphabetic[2]*  Z, z         alphadigit
|            alphabetic[2]*  Rubout       invalid
~            alphabetic[2]

The interpretations in this table apply only to characters whose syntax type is constituent.
Entries marked with an asterisk (*) are normally shadowed[2] because the indicated characters are of syntax
type whitespace[2], macro character, single escape, or multiple escape; these constituent traits apply
to them only if their syntax types are changed to constituent.
|#
