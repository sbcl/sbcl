;;;; A partially-compiling interpreted EVAL

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-INTERPRETER")

;; +NONE+ is not accidentally acceptable - as would be NIL -
;; to functions accepting a SEQUENCE type.
(defconstant +none+ 0)
(deftype value-vector () `(or (eql ,+none+) simple-vector))

(defstruct (decl-scope (:conc-name)
                       (:constructor make-decl-scope (declarations %policy))
                       (:copier nil))
  (declarations (missing-arg) :type list :read-only t) ; in their original form
  ;; lexical policy. Don't read directly. Use ENV-POLICY instead.
  (%policy nil :type sb-c::policy :read-only t)

  ;; A vector parallel to the bound symbols in a LET or LAMBDA frame.
  (binding-typechecks +none+ :type value-vector)
  ;; A vector of extra checks performed on entry to this scope.
  ;; Each check takes 2 cells to represent: a binding and a ctype.
  ;; The binding is either a frame-ptr or a symbol.
  (extra-typechecks +none+ :type value-vector)

  ;; (BINDING . CTYPE) where BINDING is either a binding cell - a cons -
  ;; or a symbol in the case of free specials.
  ;; Name ambiguities are resolved when the cell is a cons.
  ;; Restrictions are stored even when current policy precludes type checking,
  ;; so that a nested scope in a safer policy works as it should.
  (type-restrictions nil :type list))

;; Some limits of the number of levels of lexical nesting and number
;; of things that can be bound at one level.
;; For 32-bit machines, could replace these with a cons of two fixnums
;; if the limits prove to be inadequate.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +frame-depth-bits+ 8)  ; 2^8 levels of lexical scope nesting
  (defconstant +frame-size-bits+ 21)) ; 2^21 {vars,funs,tags} per scope

;; Environment structure.
;; The interpreter never stack-allocates any ENV, for two reasons:
;; 1. it does not analyze which would environments could be dx
;; 2. heap allocation is better for being tail-recursive
;; But the code-walker using these structures *does* allocate its ENVs,
;; on the stack, so it needs an inlineable constructor.
(declaim (inline make-basic-env))
;; This is 6 words including the header. It could be shrunk to 4 words
;; with the compact-instance patch because the 'symbols' slot is strictly
;; a lexical (static) aspect that can be moved into the 'decl-scope' object.
(defstruct (basic-env ; 6 words incl. header word.
             (:conc-name env-)
             (:include sb-c::abstract-lexenv)
             (:constructor make-basic-env (parent payload symbols contour))
             ;(:predicate nil)
             (:copier nil))
  (parent nil :type (or null basic-env))
  (payload nil :read-only t) ; whatever the particular ENV subtype wants to put here
  ;; #(name1 ... nameN) or (#(name1 ... nameN) . fill-pointer)
  ;; All ENV subtypes except tagbody and block may have symbols,
  ;; either to create free specials or lexical bindings or both.
  ;; FIXME: NIL should be +NONE+ so that we don't have to distinguish it from CONS,
  ;; and then MUTABLE-P would be LISTP instead of CONSP for a faster test.
  (symbols nil :type (or null simple-vector
                               (cons (unsigned-byte #.+frame-size-bits+)
                                     simple-vector)))
  ;; The CONTOUR is the static aspect of the dynamic contour,
  ;; all the "compile-time-ish" stuff such as parsed bindings, canonical policy.
  ;; It's analogous to a LEXENV, but naming it LEXENV would be confusing
  ;; as heck, since ENV-LEXENV could be reasonably construed as the function
  ;; that returns a compiler LEXENV corresponding to this BASIC-ENV.
  (contour nil :type decl-scope :read-only t))
