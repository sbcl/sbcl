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
;; with #+compact-instance-header because the 'symbols' slot is strictly
;; a lexical aspect that could be moved into the 'decl-scope' object.
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

;;; Some of these structures could be defined in warm load, but then they
;;; couldn't benefit from the assignment of a single-byte layout ID.
;;; (After self-build is complete, layout-ids are always 4 bytes.)

;; Binding frame specification for LET and LET*
;; This is a prototype stack-frame rather than a runtime stack frame in that
;; one exists per syntactic form, not per dynamic invocation of same.
(defstruct (frame (:include decl-scope)
                  (:copier nil) (:predicate nil)
                  (:constructor make-let-frame
                                (declarations %policy
                                 symbols special-b values sexpr specials)))
  ;; If more symbols exist than values, the remainder are free specials.
  (symbols    nil :read-only t :type simple-vector)
  ;; Bitmask over symbols. 1 in bit N means bind the Nth symbol as special.
  (special-b  nil :read-only t :type integer)
  ;; A let frame can't have zero values. (It would be converted to LOCALLY)
  (values nil :read-only t :type simple-vector)
  (sexpr      nil :read-only t) ; code to execute
  ;; To avoid reconstituting the first PROGV operand from the special-b mask
  ;; and vector of all bound symbols, store the bound specials as follows:
  ;;   for LET    - a list of all bound specials
  ;;   for LET*   - a list of singleton lists of bound specials
  ;;   for LAMBDA - possibly both of the preceding: a list for mandatory args
  ;;                and lists of singleton lists for &optional/&rest/&key.
  (specials   nil :read-only t))

;;; LET and LET* share the same frame structure
(defstruct (let*-frame
             (:include frame) (:predicate nil) (:copier nil)
             (:constructor make-let*-frame
                           (declarations %policy symbols special-b
                            values sexpr specials))))

;; Fancy binding frame specification
(defstruct (lambda-frame (:include let*-frame) (:predicate nil) (:copier nil))
  ;; Unlike for a LET* frame the count of bound values can not be determined
  ;; from the length of the VALUES vector, which contains various extra markers
  ;; dictating how the arguments are to be parsed.
  (n-bound-vars  0   :read-only t :type fixnum)
  ;; Number of mandatory and optional arguments.
  (min-args      0   :read-only t :type fixnum)
  (n-optional    0   :read-only t :type fixnum)
  ;; Packed flags indicating presence of &REST/&KEY/&ALLOW-OTHER-KEYS.
  (keyword-bits  0   :read-only t :type fixnum)
  ;; A BLOCK name in which to wrap the lambda's evaluable forms.
  ;; This behaves exactly the same as using a block-env around the forms,
  ;; however a lambda-env consumes only 8 words plus 2 for the freshly consed
  ;; catch tag; whereas a var-env + block-env would consume 6 + 6 + 2, which
  ;; entails 40% more overhead to enter the most trivial interpreted function.
  (block-name    0   :read-only t :type (or (eql 0) symbol))
  ;; SHARE-BLOCK-P is T if BLOCK-NAME can be created concurrently
  ;; with variable bindings. If NIL when a block-name is present,
  ;; then another environment is allocated to enclose the block,
  ;; which is not as big a win as combining the block-env and var-env,
  ;; but still beneficial as it avoids separately calling the block handler.
  (share-block-p nil :read-only t :type boolean))
