;;;; code to manipulate symbols (but not packages, which are handled
;;;; elsewhere)
;;;;
;;;; Many of these definitions are trivial interpreter entries to
;;;; functions open-coded by the compiler.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

(declaim (maybe-inline get3 %put getf remprop %putf get-properties keywordp))

(defun symbol-value (symbol)
  "Return SYMBOL's current bound value."
  (declare (optimize (safety 1)))
  (symbol-value symbol))

(defun boundp (symbol)
  "Return non-NIL if SYMBOL is bound to a value."
  (boundp symbol))

;;; Same as BOUNDP but without a transform. Used for initialization forms
;;; to avoid a local notinline decl on BOUNDP in the expansion of DEFVAR etc.
(defun %boundp (symbol)
  (boundp symbol))

(defun set (symbol new-value)
  "Set SYMBOL's value cell to NEW-VALUE."
  (declare (type symbol symbol))
  (about-to-modify-symbol-value symbol 'set new-value)
  (%set-symbol-value symbol new-value))

(defun %set-symbol-value (symbol new-value)
  (%set-symbol-value symbol new-value))

(defun symbol-global-value (symbol)
  "Return the SYMBOL's current global value. Identical to SYMBOL-VALUE,
in single-threaded builds: in multithreaded builds bound values are
distinct from the global value. Can also be SETF."
  (declare (optimize (safety 1)))
  (symbol-global-value symbol))

(defun set-symbol-global-value (symbol new-value)
  (about-to-modify-symbol-value symbol 'set new-value)
  (%set-symbol-global-value symbol new-value))

(declaim (inline %makunbound))
(defun %makunbound (symbol)
  (%set-symbol-value symbol (make-unbound-marker)))

(defun makunbound (symbol)
  "Make SYMBOL unbound, removing any value it may currently have."
  (with-single-package-locked-error (:symbol symbol "unbinding the symbol ~A")
    ;; :EVENTUALLY is allowed for :always-bound here, as it has no bearing
    (when (eq (info :variable :always-bound symbol) :always-bound)
      (error "Can't make ~A variable unbound: ~S" 'always-bound symbol))
    (about-to-modify-symbol-value symbol 'makunbound)
    (when (eq (info :variable :kind symbol) :constant)
      (clear-info :variable :kind symbol))
    (%makunbound symbol)
    symbol))

;; Compute a symbol's hash. Also used by FIND-SYMBOL which requires that a hash
;; be a pure function of the name and not a semi-opaque property of the symbol.
;; The hash of all symbols named "NIL" must be the same, so not to pessimize
;; FIND-SYMBOL by special-casing the finding of CL:NIL with an extra "or"
;; in the hash-equality test. i.e. We can't recognize that CL:NIL was the
;; object sought (having an exceptional hash) until it has been found.
(defun calc-symbol-name-hash (string length)
  (declare (simple-string string) (index length))
  (cond
    ;; first case is only needed if NIL's hash is unusual, but it's OK either way
    #+64-bit
    ((and (= length 3)
           (locally
            ;; SXHASH-SUBSTRING is unsafe, so this is too. but do we know that
            ;; length is ok, or is it an accident that it can scan too far?
            (declare (optimize (safety 0)))
            (string-dispatch (simple-base-string (simple-array character (*)))
                             string
              (and (char= (schar string 0) #\N)
                   (char= (schar string 1) #\I)
                   (char= (schar string 2) #\L)))))
     #.(symbol-hash 'nil)) ; utilize the host's function
    (t
     ;; Flip the bits so that a symbol hashes differently from its print name,
     ;; and extract the lesser of 32 or N-POSITIVE-FIXNUM-BITS significant bits.
     (ldb (byte 32 0)
          (logxor (%sxhash-simple-substring string 0 length)
                  most-positive-fixnum)))))

;;; Return the function binding of SYMBOL or NIL if not fboundp.
;;; Don't strip encapsulations.
(declaim (inline %symbol-function))
(defun %symbol-function (symbol)
  #+linkage-space (%primitive sb-vm::fdefn-fun symbol)
  #-linkage-space
  (let ((fdefn (sb-vm::%symbol-fdefn symbol)))
    (if (eql fdefn 0) nil (fdefn-fun (truly-the fdefn fdefn)))))
(defun (setf %symbol-function) (newval symbol) (fset symbol newval))

(defun symbol-function (symbol)
  "Return SYMBOL's current function definition. Settable with SETF."
  (truly-the function (or (%symbol-function symbol) ; fast way
                          (%coerce-name-to-fun symbol)))) ; fallback w/restart

;; I think there are two bugs here.
;; Per CLHS "SETF may be used with symbol-function to replace a global
;;           function definition when the symbol's function definition
;;           does not represent a special operator."
;; 1. This should fail:
;;    * (in-package CL) ; circumvent package lock
;;    * (setf (symbol-function 'if) #'cons) => #<FUNCTION CONS>
;; 2. (SETF (SYMBOL-FUNCTION 'I-ONCE-WAS-A-MACRO) #'CONS)
;;    should _probably_ make I-ONCE-WAS-A-MACRO not a macro
(defun (setf symbol-function) (new-value symbol)
  (declare (type symbol symbol) (type function new-value))
  ;; (SYMBOL-FUNCTION symbol) == (FDEFINITION symbol) according to the writeup
  ;; on SYMBOL-FUNCTION. It doesn't say that SETF behaves the same, but let's
  ;; assume it does, and that we can't assign our macro/special guard funs.
  (err-if-unacceptable-function new-value '(setf symbol-function))
  (setq new-value (strip-encapsulation new-value))
  (with-single-package-locked-error
      (:symbol symbol "setting the symbol-function of ~A")
    ;; This code is a little "surprising" in that it is not just a limited
    ;; case of (SETF FDEFINITION), but instead a different thing.
    ;; I really think the code paths should be reconciled.
    ;; e.g. what's up with *USER-HASH-TABLE-TESTS* being checked
    ;; in %SET-FDEFINITION but not here?
    (maybe-clobber-ftype symbol new-value)
    (fset symbol new-value)))

;;; Incredibly bogus kludge: the :CAS-TRANS option in objdef makes no indication
;;; that you can not use it on certain platforms, so then you do try to use it,
;;; and you silently get no automatic IR2 conversion. The workaround in src/code/cas
;;; is unnecessary imho - why are we comparing the old value?
;;; To catch programming errors that occur only for non-threads apparently?
;;; The flaw is that it's dissociated from objdef, which ought to you give you
;;; the stub automatically somehow.
;;; Furthermore it's annoying that you can't name the CAS function (CAS fn).
#-compare-and-swap-vops
(progn
(defun cas-symbol-%info (symbol old new)
  (%primitive sb-c:set-slot symbol new
              '(setf symbol-%info) sb-vm:symbol-info-slot sb-vm:other-pointer-lowtag)
  old)
(defun sb-vm::cas-symbol-fdefn (symbol old new)
  (%primitive sb-c:set-slot symbol new
              '(setf symbol-fdefn) sb-vm:symbol-fdefn-slot sb-vm:other-pointer-lowtag)
  old))

;;; Accessors for the dual-purpose info/plist slot

;; A symbol's INFO slot is always in one of three states:
;;   1. NIL                                    ; the initial state
;;   2. #(data ....)                           ; globaldb used the slot
;;   3. (PLIST . NIL) or (PLIST . #(data ...)) ; plist was touched,
;;      and also possibly globaldb used the slot
;;
;; State 1 transitions to state 2 by assigning globaldb data,
;;         or to state 3 via ({SETF|CAS} SYMBOL-PLIST).
;;         (SETF INFO) by itself will never cause 1 -> 3 transition.
;; State 2 transitions to state 3 via ({SETF|CAS} SYMBOL-PLIST).
;; There are *no* other permissible state transitions.

;; Atomically update SYMBOL's info/plist slot to contain a new info vector.
;; The vector is computed by calling UPDATE-FN on the old vector,
;; repeatedly as necessary, until no conflict happens with other updaters.
;; The function may choose to abort the update by returning NIL.
(defun update-symbol-info (symbol update-fn)
  (declare (symbol symbol)
           (type (function (t) t) update-fn)
           (dynamic-extent update-fn))
  (prog ((info-holder (symbol-%info symbol))
         (current-info)) ; a PACKED-INFO or NIL
   outer-restart
    ;; This _must_ _not_ perform another read of the INFO slot here.
    (setq current-info (if (listp info-holder) (cdr info-holder) info-holder))
   inner-restart
    (let ((new-info (funcall update-fn (or current-info +nil-packed-infos+))))
      (unless (%instancep new-info)
        (aver (null new-info))
        (return)) ; nothing to do
      (if (consp info-holder) ; State 3: exchange the CDR
          (let ((old (cas (cdr info-holder) current-info new-info)))
            (when (eq old current-info) (return t)) ; win
            (setq current-info old) ; Don't touch holder- it's still a cons
            (go inner-restart)))
      ;; State 1 or 2: info-holder is NIL or a PACKED-INFO.
      ;; Exchange the contents of the info slot. Type-inference should have
      ;; derived that NEW-INFO satisfies the slot type restriction (I hope).
      (let ((old (cas-symbol-%info symbol info-holder new-info)))
        (when (eq old info-holder) (return t)) ; win
        ;; Check whether we're in state 2 or 3 now.
        ;; Impossible to be in state 1: nobody ever puts NIL in the slot.
        ;; Up above, we bailed out if the update-fn returned NIL.
        (setq info-holder old)
        (go outer-restart)))))

(defun symbol-plist (symbol)
  "Return SYMBOL's property list."
  (let ((list (symbol-%info symbol)))
    ;; The compiler can't possibly know that the CAR of LIST
    ;; is also a list (if LIST is a LIST), so force it with a TRULY-THE.
    ;; See the comments above UPDATE-SYMBOL-INFO for a
    ;; reminder as to why this logic is right.
    (if (%instancep list) nil (truly-the list (car list)))))

(declaim (ftype (sfunction (symbol t) cons) %ensure-plist-holder)
         (inline %ensure-plist-holder))

;; When a plist update (setf or cas) is first performed on a symbol,
;; a one-time allocation of an extra cons is done which creates two
;; "slots" from one: a slot for the PACKED-INFO and a slot for the plist.
;; This avoids complications in the implementation of the user-facing
;; (CAS SYMBOL-PLIST) function, which should not have to be aware of
;; competition from globaldb mutators even if no other threads attempt
;; to manipulate the plist per se.

;; Given a SYMBOL and its current INFO of type (OR LIST INSTANCE)
;; ensure that SYMBOL's current info is a cons, and return that.
;; If racing with multiple threads, at most one thread will install the cons.
(defun %ensure-plist-holder (symbol info)
  ;; Invoked only when SYMBOL is known to be a symbol.
  (declare (optimize (safety 0)))
  (declare (sb-c::tlab :system)) ; holder must be in the heap
  (if (consp info) ; it's fine to call this with a cell already installed
      info ; all done
      (let (newcell)
        ;; The pointer from the new cons to the old info must be persisted
        ;; to memory before the symbol's info slot points to the cons.
        ;; [x86oid doesn't need the barrier, others might]
        (setq newcell (cons nil info))
        (sb-thread:barrier (:write)) ; oh such ghastly syntax
        (loop (let ((old (cas-symbol-%info symbol info newcell)))
                (cond ((eq old info) (return newcell)) ; win
                      ((consp old) (return old))) ; somebody else made a cons!
                (setq info old)
                (rplacd newcell info)
                (sb-thread:barrier (:write))))))) ; Retry using same newcell

(declaim (inline (cas symbol-plist) (setf symbol-plist)))

(defun (cas symbol-plist) (old new symbol)
  ;; If SYMBOL's info cell is a cons, we can do (CAS CAR). Otherwise punt.
  (declare (symbol symbol) (list old new))
  (let ((cell (symbol-%info symbol)))
    (if (consp cell)
        (%compare-and-swap-car cell old new)
        (%cas-symbol-plist old new symbol))))

(defun %cas-symbol-plist (old new symbol)
  ;; This is just the second half of a partially-inline function, to avoid
  ;; code bloat in the exceptional case.  Type assertions should have been
  ;; done - or not, per policy - by the caller of %COMPARE-AND-SWAP-SYMBOL-PLIST
  ;; so now use TRULY-THE to avoid further type checking.
  (%compare-and-swap-car (%ensure-plist-holder (truly-the symbol symbol)
                                               (symbol-%info symbol))
                         old new))

(defun (setf symbol-plist) (new-value symbol)
  ;; If SYMBOL's info cell is a cons, we can do (SETF CAR). Otherwise punt.
  (declare (symbol symbol) (list new-value))
  (let ((cell (symbol-%info symbol)))
    (if (consp cell)
        (setf (car cell) new-value)
        (%set-symbol-plist symbol new-value))))

(defun %set-symbol-plist (symbol new-value)
  ;; Same considerations as for %CAS-SYMBOL-PLIST,
  ;; with a slight efficiency hack: if the symbol has no plist holder cell
  ;; and the NEW-VALUE is NIL, try to avoid creating a holder cell.
  ;; Yet we must write something, because omitting a memory operation
  ;; could have a subtle effect in the presence of multi-threading.
  (let ((info (symbol-%info (truly-the symbol symbol))))
    (when (and (not new-value) (atom info)) ; try to treat this as a no-op
      ;; INFO is either an INSTANCE (a PACKED-INFO) or NIL.
      ;; Write the same thing back, to say we set the plist to NIL.
      (let ((old (cas-symbol-%info symbol info info)))
        (if (eq old info) ; good enough
            (return-from %set-symbol-plist new-value) ; = nil
            (setq info old))))
    (setf (car (%ensure-plist-holder symbol info)) new-value)))

;;; End of Info/Plist slot manipulation

(defun symbol-name (symbol)
  "Return SYMBOL's name as a string."
  (symbol-name symbol))

(define-symbol-macro *id->package*
    (truly-the simple-vector
     (sap-ref-lispobj (foreign-symbol-sap "lisp_package_vector" t) 0)))
(export '*id->package*)

(defun sb-xc:symbol-package (symbol)
  "Return SYMBOL's home package, or NIL if none."
  (%symbol-package symbol))
(defun %symbol-package (symbol)
  ;; only called via transform
  ;; don't need arg-count check, type check, or vector bounds check.
  (declare (optimize (safety 0)))
  (let ((id (symbol-package-id symbol)))
    (truly-the (or null package)
               (if (= id +package-id-overflow+)
                   (values (info :symbol :package symbol))
                   (aref *id->package* id)))))

(defun %set-symbol-package (symbol package)
  (declare (type symbol symbol))
  (let* ((new-id (cond ((not package) +package-id-none+)
                       ((package-id package))
                       (t +package-id-overflow+)))
         (old-id (symbol-package-id symbol)))
    (when (= new-id +package-id-overflow+) ; put the package in the dbinfo
      (setf (info :symbol :package symbol) package))
    #-compact-symbol (set-symbol-package-id symbol new-id)
    #+compact-symbol
    (let ((disp #+big-endian 4
                #+x86-64 1
                #+(and little-endian (not x86-64)) 2))
      (with-pinned-objects (symbol)
        (setf (sap-ref-16 (int-sap (get-lisp-obj-address symbol))
                          (- disp sb-vm:other-pointer-lowtag))
              new-id)))
    ;; CLEAR-INFO is inefficient, so try not to call it.
    (when (and (= old-id +package-id-overflow+) (/= new-id +package-id-overflow+))
      (clear-info :symbol :package symbol))
    package))

;;; MAKE-SYMBOL is the external API, %MAKE-SYMBOL is the internal function receiving
;;; a known simple-string, and %ALLOC-SYMBOL is the primitive constructor.
(defun make-symbol (string)
  "Make and return a new symbol with the STRING as its print name."
  (declare (type string string))
  (%make-symbol 0 (if (simple-string-p string) string (subseq string 0))))

;;; All symbols go into immobile space if #+immobile-symbols is enabled,
;;; but not if disabled. The win with immobile space that is that all symbols
;;; can be considered static from an addressing viewpoint, but GC'able.
;;; (After codegen learns how, provided that defrag becomes smart enough
;;; to fixup machine code so that defrag remains meaningful)
;;;
;;; However, with immobile space being limited in size, you might not want
;;; symbols in there. In particular, if an application uses symbols as data
;;; - perhaps symbolic algebra on a Raspberry Pi - then not only is a faster
;;; purely Lisp allocator better, you probably want not to run out of space.
;;; The plausible heuristic that interned symbols be immobile, and otherwise not,
;;; is mostly ok, except for the unfortunate possibility of calling IMPORT
;;; on a random gensym. And even if a symbol is in immobile space at compile-time,
;;; it might not be at load-time, if you do nasty things like that, so really
;;; we can't make any reasonable determination - it's sort of all or nothing.

;;; We can perhaps hardcode addresses of keywords in any case if we think that
;;; people aren't in the habit of importing gensyms into #<package KEYWORD>.
;;; It's kinda useless to do that, though not technically forbidden.
;;; (It can produce a not-necessarily-self-evaluating keyword)

(defun %make-symbol (kind name)
  (declare (ignorable kind) (type simple-string name))
  ;; This constructor assumes that you should never create a symbol in an arena.
  ;; That being the case, you also never want to have its name in an arena,
  ;; or else it would make a heap->arena reference. Theoretically gensyms could
  ;; go in an arena, but that's such an obscure case. This has worked well so far
  ;; with the system TLAB assumption that I'm leaving that as-is, and mitigating the
  ;; problem with arena-allocated strings, rather than guessing whether you really
  ;; did want an arena-allocated symbol.
  (declare (sb-c::tlab :system))
  (binding*
      ((name
        ;; We clearly have permission to copy: "It is implementation-dependent whether
        ;; the string that becomes the new-symbol's name is the given name"
        ;; but may or may not have leeway to change the element-type.
        ;; I don't feel like writing a different variant of POSSIBLY-FROB-TO-HEAP just
        ;; to satisfy pedants. However, only users of arenas would detect
        ;; a change of element-type, if they care at all, which they don't.
        (if (or (dynamic-space-obj-p name) (read-only-space-obj-p name))
            name ; use as-is
            (possibly-base-stringize-to-heap name)))
       (()
        (when (and (not (logtest (ash sb-vm:+vector-shareable+ 8) (get-header-data name)))
                   ;; Readonly space is physically unwritable. Don't touch it.
                   (not (read-only-space-obj-p name)))
          (logior-array-flags name sb-vm:+vector-shareable+))) ; Set "logically read-only" bit
       (name-hash (calc-symbol-name-hash name (length name)))
       (symbol
         #+permgen
         (truly-the symbol (if (eql kind 0) ; uninterned
                               (sb-vm::%alloc-symbol name)
                               (allocate-permgen-symbol name)))
         #-permgen
         (truly-the symbol
          ;; If no immobile-space, easy: all symbols go in dynamic-space
          #-immobile-space (sb-vm::%alloc-symbol name)
          ;; If #+immobile-symbols, then uninterned symbols go in dynamic space, but
          ;; interned symbols go in immobile space. Good luck IMPORTing an uninterned symbol-
          ;; it'll work at least superficially, but if used as a code constant, the symbol's
          ;; address may violate the assumption that it's an imm32 operand.
          #+immobile-symbols
          (if (eql kind 0) (sb-vm::%alloc-symbol name) (sb-vm::%alloc-immobile-symbol name))
          #+(and immobile-space (not immobile-symbols))
          (if (or (eql kind 1) ; keyword
                  (and (eql kind 2) ; random interned symbol
                       (plusp (length name))
                       (char= (char name 0) #\*)
                       (char= (char name (1- (length name))) #\*)))
              (sb-vm::%alloc-immobile-symbol name)
              (sb-vm::%alloc-symbol name)))))
    #-salted-symbol-hash (%set-symbol-hash symbol name-hash)
    #+salted-symbol-hash
    (let ((salt (murmur-hash-word/fixnum
                 (word-mix name-hash (get-lisp-obj-address symbol)))))
      #+64-bit
      (let ((hash (logior (ash name-hash 32) (mask-field symbol-hash-prng-byte salt))))
        ;; %SET-SYMBOL-HASH wants a unsigned fixnum, which HASH is not.
        (%primitive sb-vm::set-slot symbol (%make-lisp-obj hash)
                    'make-symbol sb-vm:symbol-hash-slot sb-vm:other-pointer-lowtag))
      #-64-bit
      (with-pinned-objects (symbol) ; no vop sets the raw slot
        (setf (sap-ref-32 (int-sap (get-lisp-obj-address symbol))
                          (- (ash sb-vm:symbol-hash-slot sb-vm:word-shift)
                             sb-vm:other-pointer-lowtag))
              (logior (ash name-hash 3) (ldb (byte 3 0) salt)))))
    ;; Compact-symbol (which is equivalent to #+64-bit) has the package already NIL
    ;; because the PACKAGE-ID-BITS field defaults to 0.
    #-compact-symbol (%set-symbol-package symbol nil)
    symbol))

(defun get (symbol indicator &optional (default nil))
  "Look on the property list of SYMBOL for the specified INDICATOR. If this
  is found, return the associated value, else return DEFAULT."
  (get3 symbol indicator default))

(defun get3 (symbol indicator default)
  (let (cdr-pl)
    (do ((pl (symbol-plist symbol) (cdr cdr-pl)))
        ((atom pl) default)
      (setq cdr-pl (cdr pl))
      (cond ((atom cdr-pl)
             (error "~S has an odd number of items in its property list."
                    symbol))
            ((eq (car pl) indicator)
             (return (car cdr-pl)))))))

(defun %put (symbol indicator value)
  "The VALUE is added as a property of SYMBOL under the specified INDICATOR.
  Returns VALUE."
  (declare (sb-c::tlab :system))
  (do ((pl (symbol-plist symbol) (cddr pl)))
      ((endp pl)
       (setf (symbol-plist symbol)
             (list* indicator value (symbol-plist symbol)))
       value)
    (cond ((endp (cdr pl))
           (error "~S has an odd number of items in its property list."
                  symbol))
          ((eq (car pl) indicator)
           (rplaca (cdr pl) value)
           (return value)))))

(defun remprop (symbol indicator)
  "Look on property list of SYMBOL for property with specified
  INDICATOR. If found, splice this indicator and its value out of
  the plist, and return the tail of the original list starting with
  INDICATOR. If not found, return () with no side effects.

  NOTE: The ANSI specification requires REMPROP to return true (not false)
  or false (the symbol NIL). Portable code should not rely on any other value."
  (do ((pl (symbol-plist symbol) (cddr pl))
       (prev nil pl))
      ((atom pl) nil)
    (cond ((atom (cdr pl))
           (error "~S has an odd number of items in its property list."
                  symbol))
          ((eq (car pl) indicator)
           (cond (prev (rplacd (cdr prev) (cddr pl)))
                 (t
                  (setf (symbol-plist symbol) (cddr pl))))
           (return pl)))))

(defun getf (place indicator &optional (default ()))
  "Search the property list stored in PLACE for an indicator EQ to INDICATOR.
  If one is found, return the corresponding value, else return DEFAULT."
  (declare (explicit-check))
  (do* (cdr
        (plist place (cdr (truly-the cons cdr))))
       ((null plist) default)
    (cond ((or (atom plist)
               (atom (setf cdr (cdr plist))))
           (error 'simple-type-error
                  :format-control "malformed property list: ~S."
                  :format-arguments (list place)
                  :datum (if (atom plist)
                             plist
                             (cdr plist))
                  :expected-type 'cons))
          ((eq (car plist) indicator)
           (return (car (truly-the cons cdr)))))))

;;; Note: this will cons in an arena if you're using one.
(defun %putf (place indicator new-value)
  (do* (cdr
        (plist place (cdr (truly-the cons cdr))))
       ((null plist) (list* indicator new-value place))
    (cond ((or (atom plist)
               (atom (setf cdr (cdr plist))))
           (error 'simple-type-error
                  :format-control "malformed property list: ~S."
                  :format-arguments (list place)
                  :datum (if (atom plist)
                             plist
                             (cdr plist))
                  :expected-type 'cons))
          ((eq (car plist) indicator)
           (setf (cadr plist) new-value)
           (return place)))))

(defun get-properties (place indicator-list)
  "Like GETF, except that INDICATOR-LIST is a list of indicators which will
  be looked for in the property list stored in PLACE. Three values are
  returned, see manual for details."
  (do ((plist place (cddr plist)))
      ((null plist) (values nil nil nil))
    (cond ((atom (cdr plist))
           (error 'simple-type-error
                  :format-control "malformed property list: ~S."
                  :format-arguments (list place)
                  :datum (cdr plist)
                  :expected-type 'cons))
          ((memq (car plist) indicator-list)
           (return (values (car plist) (cadr plist) plist))))))

(defun copy-symbol (symbol &optional (copy-props nil))
  "Make and return a new uninterned symbol with the same print name
  as SYMBOL. If COPY-PROPS is false, the new symbol is neither bound
  nor fbound and has no properties, else it has a copy of SYMBOL's
  function, value and property list."
  (declare (type symbol symbol))
  (declare (sb-c::tlab :system)) ; heap-cons the property list if copying it
  (let ((new-symbol (make-symbol (symbol-name symbol))))
    (when copy-props
      ;; Should this really copy a thread-local value ?
      ;; I would think it more correct to copy only a global value.
      (%set-symbol-value new-symbol (%primitive sb-c:fast-symbol-value symbol))
      (locally (declare (optimize speed)) ; will inline COPY-LIST
        (setf (symbol-plist new-symbol)
              (copy-list (symbol-plist symbol))))
      (when (fboundp symbol)
        (setf (symbol-function new-symbol) (symbol-function symbol))))
    new-symbol))

(defun keywordp (object)
  "Return true if Object is a symbol in the \"KEYWORD\" package."
  (keywordp object)) ; transformed

;;;; GENSYM and friends

(defvar *gentemp-counter* 0)
(declaim (type unsigned-byte *gentemp-counter*))

(flet ((%symbol-nameify (prefix counter)
  (declare (string prefix))
  (if (and (typep prefix 'simple-base-string)
           (typep counter '(and fixnum unsigned-byte)))
      (let ((s ""))
        (declare (simple-base-string s))
        (labels ((recurse (depth n)
                   (multiple-value-bind (q r) (truncate n 10)
                     (if (plusp q)
                         (recurse (1+ depth) q)
                         (replace (setq s (make-string (+ (length prefix) depth)
                                                       :element-type 'base-char))
                                  (truly-the simple-base-string prefix)))
                     (setf (char s (- (length s) depth))
                           (code-char (+ (char-code #\0) r)))
                     s)))
          (recurse 1 counter)))
      (%with-output-to-string (s)
        (write-string prefix s)
        (%output-integer-in-base counter 10 s)))))

(defvar *gensym-counter* 0
  "counter for generating unique GENSYM symbols")

(defun gensym (&optional (thing "G"))
  "Creates a new uninterned symbol whose name is a prefix string (defaults
   to \"G\"), followed by a decimal number. Thing, when supplied, will
   alter the prefix if it is a string, or be used for the decimal number
   if it is a number, of this symbol. The default value of the number is
   the current value of *gensym-counter* which is incremented each time
   it is used."
  (multiple-value-bind (prefix int)
      (if (integerp thing)
          (values "G" thing)
          (values thing (let ((old *gensym-counter*))
                          (setq *gensym-counter* (1+ old))
                          old)))
    (make-symbol (%symbol-nameify prefix int))))

(defun gentemp (&optional (prefix "T") (package (sane-package)))
  "Creates a new symbol interned in package PACKAGE with the given PREFIX."
  (loop (multiple-value-bind (sym accessibility)
            (intern (%symbol-nameify prefix (incf *gentemp-counter*)) package)
          (unless accessibility (return sym))))))

(defmacro frob-symbol-progv-optimize (sym bit)
  (sb-c::if-vop-existsp (:translate reset-header-bits)
    (ecase bit
      (1 `(logior-header-bits ,sym sb-vm::+symbol-fast-bindable+))
      (0 `(reset-header-bits ,sym sb-vm::+symbol-fast-bindable+)))

    ;; This way avoids a race with a thread assigning the TLS index
    ;; when the required vops don't exist, which matters if and only
    ;; if #+(and sb-thread 64-bit).
    ;; Consider two threads binding the same symbol using PROGV:
    ;;  thread A                    Thread B
    ;;  --------                    --------
    ;;  SET BIT:                    SET BIT:
    ;;    x := load header word        x := load header word
    ;;    logior x, bit                logior x, bit
    ;;    store header word            ...
    ;;                                 ... (descheduled by kernel)
    ;;  DYNBIND:                       ...
    ;;    ensure-tls-index             ...
    ;;                                 store header word ; BUG: clobbers TLS index
    ;;                              DYNBIND:
    ;;                                 ensure-tls-index ; BUG: picks a new TLS index
    ;;
    `(with-pinned-objects (,sym)
       (let ((sap (int-sap (get-lisp-obj-address ,sym)))
             (offset (+ (- sb-vm:other-pointer-lowtag)
                        #+big-endian (- sb-vm:n-word-bytes 2)
                        #+little-endian 1)))
         ;; ASSUMPTION: byte stores are atomic and do not affect adjacent bytes
         (setf (sap-ref-8 sap offset)
               (,(ecase bit (1 'logior) (0 'logandc2)) (sap-ref-8 sap offset)
                 sb-vm::+symbol-fast-bindable+))))))

(defun unset-symbol-progv-optimize (symbol)
  (frob-symbol-progv-optimize symbol 0)
  symbol)

(macrolet ((signal-type-error (action-description)
             `(let ((spec (type-specifier type)))
                (cerror "Proceed anyway"
                        'simple-type-error
                        :format-control "~@<Cannot ~@? to ~S, not of type ~S.~:@>"
                        :format-arguments (list ,action-description symbol new-value spec)
                        :datum new-value
                        :expected-type spec))))
;;; This function is to be called just before a change which would affect the
;;; symbol value. We don't absolutely have to call this function before such
;;; changes, since such changes to constants are given as undefined behavior,
;;; it's nice to do so. To circumvent this you need code like this:
;;;
;;;   (defvar foo)
;;;   (defun set-foo (x) (setq foo x))
;;;   (defconstant foo 42)
;;;   (set-foo 13)
;;;   foo => 13, (constantp 'foo) => t
;;;
;;; ...in which case you frankly deserve to lose.
;;;
;;; If this function returns normally and was called from PROGV in unsafe code,
;;; then we'll remember that it's OK not to call it again.
;;; However, in safe code, PROGV still performs its type checking.
(defun about-to-modify-symbol-value (symbol action &optional (new-value nil valuep) bind)
  (declare (symbol symbol))
  (declare (explicit-check))
  (flet ((describe-action ()
           (ecase action
             (set "set SYMBOL-VALUE of ~S")
             (progv "bind ~S")
             (compare-and-swap "compare-and-swap SYMBOL-VALUE of ~S")
             (defconstant "define ~S as a constant")
             (makunbound "make ~S unbound"))))
    (let ((kind (info :variable :kind symbol)))
      (multiple-value-bind (complaint continuable)
          (cond ((eq kind :constant)
                 (cond ((eq symbol t)
                        (values "Veritas aeterna. (can't ~@?)" nil))
                       ((eq symbol nil)
                        (values "Nihil ex nihil. (can't ~@?)" nil))
                       ((keywordp symbol)
                        (values "Can't ~@?." nil))
                       (t
                        (values "Constant modification: attempt to ~@?." t))))
                ((and bind (eq kind :global))
                 (values "Can't ~@? (global variable)." nil))
                ((and (eq action 'set)
                      (eq kind :unknown))
                 (with-single-package-locked-error
                     (:symbol symbol "setting the value of ~S"))
                 nil)
                ((eq action 'makunbound)
                 (with-single-package-locked-error (:symbol symbol "unbinding the symbol ~A")
                   (when (eq (info :variable :always-bound symbol) :always-bound)
                     (values "Can't ~@?" nil)))))
        (cond ((not complaint)
               ;; the optimize bit says whether PROGV can be optimized, and not other actions
               (when (eq action 'progv)
                 (let ((package (symbol-package symbol)))
                   (if (or (not package) (not (package-locked-p package)))
                       (frob-symbol-progv-optimize symbol 1)))))
              (continuable
               (cerror "Modify the constant." complaint (describe-action) symbol))
              (t
               (error complaint (describe-action) symbol)))))
    (when (and valuep (neq action 'progv))
      (multiple-value-bind (type declaredp) (info :variable :type symbol)
        ;; If globaldb returned the default of *UNIVERSAL-TYPE*,
        ;; don't bother with a type test.
        (when (and declaredp (not (%%typep new-value type 'functionp)))
          (signal-type-error (describe-action)))))
    nil))

;;; Despite the naming symmetry, these functions are not exactly symmetrical in
;;; how they are used by the translation of PROGV
;;; In safe code, the fast-bindable bit is checked inside the assertion,
;;; because in all cases, we potentially perform a type-check which is too much
;;; to inline into PROGV.
;;; In unsafe code, if the symbol's fast-bindable bit is on, then we do NOT call
;;; the assertion function. When called, its role is to assert that the symbol is
;;; bindable and then set the bit saying never to call it again unless
;;; the bit gets unset.
(defun assert-dynbindable-safe (symbol new-value)
  (declare (symbol symbol))
  (unless (test-header-data-bit symbol sb-vm::+symbol-fast-bindable+)
    (about-to-modify-symbol-value symbol 'progv nil t))
  ;; Perform the type check here, not in ABOUT-TO-MODIFY-SYMBOL-VALUE, so that that
  ;; function does not have to be informed when NOT to peform a check (i.e. in usafe code).
  ;; Specifically, it can always bypass a type-check when the action is progv.
  (multiple-value-bind (type declaredp) (info :variable :type symbol)
    (when (and declaredp (not (%%typep new-value type 'functionp)))
      (signal-type-error "bind ~S"))))
(defun assert-dynbindable-unsafe (symbol)
  (declare (symbol symbol))
  (about-to-modify-symbol-value symbol 'progv nil t))
) ; end MACROLET

#+sb-thread (defun symbol-tls-index (x) (symbol-tls-index x)) ; necessary stub

(defun symbol-name-hash (symbol) (symbol-name-hash symbol)) ; transformed
(sb-c::when-vop-existsp (:translate hash-as-if-symbol-name)
  (defun hash-as-if-symbol-name (x) (hash-as-if-symbol-name x))) ; transformed
