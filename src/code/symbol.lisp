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

(in-package "SB!IMPL")

(declaim (maybe-inline get3 %put getf remprop %putf get-properties keywordp))

;;; Used by [GLOBAL-]SYMBOL-VALUE compiler-macros:
;;;
;;; When SYMBOL is constant, check whether it names a deprecated
;;; variable, potentially signaling a {EARLY,LATE}-DEPRECATION-WARNING
;;; in the process. Furthermore, if the deprecation state is :FINAL,
;;; replace FORM by SYMBOL, causing the symbol-macro on SYMBOL to
;;; expand into a call to DEPRECATION-ERROR.
;;;
;;; See SB-IMPL:SETUP-VARIABLE-IN-FINAL-DEPRECATION.
#-sb-xc-host
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun maybe-handle-deprecated-global-variable (symbol env)
    (when (sb!xc:constantp symbol env)
      (let ((name (constant-form-value symbol env)))
        (when (symbolp name)
          (case (deprecated-thing-p 'variable name)
            ((:early :late)
             (check-deprecated-thing 'variable name)
             nil)
            ;; In this case, there is a symbol-macro for NAME that
            ;; will signal the FINAL-DEPRECATION-WARNING when
            ;; ir1converted and the DEPRECATION-ERROR at runtime.
            (:final
             name)))))))

(defun symbol-value (symbol)
  #!+sb-doc
  "Return SYMBOL's current bound value."
  (declare (optimize (safety 1)))
  (symbol-value symbol))

#-sb-xc-host
(define-compiler-macro symbol-value (&whole form symbol &environment env)
  (or (maybe-handle-deprecated-global-variable symbol env) form))

(defun boundp (symbol)
  #!+sb-doc
  "Return non-NIL if SYMBOL is bound to a value."
  (boundp symbol))

(defun set (symbol new-value)
  #!+sb-doc
  "Set SYMBOL's value cell to NEW-VALUE."
  (declare (type symbol symbol))
  (about-to-modify-symbol-value symbol 'set new-value)
  (%set-symbol-value symbol new-value))

(defun %set-symbol-value (symbol new-value)
  (%set-symbol-value symbol new-value))

(defun symbol-global-value (symbol)
  #!+sb-doc
  "Return the SYMBOL's current global value. Identical to SYMBOL-VALUE,
in single-threaded builds: in multithreaded builds bound values are
distinct from the global value. Can also be SETF."
  (declare (optimize (safety 1)))
  (symbol-global-value symbol))

#-sb-xc-host
(define-compiler-macro symbol-global-value (&whole form symbol
                                            &environment env)
  (or (maybe-handle-deprecated-global-variable symbol env) form))

(defun set-symbol-global-value (symbol new-value)
  (about-to-modify-symbol-value symbol 'set new-value)
  (%set-symbol-global-value symbol new-value))

(declaim (inline %makunbound))
(defun %makunbound (symbol)
  (%set-symbol-value symbol (%primitive sb!c:make-unbound-marker)))

(defun makunbound (symbol)
  #!+sb-doc
  "Make SYMBOL unbound, removing any value it may currently have."
  (with-single-package-locked-error (:symbol symbol "unbinding the symbol ~A")
    ;; :EVENTUALLY is allowed for :always-bound here, as it has no bearing
    (when (eq (info :variable :always-bound symbol) :always-bound)
      (error "Can't make ~A variable unbound: ~S" 'always-bound symbol))
    (about-to-modify-symbol-value symbol 'makunbound)
    (%makunbound symbol)
    symbol))

;; Compute a symbol's hash. Also used by FIND-SYMBOL which requires that a hash
;; be a pure function of the name and not a semi-opaque property of the symbol.
;; The hash of all symbols named "NIL" must be the same, so not to pessimize
;; FIND-SYMBOL by special-casing the finding of CL:NIL with an extra "or"
;; in the hash-equality test. i.e. We can't recognize that CL:NIL was the
;; object sought (having an exceptional hash) until it has been found.
(defun compute-symbol-hash (string length)
  (declare (simple-string string) (index length))
  (if (and (= length 3)
           (locally
            ;; SXHASH-SUBSTRING is unsafe, so this is too. but do we know that
            ;; length is ok, or is it an accident that it can scan too far?
            (declare (optimize (safety 0)))
            (string-dispatch (simple-base-string (simple-array character (*)))
                             string
              (and (char= (schar string 0) #\N)
                   (char= (schar string 1) #\I)
                   (char= (schar string 2) #\L)))))
      ;; FIXME: hardwire this. See similar comment at
      ;;   (deftransform sxhash ((x) (symbol))
      (return-from compute-symbol-hash (symbol-hash nil)))
  ;; And make a symbol's hash not the same as (sxhash name) in general.
  (let ((sxhash (logand (lognot (%sxhash-simple-substring string length))
                        sb!xc:most-positive-fixnum)))
    (if (zerop sxhash) #x55AA sxhash))) ; arbitrary substitute for 0

;; Return SYMBOL's hash, a strictly positive fixnum, computing it if not stored.
;; The inlined code for (SXHASH symbol) only calls ENSURE-SYMBOL-HASH if
;; needed, however this is ok to call even if the hash is already nonzero.
(defun ensure-symbol-hash (symbol)
  (let ((hash (symbol-hash symbol)))
    (if (zerop hash)
        (let ((name (symbol-name symbol)))
          (%set-symbol-hash symbol (compute-symbol-hash name (length name))))
      hash)))

;;; Interpreter stub: Return whatever is in the SYMBOL-HASH slot of SYMBOL.
(defun symbol-hash (symbol)
  (symbol-hash symbol))

(defun symbol-function (symbol)
  #!+sb-doc
  "Return SYMBOL's current function definition. Settable with SETF."
  (%coerce-name-to-fun symbol symbol-fdefn))

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
  (with-single-package-locked-error
      (:symbol symbol "setting the symbol-function of ~A")
    ;; This code is a little "surprising" in that it is not just a limited
    ;; case of (SETF FDEFINITION), but instead a different thing.
    ;; I really think the code paths should be reconciled.
    ;; e.g. what's up with *USER-HASH-TABLE-TESTS* being checked
    ;; in %SET-FDEFINITION but not here?
    (maybe-clobber-ftype symbol new-value)
    (let ((fdefn (find-or-create-fdefn symbol)))
      (setf (fdefn-fun fdefn) new-value))))

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

(defun symbol-info (symbol)
  (symbol-info symbol))

;; An "interpreter stub" for an operation that is only implemented for
;; the benefit of platforms without compare-and-swap-vops.
(defun (setf symbol-info) (new-info symbol)
  (setf (symbol-info symbol) new-info))

;; Atomically update SYMBOL's info/plist slot to contain a new info vector.
;; The vector is computed by calling UPDATE-FN on the old vector,
;; repeatedly as necessary, until no conflict happens with other updaters.
;; The function may choose to abort the update by returning NIL.
(defun update-symbol-info (symbol update-fn)
  (declare (symbol symbol)
           (type (function (t) t) update-fn))
  (prog ((info-holder (symbol-info symbol))
         (current-vect))
   outer-restart
    ;; Do not use SYMBOL-INFO-VECTOR - this must not perform a slot read again.
    (setq current-vect (if (listp info-holder) (cdr info-holder) info-holder))
   inner-restart
    ;; KLUDGE: The "#." on +nil-packed-infos+ is due to slightly crippled
    ;; fops in genesis's fasload. Anonymizing the constant works around the
    ;; issue, at the expense of an extra copy of the empty info vector.
    (let ((new-vect (funcall update-fn
                             (or current-vect #.+nil-packed-infos+))))
      (unless (simple-vector-p new-vect)
        (aver (null new-vect))
        (return)) ; nothing to do
      (if (consp info-holder) ; State 3: exchange the CDR
          (let ((old (%compare-and-swap-cdr info-holder current-vect new-vect)))
            (when (eq old current-vect) (return t)) ; win
            (setq current-vect old) ; Don't touch holder- it's still a cons
            (go inner-restart)))
      ;; State 1 or 2: info-holder is NIL or a vector.
      ;; Exchange the contents of the info slot. Type-inference derives
      ;; SIMPLE-VECTOR-P on the args to CAS, so no extra checking.
      (let ((old (%compare-and-swap-symbol-info symbol info-holder new-vect)))
        (when (eq old info-holder) (return t)) ; win
        ;; Check whether we're in state 2 or 3 now.
        ;; Impossible to be in state 1: nobody ever puts NIL in the slot.
        ;; Up above, we bailed out if the update-fn returned NIL.
        (setq info-holder old)
        (go outer-restart)))))

(eval-when (:compile-toplevel)
  ;; If we're in state 1 or state 3, we can take (CAR (SYMBOL-INFO S))
  ;; to get the property list. If we're in state 2, this same access
  ;; gets the fixnum which is the VECTOR-LENGTH of the info vector.
  ;; So all we have to do is turn any fixnum to NIL, and we have a plist.
  ;; Ensure that this pun stays working.
  (assert (= (- (* sb!vm:n-word-bytes sb!vm:cons-car-slot)
                sb!vm:list-pointer-lowtag)
             (- (* sb!vm:n-word-bytes sb!vm:vector-length-slot)
                sb!vm:other-pointer-lowtag))))

(defun symbol-plist (symbol)
  #!+sb-doc
  "Return SYMBOL's property list."
  #!+symbol-info-vops
  (symbol-plist symbol) ; VOP translates it
  #!-symbol-info-vops
  (let ((list (car (truly-the list (symbol-info symbol))))) ; a white lie
    ;; Just ensure the result is not a fixnum, and we're done.
    (if (fixnump list) nil list)))

(declaim (ftype (sfunction (symbol t) cons) %ensure-plist-holder)
         (inline %ensure-plist-holder))

;; When a plist update (setf or cas) is first performed on a symbol,
;; a one-time allocation of an extra cons is done which creates two
;; "slots" from one: a slot for the info-vector and a slot for the plist.
;; This avoids complications in the implementation of the user-facing
;; (CAS SYMBOL-PLIST) function, which should not have to be aware of
;; competition from globaldb mutators even if no other threads attempt
;; to manipulate the plist per se.

;; Given a SYMBOL and its current INFO of type (OR LIST SIMPLE-VECTOR)
;; ensure that SYMBOL's current info is a cons, and return that.
;; If racing with multiple threads, at most one thread will install the cons.
(defun %ensure-plist-holder (symbol info)
  ;; Invoked only when SYMBOL is known to be a symbol.
  (declare (optimize (safety 0)))
  (if (consp info) ; it's fine to call this with a cell already installed
      info ; all done
      (let (newcell)
        ;; The pointer from the new cons to the old info must be persisted
        ;; to memory before the symbol's info slot points to the cons.
        ;; [x86oid doesn't need the barrier, others might]
        (sb!thread:barrier (:write)
          (setq newcell (cons nil info)))
        (loop (let ((old (%compare-and-swap-symbol-info symbol info newcell)))
                (cond ((eq old info) (return newcell)) ; win
                      ((consp old) (return old))) ; somebody else made a cons!
                (setq info old)
                (sb!thread:barrier (:write) ; Retry using same newcell
                  (rplacd newcell info)))))))

(declaim (inline %compare-and-swap-symbol-plist
                 %set-symbol-plist))

(defun %compare-and-swap-symbol-plist (symbol old new)
  ;; This is the entry point into which (CAS SYMBOL-PLIST) is transformed.
  ;; If SYMBOL's info cell is a cons, we can do (CAS CAR). Otherwise punt.
  (declare (symbol symbol) (list old new))
  (let ((cell (symbol-info symbol)))
    (if (consp cell)
        (%compare-and-swap-car cell old new)
        (%%compare-and-swap-symbol-plist symbol old new))))

(defun %%compare-and-swap-symbol-plist (symbol old new)
  ;; This is just the second half of a partially-inline function, to avoid
  ;; code bloat in the exceptional case.  Type assertions should have been
  ;; done - or not, per policy - by the caller of %COMPARE-AND-SWAP-SYMBOL-PLIST
  ;; so now use TRULY-THE to avoid further type checking.
  (%compare-and-swap-car (%ensure-plist-holder (truly-the symbol symbol)
                                               (symbol-info symbol))
                         old new))

(defun %set-symbol-plist (symbol new-value)
  ;; This is the entry point into which (SETF SYMBOL-PLIST) is transformed.
  ;; If SYMBOL's info cell is a cons, we can do (SETF CAR). Otherwise punt.
  (declare (symbol symbol) (list new-value))
  (let ((cell (symbol-info symbol)))
    (if (consp cell)
        (setf (car cell) new-value)
        (%%set-symbol-plist symbol new-value))))

(defun %%set-symbol-plist (symbol new-value)
  ;; Same considerations as for %%COMPARE-AND-SWAP-SYMBOL-PLIST,
  ;; with a slight efficiency hack: if the symbol has no plist holder cell
  ;; and the NEW-VALUE is NIL, try to avoid creating a holder cell.
  ;; Yet we must write something, because omitting a memory operation
  ;; could have a subtle effect in the presence of multi-threading.
  (let ((info (symbol-info (truly-the symbol symbol))))
    (when (and (not new-value) (atom info)) ; try to treat this as a no-op
      (let ((old (%compare-and-swap-symbol-info symbol info info)))
        (if (eq old info) ; good enough
            (return-from %%set-symbol-plist new-value) ; = nil
            (setq info old))))
    (setf (car (%ensure-plist-holder symbol info)) new-value)))

;;; End of Info/Plist slot manipulation

(defun symbol-name (symbol)
  #!+sb-doc
  "Return SYMBOL's name as a string."
  (symbol-name symbol))

(defun symbol-package (symbol)
  #!+sb-doc
  "Return the package SYMBOL was interned in, or NIL if none."
  (symbol-package symbol))

(defun %set-symbol-package (symbol package)
  (declare (type symbol symbol))
  (%set-symbol-package symbol package))

(defun make-symbol (string)
  #!+sb-doc
  "Make and return a new symbol with the STRING as its print name."
  (declare (type string string))
  (%make-symbol (if (simple-string-p string)
                    string
                    (subseq string 0))))

(defun get (symbol indicator &optional (default nil))
  #!+sb-doc
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
  #!+sb-doc
  "The VALUE is added as a property of SYMBOL under the specified INDICATOR.
  Returns VALUE."
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
  #!+sb-doc
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
  #!+sb-doc
  "Search the property list stored in PLACE for an indicator EQ to INDICATOR.
  If one is found, return the corresponding value, else return DEFAULT."
  (do ((plist place (cddr plist)))
      ((null plist) default)
    (cond ((atom (cdr plist))
           (error 'simple-type-error
                  :format-control "malformed property list: ~S."
                  :format-arguments (list place)
                  :datum (cdr plist)
                  :expected-type 'cons))
          ((eq (car plist) indicator)
           (return (cadr plist))))))

(defun %putf (place property new-value)
  (declare (type list place))
  (do ((plist place (cddr plist)))
      ((endp plist) (list* property new-value place))
    (declare (type list plist))
    (when (eq (car plist) property)
      (setf (cadr plist) new-value)
      (return place))))

(defun get-properties (place indicator-list)
  #!+sb-doc
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

(defun copy-symbol (symbol &optional (copy-props nil) &aux new-symbol)
  #!+sb-doc
  "Make and return a new uninterned symbol with the same print name
  as SYMBOL. If COPY-PROPS is false, the new symbol is neither bound
  nor fbound and has no properties, else it has a copy of SYMBOL's
  function, value and property list."
  (declare (type symbol symbol))
  (setq new-symbol (make-symbol (symbol-name symbol)))
  (when copy-props
    (%set-symbol-value new-symbol
                       (%primitive sb!c:fast-symbol-value symbol))
    (setf (symbol-plist new-symbol)
          (copy-list (symbol-plist symbol)))
    (when (fboundp symbol)
      (setf (symbol-function new-symbol) (symbol-function symbol))))
  new-symbol)

(defun keywordp (object)
  #!+sb-doc
  "Return true if Object is a symbol in the \"KEYWORD\" package."
  (and (symbolp object)
       (eq (symbol-package object) *keyword-package*)))

;;;; GENSYM and friends

(defun %make-symbol-name (prefix counter)
  (declare (string prefix))
  (if (typep counter '(and fixnum unsigned-byte))
      (let ((s ""))
        (declare (simple-string s))
        (labels ((recurse (depth n)
                   (multiple-value-bind (q r) (truncate n 10)
                     (if (plusp q)
                         (recurse (1+ depth) q)
                         (let ((et (if (or (base-string-p prefix)
                                           (every #'base-char-p prefix))
                                       'base-char 'character)))
                           (setq s (make-string (+ (length prefix) depth)
                                                :element-type et))
                           (replace s prefix)))
                     (setf (char s (- (length s) depth))
                           (code-char (+ (char-code #\0) r)))
                     s)))
          (recurse 1 counter)))
      (with-simple-output-to-string (s)
        (write-string prefix s)
        (%output-integer-in-base counter 10 s))))

(defvar *gensym-counter* 0
  #!+sb-doc
  "counter for generating unique GENSYM symbols")
(declaim (type unsigned-byte *gensym-counter*))

(defun gensym (&optional (thing "G"))
  #!+sb-doc
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
    (make-symbol (%make-symbol-name prefix int))))

(defvar *gentemp-counter* 0)
(declaim (type unsigned-byte *gentemp-counter*))

(defun gentemp (&optional (prefix "T") (package (sane-package)))
  #!+sb-doc
  "Creates a new symbol interned in package PACKAGE with the given PREFIX."
  (declare (type string prefix))
  (loop for name = (%make-symbol-name prefix (incf *gentemp-counter*))
        while (nth-value 1 (find-symbol name package))
        finally (return (values (intern name package)))))

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
      (multiple-value-bind (what continue)
          (cond ((eq :constant kind)
                 (cond ((eq symbol t)
                        (values "Veritas aeterna. (can't ~@?)" nil))
                       ((eq symbol nil)
                        (values "Nihil ex nihil. (can't ~@?)" nil))
                       ((keywordp symbol)
                        (values "Can't ~@?." nil))
                       (t
                        (values "Constant modification: attempt to ~@?." t))))
                ((and bind (eq :global kind))
                 (values "Can't ~@? (global variable)." nil)))
        (when what
          (if continue
              (cerror "Modify the constant." what (describe-action) symbol)
              (error what (describe-action) symbol)))
        (when valuep
          ;; :VARIABLE :TYPE is in the db only if it is declared, so no need to
          ;; check.
          (let ((type (info :variable :type symbol)))
            (unless (%%typep new-value type nil)
              (let ((spec (type-specifier type)))
                (error 'simple-type-error
                       :format-control "~@<Cannot ~@? to ~S, not of type ~S.~:@>"
                       :format-arguments (list (describe-action) symbol new-value spec)
                       :datum new-value
                       :expected-type spec)))))))
    nil))
