;;;; This file contains functions that hack on the global function
;;;; namespace (primarily concerned with SETF functions here). Also,
;;;; function encapsulation and routines that set and return
;;;; definitions disregarding whether they might be encapsulated.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;; This variable properly belongs in 'target-hash-table',
;; but it's compiled after this file is.
(define-load-time-global *user-hash-table-tests* nil)


;;;; fdefinition (fdefn) objects

(defun make-fdefn (name)
  #-immobile-space (make-fdefn name)
  #+immobile-space
  (let ((fdefn (truly-the (values fdefn &optional)
                          (sb-vm::alloc-immobile-fdefn))))
    (sb-vm::%set-fdefn-name fdefn name)
    (fdefn-makunbound fdefn)
    fdefn))

(defun (setf fdefn-fun) (fun fdefn)
  (declare (type function fun)
           (type fdefn fdefn)
           (values function))
  #+immobile-code (sb-vm::%set-fdefn-fun fdefn fun)
  #-immobile-code (setf (fdefn-fun fdefn) fun))

;; Return SYMBOL's fdefinition, if any, or NIL. SYMBOL must already
;; have been verified to be a symbol by the caller.
(defun symbol-fdefn (symbol)
  (declare (optimize (safety 0)))
  (let ((fdefn (sb-vm::%symbol-fdefn symbol)))
    ;; The slot default is 0, not NIL, because I'm thinking that it might also
    ;; be used to store the property list if there is no FDEFN,
    ;; or a cons of an FDEFN and list, so 0 is unambiguously "no value"
    (if (eql fdefn 0) nil fdefn)))

;; Return the fdefn object for NAME, or NIL if there is no fdefn.
;; Signal an error if name isn't valid.
;; Assume that exists-p implies LEGAL-FUN-NAME-P.
;;
(declaim (ftype (sfunction ((or symbol list)) (or fdefn null)) find-fdefn))
(defun find-fdefn (name)
  (declare (explicit-check))
  (when (symbolp name) ; Don't need LEGAL-FUN-NAME-P check
    (return-from find-fdefn (symbol-fdefn name)))
  ;; Technically the ALLOW-ATOM argument of NIL isn't needed, but
  ;; the compiler isn't figuring out not to test SYMBOLP twice in a row.
  (with-globaldb-name (key1 key2 nil) name
      :hairy
      (awhen (get-fancily-named-fdefn name nil)
        (return-from find-fdefn it))
      :simple
      (progn
        (awhen (symbol-dbinfo key1)
          (multiple-value-bind (data-idx descriptor-idx field-idx)
              (info-find-aux-key/packed it key2)
            (declare (type index descriptor-idx)
                     (type (integer 0 #.+infos-per-word+) field-idx))
          ;; Secondary names must have at least one info, so if a descriptor
          ;; exists, there's no need to extract the n-infos field.
            (when data-idx
              (when (eql (incf field-idx) +infos-per-word+)
                (setq field-idx 0 descriptor-idx (1+ descriptor-idx)))
              (when (eql (packed-info-field it descriptor-idx field-idx)
                         +fdefn-info-num+)
                (return-from find-fdefn
                  (%info-ref it (1- (the index data-idx))))))))
        (when (eq key1 'setf) ; bypass the legality test
          (return-from find-fdefn nil))))
  (legal-fun-name-or-type-error name))

(declaim (ftype (sfunction (t) fdefn) find-or-create-fdefn))
(defun find-or-create-fdefn (name)
  (cond
    ((symbolp name)
     (let ((fdefn (sb-vm::%symbol-fdefn name)))
       (if (eql fdefn 0)
           (let* ((new (make-fdefn name))
                  (actual (sb-vm::cas-symbol-fdefn name 0 new)))
             (if (eql actual 0) new (the fdefn actual)))
           fdefn)))
    ((find-fdefn name))
    (t
      ;; We won't reach here if the name was not legal
      (let (made-new)
        (dx-flet ((new (name)
                    (setq made-new t)
                    (make-fdefn name)))
          (let ((fdefn (with-globaldb-name (key1 key2) name
                        :simple (get-info-value-initializing
                                 :function :definition name (new name))
                        :hairy (get-fancily-named-fdefn name #'new))))
            ;; Slot accessors spring into existence as soon as a reference
            ;; is made to the respective fdefn, but we can't do this in
            ;; (flet NEW) because ENSURE-ACCESSOR calls (SETF FDEFINITION)
            ;; which would recurse, as the fdefn would not have been
            ;; installed yet.
            (when (and made-new
                       (typep name '(cons (eql sb-pcl::slot-accessor))))
              (sb-pcl::ensure-accessor name))
            fdefn))))))

;;; Return T if FUNCTION is the error-signaling trampoline for a macro or a
;;; special operator. Test for this by seeing whether FUNCTION is the same
;;; closure as for a known macro.
(declaim (inline macro/special-guard-fun-p))
(defun macro/special-guard-fun-p (function)
  ;; When inlined, this is a few instructions shorter than CLOSUREP
  ;; if we already know that FUNCTION is a function.
  ;; It will signal a type error if not, which is the right thing to do anyway.
  ;; (this isn't quite a true predicate)
  (and (= (%fun-pointer-widetag function) sb-vm:closure-widetag)
       ;; This test needs to reference the name of any macro, but in order for
       ;; cold-init to work, the macro has to be defined first.
       ;; So pick DX-LET, as it's in primordial-extensions.
       ;; Prior to cold-init fixing up the load-time-value, this compares
       ;; %closure-fun to 0, which is ok - it returns NIL.
       (eq (load-time-value (%closure-fun (symbol-function 'dx-let)) t)
           (%closure-fun function))))

;;; Remove NAME's FTYPE information unless it was explicitly PROCLAIMED.
;;; The NEW-FUNCTION argument is presently unused, but could be used
;;; for checking compatibility of the NEW-FUNCTION against a proclamation.
;;; (We could issue a warning and/or remove the type if incompatible.)
(defun maybe-clobber-ftype (name new-function)
  (declare (ignore new-function))
  ;; Ignore PCL-internal function names.
  (unless (pcl-methodfn-name-p name)
    (unless (eq :declared (info :function :where-from name))
      (clear-info :function :type name))))

;;; Return the fdefn-fun of NAME's fdefinition including any encapsulations.
;;; LOOKUP-FN, defaulting to FIND-FDEFN, specifies how to lookup the fdefn.
;;; As a special case it can be given as SYMBOL-FDEFN which is slightly quicker.
;;; This is the core of the implementation of the standard FDEFINITION function,
;;; but as we've defined FDEFINITION, that strips encapsulations.
(defmacro %coerce-name-to-fun (name &optional (lookup-fn 'find-fdefn)
                                    strictly-functionp)
  ;; Whoa! We were getting a warning from the *host* here -
  ;;   "Abbreviated type declaration: (BOOLEAN SB-IMPL::STRICTLY-FUNCTIONP)."
  ;; I guess it's because we hand it a lambda and it doesn't like our style?
  (declare (type boolean strictly-functionp))
  `(let* ((name ,name) (fdefn (,lookup-fn name)) f)
     (if (and fdefn
              (setq f (fdefn-fun (truly-the fdefn fdefn)))
                ;; If STRICTLY-FUNCTIONP is true, we make sure not to return an error
                ;; trampoline. This extra check ensures that full calls such as
                ;; (MAPCAR 'OR '()) signal an error that OR isn't a function.
                ;; This accords with the non-requirement that macros store strictly
                ;; a function in the symbol that names them. In many implementations,
                ;; (FUNCTIONP (SYMBOL-FUNCTION 'OR)) => NIL. We want to pretend that.
              ,@(if strictly-functionp '((not (macro/special-guard-fun-p f)))))
         f
         (retry-%coerce-name-to-fun name ,strictly-functionp))))

;;; If %COERCE-NAME-TO-FUN fails, continue here.
;;; LOOKUP-FN, being more about speed than semantics, is irrelevant.
;;; Once we're forced down the slow path, it doesn't matter whether the fdefn
;;; lookup considers generalized function names (which require a hash-table)
;;; versus optimizing for just symbols (by using SYMBOL-INFO).
;;;
;;; Furthermore we explicitly allow any function name when retrying,
;;; even if the erring caller was SYMBOL-FUNCTION. It is consistent
;;; that both #'(SETF MYNEWFUN) and '(SETF MYNEWFUN) are permitted
;;; as the object to use in the USE-VALUE restart.
(defun retry-%coerce-name-to-fun (name strictly-functionp)
  (setq name (restart-case (error 'undefined-function :name name)
               (continue ()
                 :report (lambda (stream)
                           (format stream "Retry using ~s." name))
                 name)
               (use-value (value)
                 :report (lambda (stream)
                           (format stream "Use specified function"))
                 :interactive read-evaluated-form
                 (if (functionp value)
                     (return-from retry-%coerce-name-to-fun value)
                     value))))
  (let ((fdefn (find-fdefn name)))
    (when fdefn
      (let ((f (fdefn-fun (truly-the fdefn fdefn))))
        (when (and f (or (not strictly-functionp)
                         (not (macro/special-guard-fun-p f))))
          (return-from retry-%coerce-name-to-fun f)))))
  (retry-%coerce-name-to-fun name strictly-functionp))

;; Coerce CALLABLE (a function-designator) to a FUNCTION.
;; The compiler emits this when someone tries to FUNCALL something.
;; Extended-function-designators are not accepted,
;; This function declares EXPLICIT-CHECK, and we avoid calling
;; SYMBOL-FUNCTION because that would do another check.
;; It would be great if this could change its error message
;; depending on the input to either:
;;   "foo is not a function designator" if not a CALLABLE
;;   "foo does not designate a currently defined function"
;;    if a symbol does not satisfy FBOUNDP.
(defun %coerce-callable-to-fun (callable)
  (declare (explicit-check))
  (etypecase callable
    (function callable)
    (symbol (%coerce-name-to-fun callable symbol-fdefn t))))

;;; Behaves just like %COERCE-CALLABLE-TO-FUN but has an ir2-convert optimizer.
(defun %coerce-callable-for-call (callable)
  (declare (explicit-check))
  (etypecase callable
    (function callable)
    (symbol (%coerce-name-to-fun callable symbol-fdefn t))))


;;;; definition encapsulation

(defstruct (encapsulation-info (:constructor make-encapsulation-info
                                             (type definition))
                               (:copier nil))
  ;; This is definition's encapsulation type. The encapsulated
  ;; definition is in the previous ENCAPSULATION-INFO element or
  ;; installed as the global definition of some function name.
  type
  ;; the previous, encapsulated definition. This used to be installed
  ;; as a global definition for some function name, but it was
  ;; replaced by an encapsulation of type TYPE.
  (definition nil :type function))
(declaim (freeze-type encapsulation-info))

;;; Replace the definition of NAME with a function that calls FUNCTION
;;; with the original function and its arguments.
;;; TYPE is whatever you would like to associate with this
;;; encapsulation for identification in case you need multiple
;;; encapsulations of the same name.
(defun encapsulate (name type function)
  (let* ((fdefn (find-fdefn name))
         (underlying-fun (sb-c:safe-fdefn-fun fdefn)))
    (when (macro/special-guard-fun-p underlying-fun)
      (error "~S can not be encapsulated" name))
    (when (typep underlying-fun 'generic-function)
      (return-from encapsulate
        (encapsulate-generic-function underlying-fun type function)))
    ;; We must bind and close over INFO. Consider the case where we
    ;; encapsulate (the second) an encapsulated (the first)
    ;; definition, and later someone unencapsulates the encapsulated
    ;; (first) definition. We don't want our encapsulation (second) to
    ;; bind basic-definition to the encapsulated (first) definition
    ;; when it no longer exists. When unencapsulating, we make sure to
    ;; clobber the appropriate INFO structure to allow
    ;; basic-definition to be bound to the next definition instead of
    ;; an encapsulation that no longer exists.
    (let ((info (make-encapsulation-info type underlying-fun)))
      (setf (fdefn-fun fdefn)
            (named-lambda encapsulation (&rest args)
              (apply function (encapsulation-info-definition info)
                     args))))))

;;; Find the encapsulation info that has been closed over.
(defun encapsulation-info (fun)
  (when (closurep fun)
    (find-if-in-closure #'encapsulation-info-p fun)))

;;; When removing an encapsulation, we must remember that
;;; encapsulating definitions close over a reference to the
;;; ENCAPSULATION-INFO that describes the encapsulating definition.
;;; When you find an info with the target type, the previous info in
;;; the chain has the ensulating definition of that type. We take the
;;; encapsulated definition from the info with the target type, and we
;;; store it in the previous info structure whose encapsulating
;;; definition it describes looks to this previous info structure for
;;; a definition to bind (see ENCAPSULATE). When removing the first
;;; info structure, we do something conceptually equal, but
;;; mechanically it is different.
(defun unencapsulate (name type)
  "Removes NAME's most recent encapsulation of the specified TYPE."
  (let* ((fdefn (find-fdefn name))
         (encap-info (encapsulation-info (fdefn-fun fdefn))))
    (declare (type (or encapsulation-info null) encap-info))
    (when (and fdefn (typep (fdefn-fun fdefn) 'generic-function))
      (return-from unencapsulate
        (unencapsulate-generic-function (fdefn-fun fdefn) type)))
    (cond ((not encap-info)
           ;; It disappeared on us, so don't worry about it.
           )
          ((eq (encapsulation-info-type encap-info) type)
           ;; It's the first one, so change the fdefn object.
           (setf (fdefn-fun fdefn)
                 (encapsulation-info-definition encap-info)))
          (t
           ;; It must be an interior one, so find it.
           (loop
             (let ((next-info (encapsulation-info
                               (encapsulation-info-definition encap-info))))
               (unless next-info
                 ;; Not there, so don't worry about it.
                 (return))
               (when (eq (encapsulation-info-type next-info) type)
                 ;; This is it, so unlink us.
                 (setf (encapsulation-info-definition encap-info)
                       (encapsulation-info-definition next-info))
                 (return))
               (setf encap-info next-info))))))
  t)

;;; Does NAME have an encapsulation of the given TYPE?
(defun encapsulated-p (name type)
  (let ((fdefn (find-fdefn name)))
    (when (and fdefn (typep (fdefn-fun fdefn) 'generic-function))
      (return-from encapsulated-p
        (encapsulated-generic-function-p (fdefn-fun fdefn) type)))
    (do ((encap-info (encapsulation-info (fdefn-fun fdefn))
                     (encapsulation-info
                      (encapsulation-info-definition encap-info))))
        ((null encap-info) nil)
      (declare (type (or encapsulation-info null) encap-info))
      (when (eq (encapsulation-info-type encap-info) type)
        (return t)))))

;;;; FDEFINITION

;;; KLUDGE: Er, it looks as though this means that
;;;    (FUNCALL (FDEFINITION 'FOO))
;;; doesn't do the same thing as
;;;    (FUNCALL 'FOO),
;;; and (SYMBOL-FUNCTION 'FOO) isn't in general the same thing
;;; as (FDEFINITION 'FOO). That doesn't look like ANSI behavior to me.
;;; Look e.g. at the ANSI definition of TRACE: "Whenever a traced
;;; function is invoked, information about the call, ..". Try this:
;;;   (DEFUN FOO () (PRINT "foo"))
;;;   (TRACE FOO)
;;;   (FUNCALL 'FOO)
;;;   (FUNCALL (FDEFINITION 'FOO))
;;; What to do? ANSI says TRACE "Might change the definitions of the
;;; functions named by function-names." Might it be OK to just get
;;; punt all this encapsulation stuff and go back to a simple but
;;; correct implementation of TRACE? We'd lose the ability to redefine
;;; a TRACEd function and keep the trace in place, but that seems
;;; tolerable to me. (Is the wrapper stuff needed for anything else
;;; besides TRACE?)
;;;
;;; The only problem I can see with not having a wrapper: If tracing
;;; EQ, EQL, EQUAL, or EQUALP causes its function address to change,
;;; it will mess up the MAKE-HASH-TABLE logic which uses EQ tests
;;; on those function values. But given the ANSI statement about
;;; TRACE causing things to change, that doesn't seem too unreasonable;
;;; and we might even be able to forbid tracing these functions.
;;; -- WHN 2001-11-02
(defun fdefinition (name)
  "Return name's global function definition taking care to respect any
   encapsulations and to return the innermost encapsulated definition.
   This is SETF'able."
  (declare (explicit-check))
  (let ((fun (%coerce-name-to-fun name)))
    (loop
     (let ((encap-info (encapsulation-info fun)))
       (if encap-info
           (setf fun (encapsulation-info-definition encap-info))
           (return fun))))))

(defvar *setf-fdefinition-hook* nil
  "A list of functions that (SETF FDEFINITION) invokes before storing the
   new value. The functions take the function name and the new value.")

;; Reject any "object of implementation-dependent nature" that
;; so happens to be a function in SBCL, but which must not be
;; bound to a function-name by way of (SETF FEDFINITION).
(defun err-if-unacceptable-function (object setter)
  (when (macro/special-guard-fun-p object)
    (error 'simple-reference-error
           :references '((:ansi-cl :function fdefinition))
           :format-control "~S is not acceptable to ~S."
           :format-arguments (list object setter))))

(defun %set-fdefinition (name new-value)
  "Set NAME's global function definition."
  (declare (type function new-value) (optimize (safety 1)))
  (declare (explicit-check))
  (err-if-unacceptable-function new-value '(setf fdefinition))
  (with-single-package-locked-error (:symbol name "setting fdefinition of ~A")
    (maybe-clobber-ftype name new-value)

    ;; Check for hash-table stuff. Woe onto him that mixes encapsulation
    ;; with this.
    (when (and (symbolp name) (fboundp name))
      (let ((old (symbol-function name)))
        (when (boundp '*setf-fdefinition-hook*)
          (dolist (spec *user-hash-table-tests*)
            (cond ((eq old (second spec))
                   ;; test-function
                   (setf (second spec) new-value))
                  ((eq old (third spec))
                   ;; hash-function
                   (setf (third spec) new-value)))))))

    ;; FIXME: This is a good hook to have, but we should probably
    ;; reserve it for users.
    (let ((fdefn (find-or-create-fdefn name)))
      ;; *SETF-FDEFINITION-HOOK* won't be bound when initially running
      ;; top level forms in the kernel core startup.
      (when (boundp '*setf-fdefinition-hook*)
        (dolist (f *setf-fdefinition-hook*)
          (declare (type function f))
          (funcall f name new-value)))

      (let ((encap-info (encapsulation-info (fdefn-fun fdefn))))
        (cond (encap-info
               (loop
                (let ((more-info
                       (encapsulation-info
                        (encapsulation-info-definition encap-info))))
                  (if more-info
                      (setf encap-info more-info)
                      (return
                        (setf (encapsulation-info-definition encap-info)
                              new-value))))))
              (t
               (setf (fdefn-fun fdefn) new-value)))))))

;;;; FBOUNDP and FMAKUNBOUND

(defun fboundp (name)
  "Return true if name has a global function definition."
  (declare (explicit-check))
  (awhen (find-fdefn name) (fdefn-fun it)))

(defun fmakunbound (name)
  "Make NAME have no global function definition."
  (declare (explicit-check))
  (with-single-package-locked-error
      (:symbol name "removing the function or macro definition of ~A")
    (let ((fdefn (find-fdefn name)))
      (when fdefn
        #+immobile-code
        (when (sb-vm::fdefn-has-static-callers fdefn)
          (sb-vm::remove-static-links fdefn))
        (fdefn-makunbound fdefn)))
    (undefine-fun-name name)
    name))

;;; A simple open-addressing hashset.
(define-load-time-global *fdefns*
  (cons (make-array 128 :initial-element 0) 0))
(define-load-time-global *fdefns-lock* (sb-thread:make-mutex :name "fdefns"))

;;; Fancily named fdefns are not attached to symbols, but instead in a custom
;;; data structure which we probe in the manner of a quadratic probing hash-table.
;;; A max load factor ensures that probing terminates.
;;; https://fgiesen.wordpress.com/2015/02/22/triangular-numbers-mod-2n/
;;; contains a proof that triangular numbers mod 2^N visit every cell.

;;; The intent here - which may be impossible to realize - was to allow garbage-collection
;;; of FDEFNs whose name is not reachable.  I couldn't get it to do the right thing.
;;; e.g. (defmethod foo (x (y cons)) ...) creates mappings:
;;; (SB-PCL::FAST-METHOD FOO (T CONS)) -> #<SB-KERNEL:FDEFN (SB-PCL::FAST-METHOD FOO (T CONS))>
;;; (SB-PCL::SLOW-METHOD FOO (T CONS)) -> #<SB-KERNEL:FDEFN (SB-PCL::SLOW-METHOD FOO (T CONS))>
;;; where it seems like (unintern 'FOO) should allow both of those to get GCd.
;;; I suspect that it will require hanging those fancily named fdefns off the symbol
;;; FOO rather than having a global table.  Alternatively, that can be simulated by
;;; having GC preserve liveness of any element whenever the second item in the list
;;; comprising fdefn-name is an a-priori live symbol.  That will be more efficient than
;;; having a hash-table hanging off every symbol that names a method.
;;; e.g. both of the preceding names would be hanging off of FOO, as would others
;;; such as (FAST-METHOD FOO :AROUND (LIST INTEGER)) and a myriad of others.
;;; I suspect that any approach of hanging off the symbols will be space-inefficient
;;; and difficult to implement.

;;; At any rate, we can make use of the key-in-value nature of fdefns to halve
;;; the number of words required to store the name -> object mapping.
(defun get-fancily-named-fdefn (name constructor &aux (hash (globaldb-sxhashoid name)))
  (declare (type (or function null) constructor))
  (labels ((lookup (vector &aux (mask (1- (length vector)))
                                (index (logand hash mask))
                                (step 0)
                                (empty-cell nil))
             ;; Because rehash is forced well before the table becomes 100% full,
             ;; it should not be possible to loop infinitely here.
             (loop (let ((fdefn (svref vector index)))
                     (cond ((eql fdefn 0) ; not found
                            (return-from lookup (or empty-cell index)))
                           #+nil ((eql fdefn nil) ; smashed by GC
                                  (unless empty-cell (setq empty-cell index)))
                           ((equal (fdefn-name fdefn) name)
                            (return-from lookup fdefn))))
                   (setq index (logand (+ index (incf step)) mask))))
           (insert (hash item vector mask &aux (index (logand hash mask))
                                               (step 0)
                                               (empty-cell nil))
             (loop (case (svref vector index)
                    ((0) ; not found
                     (return (setf (svref vector (or empty-cell index)) item)))
                    #+nil ((nil) ; smashed by GC
                           (unless empty-cell (setq empty-cell index))))
                   (setq index (logand (+ index (incf step)) mask)))))
    (or (let ((result (lookup (car *fdefns*))))
          (when (fdefn-p result) result))
        (when constructor ; double-check w/lock before inserting
          (with-system-mutex (*fdefns-lock*)
            (let* ((fdefns *fdefns*)
                   (vector (car fdefns))
                   (result (lookup vector)))
              (if (fdefn-p result)
                  result
                  (let ((new-fdefn (funcall constructor name)))
                    (if (<= (incf (cdr fdefns)) (ash (length vector) -1)) ; under 50% full
                        ;; It might even be less full than that due to GC.
                        (setf (svref vector result) new-fdefn)
                        ;; The actual count is unknown without re-counting.
                        (let* ((count (count-if #'fdefn-p vector))
                               (new-size (power-of-two-ceiling
                                          (ceiling (* count 2))))
                               (new-vect (make-array new-size :initial-element 0))
                               (new-mask (1- new-size)))
                          (dovector (item vector)
                            (when (fdefn-p item)
                              (insert (globaldb-sxhashoid (fdefn-name item)) item
                                      new-vect new-mask)))
                          (insert hash new-fdefn new-vect new-mask)
                          (setf *fdefns* (cons new-vect (1+ count)))))
                    new-fdefn))))))))
