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

(in-package "SB!IMPL")

(sb!int::/show0 "fdefinition.lisp 22")

;;;; fdefinition (fdefn) objects

(defun make-fdefn (name)
  (make-fdefn name))

(defun fdefn-name (fdefn)
  (declare (type fdefn fdefn))
  (fdefn-name fdefn))

(defun fdefn-fun (fdefn)
  (declare (type fdefn fdefn)
           (values (or function null)))
  (fdefn-fun fdefn))

(defun (setf fdefn-fun) (fun fdefn)
  (declare (type function fun)
           (type fdefn fdefn)
           (values function))
  (setf (fdefn-fun fdefn) fun))

(defun fdefn-makunbound (fdefn)
  (declare (type fdefn fdefn))
  (fdefn-makunbound fdefn))

#!-sb-fluid (declaim (inline symbol-fdefn))
;; Return SYMBOL's fdefinition, if any, or NIL. SYMBOL must already
;; have been verified to be a symbol by the caller.
(defun symbol-fdefn (symbol)
  (declare (optimize (safety 0)))
  (info-vector-fdefn (symbol-info-vector symbol)))

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
      ;; INFO-GETHASH returns NIL or a vector. INFO-VECTOR-FDEFN accepts
      ;; either. If fdefn isn't found, fall through to the legality test.
      (awhen (info-vector-fdefn (info-gethash name *info-environment*))
        (return-from find-fdefn it))
      :simple
      (progn
        (awhen (symbol-info-vector key1)
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
                  (aref it (1- (the index data-idx))))))))
        (when (eq key1 'setf) ; bypass the legality test
          (return-from find-fdefn nil))))
  (legal-fun-name-or-type-error name))

(declaim (ftype (sfunction (t) fdefn) find-or-create-fdefn))
(defun find-or-create-fdefn (name)
  (or (find-fdefn name)
      ;; We won't reach here if the name was not legal
      (get-info-value-initializing :function :definition name
                                   (make-fdefn name))))

;;; Remove NAME's FTYPE information unless it was explicitly PROCLAIMED.
;;; The NEW-FUNCTION argument is presently unused, but could be used
;;; for checking compatibility of the NEW-FUNCTION against a proclamation.
;;; (We could issue a warning and/or remove the type if incompatible.)
(defun maybe-clobber-ftype (name new-function)
  (declare (ignore new-function))
  (unless (eq :declared (info :function :where-from name))
    (clear-info :function :type name)))

;;; Return the fdefn-fun of NAME's fdefinition including any encapsulations.
;;; LOOKUP-FN, defaulting to FIND-FDEFN, specifies how to lookup the fdefn.
;;; As a special case it can be given as SYMBOL-FDEFN which is slightly quicker.
;;; This is the core of the implementation of the standard FDEFINITION function,
;;; but as we've defined FDEFINITION, that strips encapsulations.
(defmacro %coerce-name-to-fun (name &optional (lookup-fn 'find-fdefn))
  `(let* ((name ,name) (fdefn (,lookup-fn name)))
     (if fdefn
         (sb!c:safe-fdefn-fun fdefn)
         (error 'undefined-function :name name))))

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
    (symbol (%coerce-name-to-fun callable symbol-fdefn))))


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

;;; Replace the definition of NAME with a function that calls FUNCTION
;;; with the original function and its arguments.
;;; TYPE is whatever you would like to associate with this
;;; encapsulation for identification in case you need multiple
;;; encapsulations of the same name.
(defun encapsulate (name type function)
  (let* ((fdefn (find-fdefn name))
         (underlying-fun (sb!c:safe-fdefn-fun fdefn)))
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

;;; This is like FIND-IF, except that we do it on a compiled closure's
;;; environment.
(defun find-if-in-closure (test closure)
  (declare (closure closure))
  (do-closure-values (value closure)
    (when (funcall test value)
      (return value))))

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
  #!+sb-doc
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
  #!+sb-doc
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
  #!+sb-doc
  "A list of functions that (SETF FDEFINITION) invokes before storing the
   new value. The functions take the function name and the new value.")

;; Return T if FUNCTION is the error-signaling trampoline
;; for a macro or a special operator. Test for this by seeing
;; whether FUNCTION is the same closure as for a known macro.
;; For cold-init to work, this must pick any macro defined before
;; this function is. A safe choice is a macro from this same file.
(defun macro/special-guard-fun-p (function)
  (and (closurep function)
       ;; Prior to cold-init fixing up the load-time-value, this compares
       ;; %closure-fun to 0, which is ok - it returns NIL.
       (eq (load-time-value
            (%closure-fun (symbol-function '%coerce-name-to-fun)) t)
           (%closure-fun function))))

;; Reject any "object of implementation-dependent nature" that
;; so happens to be a function in SBCL, but which must not be
;; bound to a function-name by way of (SETF FEDFINITION).
(defun err-if-unacceptable-function (object setter)
  (when (macro/special-guard-fun-p object)
    (error 'simple-reference-error
           :references (list '(:ansi-cl :function fdefinition))
           :format-control "~S is not acceptable to ~S."
           :format-arguments (list object setter))))

(defun %set-fdefinition (name new-value)
  #!+sb-doc
  "Set NAME's global function definition."
  (declare (type function new-value) (optimize (safety 1)))
  (declare (explicit-check))
  (err-if-unacceptable-function new-value '(setf fdefinition))
  (with-single-package-locked-error (:symbol name "setting fdefinition of ~A")
    (maybe-clobber-ftype name new-value)

    ;; Check for hash-table stuff. Woe onto him that mixes encapsulation
    ;; with this.
    (when (and (symbolp name) (fboundp name)
               (boundp '*user-hash-table-tests*))
      (let ((old (symbol-function name)))
        (declare (special *user-hash-table-tests*))
        (dolist (spec *user-hash-table-tests*)
          (cond ((eq old (second spec))
                 ;; test-function
                 (setf (second spec) new-value))
                ((eq old (third spec))
                 ;; hash-function
                 (setf (third spec) new-value))))))

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
  #!+sb-doc
  "Return true if name has a global function definition."
  (declare (explicit-check))
  (let ((fdefn (find-fdefn name)))
    (and fdefn (fdefn-fun fdefn) t)))

(defun fmakunbound (name)
  #!+sb-doc
  "Make NAME have no global function definition."
  (declare (explicit-check))
  (with-single-package-locked-error
      (:symbol name "removing the function or macro definition of ~A")
    (let ((fdefn (find-fdefn name)))
      (when fdefn
        (fdefn-makunbound fdefn)))
    (undefine-fun-name name)
    name))
