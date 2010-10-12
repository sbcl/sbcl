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

;;; This function is called by !COLD-INIT after the globaldb has been
;;; initialized, but before anything else. We need to install these
;;; fdefn objects into the globaldb before any top level forms run, or
;;; we will end up with two different fdefn objects being used for the
;;; same function name. *!INITIAL-FDEFN-OBJECTS* is set up by GENESIS.
(defvar *!initial-fdefn-objects*)
(defun !fdefn-cold-init ()
  (dolist (fdefn *!initial-fdefn-objects*)
    (setf (info :function :definition (fdefn-name fdefn)) fdefn)))

;;; Return the fdefn object for NAME. If it doesn't already exist and
;;; CREATE is non-NIL, create a new (unbound) one.
(defun fdefinition-object (name create)
  (declare (values (or fdefn null)))
  (legal-fun-name-or-type-error name)
  (let ((fdefn (info :function :definition name)))
    (if (and (null fdefn) create)
        (setf (info :function :definition name) (make-fdefn name))
        fdefn)))

(defun maybe-clobber-ftype (name)
  (unless (eq :declared (info :function :where-from name))
    (clear-info :function :type name)))

;;; Return the fdefinition of NAME, including any encapsulations.
;;; The compiler emits calls to this when someone tries to FUNCALL
;;; something. SETFable.
#!-sb-fluid (declaim (inline %coerce-name-to-fun))
(defun %coerce-name-to-fun (name)
  (let ((fdefn (fdefinition-object name nil)))
    (or (and fdefn (fdefn-fun fdefn))
        (error 'undefined-function :name name))))
(defun (setf %coerce-name-to-fun) (function name)
  (maybe-clobber-ftype name)
  (let ((fdefn (fdefinition-object name t)))
    (setf (fdefn-fun fdefn) function)))

(defun %coerce-callable-to-fun (callable)
  (if (functionp callable)
      callable
      (%coerce-name-to-fun callable)))

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

;;; Replace the definition of NAME with a function that binds NAME's
;;; arguments to a variable named ARG-LIST, binds name's definition
;;; to a variable named BASIC-DEFINITION, and evaluates BODY in that
;;; context. TYPE is whatever you would like to associate with this
;;; encapsulation for identification in case you need multiple
;;; encapsulations of the same name.
(defun encapsulate (name type body)
  (let ((fdefn (fdefinition-object name nil)))
    (unless (and fdefn (fdefn-fun fdefn))
      (error 'undefined-function :name name))
    ;; We must bind and close over INFO. Consider the case where we
    ;; encapsulate (the second) an encapsulated (the first)
    ;; definition, and later someone unencapsulates the encapsulated
    ;; (first) definition. We don't want our encapsulation (second) to
    ;; bind basic-definition to the encapsulated (first) definition
    ;; when it no longer exists. When unencapsulating, we make sure to
    ;; clobber the appropriate INFO structure to allow
    ;; basic-definition to be bound to the next definition instead of
    ;; an encapsulation that no longer exists.
    (let ((info (make-encapsulation-info type (fdefn-fun fdefn))))
      (setf (fdefn-fun fdefn)
            (named-lambda encapsulation (&rest arg-list)
              (declare (special arg-list))
              (let ((basic-definition (encapsulation-info-definition info)))
                (declare (special basic-definition))
                (eval body)))))))

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
  (let* ((fdefn (fdefinition-object name nil))
         (encap-info (encapsulation-info (fdefn-fun fdefn))))
    (declare (type (or encapsulation-info null) encap-info))
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
  (let ((fdefn (fdefinition-object name nil)))
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

(defun %set-fdefinition (name new-value)
  #!+sb-doc
  "Set NAME's global function definition."
  (declare (type function new-value) (optimize (safety 1)))
  (with-single-package-locked-error (:symbol name "setting fdefinition of ~A")
    (maybe-clobber-ftype name)

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
    (let ((fdefn (fdefinition-object name t)))
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
  (let ((fdefn (fdefinition-object name nil)))
    (and fdefn (fdefn-fun fdefn) t)))

(defun fmakunbound (name)
  #!+sb-doc
  "Make NAME have no global function definition."
  (with-single-package-locked-error
      (:symbol name "removing the function or macro definition of ~A")
    (let ((fdefn (fdefinition-object name nil)))
      (when fdefn
        (fdefn-makunbound fdefn)))
    (sb!kernel:undefine-fun-name name)
    name))
