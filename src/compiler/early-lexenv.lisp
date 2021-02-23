;;;; This file contains early compiler-related structure definitions.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

(defconstant-eqx +policy-primary-qualities+
        #(;; ANSI standard qualities
          compilation-speed
          debug
          safety
          space
          speed
          ;; SBCL extensions
          ;;
          ;; FIXME: INHIBIT-WARNINGS is a misleading name for this.
          ;; Perhaps BREVITY would be better. But the ideal name would
          ;; have connotations of suppressing not warnings but only
          ;; optimization-related notes, which is already mostly the
          ;; behavior, and should probably become the exact behavior.
          ;; Perhaps INHIBIT-NOTES?
          inhibit-warnings)
    #'equalp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant n-policy-primary-qualities (length +policy-primary-qualities+))
  ;; 1 bit per quality is stored to indicate whether it was explicitly given
  ;; a value in a lexical policy. In addition to the 5 ANSI-standard qualities,
  ;; SBCL defines one more "primary" quality and 16 dependent qualities.
  ;; Both kinds take up 1 bit in the mask of specified qualities.
  (defconstant max-policy-qualities 32))

;; Each primary and dependent quality policy is assigned a small integer index.
;; The POLICY struct represents a set of policies in an order-insensitive way
;; that facilitates quicker lookup than scanning an alist.
(defstruct (policy (:constructor make-policy
                       (primary-qualities &optional
                                          presence-bits dependent-qualities)))
  ;; Mask with a 1 for each quality that has an explicit value in this policy.
  ;; Primary qualities fill the mask from left-to-right and dependent qualities
  ;; from right-to-left.
  ;; xc has trouble folding this MASK-FIELD, but it works when host-evaluated.
  (presence-bits #.(mask-field
                    (byte n-policy-primary-qualities
                          (- max-policy-qualities n-policy-primary-qualities))
                    -1)
                 :type (unsigned-byte #.max-policy-qualities))
  ;; For efficiency, primary qualities are segregated because there are few
  ;; enough of them to fit in a fixnum.
  (primary-qualities 0 :type (unsigned-byte #.(* 2 n-policy-primary-qualities)))
  ;; 2 bits per dependent quality is a fixnum on 64-bit build, not on 32-bit.
  ;; It would certainly be possible to constrain this to storing exactly
  ;; the 16 currently defined dependent qualities,
  ;; but that would be overly limiting.
  (dependent-qualities 0
   :type (unsigned-byte #.(* (- max-policy-qualities n-policy-primary-qualities)
                             2))))
(declaim (freeze-type policy))

(defvar *handled-conditions* nil)
(defvar *disabled-package-locks* nil)

;;; The LEXENV represents the lexical environment used for IR1 conversion.
;;; (This is also what shows up as an ENVIRONMENT value in macroexpansion.)
(declaim (inline internal-make-lexenv))
(defstruct (lexenv
            (:include abstract-lexenv)
             (:print-function
              (lambda (lexenv stream depth)
                (if (null-lexenv-p lexenv)
                    (print-unreadable-object (lexenv stream)
                      (write-string "NULL-LEXENV" stream))
                    (default-structure-print lexenv stream depth))))
             (:copier nil)
             (:constructor make-null-lexenv ())
             (:constructor make-almost-null-lexenv (%policy handled-conditions
                                                    flushable lambda parent))
             (:constructor make-package-lock-lexenv
                           (disabled-package-locks %policy
                            &aux (handled-conditions nil)))
             (:constructor internal-make-lexenv
                           (funs vars blocks tags
                            type-restrictions
                            flushable
                            lambda cleanup handled-conditions
                            disabled-package-locks %policy user-data
                            parent)))
  ;; an alist of (NAME . WHAT), where WHAT is either a FUNCTIONAL (a
  ;; local function), a DEFINED-FUN, representing an
  ;; INLINE/NOTINLINE declaration, or a list (MACRO . <function>) (a
  ;; local macro, with the specifier expander). Note that NAME may be
  ;; a (SETF <name>) list, not necessarily a single symbol.
  (funs nil :type list)
  ;; an alist translating variable names to LEAF structures. A special
  ;; binding is indicated by a :SPECIAL GLOBAL-VAR leaf. Each special
  ;; binding within the code gets a distinct leaf structure, as does
  ;; the current "global" value on entry to the code compiled.
  ;; (locally (special ...)) is handled by adding the most recent
  ;; special binding to the front of the list.
  ;;
  ;; If the CDR is (MACRO . <exp>), then <exp> is the expansion of a
  ;; symbol macro.
  (vars nil :type list)
  ;; BLOCKS and TAGS are alists from block and go-tag names to 2-lists
  ;; of the form (<entry> <continuation>), where <continuation> is the
  ;; continuation to exit to, and <entry> is the corresponding ENTRY
  ;; node.
  (blocks nil :type list)
  (tags nil :type list)
  ;; an alist (THING . CTYPE) which is used to keep track of
  ;; "pervasive" type declarations. When THING is a leaf, this is for
  ;; type declarations that pertain to the type in a syntactic extent
  ;; which does not correspond to a binding of the affected name.
  (type-restrictions nil :type list)
  ;; the lexically enclosing lambda, if any
  ;;
  ;; FIXME: This should be :TYPE (OR CLAMBDA NULL), but it was too hard
  ;; to get CLAMBDA defined in time for the cross-compiler.
  (lambda nil)
  ;; the lexically enclosing cleanup, or NIL if none enclosing within LAMBDA
  (cleanup nil)
  ;; condition types we handle with a handler around the compiler
  (handled-conditions *handled-conditions*)
  ;; lexically disabled package locks (list of symbols)
  (disabled-package-locks *disabled-package-locks*)
  ;; the current OPTIMIZE policy. this is null in the null environment,
  ;; and the global policy is stored in *POLICY*. (Because we want to
  ;; be able to affect it from :WITH-COMPILATION-UNIT.) NIL here also
  ;; works as a convenient null-lexenv identifier.
  (%policy nil :type (or null policy))
  ;; A list associating extra user info to symbols.  The entries
  ;; are of the form (:declare name . value),
  ;; (:variable name key . value), or (:function name key . value)
  (user-data nil :type list)
  (parent nil)
  ;; Cache of all visible variables, including the ones coming from
  ;; (call-lexenv lambda)
  ;; Used for LEAF-VISIBLE-TO-DEBUGGER-P
  (var-cache nil :type (or null hash-table))
  ;; A list of functions that can be removed when unused.
  ;; Similar to the FLUSHABLE attribute in DEFKNOWN, but can applied
  ;; locally to things that are generally not flushable but can be
  ;; flushed in some circumstances.
  (flushable nil :type list))

;;; the lexical environment we are currently converting in
(defvar *lexenv*)
(declaim (type lexenv *lexenv*))

;;; an object suitable for input to standard functions that accept
;;; "environment objects" (of the ANSI glossary)
(def!type lexenv-designator () '(or abstract-lexenv null))

(defvar *policy*)
(defun lexenv-policy (lexenv)
  (or (lexenv-%policy lexenv) *policy*))

(defun null-lexenv-p (lexenv)
  (not (lexenv-%policy lexenv)))


;;; translation from template names to template structures
(defglobal *backend-template-names* (make-hash-table)) ; keys are symbols
(declaim (type hash-table *backend-template-names*))

;;; When compiling the cross-compiler, a %VOP-EXISTS-P result could depend on
;;; the build order. Usually it will not, because the decision to use a vop is
;;; typically made in a transform, so the query occurs only when a transform runs.
;;; However, sometimes the existsp check is performed to decide whether or not
;;; to define a function or other transform. In that case the existsp check is
;;; sensitive to the order of vop definitions.  Such uses will often occur inside
;;; a "#." so that the defining form remains toplevel.
;;; If called with OPTIMISTIC = T then we're trying to return NIL or T
;;; or the EXISTSP macroexpander.
#+sb-xc-host
(progn
  (defvar *vop-not-existsp* nil)
  ;;; This function is invoked after compiling the cross-compiler
  ;;; before quitting the image, and when loading it from compiled fasls
  ;;; (because toplevel forms might use %VOP-EXISTSP at any time).
  (defun check-vop-existence-correctness ()
    (dolist (entry *vop-not-existsp*)
      (assert (not (%vop-existsp (car entry) (cdr entry))))))
  (defun %vop-existsp (name query &optional optimistic)
    (declare (notinline info fun-info-templates))
    (let ((answer
           (not (null (ecase query
                        (:named
                         (gethash name *backend-template-names*))
                        (:translate
                         (awhen (info :function :info name)
                           (fun-info-templates it))))))))
      ;; Negatives won't be stored in the journal in optimistic mode.
      (when (and (not answer) (not optimistic))
        (pushnew (cons name query) *vop-not-existsp* :test 'equal))
      answer)))

(defmacro vop-existsp (query name)
  #+sb-xc-host
  (cond ((%vop-existsp name query t)
         ;;(format t "~&VOP-EXISTSP ~s ~s: Yes~%" name query)
         t)
        (t
         ;;(format t "~&VOP-EXISTSP ~s ~s: DEFER~%" name query)
         `(%vop-existsp ',name ,query)))
  ;; When running the cross-compiler, all the inquiries to VOP-EXISTSP have
  ;; definitive answers, so this never defers.
  ;; We use the version of %VOP-EXISTSP that was built in to the host.
  #-sb-xc-host
  (funcall '%vop-existsp name query))

;;; For situations where you want to write (IF (VOP-EXISTSP ...) (THEN) (ELSE))
;;; but at least one of (THEN) or (ELSE) contains code that can't be macroexpanded
;;; or compiled, as may occur with (VOP* ...), use a different macro that never
;;; defers. Correctness of the result requires that the vop be defined in time.
(defmacro if-vop-existsp ((query name) then &optional else)
  (if (funcall '%vop-existsp name query) then else))
(defmacro when-vop-existsp ((query name) &rest body)
  (if (funcall '%vop-existsp name query) `(progn ,@body)))
(defmacro unless-vop-existsp ((query name) &rest body)
  (if (not (funcall '%vop-existsp name query)) `(progn ,@body)))
