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

;;; an object suitable for input to standard functions that accept
;;; "environment objects" (of the ANSI glossary)
(def!type lexenv-designator () '(or abstract-lexenv null))

(defvar *policy*)

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
