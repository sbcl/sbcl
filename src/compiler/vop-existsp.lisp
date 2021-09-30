;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

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
;;; from the VOP-EXISTSP macroexpander, and if NIL then we decide later.
#-sb-xc
(progn
  (defvar *vop-not-existsp* nil)
  ;;; This function is invoked after compiling the cross-compiler
  ;;; before quitting the image, and when loading it from compiled fasls
  ;;; (because toplevel forms might use %VOP-EXISTSP at any time).
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
      answer))
  (defun check-vop-existence-correctness ()
    (dolist (entry *vop-not-existsp*)
      (assert (not (%vop-existsp (car entry) (cdr entry)))))))

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
(defmacro when-vop-existsp ((query name) &body body)
  (if (funcall '%vop-existsp name query) `(progn ,@body)))
(defmacro unless-vop-existsp ((query name) &body body)
  (if (not (funcall '%vop-existsp name query)) `(progn ,@body)))
