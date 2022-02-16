;;;; code coverage

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

(defknown %mark-covered (cons) t (always-translatable))

;;; Check the policy for whether we should generate code coverage
;;; instrumentation. If not, just return the original START
;;; ctran. Otherwise insert code coverage instrumentation after
;;; START, and return the new ctran.
(defun instrument-coverage (start mode form
                            &aux (metadata (coverage-metadata *compilation*)))
  ;; We don't actually use FORM for anything, it's just convenient to
  ;; have around when debugging the instrumentation.
  (declare (ignore form))
  (if (and metadata
           (policy *lexenv* (> store-coverage-data 0))
           *allow-instrumenting*)
      (let ((path (source-path-original-source *current-path*)))
        (when mode
          (push mode path))
        (if (member (ctran-block start)
                    (gethash path (code-coverage-blocks metadata)))
            ;; If this source path has already been instrumented in
            ;; this block, don't instrument it again.
            start
            (let ((next (make-ctran))
                  (*allow-instrumenting* nil))
              (setf (gethash path (code-coverage-records metadata)) t)
              (push (ctran-block start)
                    (gethash path (code-coverage-blocks metadata)))
              (ir1-convert start next nil `(%mark-covered ',path))
              next)))
      start))

;;; In contexts where we don't have a source location for FORM
;;; e.g. due to it not being a cons, but where we have a source
;;; location for the enclosing cons, use the latter source location if
;;; available. This works pretty well in practice, since many PROGNish
;;; macroexpansions will just directly splice a block of forms into
;;; some enclosing form with `(progn ,@body), thus retaining the
;;; EQness of the conses.
(defun maybe-instrument-progn-like (start forms form)
  (or (when (and *allow-instrumenting*
                 (not (get-source-path form)))
        (let ((*current-path* (get-source-path forms)))
          (when *current-path*
            (instrument-coverage start nil form))))
      start))

(defoptimizer (%mark-covered ir2-convert) ((path) node block)
  (aver (constant-lvar-p path))
  (let ((2comp (component-info (node-component node))))
    (unless (car (ir2-block-covered-paths-ref block))
      (vop mark-covered node block
           (vector-push-extend (ir2-block-covered-paths-ref blocK)
                               (ir2-component-coverage-map 2comp))))
    (push (lvar-value path) (car (ir2-block-covered-paths-ref block)))
    (values)))
