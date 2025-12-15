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

;;; The vector of source PATHS is not too bad in terms of space consumption
;;; due to all the sharing that occurs. Each vector element consumes typically at most
;;; 1 cons cell, because it extends a prior element by 1 path component, e.g.
;;;   :PATHS #((0) #1=(1) #2=(3 . #1#) #3=(1 . #2#) (1 . #3#) (2 . #3#)
;;;            (:THEN . #3#) #4=(2 . #2#) (1 . #4#) (2 . #4#) (:ELSE . #3#)
;;;            #5=(3 . #2#) #6=(2 . #5#) #7=(1 . #6#) #8=(0 . #9=(1 . #7#))
;;;            (1 . #8#) (2 . #8#) (:THEN . #8#) #10=(1 . #9#) #11=(1 . #10#) ...)
(defstruct (covered-file
             (:constructor make-coverage-instrumented-file
                           (paths &aux (executed (make-array (length paths)
                                                             :element-type 'bit))))
             (:copier nil) (:predicate nil))
  (executed #* :type simple-bit-vector :read-only t)
  (paths #() :type simple-vector :read-only t))

(defknown %mark-covered (cons) t (always-translatable))

;;; Check the policy for whether we should generate code coverage
;;; instrumentation. If not, just return the original START
;;; ctran. Otherwise insert code coverage instrumentation after
;;; START, and return the new ctran.
(defun instrument-coverage (start mode form
                            &aux (code-coverage-records (coverage-records *compilation*)))
  ;; We don't actually use FORM for anything, it's just convenient to
  ;; have around when debugging the instrumentation.
  (declare (ignore form))
  (if (and *allow-instrumenting*
           code-coverage-records
           (policy *lexenv* (> store-coverage-data 0)))
      (let ((path (source-path-original-source *current-path*)))
        (when mode
          (push mode path))
        (let* ((block (ctran-block start))
               (source-path-marks (block-source-path-marks block)))
          ;; If this source path has already been instrumented in this
          ;; block, don't instrument it again.
          (if (and source-path-marks (gethash path source-path-marks))
              start
              (let ((next (make-ctran))
                    (*allow-instrumenting* nil))
                (setf (gethash path code-coverage-records) t)
                (unless source-path-marks
                  (setf source-path-marks (make-hash-table :test 'equal)
                        (block-source-path-marks block) source-path-marks))
                (setf (gethash path source-path-marks) t)
                (ir1-convert start next nil `(%mark-covered ',path))
                next))))
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

;;; Emit a coverage mark for NODE if there is a possibility that
;;; marking a previous coverage mark does not entail NODE's
;;; evaluation. We scan backwards in the block, looking for any
;;; intervening combinations between this node and another
;;; %MARK-COVERED. If the intervening combination is a combination for
;;; a LET or ASSIGNMENT lambda, then we don't need to emit a coverage
;;; mark, as the control flow has been made explicit in the CFG.
;;; KLUDGE: Technically we should stick with merging strictly adjacent
;;; emitted coverage marks in the IR2, as that would make coverage
;;; instrumentation interrupt safe as well. However, doing it this way
;;; produces much less instrumentation.
(defoptimizer (%mark-covered ir2-convert) ((path) node block)
  (aver (constant-lvar-p path))
  (do ((prev (ctran-use (node-prev node))
             (ctran-use (node-prev prev)))
       (start-node (block-start-node (ir2-block-block block))))
      ((eq prev start-node))
    (when (basic-combination-p prev)
      (let ((kind (basic-combination-kind prev)))
        (when (case kind
                (:known
                 (when (eq (lvar-fun-name (basic-combination-fun prev) t)
                           '%mark-covered)
                   (return))
                 t)
                (:local
                 (not (functional-somewhat-letlike-p (combination-lambda prev))))
                (t t))
          (setf (ir2-block-covered-paths-ref block)
                (list nil))))))
  (let ((2comp (component-info (node-component node))))
    (unless (car (ir2-block-covered-paths-ref block))
      (vop mark-covered node block
           (vector-push-extend (ir2-block-covered-paths-ref blocK)
                               (ir2-component-coverage-map 2comp))))
    ;; Duplicate form paths could sneak in, but repetition is meaningless
    (pushnew (lvar-value path) (car (ir2-block-covered-paths-ref block)) :test 'equal)
    (values)))

#-sb-xc-host
(defun code-coverage-map (code &aux (n (code-header-words code)))
  (declare (type sb-kernel:code-component code))
  (or (let ((map (code-header-ref code (1- n))))
        (when (typep map '(cons (eql coverage-map))) (cdr map)))
      (and (= code-boxed-words-align 2) ; try the next-to-last slot
           (let ((map (code-header-ref code (- n 2))))
             (when (typep map '(cons (eql coverage-map))) (cdr map))))))
