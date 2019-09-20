;;;; Compile the fundamental system sources (not CLOS, and possibly
;;;; not some other warm-load-only stuff like DESCRIBE) to produce
;;;; object files.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-COLD")

(let ((*features* (cons :sb-xc *features*)))
  (load "src/cold/muffler.lisp"))

;; Avoid forward-reference to an as-yet unknown type.
;; NB: This is not how you would write this function, if you required
;; such a thing. It should be (TYPEP X 'CODE-DELETION-NOTE).
;; Do as I say, not as I do.
(defun code-deletion-note-p (x)
  (eq (type-of x) 'sb-ext:code-deletion-note))
(setq sb-c::*handled-conditions*
      `((,(sb-kernel:specifier-type
           '(or (satisfies unable-to-optimize-note-p)
                (satisfies code-deletion-note-p)))
         . muffle-warning)))

(defun proclaim-target-optimization ()
  ;; The difference between init'ing the XC policy vs just proclaiming
  ;; is that INIT makes the settings stick in the baseline policy,
  ;; which affects POLICY-COLD-INIT-OR-RESANIFY.
  (sb-c::init-xc-policy #+cons-profiling '((sb-c::instrument-consing 2)))
  (sb-xc:proclaim
     `(optimize
       (compilation-speed 1)
       (debug ,(if (find :sb-show sb-xc:*features*) 2 1))
       (sb-ext:inhibit-warnings 2)
       ;; SAFETY = SPEED (and < 3) should provide reasonable safety,
       ;; but might skip some unreasonably expensive stuff
       (safety 2) (space 1) (speed 2)
       ;; sbcl-internal optimization declarations:
       ;;
       ;; never insert stepper conditions
       (sb-c:insert-step-conditions 0)
       ;; save FP and PC for alien calls -- or not
       (sb-c:alien-funcall-saves-fp-and-pc
        ,(if (find :x86 sb-xc:*features*) 3 0)))))

(defun in-target-cross-compilation-mode (fun)
  "Call FUN with everything set up appropriately for cross-compiling
   a target file."
  (let (;; In order to increase microefficiency of the target Lisp,
        ;; enable old CMU CL defined-function-types-never-change
        ;; optimizations. (ANSI says users aren't supposed to
        ;; redefine our functions anyway; and developers can
        ;; fend for themselves.)
        (sb-ext:*derive-function-types*
         (if (find :sb-fluid sb-xc:*features*) nil t))
        ;; Let the target know that we're the cross-compiler.
        (sb-xc:*features* (cons :sb-xc sb-xc:*features*))
        (*readtable* sb-cold:*xc-readtable*))
    ;; Control optimization policy.
    (proclaim-target-optimization)
    (funcall fun)))

(setf *target-compile-file* #'sb-xc:compile-file)
(setf *target-assemble-file* #'sb-c:assemble-file)
(setf *in-target-compilation-mode-fn* #'in-target-cross-compilation-mode)

;; Update the xc-readtable
(set-macro-character #\` #'sb-impl::backquote-charmacro nil *xc-readtable*)
(set-macro-character #\, #'sb-impl::comma-charmacro nil *xc-readtable*)
;; ... and since the cross-compiler hasn't seen a DEFMACRO for QUASIQUOTE,
;; make it think it has, otherwise it fails more-or-less immediately.
(setf (sb-xc:macro-function 'sb-int:quasiquote)
      (lambda (form env)
        (the sb-kernel:lexenv-designator env)
        (sb-impl::expand-quasiquote (second form) t)))

(setq sb-c::*track-full-called-fnames* :minimal) ; Change this as desired

#+#.(cl:if (cl:find-package "HOST-SB-POSIX") '(and) '(or))
(defun parallel-make-host-2 (max-jobs)
  (let ((subprocess-count 0)
        (subprocess-list nil)
        stop)
    (labels ((wait ()
               (multiple-value-bind (pid status) (host-sb-posix:wait)
                 (format t "~&; Subprocess ~D exit status ~D~%"  pid status)
                 (unless (zerop status)
                   (let ((stem (cdr (assoc pid subprocess-list))))
                     (format t "; File: ~a~%" stem)
                     (show-log pid "out" "; Standard output:")
                     (show-log pid "err" "; Error output:"))
                   (setf stop t))
                 (setq subprocess-list (delete pid subprocess-list :key #'car)))
               (decf subprocess-count))
             (show-log (pid logsuffix label)
               (format t "~a~%" label)
               (with-open-file (f (format nil "~d.~a" pid logsuffix))
                 (loop (let ((line (read-line f nil)))
                         (unless line (return))
                         (write-string line)
                         (terpri)))
                 (delete-file f))))
      (host-sb-ext:disable-debugger)
      (unwind-protect
           (do-stems-and-flags (stem flags 2)
             (unless (position :not-target flags)
               (when (>= subprocess-count max-jobs)
                 (wait))
               (when stop
                 (return))
               (let ((pid (host-sb-posix:fork)))
                 (when (zerop pid)
                   (let ((pid (host-sb-unix:unix-getpid)))
                     (let ((*standard-output*
                            (open (format nil "~d.out" pid)
                                  :direction :output :if-exists :supersede))
                           (*error-output*
                            (open (format nil "~d.err" pid)
                                  :direction :output :if-exists :supersede)))
                       (handler-case (target-compile-stem stem flags)
                         (error (e)
                           (close *standard-output*)
                           (close *error-output*)
                           (sb-cold::exit-process 1))
                         (:no-error (res)
                           (declare (ignore res))
                           (delete-file *standard-output*)
                           (delete-file *error-output*)
                           (sb-cold::exit-process 0))))))
                 (push (cons pid stem) subprocess-list))
               (incf subprocess-count)
               ;; Cause the compile-time effects from this file
               ;; to appear in subsequently forked children.
               (let ((*compile-for-effect-only* t))
                 (target-compile-stem stem flags))))
        (loop (if (plusp subprocess-count) (wait) (return)))
        (when stop
          (sb-cold::exit-process 1)))
      (values))))

;;; Actually compile
(let ((sb-xc:*compile-print* nil))
  (if (make-host-2-parallelism)
      (funcall 'parallel-make-host-2 (make-host-2-parallelism))
      (let ((total
             (count-if (lambda (x) (not (find :not-target (cdr x))))
                       (get-stems-and-flags 2)))
            (n 0)
            (sb-xc:*compile-verbose* nil))
        (with-math-journal
         (do-stems-and-flags (stem flags 2)
           (unless (position :not-target flags)
             (format t "~&[~D/~D] ~A" (incf n) total (stem-remap-target stem))
             (target-compile-stem stem flags)
             (terpri)))))))
