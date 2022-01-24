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

;;; FIXME: I think it's a mistake that we load muffler twice in
;;; make-host-2 (once for the host, once for XC), because the host
;;; should produce no new warnings, and because it's really hard
;;; to think straight when you figure that we're using the host's
;;; SIGNAL and type system but mixing it with our types.
;;; We can just bake in some behavior to the cross-compiler never to warn
;;; about sh*t that we think isn't warning-worthy.
;;; (i.e. do it in source code using #[-+]sb-xc-host).
;;; The target compiler will still get everything as usual.
(let ((*features* (cons :sb-xc *features*)))
  (load (sb-cold:find-bootstrap-file "^muffler")))

;;; Ordinarily the types carried around as "handled conditions" while compiling
;;; have been parsed into internal CTYPE objects. However, using parsed objects
;;; in the cross-compiler was confusing as hell.
;;; Consider any toplevel form in make-host-2 - it will have constants in it,
;;; and we need to know if each constant is dumpable. So we call DUMPABLE-LEAFLIKE-P
;;; which invokes SB-XC:TYPEP. But SB-XC:TYPEP may know nothing of DEBUG-NAME-MARKER
;;; until that DEFSTRUCT is seen. So how did it ever work? Well, for starters,
;;; if it's an unknown type, we need to signal a PARSE-UNKNOWN-TYPE condition.
;;; To signal that, we check whether that condition is in *HANDLED-CONDITIONS*.
;;; The way we tested that is to unparse the entry and then use CL:TYPEP on the
;;; specifier.  (rev 2bad5ce54d5692a0 "Represent LEXENV-HANDLED-CONDITIONS as CTYPEs")
;;; So we had:
;;;   (HANDLE-CONDITION-P #<PARSE-UNKNOWN-TYPE {1003172603}>)
;;;   -> (UNION-UNPARSE-TYPE-METHOD #<UNION-TYPE (OR (SATISFIES UNABLE-TO-OPTIMIZE-NOTE-P) ...)>)
;;;   -> (TYPE= #<UNION-TYPE (OR (SATISFIES UNABLE-TO-OPTIMIZE-NOTE-P) ...)> #<UNION-TYPE LIST>)
;;;   -> ... lots more frames ...
;;;   -> (CTYPEP NIL #<HAIRY-TYPE (SATISFIES UNABLE-TO-OPTIMIZE-NOTE-P)>)
;;; While means while merely unparsing, we have to reason about UNION-TYPE
;;; and UNKNOWN-TYPE, which might entail invoking (CROSS-TYPEP 'NIL #<an-unknown-type>)
;;; i.e. we can't even unparse a parsed type without reasoning about whether
;;; we should signal a condition about the condition we're trying to signal.
;;; That could only be described as an unmitigated disaster.
;;; So now, as a special case within the cross-compiler, *HANDLED-CONDITIONS*
;;; uses type designators instead of parsed types.
(setq sb-c::*handled-conditions*
      `(((or (satisfies unable-to-optimize-note-p)
             sb-ext:code-deletion-note)
         . muffle-warning)))

(defun proclaim-target-optimization ()
  ;; The difference between init'ing the XC policy vs just proclaiming
  ;; is that INIT makes the settings stick in the baseline policy,
  ;; which affects POLICY-COLD-INIT-OR-RESANIFY.
  (sb-c::init-xc-policy (if (member :cons-profiling sb-xc:*features*)
                            '((sb-c::instrument-consing 2))
                            '()))
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

(read-undefined-fun-allowlist)
(defun parallel-make-host-2 (max-jobs)
  (let ((subprocess-count 0)
        (subprocess-list nil)
        stop)
    (labels ((wait ()
               (multiple-value-bind (pid status) (sb-cold::posix-wait)
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
               (with-open-file (f (format nil "output/~d.~a" pid logsuffix))
                 (loop (let ((line (read-line f nil)))
                         (unless line (return))
                         (write-string line)
                         (terpri)))
                 (delete-file f))))
      #+sbcl (host-sb-ext:disable-debugger)
      (sb-cold::with-subprocesses
        (unwind-protect
             (do-stems-and-flags (stem flags 2)
               (unless (position :not-target flags)
                 (when (>= subprocess-count max-jobs)
                   (wait))
                 (when stop
                   (return))
                 (let ((pid (sb-cold::posix-fork)))
                   (when (zerop pid)
                     (let ((pid (sb-cold::getpid)))
                       (let ((*standard-output*
                              (open (format nil "output/~d.out" pid)
                                    :direction :output :if-exists :supersede))
                             (*error-output*
                              (open (format nil "output/~d.err" pid)
                                    :direction :output :if-exists :supersede)))
                         (handler-case (target-compile-stem stem flags)
                           (error (e)
                             (format *error-output* "~a~%" e)
                             (close *standard-output*)
                             (close *error-output*)
                             (sb-cold::exit-subprocess 1))
                           (:no-error (res)
                             (declare (ignore res))
                             (delete-file *standard-output*)
                             (delete-file *error-output*)
                             (sb-cold::exit-subprocess 0))))))
                   (push (cons pid stem) subprocess-list))
                 (incf subprocess-count)
                 ;; Cause the compile-time effects from this file
                 ;; to appear in subsequently forked children.
                 (let ((*compile-for-effect-only* t))
                   (target-compile-stem stem flags))))
          (loop (if (plusp subprocess-count) (wait) (return)))
          (when stop
            (sb-cold::exit-process 1))))
      (values))))

;;; See whether we're in individual file mode
(cond
  ((boundp 'cl-user::*compile-files*)
   (let ((files
          (mapcar (lambda (x) (concatenate 'string "src/" x))
                  (symbol-value 'cl-user::*compile-files*))))
     (with-compilation-unit ()
       (do-stems-and-flags (stem flags 2)
         (unless (position :not-target flags)
           (let* ((*compile-for-effect-only* (not (member stem files :test #'string=)))
                  (sb-xc:*compile-print* (not *compile-for-effect-only*)))
             (target-compile-stem stem flags)))))))
  (t
   ;; Actually compile
   (let ((sb-xc:*compile-print* nil))
     (if (make-host-2-parallelism)
         (funcall 'parallel-make-host-2 (make-host-2-parallelism))
         (let ((total-files
                (count-if (lambda (x) (not (find :not-target (cdr x))))
                          (get-stems-and-flags 2)))
               (total-time 0)
               (n 0)
               (sb-xc:*compile-verbose* nil))
           ;; Workaround memory exhaustion in SB-FASTEVAL.
           ;; In SB-EVAL the default evaluator-mode is :compile,
           ;; but it also would exhaust memory if interpreting.
           #+sbcl (setq host-sb-ext:*evaluator-mode* :compile)
           (with-math-journal
            (do-stems-and-flags (stem flags 2)
              (unless (position :not-target flags)
                (format t "~&[~3D/~3D] ~40A" (incf n) total-files (stem-remap-target stem))
                (let ((start (get-internal-real-time)))
                  (target-compile-stem stem flags)
                  (let ((elapsed (/ (- (get-internal-real-time) start)
                                    internal-time-units-per-second)))
                    (format t " (~f sec)~%" elapsed)
                    (incf total-time elapsed)))
                ;; The specialized array registry has file-wide scope. Hacking that aspect
                ;; into the xc build scaffold seemed slightly easier than hacking the
                ;; compiler (i.e. making the registry a slot of the fasl-output struct)
                (clear-specialized-array-registry)))
             (format t "~&~50t ~f~%" total-time))
           (sb-c::dump/restore-interesting-types 'write)))
     (sb-kernel::write-structure-definitions-as-text
      (sb-cold:stem-object-path "defstructs.lisp-expr"
                                '(:extra-artifact) :target-compile)))))
