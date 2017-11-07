;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-COLD")

#+#.(cl:if (cl:find-package "SB-POSIX") '(and) '(or))
(defun parallel-make-host-1 (max-jobs)
  (let ((subprocess-count 0)
        (subprocess-list nil))
    (flet ((wait ()
             (multiple-value-bind (pid status) (sb-posix:wait)
               (format t "~&; Subprocess ~D exit status ~D~%"  pid status)
               (setq subprocess-list (delete pid subprocess-list)))
             (decf subprocess-count)))
      (do-stems-and-flags (stem flags)
        (unless (position :not-host flags)
          (when (>= subprocess-count max-jobs)
            (wait))
          (let ((pid (sb-posix:fork)))
            (when (zerop pid)
              (in-host-compilation-mode
               (lambda () (compile-stem stem flags :host-compile)))
              ;; FIXME: convey exit code based on COMPILE result.
              (sb-cold::exit-process 0))
            (push pid subprocess-list)
            (incf subprocess-count)
            ;; Do not wait for the compile to finish. Just load as source.
            (let ((source (merge-pathnames (stem-remap-target stem)
                                           (make-pathname :type "lisp"))))
              (let ((sb-ext:*evaluator-mode* :interpret))
                (in-host-compilation-mode
                 (lambda ()
                   (load source :verbose t :print nil))))))))
      (loop (if (plusp subprocess-count) (wait) (return)))))

  ;; We want to load compiled files, because that's what this function promises.
  ;; Reloading is tricky because constructors for interned ctypes will construct
  ;; new objects via their LOAD-TIME-VALUE forms, but globaldb already stored
  ;; some objects from the interpreted pre-load.
  ;; So wipe everything out that causes problems down the line.
  ;; (Or perhaps we could make their effects idempotent)
  (format t "~&; Parallel build: Clearing globaldb~%")
  (do-all-symbols (s)
    (when (get s :sb-xc-globaldb-info)
      (remf (symbol-plist s) :sb-xc-globaldb-info)))
  (fill (symbol-value 'sb!impl::*info-types*) nil)
  (clrhash (symbol-value 'sb!kernel::*forward-referenced-layouts*))
  (setf (symbol-value 'sb!kernel:*type-system-initialized*) nil)
  (makunbound 'sb!c::*backend-primitive-type-names*)
  (makunbound 'sb!c::*backend-primitive-type-aliases*)

  (format t "~&; Parallel build: Reloading compilation artifacts~%")
  ;; Now it works to load fasls.
  (in-host-compilation-mode
   (lambda ()
     (handler-bind ((sb-kernel:redefinition-warning #'muffle-warning))
       (do-stems-and-flags (stem flags)
         (unless (position :not-host flags)
           (load (stem-object-path stem flags :host-compile)
                 :verbose t :print nil))))))
  (format t "~&; Parallel build: Fasl loading complete~%"))

;;; Either load or compile-then-load the cross-compiler into the
;;; cross-compilation host Common Lisp.
(defun load-or-cload-xcompiler (load-or-cload-stem)
  (declare (type function load-or-cload-stem))
  ;; Build a version of Python to run in the host Common Lisp, to be
  ;; used only in cross-compilation.
  ;;
  ;; Note that files which are marked :ASSEM, to cause them to be
  ;; processed with SB!C:ASSEMBLE-FILE when we're running under the
  ;; cross-compiler or the target lisp, are still processed here, just
  ;; with the ordinary Lisp compiler, and this is intentional, in
  ;; order to make the compiler aware of the definitions of assembly
  ;; routines.
  (if (and (make-host-1-parallelism)
           (eq load-or-cload-stem #'host-cload-stem))
      (funcall (intern "PARALLEL-MAKE-HOST-1" 'sb-cold)
               (make-host-1-parallelism))
      (do-stems-and-flags (stem flags)
        (unless (find :not-host flags)
          (funcall load-or-cload-stem stem flags)
          #!+sb-show (warn-when-cl-snapshot-diff *cl-snapshot*))))

  ;; If the cross-compilation host is SBCL itself, we can use the
  ;; PURIFY extension to freeze everything in place, reducing the
  ;; amount of work done on future GCs. In machines with limited
  ;; memory, this could help, by reducing the amount of memory which
  ;; needs to be juggled in a full GC. And it can hardly hurt, since
  ;; (in the ordinary build procedure anyway) essentially everything
  ;; which is reachable at this point will remain reachable for the
  ;; entire run.
  ;;
  ;; (Except that purifying actually slows down GENCGC). -- JES, 2006-05-30
  #+(and sbcl (not gencgc))
  (sb-ext:purify)

  (values))
