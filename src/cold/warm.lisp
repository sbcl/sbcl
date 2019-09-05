;;;; "warm initialization": initialization which comes after cold init

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "COMMON-LISP-USER")

;;;; general warm init compilation policy

;;;; Use the same settings as PROCLAIM-TARGET-OPTIMIZATION
;;;; I could not think of a trivial way to ensure that this stays functionally
;;;; identical to the corresponding code in 'compile-cold-sbcl'.
;;;; (One possibility would be to read this form from a lisp-expr file)
;;;; The intent is that we should generate identical code if a file is moved
;;;; from the cross-compiled sources to warm-compiled or vice-versa.
(proclaim '(optimize
            #+sb-show (debug 2)
            (safety 2) (speed 2)
            ;; never insert stepper conditions
            (sb-c:insert-step-conditions 0)
            (sb-c:alien-funcall-saves-fp-and-pc #+x86 3 #-x86 0)))

(locally
    (declare (notinline find-symbol)) ; don't ask
  (let ((s (find-symbol "*/SHOW*" "SB-INT")))
  ;; If you made it this far, chances are that you no longer wish to see
  ;; whatever it is that show would have shown. Comment this out if you need.
    (when s (set s nil))))

(assert (zerop (deref (extern-alien "lowtag_for_widetag" (array char 64))
                      (ash sb-vm:character-widetag -2))))
(gc :full t)

;;; Verify that all defstructs except for one were compiled in a null lexical
;;; environment. Compiling any call to a structure constructor would like to
;;; know whether some slots get their default value especially if the default
;;; is incompatible with the slot type (consider MISSING-ARG, e.g).
;;; If some initform was compiled in a non-null environment, it might not refer
;;; to a global function. We'd rather ignore it than incorrectly style-warn.
(let (result)
  (do-all-symbols (s)
    (let ((dd (sb-kernel:find-defstruct-description s nil)))
      (when (and dd (not (sb-kernel::dd-null-lexenv-p dd)))
        (push (sb-kernel:dd-name dd) result))))
  (assert (equal result '(sb-c::conset))))

;;; Assert that genesis preserved shadowing symbols.
(let ((p sb-assem::*backend-instruction-set-package*))
  (unless (eq p (find-package "SB-VM"))
    (dolist (expect '("SEGMENT" "MAKE-SEGMENT"))
      (assert (find expect (package-shadowing-symbols p) :test 'string=)))))

;;; Verify that compile-time floating-point math matches load-time.
(defvar *compile-files-p*)
(when (or (not (boundp '*compile-files-p*)) *compile-files-p*)
  (with-open-file (stream "float-math.lisp-expr" :if-does-not-exist nil)
    (when stream
      (format t "; Checking ~S~%" (pathname stream))
      ;; Ensure that we're reading the correct variant of the file
      ;; in case there is more than one set of floating-point formats.
      (assert (eq (read stream) :default))
      (let ((*package* (find-package "SB-KERNEL")))
        (dolist (expr (read stream))
          (destructuring-bind (fun args result) expr
            (let ((actual (apply fun (sb-int:ensure-list args))))
              (unless (eql actual result)
                (#+sb-devel cerror #+sb-devel ""
                 #-sb-devel format #-sb-devel t
                 "FLOAT CACHE LINE ~S vs COMPUTED ~S~%"
                 expr actual)))))))))


;;;; compiling and loading more of the system

;;; FIXME: CMU CL's pclcom.lisp had extra optional stuff wrapped around
;;; COMPILE-PCL, at least some of which we should probably have too:
;;;
;;; (with-compilation-unit
;;;     (:optimize '(optimize (debug #+(and (not high-security) small) .5
;;;                               #-(or high-security small) 2
;;;                               #+high-security 3)
;;;                        (speed 2) (safety #+(and (not high-security) small) 0
;;;                                          #-(or high-security small) 2
;;;                                          #+high-security 3)
;;;                        (inhibit-warnings 2))
;;;      :optimize-interface '(optimize-interface #+(and (not high-security) small)
;;; (safety 1)
;;;                                            #+high-security (safety 3))
;;;      :context-declarations
;;;      '((:external (declare (optimize-interface (safety #-high-security 2 #+high-
;;; security 3)
;;;                                             (debug #-high-security 1 #+high-s
;;; ecurity 3))))
;;;     ((:or :macro (:match "$EARLY-") (:match "$BOOT-"))
;;;     (declare (optimize (speed 0))))))
;;;
(let ((sources (with-open-file (f (merge-pathnames "../../build-order.lisp-expr"
                                                   *load-pathname*))
                 (read f) ; skip over the make-host-{1,2} input files
                 (read f)))
      (sb-c::*handled-conditions* sb-c::*handled-conditions*))
 (proclaim '(sb-ext:muffle-conditions compiler-note))
 (flet ((do-srcs (list)
         (dolist (stem list)
          ;; Do like SB-COLD::LPNIFY-STEM for consistency, though parse/xlate/unparse
          ;; would probably also work. I don't think that's better.
          (let ((fullname (sb-int:logically-readonlyize
                           (format nil "SYS:~:@(~A~).LISP" (substitute #\; #\/ stem))
                           ;; indicate shareable string even if not dumped as
                           ;; a literal (when compiling in the LOAD step)
                           t))
                (output
                  (compile-file-pathname stem
                   :output-file
                   (merge-pathnames
                    (concatenate
                     'string sb-fasl::*!target-obj-prefix*
                     (subseq stem 0 (1+ (position #\/ stem :from-end t))))))))
           (flet ((report-recompile-restart (stream)
                    (format stream "Recompile file ~S" stem))
                  (report-continue-restart (stream)
                    (format stream "Continue, using possibly bogus file ~S" output)))
             (tagbody
              retry-compile-file
                (multiple-value-bind (output-truename warnings-p failure-p)
                    (ecase (if (boundp '*compile-files-p*) *compile-files-p* t)
                     ((t)   (let ((sb-c::*source-namestring* fullname))
                              (ensure-directories-exist output)
                              (compile-file stem :output-file output)))
                     ((nil) output))
                  (declare (ignore warnings-p))
                  (cond ((not output-truename)
                         (error "COMPILE-FILE of ~S failed." stem))
                        (failure-p
                         (unwind-protect
                              (restart-case
                                  (error "FAILURE-P was set when creating ~S."
                                         output-truename)
                                (recompile ()
                                  :report report-recompile-restart
                                  (go retry-compile-file))
                                (continue ()
                                  :report report-continue-restart
                                  (setf failure-p nil)))
                           ;; Don't leave failed object files lying around.
                           (when (and failure-p (probe-file output-truename))
                                 (delete-file output-truename)
                                 (format t "~&deleted ~S~%" output-truename))))
                        ;; Otherwise: success, just fall through.
                        (t nil))
                  (unless (handler-bind
                              ((sb-kernel:redefinition-with-defgeneric
                                #'muffle-warning))
                            (let ((sb-c::*source-namestring* fullname))
                              (load output-truename)))
                    (error "LOAD of ~S failed." output-truename))
                  (sb-int:/show "done loading" output-truename))))))))

  (let ((*compile-print* nil))
    (dolist (group sources)
      (handler-bind ((simple-warning
                      (lambda (c)
                        ;; escalate "undefined variable" warnings to errors.
                        ;; There's no reason to allow them in our code.
                        (when (search "undefined variable"
                                      (write-to-string c :escape nil))
                          (cerror "Finish warm compile ignoring the problem" c)))))
        (with-compilation-unit () (do-srcs group)))))))
