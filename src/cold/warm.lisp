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

(assert (zerop (deref (extern-alien "lowtag_for_widetag" (array char 64))
                      (ash sb-vm:character-widetag -2))))

(proclaim '(optimize (compilation-speed 1)
                     (debug #+sb-show 2 #-sb-show 1)
                     (safety 2)
                     (space 1)
                     (speed 2)))

;;; Assert that genesis preserved shadowing symbols.
(let ((p sb-assem::*backend-instruction-set-package*))
  (unless (eq p (find-package "SB-VM"))
    (dolist (expect '("SEGMENT" "MAKE-SEGMENT"))
      (assert (find expect (package-shadowing-symbols p) :test 'string=)))))


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
(let ((sources (with-open-file (f "build-order.lisp-expr")
                 (let ((*features* (cons :warm-build-phase *features*)))
                   (read f))))
      (sb-c::*handled-conditions* sb-c::*handled-conditions*))
 (declare (special *compile-files-p*))
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
                   ;; Specifying the directory name for :OUTPUT-FILE is enough.
                   ;; It does the right thing. (Does it work on Windows? I hope so)
                   (concatenate
                     'string sb-fasl::*!target-obj-prefix*
                     ;; OR: (namestring (make-pathname :directory (pathname-directory stem)))
                     (subseq stem 0 (1+ (position #\/ stem :from-end t)))))))
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

  (let ((*print-length* 10)
        (*print-level* 5)
        (*print-circle* t)
        (*compile-print* nil))
    (dolist (group sources)
      (with-compilation-unit () (do-srcs group))))))
