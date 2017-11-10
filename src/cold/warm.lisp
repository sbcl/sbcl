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
                     (inhibit-warnings 2)
                     (safety 2)
                     (space 1)
                     (speed 2)))

;;; Assert that genesis preserved shadowing symbols.
(let ((p sb-assem::*backend-instruction-set-package*))
  (unless (eq p (find-package "SB-VM"))
    (dolist (expect '("SEGMENT" "MAKE-SEGMENT"))
      (assert (find expect (package-shadowing-symbols p) :test 'string=)))))


;;;; compiling and loading more of the system

(load "src/cold/muffler.lisp")

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
(let ((early-srcs
              '("src/code/warm-error"
                "src/code/room" ; for MAP-ALLOCATED-OBJECTS
                ;; We re-nickname SB-SEQUENCE as SEQUENCE now.
                ;; It could be done in genesis, but not earlier,
                ;; since the host has a package of that name.
                "src/code/defpackage"))

      (interpreter-srcs
              #+sb-fasteval
              '("src/interpreter/macros"
                "src/interpreter/checkfuns"
                "src/interpreter/env"
                "src/interpreter/sexpr"
                "src/interpreter/special-forms"
                "src/interpreter/eval"
                "src/interpreter/debug"))
      (external-format-srcs
       (append '("src/code/external-formats/enc-ebcdic")
               #+sb-unicode
               '("src/code/external-formats/enc-cyr"
                 "src/code/external-formats/enc-dos"
                 "src/code/external-formats/enc-iso"
                 "src/code/external-formats/enc-win"
                 "src/code/external-formats/enc-mac"
                 "src/code/external-formats/mb-util"
                 "src/code/external-formats/enc-cn-tbl"
                 "src/code/external-formats/enc-cn"
                 "src/code/external-formats/enc-jpn-tbl"
                 "src/code/external-formats/enc-jpn"
                 "src/code/external-formats/enc-utf")))
       (pcl-srcs
              '(;; CLOS, derived from the PCL reference implementation
                ;;
                ;; This PCL build order is based on a particular
                ;; (arbitrary) linearization of the declared build
                ;; order dependencies from the old PCL defsys.lisp
                ;; dependency database.
                "src/pcl/macros"
                "src/pcl/compiler-support"
                "src/pcl/defclass"
                "src/pcl/defs"
                "src/pcl/fngen"
                "src/pcl/wrapper"
                "src/pcl/cache"
                "src/pcl/dlisp"
                "src/pcl/boot"
                "src/pcl/vector"
                "src/pcl/slots-boot"
                "src/pcl/combin"
                "src/pcl/dfun"
                "src/pcl/ctor"
                "src/pcl/braid"
                "src/pcl/dlisp3"
                "src/pcl/generic-functions"
                "src/pcl/slots"
                "src/pcl/init"
                "src/pcl/std-class"
                "src/pcl/cpl"
                "src/pcl/fsc"
                "src/pcl/methods"
                "src/pcl/fixup"
                "src/pcl/defcombin"
                "src/pcl/ctypes"
                "src/pcl/env"
                "src/pcl/documentation"
                "src/pcl/print-object"
                "src/pcl/precom1"
                "src/pcl/precom2"))
      (other-srcs
              '("src/code/setf-funs"
                "src/code/stubs"
                ;; miscellaneous functionality which depends on CLOS
                "src/code/late-condition"

                ;; CLOS-level support for the Gray OO streams
                ;; extension (which is also supported by various
                ;; lower-level hooks elsewhere in the code)
                "src/pcl/gray-streams-class"
                "src/pcl/gray-streams"

                ;; CLOS-level support for User-extensible sequences.
                "src/pcl/sequence"

                ;; other functionality not needed for cold init, moved
                ;; to warm init to reduce peak memory requirement in
                ;; cold init
                "src/code/describe"

                "src/code/describe-policy"
                "src/code/inspect"
                "src/code/profile"
                "src/code/ntrace"
                "src/code/step"
                "src/code/warm-lib"
                #+win32 "src/code/warm-mswin"
                "src/code/run-program"
                #+gencgc "src/code/traceroot"

                #+immobile-code "src/code/immobile-space"
                "src/code/repack-xref"
                #+cheneygc "src/code/purify"
                "src/code/save"))
      (sb-c::*handled-conditions* sb-c::*handled-conditions*))
 (declare (special *compile-files-p*))
 (proclaim '(sb-ext:muffle-conditions
             (or (satisfies unable-to-optimize-note-p)
                 (satisfies optional+key-style-warning-p))))
 (flet ((do-srcs (list)
         (dolist (stem list)
          ;; Do like SB-COLD::LPNIFY-STEM for consistency, though parse/xlate/unparse
          ;; would probably also work. I don't think that's better.
          (let ((fullname (format nil "SYS:~:@(~A~).LISP" (substitute #\; #\/ stem)))
                (output
                  (compile-file-pathname stem
                   :output-file
                   ;; Specifying the directory name for :OUTPUT-FILE is enough.
                   ;; It does the right thing. (Does it work on Windows? I hope so)
                   (truename
                    (concatenate
                     'string sb-fasl::*!target-obj-prefix*
                     ;; OR: (namestring (make-pathname :directory (pathname-directory stem)))
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
    (do-srcs early-srcs)
    (with-compilation-unit () (do-srcs interpreter-srcs))
    (do-srcs external-format-srcs)
    (with-compilation-unit () (do-srcs pcl-srcs))
    (do-srcs other-srcs))))
