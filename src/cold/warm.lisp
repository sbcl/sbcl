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

(proclaim '(optimize (compilation-speed 1)
                     (debug #+sb-show 2 #-sb-show 1)
                     (inhibit-warnings 2)
                     (safety 2)
                     (space 1)
                     (speed 2)))


;;;; package hacking

;;; Our cross-compilation host is out of the picture now, so we no
;;; longer need to worry about collisions between our package names
;;; and cross-compilation host package names, so now is a good time to
;;; rename any package with a bootstrap-only name SB!FOO to its
;;; permanent name SB-FOO.
;;;
;;; (In principle it might be tidier to do this when dumping the cold
;;; image in genesis, but in practice the logic might be a little
;;; messier because genesis dumps both symbols and packages, and we'd
;;; need to make sure that dumped symbols were renamed in the same way
;;; as dumped packages. Or we could do it in cold init, but it's
;;; easier to experiment with and debug things here in warm init than
;;; in cold init, so we do it here instead.)
(let ((boot-prefix "SB!")
      (perm-prefix "SB-"))
  (dolist (package (list-all-packages))
    (let ((old-package-name (package-name package)))
      (when (and (>= (length old-package-name) (length boot-prefix))
                 (string= boot-prefix old-package-name
                          :end2 (length boot-prefix)))
        (let ((new-package-name (concatenate 'string
                                             perm-prefix
                                             (subseq old-package-name
                                                     (length boot-prefix)))))
          (rename-package package
                          new-package-name
                          (package-nicknames package)))))))

;;; FIXME: This nickname is a deprecated hack for backwards
;;; compatibility with code which assumed the CMU-CL-style
;;; SB-ALIEN/SB-C-CALL split. That split went away and was deprecated
;;; in 0.7.0, so we should get rid of this nickname after a while.
(let ((package (find-package "SB-ALIEN")))
  (rename-package package
                  (package-name package)
                  (cons "SB-C-CALL" (package-nicknames package))))

(let ((package (find-package "SB-SEQUENCE")))
  (rename-package package (package-name package) (list "SEQUENCE")))

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
;;; FIXME: This has mutated into a hack which crudely duplicates
;;; functionality from the existing mechanism to load files from
;;; build-order.lisp-expr, without being quite parallel. (E.g. object
;;; files end up alongside the source files instead of ending up in
;;; parallel directory trees.) Maybe we could merge the filenames here
;;; into build-order.lisp-expr with some new flag (perhaps :WARM) to
;;; indicate that the files should be handled not in cold load but
;;; afterwards.
(dolist (stem '(;; CLOS, derived from the PCL reference implementation
                ;;
                ;; This PCL build order is based on a particular
                ;; (arbitrary) linearization of the declared build
                ;; order dependencies from the old PCL defsys.lisp
                ;; dependency database.
                #+nil "src/pcl/walk" ; #+NIL = moved to build-order.lisp-expr
                "SRC;PCL;EARLY-LOW"
                "SRC;PCL;MACROS"
                "SRC;PCL;COMPILER-SUPPORT"
                "SRC;PCL;LOW"
                "SRC;PCL;SLOT-NAME"
                "SRC;PCL;DEFCLASS"
                "SRC;PCL;DEFS"
                "SRC;PCL;FNGEN"
                "SRC;PCL;WRAPPER"
                "SRC;PCL;CACHE"
                "SRC;PCL;DLISP"
                "SRC;PCL;BOOT"
                "SRC;PCL;VECTOR"
                "SRC;PCL;SLOTS-BOOT"
                "SRC;PCL;COMBIN"
                "SRC;PCL;DFUN"
                "SRC;PCL;CTOR"
                "SRC;PCL;BRAID"
                "SRC;PCL;DLISP3"
                "SRC;PCL;GENERIC-FUNCTIONS"
                "SRC;PCL;SLOTS"
                "SRC;PCL;INIT"
                "SRC;PCL;STD-CLASS"
                "SRC;PCL;CPL"
                "SRC;PCL;FSC"
                "SRC;PCL;METHODS"
                "SRC;PCL;FIXUP"
                "SRC;PCL;DEFCOMBIN"
                "SRC;PCL;CTYPES"
                "SRC;PCL;ENV"
                "SRC;PCL;DOCUMENTATION"
                "SRC;PCL;PRINT-OBJECT"
                "SRC;PCL;PRECOM1"
                "SRC;PCL;PRECOM2"

                ;; miscellaneous functionality which depends on CLOS
                "SRC;CODE;FORCE-DELAYED-DEFBANGMETHODS"
                "SRC;CODE;LATE-CONDITION"

                ;; CLOS-level support for the Gray OO streams
                ;; extension (which is also supported by various
                ;; lower-level hooks elsewhere in the code)
                "SRC;PCL;GRAY-STREAMS-CLASS"
                "SRC;PCL;GRAY-STREAMS"

                ;; CLOS-level support for User-extensible sequences.
                "SRC;PCL;SEQUENCE"

                ;; other functionality not needed for cold init, moved
                ;; to warm init to reduce peak memory requirement in
                ;; cold init
                "SRC;CODE;DESCRIBE"
                "SRC;CODE;DESCRIBE-POLICY"
                "SRC;CODE;INSPECT"
                "SRC;CODE;PROFILE"
                "SRC;CODE;NTRACE"
                "SRC;CODE;STEP"
                "SRC;CODE;RUN-PROGRAM"))

  (let ((fullname (concatenate 'string "SYS:" stem ".LISP")))
    (sb-int:/show "about to compile" fullname)
    (flet ((report-recompile-restart (stream)
             (format stream "Recompile file ~S" fullname))
           (report-continue-restart (stream)
             (format stream
                     "Continue, using possibly bogus file ~S"
                     (compile-file-pathname fullname))))
      (tagbody
       retry-compile-file
         (multiple-value-bind (output-truename warnings-p failure-p)
             (if *compile-files-p*
                 (compile-file fullname)
                 (compile-file-pathname fullname))
           (declare (ignore warnings-p))
           (sb-int:/show "done compiling" fullname)
           (cond ((not output-truename)
                  (error "COMPILE-FILE of ~S failed." fullname))
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
           (unless (load output-truename)
             (error "LOAD of ~S failed." output-truename))
           (sb-int:/show "done loading" output-truename))))))

;;;; setting package documentation

;;; While we were running on the cross-compilation host, we tried to
;;; be portable and not overwrite the doc strings for the standard
;;; packages. But now the cross-compilation host is only a receding
;;; memory, and we can have our way with the doc strings.
(sb-int:/show "setting package documentation")
#+sb-doc (setf (documentation (find-package "COMMON-LISP") t)
"public: home of symbols defined by the ANSI language specification")
#+sb-doc (setf (documentation (find-package "COMMON-LISP-USER") t)
               "public: the default package for user code and data")
#+sb-doc (setf (documentation (find-package "KEYWORD") t)
               "public: home of keywords")


