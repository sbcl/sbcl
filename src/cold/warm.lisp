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

;;; Assert that genesis preserves shadowing symbols.
(let ((p sb-assem::*backend-instruction-set-package*))
  (unless (eq p (find-package "SB-VM"))
    (dolist (expect '("SEGMENT" "MAKE-SEGMENT"))
      (assert (find expect (package-shadowing-symbols p) :test 'string=)))))

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
;;; FIXME: This has mutated into a hack which crudely duplicates
;;; functionality from the existing mechanism to load files from
;;; build-order.lisp-expr, without being quite parallel. (E.g. object
;;; files end up alongside the source files instead of ending up in
;;; parallel directory trees.) Maybe we could merge the filenames here
;;; into build-order.lisp-expr with some new flag (perhaps :WARM) to
;;; indicate that the files should be handled not in cold load but
;;; afterwards.
(let ((pcl-srcs
              '(;; CLOS, derived from the PCL reference implementation
                ;;
                ;; This PCL build order is based on a particular
                ;; (arbitrary) linearization of the declared build
                ;; order dependencies from the old PCL defsys.lisp
                ;; dependency database.
                #+nil "src/pcl/walk" ; #+NIL = moved to build-order.lisp-expr
                #+nil "SRC;PCL;EARLY-LOW"
                "SRC;PCL;MACROS"
                "SRC;PCL;COMPILER-SUPPORT"
                #+nil "SRC;PCL;LOW"
                #+nil "SRC;PCL;SLOT-NAME" ; moved to build-order.lisp-expr
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
                "SRC;PCL;PRECOM2"))
      (other-srcs
              '(;; miscellaneous functionality which depends on CLOS
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

                #+sb-fasteval "SRC;INTERPRETER;MACROS"
                #+sb-fasteval "SRC;INTERPRETER;CHECKFUNS"
                #+sb-fasteval "SRC;INTERPRETER;ENV"
                #+sb-fasteval "SRC;INTERPRETER;SEXPR"
                #+sb-fasteval "SRC;INTERPRETER;SPECIAL-FORMS"
                #+sb-fasteval "SRC;INTERPRETER;EVAL"
                #+sb-fasteval "SRC;INTERPRETER;DEBUG"

                "SRC;CODE;DESCRIBE-POLICY"
                "SRC;CODE;INSPECT"
                "SRC;CODE;PROFILE"
                "SRC;CODE;NTRACE"
                "SRC;CODE;STEP"
                "SRC;CODE;WARM-LIB"
                #+win32 "SRC;CODE;WARM-MSWIN"
                "SRC;CODE;RUN-PROGRAM"))
      (sb-c::*handled-conditions* sb-c::*handled-conditions*))
 (declare (special *compile-files-p*))
 (proclaim '(sb-ext:muffle-conditions
             (or (satisfies unable-to-optimize-note-p)
                 (satisfies optional+key-style-warning-p))))
 (flet
    ((do-srcs (list)
       (dolist (stem list)
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
                  (unless (handler-bind
                              ((sb-kernel:redefinition-with-defgeneric
                                #'muffle-warning))
                            (load output-truename))
                    (error "LOAD of ~S failed." output-truename))
                  (sb-int:/show "done loading" output-truename))))))))

   (let* ((ppd (copy-pprint-dispatch))
          (sb-debug:*debug-print-variable-alist*
           (list (cons '*print-pprint-dispatch* ppd)))
          (*print-pprint-dispatch* ppd))
     (set-pprint-dispatch
      'sb-kernel:layout (lambda (stream obj)
                          (print-unreadable-object (obj stream :type t)
                            (write (sb-int:awhen (sb-kernel:layout-classoid obj)
                                     (sb-kernel:classoid-name sb-int:it))
                                   :stream stream))))
     (set-pprint-dispatch
      'sb-kernel:classoid (lambda (stream obj)
                            (print-unreadable-object (obj stream :type t)
                              (write (sb-kernel:classoid-name obj) :stream stream))))
     (set-pprint-dispatch
      'sb-kernel:ctype (lambda (stream obj)
                         (print-unreadable-object (obj stream :type t)
                           (prin1 (sb-kernel:type-specifier obj) stream))))
     (set-pprint-dispatch
      'package (lambda (stream obj)
                 (print-unreadable-object (obj stream :type t)
                   (write (package-name obj) :stream stream))))
     (set-pprint-dispatch
      'pathname (lambda (stream obj)
                  (write-string "#P" stream)
                  (write (namestring obj) :stream stream)))
     (set-pprint-dispatch
      'sb-thread:thread (lambda (stream obj)
                          (declare (ignore obj))
                          (write-string "#<main-thread>" stream)))
     (set-pprint-dispatch
      'restart (lambda (stream obj)
                 (print-unreadable-object (obj stream :type t :identity t)
                   (write (restart-name obj) :stream stream))))
     ;; These next two are coded in a totally brittle way, but no more wrong
     ;; than typing the decoding expressions into the debugger to decipher
     ;; a backtrace. Anyway, if it ceases to print right, just fix it again!
     (set-pprint-dispatch
      ;; Circumlocution avoids use of unknown type.
      '(and function (satisfies sb-pcl::generic-function-p))
      (lambda (stream obj)
        (format stream "<~S ~S>" (type-of obj)
                (svref (sb-kernel:%funcallable-instance-info obj 1) 5))))
     ;; dangerous: this type doesn't exist, and the testable-type-p thing
     ;; isn't working the way it should. It's supposed to enable the ppd entry
     ;; as soon as CLASS becomes defined, but instead it goes bonkers.
     #+nil
     (set-pprint-dispatch
      'class
      (lambda (stream obj)
        (format stream "<~S ~S>" (type-of obj)
                (svref (sb-kernel:%instance-ref obj 1) 3))))
     (with-compilation-unit ()
       (let ((*compile-print* nil))
         (do-srcs pcl-srcs)))
     (when *compile-files-p*
       (format t "~&; Done with PCL compilation~2%"))
     (do-srcs other-srcs))))

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


