;;;; stuff which is not specific to any particular build phase, but
;;;; used by most of them
;;;;
;;;; Note: It's specifically not used when bootstrapping PCL, because
;;;; we do SAVE-LISP after that, and we don't want to save extraneous
;;;; bootstrapping machinery into the frozen image which will
;;;; subsequently be used as the mother of all Lisp sessions.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;; SB-COLD holds stuff used to build the initial SBCL core file
;;; (including not only the final construction of the core file, but
;;; also the preliminary steps like e.g. building the cross-compiler
;;; and running the cross-compiler to produce target FASL files).
(defpackage "SB-COLD" (:use "CL"))

(in-package "SB-COLD")

;;; If TRUE, then COMPILE-FILE is being invoked only to process
;;; :COMPILE-TOPLEVEL forms, not to produce an output file.
(defvar *compile-for-effect-only* nil)

;;; prefixes for filename stems when cross-compiling. These are quite arbitrary
;;; (although of course they shouldn't collide with anything we don't want to
;;; write over). In particular, they can be either relative path names (e.g.
;;; "host-objects/" or absolute pathnames (e.g. "/tmp/sbcl-xc-host-objects/").
;;;
;;; The cross-compilation process will force the creation of these directories
;;; by executing CL:ENSURE-DIRECTORIES-EXIST (on the xc host Common Lisp).
(defvar *host-obj-prefix*)
(defvar *target-obj-prefix*)

(defvar *target-obj-suffix*
  ;; Target fasl files are LOADed (actually only quasi-LOADed, in
  ;; GENESIS) only by SBCL code, and it doesn't care about particular
  ;; extensions, so we can use something arbitrary.
  ".lisp-obj")
(defvar *target-assem-obj-suffix*
  ;; Target fasl files from SB!C:ASSEMBLE-FILE are LOADed via GENESIS.
  ;; The source files are compiled once as assembly files and once as
  ;; normal lisp files.  In the past, they were kept separate by
  ;; clever symlinking in the source tree, but that became less clean
  ;; as ports to host environments without symlinks started appearing.
  ;; In order to keep them separate, we have the assembled versions
  ;; with a separate suffix.
  ".assem-obj")

;;; a function of one functional argument, which calls its functional argument
;;; in an environment suitable for compiling the target. (This environment
;;; includes e.g. a suitable *FEATURES* value.)
(declaim (type function *in-target-compilation-mode-fn*))
(defvar *in-target-compilation-mode-fn*)

;;; a function with the same calling convention as CL:COMPILE-FILE, to be
;;; used to translate ordinary Lisp source files into target object files
(declaim (type function *target-compile-file*))
(defvar *target-compile-file*)

;;; designator for a function with the same calling convention as
;;; SB-C:ASSEMBLE-FILE, to be used to translate assembly files into target
;;; object files
(defvar *target-assemble-file*)

;;;; some tools

;;; Take the file named X and make it into a file named Y. Sorta like
;;; UNIX, and unlike Common Lisp's bare RENAME-FILE, we don't allow
;;; information from the original filename to influence the final
;;; filename. (The reason that it's only sorta like UNIX is that in
;;; UNIX "mv foo bar/" will work, but the analogous
;;; (RENAME-FILE-A-LA-UNIX "foo" "bar/") should fail.)
;;;
;;; (This is a workaround for the weird behavior of Debian CMU CL
;;; 2.4.6, where (RENAME-FILE "dir/x" "dir/y") tries to create a file
;;; called "dir/dir/y". If that behavior goes away, then we should be
;;; able to get rid of this function and use plain RENAME-FILE in the
;;; COMPILE-STEM function above. -- WHN 19990321
(defun rename-file-a-la-unix (x y)

  (let ((path    ;; (Note that the TRUENAME expression here is lifted from an
                 ;; example in the ANSI spec for TRUENAME.)
         (with-open-file (stream y :direction :output)
           (close stream)
           ;; From the ANSI spec: "In this case, the file is closed
           ;; when the truename is tried, so the truename
           ;; information is reliable."
           (truename stream))))
    (delete-file path)
    (rename-file x path)))
(compile 'rename-file-a-la-unix)

;;; other miscellaneous tools
(load "src/cold/read-from-file.lisp")
(load "src/cold/rename-package-carefully.lisp")
(load "src/cold/with-stuff.lisp")

;;; Try to minimize/conceal any non-standardness of the host Common Lisp.
(load "src/cold/ansify.lisp")

;;;; special read-macros for building the cold system (and even for
;;;; building some of our tools for building the cold system)

(load "src/cold/shebang.lisp")

;;; When cross-compiling, the *FEATURES* set for the target Lisp is
;;; not in general the same as the *FEATURES* set for the host Lisp.
;;; In order to refer to target features specifically, we refer to
;;; *SHEBANG-FEATURES* instead of *FEATURES*, and use the #!+ and #!-
;;; readmacros instead of the ordinary #+ and #- readmacros.
(setf *shebang-features*
      (let* ((default-features
               (funcall (compile
                         nil
                         (read-from-file "local-target-features.lisp-expr"))
                        (read-from-file "base-target-features.lisp-expr")))
             (customizer-file-name "customize-target-features.lisp")
             (customizer (if (probe-file customizer-file-name)
                             (compile nil
                                      (read-from-file customizer-file-name))
                             #'identity)))
        (funcall customizer default-features)))
(let ((*print-length* nil)
      (*print-level* nil))
  (format t
          "target features *SHEBANG-FEATURES*=~%~@<~S~:>~%"
          *shebang-features*))

(defvar *shebang-backend-subfeatures*
  (let* ((default-subfeatures nil)
         (customizer-file-name "customize-backend-subfeatures.lisp")
         (customizer (if (probe-file customizer-file-name)
                         (compile nil
                                  (read-from-file customizer-file-name))
                         #'identity)))
    (funcall customizer default-subfeatures)))
(let ((*print-length* nil)
      (*print-level* nil))
  (format t
          "target backend-subfeatures *SHEBANG-BACKEND-FEATURES*=~@<~S~:>~%"
          *shebang-backend-subfeatures*))

;;; Call for effect of signaling an error if no target picked.
(target-platform-name)

;;; You can get all the way through make-host-1 without either one of these
;;; features, but then 'bit-bash' will fail to cross-compile.
(unless (intersection '(:big-endian :little-endian) *shebang-features*)
  (warn "You'll have bad time without either endian-ness defined"))

;;; Some feature combinations simply don't work, and sometimes don't
;;; fail until quite a ways into the build.  Pick off the more obvious
;;; combinations now, and provide a description of what the actual
;;; failure is (not always obvious from when the build fails).
(let ((feature-compatibility-tests
       '(("(and sb-thread (not gencgc))"
          ":SB-THREAD requires :GENCGC")
         ("(and sb-thread (not (or ppc x86 x86-64 arm64)))"
          ":SB-THREAD not supported on selected architecture")
         ("(and gencgc cheneygc)"
          ":GENCGC and :CHENEYGC are incompatible")
         ("(and cheneygc (not (or alpha arm hppa mips ppc sparc)))"
          ":CHENEYGC not supported on selected architecture")
         ("(and gencgc (not (or sparc ppc x86 x86-64 arm arm64)))"
          ":GENCGC not supported on selected architecture")
         ("(not (or gencgc cheneygc))"
          "One of :GENCGC or :CHENEYGC must be enabled")
         ("(and win32 (not (and sb-thread
                                sb-safepoint sb-thruption sb-wtimer
                                sb-dynamic-core)))"
          ":SB-WIN32 requires :SB-THREAD and related features")
         ("(and sb-dynamic-core (not (and linkage-table sb-thread)))"
          ;; Subtle memory corruption follows when sb-dynamic-core is
          ;; active, and non-threaded allocation routines have not been
          ;; updated to take the additional indirection into account.
          ;; Let's avoid this unusual combination.
          ":SB-DYNAMIC-CORE requires :LINKAGE-TABLE and :SB-THREAD")
         ("(and sb-eval sb-fasteval)"
          ;; It sorta kinda works to have both, but there should be no need,
          ;; and it's not really supported.
          "At most one interpreter can be selected")
         ;; There is still hope to make multithreading on DragonFly x86-64
         ("(and sb-thread x86 dragonfly)"
          ":SB-THREAD not supported on selected architecture")))
      (failed-test-descriptions nil))
  (dolist (test feature-compatibility-tests)
    (let ((*features* *shebang-features*))
      (when (read-from-string (concatenate 'string "#+" (first test) "T NIL"))
        (push (second test) failed-test-descriptions))))
  (when failed-test-descriptions
    (error "Feature compatibility check failed, ~S"
           failed-test-descriptions)))

;;;; cold-init-related PACKAGE and SYMBOL tools

;;; Once we're done with possibly ANSIfying the COMMON-LISP package,
;;; it's probably a mistake if we change it (beyond changing the
;;; values of special variables such as *** and +, anyway). Set up
;;; machinery to warn us when/if we change it.
;;;
;;; All code depending on this is itself dependent on #!+SB-SHOW.
#!+sb-show
(progn
  (load "src/cold/snapshot.lisp")
  (defvar *cl-snapshot* (take-snapshot "COMMON-LISP")))

;;;; master list of source files and their properties

;;; flags which can be used to describe properties of source files
(defparameter
  *expected-stem-flags*
  '(;; meaning: This file is not to be compiled when building the
    ;; cross-compiler which runs on the host ANSI Lisp. ("not host
    ;; code", i.e. does not execute on host -- but may still be
    ;; cross-compiled by the host, so that it executes on the target)
    :not-host
    ;; meaning: This file is not to be compiled as part of the target
    ;; SBCL. ("not target code" -- but still presumably host code,
    ;; used to support the cross-compilation process)
    :not-target
    ;; meaning: The #'COMPILE-STEM argument :TRACE-FILE should be T.
    ;; When the compiler is SBCL's COMPILE-FILE or something like it,
    ;; compiling "foo.lisp" will generate "foo.trace" which contains lots
    ;; of exciting low-level information about representation selection,
    ;; VOPs used by the compiler, and bits of assembly.
    :trace-file
    ;; meaning: This file is to be processed with the SBCL assembler,
    ;; not COMPILE-FILE. (Note that this doesn't make sense unless
    ;; :NOT-HOST is also set, since the SBCL assembler doesn't exist
    ;; while the cross-compiler is being built in the host ANSI Lisp.)
    :assem
    ;; meaning: The #'COMPILE-STEM argument called :IGNORE-FAILURE-P
    ;; should be true. (This is a KLUDGE: I'd like to get rid of it.
    ;; For now, it exists so that compilation can proceed through the
    ;; legacy warnings in src/compiler/x86/array.lisp, which I've
    ;; never figured out but which were apparently acceptable in CMU
    ;; CL. Eventually, it would be great to just get rid of all
    ;; warnings and remove support for this flag. -- WHN 19990323)
    :ignore-failure-p))

(defparameter *stems-and-flags* (read-from-file "build-order.lisp-expr"))

(defvar *array-to-specialization* (make-hash-table :test #'eq))

(defmacro do-stems-and-flags ((stem flags) &body body)
  (let ((stem-and-flags (gensym "STEM-AND-FLAGS")))
    `(dolist (,stem-and-flags *stems-and-flags*)
       (let ((,stem (first ,stem-and-flags))
             (,flags (rest ,stem-and-flags)))
         ,@body
         (clrhash *array-to-specialization*)))))

;;; Given a STEM, remap the path component "/target/" to a suitable
;;; target directory.
(defun stem-remap-target (stem)
  (let ((position (search "/target/" stem)))
    (if position
      (concatenate 'string
                   (subseq stem 0 (1+ position))
                   (target-platform-name)
                   (subseq stem (+ position 7)))
      stem)))
(compile 'stem-remap-target)

;;; Determine the source path for a stem.
(defun stem-source-path (stem)
  (concatenate 'string "" (stem-remap-target stem) ".lisp"))
(compile 'stem-source-path)

;;; Determine the object path for a stem/flags/mode combination.
(defun stem-object-path (stem flags mode)
  (multiple-value-bind
        (obj-prefix obj-suffix)
      (ecase mode
        (:host-compile
         ;; On some xc hosts, it's impossible to LOAD a fasl file unless it
         ;; has the same extension that the host uses for COMPILE-FILE
         ;; output, so we have to be careful to use the xc host's preferred
         ;; extension.
         (values *host-obj-prefix*
                 (concatenate 'string "."
                              (pathname-type (compile-file-pathname stem)))))
        (:target-compile (values *target-obj-prefix*
                                 (if (find :assem flags)
                                     *target-assem-obj-suffix*
                                     *target-obj-suffix*))))
    (concatenate 'string obj-prefix (stem-remap-target stem) obj-suffix)))
(compile 'stem-object-path)

;;; Check for stupid typos in FLAGS list keywords.
(let ((stems (make-hash-table :test 'equal)))
  (do-stems-and-flags (stem flags)
    ;; We do duplicate stem comparison based on the object path in
    ;; order to cover the case of stems with an :assem flag, which
    ;; have two entries but separate object paths for each.  KLUDGE:
    ;; We have to bind *target-obj-prefix* here because it's normally
    ;; set up later in the build process and we don't actually care
    ;; what it is so long as it doesn't change while we're checking
    ;; for duplicate stems.
    (let* ((*target-obj-prefix* "")
           (object-path (stem-object-path stem flags :target-compile)))
      (if (gethash object-path stems)
          (error "duplicate stem ~S in *STEMS-AND-FLAGS*" stem)
          (setf (gethash object-path stems) t)))
    ;; FIXME: We should make sure that the :assem flag is only used
    ;; when paired with :not-host.
    (let ((set-difference (set-difference flags *expected-stem-flags*)))
      (when set-difference
        (error "found unexpected flag(s) in *STEMS-AND-FLAGS*: ~S"
               set-difference)))))

;;;; tools to compile SBCL sources to create the cross-compiler

;;; a wrapper for compilation/assembly, used mostly to centralize
;;; the procedure for finding full filenames from "stems"
;;;
;;; Compile the source file whose basic name is STEM, using some
;;; standard-for-the-SBCL-build-process procedures to generate the
;;; full pathnames of source file and object file. Return the pathname
;;; of the object file for STEM.
;;;
;;; STEM and FLAGS are as per DO-STEMS-AND-FLAGS.  MODE is one of
;;; :HOST-COMPILE and :TARGET-COMPILE.
(defun compile-stem (stem flags mode)

  (let* (;; KLUDGE: Note that this CONCATENATE 'STRING stuff is not The Common
         ;; Lisp Way, although it works just fine for common UNIX environments.
         ;; Should it come to pass that the system is ported to environments
         ;; where version numbers and so forth become an issue, it might become
         ;; urgent to rewrite this using the fancy Common Lisp PATHNAME
         ;; machinery instead of just using strings. In the absence of such a
         ;; port, it might or might be a good idea to do the rewrite.
         ;; -- WHN 19990815
         (src (stem-source-path stem))
         (obj (stem-object-path stem flags mode))
         (tmp-obj (concatenate 'string obj "-tmp"))

         (compile-file (ecase mode
                         (:host-compile #'compile-file)
                         (:target-compile (if (find :assem flags)
                                              *target-assemble-file*
                                              *target-compile-file*))))
         (trace-file (find :trace-file flags))
         (ignore-failure-p (find :ignore-failure-p flags)))
    (declare (type function compile-file))

    (ensure-directories-exist obj :verbose t)

    ;; We're about to set about building a new object file. First, we
    ;; delete any preexisting object file in order to avoid confusing
    ;; ourselves later should we happen to bail out of compilation
    ;; with an error.
    (when (probe-file obj)
      (delete-file obj))

    ;; Original comment:
    ;;
    ;;   Work around a bug in CLISP 1999-01-08 #'COMPILE-FILE: CLISP
    ;;   mangles relative pathnames passed as :OUTPUT-FILE arguments,
    ;;   but works OK with absolute pathnames.
    ;;
    ;; following discussion on cmucl-imp 2002-07
    ;; "COMPILE-FILE-PATHNAME", it would seem safer to deal with
    ;; absolute pathnames all the time; it is no longer clear that the
    ;; original behaviour in CLISP was wrong or that the current
    ;; behaviour is right; and in any case absolutifying the pathname
    ;; insulates us against changes of behaviour. -- CSR, 2002-08-09
    (setf tmp-obj
          ;; (Note that this idiom is taken from the ANSI
          ;; documentation for TRUENAME.)
          (with-open-file (stream tmp-obj
                                  :direction :output
                                  ;; Compilation would overwrite the
                                  ;; temporary object anyway and overly
                                  ;; strict implementations default
                                  ;; to :ERROR.
                                  :if-exists :supersede)
            (close stream)
            (truename stream)))
    ;; and some compilers (e.g. OpenMCL) will complain if they're
    ;; asked to write over a file that exists already (and isn't
    ;; recognizeably a fasl file), so
    (when (probe-file tmp-obj)
      (delete-file tmp-obj))

    ;; Try to use the compiler to generate a new temporary object file.
    (flet ((report-recompile-restart (stream)
             (format stream "Recompile file ~S" src))
           (report-continue-restart (stream)
             (format stream "Continue, using possibly bogus file ~S" obj)))
      (tagbody
       retry-compile-file
         (multiple-value-bind (output-truename warnings-p failure-p)
            (if trace-file
                (funcall compile-file src :output-file tmp-obj
                         :trace-file t :allow-other-keys t)
                (funcall compile-file src :output-file tmp-obj))
           (declare (ignore warnings-p))
           (cond ((not output-truename)
                  (error "couldn't compile ~S" src))
                 (failure-p
                  (if ignore-failure-p
                      (warn "ignoring FAILURE-P return value from compilation of ~S"
                            src)
                      (unwind-protect
                           (restart-case
                               (error "FAILURE-P was set when creating ~S."
                                      obj)
                             (recompile ()
                               :report report-recompile-restart
                               (go retry-compile-file))
                             (continue ()
                               :report report-continue-restart
                               (setf failure-p nil)))
                        ;; Don't leave failed object files lying around.
                        (when (and failure-p (probe-file tmp-obj))
                          (delete-file tmp-obj)
                          (format t "~&deleted ~S~%" tmp-obj)))))
                 ;; Otherwise: success, just fall through.
                 (t nil)))))

    ;; If we get to here, compilation succeeded, so it's OK to rename
    ;; the temporary output file to the permanent object file.
    ;; ASSEMBLE-FILE produces no output in the preload pass.
    ;; (The compiler produces an empty file.)
    (if (and *compile-for-effect-only* (search "/assembly/" obj))
        nil
        (rename-file-a-la-unix tmp-obj obj))

    ;; nice friendly traditional return value
    (pathname obj)))
(compile 'compile-stem)

;;; Execute function FN in an environment appropriate for compiling the
;;; cross-compiler's source code in the cross-compilation host.
(defun in-host-compilation-mode (fn)
  (declare (type function fn))
  (let ((*features* (cons :sb-xc-host *features*))
        ;; the CROSS-FLOAT-INFINITY-KLUDGE, as documented in
        ;; base-target-features.lisp-expr:
        (*shebang-features* (set-difference *shebang-features*
                                            '(:sb-propagate-float-type
                                              :sb-propagate-fun-type))))
    (with-additional-nickname ("SB-XC" "SB!XC")
      (funcall fn))))
(compile 'in-host-compilation-mode)

;;; Process a file as source code for the cross-compiler, compiling it
;;; (if necessary) in the appropriate environment, then loading it
;;; into the cross-compilation host Common lisp.
(defun host-cload-stem (stem flags)
  (let ((compiled-filename (in-host-compilation-mode
                            (lambda ()
                              (compile-stem stem flags :host-compile)))))
    (load compiled-filename)))
(compile 'host-cload-stem)

;;; like HOST-CLOAD-STEM, except that we don't bother to compile
(defun host-load-stem (stem flags)
  (load (stem-object-path stem flags :host-compile)))
(compile 'host-load-stem)

;;;; tools to compile SBCL sources to create object files which will
;;;; be used to create the target SBCL .core file

;;; Run the cross-compiler on a file in the source directory tree to
;;; produce a corresponding file in the target object directory tree.
(defun target-compile-stem (stem flags)
  (funcall *in-target-compilation-mode-fn*
           (lambda ()
             (compile-stem stem flags :target-compile))))
(compile 'target-compile-stem)

;;; (This function is not used by the build process, but is intended
;;; for interactive use when experimenting with the system. It runs
;;; the cross-compiler on test files with arbitrary filenames, not
;;; necessarily in the source tree, e.g. in "/tmp".)
(defun target-compile-file (filename)
  (funcall *in-target-compilation-mode-fn*
           (lambda ()
             (funcall *target-compile-file* filename))))
(compile 'target-compile-file)

(defun make-assembler-package (pkg-name)
  (when (find-package pkg-name)
    (delete-package pkg-name))
  (let ((pkg (make-package pkg-name
                           :use '("CL" "SB!INT" "SB!EXT" "SB!KERNEL" "SB!VM"
                                  "SB!SYS" ; for SAP accessors
                                  ;; Dependence of the assembler on the compiler
                                  ;; feels a bit backwards, but assembly needs
                                  ;; TN-SC, TN-OFFSET, etc. because the compiler
                                  ;; doesn't speak the assembler's language.
                                  ;; Rather vice-versa.
                                  "SB!C"))))
    ;; Both SB-ASSEM and SB-DISASSEM export these two symbols.
    ;; Neither is shadowing-imported. If you need one, package-qualify it.
    (shadow '("SEGMENT" "MAKE-SEGMENT") pkg)
    (use-package '("SB!ASSEM" "SB!DISASSEM") pkg)
    pkg))
