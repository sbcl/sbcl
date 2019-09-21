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

(defun parse-make-host-parallelism (str)
  (multiple-value-bind (value1 end) (parse-integer str :junk-allowed t)
    (when value1
      (let ((value2 (if (and value1
                             (< end (1- (length str))) ; ~ /,[\d]+/
                             (eql (char str end) #\,))
                        (parse-integer str :start (1+ end)))))
        ;; If only 1 integer, assume same parallelism for both passes.
        (unless value2
          (setq value2 value1))
        ;; 0 means no parallelism. 1 means use at most one subjob,
        ;; just in case you want to test the controlling loop.
        (when (eql value1 0) (setq value1 nil))
        (when (eql value2 0) (setq value2 nil))
        ;; Parallelism on pass 1 works only if LOAD does not compile.
        ;; Otherwise it's slower than compiling serially.
        ;; (And this has only been tested with sb-fasteval, not sb-eval.)
        (cons (and (find-package "SB-INTERPRETER") value1)
              value2)))))

(defvar *make-host-parallelism*
  (or #+sbcl
      (let ((envvar (sb-ext:posix-getenv "SBCL_MAKE_PARALLEL")))
        (when envvar
          (require :sb-posix)
          (parse-make-host-parallelism envvar)))))

(defun make-host-1-parallelism () (car *make-host-parallelism*))
(defun make-host-2-parallelism () (cdr *make-host-parallelism*))

#+sbcl
(let ((f (multiple-value-bind (sym access) (find-symbol "OS-EXIT" "SB-SYS")
           (if (eq access :external) sym 'sb-unix:unix-exit))))
  (defun exit-process (arg) (funcall f arg)))

;;; If TRUE, then COMPILE-FILE is being invoked only to process
;;; :COMPILE-TOPLEVEL forms, not to produce an output file.
;;; This is part of the implementation of parallelized make-host-2.
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
  ;; Target fasl files from SB-C:ASSEMBLE-FILE are LOADed via GENESIS.
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

(export '(prepend-genfile-path read-from-file *generated-sources-root*))
(defvar *generated-sources-root* "")

;;; See remark in COMPILE-STEM about strings vs. The Common Lisp Way
(defun prepend-genfile-path (namestring)
  (concatenate 'string
               ;; if exact match to "output/", or mismatch at the next character
               (if (member (mismatch "output/" namestring) '(nil 7))
                   *generated-sources-root*
                   "")
               namestring))
(compile 'prepend-genfile-path) ; seems in vogue to compile everything in this file

;;; Return an expression read from the file named NAMESTRING.
;;; For user-supplied inputs, protect against more than one expression
;;; appearing in the file. With trusted inputs we needn't bother.
(defun read-from-file (namestring &optional (enforce-single-expr t))
  (with-open-file (s (prepend-genfile-path namestring))
    (let* ((result (read s))
           (eof-result (cons nil nil)))
      (unless enforce-single-expr
        (return-from read-from-file result))
      (unless (eq (read s nil eof-result) eof-result)
        (error "more than one expression in file ~S" namestring))
      result)))
(compile 'read-from-file)

;;; Try to minimize/conceal any non-standardness of the host Common Lisp.
#-sbcl (load "src/cold/ansify.lisp")

;;;; Do not put SBCL-specific things in 'ansify'. Put them here.
;;;; And there had better not be a reason that SBCL needs ansification.
#+sbcl
(progn
  (setq *compile-print* nil)
  (load "src/cold/muffler.lisp")
  ;; Let's just say we never care to see these.
  (declaim (sb-ext:muffle-conditions
            (satisfies unable-to-optimize-note-p)
            (satisfies optional+key-style-warning-p)
            sb-ext:code-deletion-note)))

;;;; special read-macros for building the cold system (and even for
;;;; building some of our tools for building the cold system)

(load "src/cold/shebang.lisp")

;;; When cross-compiling, the *FEATURES* set for the target Lisp is
;;; not in general the same as the *FEATURES* set for the host Lisp.
;;; In order to refer to target features specifically, we refer to
;;; SB-XC:*FEATURES* instead of CL:*FEATURES*.
;;;
;;; To support building in a read-only filesystem, the 'local-target-features'
;;; file might not be directly located here, since it's a generated file.
;;; In as much as we use files as the means of passing parameters to
;;; our Lisp scripts - because we can't in general assume that we can read
;;; the command-line arguments in any Lisp - it doesn't make sense to have
;;; another file specifying the name of the local-target-features file.
;;; The compromise is to examine a variable specifying a path
;;; (and it can't go in SB-COLD because the package is not made soon enough)
(setf sb-xc:*features*
      (let* ((pathname (let ((var 'cl-user::*sbcl-target-features-file*))
                         (if (boundp var)
                             (symbol-value var)
                             "local-target-features.lisp-expr")))
             (default-features
               (funcall (compile nil (read-from-file pathname))
                        (read-from-file "base-target-features.lisp-expr")))
             (customizer-file-name "customize-target-features.lisp")
             (customizer (if (probe-file customizer-file-name)
                             (compile nil
                                      (read-from-file customizer-file-name))
                             #'identity))
             (target-feature-list (funcall customizer default-features))
             (arch (target-platform-keyword target-feature-list)))
        ;; Sort the arch name to the front and de-dup the rest in case the
        ;; command line had a redundant --with-mumble option and/or the
        ;; customizer decided to return dups.
        (cons arch (sort (remove-duplicates (remove arch target-feature-list))
                         #'string<))))

(defvar *build-features* (let ((filename "build-features.lisp-expr"))
                           (when (probe-file filename)
                             (read-from-file filename))))
(dolist (target-feature '(:sb-after-xc-core :cons-profiling))
  (when (member target-feature sb-xc:*features*)
    (setf sb-xc:*features* (delete target-feature sb-xc:*features*))
    ;; If you use --fancy and --with-sb-after-xc-core you might
    ;; add the feature twice if you don't use pushnew
    (pushnew target-feature *build-features*)))

;; We update the host's features, because a build-feature is essentially
;; an option to check in the host enviroment
(setf *features* (append *build-features* *features*))

(defvar *shebang-backend-subfeatures*
  (let* ((default-subfeatures nil)
         (customizer-file-name "customize-backend-subfeatures.lisp")
         (customizer (if (probe-file customizer-file-name)
                         (compile nil
                                  (read-from-file customizer-file-name))
                         #'identity)))
    (funcall customizer default-subfeatures)))

;;; Call for effect of signaling an error if no target picked.
(target-platform-keyword)

;;; You can get all the way through make-host-1 without either one of these
;;; features, but then 'bit-bash' will fail to cross-compile.
(unless (intersection '(:big-endian :little-endian) sb-xc:*features*)
  (warn "You'll have bad time without either endian-ness defined"))

;;; Some feature combinations simply don't work, and sometimes don't
;;; fail until quite a ways into the build.  Pick off the more obvious
;;; combinations now, and provide a description of what the actual
;;; failure is (not always obvious from when the build fails).
(let ((feature-compatibility-tests
       '(("(and sb-thread (not gencgc))"
          ":SB-THREAD requires :GENCGC")
         ("(and sb-thread (not (or ppc ppc64 x86 x86-64 arm64)))"
          ":SB-THREAD not supported on selected architecture")
         ("(and gencgc cheneygc)"
          ":GENCGC and :CHENEYGC are incompatible")
         ("(and cheneygc (not (or alpha arm hppa mips ppc riscv sparc)))"
          ":CHENEYGC not supported on selected architecture")
         ("(and gencgc (not (or sparc ppc ppc64 x86 x86-64 arm arm64 riscv)))"
          ":GENCGC not supported on selected architecture")
         ("(not (or gencgc cheneygc))"
          "One of :GENCGC or :CHENEYGC must be enabled")
         ("(and sb-safepoint (not (or arm64 ppc x86 x86-64)))"
          ":SB-SAFEPOINT not supported on selected architecture")
         ("(and sb-safepoint-strictly (not sb-safepoint))"
          ":SB-SAFEPOINT-STRICTLY requires :SB-SAFEPOINT")
         ("(not (or elf mach-o win32))"
          "No execute object file format feature defined")
         ("(and sb-dynamic-core (not linkage-table))"
          ":SB-DYNAMIC-CORE requires :LINKAGE-TABLE")
         ("(and relocatable-heap win32)"
          "Relocatable heap requires (not win32)")
         ("(and sb-linkable-runtime (not sb-dynamic-core))"
          ":SB-LINKABLE-RUNTIME requires :SB-DYNAMIC-CORE")
         ("(and sb-linkable-runtime (not (or x86 x86-64)))"
          ":SB-LINKABLE-RUNTIME not supported on selected architecture")
         ("(and sb-linkable-runtime (not (or darwin linux win32)))"
          ":SB-LINKABLE-RUNTIME not supported on selected operating system")
         ("(and sb-eval sb-fasteval)"
          ;; It sorta kinda works to have both, but there should be no need,
          ;; and it's not really supported.
          "At most one interpreter can be selected")
         ("(and immobile-space (not x86-64))"
          ":IMMOBILE-SPACE is supported only on x86-64")
         ("(and immobile-space (not relocatable-heap))"
          ":IMMOBILE-SPACE requires :RELOCATABLE-HEAP")
         ("(and compact-instance-header (not immobile-space))"
          ":COMPACT-INSTANCE-HEADER requires :IMMOBILE-SPACE feature")
         ("(and immobile-code (not immobile-space))"
          ":IMMOBILE-CODE requires :IMMOBILE-SPACE feature")
         ("(and immobile-symbols (not immobile-space))"
          ":IMMOBILE-SYMBOLS requires :IMMOBILE-SPACE feature")
         ("(and int4-breakpoints x86)"
          ;; 0xCE is a perfectly good 32-bit instruction,
          ;; unlike on x86-64 where it is illegal. It's therefore
          ;; confusing to allow this feature in a 32-bit build.
          ":INT4-BREAKPOINTS are incompatible with x86")
         ;; There is still hope to make multithreading on DragonFly x86-64
         ("(and sb-thread x86 dragonfly)"
          ":SB-THREAD not supported on selected architecture")))
      (failed-test-descriptions nil))
  (dolist (test feature-compatibility-tests)
    (let ((cl:*features* sb-xc:*features*))
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
;;; All code depending on this is itself dependent on #+SB-SHOW.
(defvar *cl-snapshot*)
(when (member :sb-show sb-xc:*features*)
  (load "src/cold/snapshot.lisp")
  (setq *cl-snapshot* (take-snapshot "COMMON-LISP")))

;;;; master list of source files and their properties

;;; flags which can be used to describe properties of source files
(defparameter
  *expected-stem-flags*
  '(;; meaning: This file is needed to generate C headers if doing so
    ;; independently of make-host-1
    :c-headers
    ;; meaning: This file is not to be compiled when building the
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
    :ignore-failure-p
    ;; meaning: ignore this flag.
    ;; This works around nonstandard behavior of "#." in certain hosts.
    ;; When the evaluated form yields 0 values, ECL and CLISP treat it
    ;; as though if yielded NIL:
    ;; * (read-from-string "#(#.(cl:if (cl:eql 1 2) x (values)))")
    ;;   => #(NIL)
    ;; The correct value for the above expression - as obtained in SBCL,
    ;; CCL, and ABCL - is #() because _any_ reader macro is permitted
    ;; to produce 0 values. In fact you can demonstrate this by actually
    ;; implementing your own "#." which conditionally returns 0 values,
    ;; and seeing that it works in any lisp including the suspect ones.
    ;; The oft-used idiom of "#+#.(cl:if (test) '(and) '(or)) X"
    ;; is sufficiently unclear that its worth allowing a spurious NIL
    ;; just to avoid that ugly mess.
    nil))

;;; The specialized array registry has file-wide scope. Hacking that aspect
;;; into the xc build scaffold seemed slightly easier than hacking the
;;; compiler (i.e. making the registry a slot of the fasl-output struct)
(defvar *array-to-specialization* (make-hash-table :test #'eq))

(defmacro do-stems-and-flags ((stem flags build-phase) &body body)
  (let ((stem-and-flags (gensym "STEM-AND-FLAGS")))
    `(dolist (,stem-and-flags (get-stems-and-flags ,build-phase))
       (let ((,stem (first ,stem-and-flags))
             (,flags (rest ,stem-and-flags)))
         ,@body
         (clrhash *array-to-specialization*)))))

;;; Given a STEM, remap the path component "/target/" to a suitable
;;; target directory.
(defun stem-remap-target (stem)
  (flet ((try-replacing (this that)
           (let ((position (search this stem)))
             (when position
               (concatenate 'string
                            (subseq stem 0 (1+ position))
                            (string-downcase that)
                            (subseq stem (+ position (length this) -1)))))))
    (or (try-replacing "/target/" (target-platform-keyword))
        (try-replacing "/asm-target/" (backend-assembler-target-name))
        stem)))
(compile 'stem-remap-target)

;;; Determine the source path for a stem by remapping from the abstract name
;;; if it contains "/target/" and appending a ".lisp" suffix.
;;; Assume that STEM is source-tree-relative unless it starts with "output/"
;;; in which case it could be elsewhere, if you prefer to keep the sources
;;; devoid of compilation artifacts. (The production of out-of-tree artifacts
;;; is not actually implemented in the generic build, however if your build
;;; system does that by itself, then hooray for you)
(defun stem-source-path (stem)
  (concatenate 'string (prepend-genfile-path (stem-remap-target stem)) ".lisp"))
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

(defvar *stems-and-flags* nil)
;;; Read the set of files to compile with respect to a build phase, 1 or 2.
(defun get-stems-and-flags (build-phase)
  (when (and *stems-and-flags* (eql (car *stems-and-flags*) build-phase))
    (return-from get-stems-and-flags (cdr *stems-and-flags*)))
  (let* ((feature (aref #(:sb-xc-host :sb-xc) (1- build-phase)))
         (list
          ;; The build phase feature goes into CL:*FEATURES*, not SB-XC:*FEATURES*
          ;; because firstly we don't use feature expressions to control the set of
          ;; files pertinent to the build phase - that is governed by :NOT-{HOST,TARGET}
          ;; flags, and secondly we can not assume existence of the SB-XC package in
          ;; warm build. The sole reason for this hack is to allow testing for CMU
          ;; as the build host in make-host-1 which apparently needs to be allowed
          ;; to produce warnings as a bug workaround.
          (let ((cl:*features* (cons feature cl:*features*))
                (*readtable* *xc-readtable*))
            (read-from-file "build-order.lisp-expr" nil))))
    (setf *stems-and-flags* (cons build-phase list)))
  ;; Now check for duplicate stems and bogus flags.
  (let ((stems (make-hash-table :test 'equal)))
    (do-stems-and-flags (stem flags build-phase)
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
    ;; Check for stupid typos in FLAGS list keywords.
    ;; FIXME: We should make sure that the :assem flag is only used
    ;; when paired with :not-host.
      (let ((set-difference (set-difference flags *expected-stem-flags*)))
        (when set-difference
          (error "found unexpected flag(s) in *STEMS-AND-FLAGS*: ~S"
                 set-difference)))))
  (cdr *stems-and-flags*))

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
         ;; Compile-for-effect happens simultaneously with a forked compile,
         ;; so we need the for-effect output not to stomp on the real output.
         (tmp-obj
          (concatenate 'string obj
                       (if *compile-for-effect-only* "-scratch" "-tmp")))

         (compile-file (ecase mode
                         (:host-compile #'compile-file)
                         (:target-compile (if (find :assem flags)
                                              *target-assemble-file*
                                              *target-compile-file*))))
         (trace-file (find :trace-file flags))
         (ignore-failure-p (find :ignore-failure-p flags)))
    (declare (type function compile-file))

    (ensure-directories-exist obj :verbose *compile-print*) ; host's value

    ;; We're about to set about building a new object file. First, we
    ;; delete any preexisting object file in order to avoid confusing
    ;; ourselves later should we happen to bail out of compilation
    ;; with an error.
    (when (and (not *compile-for-effect-only*) (probe-file obj))
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
             (restart-case
                 (if trace-file
                     (funcall compile-file src :output-file tmp-obj
                                               :trace-file t :allow-other-keys t)
                     (funcall compile-file src :output-file tmp-obj))
               (recompile ()
                 :report report-recompile-restart
                 (go retry-compile-file)))
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
    (cond ((not *compile-for-effect-only*)
           (rename-file-a-la-unix tmp-obj obj))
          ((probe-file tmp-obj)
           (delete-file tmp-obj))) ; clean up the trash

    ;; nice friendly traditional return value
    (pathname obj)))
(compile 'compile-stem)

(defparameter *host-quirks*
  (or #+cmu  '(:host-quirks-cmu :no-ansi-print-object)
      #+ecl  '(:host-quirks-ecl)
      #+sbcl '(:host-quirks-sbcl))) ; not so much a "quirk", but consistent anyway

;;; Execute function FN in an environment appropriate for compiling the
;;; cross-compiler's source code in the cross-compilation host.
(defun in-host-compilation-mode (fn)
  (declare (type function fn))
  (let ((sb-xc:*features* (append '(:sb-xc-host) *host-quirks* sb-xc:*features*))
        (*readtable* *xc-readtable*))
    (funcall fn)))
(compile 'in-host-compilation-mode)

;;; Process a file as source code for the cross-compiler, compiling it
;;; (if necessary) in the appropriate environment, then loading it
;;; into the cross-compilation host Common lisp.
(defun host-cload-stem (stem flags)
  (loop
   (with-simple-restart (recompile "Recompile")
     (let ((compiled-filename (in-host-compilation-mode
                               (lambda ()
                                 (compile-stem stem flags :host-compile)))))
       (return
         (load compiled-filename))))))
(compile 'host-cload-stem)

;;; like HOST-CLOAD-STEM, except that we don't bother to compile
(defun host-load-stem (stem flags)
  (loop
   (with-simple-restart (recompile "Reload")
     (return (load (stem-object-path stem flags :host-compile))))))
(compile 'host-load-stem)

;;;; tools to compile SBCL sources to create object files which will
;;;; be used to create the target SBCL .core file

(defun lpnify-stem (stem)
  ;; Don't want genfiles path to sneak in - avoid (STEM-SOURCE-PATH ...) here.
  (let ((string (stem-remap-target stem)))
    ;; Distrust that random hosts don't bork up the translation.
    ;; Simply replace '/' with ';' and be done.
    (format nil "SYS:~:@(~A~).LISP" (substitute #\; #\/ string))))
(compile 'lpnify-stem)

;;; Run the cross-compiler on a file in the source directory tree to
;;; produce a corresponding file in the target object directory tree.
(defun target-compile-stem (stem flags)
  (funcall *in-target-compilation-mode-fn*
           (lambda ()
             (progv (list (intern "*SOURCE-NAMESTRING*" "SB-C"))
                    (list (lpnify-stem stem))
               (loop
                (with-simple-restart (recompile "Recompile")
                  (return (compile-stem stem flags :target-compile))))))))
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

;;;; Floating-point number reader interceptor

(defvar *choke-on-host-irrationals* t)
(defun install-read-interceptor ()
  ;; Intercept READ to catch inadvertent use of host floating-point literals.
  ;; This prevents regressions in the portable float logic and allows passing
  ;; characters to a floating-point library if we so choose.
  ;; Only do this for new enough SBCL.
  ;; DO-INSTANCE-TAGGED-SLOT was defined circa Nov 2014 and VERSION>= was defined
  ;; ca. Nov 2013, but got moved from SB-IMPL or SB-C (inadvertently perhaps).
  ;; It is not critical that this be enabled on all possible build hosts.
  #+#.(cl:if (cl:and (cl:find-package "SB-C")
                     (cl:find-symbol "SPLIT-VERSION-STRING" "SB-C")
                     (cl:funcall (cl:find-symbol "VERSION>=" "SB-C")
                                 (cl:funcall (cl:find-symbol "SPLIT-VERSION-STRING" "SB-C")
                                             (cl:lisp-implementation-version))
                                 '(1 4 6)))
             '(and)
             '(or))
  (labels ((contains-irrational (x)
             (typecase x
               (cons (or (contains-irrational (car x))
                         (contains-irrational (cdr x))))
               (simple-vector (some #'contains-irrational x))
               ;; We use package literals -- see e.g. SANE-PACKAGE - which
               ;; must be treated as opaque, but COMMAs should not be opaque.
               ;; There are also a few uses of "#.(find-layout)".
               ;; However, the target-num objects should also be opaque
               ;; and, testing for those types before the structure is defined
               ;; is not fun. Other than moving the definitions into here
               ;; from cross-early, there's no good way. But 'chill'
               ;; should not define those structures.
               ((and structure-object (not package))
                (let ((type-name (string (type-of x))))
                  ;; This "LAYOUT" refers to *our* object, not host-sb-kernel:layout.
                  (unless (member type-name '("LAYOUT" "FLOAT" "COMPLEXNUM")
                                  :test #'string=)
                    ;(Format t "visit a ~/host-sb-ext:print-symbol-with-prefix/~%" (type-of x))
                    ;; This generalizes over any structure. I need it because we
                    ;; observe instances of SB-IMPL::COMMA and also HOST-SB-IMPL::COMMA.
                    ;; (primordial-extensions get compiled before 'backq' is installed)
                    (sb-kernel:do-instance-tagged-slot (i x)
                      (when (contains-irrational (sb-kernel:%instance-ref x i))
                        (return-from contains-irrational t))))))
               ((or cl:complex cl:float)
                x)))
           (reader-intercept (f &optional stream (errp t) errval recursive)
             (let* ((form (funcall f stream errp errval recursive))
                    (bad-atom (and (not recursive) ; avoid checking inner forms
                                   (not (eq form errval))
                                   *choke-on-host-irrationals*
                                   (contains-irrational form))))
               (when bad-atom
                 (setq *choke-on-host-irrationals* nil) ; one shot, otherwise tough to debug
                 (error "Oops! didn't expect to read ~s containing ~s" form bad-atom))
               form)))
    (unless (sb-kernel:closurep (symbol-function 'read))
      (sb-int:encapsulate 'read-preserving-whitespace 'protect #'reader-intercept)
      (sb-int:encapsulate 'read 'protect #'reader-intercept)
      (format t "~&; Installed READ interceptor~%"))))
(compile 'install-read-interceptor)

(defvar *math-ops-memoization* (make-hash-table :test 'equal))
(defmacro with-math-journal (&body body)
  `(let* ((table *math-ops-memoization*)
          (memo (cons table (hash-table-count table))))
     (assert (atom table)) ; prevent nested use of this macro
     ;; Don't intercept READ until just-in-time, so that "chill" doesn't
     ;; annoyingly get the interceptor installed.
     (install-read-interceptor)
     (let ((*math-ops-memoization* memo))
       ,@body)
     (when nil ; *compile-verbose*
       (funcall (intern "SHOW-INTERNED-NUMBERS" "SB-IMPL") *standard-output*))
     (when (> (hash-table-count table) (cdr memo))
       (let ((filename "float-math.lisp-expr"))
         (with-open-file (stream filename :direction :output
                                          :if-exists :supersede)
           (funcall (intern "DUMP-MATH-MEMOIZATION-TABLE" "SB-IMPL")
                    table stream))
         (format t "~&; wrote ~a - ~d entries"
                 filename (hash-table-count table))))))
