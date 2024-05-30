(when (member "--gc-stress" *posix-argv* :test #'equal)
  (push :gc-stress *features*))

(load "test-util.lisp")
(load "assertoid.lisp")
(load "compiler-test-util.lisp")

(defpackage :run-tests
  (:use :cl :test-util :sb-ext))

(in-package run-tests)

(load "colorize.lisp")

(defvar *all-failures* nil)
(defvar *break-on-error* nil)
(defvar *report-skipped-tests* nil)
(defvar *explicit-test-files* nil)
(defvar *input-manifest*)
(defvar *allowed-inputs*)

(load "test-funs")

(defun run-all (&aux (start-time (get-internal-real-time)))
  (let (skip-to)
    (loop :with remainder = (rest *posix-argv*)
          :for arg = (car remainder)
          :while remainder
          :do
          (pop remainder)
          (cond
            ((string= arg "--evaluator-mode")
             (let ((mode (pop remainder)))
               (cond
                 ((string= mode "interpret")
                  (setf *test-evaluator-mode* :interpret))
                 ((string= mode "compile")
                  (setf *test-evaluator-mode* :compile))
                 (t
                  (error "~@<Invalid evaluator mode: ~A. Must be one ~
                           of interpret, compile.~@:>"
                         mode)))))
            ((string= arg "--break-on-failure")
             (setf *break-on-error* t)
             (setf test-util:*break-on-failure* t))
            ((string= arg "--break-on-expected-failure")
             (setf test-util:*break-on-expected-failure* t))
            ((string= arg "--report-skipped-tests")
             (setf *report-skipped-tests* t))
            ((string= arg "--no-color"))
            ((string= arg "--slow")
             (push :slow *features*))
            ((string= arg "--gc-stress"))
            ((string= arg "--skip-to")
             (setf skip-to (pop remainder)))
            (t
             (push (merge-pathnames (parse-namestring arg)) *explicit-test-files*))))
   (setf *explicit-test-files* (nreverse *explicit-test-files*))
   ;; FIXME: randomizing the order tests are run in, especially the "pure" ones,
   ;; might help detect accidental side-effects by inducing failures elsewhere.
   ;; And/or try all permutations using an infinite number of machines.
   (with-open-file (log "test.log" :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
     (let ((pure-load (pure-load-files))
           (pure-cload (pure-cload-files))
           (impure-load (impure-load-files))
           (impure-cload (impure-cload-files))
           (sh (sh-files)))
       (when skip-to
         (cond ((equal skip-to "impure")
                (setf pure-load nil
                      pure-cload nil))
               (t
                (flet ((skip (files)
                         (member skip-to files :key #'file-namestring :test #'equal)))
                  (or (setf pure-load (skip pure-load))
                      (setf pure-cload (skip pure-cload))
                      (setf impure-load (skip impure-load))
                      (setf impure-cload (skip impure-cload))
                      (setf sh (skip sh)))))))
       (pure-runner pure-load 'load-test log)
       (pure-runner pure-cload 'cload-test log)
       (impure-runner impure-load 'load-test log)
       (impure-runner impure-cload 'cload-test log)
       (impure-runner sh 'sh-test log))
     (log-file-elapsed-time "GRAND TOTAL" start-time log))
   (report)
   (sb-ext:exit :code (if (unexpected-failures) 1 104))))

(defun report ()
  (terpri)
  (format t "Finished running tests.~%")
  (let ((skipcount 0)
        (*print-pretty* nil))
    (cond (*all-failures*
           (format t "Status:~%")
           (dolist (fail (reverse *all-failures*))
             (cond ((eq (car fail) :unhandled-error)
                    (output-colored-text (car fail)
                                          " Unhandled Error")
                    (format t " ~a~%"
                            (enough-namestring (second fail))))
                   ((eq (car fail) :invalid-exit-status)
                    (output-colored-text (car fail)
                                          " Invalid exit status:")
                    (format t " ~a~%"
                            (enough-namestring (second fail))))
                   ((eq (car fail) :skipped-disabled)
                    (when *report-skipped-tests*
                      (format t " ~20a ~a / ~a~%"
                              "Skipped (irrelevant):"
                              (enough-namestring (second fail))
                              (third fail)))
                    (incf skipcount))
                   (t
                    (output-colored-text
                     (first fail)
                     (ecase (first fail)
                       (:expected-failure " Expected failure:")
                       (:unexpected-failure " Failure:")
                       (:leftover-thread " Leftover thread (broken):")
                       (:unexpected-success " Unexpected success:")
                       (:skipped-broken " Skipped (broken):")
                       (:skipped-disabled " Skipped (irrelevant):")))
                    (format t " ~a / ~a~%"
                            (enough-namestring (second fail))
                            (third fail)))))
           (when (> skipcount 0)
             (format t " (~a tests skipped for this combination of platform and features)~%"
                     skipcount)))
          (t
           (format t "All tests succeeded~%")))))

(defun log-file-elapsed-time (source-file begin-time log)
  (let ((end-time (get-internal-real-time)))
    (format log "~6d - ~a~%" (- end-time begin-time) source-file)
    (force-output log)))

;;; This is a bit of a hack designed to emulate a sandboxed test runner.
;;; For the sandbox to be properly set up to execute each test, it needs
;;; the names of the files that the test will access for reading.
;;; Without a sandbox, we want to fail in the same way that the sandboxed
;;; executor would, so that we can know when 'input-manifest.lisp-expr'
;;; needs to be edited.
;;; I'm not sure of the best way to allow deliberately nonexistent files
;;; here other than by hardcoding the names we expect.
;;; A more pedantic approach might check whether FILENAME definitely is
;;; a file on disk that was not declared. That's getting a bit insane,
;;; because then we'd have to keep track of directory changes, when this is
;;; only a bare minimum of effort to make sure that the manifest stays in
;;; sync with reality for people who run tests in the ordinary way.
;;;
;;; If *allowed-inputs* is :ANY, then we at worst print a note, and don't
;;; signal an error.  This was the simplest way I could imagine to get tests
;;; to pass when truenames don't match source file names, as long as declared
;;; input filtering has already happened.
;;; If you're using something like https://bazel.build/ then it provides
;;; the sandboxing, and the program under test shouldn't try to add another layer.
;;; e.g. if you have two worker nodes, one of them is in a file tree which for
;;; purposes of a test contains only the files named "run-tests.lisp" and
;;; "foo.pure.lisp", and the other has only "run-tests.lisp" and "bar.pure.lisp",
;;; - and perhaps they share storage - such that all of the files
;;; are actually links:
;;;   run-tests.lisp -> /blah/xyz12431/c984
;;;   foo.pure.lisp  -> /blah/zyz23431/c134
;;;   bar.pure.lisp  -> /blah/fr0bbotz/2344
;;; then everything that the code below tries to do to determine acceptability
;;; of filenames in pretty much broken.
(defun check-manifest (filename)
  ;; We might see:
  ;;  - "data/compile-file-pos.lisp" or
  ;;  - #P"/path/to/sbcl/tests/data/compile-file-pos.lisp"
  ;; The latter is generally from COMPILE-FILE.
  ;; For the latter we could compute the pathname relative to this directory.
  ;; However, for the moment it suffices to just match on the name
  ;; without its directory components.
  (declare (ignorable filename))
  #-win32
  (labels ((stem-of (thing)
             (namestring (make-pathname :name (pathname-name thing)
                                        :type (pathname-type thing))))
           (stem= (a b)
             (string= (stem-of a) (stem-of b)))
           (starts-with-p (string prefix)
             (= (mismatch string prefix) (length prefix)))
           (within-directory-p (path directory)
             (starts-with-p (namestring (merge-pathnames path))
                            (namestring directory))))
    (unless (eq (pathname-host filename) sb-impl::*physical-host*)
      (return-from check-manifest))
    (let ((string (namestring filename)))
      (when (or (find #\* (stem-of filename)) ; wild
                (starts-with-p string "/dev/") ; dev/null and dev/random
                (starts-with-p string "/proc/")
                ;; Temp files created by test-util's scratch file routine
                (starts-with-p (stem-of string) *scratch-file-prefix*)
                ;; These have been accepted as okay for a while.  Test
                ;; files should never explicitly create files in these
                ;; directories, but should always use a scratch file.
                (starts-with-p string "/tmp/")
                (starts-with-p string "/var/tmp/")
                (starts-with-p string "/private/var/folders/")
                (and (boundp '*test-directory*)
                     (within-directory-p filename *test-directory*))
                (string= string "exists")
                (member (stem-of filename) '("compiler-test-util.lisp"
                                             "no-such-file")
                        :test #'string=)
                (string= (pathname-name filename) "i-am-not") ; any extension
                (and (boundp '*allowed-inputs*)
                     (listp *allowed-inputs*)
                     (find filename *allowed-inputs* :test #'stem=)))
        (return-from check-manifest)))
    (if (and (boundp '*allowed-inputs*) (eq *allowed-inputs* :any))
        (format *error-output* "~&Assumed valid input file: ~S" filename)
        (error "Missing input file in manifest: ~S~%" filename))))

(defparameter *ignore-symbol-value-change*
  (flet ((maybe (p s) (and (find-package p)
                           (find-symbol s p))))
    `(sb-c::*code-serialno*
      sb-c::*compile-elapsed-time*
      sb-c::*compile-file-elapsed-time*
      sb-c::*phash-lambda-cache*
      ,(maybe "SB-IMPL" "*RUN-GC-HOOKS*")
      sb-impl::**finalizer-store**
      sb-impl::*finalizer-rehashlist*
      sb-impl::*finalizers-triggered*
      sb-impl::*all-packages*
      sb-impl::*package-names-cookie*
      sb-impl::*available-buffers*
      sb-impl::*token-buf-pool*
      sb-impl::*user-hash-table-tests*
      sb-impl::*pn-dir-table*
      sb-impl::*pn-table*
      sb-vm::*immobile-codeblob-tree*
      sb-vm::*dynspace-codeblob-tree*
      ,(maybe "SB-KERNEL" "*EVAL-CALLS*")
      sb-kernel::*type-cache-nonce*
      sb-ext:*gc-run-time*
      sb-ext:*gc-real-time*
      sb-vm::*code-alloc-count*
      sb-kernel::*gc-epoch*
      sb-int:*n-bytes-freed-or-purified*
      ,(maybe "SB-APROF" "*ALLOCATION-PROFILE-METADATA*")
      ,(maybe "SB-VM" "*BINDING-STACK-POINTER*")
      ,(maybe "SB-VM" "*CONTROL-STACK-POINTER*")
      ,(maybe "SB-THREAD" "*JOINABLE-THREADS*")
      ,(maybe "SB-THREAD" "*STARTING-THREADS*")
      ,(maybe "SB-THREAD" "*SPROF-DATA*")
      ,(maybe "SB-APROF" "*N-PROFILE-SITES*")
      sb-thread::*all-threads*
      ,(maybe "SB-VM" "*FREE-TLS-INDEX*")
      ,(maybe "SB-VM" "*STORE-BARRIERS-POTENTIALLY-EMITTED*")
      ,(maybe "SB-VM" "*STORE-BARRIERS-EMITTED*")
      ,(maybe "SB-INTERPRETER" "*LAST-TOPLEVEL-ENV*")
      ,(maybe "SB-SYS" "*THRUPTION-PENDING*")
      ,(maybe "SB-THREAD" "*ALLOCATOR-METRICS*")
      sb-pcl::*dfun-constructors*
      sb-di::*uncompacted-fun-maps*
      sb-di::*compiled-debug-funs*
      #+win32 sb-impl::*waitable-timer-handle*
      #+win32 sb-impl::*timer-thread*
      sb-unicode::*name->char-buffers*
      sb-impl::*fdefn-of-nil*)))

(defun collect-symbol-values ()
  (let (result)
    (sb-int:drop-all-hash-caches)
    (do-all-symbols (s)
      (when (and (not (keywordp s))
                 (boundp s)
                 (not (constantp s))
                 (sb-int:system-package-p (symbol-package s))
                 (not (member s *ignore-symbol-value-change*)))
        (push (cons s (symbol-value s)) result)))
    result))

(defun compare-symbol-values (expected)
  (sb-int:drop-all-hash-caches)
  (dolist (item expected)
    (let ((val (symbol-value (car item))))
      (unless (eq val (cdr item))
        (error "Symbol value differs: ~S" (car item))))))

(defun safe-gf-p (x)
  (declare (notinline sb-kernel:%fun-layout))
  (and (sb-kernel:funcallable-instance-p x)
       (not (eq (opaque-identity (sb-kernel:%fun-layout x)) 0))
       (sb-pcl::generic-function-p x)))

(defun summarize-generic-functions ()
  (loop for gf in (sb-vm:list-allocated-objects :all :test #'safe-gf-p)
        collect (cons gf
                      (when (and (slot-exists-p gf 'sb-pcl::methods)
                                 (slot-boundp gf 'sb-pcl::methods))
                        (length (sb-mop:generic-function-methods gf))))))


;;; Because sb-pcl::compile-or-load-defgeneric is disabled in pure tests,
;;; there should be no way to cause this to fail in pure tests except by direct
;;; use of ADD-METHOD or REMOVE-METHOD.
(defun compare-gf-summary (expected)
  (dolist (item expected)
    (let* ((gf (car item))
           (n-old (cdr item))
           (n-new (when (and (slot-exists-p gf 'sb-pcl::methods)
                             (slot-boundp gf 'sb-pcl::methods))
                    (length (sb-mop:generic-function-methods gf)))))
      (unless (eql n-old n-new)
        (error "Generic-Function change: ~S (had ~D methods)" gf n-old)))))

(defun tersely-summarize-globaldb ()
  (let* ((symbols-with-properties)
         (types-ht (make-hash-table))
         (setfs-ht (make-hash-table))
         (structure-classoids
          (sb-kernel:classoid-subclasses
           (sb-kernel:find-classoid 'structure-object)))
         (ignored-stream-classoids
          '(sb-gray:fundamental-binary-output-stream
            sb-gray:fundamental-binary-input-stream
            sb-gray:fundamental-binary-stream
            sb-gray:fundamental-character-stream
            sb-gray:fundamental-output-stream
            sb-gray:fundamental-input-stream))
         (standard-classoids
          (sb-kernel:classoid-subclasses
           (sb-kernel:find-classoid 'standard-object)))
         (condition-classoids
          (sb-kernel:classoid-subclasses
           (sb-kernel:find-classoid 'condition))))
    (do-all-symbols (s)
      (when (symbol-plist s)
        (push s symbols-with-properties))
      (when (sb-int:info :type :kind s)
        (setf (gethash s types-ht) t))
      (when (sb-int:info :setf :expander s)
        (setf (gethash s setfs-ht) t)))
    ;; The first two elements of this list are employed to delete new structure
    ;; and condition definitions after the test file is executed.
    ;; The other elements are used only as a comparison to see that nothing else
    ;; was altered in the classoid or type namespace, etc.
    (list (loop for key being each hash-key of structure-classoids
                collect (sb-kernel:classoid-name key))
          (loop for key being each hash-key of condition-classoids
                collect (sb-kernel:classoid-name key))
          (loop for key being each hash-key of standard-classoids
                unless (member (sb-kernel:classoid-name key)
                               ignored-stream-classoids)
                collect (sb-kernel:classoid-name key))
          (sort symbols-with-properties #'string<)
          (loop for key being each hash-key of types-ht collect key)
          (loop for key being each hash-key of setfs-ht collect key)
          (list (sb-impl::info-env-count sb-int:*info-environment*)))))

(defun structureish-classoid-ancestors (classoid)
  (map 'list 'sb-kernel:layout-classoid
       (sb-kernel:layout-inherits (sb-kernel:classoid-layout classoid))))

(defun globaldb-cleanup (initial-packages globaldb-summary)
  ;; Package deletion suffices to undo DEFVAR,DEFTYPE,DEFSETF,DEFUN,DEFMACRO
  ;; but not DEFSTRUCT or DEFCLASS.
  ;; UNDECLARE-STRUCTURE isn't destructive enough for our needs here.
  (flet ((delete-classoids (root saved-names)
           (let ((worklist
                  (loop for key being each hash-key
                     of (sb-kernel:classoid-subclasses (sb-kernel:find-classoid root))
                     unless (member (the (and symbol (not null))
                                         (sb-kernel:classoid-name key))
                                    saved-names)
                     collect key)))
             (dolist (classoid worklist)
               (dolist (ancestor (structureish-classoid-ancestors classoid))
                 (sb-kernel::remove-subclassoid classoid ancestor))
               (let ((direct-supers
                      (mapcar 'sb-kernel:classoid-pcl-class
                              (sb-kernel:classoid-direct-superclasses classoid)))
                     (this-class (sb-kernel:classoid-pcl-class classoid)))
                 (dolist (super direct-supers)
                   (sb-mop:remove-direct-subclass super this-class)))))))
    (delete-classoids 'structure-object (first globaldb-summary))
    (delete-classoids 'condition (second globaldb-summary)))
  (let (delete)
    (dolist (package (list-all-packages))
      (unless (member package initial-packages)
        (push package delete)))
    ;; Do all UNUSE-PACKAGE operations first
    (dolist (package delete)
      (unuse-package (package-use-list package) package))
    ;; Then all deletions
    (mapc 'delete-package delete)
    (when delete
      (format t "::: NOTE: Deleted ~D package~:P~%" (length delete))))
  ;; Remove PRINT-OBJECT methods specialized on uninterned symbols
  (let ((gf #'print-object))
    (dolist (method (sb-mop:generic-function-methods gf))
      (let ((first-specializer
             (class-name (car (sb-mop:method-specializers method)))))
        (unless (symbol-package first-specializer)
          (remove-method gf method)))))
  (loop for x in (cdr globaldb-summary) for y in (cdr (tersely-summarize-globaldb))
        for index from 0
        unless (equal x y)
          do (let ((diff (list (set-difference x y)
                               (set-difference y x))))
               (cond
                 ((equal diff '(nil nil))) ; reordering only, from rehash?
                 ((equal diff '((sb-gray:fundamental-character-output-stream
                                 sb-gray:fundamental-character-input-stream) nil))
                  (warn "Ignoring mystery change to gray stream classoids"))
                 (t
                  (let ((*print-pretty* nil))
                    (error "Mismatch on element index ~D of globaldb snapshot: diffs=~S"
                           index diff)))))))

(defun pure-runner (files test-fun log)
  (unless files
    (return-from pure-runner))
  (unless (boundp '*input-manifest*)
    (with-open-file (manifest "input-manifest.lisp-expr" :if-does-not-exist nil)
      (setf *input-manifest*
            (if manifest (read manifest) :ignore))))
  (format t "// Running pure tests (~a)~%" test-fun)
  (let ((*failures* nil)
        ;; in case somebody corrupts CL-USER's use list, of course
        (standard-use-list (package-use-list "CL-USER")))
    (dolist (file files)
      (format t "// Running ~a in ~a evaluator mode~%"
              file *test-evaluator-mode*)
      (let* ((actually-pure
              (not (or (search ".impure" (namestring file))
                       (search ".impure-cload" (namestring file)))))
             (packages-to-use '("ASSERTOID" "TEST-UTIL"))
             (initial-packages (list-all-packages))
             ;; It is not only permitted to change this GC parameter in tests, it is
             ;; _encouraged_ as a way to show insensitivity to object address stability.
             (gen0-gcs-before-promo (generation-number-of-gcs-before-promotion 0))
             (global-symbol-values (when actually-pure
                                     (collect-symbol-values)))
             (gf-summary (summarize-generic-functions))
             (globaldb-summary (when actually-pure
                                 (tersely-summarize-globaldb)))
             (logical-hosts sb-impl::*logical-hosts*)
             (test-package
              (if actually-pure
                  (make-package
                   (format nil "TEST~36,5,'_R" (random (expt 36 5)))
                   :use (append packages-to-use standard-use-list))
                  (find-package "CL-USER"))))
        (setq *allowed-inputs*
              (if (eq *input-manifest* :ignore)
                  :any
                  (append (cdr (assoc (namestring (make-pathname :name (pathname-name file)
                                                                 :type (pathname-type file)))
                                      *input-manifest* :test #'string=))
                          (list file))))
        (sb-int:encapsulate
         'open 'open-guard
         (lambda (f filename &rest args &key direction &allow-other-keys)
           (when (or (not direction) (eq direction :input))
             (check-manifest filename))
           (apply f filename args)))
        ;; We want to ensure that pure tests remain as pure as possible.
        ;; DEFSTRUCT, DEFCLASS, DEFGENERIC, DEFMETHOD are certainly impure
        ;; as there is no easy way to eradicate after-effects. Supposing that
        ;; one did (SETF (FIND-CLASS 'x) NIL) for each classoid type defined in
        ;; a test, it does not remove from CLASS-DIRECT-SUBCLASSES of the ancestor.
        ;; We need to disallow all those impure macros by shadowing them
        ;; and providing no definition.
        ;; However, parallel execution uses PURE-RUNNER for impure tests,
        ;; so we need to leave the definitions alone in that case.
        ;; DEF{constant,fun,macro,parameter,setf,type,var} are generally ok
        ;; except when DEFfoo defines something too hairy to hang off a symbol.
        (cond (actually-pure
               ;; DEFMACRO and DEFUN are allowed in pure tests because their effect
               ;; is undone by deleting the package. We're pretty good now about not
               ;; polluting any sort of gobal namespace with compiler metadata
               ;; as long as the function is named by just a symbol or (SETF x).
               ;; FIXME: probably should shadow DECLAIM, and reject some if not all
               ;; possible things that could be declaimed.
               ;; Even the impure tests suffer from a DECLAIM leaking into subsequent
               ;; tests within the same file, so it's a more general problem.
               (shadow '("DEFMETHOD"
                         "EXIT"
                         ;; Hiding IN-PACKAGE is a good preventative measure.
                         ;; There are other ways to do nasty things of course.
                         ;; Deliberately violating a package lock has got to be impure.
                         "IN-PACKAGE" "*PACKAGE*" "WITHOUT-PACKAGE-LOCKS")
                       test-package)
               ;; We have pure tests that exercise the DEFCLASS and DEFGENERIC
               ;; macros to generate macroexpansion-time errors.  That's mostly ok.
               ;; We can trap attempts to use SB-KERNEL::%COMPILER-mumble
               ;; functions though.
               (dolist (symbol '(sb-kernel::%compiler-defclass
                                 sb-pcl::compile-or-load-defgeneric))
                 (sb-int:encapsulate symbol 'defblah-guard
                                     (lambda (f &rest args)
                                       (if (eq *package* test-package)
                                           (error "Can't call ~S" f)
                                           (apply f args))))))
              (t
               (use-package packages-to-use test-package)))
        (let ((*package* test-package)
              (sb-impl::*gentemp-counter* sb-impl::*gentemp-counter*)
              (sb-c::*check-consistency* sb-c::*check-consistency*)
              (sb-c:*compile-to-memory-space* sb-c:*compile-to-memory-space*)
              (sb-c::*policy-min* sb-c::*policy-min*)
              (sb-c::*policy-max* sb-c::*policy-max*))
          (restart-case
            (handler-bind ((error (make-error-handler file)))
              (let* ((sb-ext:*evaluator-mode* *test-evaluator-mode*)
                     (*features*
                       (if (eq sb-ext:*evaluator-mode* :interpret)
                           (cons :interpreter *features*)
                           *features*)))
                (let ((start (get-internal-real-time)))
                  (funcall test-fun file)
                  (log-file-elapsed-time file start log))))
            (skip-file ())))
        (sb-impl::disable-stepping)
        (sb-int:unencapsulate 'open 'open-guard)
        (unless (eql (generation-number-of-gcs-before-promotion 0) gen0-gcs-before-promo)
          (format t "~&::: NOTE: nursery space promotion rate restored to nominal~%")
          (setf (generation-number-of-gcs-before-promotion 0) gen0-gcs-before-promo))
        (setq sb-impl::*logical-hosts* logical-hosts)
        (when actually-pure
          (setq sb-disassem::*disassem-inst-space* nil
                sb-disassem::*assembler-routines-by-addr* nil)
          (compare-symbol-values global-symbol-values)
          (globaldb-cleanup initial-packages globaldb-summary)
          (compare-gf-summary gf-summary)
          (dolist (symbol '(sb-pcl::compile-or-load-defgeneric
                            sb-kernel::%compiler-defclass))
            (sb-int:unencapsulate symbol 'defblah-guard)))))
    (makunbound '*allowed-inputs*)
    ;; after all the files are done
    (append-failures)))

(defun run-in-child-sbcl (load eval)
  (process-exit-code
   (sb-ext:run-program
    (first *POSIX-ARGV*)
    (list "--core" SB-INT:*CORE-STRING*
          "--lose-on-corruption"
           "--noinform"
           "--no-sysinit"
           "--no-userinit"
           "--noprint"
           "--disable-debugger"
           #+gc-stress "--eval" #+gc-stress "(push :gc-stress *features*)"
           "--load" load
           "--eval" (write-to-string eval
                                     :right-margin 1000))
    :output t
    :input t)))

(defun run-impure-in-child-sbcl (test-file test-fun)
  (clear-test-status)
  (run-in-child-sbcl
   "impure-runner"
   `(run-tests::run
     ,(enough-namestring test-file)
     ',test-fun
     ,*break-on-failure*
     ,*break-on-expected-failure*
     ,*break-on-error*
     ,(eq *test-evaluator-mode* :interpret)
     ,(and (member :slow *features*)
           t))))

(defun impure-runner (files test-fun log)
  (when files
    (format t "// Running impure tests (~a)~%" test-fun)
    (dolist (file files)
      (force-output)
      (let ((start (get-internal-real-time))
            (exit-code (run-impure-in-child-sbcl file test-fun)))
        (log-file-elapsed-time file start log)
        (if (= exit-code 104)
            (with-open-file (stream #.(merge-pathnames "test-status.lisp-expr"
                                                       *load-pathname*)
                                    :direction :input
                                    :if-does-not-exist :error)
              (append-failures (read stream)))
            (push (list :invalid-exit-status file)
                  *all-failures*))))))

(defun make-error-handler (file)
  (lambda (condition)
    (push (list :unhandled-error file) *failures*)
    (cond (*break-on-error*
           (test-util:really-invoke-debugger condition))
          (t
           (format *error-output* "~&Unhandled ~a: ~a~%"
                   (type-of condition) condition)
           (sb-debug:print-backtrace)))
    (invoke-restart 'skip-file)))

(defun append-failures (&optional (failures *failures*))
  (setf *all-failures* (append failures *all-failures*)))

(defun unexpected-failures ()
  (remove-if (lambda (x)
               (or (eq (car x) :expected-failure)
                   (eq (car x) :unexpected-success)
                   (eq (car x) :skipped-broken)
                   (eq (car x) :skipped-disabled)))
             *all-failures*))

(defun filter-test-files (wild-mask)
  (if *explicit-test-files*
      (loop for file in *explicit-test-files*
            when (pathname-match-p file wild-mask)
            collect file)
      (directory wild-mask
                 ;; If we're in a tree whose non-directories are
                 ;; symlinks, the truenames of those symlinks might
                 ;; not have the same relationships to each other as
                 ;; we need.
                 :resolve-symlinks nil)))

(defun pure-load-files ()
  (filter-test-files "*.pure.lisp"))

(defun pure-cload-files ()
  (filter-test-files "*.pure-cload.lisp"))

(defun impure-load-files ()
  (filter-test-files "*.impure.lisp"))

(defun impure-cload-files ()
  (filter-test-files "*.impure-cload.lisp"))

(defun sh-files ()
  (let ((result (filter-test-files "*.test.sh")))
    #+unix result
    ;; Rather than hack up the shell scripts which don't pass on #-unix
    ;; (which would require at least a few lines of shell script and lisp
    ;; to invoke SBCL and exit with some other code), just confine the kludge
    ;; to this file.
    #-unix
    (if *explicit-test-files*
        result
      (remove-if
       (lambda (x)
         (member (pathname-name x)
                 '("filesys.test" ; too many assertions about symlinks to care about just yet
                   ;; foreign-test-noop-dlclose-test.c:1:10: fatal error: dlfcn.h: No such file or directory
                   "foreign.test"
                   ;; No built SBCL here (.../tests/run-sbcl-test-5863): run 'sh make.sh' first!
                   "run-sbcl.test"
                   "side-effectful-pathnames.test") ; no idea
                 :test 'string=))
        result))))
