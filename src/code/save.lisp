;;;; Dump the current Lisp image into a core file. Also contains
;;;; various high-level initialization stuff: loading init files and
;;;; parsing environment variables.
;;;;
;;;; (All the real work is done by C.)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;;; SAVE-LISP-AND-DIE itself

(define-alien-routine "save" (boolean)
  (file c-string)
  (initial-fun (unsigned #.sb!vm:n-word-bits))
  (prepend-runtime int)
  (save-runtime-options int)
  (compressed int)
  (compression-level int)
  (application-type int))

#!+gencgc
(define-alien-routine "gc_and_save" void
  (file c-string)
  (prepend-runtime int)
  (save-runtime-options int)
  (compressed int)
  (compression-level int)
  (application-type int))

#!+gencgc
(defvar sb!vm::*restart-lisp-function*)

(define-condition save-condition (reference-condition)
  ()
  (:default-initargs
   :references (list '(:sbcl :node "Saving a Core Image"))))

(define-condition save-error (error save-condition)
  ()
  (:report "Could not save core."))

(define-condition save-with-multiple-threads-error (save-error)
  ((interactive-thread :initarg :interactive-threads
                       :reader save-with-multiple-threads-error-interactive-threads)
   (other-threads :initarg :other-threads
                  :reader save-with-multiple-threads-error-other-threads))
  (:report (lambda (condition stream)
             (let ((interactive (save-with-multiple-threads-error-interactive-threads condition))
                   (other (save-with-multiple-threads-error-other-threads condition)))
               (format stream  "~@<Cannot save core with multiple threads running.~
                                ~@:_~@:_Interactive thread~P (of current session):~
                                ~@:_~2@T~<~{~A~^, ~}~:>~
                                ~@:_~@:_Other thread~P:~
                                ~@:_~2@T~<~{~A~^, ~}~:>~@:>"
                       (length interactive) (list interactive)
                       (length other) (list other))))))

(defun save-lisp-and-die (core-file-name &key
                                         (toplevel #'toplevel-init)
                                         (executable nil)
                                         (save-runtime-options nil)
                                         (purify t)
                                         (root-structures ())
                                         (environment-name "auxiliary")
                                         (compression nil)
                                         #!+win32
                                         (application-type :console))
  #!+sb-doc
  "Save a \"core image\", i.e. enough information to restart a Lisp
process later in the same state, in the file of the specified name.
Only global state is preserved: the stack is unwound in the process.

The following &KEY arguments are defined:

  :TOPLEVEL
     The function to run when the created core file is resumed. The
     default function handles command line toplevel option processing
     and runs the top level read-eval-print loop. This function returning
     is equivalent to (SB-EXT:EXIT :CODE 0) being called.

     TOPLEVEL functions should always provide an ABORT restart: otherwise
     code they call will run without one.

  :EXECUTABLE
     If true, arrange to combine the SBCL runtime and the core image
     to create a standalone executable.  If false (the default), the
     core image will not be executable on its own. Executable images
     always behave as if they were passed the --noinform runtime option.

  :SAVE-RUNTIME-OPTIONS
     If true, values of runtime options --dynamic-space-size and
     --control-stack-size that were used to start SBCL are stored in
     the standalone executable, and restored when the executable is
     run. This also inhibits normal runtime option processing, causing
     all command line arguments to be passed to the toplevel.
     Meaningless if :EXECUTABLE is NIL.

  :PURIFY
     If true (the default on cheneygc), do a purifying GC which moves all
     dynamically allocated objects into static space. This takes
     somewhat longer than the normal GC which is otherwise done, but
     it's only done once, and subsequent GC's will be done less often
     and will take less time in the resulting core file. See the PURIFY
     function. This parameter has no effect on platforms using the
     generational garbage collector.

  :ROOT-STRUCTURES
     This should be a list of the main entry points in any newly loaded
     systems. This need not be supplied, but locality and/or GC performance
     may be better if they are. Meaningless if :PURIFY is NIL. See the
     PURIFY function.

  :ENVIRONMENT-NAME
     This is also passed to the PURIFY function when :PURIFY is T.
     (rarely used)

  :COMPRESSION
     This is only meaningful if the runtime was built with the :SB-CORE-COMPRESSION
     feature enabled. If NIL (the default), saves to uncompressed core files. If
     :SB-CORE-COMPRESSION was enabled at build-time, the argument may also be
     an integer from -1 to 9, corresponding to zlib compression levels, or T
     (which is equivalent to the default compression level, -1).

  :APPLICATION-TYPE
     Present only on Windows and is meaningful only with :EXECUTABLE T.
     Specifies the subsystem of the executable, :CONSOLE or :GUI.
     The notable difference is that :GUI doesn't automatically create a console
     window. The default is :CONSOLE.

The save/load process changes the values of some global variables:

  *STANDARD-OUTPUT*, *DEBUG-IO*, etc.
    Everything related to open streams is necessarily changed, since
    the OS won't let us preserve a stream across save and load.

  *DEFAULT-PATHNAME-DEFAULTS*
    This is reinitialized to reflect the working directory where the
    saved core is loaded.

SAVE-LISP-AND-DIE interacts with SB-ALIEN:LOAD-SHARED-OBJECT: see its
documentation for details.

On threaded platforms only a single thread may remain running after
SB-EXT:*SAVE-HOOKS* have run. Applications using multiple threads can
be SAVE-LISP-AND-DIE friendly by registering a save-hook that quits
any additional threads, and an init-hook that restarts them.

This implementation is not as polished and painless as you might like:
  * It corrupts the current Lisp image enough that the current process
    needs to be killed afterwards. This can be worked around by forking
    another process that saves the core.
  * There is absolutely no binary compatibility of core images between
    different runtime support programs. Even runtimes built from the same
    sources at different times are treated as incompatible for this
    purpose.
This isn't because we like it this way, but just because there don't
seem to be good quick fixes for either limitation and no one has been
sufficiently motivated to do lengthy fixes."
  #!+gencgc
  (declare (ignore purify root-structures environment-name))
  #!+sb-core-compression
  (check-type compression (or boolean (integer -1 9)))
  #!-sb-core-compression
  (when compression
    (error "Unable to save compressed core: this runtime was not built with zlib support"))
  (when *dribble-stream*
    (restart-case (error "Dribbling to ~s is enabled." (pathname *dribble-stream*))
      (continue ()
        :report "Stop dribbling and save the core."
        (dribble))
      (abort ()
        :report "Abort saving the core."
        (return-from save-lisp-and-die))))
  (when (eql t compression)
    (setf compression -1))
  #!+sb-fasteval (sb!interpreter::flush-everything)
  (tune-hashtable-sizes-of-all-packages)
  (deinit)
  ;; FIXME: Would it be possible to unmix the PURIFY logic from this
  ;; function, and just do a GC :FULL T here? (Then if the user wanted
  ;; a PURIFYed image, he'd just run PURIFY immediately before calling
  ;; SAVE-LISP-AND-DIE.)
  (labels ((restart-lisp ()
             (handling-end-of-the-world
               (reinit)
               #!+hpux (%primitive sb!vm::setup-return-from-lisp-stub)
               (funcall toplevel)))
           (foreign-bool (value)
             (if value 1 0))
           (save-core (gc)
             (let ((name (native-namestring
                          (physicalize-pathname core-file-name)
                          :as-file t)))
               (when gc
                 #!-gencgc (gc)
                 ;; Do a destructive non-conservative GC, and then save a core.
                 ;; A normal GC will leave huge amounts of storage unreclaimed
                 ;; (over 50% on x86). This needs to be done by a single function
                 ;; since the GC will invalidate the stack.
                 #!+gencgc (gc-and-save name
                                        (foreign-bool executable)
                                        (foreign-bool save-runtime-options)
                                        (foreign-bool compression)
                                        (or compression 0)
                                        #!+win32
                                        (ecase application-type
                                          (:console 0)
                                          (:gui 1))
                                        #!-win32 0))
               (without-gcing
                 (save name
                       (get-lisp-obj-address #'restart-lisp)
                       (foreign-bool executable)
                       (foreign-bool save-runtime-options)
                       (foreign-bool compression)
                       (or compression 0)
                       #!+win32
                       (ecase application-type
                         (:console 0)
                         (:gui 1))
                       #!-win32 0)))))
    ;; Save the restart function into a static symbol, to allow GC-AND-SAVE
    ;; access to it even after the GC has moved it.
    #!+gencgc
    (setf sb!vm::*restart-lisp-function* #'restart-lisp)
    (cond #!-gencgc
          (purify
           (purify :root-structures root-structures
                   :environment-name environment-name)
           (save-core nil))
          (t
           (save-core t)))
    ;; Something went very wrong -- reinitialize to have a prayer
    ;; of being able to report the error.
    (reinit)
    (error 'save-error)))

(defun deinit ()
  (call-hooks "save" *save-hooks*)
  #!+sb-wtimer
  (itimer-emulation-deinit)
  (let ((threads (sb!thread:list-all-threads)))
    (unless (= 1 (length threads))
      (let* ((interactive (sb!thread::interactive-threads))
             (other (set-difference threads interactive)))
        (error 'save-with-multiple-threads-error
               :interactive-threads interactive
               :other-threads other))))
  (float-deinit)
  (profile-deinit)
  (foreign-deinit)
  (stream-deinit)
  (deinit-finalizers)
  (drop-all-hash-caches)
  (os-deinit)
  (setf * nil ** nil *** nil
        - nil + nil ++ nil +++ nil
        /// nil // nil / nil))
