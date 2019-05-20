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

(in-package "SB-IMPL")

;;;; SAVE-LISP-AND-DIE itself

#-gencgc
(define-alien-routine "save" (boolean)
  (file c-string)
  (initial-fun (unsigned #.sb-vm:n-word-bits))
  (prepend-runtime int)
  (save-runtime-options int)
  (compressed int)
  (compression-level int)
  (application-type int))

#+gencgc
(define-alien-routine "gc_and_save" void
  (file c-string)
  (prepend-runtime int)
  (save-runtime-options int)
  (compressed int)
  (compression-level int)
  (application-type int))

#+gencgc
(define-alien-variable "lisp_init_function" (unsigned #.sb-vm:n-machine-word-bits))

(define-condition save-condition (reference-condition)
  ()
  (:default-initargs
   :references '((:sbcl :node "Saving a Core Image"))))

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
                                         #+win32
                                         (application-type :console))
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
     may be better if they are. This has two different but related meanings:
     If :PURIFY is true - and only for cheneygc - the root structures
     are those which anchor the set of objects moved into static space.
     On gencgc - and only on platforms supporting immobile code - these are
     the functions and/or function-names which commence a depth-first scan
     of code when reordering based on the statically observable call chain.
     The complete set of reachable objects is not affected per se.
     This argument is meaningless if neither enabling precondition holds.

  :ENVIRONMENT-NAME
     This has no purpose; it is accepted only for legacy compatibility.

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
  (declare (ignore environment-name))
  #+gencgc
  (declare (ignore purify) (ignorable root-structures))
  ;; If the toplevel function is not defined, this will signal an
  ;; error before saving, not at startup time.
  (let ((toplevel (%coerce-callable-to-fun toplevel)))
    #+sb-core-compression
    (check-type compression (or boolean (integer -1 9)))
    #-sb-core-compression
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
    (labels ((restart-lisp ()
               (handling-end-of-the-world
                 (reinit)
                 ;; REINIT can not discern between a restarted image and a
                 ;; failure to save. It doesn't make a lot of sense to start
                 ;; a finalizer thread in the failed case, so we set the flag
                 ;; here, not in REINIT which would do it for both cases.
                 #+sb-thread (setq *finalizer-thread* t)
                 #+hpux (%primitive sb-vm::setup-return-from-lisp-stub)
                 (funcall toplevel)))
             (foreign-bool (value)
               (if value 1 0)))
      (let ((name (native-namestring (physicalize-pathname core-file-name)
                                     :as-file t)))
        (deinit)
        ;; FIXME: Would it be possible to unmix the PURIFY logic from this
        ;; function, and just do a GC :FULL T here? (Then if the user wanted
        ;; a PURIFYed image, he'd just run PURIFY immediately before calling
        ;; SAVE-LISP-AND-DIE.)
        #+gencgc
        (progn
          ;; Scan roots as close as possible to GC-AND-SAVE, in case anything
          ;; prior causes compilation to occur into immobile space.
          ;; Failing to see all immobile code would miss some relocs.
          #+immobile-code (sb-vm::choose-code-component-order root-structures)
          ;; Save the restart function. Logically a passed argument, but can't be,
          ;; as it would require pinning around the whole save operation.
          (with-pinned-objects (#'restart-lisp)
            (setf lisp-init-function (get-lisp-obj-address #'restart-lisp)))
          ;; Do a destructive non-conservative GC, and then save a core.
          ;; A normal GC will leave huge amounts of storage unreclaimed
          ;; (over 50% on x86). This needs to be done by a single function
          ;; since the GC will invalidate the stack.
          (gc-and-save name
                       (foreign-bool executable)
                       (foreign-bool save-runtime-options)
                       (foreign-bool compression)
                       (or compression 0)
                       #+win32 (ecase application-type (:console 0) (:gui 1))
                       #-win32 0)
          (setf lisp-init-function 0)) ; only reach here on save error
        #-gencgc
        (progn
          ;; Coalescing after GC will do no good - the un-needed dups
          ;; of things won't actually go away. Do it before.
          (alien-funcall (extern-alien "coalesce_similar_objects"
                                       (function void)))
          (if purify (purify :root-structures root-structures) (gc))
          (without-gcing
            (save name
                  (get-lisp-obj-address #'restart-lisp)
                  (foreign-bool executable)
                  (foreign-bool save-runtime-options)
                  (foreign-bool compression)
                  (or compression 0)
                  #+win32 (ecase application-type (:console 0) (:gui 1))
                  #-win32 0)))))

    ;; Something went very wrong -- reinitialize to have a prayer
    ;; of being able to report the error.
    (reinit)
    (error 'save-error)))

(defun tune-image-for-dump ()
  ;; C code will GC again (nonconservatively if pertinent), but the coalescing
  ;; steps done below will be more efficient if some junk is removed now.
  #+gencgc (gc :full t)

  ;; Share EQUALP FUN-INFOs
  (let ((ht (make-hash-table :test 'equalp)))
    (sb-int:call-with-each-globaldb-name
     (lambda (name)
       (binding* ((info (info :function :info name) :exit-if-null)
                  (shared-info (gethash info ht)))
         (if shared-info
             (setf (info :function :info name) shared-info)
             (setf (gethash info ht) info))))))
  (sb-c::coalesce-debug-info) ; Share even more things
  #+sb-fasteval (sb-interpreter::flush-everything)
  (tune-hashtable-sizes-of-all-packages))

(defun deinit ()
  (call-hooks "save" *save-hooks*)
  #+sb-wtimer
  (itimer-emulation-deinit)
  ;; Terminate finalizer thread now, especially given that the thread runs
  ;; user-supplied code that might not even work in later steps of deinit.
  ;; See also the comment at definition of THREAD-EPHEMERAL-P.
  #+sb-thread (finalizer-thread-stop)
  (let ((threads (sb-thread:list-all-threads)))
    (unless (= 1 (length threads))
      (let* ((interactive (sb-thread::interactive-threads))
             (other (set-difference threads interactive)))
        (error 'save-with-multiple-threads-error
               :interactive-threads interactive
               :other-threads other))))
  (tune-image-for-dump)
  (float-deinit)
  (profile-deinit)
  (foreign-deinit)
  (finalizers-deinit)
  (fill *pathnames* nil)
  ;; Clean up the simulated weak list of covered code components.
  (rplacd sb-c:*code-coverage-info*
          (delete-if-not #'weak-pointer-value (cdr sb-c:*code-coverage-info*)))
  ;; Clearing the hash caches must be done after coalescing ctype instances
  ;; because coalescing compares by TYPE= which creates more cache entries.
  (coalesce-ctypes)
  (drop-all-hash-caches)
  ;; Must clear this cache if asm routines are movable.
  (setq sb-disassem::*assembler-routines-by-addr* nil)
  (os-deinit)
  ;; Perform static linkage. Functions become un-statically-linked
  ;; on demand, for TRACE, redefinition, etc.
  #+immobile-code (sb-vm::statically-link-core)
  ;; Do this last, to have some hope of printing if we need to.
  (stream-deinit)
  (setf * nil ** nil *** nil
        - nil + nil ++ nil +++ nil
        /// nil // nil / nil))

;;; Try to produce a unique representative of each ctype in memory as
;;; compared by TYPE=, redirecting references on to the chosen representative.
;;; In the base SBCL image this removes about 400 ctypes instances.
;;; When saving a large application it can (and does) remove thousands more.
;;; This is actually not about space saving, but reducing non-determinism.
;;; Because of the random nature of the type caches (using opaque hashes that
;;; are generated based on memory address) it's totally arbitrary when we create
;;; new instances of ctypes. Coalescing tries to make it less so.  As to
;;; reproducibility, the fact that type-hash-value is an unintelligent key
;;; is a big problem. I can't think of how to easily make it intelligent,
;;; but it might work to zero them all out, and restore the hash on demand
;;; (much the way symbol-hash is lazily computed) which ought to be fine
;;; since all hash caches start out empty.
;;;
;;; To make this even more aggressive, it should coalesce "bottom up"
;;; so that ctypes contained in other ctypes would be uniquified first.
;;; The algorithm is too naive to do that at present.
;;;
;;; Doing too much consing within MAP-ALLOCATED-OBJECTS can lead to heap
;;; exhaustion (due to inhibited GC), so this takes several passes.
(defun coalesce-ctypes (&optional verbose)
  (let* ((table (make-hash-table :test 'equal))
         interned-ctypes
         referencing-objects)
    (labels ((interesting-subpart-p (part)
               ;; Heap objects can point to "dead" stack objects - those
               ;; from a no-longer-existing stack frame - so only examine
               ;; outgoing references within the dynamic space.
               ;; As to why the pointing object didn't die - who knows?
               (and (eq (heap-allocated-p part) :dynamic)
                    (typep part 'ctype)
                    ;; PART is not interesting if it points to an interned
                    ;; ctype, because that's already a canonical object.
                    (not (minusp (type-hash-value part)))))
             (coalesce (type &aux
                             ;; Deal with ctypes instances whose unparser fails.
                             (spec (ignore-errors (type-specifier type))))
               ;; There are ctypes that unparse to the same s-expression
               ;; but are *NOT* TYPE=. Some examples:
               ;;   classoid LIST  vs UNION-TYPE LIST  = (OR CONS NULL)
               ;;   classoid FLOAT vs UNION-TYPE FLOAT = (OR SINGLE-FLOAT DOUBLE-FLOAT)
               ;;   classoid REAL  vs UNION-TYPE REAL  = (OR FLOAT RATIONAL)
               ;;   classoid RATIO vs INTERSECTION-TYPE RATIO = (AND RATIONAL (NOT INTEGER))
               (if spec
                   (dolist (choice (gethash spec table)
                                   (progn (push type (gethash spec table))
                                          type))
                     (when (type= choice type)
                       (return choice)))
                   type)))
      ;; Start by collecting interned types, as well as any object that points
      ;; to a ctype.
      ;; Interned ctypes (mostly classoids, but a few others) have the aspect
      ;; that if two specifiers are equal, then they map to the same internal
      ;; object. This does not discount the possibility that some other ctype
      ;; could be EQ to that type, as occurs with array upgrading.
      (sb-vm:map-allocated-objects
       (lambda (obj type size)
         (declare (ignore type size))
         (when (and (typep obj 'ctype) (minusp (type-hash-value obj)))
           (push obj interned-ctypes))
         (macrolet ((examine (form)
                      ;; when the subpart of OBJ is possibly going
                      ;; to get coalesced, then record OBJ.
                      `(when (interesting-subpart-p ,form)
                         (push obj referencing-objects)
                         (return-from skip))))
           ;; Wrap a block named other than NIL since
           ;; DO-REFERENCED-OBJECTS has several named NIL.
           (block skip (sb-vm:do-referenced-object (obj examine)))))
       :all)
      (when verbose
        (format t "Found ~d interned types, ~d referencing objects~%"
                (length interned-ctypes) (length referencing-objects)))
      (dolist (type interned-ctypes)
        (setf (gethash (type-specifier type) table) (list type)))
      (dolist (obj referencing-objects)
        (let (written)
          (macrolet ((examine (form &aux (accessor (if (listp form) (car form))))
                       (cond
                         ((not (listp form))
                          ;; do-closure-values passes an access form that
                          ;; can't be inverted to a writing form
                          `(progn ,form nil))
                         ((eq accessor 'data-vector-ref)
                          `(let ((part ,form))
                             (when (interesting-subpart-p part)
                               (let ((new (coalesce part)))
                                 (unless (eq new part)
                                   (setf (svref obj ,(caddr form)) new
                                         written t))))))
                         ((and (eq accessor '%primitive)
                               (eq (cadr form) 'sb-c:fast-symbol-global-value))
                          `(let ((part ,form))
                             (when (interesting-subpart-p part)
                               ;; just do it - skip the attempt-to-modify check
                               (%set-symbol-global-value obj (coalesce part)))))
                         ((not (memq accessor
                                     '(%closure-fun
                                       symbol-package symbol-name fdefn-name
                                       %numerator %denominator
                                       %realpart %imagpart
                                       %make-lisp-obj ; fdefn referent
                                       ;; hope no weak pointers point at ctypes
                                       weak-pointer-value)))
                          `(let ((part ,form))
                             (when (interesting-subpart-p part)
                               (setf ,form (coalesce part))))))))
            (sb-vm:do-referenced-object (obj examine)
              (simple-vector
               :extend
               (when (and written (eql sb-vm:vector-valid-hashing-subtype
                                       (get-header-data obj)))
                 (setf (svref obj 1) 1)))))))))) ; set need-to-rehash

sb-c::
(defun coalesce-debug-info ()
  (flet ((debug-source= (a b)
           (and (equal (debug-source-plist a) (debug-source-plist b))
                (eql (debug-source-created a) (debug-source-created b))
                (eql (debug-source-compiled a) (debug-source-compiled b)))))
    ;; Coalesce the following:
    ;;  DEBUG-INFO-SOURCE, DEBUG-FUN-NAME
    ;;  SIMPLE-FUN-ARGLIST, SIMPLE-FUN-TYPE
    ;; FUN-NAMES-EQUALISH considers any two string= gensyms as EQ.
    (let ((source-ht (make-hash-table :test 'equal))
          (name-ht (make-hash-table :test 'equal))
          (arglist-hash (make-hash-table :hash-function 'sb-impl::equal-hash
                                         :test 'sb-impl::fun-names-equalish))
          (type-hash (make-hash-table :test 'equal)))
      (sb-vm:map-allocated-objects
       (lambda (obj widetag size)
         (declare (ignore size))
         (case widetag
          (#.sb-vm:code-header-widetag
           (dotimes (i (sb-kernel:code-n-entries obj))
             (let* ((fun (sb-kernel:%code-entry-point obj i))
                    (arglist (%simple-fun-arglist fun))
                    (type (sb-vm::%%simple-fun-type fun)))
               (setf (%simple-fun-arglist fun)
                     (ensure-gethash arglist arglist-hash arglist))
               (setf (sb-kernel:%simple-fun-type fun)
                     (ensure-gethash type type-hash type)))))
          (#.sb-vm:instance-widetag
           (typecase obj
            (compiled-debug-info
             (let ((source (compiled-debug-info-source obj)))
               (typecase source
                 (core-debug-source)    ; skip
                 (debug-source
                  (let* ((namestring (debug-source-namestring source))
                         (canonical-repr
                           (find-if (lambda (x) (debug-source= x source))
                                    (gethash namestring source-ht))))
                    (cond ((not canonical-repr)
                           (push source (gethash namestring source-ht)))
                          ((neq source canonical-repr)
                           (setf (compiled-debug-info-source obj)
                                 canonical-repr)))))))
             (loop for debug-fun = (compiled-debug-info-fun-map obj) then next
                   for next = (sb-c::compiled-debug-fun-next debug-fun)
                   do
                   (binding* ((name (compiled-debug-fun-name debug-fun))
                              ((new foundp) (gethash name name-ht)))
                     (cond ((not foundp)
                            (setf (gethash name name-ht) name))
                           ((neq name new)
                            (setf (%instance-ref debug-fun
                                                 (get-dsd-index compiled-debug-fun name))
                                  new))))
                   while next))
            (sb-lockless::linked-list
             ;; In the normal course of execution, incompletely deleted nodes
             ;; exist only for a brief moment, as the next operation on the list by
             ;; any thread that touches the logically deleted node can fully delete it.
             ;; If somehow we get here and there are in fact pending deletions,
             ;; they must be finished or else bad things can happen, since 'coreparse'
             ;; can not deal with the untagged pointer convention.
             (sb-lockless::finish-incomplete-deletions obj))))))
       :all))))
