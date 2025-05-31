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

#+cheneygc
(define-alien-routine "save" (boolean)
  (file c-string)
  (initial-fun (unsigned #.sb-vm:n-word-bits))
  (prepend-runtime int)
  (save-runtime-options int)
  (compressed int)
  (compression-level int)
  (application-type int))

(define-alien-routine "gc_and_save" void
  (file c-string)
  (prepend-runtime int)
  (purify int)
  (save-runtime-options int)
  (compressed int)
  (compression-level int)
  (application-type int))

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

;;; This variable is accessed by C code when saving. Export it to survive tree-shaker.
;;; The symbols in this set are clobbered just in time to avoid saving them to the core
;;; but not so early that we kill the running image.
(export 'sb-kernel::+save-lisp-clobbered-globals+ 'sb-kernel)
(defconstant-eqx sb-kernel::+save-lisp-clobbered-globals+
    '#(sb-impl::*exit-lock*
       sb-vm::*allocator-mutex*
       sb-thread::*make-thread-lock*
       sb-thread::*initial-thread*
       ;; Saving *JOINABLE-THREADS* could cause catastophic failure on restart.
       ;; SAVE-LISP-AND-DIE should have cleaned up, but there's a timing problem
       ;; with the finalizer thread, and I'm loathe to put in a SLEEP delay.
       sb-thread::*joinable-threads*
       sb-thread::*all-threads*
       sb-thread::*session*
       sb-kernel::*gc-epoch*)
  #'equalp)

(defun start-lisp (toplevel callable-exports)
  (if callable-exports
      (named-lambda %start-lisp ()
        (reinit t)
        (dolist (export callable-exports)
          (sb-alien::initialize-alien-callable-symbol export)))
      (named-lambda %start-lisp ()
        (handling-end-of-the-world
          (reinit t)
          (funcall toplevel)))))

(defun save-lisp-and-die (core-file-name &key
                                         (toplevel #'toplevel-init toplevel-supplied)
                                         (executable nil)
                                         (save-runtime-options nil)
                                         (callable-exports ())
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
     If :ACCEPT-RUNTIME-OPTIONS then --dynamic-space-size and
     --control-stack-size are still processed by the runtime.
     Meaningless if :EXECUTABLE is NIL.

  :CALLABLE-EXPORTS
     This should be a list of symbols to be initialized to the
     appropriate alien callables on startup. All exported symbols should
     be present as global symbols in the symbol table of the runtime
     before the saved core is loaded. When this list is non-empty, the
     :TOPLEVEL argument cannot be supplied.

  :PURIFY
     If true (the default), then some objects in the restarted core will
     be memory-mapped as read-only. Among those objects are numeric vectors
     that were determined to be compile-time constants, and any immutable
     values according to the language specification such as symbol names.

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
     an integer from -7 to 22, corresponding to zstd compression levels, or T
     (which is equivalent to the default compression level, 9).

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
  (declare (ignorable root-structures))
  (when (and callable-exports toplevel-supplied)
    (error ":TOPLEVEL cannot be supplied when there are callable exports."))
  ;; If the toplevel function is not defined, this will signal an
  ;; error before saving, not at startup time.
  (let ((toplevel (%coerce-callable-to-fun toplevel))
        *streams-closed-by-slad*)
    #+sb-core-compression
    (check-type compression (or boolean (integer -7 22)))
    #-sb-core-compression
    (when compression
      (error "Unable to save compressed core: this runtime was not built with zstd support"))
    (when *dribble-stream*
      (restart-case (error "Dribbling to ~s is enabled." (pathname *dribble-stream*))
        (continue ()
          :report "Stop dribbling and save the core."
          (dribble))
        (abort ()
          :report "Abort saving the core."
          (return-from save-lisp-and-die))))
    (when (eql t compression)
      (setf compression 9))
    (flet ((foreign-bool (value)
             (if value 1 0)))
      (let ((name (native-namestring (physicalize-pathname core-file-name)
                                     :as-file t))
            (startfun (start-lisp toplevel callable-exports)))
        (deinit)
        #+generational
        (progn
          ;; Scan roots as close as possible to GC-AND-SAVE, in case anything
          ;; prior causes compilation to occur into immobile space.
          ;; Failing to see all immobile code would miss some relocs.
          ;; FIXME: this could work on non-x86, but it doesn't right now.
          #+(and x86-64 immobile-code) (sb-vm::choose-code-component-order root-structures)
          ;; Must clear this cache if asm routines are movable.
          (setq sb-disassem::*assembler-routines-by-addr* nil
                ;; and save some space by deleting the instruction decoding table
                ;; which can be rebuilt on demand. Must be done after DEINIT
                ;; and CHOOSE-CODE-COMPONENT-ORDER both of which disassemble.
                sb-disassem::*disassem-inst-space* nil)
          ;; Save the restart function. Logically a passed argument, but can't be,
          ;; as it would require pinning around the whole save operation.
          (with-pinned-objects (startfun)
            (setf lisp-init-function (get-lisp-obj-address startfun)))
          ;; Do a destructive non-conservative GC, and then save a core.
          ;; A normal GC will leave huge amounts of storage unreclaimed
          ;; (over 50% on x86). This needs to be done by a single function
          ;; since the GC will invalidate the stack.
          (sb-kernel::unsafe-clear-roots sb-vm:+highest-normal-generation+)
          (gc-and-save name
                       (foreign-bool executable)
                       (foreign-bool purify)
                       (case save-runtime-options
                         (:accept-runtime-options 2)
                         ((nil) 0)
                         (t 1))
                       (foreign-bool compression)
                       (or compression 0)
                       #+win32 (ecase application-type (:console 0) (:gui 1))
                       #-win32 0)
          (setf lisp-init-function 0)) ; only reach here on save error
        #-generational
        (progn
          ;; Coalescing after GC will do no good - the un-needed dups
          ;; of things won't actually go away. Do it before.
          (alien-funcall (extern-alien "coalesce_similar_objects"
                                       (function void)))
          (if purify (purify :root-structures root-structures) (gc))
          (without-gcing
            (save name
                  (get-lisp-obj-address startfun)
                  (foreign-bool executable)
                  (foreign-bool save-runtime-options)
                  (foreign-bool compression)
                  (or compression 0)
                  #+win32 (ecase application-type (:console 0) (:gui 1))
                  #-win32 0)))))

    ;; Something went very wrong -- reinitialize to have a prayer
    ;; of being able to report the error.
    (restore-fd-streams)
    (reinit nil)
    (error 'save-error)))

(defun tune-image-for-dump ()
  ;; C code will GC again (nonconservatively if pertinent), but the coalescing
  ;; steps done below will be more efficient if some junk is removed now.
  (gc :full t)

  ;; Share EQUALP FUN-INFOs
  (let ((ht (make-hash-table :test 'equalp)))
    (sb-int:call-with-each-globaldb-name
     (lambda (name)
       (binding* ((info (info :function :info name) :exit-if-null)
                  (shared-info (gethash info ht)))
         (if shared-info
             (setf (info :function :info name) shared-info)
             (setf (gethash info ht) info))))))

  ;; Don't try to assign header slots of code objects. Any of them could be in
  ;; readonly space. It's not worth the trouble to try to figure out which aren't.
  #-cheneygc (sb-c::coalesce-debug-info) ; Share even more things

  #+sb-fasteval (sb-interpreter::flush-everything)
  (tune-hashset-sizes-of-all-packages))

(defun deinit ()
  (call-hooks "save" *save-hooks*)
  #+win32 (itimer-emulation-deinit)
  #+sb-thread
  (let (error)
    (with-system-mutex (sb-thread::*make-thread-lock*)
      (finalizer-thread-stop)
      (sb-thread::%dispose-thread-structs)
      (let ((threads (sb-thread:list-all-threads))
            (starting
             (setq sb-thread::*starting-threads* ; ordinarily pruned in MAKE-THREAD
                   (delete 0 sb-thread::*starting-threads*)))
            (joinable sb-thread::*joinable-threads*))
        (when (or (cdr threads) starting joinable)
          (let* ((interactive (sb-thread::interactive-threads))
                 (other (union (set-difference threads interactive)
                               (union starting joinable))))
            (setf error (make-condition 'save-with-multiple-threads-error
                                        :interactive-threads interactive
                                        :other-threads other))))))
    (when error (error error))
    #+allocator-metrics (setq sb-thread::*allocator-metrics* nil)
    (setq sb-thread::*sprof-data* nil))
  (tune-image-for-dump)
  (float-deinit)
  (profile-deinit)
  (foreign-deinit)
  (when (zerop (hash-table-count sb-kernel::*forward-referenced-layouts*))
    ;; I think this table should always be empty but I'm not sure. If it is,
    ;; recreate it so that we don't preserve an empty vector taking up 16KB
    (setq sb-kernel::*forward-referenced-layouts* (make-hash-table :test 'equal)))
  ;; Clean up the simulated weak list of covered code components.
  (rplacd *code-coverage-info*
          (delete-if-not #'weak-pointer-value (cdr *code-coverage-info*)))
  (sb-kernel::rebuild-ctype-hashsets)
  (drop-all-hash-caches)
  (os-deinit)
  (finalizers-deinit)
  ;; Try to shrink the pathname cache. It might be largely nulls
  (rebuild-pathname-cache)
  (sb-vm::restore-cpu-specific-routines)
  ;; Do this last, to have some hope of printing if we need to.
  (stream-deinit)
  (setf sb-c::*compile-elapsed-time* 0
        sb-c::*compile-file-elapsed-time* 0)
  (setf * nil ** nil *** nil
        - nil + nil ++ nil +++ nil
        /// nil // nil / nil))

(in-package "SB-C")

(defun coalesce-debug-info ()
  ;; Discard the uncompacted fun map cache.
  (setq sb-di::*uncompacted-fun-maps* nil)
  ;; Discard the debugger's cached mapping of debug functions.
  (setq sb-di::*compiled-debug-funs* nil)
  (flet ((debug-source= (a b)
           (and (equalp a b)
                ;; Case sensitive
                (equal (debug-source-plist a) (debug-source-plist b)))))
    ;; Coalesce the following:
    ;;  DEBUG-INFO-SOURCE, SIMPLE-FUN-ARGLIST, SIMPLE-FUN-TYPE
    ;; FUN-NAMES-EQUALISH considers any two string= gensyms as EQ.
    (let ((source-ht (make-hash-table :test 'equal))
          (arglist-hash (make-hash-table :hash-function 'sb-impl::equal-hash
                                         :test 'sb-impl::fun-names-equalish))
          (type-hash (make-hash-table :test 'equal)))
      (sb-vm:map-allocated-objects
       (lambda (obj widetag size)
         (declare (ignore size))
         (case widetag
           (#.sb-vm:code-header-widetag
            (dotimes (i (if (compiled-debug-info-p (%code-debug-info obj))
                            (code-n-entries obj)
                            0))
              ;; FIXME: now that the metadata are not physically in the code primitive
              ;; object, wouldn't it be better to process the debug-info for deduplication
              ;; rather than treating the code as if it contained the displaced slots?
              (let* ((fun (%code-entry-point obj i))
                     (arglist (%simple-fun-arglist fun))
                     (info (%simple-fun-info fun))
                     (type (typecase info
                             ((cons t simple-vector) (car info))
                             ((not simple-vector) info)))
                     (type  (ensure-gethash type type-hash type))
                     (xref (%simple-fun-xrefs fun)))
                (setf (%simple-fun-arglist fun)
                      (ensure-gethash arglist arglist-hash arglist))
                (setf (%simple-fun-info fun)
                      (if (and type xref) (cons type xref) (or type xref))))))
           (#.sb-vm:instance-widetag
            (typecase obj
              (compiled-debug-info
               (let ((source (compiled-debug-info-source obj)))
                 (typecase source
                   (core-debug-source)  ; skip - uh, why?
                   (debug-source
                    (let* ((namestring (debug-source-namestring source))
                           (canonical-repr
                             (find-if (lambda (x) (debug-source= x source))
                                      (gethash namestring source-ht))))
                      (cond ((not canonical-repr)
                             (push source (gethash namestring source-ht)))
                            ((neq source canonical-repr)
                             (setf (compiled-debug-info-source obj)
                                   canonical-repr))))))))
              (sb-lockless::linked-list
               ;; In the normal course of execution, incompletely deleted nodes
               ;; exist only for a brief moment, as the next operation on the list by
               ;; any thread that touches the logically deleted node can fully delete it.
               ;; If somehow we get here and there are in fact pending deletions,
               ;; they must be finished or else bad things can happen, since 'coreparse'
               ;; can not deal with the untagged pointer convention.
               (sb-lockless::finish-incomplete-deletions obj))))))
       :all))))

(in-package "SB-VM")

;;; Return the caller -> callee graph as an array grouped by caller.
;;; i.e. each element is (CALLING-CODE-COMPONENT . CODE-COMPONENT*)).
;;; A call is assumed only if we see a function or fdefn in the calling
;;; component. This underestimates the call graph of course,
;;; because it's impossible to predict whether calls occur through symbols,
;;; arrays of functions, or anything else. But it's a good approximation.
(defun compute-direct-call-graph (&optional verbose)
  (let ((graph (make-array 10000 :adjustable t :fill-pointer 0))
        (gf-code-cache (make-hash-table :test 'eq))
        (n-code-objs 0))
    (labels ((get-gf-code (gf)
               (ensure-gethash
                gf gf-code-cache
                (let (result)
                  (dolist (method (sb-mop:generic-function-methods gf) result)
                    (let ((fun (sb-mop:method-function method)))
                      (if (typep fun 'sb-pcl::%method-function)
                          (setq result
                                (list* (code-from-fun (sb-pcl::%method-function-fast-function fun))
                                       (code-from-fun (%funcallable-instance-fun fun))
                                       result))
                          (pushnew (code-from-fun fun) result)))))))
             (code-from-fun (fun)
               (ecase (%fun-pointer-widetag fun)
                 (#.simple-fun-widetag
                  (fun-code-header fun))
                 (#.funcallable-instance-widetag
                  (code-from-fun (%funcallable-instance-fun fun)))
                 (#.closure-widetag
                  (fun-code-header (%closure-fun fun))))))
      (map-allocated-objects
       (lambda (obj type size)
         obj size
         (when (and (= type code-header-widetag)
                    (plusp (code-n-entries obj)))
           (incf n-code-objs)
           (let (list)
             (loop for j from code-constants-offset
                   below (code-header-words obj)
                   do (let* ((const (code-header-ref obj j))
                             (fun (typecase const
                                    (fdefn (fdefn-fun const))
                                    (function const))))
                        (when fun
                          (if (typep fun 'generic-function)
                              ;; Don't claim thousands of callees
                              (unless (and (typep const 'fdefn)
                                           (eq (fdefn-name const) 'print-object))
                                (setf list (union (copy-list (get-gf-code fun))
                                                  list)))
                              (pushnew (code-from-fun fun) list :test 'eq)))))
             (when list
               (vector-push-extend (cons obj list) graph)))))
       :immobile))
    (when verbose
      (format t "~&Call graph: ~D nodes, ~D with out-edges, max-edges=~D~%"
              n-code-objs
              (length graph)
              (reduce (lambda (x y) (max x (length (cdr y))))
                      graph :initial-value 0)))
    graph))

;;; Return list of code components ordered in a quasi-predictable way,
;;; provided that LOAD happened in a most 1 thread.
;;; In general: user code sorts before system code, never-called code sorts
;;; to the end, and ties are impossible due to uniqueness of serial#.
(defun deterministically-sort-immobile-code ()
  (let ((forward-graph (compute-direct-call-graph))
        (reverse-graph (make-hash-table :test 'eq))
        (ranking))
    ;; Compute the inverted call graph as a hash-table
    ;; for O(1) lookup of callers of any component.
    (dovector (item forward-graph)
      (let ((caller (car item))
            (callees (cdr item)))
        (dolist (callee callees)
          (push caller (gethash callee reverse-graph)))))
    ;; Compute popularity of each code component in text space
    (map-allocated-objects
     (lambda (obj type size)
       (declare (ignore size))
       (when (and (= type code-header-widetag)
                  (plusp (code-n-entries obj))
                  (immobile-space-addr-p (get-lisp-obj-address obj)))
         (push (cons (length (gethash obj reverse-graph)) obj) ranking)))
     :immobile)
    ;; Sort by a 4-part key:
    ;; -  1 bit  : 0 = ever called, 1 = apparently un-called
    ;; -  1 bit  : system/non-system source file (system has lower precedence)
    ;; -  8 bits : popularity (as computed above)
    ;; - 32 bits : code component serial# (as stored on creation)
    (flet ((calc-key (item &aux (code (cdr item)))
             (let ((systemp
                    (or (let ((di (%code-debug-info code)))
                          (and (typep di 'sb-c::compiled-debug-info)
                               (let ((src (sb-c::compiled-debug-info-source di)))
                                 (and (typep src 'sb-c::debug-source)
                                      (let ((str (sb-c::debug-source-namestring src)))
                                        (if (= (mismatch str "SYS:") 4) 1))))))
                        0))
                   ;; cap the popularity index to 255 and negate so that higher
                   ;; sorts earlier
                   (popularity (- 255 (min (car item) 255)))
                   (serialno (sb-impl::%code-serialno code)))
               (logior (ash (if (= (car item) 0) 1 0) 41)
                       (ash systemp 40)
                       (ash popularity 32)
                       serialno))))
      (mapcar #'cdr (sort ranking #'< :key #'calc-key)))))

#+nil
(defun order-by-in-degree ()
  (let ((compiler-stuff (make-hash-table :test 'eq))
        (other-stuff (make-hash-table :test 'eq)))
    (flet ((pick-table (fun-name)
             (if (symbolp fun-name)
                 (let ((package (symbol-package fun-name)))
                   (if (member package
                               (load-time-value
                                (cons sb-assem::*backend-instruction-set-package*
                                      (mapcar 'find-package
                                              '("SB-C" "SB-VM" "SB-FASL"
                                                "SB-ASSEM" "SB-DISASSEM"
                                                "SB-REGALLOC")))
                                t))
                       compiler-stuff
                       other-stuff))
                 other-stuff))
           (hashtable-keys-sorted (table)
             (mapcar #'car
              (sort (%hash-table-alist table)
                    (lambda (a b)
                      (cond ((> (cdr a) (cdr b)) t) ; higher in-degree
                            ((< (cdr a) (cdr b)) nil) ; lower in-degree
                            ;; break ties by name, and failing that,
                            ;; by address (which = random)
                            (t
                             (let ((name1
                                    (%simple-fun-name (%code-entry-point (car a) 0)))
                                   (name2
                                    (%simple-fun-name (%code-entry-point (car b) 0))))
                               (if (and (symbolp name1) (symbol-package name1)
                                        (symbolp name2) (symbol-package name2))
                                   (let ((p1 (package-name (symbol-package name1)))
                                         (p2 (package-name (symbol-package name2))))
                                     (cond ((string< p1 p2) t)
                                           ((string> p1 p2) nil)
                                           ((string< name1 name2))))
                                   (< (get-lisp-obj-address (car a))
                                      (get-lisp-obj-address (car b))))))))))))
      (sb-vm:map-allocated-objects
       (lambda (obj type size)
         size
         (when (= type sb-vm:code-header-widetag)
           (loop for i from sb-vm:code-constants-offset
                 below (code-header-words obj)
                 do (let ((ref (code-header-ref obj i))
                          (fun))
                      (when (and (fdefn-p ref)
                                 (simple-fun-p (setq fun (fdefn-fun ref)))
                                 (immobile-space-obj-p fun))
                        (let* ((code (fun-code-header fun))
                               (ht (pick-table (%simple-fun-name
                                                (%code-entry-point code 0)))))
                          (incf (gethash code ht 0))))))))
       :immobile)
      (append (hashtable-keys-sorted other-stuff)
              (hashtable-keys-sorted compiler-stuff)))))

;;; Passing your own toplevel functions as the root set
;;; will encourage the defrag procedure to place them early
;;; in the space, which should be better than leaving the
;;; organization to random chance.
;;; Note that these aren't roots in the GC sense, just a locality sense.
#+immobile-code
(defun choose-code-component-order (&optional roots)
  (declare (ignore roots))
  (let ((ordering (make-array 10000 :adjustable t :fill-pointer 0))
        (hashset (make-hash-table :test 'eq)))

    (labels ((emplace (code)
               (unless (gethash code hashset)
                 (setf (gethash code hashset) t)
                 (vector-push-extend code ordering)))
             (visit (thing)
               (typecase thing
                 (code-component (visit-code thing))
                 (simple-fun (visit-code (fun-code-header thing)))
                 (closure (visit (%closure-fun thing)))
                 (symbol (when (and (fboundp thing)
                                    (not (special-operator-p thing))
                                    (not (macro-function thing)))
                           (visit (symbol-function thing))))))
             (visit-code (code-component)
               (when (or (not (immobile-space-obj-p code-component))
                         (gethash code-component hashset))
                 (return-from visit-code))
               (setf (gethash code-component hashset) t)
               (vector-push-extend code-component ordering)
               (loop for i from sb-vm:code-constants-offset
                     below (code-header-words code-component)
                     do (let ((obj (code-header-ref code-component i)))
                          (typecase obj
                            (fdefn  (awhen (fdefn-fun obj) (visit it)))
                            (symbol (visit obj))
                            (vector (map nil #'visit obj)))))))

      ;; Place assembler routines first.
      (emplace sb-fasl:*assembler-routines*)
      ;; Place functions called by assembler routines next.
      (dovector (f +static-fdefns+)
        (emplace (fun-code-header (symbol-function f))))
      #+nil
      (mapc #'visit
            (mapcan (lambda (x)
                      (let ((f (coerce x 'function)))
                        (when (simple-fun-p f)
                          (list (fun-code-header f)))))
                    (or roots '(read eval print compile))))

      (mapc #'emplace (deterministically-sort-immobile-code))

      (map-allocated-objects
       (lambda (obj type size)
         (declare (ignore size))
         (when (and (= type code-header-widetag)
                    (not (typep (%code-debug-info obj) 'function)))
           (emplace obj)))
       :immobile))

    (let* ((n (length ordering))
           (array (make-alien unsigned (1+ (* n 2)))))
      (loop for i below n
            do (setf (deref array (* i 2)) (get-lisp-obj-address (aref ordering i))))
      (setf (deref array (* n 2)) 0) ; null-terminate the array
      (setf (extern-alien "code_component_order" unsigned)
            (sap-int (alien-value-sap array)))))

  (multiple-value-bind (index relocs) (collect-immobile-code-relocs)
    (let* ((n (length index))
           (array (make-alien unsigned n)))
      (dotimes (i n) (setf (deref array i) (aref index i)))
      (setf (extern-alien "immobile_space_reloc_index" unsigned)
            (sap-int (alien-value-sap array))))
    (let* ((n (length relocs))
           (array (make-alien unsigned n)))
      (dotimes (i n) (setf (deref array i) (aref relocs i)))
      (setf (extern-alien "immobile_space_relocs" unsigned)
            (sap-int (alien-value-sap array))))))
