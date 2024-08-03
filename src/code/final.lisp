;;;; finalization based on weak Split-Ordered Lists

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;; If loading a weak ref requires a read barrier (as per #+weak-vector-readbarrier)
;;; then the finalizer table based on a split-ordered-list does not work as-is.
;;; It could be made to via a few approaches of varying levels of difficulty:
;;; * make the split-ordered-list contains keys which are weak pointers
;;;   so that _only_ SB-VM:WEAK-POINTER-WIDETAG can weakly reference things.
;;;   Also make the solist hash on the thing inside the weak-pointer
;;;   rather than the address of the weak pointer. Similarly, detect
;;;   address movement if the thing inside the weak pointer moves.
;;; * turn SO-NODE into something that is not STRUCURE-OBJECT so that at least
;;;   we can't accidentally use %INSTANCE-REF to circumvent a read barrier.
;;; * put a read barrier on SO-NODE-KEY, but only for the finalizer table.
;;;   %INSTANCE-REF with an index that is (GET-DSD-INDEX SO-DATA-NODE SO-KEY)
;;;   has to not only re-tag an untagged pointer, but also ensure that a concurrent
;;;   sweep phase isn't in the midst of smashing weak pointers whose referent is
;;;   dead. But there's no good way to do that on arbitrary structure-object slots.
;;;   The vop translator for instance-ref could potentially do the right thing on
;;;   a known structure type, but I would worry that interpreted instance-ref would
;;;   either do the wrong thing, or pessimize _every_ call. And even then,
;;;   compile-time instance-ref could also do the wrong thing unless we make
;;;   the general case of instance-ref assume the worst, namely that it has to
;;;   check whether the slot is weak.
;;; Though the first is conceptually easy, I don't feel like doing anything yet.
;;; Therefore, if #+weak-vector-readbarrier is enabled, simply fall back upon the
;;; old alist of (weakptr . action) which should be adequate for the time being.
#+weak-vector-readbarrier
(progn
  (define-load-time-global **finalizer-store** nil)
  (declaim (list **finalizer-store**)))

;;; Finalizer table keys are fixnums - NOT objects or SB-VM:WORD - representing
;;; the aligned base address of a lisp object. This is purposely opaque to GC
;;; so that we don't need a special variant of lockfree list that adds weakness.
;;; GC understands the untagged pointer nature of the keys in this table
;;; in exactly the one place that it needs to.
#-weak-vector-readbarrier
(progn
  (define-load-time-global **finalizer-store** (sb-lockless:make-so-map/addr))
  (declaim (type sb-lockless::split-ordered-list **finalizer-store**)))

;;; A mutex is used during rehash due to key movement, but NOT if rehashing
;;; due to table growth. (If growing organically, hashes are valid, so you'll
;;; find what you're looking for if it's there. Invalid hashes are trickier)
(define-load-time-global *finalizer-lock* (sb-thread:make-mutex :name "finalizer"))
(declaim (type sb-thread:mutex *finalizer-lock*))

;;; List of nodes removed from the split-ordered list due to key movement.
;;; These get reinserted when searching the table.
;;; The value of *FINALIZER-REHASHLIST* has to always be non-nil
;;; when rehashing is in progress. FINALIZE and CANCEL-FINALIZATION
;;; use the double-checked lock pattern to decide whether there
;;; might be any in-flight keys based on the list being non-nil.
;;; It can only be reset to NIL while holding the mutex.
(declaim (type (or list (eql 0)) *finalizer-rehashlist*))
(define-load-time-global *finalizer-rehashlist* nil)

;;; List of all nodes whose finalizer(s) should be invoked.
;;; This is an ordinary list. Only the front node can be pushed/popped.
(declaim (type list *finalizers-triggered*))
(define-load-time-global *finalizers-triggered* nil)

;;; Side note: just about every compare-and-swap in this file would be better off
;;; as a "weak" compare-and-swap if we had such thing. When spurious failure
;;; occurs, we're already inside a loop, and will retry the CAS anyway.

;;; Rehashing due to key movement is synchronized by the finalizer-lock.
;;; When adding or canceling a finalizer, we first have to handle the possibility
;;; that GC moved the object of interest from FINALIZER-STORE into FINALIZER-REHASHLIST.
;;; All those keys have to be re-inserted, otherwise there's no way to know the
;;; disposition of a CANCEL-FINALIZATION request.
;;; Even if users are always careful never to operate on one object from two different
;;; threads - so they never introduce a data race between FINALIZE and CANCEL -
;;; we could, if rehashing in multiple threads, create a data race between a user
;;; trying to CANCEL, and a different thread processing the rehashlist and inserting
;;; the canceled key. To prevent that, only one thread will rehash if keys moved.
;;;
;;; Note: Other keys could appear in the FINALIZER-REHASHLIST _after_ we test
;;; whether it is equal to +TAIL+.  This can occur because we're not synchronized
;;; with GC. But that's OK - we pin OBJECT before entering the mutex scope,
;;; therefore it can not move to the rehashlist. So upon seeing the alleged end of the
;;; rehashlist, OBJECT can not be in it. But it could actually be in the FINALIZER-STORE
;;; already because if two threads observe *FINALIZER-REHASHLIST* to be non-nil, they'll
;;; both try to rehash, and presumably one will have no work to do. (Unless GC moves
;;; even more things after the first thread to rehash leaves its mutex scope)
(macrolet
    ((base-pointer (object) ; Cast OBJECT to fixnum in the DESCRIPTOR-SAP representation.
       `(let* ((o ,object)
               (address
                (get-lisp-obj-address
                 (if (simple-fun-p o) (fun-code-header o) o))))
          (%make-lisp-obj (logandc2 address sb-vm:lowtag-mask))))
     (insert (k v)
       `(with-pinned-objects (,k)
          (prog1 (let ((node (sb-lockless:so-insert table (base-pointer ,k) ,v)))
                   ;; I didn't feel like changing INSERT to return a FINALIZER-NODE, though I should.
                   ;; Having the right type enables an extra verification in GC but it's not critical
                   ;; for correctness.
                   (%set-instance-layout node ,(sb-kernel:find-layout 'sb-lockless::finalizer-node))
                   node)
            (sb-thread:barrier (:write)))))
     (update (accessor)
       ;; Decide how to represent NEW-DATA
       ;;   choice (a) FUNCTION | VALUE-CELL = just one finalizer
       ;;   choice (b) LIST of (OR FUNCTION VALUE-CELL) = more than one
       `(let ((old-data (,accessor node)))
          (loop
           (let ((new-data (if old-data (cons action (ensure-list old-data)) action)))
             (when (eq old-data (setf old-data (cas (,accessor node) old-data new-data)))
               (return object))))))
     (get-table ()
       ;; The global var itself is actually invariant, but I suspect that lookups
       ;; need to ensure that writes became visible.
       `(progn (sb-thread:barrier (:read)) **finalizer-store**))
     (with-rehashing (result-expression)
       `(progn
          ;; Must not observe *FINALIZER-REHASHLIST* until _after_ we've pinned OBJECT.
          ;; Otherwise, for a relaxed-memory-order CPU you could read the rehashlist,
          ;; see NIL, store to *PINNED-OBJECTS*, but in between the read and your store,
          ;; GC moved the OBJECT you want to lookup into the rehashlist.
          (sb-thread:barrier (:read))
          (when *finalizer-rehashlist*
            (with-system-mutex (*finalizer-lock*)
              (let* ((found (%finalizers-rehash object table))
                     (result ,result-expression))
                ;; Change 0 to NIL as an optimization which avoids acquiring the mutex
                ;; until some keys get moved again.
                (cas *finalizer-rehashlist* 0 nil)
                result))))))

;;; Insert everything from *finalizer-rehashlist* into TABLE.
;;; There's no race with other threads now, except for a GCing thread.
;;; Thus we need atomic operations despite the mutex.
;;; Possible TODO: 'target-hash-table' uses WITH-PINNED-OBJECT-ITERATOR
;;; which on the precise stack platforms is slightly preferable
;;; to repeated binds and unbinds of *PINNED-OBJECTS.
(defun %finalizers-rehash (key table)
  (labels ((our-pop (list)
             (if (or (eq list nil) (eq list 0))
                 (values nil nil)
                 (let ((new (or (cdr list) 0)))
                   (if (eq list (setf list (cas *finalizer-rehashlist* list new)))
                       (values (car (truly-the list list)) new)
                       (our-pop list))))))
    (let ((list *finalizer-rehashlist*)
          (pair))
      (loop (multiple-value-setq (pair list) (our-pop list))
            (cond ((null pair) (return nil))
                  ((eq (car pair) key) ; no need to finish rehashing
                   (return pair))
                  (t (insert (car pair) (cdr pair))))))))

(defun finalizers-rehash ()
  (with-system-mutex (*finalizer-lock*)
    #+weak-vector-readbarrier
    ;; Scan for broken weak pointers. Doesn't matter that this conses.
    ;; It's only temporary until I can come up with something better.
    (setq **finalizer-store**
          (delete-if
           (lambda (cell)
             (cond ((weak-pointer-value (car cell)) nil) ; keep it
                   (t (atomic-push (cdr cell) *finalizers-triggered*)
                      t))) ; delete it
           **finalizer-store**))
    ;; won't find T in the rehashlist, so this rehashes everything
    (%finalizers-rehash t (get-table))
    (cas *finalizer-rehashlist* 0 nil)))

;;; For debugging/regression testing
(export '%lookup-finalizer)
(defun %lookup-finalizer (x)
  (with-pinned-objects (x)
    (finalizers-rehash)
    (sb-lockless:so-find (get-table) (base-pointer x))))

(defun finalize (object function &key dont-save
                        &aux (function (%coerce-callable-to-fun function)))
  "Arrange for the designated FUNCTION to be called when there
are no more references to OBJECT, including references in
FUNCTION itself.

If DONT-SAVE is true, the finalizer will be cancelled when
SAVE-LISP-AND-DIE is called: this is useful for finalizers
deallocating system memory, which might otherwise be called
with addresses from the old image.

In a multithreaded environment FUNCTION may be called in any
thread. In both single and multithreaded environments FUNCTION
may be called in any dynamic scope: consequences are unspecified
if FUNCTION is not fully re-entrant.

Errors from FUNCTION are handled and cause a WARNING to be
signalled in whichever thread the FUNCTION was called in.

Examples:

  ;;; GOOD, assuming RELEASE-HANDLE is re-entrant.
  (let* ((handle (get-handle))
         (object (make-object handle)))
   (finalize object (lambda () (release-handle handle)))
   object)

  ;;; BAD, finalizer refers to object being finalized, causing
  ;;; it to be retained indefinitely!
  (let* ((handle (get-handle))
         (object (make-object handle)))
    (finalize object
              (lambda ()
                (release-handle (object-handle object)))))

  ;;; BAD, not re-entrant!
  (defvar *rec* nil)

  (defun oops ()
   (when *rec*
     (error \"recursive OOPS\"))
   (let ((*rec* t))
     (gc))) ; or just cons enough to cause one

  (progn
    (finalize \"oops\" #'oops)
    (oops)) ; GC causes re-entry to #'oops due to the finalizer
            ; -> ERROR, caught, WARNING signalled"
  (declare (sb-c::tlab :system))
  (let ((space (heap-allocated-p object)))
    ;; Rule out immediate, stack, arena, readonly, and static objects.
    ;; (Is it really an error for a readonly? Maybe a warning? I'll leave it this way unless
    ;; users complain. Surely DX and arena are errors, and NIL was always an error.)
    (unless (member space '(:dynamic :immobile))
      (if (eq space :static)
          (error "Cannot finalize ~S." object)
          ;; silently discard finalizers on file streams in arenas I guess
          ;; and also silently do nothing on fixnum/character.
          (progn ; (warn "Will not finalize ~S." object)
            (return-from finalize object)))))
  ;; Wrapping a VALUE-CELL around FUNCTION indicates the :DONT-SAVE option without
  ;; having to invent a struct of a function and boolean.
  ;; I believe that most finalizers will *not* have the :DONT-SAVE flag set.
  ;; As evidence the https://github.com/trivial-garbage/trivial-garbage portability
  ;; library does not offer a way to specify :DONT-SAVE.
  (let ((action
         (if dont-save (sb-sys:%primitive sb-vm::make-value-cell function nil) function)))
    (with-pinned-objects (object)
      #+weak-vector-readbarrier
      (with-system-mutex (*finalizer-lock*)
        (let* ((table **finalizer-store**)
               (node (assoc object table :key #'weak-pointer-value :test #'eq)))
          (unless node
            (setq node (cons (make-weak-pointer object) nil))
            (push node **finalizer-store**))
          (update cdr)))
      #-weak-vector-readbarrier
      (let* ((node
              (let ((table (get-table)))
                ;; Attempt 1: optimistically look in the solist assuming valid hashes
                (or (sb-lockless:so-find table (base-pointer object))
                    ;; Attempt 2: perform rehashing and examine each key while looping
                    (with-rehashing (when found
                                      (insert object (cdr found))))
                    ;; Attempt 3: another thread could have done all the rehashing and
                    ;; inserted OBJECT. If not, this will insert a new node.
                    (insert object nil)))))
        (update sb-lockless:so-data)))))

(defun cancel-finalization (object)
  "Cancel all finalizations for OBJECT, returning T if it had a finalizer."
  (when (and object (sb-vm:is-lisp-pointer (get-lisp-obj-address object)))
    (with-pinned-objects (object)
      #+weak-vector-readbarrier
      (with-system-mutex (*finalizer-lock*)
        (let* ((table **finalizer-store**)
               (node (assoc object table :key #'weak-pointer-value :test #'eq)))
          (when node
            (setf **finalizer-store** (setq table (delq1 node table)))
            t)))
      #-weak-vector-readbarrier
      (let ((table (get-table)))
        ;; Attempt 1: optimistically look in the solist assuming valid hashes
        (or (sb-lockless:so-delete table (base-pointer object))
            ;; Attempt 2: perform rehashing and examine each key while looping
            (with-rehashing
                (when found
                  t)) ; implies no re-insert, so we're done
            ;; Attempt 3: Give it another chance. Third time's a charm?
            ;; (Technically do not need this if current thread rehashed? not sure)
            (sb-lockless:so-delete table (base-pointer object)))))))
) ; end MACROLET

;;; FIXME: probably want vop for this, it's just PSEUDO-ATOMIC wrapped around
;;; reconstitute-object, but I don't want to hand-write all that assembly.
;;; So for now: MUST be wrapped in WITHOUT-GCING by calling code
(export 'finalizer-object) ; for regression test
(defun finalizer-object (node)
  ;; if no lowtag on NEXT, then NODE is deleted, or is on its way to being deleted
  (if (fixnump (sb-lockless:%node-next node))
      (bug "can't reconstitute key of deleted finalizer node")
      (sb-vm::reconstitute-object (sb-lockless:so-key node))))

;;; Perform various cleanups around finalizers
(defglobal *saved-finalizers* nil)
(defun finalizers-deinit (&aux save)
  (labels
      ((filter-actions (object actions)
         (when (fd-stream-p object)
           (flameout object)
           ;; Never save a finalizer on an fd-stream. If it runs, it might close
           ;; a file descriptor open to something else. Too bad if a user adds a finalizer
           ;; on an fd-stream- we'll drop it. It's possible to figure out whether this
           ;; is a closure made by fd-stream, but saving finalizers seems like such a
           ;; generally bad idea anyway to be honest.
           (return-from filter-actions))
         ;; Remove any finalizer created with the DONT-SAVE option. The rest are saved
         ;; but that's actually somewhat bogus anyway for two reasons: (1) there's no
         ;; guarantee that finalizers on objects killed in the last GC of save-lisp-and-die
         ;; will be executed; and (2) dumped objects are immortal.
         ;; Therefore saved finalizers are best avoided.
         (let ((list
                (cond ((functionp actions) (list actions)) ; FUNCTIONP implies save it
                      ((listp actions) (delete-if-not #'functionp actions)))))
           ;; FINALIZERS-REINIT employs FINALIZE to reinstall saved finalizers. That
           ;; function does not accept a list of actions, so insert each action
           ;; individually if there is more than one, which there probably isn't.
           (dolist (action list)
             (push (cons (make-weak-pointer object) action) save))))
       (flameout (object)
         (push (list object
                     (ansi-stream-in object)
                     (ansi-stream-bin object)
                     (ansi-stream-n-bin object)
                     (ansi-stream-cout object)
                     (ansi-stream-bout object)
                     (ansi-stream-sout object)
                     (ansi-stream-misc object))
               *streams-closed-by-slad*)
         ;; Nobody asked us to actually close the fd,
         ;; so just make it unusable.
         (set-closed-flame-by-slad object)))
    #+weak-vector-readbarrier
    (without-gcing
        (dolist (x **finalizer-store** (setq **finalizer-store** nil))
          (awhen (weak-pointer-value (car x))
            (filter-actions it (cdr x)))))
    #-weak-vector-readbarrier
    ;; Create a unified collection of finalized objects. Use a plain list of
    ;; weak pointers for this purpose.
    (let* ((table **finalizer-store**)
           (count (sb-lockless::so-count table))
           ;; Preallocate to avoid consing inside WITHOUT-GCING
           (keys (make-array count))
           (values (make-array count))
           (n 0))
      ;; As much as we should eschew WITHOUT-GCING in system internals, this code is
      ;; single-threaded and preparing to exit, so it's kind of excusable.
      (without-gcing
       ;; SO-MAPLIST skips deleted nodes
       (sb-lockless:so-maplist (lambda (node)
                                 (setf (aref keys n) (finalizer-object node)
                                       (aref values n) (sb-lockless:so-data node))
                                 (incf n))
                               table)
        ;; Clobber the split-ordered list so we don't run any finalizer twice
        (setf **finalizer-store** (sb-lockless::make-so-map/addr)))
      (dotimes (i n) (filter-actions (aref keys i) (aref values i)))
      ;; Add in the items in need of rehash
      (do ((tail *finalizer-rehashlist* (cdr tail)))
          ((atom tail)) ; could be terminated by either 0 or NIL
        (let ((pair (car tail)))
          (filter-actions (car pair) (cdr pair))))))
  ;; Clobber the rehashlist
  (setf *finalizer-rehashlist* nil)
  ;; Drop any triggered actions, becaused the system is in a state where
  ;; executing random user code is probably undesirable if not impossible
  ;; given that OS-DEINIT and CLOSE-SHARED-OBJECTS were already invoked.
  (setq *finalizers-triggered* nil)
  (setq *saved-finalizers* save))

(defun finalizers-reinit ()
  (dolist (pair (prog1 *saved-finalizers* (setq *saved-finalizers* nil)))
    (let ((key (weak-pointer-value (car pair))))
      (if key
          (finalize key (cdr pair))
          ;; Objects that died in the final GC do ** NOT ** get their finalizer
          ;; run. Consider: some object that dies which acquired an OS resource
          ;; identified by a file-descriptor that might now be open to
          ;; some other descriptor.
          ;; (atomic-push (cdr pair) *finalizers-triggered*)
          ))))

(defvar *in-a-finalizer* nil)
(define-load-time-global *user-finalizer-runcount* 0)
(defun run-user-finalizer () ; Return T if this did anything
  (let* ((data (atomic-pop *finalizers-triggered*))
         (data-list (list data)))
    ;; DATA could already be a list. This is basically a no-consing ENSURE-LIST
    (declare (dynamic-extent data-list))
    (dolist (finalizer (if (listp data) data data-list) (not (null data)))
      ;; :DONT-SAVE finalizers are wrapped in value-cells. Unwrap as necessary
      (let ((fun (the function (if (functionp finalizer)
                                   finalizer
                                   (value-cell-ref finalizer)))))
        ;; Binding *IN-A-FINALIZER* prevents recursive run-pending-finalizers
        ;; if #-sb-thread. #+sb-thread probably doesn't require it.
        (handler-case (let ((*in-a-finalizer* t)) (funcall fun))
          (error (c) (warn "Error calling finalizer ~S:~%  ~A" fun c)))))))

#+sb-thread
(progn
(define-alien-variable finalizer-thread-runflag int)
;; post-GC hooks are synchronously invoked after GC if #-sb-thread
(define-load-time-global *after-gc-hooks* nil)
(define-load-time-global *run-gc-hooks* 0)
(declaim (fixnum *run-gc-hooks*)))

;;; Drain the queue of finalizers and return when empty.
;;; Concurrent invocations of this function in different threads are ok.
;;; Nested invocations (from a GC forced by a finalizer) are not ok.
;;; See the trace at the bottom of this file.
(define-load-time-global *bg-compiler-function* #'sb-c::default-compiler-worker)
(defun run-pending-finalizers (&aux (system-finalizer-scratchpad (list 0)))
  (declare (dynamic-extent system-finalizer-scratchpad))
  (finalizers-rehash)
  (loop
   #+sb-thread
   (progn
     ;; Perform no further work if trying to stop the thread, even if there is work.
     (when (zerop finalizer-thread-runflag) (return))
     ;; If this thread is very slow to notice *RUN-GC-HOOKS* due to a slow finalizer,
     ;; requests to run can stack up. A request will not be lost but may be delayed
     (when (plusp *run-gc-hooks*)
       (sb-vm:without-arena (call-hooks "after-GC" *after-gc-hooks* :on-error :warn))
       (atomic-decf *run-gc-hooks*)))
   (let ((ran-bg-compile ; Try to run a background compilation task
          (funcall *bg-compiler-function*))
         (ran-a-system-finalizer ; Try to run 1 system finalizer
          (sb-vm::immobile-code-dealloc-1 system-finalizer-scratchpad))
         (ran-a-user-finalizer ; Try to run 1 user finalizer
          (run-user-finalizer)))
     ;; Did this iteration do anything at all?
     (unless (or ran-bg-compile ran-a-system-finalizer ran-a-user-finalizer)
       (return)))))

(define-load-time-global *finalizer-thread* nil)
(declaim (type (or sb-thread:thread (eql :start) null) *finalizer-thread*))
#+sb-thread
(progn
(defun finalizer-thread-notify (run-hooks)
  (declare (bit run-hooks))
  (alien-funcall (extern-alien "finalizer_thread_wake" (function void int))
                 run-hooks)
  nil)

;;; The following operations are synchronized by *MAKE-THREAD-LOCK* -
;;;   FINALIZER-THREAD-{START,STOP}, S-L-A-D, SB-POSIX:FORK
(defun finalizer-thread-start ()
  (with-system-mutex (sb-thread::*make-thread-lock*)
    #+(and unix sb-safepoint)
    (sb-thread::make-system-thread "sigwait"
                                   #'sb-unix::signal-handler-loop
                                   nil 'sb-unix::*sighandler-thread*)
    (aver (not *finalizer-thread*))
    (setf finalizer-thread-runflag 1)
    (setq *finalizer-thread* :start)
    (let ((thread
           (sb-thread::make-system-thread
            "finalizer"
            (lambda ()
              (setf *finalizer-thread* sb-thread:*current-thread*)
              (loop (run-pending-finalizers)
                    ;; a slight race here- technically the finalizer thread should not
                    ;; put itself to sleep unless it has ascertained that there is no
                    ;; background work while it holds a mutex on the various queues.
                    (alien-funcall (extern-alien "finalizer_thread_wait" (function void)))
                    (when (zerop finalizer-thread-runflag) (return)))
              (setq *finalizer-thread* nil))
            nil nil)))
      ;; Don't return from this function until *FINALIZER-THREAD* has a good value,
      ;; but don't set it to a thread if the thread was not created, or exited already.
      (cas *finalizer-thread* :start thread))))

;;; You should almost always invoke this with *MAKE-THREAD-LOCK* held.
;;; Some tests violate that, but they know what they're doing.
(defun finalizer-thread-stop ()
  #+(and unix sb-safepoint)
  (let ((thread sb-unix::*sighandler-thread*))
    (aver (sb-thread::thread-p thread))
    (setq sb-unix::*sighandler-thread* nil)
    ;; This kill causes the thread's sigwait() syscall to return normally
    ;; and then not invoke any handler.
    (sb-unix:pthread-kill (sb-thread::thread-os-thread thread) sb-unix:sigterm)
    (sb-thread:join-thread thread))
  (let ((thread *finalizer-thread*))
    (aver (sb-thread::thread-p thread))
    (alien-funcall (extern-alien "finalizer_thread_stop" (function void)))
    (sb-thread:join-thread thread)))
)

(export 'show-finalizers)
(defun show-finalizers (&aux (*print-pretty* nil))
  (flet ((display (key)
           (if key
               (format t "~D ~X ~S~%" (generation-of key) (get-lisp-obj-address key) key)
               (format t "<triggered-finalizer>~%"))))
    (format t "~&Unhashed:~%")
    (do ((node (or *finalizer-rehashlist* sb-lockless:+tail+)
               (sb-lockless:%node-next node)))
        ((eq node sb-lockless:+tail+))
      (display (weak-pointer-value (sb-lockless:so-key node))))
    (format t "~&Hashed:~%")
    (sb-lockless:so-maplist (lambda (node)
                              ;; For debugging only; caveat emptor.
                              (display (without-gcing (finalizer-object node))))
                            **finalizer-store**)))

#|
;;; This is a display produced by annotating parts of gc-common.c and
;;; interrupt.c with each thread's output in its own column.
;;; The main thread is on the right.

;;; This output shows that if the finalizer thread calls a function that
;;; triggers a GC, the interrupt nesting depth can grow without limit.
;;; The finalizer thread does not have to be a memory hog - it just has
;;; to be unlucky enough to be the thread that triggers the collection.
;;; _Any_ thread can bring the GC trigger up to the threshold,
;;; and as long as the finalizer thread is the one to cross the
;;; the threshold, it is tasked with triggering the next scan
;;; of finalizers, but it MUST NOT do so recursively.
;;;
;;; The same thing can happen without using a finalizer thread,
;;; but it's actually easier to understand the output this way.

Thread 2                                     Main
--------                                     --------
|                                            Enter SB-EXT:GC 0
|                                            Stopping world
|                                            Stopped world
|                                            SUB-GC calling gc(0)
|                                            set auto_gc_trig=4858833
|                                            completed GC
|                                            Restarted world
|                                            SB-EXT:GC calling POST-GC
|                                            ENTER run-pending-finalizers
|                                            Enter SB-EXT:GC 0
|                                            Stopping world
|                                            Stopped world
|                                            SUB-GC calling gc(6)
|                                            set auto_gc_trig=5075473
|                                            completed GC
|                                            Restarted world
|                                            SB-EXT:GC calling POST-GC
|                                            ENTER run-pending-finalizers
|                                            Trying to start finalizer thread
| starting
|                                            Enter SB-EXT:GC 0
|                                            Stopping world
| Caught SIGUSR2, pc=52a946bd
| STOP_FOR_GC PA=1 inh=N sigmask=0
| Caught SIGILL, pc=52a946cc code 0x9
| evt 1 >handle_pending
| STOP_FOR_GC PA=0 inh=N sigmask=0
| fake ffcall "stop_for_gc"
| bind(free_ICI, 1)
|                                            Stopped world
|                                            SUB-GC calling gc(6)
|                                            set auto_gc_trig=520eeb3
|                                            completed GC
|                                            Restarted world
|                                            SB-EXT:GC calling POST-GC
|                                            ENTER run-pending-finalizers
|                                            Notify finalizer thread
| unbind free_ICI -> 0
| leave STOP_FOR_GC
| evt 1 <handle_pending
| gc_trig=52bcf80, setting PA-int
| Caught SIGILL, pc=52a946cc code 0x9
| evt 2 >handle_pending
| ENTER maybe_gc (pc was 52a946ce)
| fake ffcall "maybe_gc"
| bind(free_ICI, 1)
| maybe_gc calling SUB-GC
| Stopping world
|                                            Caught SIGUSR2, pc=7ff010deef47
|                                            STOP_FOR_GC PA=0 inh=N sigmask=0
|                                            fake ffcall "stop_for_gc"
|                                            bind(free_ICI, 1)
| Stopped world
| SUB-GC calling gc(0)
| set auto_gc_trig=552b033
| completed GC
| Restarted world
| maybe_gc calling POST-GC
| ENTER run-pending-finalizers
|                                            unbind free_ICI -> 0
|                                            leave STOP_FOR_GC
| gc_trig=55d9110, setting PA-int
| Caught SIGILL, pc=52a946cc code 0x9
| evt 3 >handle_pending
| ENTER maybe_gc (pc was 52a946ce)
| fake ffcall "maybe_gc"
| bind(free_ICI, 2)
| maybe_gc calling SUB-GC
| Stopping world
|                                            Caught SIGUSR2, pc=7ff010deef47
|                                            STOP_FOR_GC PA=0 inh=N sigmask=0
|                                            fake ffcall "stop_for_gc"
|                                            bind(free_ICI, 1)
| Stopped world
| SUB-GC calling gc(0)
| set auto_gc_trig=56c0423
| completed GC
| Restarted world
| maybe_gc calling POST-GC
| ENTER run-pending-finalizers
|                                            unbind free_ICI -> 0
|                                            leave STOP_FOR_GC
| gc_trig=576e500, setting PA-int
| Caught SIGILL, pc=52a946cc code 0x9
| evt 4 >handle_pending
| ENTER maybe_gc (pc was 52a946ce)
| fake ffcall "maybe_gc"
| bind(free_ICI, 3)
| maybe_gc calling SUB-GC
| Stopping world
|                                            Caught SIGUSR2, pc=7ff010deef47
|                                            STOP_FOR_GC PA=0 inh=N sigmask=0
|                                            fake ffcall "stop_for_gc"
|                                            bind(free_ICI, 1)
| Stopped world
| SUB-GC calling gc(0)
| set auto_gc_trig=59dc213
| completed GC
| Restarted world
| maybe_gc calling POST-GC
| ENTER run-pending-finalizers
|                                            unbind free_ICI -> 0
|                                            leave STOP_FOR_GC
| gc_trig=5a8a2f0, setting PA-int
| Caught SIGILL, pc=52a946cc code 0x9
| evt 5 >handle_pending
| ENTER maybe_gc (pc was 52a946ce)
| fake ffcall "maybe_gc"
| bind(free_ICI, 4)
| maybe_gc calling SUB-GC
| Stopping world
|
;;; This pattern of binding FREE_INTERRUPT_CONTEXT_INDEX to successively
;;; higher values will continue forever until reaching the limit and crashing.
|#
