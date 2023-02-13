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

;;; Finalizer table keys are fixnums - NOT objects or SB-VM:WORD - representing
;;; the aligned base address of a lisp object. This is purposely opaque to GC
;;; so that we don't need a special variant of lockfree list that adds weakness.
;;; GC understands the untagged pointer nature of the keys in this table
;;; in exactly the one place that it needs to.
(define-load-time-global **finalizer-store** (sb-lockless:make-so-map/addr))
(declaim (type sb-lockless::split-ordered-list **finalizer-store**))

;;; A mutex is used during rehash due to key movement, but NOT if rehashing
;;; due to table growth. (If growing organically, hashes are valid, so you'll
;;; find what you're looking for if it's there. Invalid hashes are trickier)
(define-load-time-global *finalizer-lock* (sb-thread:make-mutex :name "finalizer"))
(declaim (type sb-thread:mutex *finalizer-lock*))

;;; List of nodes removed from the split-ordered list due to key movement.
;;; These get reinserted when searching the table.
;;; It is built of lockfree list nodes without using the full algorithm of Harris,
;;; and has _two_ possible empty list markers: +TAIL+ indicates that rehashing
;;; reached the last worklist item and is nominally still processing; while NIL
;;; indicates no work to be done at all.
(declaim (type (or null sb-lockless::list-node) *finalizer-rehashlist*))
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
    ((base-pointer (k) ; Cast K to fixnum in the DESCRIPTOR-SAP representation.
       `(%make-lisp-obj (logandc2 (get-lisp-obj-address ,k) sb-vm:lowtag-mask)))
     (insert (k v)
       `(with-pinned-objects (,k)
          (prog1 (sb-lockless:so-insert table (base-pointer ,k) ,v)
            (sb-thread:barrier (:write)))))
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
                ;; Regardless of what %finalizers-rehash did, try to change +TAIL+ to NIL
                ;; so that threads don't attempt to acquire the mutex until after next GC.
                (cas *finalizer-rehashlist* sb-lockless:+tail+ nil)
                result))))))

;;; Scan the rehashlist looking for KEY, stopping and returning its list node if found.
;;; Each node seen prior to stopping will be reinserted into either the triggered
;;; list or the finalizer store.
;;; There's no race with other threads now, except for a GCing thread.
;;; Thus we need atomic operations despite the mutex.
;;; Possible TODO: 'target-hash-table' uses WITH-PINNED-OBJECT-ITERATOR
;;; which on the precise stack platforms is slightly preferable
;;; to repeated binds and unbinds of *PINNED-OBJECTS.
(defun %finalizers-rehash (key table)
  (let ((node *finalizer-rehashlist*))
    (loop
      ;; Instead of setting *FINALIZER-REHASHLIST* to NIL on the last item,
      ;; it becomes +TAIL+ which forces other threads to wait on the mutex.
      ;; Otherwise they have no guarantee that the item of interest
      ;; to them isn't the one which is currently in flight in this loop.
      (when (or (eq node sb-lockless:+tail+) (null node))
        (return))
      (let* ((next (sb-lockless:%node-next (the sb-lockless::so-data-node node)))
             (actual (cas *finalizer-rehashlist* node next))) ; like ATOMIC-POP
        (if (eq node actual)
            (let* ((weakptr (the weak-pointer (sb-lockless:so-key node)))
                   (obj (weak-pointer-value weakptr)))
              (cond ((null obj) ; broken ptr, transfer to triggered list
                     (atomic-push (sb-lockless:so-data node) *finalizers-triggered*)
                     (setf (sb-lockless:so-key node) 0)) ; don't need the weak-pointer
                    ((neq obj key) ; re-insert
                     ;; GC has a nonzero amount of extra work for each weak-pointer
                     ;; but not if the value in it is NIL
                     (%primitive sb-c:set-slot weakptr nil 'setf sb-vm:weak-pointer-value-slot
                                 sb-vm:other-pointer-lowtag)
                     (insert obj (sb-lockless:so-data node)))
                    (t ; This is the _least_ likely case, so test it last
                     (return node))) ; FOUND
              (setq node next))
            (setq node actual))))))

(defun finalizers-rehash ()
  ;; won't find T in the rehashlist, so this rehashes everything
  (%finalizers-rehash t (get-table)))

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
          (progn ; (warn "Will not finalize ~S." object)
            (return-from finalize object)))))
  (let* ((node
          (with-pinned-objects (object)
            (let ((table (get-table)))
              ;; Attempt 1: optimistically look in the solist assuming valid hashes
              (or (sb-lockless:so-find table (base-pointer object))
                  ;; Attempt 2: perform rehashing and examine each key while looping
                  (with-rehashing (when found
                                    (insert object (sb-lockless:so-data found))))
                  ;; Attempt 3: another thread could have done all the rehashing and
                  ;; inserted OBJECT. If not, this will insert a new node.
                  (insert object nil)))))
         ;; Conditionally wrapping a VALUE-CELL around FUNCTION is a means to indicate
         ;; the :DONT-SAVE option without inventing a struct of a function and boolean.
         ;; I believe that most finalizers will *not* have the :DONT-SAVE flag set.
         ;; As evidence the https://github.com/trivial-garbage/trivial-garbage portability
         ;; library does not offer a way to specify :DONT-SAVE.
         (action
          (if dont-save (sb-sys:%primitive sb-vm::make-value-cell function nil) function))
         (old-data (sb-lockless:so-data node)))
    (loop
      ;; Decide how to represent NEW-DATA
      ;;   choice (a) FUNCTION | VALUE-CELL = just one finalizer
      ;;   choice (b) LIST of (OR FUNCTION VALUE-CELL) = more than one
      (let ((new-data (if old-data (cons action (ensure-list old-data)) action)))
        (when (eq old-data
                  (setf old-data (cas (sb-lockless:so-data node) old-data new-data)))
          (return object))))))

(defun cancel-finalization (object)
  "Cancel all finalizations for OBJECT, returning T if it had a finalizer."
  (when (and object (heap-allocated-p object))
    (with-pinned-objects (object)
      (let ((table (get-table)))
        ;; Attempt 1: optimistically look in the solist assuming valid hashes
        (or (sb-lockless:so-delete table (base-pointer object))
            ;; Attempt 2: perform rehashing and examine each key while looping
            (with-rehashing found) ; implies no re-insert, so we're done
            ;; Attempt 3: Give it another chance. Third time's a charm?
            ;; (Technically do not need this if current thread rehashed? not sure)
            (sb-lockless:so-delete table (base-pointer object)))))))
) ; end MACROLET

;;; FIXME: probably want vop for this, it's just PSEUDO-ATOMIC wrapped around
;;; reconstitute-object, but I don't want to hand-write all that assembly.
;;; So for now: MUST be wrapped in WITHOUT-GCING by calling code
(export 'finalizer-object) ; for regression test
(defun finalizer-object (node)
  (sb-vm::reconstitute-object (sb-lockless:so-key node)))

(defun finalizers-deinit ()
  (when (null *finalizer-rehashlist*)
    (setq *finalizer-rehashlist* sb-lockless:+tail+))
  ;; invalidate fd-streams
  (flet ((flameout (object)
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
    (do ((node *finalizer-rehashlist* (sb-lockless:%node-next node)))
        ((eq node sb-lockless:+tail+))
      (let ((object (weak-pointer-value (sb-lockless:so-key node))))
        (when (fd-stream-p object)
          (flameout object))))
    (let* ((table **finalizer-store**)
           ;; Avoid consing inside SO-MAPLIST
           (array (make-array (sb-lockless::so-count table)))
           (n 0))
      (without-gcing
       (sb-lockless:so-maplist
        (lambda (node)
          (let ((object (finalizer-object node)))
            (when (fd-stream-p object)
              (setf (aref array n) object)
              (incf n))))
        table))
      (dotimes (i n)
        (flameout (aref array i)))))
  ;; remove :dont-save finalizers
  (flet ((filter-actions (node &aux (actions (sb-lockless:so-data node)))
           (cond ((listp actions)
                  ;; (NOT FUNCTIONP) implies :DONT-SAVE
                  (let ((new (delete-if-not #'functionp actions)))
                    ;; If SINGLETON-P then just store the one. NIL stays as-is
                    (setf (sb-lockless:so-data node) (if (cdr new) new (car new)))))
                 ((functionp actions) actions))))
    ;; Process the need-rehash items, leaving them in that list if applicable.
    ;; Nodes already in rehashlist might actually be subject to removal
    ;; either because the object died, or all its actions are :DONT-SAVE.
    (do ((prev nil) ; no dummy node, you dummy
         (this *finalizer-rehashlist*))
        ((eq this sb-lockless:+tail+))
      (let ((next (sb-lockless:%node-next this)))
        (cond ((and (filter-actions this)
                    (weak-pointer-value (sb-lockless:so-key this)))
               (setf prev this this next)) ; keep
              (t ; discard
               (setf this next)
               (if prev
                   (setf (sb-lockless:%node-next prev) this)
                   (setf *finalizer-rehashlist* this))))))
    ;; Process the hashed items, moving them to the rehash list if applicable.
    ;; Safely resurrecting objects by their address requires WITHOUT-GCING.
    (without-gcing
     (sb-lockless:so-maplist
      (lambda (node)
        (when (filter-actions node)
          ;; re-use the node
          (setf (sb-lockless:so-key node) (make-weak-pointer (finalizer-object node))
                (sb-lockless:%node-next node) *finalizer-rehashlist*
                *finalizer-rehashlist* node)))
       **finalizer-store**)))
  ;; We don't promise to execute pending actions on save-lisp-and-die.
  ;; Could we? Should we?
  (setq *finalizers-triggered* nil)
  ;; Create an empty table. FINALIZE will reinsert things when next called.
  (setf **finalizer-store** (sb-lockless::make-so-map/addr)))

(defvar *in-a-finalizer* nil)
(define-load-time-global *user-finalizer-runcount* 0)
(defun run-user-finalizer () ; Return T if this did anything
  (let* ((data (atomic-pop *finalizers-triggered*))
         (data-list (list data)))
    ;; DATA could already be a list. This is basically a no-consing ENSURE-LIST
    (declare (truly-dynamic-extent data-list))
    (dolist (finalizer (if (listp data) data data-list) (not (null data)))
      ;; :DONT-SAVE finalizers are wrapped in value-cells. Unwrap as necessary
      (let ((fun (the function (if (functionp finalizer)
                                   finalizer
                                   (value-cell-ref finalizer)))))
        ;; Binding *IN-A-FINALIZER* prevents recursive run-pending-finalizers
        ;; if #-sb-thread. #+sb-thread probably doesn't require it.
        (handler-case (let ((*in-a-finalizer* t)) (funcall fun))
          (error (c) (warn "Error calling finalizer ~S:~%  ~S" fun c)))))))

#+sb-thread (define-alien-variable finalizer-thread-runflag int)
;;; Drain the queue of finalizers and return when empty.
;;; Concurrent invocations of this function in different threads are ok.
;;; Nested invocations (from a GC forced by a finalizer) are not ok.
;;; See the trace at the bottom of this file.
(define-load-time-global *bg-compiler-function* nil)
(defun run-pending-finalizers (&aux (system-finalizer-scratchpad (list 0)))
  (declare (truly-dynamic-extent system-finalizer-scratchpad))
  (finalizers-rehash)
  (loop
   ;; Perform no further work if trying to stop the thread, even if there is work.
   #+sb-thread (when (zerop finalizer-thread-runflag) (return))
   (let ((ran-bg-compile ; Try to run a background compilation task
          (when *bg-compiler-function* (funcall *bg-compiler-function*)))
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
(defun finalizer-thread-notify ()
  (alien-funcall (extern-alien "finalizer_thread_wake" (function void)))
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
