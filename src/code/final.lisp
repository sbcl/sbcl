;;;; finalization based on weak-keyed hash-table

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

(defmacro with-finalizer-store ((var) &body body)
  `(with-system-mutex ((hash-table-lock (finalizer-id-map **finalizer-store**)))
     ;; Grab the global var inside the lock in case the array was enlarged
     ;; after we referenced the mutex but before we acquired it.
     ;; It's OK to reference the FINALIZER-ID-MAP because that is always
     ;; in element 1 of the array regardless of what happens to the array.
     (let ((,var **finalizer-store**))
       ,@body)))

(defmacro finalizer-recycle-bin (store) `(cdr (elt ,store 0)))
(defmacro finalizer-id-map (store) `(elt ,store 1))
(defmacro finalizer-max-id (store) `(elt ,store 2))

(defun make-finalizer-store (array-length)
  (let* ((v (make-array (the index array-length) :initial-element 0))
         (ht (make-system-hash-table :test 'eq :weakness :key :synchronized nil
                                     :finalizer t)))
    ;; The recycle bin has a dummy item in front so that the simple-vector
    ;; is growable without messing up RUN-PENDING-FINALIZERS when it atomically
    ;; pushes items into the recycle bin - it is unaffected by looking at
    ;; an obsolete **FINALIZER-STORE** if FINALIZE has assigned a new one.
    (setf (elt v 0) (list 0)
          (finalizer-id-map v) ht
          (finalizer-max-id v) 2)
    v))

(defconstant +finalizers-initial-size+ 50) ; arbitrary
(define-load-time-global **finalizer-store**
  (make-finalizer-store +finalizers-initial-size+))
(declaim (simple-vector **finalizer-store**))

(defun finalize (object function &key dont-save
                        &aux (function (%coerce-callable-to-fun function))
                             (item (if dont-save (list function) function)))
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
  (unless object
    (error "Cannot finalize NIL."))
  (with-finalizer-store (store)
    (let ((id (gethash object (finalizer-id-map store))))
      (cond (id ; object already has at least one finalizer
             ;; Multiple finalizers are invoked in the order added.
             (let* ((old (svref store id))
                    (new (make-array (if (simple-vector-p old)
                                         (1+ (length old)) ; already > 1
                                         2))))             ; was singleton
               (if (= (length new) 2)
                   (setf (aref new 0) old) ; upgrade singleton to vector
                   (replace new old))
               (setf (aref new (1- (length new))) item
                     (svref store id) new)))
            (t ; assign the next available ID to this object
             (cond ((finalizer-recycle-bin store)
                    ;; We must operate atomically with respect to producers,
                    ;; because RUN-PENDING-FINALIZERS is lock-free.
                    ;; The initial test above said that the bin is nonempty,
                    ;; so we can't fail to obtain an item, as the list can't
                    ;; shrink except through here, which is mutually exclusive
                    ;; with other consumers of recycled items.
                    (setq id (atomic-pop (finalizer-recycle-bin store))))
                   (t
                    (setq id (incf (finalizer-max-id store)))
                    (unless (< id (length store))
                      (sb-thread:barrier (:write)
                        ;; We must completely copy the old vector into the new
                        ;; before publishing the new in **FINALIZER-STORE**.
                        ;; Perhaps a cleverer way to size up is to have a tree
                        ;; of vectors; never remove cells already created,
                        ;; but simply graft new limbs on to the tree.
                        (setq store (adjust-array store (* (length store) 2)
                                                  :initial-element 0)))
                      (setq **finalizer-store** store))))
             ;; Clear out lingering junk from (SVREF STORE ID) before
             ;; establishing that OBJECT maps to that index.
             (setf (svref store id) item
                   (gethash object (finalizer-id-map store)) id)))))
  object)

(defun invalidate-fd-streams ()
  (with-finalizer-store (store)
    (maphash (lambda (object id)
               (declare (ignore id))
               (when (fd-stream-p object)
                 (push (list object
                             (ansi-stream-in object)
                             (ansi-stream-bin object)
                             (ansi-stream-n-bin object)
                             (ansi-stream-out object)
                             (ansi-stream-bout object)
                             (ansi-stream-sout object)
                             (ansi-stream-misc object))
                       *streams-closed-by-slad*)
                 ;; Nobody asked us to actually close the fd,
                 ;; so just make it unusable.
                 (set-closed-flame-by-slad object)))
             (finalizer-id-map store))))

(defun finalizers-deinit ()
  ;; remove :dont-save finalizers
  ;; Renumber the ID range as well, but leave the array size as-is. We could
  ;; probably delete *all* finalizers prior to image dump, because saved
  ;; finalizers can in practice almost never be run, as pseudo-static objects
  ;; don't die, making this more-or-less an exercise in futility.
  (with-finalizer-store (old-store)
    ;; This doesn't need WITHOUT-GCING. MAPHASH will never present its funarg
    ;; with a culled entry. GC during the MAPHASH could remove some items
    ;; before we get to them, and that's fantastic.
      (let ((new-store
              (make-finalizer-store (max (1+ (finalizer-max-id old-store))
                                         +finalizers-initial-size+)))
            (old-objects (finalizer-id-map old-store)))
        (maphash (lambda (object old-id &aux (old (svref old-store old-id)))
                   ;; OLD is either a vector of finalizers or a single finalizer.
                   ;; Each finalizer is either a callable (a symbol or function)
                   ;; or a singleton list of a callable.
                   ;; Delete any finalizer wrapped in a cons, meaning "don't save".
                   (awhen (cond ((simple-vector-p old)
                                 (let ((new (remove-if #'consp old)))
                                   (case (length new)
                                     (0 nil)           ; all deleted
                                     (1 (svref new 0)) ; reduced to singleton
                                     (t new))))
                                ((atom old) old)) ; a single finalizer to be saved
                     (let ((new-id (incf (finalizer-max-id new-store))))
                       (setf (gethash object (finalizer-id-map new-store)) new-id
                             (svref new-store new-id) it))))
                 old-objects)
        (clrhash old-objects)
        (fill old-store 0)
        (setq **finalizer-store** new-store))))

;;; Replace the finalizer store with a copy.  Tenured (gen6 = pseudo-static)
;;; vectors are problematic in many ways for gencgc, unless immutable.
;;; Among the problems is this: after sizing **FINALIZER-STORE** up,
;;; Lisp doesn't know when there are no readers of the old vector
;;; (due to the lock-free algorithm for RUN-PENDING-FINALIZERS),
;;; so we can't safely zero-fill the old vector. Making sure that it
;;; is not immortal (i.e. not in gen6), is a reasonable workaround.
;;; [Actually, in this particular algorithm, it is slightly OK to zero-fill
;;; due to the fact that 0 is not a list; therefore if (SVREF V INDEX) is 0,
;;; we can chase down the correct value by reloading **FINALIZER-STORE**.
;;; Of course the zero-fill noise is itself a workaround for accidental
;;; transitive immortalization, which is issue that merits a general fix]
(defun finalizers-reinit ()
  ;; This must be called inside WITHOUT-GCING and with no other threads.
  (aver *gc-inhibit*)
  (let* ((old-store **finalizer-store**)
         (new-store (make-finalizer-store (length old-store)))
         (old-objects (finalizer-id-map old-store))
         (new-objects (finalizer-id-map new-store)))
    ;; Copy the max-id and all the finalizers.
    ;; The recycle bin is empty, and the hash-table is newly consed.
    (replace new-store old-store :start1 2 :start2 2)
    ;; Copy the hash-table.
    ;; Or should the old just be assigned into the new finalizer-store?
    ;; Probably not, because immortable hash-tables have a similar
    ;; problem as cited above, unless strictly constant.
    ;; (Though mitigated by a FILL in REHASH)
    (maphash (lambda (object id) (setf (gethash object new-objects) id))
             old-objects)
    (clrhash old-objects)
    (fill old-store 0)
    (setq **finalizer-store** new-store)))

(defun cancel-finalization (object)
  "Cancel all finalizations for OBJECT."
  (when object
    (with-finalizer-store (store)
     (let ((hashtable (finalizer-id-map store)))
       (awhen (gethash object hashtable)
         (remhash object hashtable)
         ;; Clear old function(s) before publishing the ID as available.
         ;; Not strictly necessary to do this: the next FINALIZE claiming
         ;; the same ID would assign a fresh list anyway.
         (setf (svref store it) 0)
         (atomic-push it (finalizer-recycle-bin store)))))
    object))

;;; Drain the queue of finalizers and return when empty.
;;; Concurrent invocations of this function in different threads are ok.
;;; Nested invocations (from a GC forced by a finalizer) are not ok.
;;; See the trace at the bottom of this file.
(defvar *in-a-finalizer* nil)
#+sb-thread (define-alien-variable finalizer-thread-runflag int)
(defun run-pending-finalizers ()
  ;; This never acquires the finalizer store lock. Code accordingly.
  (let ((hashtable (finalizer-id-map **finalizer-store**)))
    (loop
     ;; Perform no further work if trying to stop the thread, even if there is work.
     #+sb-thread (when (zerop finalizer-thread-runflag) (return))
     (let ((cell (hash-table-culled-values hashtable)))
       ;; This is like atomic-pop, but its obtains the first cons cell
       ;; in the list, not the car of the first cons.
       ;; Possible TODO: when no other work remains, free the *JOINABLE-THREADS*,
       ;; though MAKE-THREAD and JOIN-THREAD do that also, so there's no memory leak.
       (loop (unless cell (return-from run-pending-finalizers))
             (let ((actual (cas (hash-table-culled-values hashtable)
                                cell (cdr cell))))
               (if (eq actual cell) (return) (setq cell actual))))
       (let* ((id (the index (car cell)))
              ;; No other thread can modify **FINALIZER-STORE** at index ID
              ;; because the table no longer contains an object mapping to
              ;; that element; however the vector could be grown at any point,
              ;; so always load the vector again before dereferencing.
              (store **finalizer-store**)
              ;; I don't think we need a barrier; this has a data dependency
              ;; on (CAR CELL) and STORE.
              (finalizers (svref store id))) ; [1] load
         (setf (svref store id) 0)           ; [2] store
         ;; The ID can be reused right away. Link it into the recycle list,
         ;; which has an extra NIL at the head so that we can use RPLACD,
         ;; making this operation agnostic of whether the vector was switched.
         (let* ((list (svref store 0))
                (old (cdr list)))
           (loop (let ((actual (cas (cdr list) old (rplacd cell old))))
                   (if (eq actual old) (return) (setq old actual)))))
         ;; Now call the function(s)
         (flet ((call (finalizer)
                  (let ((fun (if (consp finalizer) (car finalizer) finalizer)))
                    (handler-case (let ((*in-a-finalizer* t)) (funcall fun))
                      (error (c)
                        (warn "Error calling finalizer ~S:~%  ~S" fun c))))))
           (if (simple-vector-p finalizers)
               (map nil #'call finalizers)
               (call finalizers)))
         ;; While the assignment to (SVREF STORE ID) should have been adequate,
         ;; we don't know that the vector is current - a new vector could have
         ;; gotten assigned into **FINALIZER-STORE** in between [1] and [2],
         ;; in which case the store was performed into the wrong vector.
         ;; It doesn't actually matter. Using CAS isn't an improvement, because
         ;; the vector itself is potentially wrong. But the load was valid
         ;; because the the cell's value is frozen, just duplicated into more
         ;; than one vector (in fact, an arbitrary number of vectors).
         ;; A reductio ad absurdum argument shows this:
         ;; - if you had a way to alter the contents of (SVREF STORE ID),
         ;;   then you must have been able to find via the hash-table the
         ;;   object that maps to that index, which means it wasn't dead,
         ;;   so we must not be here trying to call finalizers for it.
         ;; Smashing 'finalizers' is a good extra step in terms of
         ;; removing dangling references, but if it's just a function,
         ;; there's nothing to smash.
         (cond ((simple-vector-p finalizers) (fill finalizers 0))
               ((consp finalizers) (rplaca finalizers 0))))))))

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
