;;;; finalization based on weak-keyed hash-table

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(defmacro with-finalizer-store ((var) &body body)
  `(let ((mutex (hash-table-lock (finalizer-id-map **finalizer-store**))))
     ;; This does not inhibit GC, though the hashtable operations will,
     ;; as is (currently) required for tables with non-null weakness.
     (sb!thread::with-recursive-system-lock (mutex)
       ;; Grab the store again inside the lock in case the array was enlarged
       ;; after we referenced the mutex but before we acquired it.
       (let ((,var **finalizer-store**))
         ,@body))))

(defmacro finalizer-recycle-bin (store) `(cdr (elt ,store 0)))
(defmacro finalizer-id-map (store) `(elt ,store 1))
(defmacro finalizer-max-id (store) `(elt ,store 2))

(defun make-finalizer-store (array-length)
  (let* ((v (make-array (the index array-length)))
         (ht (make-hash-table :test 'eq :weakness :key)))
    (macrolet ((frob-hash-table-flag-bit (slot-name val)
                 (let ((dsd (find slot-name
                                  (dd-slots (find-defstruct-description 'hash-table))
                                  :key 'dsd-name)))
                   `(setf (%instance-ref ht ,(dsd-index dsd))
                          (logior (%instance-ref ht ,(dsd-index dsd)) ,val)))))
      (frob-hash-table-flag-bit flags 4))
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
                        &aux (item (if dont-save (list function) function)))
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
                      (sb!thread:barrier (:write)
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

(defun finalizers-deinit ()
  ;; remove :dont-save finalizers
  ;; Renumber the ID range as well, but leave the array size as-is. We could
  ;; probably delete *all* finalizers prior to image dump, because saved
  ;; finalizers can in practice almost never be run, as pseudo-static objects
  ;; don't die, making this more-or-less an exercise in futility.
  (with-finalizer-store (old-store)
    (without-gcing
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
                                     (0 nil) ; all deleted
                                     (1 (svref new 0)) ; reduced to singleton
                                     (t new))))
                                ((atom old) old)) ; a single finalizer to be saved
                     (let ((new-id (incf (finalizer-max-id new-store))))
                       (setf (gethash object (finalizer-id-map new-store)) new-id
                             (svref new-store new-id) it))))
                 old-objects)
        (clrhash old-objects)
        (fill old-store 0)
        (setq **finalizer-store** new-store)))))

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

;;; TODO: start a thread dedicated to finalizations.
;;; It just needs to wait on a semaphore signaled each time GC finishes.
;;; As well, this function can be run in more than one thread concurrently,
;;; if anyone wanted to do that.
(defun run-pending-finalizers ()
  ;; This never acquires the finalizer store lock. Code accordingly.
  (let ((hashtable (finalizer-id-map **finalizer-store**)))
    (loop
     (let ((cell (hash-table-culled-values hashtable)))
       ;; Remove an item from the pending list
       (unless cell (return))
       ;; This is like atomic-pop, but its obtains the first cons cell
       ;; in the list, not the car of the first cons.
       (loop (let ((actual (cas (hash-table-culled-values hashtable)
                                cell (cdr cell))))
               (if (eq actual cell) (return) (setq cell actual))))
       (let* ((id (car cell))
              ;; No other thread can modify **FINALIZER-STORE** at index ID
              ;; because the table no longer contains an object mapping to
              ;; that element; however the vector could be grown at any point,
              ;; so always load the vector again before dereferencing.
              (store **finalizer-store**)
              ;; I don't think we need a barrier; this has a data dependency
              ;; on (CAR CELL) and STORE. (Alpha with threads, anyone?)
              (finalizers (svref store id))) ; [1] load
         (setf (svref store id) 0)           ; [2] store
         (flet ((call (finalizer)
                  (let ((fun (if (consp finalizer) (car finalizer) finalizer)))
                    (handler-case (funcall fun)
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
               ((consp finalizers) (rplaca finalizers 0)))
         ;; Recycle the ID by linking CELL into the recycle bin.
         (let* ((list (svref store 0))
                (old (cdr list)))
           (loop (let ((actual (cas (cdr list) old (rplacd cell old))))
                   (if (eq actual old) (return) (setq old actual))))))))))
