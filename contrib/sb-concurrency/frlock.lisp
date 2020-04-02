;;;; -*-  Lisp -*-
;;;;
;;;; FRLocks for SBCL
;;;;
;;;; frlock is a "fast read lock", which allows readers to gain unlocked access
;;;; to values, and provides post-read verification. Readers which intersected
;;;; with writers need to retry. frlock is very efficient when there are many
;;;; readers and writes are both fast and relatively scarce. It is, however,
;;;; unsuitable when readers and writers need exclusion, such as with SBCL's
;;;; current hash-table implementation.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package :sb-concurrency)

;;; This algorithm is very subtle and relies on meticulous placement of
;;; memory barriers for CPUs that do not preserve LOAD-LOAD program order
;;; or STORE-STORE program order.

;;; Our code suffered from two problems:
;;; 1. The BARRIER macro, under the best of circumstances, has such godawful
;;;    syntax that you can't tell whether it's right. The inventor (Alastair?)
;;;    has practically disavowed it, suggesting that you should never put code
;;;    in the body of the macro, but instead should express your "before"
;;;    and "after" pieces of code so that you can see where they happen.
;;;
;;; 2. It is my educated guess that Nikodemus copied a proposed patch
;;;    from LKML almost bug-for-bug. The algorithm appeared in
;;;    https://lwn.net/Articles/7388/ and, exactly like ours, had a read barrier
;;;    in the wrong place.  Nikodemus had to one-up them though, so added
;;;    an extra bug - our store barrier was wrong, where theirs wasn't.
;;;    Possibly because our BARRIER macro is confusing, I don't know.
;;;    At the time, barriers were not well understood or used, and x86
;;;    always did the right thing [see * footnote though] with no barrier,
;;;    and the Linux contributor said the patch was only tested with x86.
;;;
;;; A subsequent LWN article https://lwn.net/Articles/21379/ said that two
;;; counters should not have been necessary in the algorithm.
;;; And yet another (https://lwn.net/Articles/21812/) said
;;;   "Despite all the discussion, there has been no good argument that original
;;;    x86_64 code was incorrect in the way it used wmb and rmb."
;;; which is easily refuted (below).
;;; But then it went ahead and changed the code to be correct using a
;;; new algorithm posted in https://lwn.net/Articles/21812/.
;;; The latter has a read barrier _after_ reading the counter, but it also
;;; does away with separate pre- and post- counters, which we should do.
;;;
;;; * As to the only way that x86-64 will display relaxed memory order, see
;;;   https://bartoszmilewski.com/2008/11/05/who-ordered-memory-fences-on-an-x86/
;;;   in which he presents one algorithm that can go wrong without
;;;   an explicit fence instruction. (This is extremely rare for x86.)
;;;   But it also seems that x86-64 emulators and/or VMs could encounter our bug,
;;;   possibly they model symmetric multiprocessing in a way that is not 100%
;;;   faithful to what physical Intel or AMD hardware would do.

;;; Anyway, here is a perfectly good argument that the original code was wrong.
;;;  - initial conditions: (X,Y) = (x0,y0); pre = post = 3
;;;  - thread A wants to load a consistent view of X and Y
;;;  - thread B wants to set them to (x1,y1)

;;; Using all correct barriers except for one misplaced rmb(), the
;;; "observe post" and "observe X" steps can occur out-of-order thusly:
;;;
;;;     Thread A                    Thread B
;;;   ------------                ------------
;;;   rmb() /* no effect */
;;;   observe X, gets x0
;;;                                 pre <- 4
;;;                                 wmb()
;;;                                 Y <- y1
;;;                                 X <- x1
;;;                                 wmb()
;;;                                 post <- 4
;;;   observe post, gets 4
;;;   observe Y, gets y1
;;;   rmb()
;;;   observe pre, gets 4
;;;
;;; Thread A thinks it saw a consistent view of (x0,y1) because pre = post = 4.
;;; The correct C code would have been
;;;    "unsigned post = rw->post_sequence; rmb(); return post;"
;;; [Note: rmb() = 'read' memory barrier; wmb() = 'write' memory barrier]

(defstruct (frlock (:constructor %make-frlock (name))
                   (:predicate nil)
                   (:copier nil))
  "FRlock, aka Fast Read Lock.

Fast Read Locks allow multiple readers and one potential writer to operate in
parallel while providing for consistency for readers and mutual exclusion for
writers.

Readers gain entry to protected regions without waiting, but need to retry if
a writer operated inside the region while they were reading. This makes frlocks
very efficient when readers are much more common than writers.

FRlocks are NOT suitable when it is not safe at all for readers and writers to
operate on the same data in parallel: they provide consistency, not exclusion
between readers and writers. Hence using an frlock to eg. protect an SBCL
hash-table is unsafe. If multiple readers operating in parallel with a writer
would be safe but inconsistent without a lock, frlocks are suitable.

The recommended interface to use is FRLOCK-READ and FRLOCK-WRITE, but those
needing it can also use a lower-level interface.

Example:

  ;; Values returned by FOO are always consistent so that
  ;; the third value is the sum of the two first ones.
  (let ((a 0)
        (b 0)
        (c 0)
        (lk (make-frlock)))
    (defun foo ()
       (frlock-read (lk) a b c))
    (defun bar (x y)
       (frlock-write (lk)
         (setf a x
               b y
               c (+ x y)))))
"
  (mutex (make-mutex :name "FRLock mutex") :type mutex :read-only t)
  ;; Using FIXNUM counters makes sure we don't need to cons a bignum
  ;; for the return value, ever.
  (pre-counter 0 :type (and unsigned-byte fixnum))
  (post-counter 0 :type (and unsigned-byte fixnum))
  ;; On 32bit platforms a fixnum can roll over pretty easily, so we also use
  ;; an epoch marker to keep track of that.
  (epoch (list t) :type cons)
  (name nil))
(declaim (sb-ext:freeze-type frlock))

(setf (documentation 'frlock-name 'function)
      "Name of an FRLOCK. SETFable.")

(declaim (inline make-frlock))
(defun make-frlock (&key name)
  "Returns a new FRLOCK with NAME."
  (%make-frlock name))

(declaim (inline frlock-read-begin))
(defun frlock-read-begin (frlock)
  "Start a read sequence on FRLOCK. Returns a read-token and an epoch to be
validated later.

Using FRLOCK-READ instead is recommended."
  (let ((a (frlock-post-counter frlock))
        (b (frlock-epoch frlock)))
    (barrier (:read))
    ;; now any state that user observes can't be speculated backwards
    ;; prior to the loads of A and B.
    (values a b)))

(declaim (inline frlock-read-end))
(defun frlock-read-end (frlock)
  "Ends a read sequence on FRLOCK. Returns a token and an epoch. If the token
and epoch are EQL to the read-token and epoch returned by FRLOCK-READ-BEGIN,
the values read under the FRLOCK are consistent and can be used: if the values
differ, the values are inconsistent and the read must be restated.

Using FRLOCK-READ instead is recommended.

Example:

  (multiple-value-bind (t0 e0) (frlock-read-begin *fr*)
    (let ((a (get-a))
          (b (get-b)))
      (multiple-value-bind (t1 e1) (frlock-read-end *fr*)
        (if (and (eql t0 t1) (eql e0 e1))
            (list :a a :b b)
            :aborted))))
"
  (barrier (:read))
  (values (frlock-pre-counter frlock)
          (frlock-epoch frlock)))

(defmacro frlock-read ((frlock) &body value-forms)
  "Evaluates VALUE-FORMS under FRLOCK till it obtains a consistent
set, and returns that as multiple values."
  (once-only ((frlock frlock))
    (with-unique-names (t0 t1 e0 e1)
      (let ((syms (make-gensym-list (length value-forms))))
        `(loop
           (multiple-value-bind (,t0 ,e0) (frlock-read-begin ,frlock)
             (let ,(mapcar 'list syms value-forms)
               (barrier (:compiler))
               (multiple-value-bind (,t1 ,e1) (frlock-read-end ,frlock)
                (when (and (eql ,t1 ,t0) (eql ,e1 ,e0))
                  (return (values ,@syms)))))))))))

;;; Actual implementation.
(defun %%grab-frlock-write-lock (frlock wait-p timeout)
  (when (grab-mutex (frlock-mutex frlock) :waitp wait-p :timeout timeout)
    (let ((new (logand most-positive-fixnum (1+ (frlock-pre-counter frlock)))))
      ;; Here's our roll-over protection: if a reader has been unlucky enough
      ;; to stand inside the lock long enough for the counter to go from 0 to
      ;; 0, they will still be holding on to the old epoch. While it is
      ;; extremely unlikely, it isn't quite "not before heath death of the
      ;; universe" stuff: a 30 bit counter can roll over in a couple of
      ;; seconds -- and a thread can easily be interrupted by eg. a timer for
      ;; that long, so a pathological system could be have a thread in a
      ;; danger-zone every second. Run that system for a year, and it would
      ;; have a 1 in 3 chance of hitting the incipient bug. Adding an epoch
      ;; makes sure that isn't going to happen.
      (when (zerop new)
        (setf (frlock-epoch frlock) (list t)))
      (setf (frlock-pre-counter frlock) new))
    (barrier (:write))
    t))

;;; Interrupt-mangling free entry point for FRLOCK-WRITE.
(declaim (inline %grab-frlock-write-lock))
(defun %grab-frlock-write-lock (frlock &key (wait-p t) timeout)
  (%%grab-frlock-write-lock frlock wait-p timeout))

;;; Normal entry-point.
(declaim (inline grab-frlock-write-lock))
(defun grab-frlock-write-lock (frlock &key (wait-p t) timeout)
  "Acquires FRLOCK for writing, invalidating existing and future read-tokens
for the duration. Returns T on success, and NIL if the lock wasn't acquired
due to eg. a timeout. Using FRLOCK-WRITE instead is recommended."
  (without-interrupts
    (allow-with-interrupts (%%grab-frlock-write-lock frlock wait-p timeout))))

(declaim (inline release-frlock-write-lock))
(defun release-frlock-write-lock (frlock)
  "Releases FRLOCK after writing, allowing valid read-tokens to be acquired again.
Signals an error if the current thread doesn't hold FRLOCK for writing. Using FRLOCK-WRITE
instead is recommended."
  (barrier (:write)) ; ensure all user state is published
  (setf (frlock-post-counter frlock)
        (logand most-positive-fixnum (1+ (frlock-post-counter frlock))))
  (release-mutex (frlock-mutex frlock) :if-not-owner :error))

(defmacro frlock-write ((frlock &key (wait-p t) timeout) &body body)
  "Executes BODY while holding FRLOCK for writing."
  (once-only ((frlock frlock))
    (with-unique-names (got-it)
      `(without-interrupts
         (let (,got-it)
           (unwind-protect
                (when (setf ,got-it (allow-with-interrupts
                                      (%grab-frlock-write-lock ,frlock :timeout ,timeout
                                                                      :wait-p ,wait-p)))
                  (with-local-interrupts ,@body))
             (when ,got-it
               (release-frlock-write-lock ,frlock))))))))
