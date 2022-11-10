(in-package sb-vm)

(export '(arena
          arena-p
          arena-bytes-used
          arena-bytes-wasted
          arena-length
          arena-userdata
          new-arena
          destroy-arena
          switch-to-arena
          rewind-arena
          unuse-arena
          thread-current-arena
          in-same-arena
          dump-arena-objects
          c-find-heap->arena
          show-heap->arena))

;;; A contiguous block is described by 'struct arena_memblk' in C.
;;; There is no corresponding lisp defstruct.
(defmacro arena-memblk-freeptr (memblk) `(sap-ref-sap ,memblk 0))
(defmacro arena-memblk-limit (memblk) `(sap-ref-sap ,memblk ,(ash 1 word-shift)))
(defmacro arena-memblk-next (memblk) `(sap-ref-sap ,memblk ,(ash 2 word-shift)))
(defmacro arena-memblk-padword (memblk) `(sap-ref-sap ,memblk ,(ash 3 word-shift)))

(defmacro do-arena-blocks ((blkvar arena) &body body)
  ;; bind BLK to a SAP pointing to successive 'struct memblk' in arena
  `(let ((,blkvar (int-sap (arena-first-block ,arena))))
     (loop (progn ,@body)
           (setq ,blkvar (arena-memblk-next ,blkvar))
           (if (zerop (sap-int ,blkvar)) (return)))))

;;; Not "just" define-alien-routine because we have to GET-LISP-OBJ-ADDRESS on the arg
(defun arena-bytes-used (arena)
  (alien-funcall (extern-alien "arena_bytes_used" (function unsigned unsigned))
                 (get-lisp-obj-address arena)))

(defun unuse-arena ()
  #+system-tlabs
  (when (arena-p (thread-current-arena))
    (switch-to-arena 0)))

(define-load-time-global *arena-index-generator* 0)
(declaim (fixnum *arena-index-generator*))
(define-load-time-global *arena-lock* (sb-thread:make-mutex))

;;; Release all memblks back to the OS, except the first one associated with this arena.
(defun rewind-arena (arena)
  #+system-tlabs
  (cond ((= (arena-token arena) most-positive-word)
         (bug "Arena token overflow. Need to implement double-precision count"))
        ((eql (arena-link arena) 0)) ; never used - do nothing
        (t
         (alien-funcall (extern-alien "arena_release_memblks" (function void unsigned))
                        (get-lisp-obj-address arena))
         (setf (arena-bytes-wasted arena) 0)
         (incf (arena-token arena))))
  arena)

;;; The arena structure has to be created in the arena,
;;; but the constructor can't do it (chicken-and-egg problem).
;;; There are one or more large blocks of memory associated with
;;; an arena, obtained via malloc(). Allocations within a block are
;;; contiguous but the blocks can be discontiguous.
(defun new-arena (size &optional (growth-amount size) (max-extensions 7))
  (declare (ignorable growth-amount max-extensions))
  (assert (>= size 65536))
  "Create a new arena of SIZE bytes which can be grown additively by GROWTH-AMOUNT
one or more times, not to exceed MAX-EXTENSIONS times"
  #-system-tlabs :placeholder
  #+system-tlabs
  (let ((layout (find-layout 'arena))
        (index (with-system-mutex (*arena-lock*) (incf *arena-index-generator*)))
        (arena (%make-lisp-obj
                (alien-funcall (extern-alien "sbcl_new_arena" (function unsigned unsigned))
                               size))))
    (%set-instance-layout arena layout)
    (setf (arena-max-extensions arena) max-extensions
          (arena-growth-amount arena) growth-amount
          (arena-max-extensions arena) max-extensions
          (arena-index arena) index
          (arena-token arena) 1
          (arena-userdata arena) nil)
    arena))

(eval-when (:compile-toplevel)
;;; Caution: this vop potentially clobbers all registers, but it doesn't declare them.
;;; It's safe to use only from DESTROY-ARENA which, being an ordinary full call,
;;; is presumed not to preserve registers.
(define-vop (delete-arena)
  (:args (x :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg :offset rdi-offset :from (:argument 0)) rdi)
  (:vop-var vop)
  (:generator 1
    (move rdi x)
    (pseudo-atomic ()
      (inst call (make-fixup "sbcl_delete_arena" :foreign))))))

;;; Destroy memory associated with ARENA, unlinking it from the global chain.
;;; Note that we do not recycle arena IDs. It would be dangerous to do so, because a thread
;;; might believe it has a valid TLAB in the deleted arena if it randomly matched on the
;;; ID and token. That's a valid argument for making arena tokens gobally unique
;;; (the way it used to work before I made tokens arena-specific)
(defun destroy-arena (arena)
  ;; C is responsible for most of the cleanup.
  (%primitive delete-arena arena)
  ;; It is illegal to access the structure now, since that was in the arena.
  ;; So return an arbitrary success indicator. It might happen that you can accidentally
  ;; refer to the structure, but technically that constitutes a use-after-free bug.
  t)

(defmacro with-arena ((arena) &body body)
  (declare (ignorable arena))
  #-system-tlabs `(progn ,@body)
  #+system-tlabs
  `(let ((a ,arena))
     (assert (typep a 'arena))
     ;; maybe allow switching from one arena to another?
     (switch-to-arena a)
     (unwind-protect (progn ,@body) (switch-to-arena 0))))

#+system-tlabs
(progn
(declaim (inline cur-thread-stack-object-p))
(defun cur-thread-stack-object-p (x)
  (let ((a (get-lisp-obj-address x)))
    (and (< (sap-int (current-sp)) a
            (sap-int (current-thread-offset-sap thread-control-stack-end-slot))))))

(declaim (inline force-to-heap-p))
(defun force-to-heap-p (x)
  (and (not (zerop (sap-int (current-thread-offset-sap thread-arena-slot))))
       (or (dynamic-space-obj-p x)
           ;; FIXME: is checking for read-only still correct???
           (read-only-space-obj-p x)))))

(defmacro in-same-arena ((object reason) &rest forms)
  (declare (ignorable object reason))
  #-system-tlabs `(progn ,@forms)
  #+system-tlabs
  `(dx-flet ((thunk () ,@forms))
     (let ((allocating-in-arena-p
            (not (zerop (sap-int (current-thread-offset-sap thread-arena-slot)))))
           (.obj. ,object))
     ;; - if it was on stack, then don't switch to/from an arena
     ;; - or if object was in readonly space. This can happen with adjust-array
     ;;   on an initially zero-length vector that was moved into readonly space.
     (if (or (cur-thread-stack-object-p .obj.)
             (read-only-space-obj-p .obj.)
             (eq allocating-in-arena-p (not (dynamic-space-obj-p .obj.))))
         (funcall #'thunk)
         (call-using-arena #'thunk .obj. ',reason)))))

;;; backward-compatibility assuming no extension blocks. DON'T USE THIS!
(defun arena-base-address (arena) (arena-first-block arena))

;;; Possible TOOD: if this needs to be efficient, then we would need
;;; a balanced tree which maps each 'struct memblk' to an arena.
;;; This would be complicated by the fact that extension occurs in C code,
;;; so C would have to maintain the balanced tree structure.
;;; It may not be terribly important to optimize.
(defun find-containing-arena (addr)
  (declare (word addr))
  (let ((chain (sap-ref-lispobj (foreign-symbol-sap "arena_chain" t) 0)))
    (unless (eql chain 0)
      (do ((arena chain (arena-link arena)))
          ((not arena))
        (do-arena-blocks (memblk arena)
          (when (< (sap-int memblk) addr (sap-int (arena-memblk-freeptr memblk)))
            (return arena)))))))

(defun maybe-show-arena-switch (direction reason)
  (declare (ignore direction reason)))
#+system-tlabs
(defun call-using-arena (thunk object reason)
  (if (dynamic-space-obj-p object)
      (let ((arena (thread-current-arena)))
        (aver (%instancep arena))
        (maybe-show-arena-switch "from" reason)
        (progn (switch-to-arena 0)
               (multiple-value-prog1 (funcall thunk) (switch-to-arena arena))))
      (let ((usable-arena (find-containing-arena (get-lisp-obj-address object))))
        (or usable-arena
            (bug "Object ~X looks arena-allocated but can't find where"
                 (get-lisp-obj-address object)))
        (maybe-show-arena-switch "to" reason)
        (progn (switch-to-arena usable-arena)
               (multiple-value-prog1 (funcall thunk) (switch-to-arena 0))))))

#+system-tlabs
(defun c-find-heap->arena (&optional arena)
  (declare (notinline coerce)) ; "Proclaiming SB-KERNEL:COERCE-TO-LIST to be INLINE, but 1 call to it was..."
  (let* ((result (make-array 10000))
         (n (with-pinned-objects (arena result)
              (alien-funcall
               (extern-alien "find_dynspace_to_arena_ptrs" (function int unsigned unsigned))
               (if arena (get-lisp-obj-address arena) 0)
               (get-lisp-obj-address result)))))
    (coerce (subseq result 0 n) 'list)))

;;; This global var is just for making 1 arena for testing purposes.
;;; It does not indicate about anything the current thread's arena usage.
;;; For that you have to examine THREAD-ARENA-SLOT.
(sb-ext:define-load-time-global *my-arena* nil)

(defparameter default-arena-size (* 10 1024 1024 1024))
(defun create-arena ()
  (cond ((null *my-arena*)
         (setq *my-arena* (new-arena default-arena-size))
         (format t "Arena memory @ ~x (struct @ ~x)~%"
                 (arena-base-address *my-arena*)
                 (sb-kernel:get-lisp-obj-address *my-arena*)))
        (t
         (format t "~&Already created~%"))))

(defglobal *foo* nil)
(defun arena-smoketest ()
  (setq *foo* nil)
  (with-arena (*my-arena*)
    (let (t1 t2)
      (setq t1 (sb-thread:make-thread
                (lambda () (declare (notinline make-list)) (vector (make-list 5 :initial-element #\a)))))
      (setq t2 (sb-thread:make-thread
                (lambda () (declare (notinline make-list)) (vector (make-list 6 :initial-element #\b)))))
      (dotimes (i 10) (sb-ext:atomic-push (cons 3 i) *foo*))
      (sb-thread:join-thread t1)
      (sb-thread:join-thread t2))))

(defmethod print-object ((self arena) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "id=~D used=~D waste=~D"
            (arena-index self)
            (arena-bytes-used self)
            (arena-bytes-wasted self))))
