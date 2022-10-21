(in-package sb-vm)

(export '(arena
          arena-p
          arena-bytes-used
          new-arena
          destroy-arena
          switch-to-arena
          rewind-arena
          unuse-arena
          in-same-arena
          c-find-heap->arena
          show-heap->arena))

;;; The arena structure has to be created in the arena,
;;; but the constructor can't do it (chicken-and-egg problem).
(defstruct (arena (:constructor nil))
  (base-address 0 :type word)
  (length 0 :type word)
  (free-pointer 0 :type word)
  ;; an opaque value which can be used by a threads in a thread pool to detect
  ;; that this arena was reset, by comparing to a cached value in the thread.
  (cookie (cons t t))
  ;; Multiple (unrelated) arenas are allowed.
  ;; also might want an extension concept - multiple backing ranges
  ;; of memory for a single logical arena.
  (next-arena 0))
(defun arena-bytes-used (a)
  (- (arena-free-pointer a) (arena-base-address a)))

(defun unuse-arena ()
  #+system-tlabs
  (when (arena-p (thread-current-arena))
    (switch-to-arena 0)))

(defun rewind-arena (a)
  #+system-tlabs
  (setf (arena-cookie a) (cons t t)
        (arena-free-pointer a) (+ (arena-base-address a)
                                  (sb-ext:primitive-object-size a)))

  a)

(defparameter default-arena-size (* 10 1024 1024 1024))

(defun new-arena ()
  #-system-tlabs :placeholder
  #+system-tlabs
  (let* ((size default-arena-size)
         (mem (allocate-system-memory size))
         (layout (find-layout 'arena)))
    (setf (sap-ref-word mem 0) (compute-object-header (1+ (dd-length (wrapper-dd layout)))
                                                      instance-widetag))
    (let ((arena (%make-lisp-obj (sap-int (sap+ mem instance-pointer-lowtag)))))
      (%set-instance-layout arena layout)
      (setf (arena-base-address arena) (sap-int mem)
            (arena-length arena) size
            (arena-free-pointer arena)
            (sap-int (sap+ mem (primitive-object-size arena))))
      arena)))

;;; Once destroyed, it is not legal to access the structure
;;; since the structure itself is in the arena.
;;; BUG: must unlink from arena chain. Not safe to use this yet.
(defun destroy-arena (arena)
  (deallocate-system-memory (arena-base-address arena) (arena-length arena))
  nil)

(defmacro with-arena ((arena) &body body)
  (declare (ignorable arena))
  #-system-tlabs `(progn ,@body)
  #+system-tlabs
  `(let ((a ,arena))
     (assert (typep a 'arena))
     ;; maybe allow switching from one arena to another?
     (switch-to-arena a)
     (unwind-protect (progn ,@body) (switch-to-arena 0))))

(declaim (inline cur-thread-stack-object-p))
(defun cur-thread-stack-object-p (x)
  (let ((a (get-lisp-obj-address x)))
    (and (< (sap-int (current-sp)) a
            (sap-int (current-thread-offset-sap thread-control-stack-end-slot))))))

(declaim (inline force-to-heap-p))
(defun force-to-heap-p (x)
  (and (not (zerop (sap-int (current-thread-offset-sap thread-arena-slot))))
       (or (dynamic-space-obj-p x)
           (read-only-space-obj-p x))))

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

;;; TOOD: consider using balanced tree for multiple arenas
(defun find-containing-arena (addr)
  (declare (word addr))
  (let ((chain (sap-ref-lispobj
                (int-sap (find-dynamic-foreign-symbol-address "arena_chain"))
                0)))
    (unless (eql chain 0)
      (do ((arena chain (arena-next-arena arena)))
          ((not arena))
        (when (< (arena-base-address arena) addr (+ (arena-base-address arena)
                                                    (arena-length arena)))
          (return arena))))))

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

(defun c-find-heap->arena ()
  (declare (notinline coerce)) ; "Proclaiming SB-KERNEL:COERCE-TO-LIST to be INLINE, but 1 call to it was..."
  (let* ((v (make-array 10000))
         (n (with-pinned-objects (v)
              (sb-alien:alien-funcall
               (sb-alien:extern-alien "find_dynspace_to_arena_ptrs" (function sb-alien:int sb-alien:unsigned))
               (get-lisp-obj-address v)))))
    (coerce (subseq v 0 n) 'list)))

;;; This global var is just for making 1 arena for testing purposes.
;;; It does not indicate about anything the current thread's arena usage.
;;; For that you have to examine THREAD-ARENA-SLOT.
(sb-ext:define-load-time-global *my-arena* nil)

(defun create-arena ()
  (cond ((null *my-arena*)
         (setq *my-arena* (new-arena))
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
