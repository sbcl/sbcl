;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;; Not loaded until warm build. exports.lisp only affects symbols
;;; that are visible to genesis.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(sb-ext::search-roots) "SB-EXT"))

;;; Convert each path to (TARGET . NODES)
;;; where the first node in NODES is one of:
;;;
;;;  1. :static ; if a heap object (even if not "technically" static)
;;;  2. :pinned ; if unknown thread stack pins the root of the path
;;;  3. (#<thread>|thread-name symbol boolean)
;;;  4. (#<thread>|thread-name guessed-PC)
;;;
;;; For case 3, the boolean value is T if the symbol's current value in TLS
;;; is the root; otherwise a shadowed value on the binding stack is the root.
;;; For case 4, the guessed-PC is a probable code address within a function
;;; whose stack frame contains a reference to the first object in the path.
;;
;;; The rest of the path is a list of (OBJECT . WORD-INDEX).
(defun preprocess-traceroot-results (inputs outputs &aux results)
  (flet ((collapse-lists (input)
           (let* ((path (coerce input 'vector))
                  (path-length (length path))
                  (index (- path-length 2)))
             ;; Combine successive CDR operations by scanning right-to-left
             ;; for adjacent entries that represent two CDR operations.
             ;; Repeat until nothing else can be spliced out.
             ;; This entails just 1 linear scan. A left-to-right algorithm
             ;; would be less efficient.
             (loop
               (when (< index 1)
                 (return (coerce (subseq path 0 path-length) 'list)))
               (let* ((this-node (aref path index))
                      (this-object (car this-node))
                      (successor-index (1+ index))
                      (successor-node (aref path successor-index))
                      (successor-object (car successor-node)))
                 (when (and (consp this-object)
                            (consp successor-object)
                            (eql (cdr this-node) 1) ; operation is CDR
                            (>= (cdr successor-node) 1)) ; operation is 1 or more CDRs
                   (setf (cdr this-node) (1+ (cdr successor-node)))
                   (replace path path :start1 successor-index :start2 (1+ successor-index))
                   (decf path-length))
                 (decf index))))))
    (dovector (path outputs (nreverse results))
      (let ((target (weak-pointer-value (pop inputs))))
        (when (and target (listp path))
          (setq path (nreverse path))
          (let* ((root (car path))
                 (root-kind (car root)))
            (if (eq root-kind 0) ; heap object
                (rplaca path :static)
                (let* ((thread (sap-ref-lispobj
                                (sb-int:descriptor-sap (cadr root))
                                (ash sb-vm::thread-lisp-thread-slot sb-vm:word-shift)))
                       (extra (cddr root))
                       (symbol
                        (unless (eql root-kind 1)
                          #+sb-thread
                          (sb-vm::symbol-from-tls-index (ash extra sb-vm:n-fixnum-tag-bits))
                          #-sb-thread
                          extra)))
                  (awhen (and thread (sb-thread:thread-name thread))
                    (setq thread it)) ; if nameless, just show as #<thread ...>
                  (rplaca path (ecase root-kind
                                 (1                   ; control stack
                                  (if thread
                                      `(,thread ,extra)
                                      :pinned))
                                 (2 `(,thread ,symbol nil)) ; binding stack
                                 (3 `(,thread ,symbol t))))))) ; TLS
          (push (cons target (collapse-lists path)) results))))))

(defun print-traceroot-path (path &key (stream *standard-output*) (multiline t))
  (destructuring-bind (target root . rest) path
    (cond (multiline
           (format stream "Path to ~S:~%" target)
           (cond ((eq root :static))
                 ((eq root :pinned)
                  ;; found in pins table, but thread unknown,
                  ;; as happens when run without GC
                  (format stream "from pinned object~%"))
                 ((symbolp (second root))
                  (destructuring-bind (thread symbol currentp) root
                    (format stream "from ~S ~S (~:[binding~;TLS~])~%"
                            thread symbol currentp)))
                 (t
                  (destructuring-bind (thread pc) root
                    (format stream "from ~S PC=~X in ~A~%"
                            thread (sap-int pc) (sb-di::code-header-from-pc pc)))))
           (let ((*print-pretty* nil)
                 (*print-circle* t))
             (dolist (node rest)
               (destructuring-bind (obj . slot) node
                 (format stream " ~D ~16X [~4D] "
                         (or (sb-kernel:generation-of obj) "S")
                         (sb-kernel:get-lisp-obj-address obj)
                         slot)
                 ;; Some objects print fairly concisely, so we'll show them.
                 ;; But do NOT show CONS, VECTOR, INSTANCE. Especially not those.
                 (typecase obj
                   (symbol
                    (format stream "~/sb-ext:print-symbol-with-prefix/~%" obj))
                   ((or package sb-kernel:fdefn sb-kernel:code-component
                        pathname sb-impl::host hash-table)
                    (format stream "~S~%" obj))
                   (t
                    ;; We want to distinguish between SB-PCL::CACHE and every other type
                    ;; that anyone invents named CACHE (of which there are many!) but not be
                    ;; so pedantic as to print as COMMON-LISP:CONS
                    (let ((type (type-of obj)))
                      (if (and (symbolp type) (eq (symbol-package type) *cl-package*))
                          ;; Putting (FORMAT stream (IF "s1" "s2") type) here failed the post-build
                          ;; assertion about strings containing SB- packages. (FIXME)
                          (format stream "a ~(~a~)" type)
                          (format stream "a ~/sb-ext:print-symbol-with-prefix/" type)))
                    (when (consp obj)
                      (write-string " = " stream)
                      (write obj :stream stream :level 1 :length 3 :pretty nil))
                    (terpri)))))))
          (t
           (let ((*print-pretty* nil))
             (when (consp root)
               (if (symbolp (second root))
                   (destructuring-bind (thread symbol currentp) root
                     (format stream "~S:~S {~:[binding~;TLS~]}"
                             thread symbol currentp))
                   (destructuring-bind (thread pc) root
                     (format stream "~S:#x~X" thread (sap-int pc)))))
             (dolist (node rest)
               (destructuring-bind (obj . slot) node
                 (format stream " -> (~S) #x~X[~D]"
                         (type-of obj) (sb-kernel:get-lisp-obj-address obj) slot)))
             (format stream " -> #x~x~%" (sb-kernel:get-lisp-obj-address target)))))))

(defun print-traceroot-paths (paths &key (stream *standard-output*) (multiline t))
  (dolist (path paths)
    (print-traceroot-path path :stream stream :multiline multiline)))

(declaim (ftype (function ((or list sb-ext:weak-pointer)
                           &key
                           (:criterion (member :oldest :pseudo-static :static))
                           (:ignore list)
                           (:print (or boolean (eql :verbose)))))
                search-roots))
(defun search-roots (weak-pointers &key (criterion :oldest)
                                        (ignore '(* ** *** / // ///))
                                        (print t))
  "Find roots keeping the targets of WEAK-POINTERS alive.

WEAK-POINTERS must be a single SB-EXT:WEAK-POINTER or a list of those,
pointing to objects for which roots should be searched.

CRITERION determines just how rooty (how deep) a root must be in order
to be considered. Possible values are:

  :OLDEST
     This says we can stop upon seeing an object in the oldest gen to
     GC, or older. This is the easiest test to satisfy.

  :PSEUDO-STATIC
     This is usually the same as :OLDEST, unless the oldest gen to GC
     has been decreased.

  :STATIC
     To find a root of an image-backed object, you want to stop only at
     a truly :STATIC object.

IGNORE is a list of objects to treat as if nonexistent in the heap.
It can often be useful for finding a path to an interned symbol other than
through its package by specifying the package as an ignored object.

PRINT controls whether discovered paths should be returned or
printed. Possible values are

  :VERBOSE
    Return no values. Print discovered paths using a verbose format
    with each node of each path on a separate line.

  true (other than :VERBOSE)
    Return no values. Print discovered paths using a compact format
    with all nodes of each path on a single line.

  NIL
    Do not print any output. Instead return the discovered paths as a
    list of lists. Each list has the form

      (TARGET . (ROOT NODE*))

    where TARGET is one of the target of one of the WEAK-POINTERS.

    ROOT is a description of the root at which the path starts and has
    one of the following forms:

      :STATIC
        If the root of the path is a non-collectible heap object.

      :PINNED
        If an unknown thread stack pins the root of the path.

      ((THREAD-NAME | THREAD-OBJECT) SYMBOL CURRENTP)
        If the path begins at a special binding of SYMBOL in a
        thread. CURRENTP is a BOOLEAN indicating whether the value is
        current or shadowed by another binding.

      ((THREAD-NAME | THREAD-OBJECT) GUESSED-PC)
        If the path begins at a lexical variable in the function whose
        code contains GUESSED-PC.

    Each NODE in the remainder of the path is a cons (OBJECT . SLOT)
    indicating that the slot at index SLOT in OBJECT references the
    next path node.

Experimental: subject to change without prior notice."
  (let* ((input (ensure-list weak-pointers))
         (output (make-array (length input)))
         (ignore (if ignore (coerce ignore 'simple-vector) 0))
         (criterion (ecase criterion
                      (:oldest 0)
                      (:pseudo-static 1)
                      (:static 2)))
         result)
    (with-alien ((gc-pathfind (function int unsigned unsigned unsigned int)
                              :extern))
      (without-gcing
          (when (sb-kernel::try-acquire-gc-lock
                 (sb-kernel::gc-stop-the-world)
                 (setq result
                       (alien-funcall gc-pathfind
                                      (get-lisp-obj-address input)
                                      (get-lisp-obj-address output)
                                      (get-lisp-obj-address ignore)
                                      criterion)))
            (sb-kernel::gc-start-the-world))))
    (case result
      ((nil) (error "Could not stop all threads"))
      (-1 (error "Input is not a proper list of weak pointers.")))
    (let ((paths (preprocess-traceroot-results input output)))
      (cond (print
             (print-traceroot-paths paths :multiline (eq print :verbose))
             (values))
            (t
             paths)))))
