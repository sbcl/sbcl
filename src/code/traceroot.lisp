;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-EXT")

;;; Not loaded until warm build. package-data-list only affects symbols
;;; that are visible to genesis.
(export '(search-roots gc-and-search-roots))

(define-alien-variable  "gc_object_watcher" unsigned)
(define-alien-variable  "gc_traceroot_criterion" int)

#+sb-thread
(defun find-symbol-from-tls-index (index)
  (unless (zerop index)
    ;; Search interned symbols first since that's probably enough
    (do-all-symbols (symbol)
      (when (= (sb-kernel:symbol-tls-index symbol) index)
        (return-from find-symbol-from-tls-index symbol)))
    ;; A specially bound uninterned symbol? how awesome
    (sb-vm::map-allocated-objects
     (lambda (obj type size)
       (declare (ignore size))
       (when (and (= type sb-vm:symbol-widetag)
                  (= (sb-kernel:symbol-tls-index obj) index))
         (return-from find-symbol-from-tls-index obj)))
     :all))
  0)

(defun find-lisp-thread-from-thread-struct (addr)
  ;; It is of course possible to do this without consing the list
  ;; of all threads, but I don't care.
  (dolist (thread (sb-thread:list-all-threads))
    (when (= (sb-thread::thread-primitive-thread thread) addr)
      (return thread))))

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
  (dovector (path outputs (nreverse results))
    (let ((target (weak-pointer-value (pop inputs))))
      (when (and target (listp path))
        (setq path (nreverse path))
        (let* ((root (car path))
               (root-kind (car root)))
          (if (eq root-kind 0) ; heap object
              (rplaca path :static)
              (let* ((thread (find-lisp-thread-from-thread-struct
                              (ash (cadr root) sb-vm:n-fixnum-tag-bits)))
                     (extra (cddr root))
                     (symbol
                      (unless (eq root-kind 1)
                        #+sb-thread
                        (find-symbol-from-tls-index (ash extra sb-vm:n-fixnum-tag-bits))
                        #-sb-thread
                        extra)))
                (awhen (and thread (sb-thread:thread-name thread))
                  (setq thread it)) ; if nameless, just show as #<thread ...>
                (ecase root-kind
                  (1 ; control stack
                   (if thread
                       (rplaca path `(,thread ,extra))
                       (rplaca path :pinned)))
                  (2 (rplaca path `(,thread ,symbol nil))) ; binding stack
                  (3 (rplaca path `(,thread ,symbol t))))))) ; TLS
        (push (cons target path) results)))))

(defun print-traceroot-paths (paths &optional (multiline t))
  (dolist (path paths)
    (destructuring-bind (target root . rest) path
      (cond (multiline
             (format t "Path to ~S:~%" target)
             (cond ((eq root :static))
                   ((eq root :pinned)
                    ;; found in pins table, but thread unknown,
                    ;; as happens when run without GC
                    (format t "from pinned object~%"))
                   ((symbolp (cadr root))
                    (destructuring-bind (thread symbol currentp) root
                      (if currentp
                          (format t "from ~S ~S (TLS)~%" thread symbol)
                          (format t "from ~S ~S (binding)~%" thread symbol))))
                   (t
                    (destructuring-bind (thread pc) root
                      (format t "from ~S PC=~X in ~A~%"
                              thread (sap-int pc) (sb-di::code-header-from-pc pc)))))
             (let ((*print-pretty* nil))
               (dolist (node rest)
                 (let ((obj (car node))
                       (slot (cdr node)))
                   (format t " ~D ~16X [~4D] "
                           (or (sb-kernel:generation-of obj) "S")
                           (sb-kernel:get-lisp-obj-address obj)
                           slot)
                   ;; Some objects print fairly concisely, so we'll show them.
                   ;; But do NOT show CONS, VECTOR, INSTANCE. Especially not those.
                   (typecase obj
                     ((or package symbol sb-kernel:fdefn sb-kernel:code-component
                          pathname sb-impl::host hash-table)
                      (format t "~S~%" obj))
                     (t
                      (format t "a ~(~a~)~%" (type-of obj))))))))
            (t
             (if (consp root)
                 (if (symbolp (cadr root))
                     (destructuring-bind (thread symbol currentp) root
                       (format t "~S:~S {~A}" thread symbol (if currentp "TLS" "binding")))
                     (destructuring-bind (thread pc) root
                       (format t "~S:#x~X" thread (sap-int pc)))))
             (dolist (node rest)
               (format t " -> #x~X[~D]"
                       (sb-kernel:get-lisp-obj-address (car node))
                       (cdr node)))
             (format t " -> #x~x~%" (sb-kernel:get-lisp-obj-address target)))))))

(flet ((criterion-value (criterion)
         (ecase criterion
           ;; CRITERION determines just how rooty (how deep) a root must be.
           ;; :OLDEST says we can stop upon seeing an object in the oldest
           ;; gen to GC, or older. This is the easiest test to satisfy.
           ;; To find a root of an image-backed object, you want to stop only
           ;; at a truly :STATIC object, because everything dumped was
           ;; effectively :PSEUDO-STATIC, which is usually the same
           ;; as :OLDEST, unless the oldest gen to GC has been decreased.
           (:oldest 0)
           (:pseudo-static 1)
           (:static 2)))
       (return-results (input output print)
         (let ((results (preprocess-traceroot-results input output)))
           (cond (print
                  (print-traceroot-paths results (eq print :verbose))
                  (values))
                 (t
                  results)))))

;;; This is the more accurate of the object liveness proof generators,
;;; as there is no chance for other code to execute in between the
;;; garbage collection and production of the chain of referencing objects.
(defun gc-and-search-roots (wps &optional (criterion :oldest) (print t))
  (declare (type (member nil t :verbose) print))
  (let* ((input (ensure-list wps))
         (output (make-array (length input)))
         (param (cons input output)))
    (declare (truly-dynamic-extent output param))
    (setf gc-traceroot-criterion (criterion-value criterion))
    (sb-sys:with-pinned-objects (param)
      (setf gc-object-watcher (sb-kernel:get-lisp-obj-address param)))
    (gc :full t)
    (setf gc-object-watcher 0)
    (return-results input output print)))

;;; This object liveness proof generator works well enough,
;;; but might be adversely affected by actions of concurrent threads.
(defun search-roots (wps &optional (criterion :oldest) (print t))
  (declare (type (member nil t :verbose) print))
  (let* ((input (ensure-list wps))
         (output (make-array (length input)))
         (param (cons input output)))
    (declare (truly-dynamic-extent output param))
    (sb-sys:without-gcing
      (alien-funcall (extern-alien "prove_liveness" (function int unsigned int))
                     (sb-kernel:get-lisp-obj-address param)
                     (criterion-value criterion)))
    (return-results input output print))))
