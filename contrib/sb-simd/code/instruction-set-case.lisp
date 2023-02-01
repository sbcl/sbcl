(in-package #:sb-simd-internals)

;;; Lisp supports an image-based workflow, so it is entirely possible that
;;; a program is transferred from one machine to another one that supports
;;; different instructions.  For example, a video game developer might
;;; create a game on a machine supporting x86 instructions up to AVX2,
;;; create an image-based executable, and ship it to a customer whose
;;; machine supports only x86 instructions up to SSE4.1.
;;;
;;; This file defines a macro for conditionally executing code based on the
;;; available instructions.  Special care was taken to make this
;;; conditional selection as efficient as possible.  On x86, it should
;;; compile to a jump table whose index is computed only at load time and
;;; whenever the image is restarted.

;;; For each instruction-set-case macro appearing in source code, we
;;; register an IDISPATCH structure that tracks the available clauses, and
;;; the index of the currently active clause.  Whenever the Lisp image is
;;; restarted, we traverse all IDISPATCH structures and recompute the index
;;; of the currently active clause.
(defparameter *idispatch-table*
  (make-hash-table :weakness :key :synchronized t))

(defstruct (idispatch
            (:constructor %make-idispatch)
            (:predicate idispatchp))
  ;; The clauses supplied to the instruction-set-case macro.
  (clauses nil :type list :read-only t)
  ;; A list of lists of instruction sets - one for each clause.
  (isets nil :type list :read-only t)
  ;; The index of the currently active clause.
  (index 0 :type (unsigned-byte 8)))

(defun make-idispatch (isets clauses)
  (let ((idispatch (%make-idispatch
                    :clauses clauses
                    :isets isets)))
    (update-idispatch-index idispatch)
    (setf (gethash idispatch *idispatch-table*) t)
    idispatch))

(defun update-idispatch-index (idispatch)
  (loop for index from 0
        for instruction-sets in (idispatch-isets idispatch)
        when (every #'instruction-set-available-p instruction-sets) do
          (setf (idispatch-index idispatch) index)
          (return-from update-idispatch-index)
        finally
           (warn "None of the idispatch clauses in ~@
                  ~{~S~%~}is available on this machine."
                 (idispatch-clauses idispatch))
           (setf (idispatch-index idispatch)
                 (length (idispatch-isets idispatch)))))

(defun update-idispatch-indices ()
  (loop for idispatch being the hash-keys of *idispatch-table* do
    (update-idispatch-index idispatch)))

(pushnew 'update-idispatch-indices sb-ext:*init-hooks*)

(defmacro instruction-set-case (&body clauses)
  "Execute the first clause whose instruction sets are available at run
time, or signal an error if no clause could be run.

Each clause has to start with an instruction set name, or a list of
instruction set names, followed by a list of statements.

Example:

 (instruction-set-case
   (:sse2 (foo))
   (:avx (bar))
   ((:avx2 :fma) (baz)))
"
  (sb-int:with-unique-names (idispatch)
    (multiple-value-bind (isets bodies)
        (parse-instruction-set-case-clauses clauses)
      `(let ((,idispatch (load-time-value (make-idispatch ',isets ',clauses))))
         (declare (idispatch ,idispatch))
         (case (idispatch-index ,idispatch)
           ,@(loop for iset in isets
                   for body in bodies
                   for index from 0
                   collect `(,index ,@body))
           (,(length clauses)
            (idispatch-no-applicable-clause ,idispatch)))))))

(defun parse-instruction-set-case-clauses (clauses)
  (flet ((clause-iset (clause)
           (unless (consp clause)
             (error "Not a valid instruction-set-case clause: ~S"
                    clause))
           (let ((head (first clause)))
             (if (listp head)
                 (mapcar #'find-instruction-set head)
                 (list (find-instruction-set head)))))
         (clause-body (clause)
           (rest clause)))
    (values
     (mapcar #'clause-iset clauses)
     (mapcar #'clause-body clauses))))

(defun idispatch-no-applicable-clause (idispatch)
  (error "None of the idispatch clauses in ~@
          ~{~S~%~}is available on this machine."
         (idispatch-clauses idispatch)))
