;;;; xref facility

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

(defvar *xref-kinds* '(:binds :calls :sets :references :macroexpands))

(defun record-component-xrefs (component)
  (declare (type component component))
  (when (policy *lexenv* (zerop store-xref-data))
    (return-from record-component-xrefs))
  (do ((block (block-next (component-head component)) (block-next block)))
      ((null (block-next block)))
    (let ((start (block-start block)))
      (flet ((handle-node (functional)
               ;; Record xref information for all nodes in the block.
               ;; Note that this code can get executed several times
               ;; for the same block, if the functional is referenced
               ;; from multiple XEPs.
               (loop for ctran = start then (node-next (ctran-next ctran))
                     while ctran
                     do (record-node-xrefs (ctran-next ctran) functional))
               ;; Properly record the deferred macroexpansion and source
               ;; transform information that's been stored in the block.
               (dolist (xref-data (block-xrefs block))
                 (destructuring-bind (kind what path) xref-data
                   (record-xref kind what
                                ;; We use the debug-name of the functional
                                ;; as an identifier. This works quite nicely,
                                ;; except for (fast/slow)-methods with non-symbol,
                                ;; non-number eql specializers, for which
                                ;; the debug-name doesn't map exactly
                                ;; to the fdefinition of the method.
                                functional
                                nil
                                path)))))
        (call-with-block-external-functionals block #'handle-node)))))

(defun call-with-block-external-functionals (block fun)
  (let* ((functional (block-home-lambda block))
         (seen nil))
    (labels ((local-function-name-p (name)
               (and (consp name)
                    (member (car name)
                            '(flet labels lambda))))
             (handle-functional (functional)
               ;; If a functional looks like a global function (has a
               ;; XEP, isn't a local function or a lambda) record xref
               ;; information for it. Otherwise recurse on the
               ;; home-lambdas of all references to the functional.
               (when (eq (functional-kind functional) :external)
                 (let ((entry (functional-entry-fun functional)))
                   (when entry
                     (let ((name (functional-debug-name entry)))
                       (unless (local-function-name-p name)
                         (return-from handle-functional
                           (funcall fun entry)))))))
               ;; Recurse only if we haven't already seen the
               ;; functional.
               (unless (member functional seen)
                 (push functional seen)
                 (dolist (ref (functional-refs functional))
                   (handle-functional (node-home-lambda ref))))))
      (unless (or (eq :deleted (functional-kind functional))
                  ;; If the block came from an inlined global
                  ;; function, ignore it.
                  (and (functional-inlinep functional)
                       (symbolp (functional-debug-name functional))))
        (handle-functional functional)))))

(defun record-node-xrefs (node context)
  (declare (type node node))
  (etypecase node
    ((or creturn cif entry mv-combination cast exit))
    (combination
     ;; Record references to globals made using SYMBOL-VALUE.
     (let ((fun (principal-lvar-use (combination-fun node)))
           (arg (car (combination-args node))))
       (when (and (ref-p fun) (eq 'symbol-value (leaf-%source-name (ref-leaf fun)))
                  (constant-lvar-p arg) (symbolp (lvar-value arg)))
         (record-xref :references (lvar-value arg) context node nil))))
    (ref
     (let ((leaf (ref-leaf node)))
       (typecase leaf
         (global-var
          (let* ((name (leaf-debug-name leaf)))
            (case (global-var-kind leaf)
              ;; Reading a special
              (:special
               (record-xref :references name context node nil))
              ;; Calling a function
              (:global-function
               (record-xref :calls name context node nil)))))
         ;; Inlined global function
         (clambda
          (let ((inline-var (functional-inline-expanded leaf)))
            (when (global-var-p inline-var)
              ;; TODO: a WHO-INLINES xref-kind could be useful
              (record-xref :calls (leaf-debug-name inline-var) context node nil))))
         ;; Reading a constant
         (constant
          (record-xref :references (ref-%source-name node) context node nil)))))
    ;; Setting a special variable
    (cset
     (let* ((var (set-var node)))
       (when (and (global-var-p var)
                  (eq :special (global-var-kind var)))
         (record-xref :sets
                      (leaf-debug-name var)
                      context
                      node
                      nil))))
    ;; Binding a special variable
    (bind
     (let ((vars (lambda-vars (bind-lambda node))))
       (dolist (var vars)
         (when (lambda-var-specvar var)
           (record-xref :binds
                        (lambda-var-%source-name var)
                        context
                        node
                        nil)))))))

(defun internal-name-p (what)
  ;; Unless we're building with SB-XREF-FOR-INTERNALS, don't store
  ;; XREF information for internals. We define anything with a symbol
  ;; from either an implementation package or from COMMON-LISP as
  ;; internal
  (typecase what
    (list
     (every #'internal-name-p what))
    (symbol
     #!+sb-xref-for-internals
     (eq '.anonymous. what)
     #!-sb-xref-for-internals
     (or (eq '.anonymous. what)
         (member (symbol-package what)
                 (load-time-value
                  (list* (find-package "COMMON-LISP")
                         #+sb-xc-host (find-package "SB-XC")
                         (remove-if-not
                          (lambda (package)
                            (= (mismatch "SB!"
                                         (package-name package))
                               3))
                          (list-all-packages)))))
         #+sb-xc-host   ; again, special case like in genesis and dump
         (multiple-value-bind (cl-symbol cl-status)
             (find-symbol (symbol-name what) sb!int:*cl-package*)
           (and (eq what cl-symbol) (eq cl-status :external)))))
    (t t)))

(defun record-xref (kind what context node path)
  (unless (internal-name-p what)
    (let ((path (reverse
                 (source-path-original-source
                  (or path
                      (node-source-path node))))))
      (push (list what path)
            (getf (functional-xref context) kind)))))

(defun record-macroexpansion (what block path)
  (unless (internal-name-p what)
    (push (list :macroexpands what path) (block-xrefs block))))

(defun record-call (what block path)
  (unless (internal-name-p what)
    (push (list :calls what path) (block-xrefs block))))

;;; Pack the xref table that was stored for a functional into a more
;;; space-efficient form, and return that packed form.
(defun pack-xref-data (xref-data)
  (when xref-data
    (let ((array (make-array (length *xref-kinds*))))
      (loop for key in *xref-kinds*
            for i from 0
            for values = (remove-duplicates (getf xref-data key)
                                            :test #'equal)
            for flattened = (reduce #'append values :from-end t)
            collect (setf (aref array i)
                          (when flattened
                            (make-array (length flattened)
                                        :initial-contents flattened))))
      array)))
