;;;; xref facility

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

(defconstant-eqx +xref-kinds+ #(:binds :calls :sets :references :macroexpands)
  #'equalp)

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
               (loop for (kind what path) in (block-xrefs block)
                     do (record-xref kind what
                                     ;; We use the debug-name of the functional
                                     ;; as an identifier. This works quite nicely,
                                     ;; except for (fast/slow)-methods with non-symbol,
                                     ;; non-number eql specializers, for which
                                     ;; the debug-name doesn't map exactly
                                     ;; to the fdefinition of the method.
                                     functional
                                     nil
                                     path))))
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
                  (and (functional-inline-expanded functional)
                       (symbolp (functional-debug-name functional))))
        (handle-functional functional)))))

(defun record-node-xrefs (node context)
  (declare (type node node))
  (etypecase node
    ((or creturn cif entry mv-combination cast exit enclose))
    (combination
     ;; Record references to globals made using SYMBOL-VALUE.
     (let ((fun (principal-lvar-use (combination-fun node)))
           (arg (car (combination-args node))))
       (when (and (ref-p fun) (eq 'symeval (leaf-%source-name (ref-leaf fun)))
                  (constant-lvar-p arg) (symbolp (lvar-value arg)))
         (record-xref :references (lvar-value arg) context node nil))))
    (ref
     (let ((leaf (ref-leaf node)))
       (typecase leaf
         (global-var
          (let* ((name (leaf-debug-name leaf)))
            (case (global-var-kind leaf)
              ;; Reading a special
              ((:special :global)
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
     (let ((var (set-var node)))
       (when (and (global-var-p var)
                  (memq (global-var-kind var) '(:special :global)))
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
     (or (eq '.anonymous. what)
         #-sb-xref-for-internals
         (let ((pkg (sb-xc:symbol-package what)))
           (or (and pkg (system-package-p pkg))
               (eq pkg *cl-package*)))))
    (t t)))

(defun record-xref (kind what context node path)
  (unless (internal-name-p what)
    (push (cons what
                (source-path-form-number (or path
                                             (node-source-path node))))
          (getf (functional-xref context) kind))))

(defun record-macroexpansion (what block path)
  (unless (internal-name-p what)
    (push (list :macroexpands what path) (block-xrefs block))))

(defun record-call (what block path)
  (unless (internal-name-p what)
    (push (list :calls what path) (block-xrefs block))))


;;;; Packing of xref tables
;;;;
;;;; xref information can be transformed into the following "packed"
;;;; form to save space:
;;;;
;;;;   #(PACKED-ENTRIES NAME1 NAME2 ...)
;;;;
;;;; where NAME1 NAME2 ... are names referred to by the entries
;;;; encoded in PACKED-ENTRIES. PACKED-ENTRIES is a (simple-array
;;;; (unsigned-byte 8) 1) containing variable-width integers (see
;;;; {READ,WRITE}-VAR-INTEGER). The contained sequence of integers is
;;;; of the following form:
;;;;
;;;;   packed-entries        ::= NAME-BITS NUMBER-BITS entries-for-xref-kind+
;;;;   entries-for-xref-kind ::= XREF-KIND-AND-ENTRY-COUNT entry+
;;;;   entry                 ::= NAME-INDEX-AND-FORM-NUMBER
;;;;
;;;; where NAME-BITS and NUMBER-BITS are variable-width integers that
;;;; encode the number of bits used for name indices in
;;;; NAME-INDEX-AND-FORM-NUMBER and the number of bits used for form
;;;; numbers in NAME-INDEX-AND-FORM-NUMBER respectively,
;;;;
;;;; XREF-KIND-AND-ENTRY-COUNT is a variable-width integer cc...kkk
;;;; where c bits encode the number of integers (encoded entries)
;;;; following this integer and k bits encode the xref kind (index
;;;; into +XREF-KINDS+) of the entries following the integer,
;;;;
;;;; NAME-INDEX-AND-FORM-NUMBER is a (name-bits+number-bits)-bit integer
;;;; ii...nn... where i bits encode a name index (see below) and n
;;;; bits encode the form number of the xref entry.
;;;;
;;;; The name index is either an integer i such that
;;;;
;;;;    (< 0 i (length **most-common-xref-names-by-index**))
;;;;
;;;; in which case it refers to the i-th name in that vector or
;;;;
;;;;    (< 0 (+ i (length **m-c-x-n-b-i**)) (1- (length XREF-DATA)))
;;;;
;;;; in which case it is an index (offset by (length **m-c-x-n-b-i**))
;;;; into the name list NAME1 NAME2 ... starting at index 1 of the
;;;; outer vector.
;;;;
;;;; When packing xref information, an initial pass over the entries
;;;; that should be packed has to be made to collect unique names and
;;;; determine the largest form number that will be encoded. Then:
;;;;
;;;;   name-bits   <- (integer-length LARGEST-NAME-INDEX)
;;;;   number-bits <- (integer-length LARGEST-FORM-NUMBER)

;;; Will be overwritten with 64 most frequently cross referenced
;;; names.
(declaim (type vector **most-common-xref-names-by-index**)
         (type hash-table **most-common-xref-names-by-name**))
(defglobal **most-common-xref-names-by-index** #())
(defglobal **most-common-xref-names-by-name** (make-hash-table :test #'equal))

(flet ((encode-kind-and-count (kind count)
         (logior kind (ash (1- count) 3)))
       (decode-kind-and-count (integer)
         (values (ldb (byte 3 0) integer) (1+ (ash integer -3))))
       (index-and-number-encoder (name-bits number-bits)
         (lambda (index number)
           (dpb number (byte number-bits name-bits) index)))
       (index-and-number-decoder (name-bits number-bits)
         (lambda (integer)
           (values (ldb (byte name-bits 0) integer)
                   (ldb (byte number-bits name-bits) integer))))
       (name->index (vector)
         (let ((common-count (length **most-common-xref-names-by-index**)))
           (lambda (name)
             (let ((found t))
               (values (or (gethash name **most-common-xref-names-by-name**)
                           (+ (or (position name vector :start 1 :test #'equal)
                                  (progn
                                    (setf found nil)
                                    (vector-push-extend name vector)
                                    (1- (length vector))))
                              -1 common-count))
                       found)))))
       (index->name (vector)
         (let ((common-count (length **most-common-xref-names-by-index**)))
           (lambda (index)
             (if (< index common-count)
                 (aref **most-common-xref-names-by-index** index)
                 (aref vector (+ index 1 (- common-count))))))))

  ;;; Pack the xref table that was stored for a functional into a more
  ;;; space-efficient form, and return that packed form.
  (defun pack-xref-data (xref-data)
    (unless xref-data (return-from pack-xref-data))
    (let* ((result (make-array 1 :adjustable t :fill-pointer 1
                                 :initial-element 0))
           (ensure-index (name->index result))
           (entries '())
           (max-index 0)
           (max-number 0))
      ;; Collect unique names, assigning indices (implicitly via
      ;; position in RESULT). Determine MAX-INDEX and MAX-NUMBER, the
      ;; largest name index and form number respectively, occurring in
      ;; XREF-DATA.
      (labels ((collect-entries-for-kind (kind records)
                 (let* ((kind-number (position kind +xref-kinds+ :test #'eq))
                        (kind-entries (cons kind-number '())))
                   (push kind-entries entries)
                   (mapc
                    (lambda (record)
                      (destructuring-bind (name . number) record
                        (binding* (((index foundp) (funcall ensure-index name))
                                   (cell
                                    (or (when foundp
                                          (find index (cdr kind-entries) :key #'first))
                                        (let ((cell (list index)))
                                          (push cell (cdr kind-entries))
                                          cell))))
                          (pushnew number (cdr cell) :test #'=)
                          (setf max-index (max max-index index)
                                max-number (max max-number number)))))
                    records))))
        (loop for (kind records) on xref-data by #'cddr
           when records do (collect-entries-for-kind kind records)))
      ;; Encode the number of index and form number bits followed by
      ;; chunks of collected entries for all kinds.
      (let* ((name-bits (integer-length max-index))
             (number-bits (integer-length max-number))
             (encoder (index-and-number-encoder name-bits number-bits))
             (vector (make-array 0 :element-type '(unsigned-byte 8)
                                 :adjustable t :fill-pointer 0 :initial-element 0)))
        (write-var-integer name-bits vector)
        (write-var-integer number-bits vector)
        (loop for (kind-number . kind-entries) in entries
           for kind-count = (reduce #'+ kind-entries
                                    :key (lambda (entry) (length (cdr entry))))
           do (write-var-integer
               (encode-kind-and-count kind-number kind-count) vector)
             (loop for (index . numbers) in kind-entries
                do (dolist (number numbers)
                     (write-var-integer (funcall encoder index number) vector))))
        (setf (aref result 0)
              (coerce vector '(simple-array (unsigned-byte 8) 1))))
      ;; RESULT is adjustable. Make it simple.
      (coerce result 'simple-vector)))

  ;;; Call FUNCTION for each entry in XREF-DATA. FUNCTION's
  ;;; lambda-list has to be compatible to
  ;;;
  ;;;   (kind name form-number)
  ;;;
  ;;; where KIND is the xref kind (see +XREF-KINDS+), NAME is the name
  ;;; of the referenced thing and FORM-NUMBER is the number of the
  ;;; form in which the reference occurred.
  (defun map-packed-xref-data (function xref-data)
    (let* ((function (coerce function 'function))
           (lookup (index->name xref-data))
           (packed (aref xref-data 0))
           (offset 0)
           (decoder (index-and-number-decoder
                     (read-var-integerf packed offset)
                     (read-var-integerf packed offset))))
      (loop while (< offset (length packed))
         do (binding* (((kind-number record-count)
                        (decode-kind-and-count (read-var-integerf packed offset)))
                       (kind (elt +xref-kinds+ kind-number)))
              (loop repeat record-count
                 do (binding* (((index number)
                                (funcall decoder (read-var-integerf packed offset)))
                               (name (funcall lookup index)))
                      (funcall function kind name number))))))))
