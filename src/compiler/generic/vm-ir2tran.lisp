;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

(def-alloc %make-structure-instance 1 :structure-alloc
           sb!vm:instance-header-widetag sb!vm:instance-pointer-lowtag
           nil)

#!+stack-allocatable-fixed-objects
(defoptimizer (%make-structure-instance stack-allocate-result)
    ((defstruct-description &rest args) node dx)
  (declare (ignore args dx))
  (aver (constant-lvar-p defstruct-description))
  ;; A structure instance can be stack-allocated if it has no raw
  ;; slots, or if we're on a target with a conservatively-scavenged
  ;; stack.  We have no reader conditional for stack conservation, but
  ;; it turns out that the only time stack conservation is in play is
  ;; when we're on GENCGC (since CHENEYGC doesn't have conservation)
  ;; and C-STACK-IS-CONTROL-STACK (otherwise, the C stack is the
  ;; number stack, and we precisely-scavenge the control stack).
  #!-(and :gencgc :c-stack-is-control-stack)
  (every (lambda (x) (eq (dsd-raw-type x) t))
         (dd-slots (lvar-value defstruct-description)))
  #!+(and :gencgc :c-stack-is-control-stack)
  t)

(defoptimizer ir2-convert-reffer ((object) node block name offset lowtag)
  (let* ((lvar (node-lvar node))
         (locs (lvar-result-tns lvar
                                (list *backend-t-primitive-type*)))
         (res (first locs)))
    (vop slot node block (lvar-tn node block object)
         name offset lowtag res)
    (move-lvar-result node block locs lvar)))

(defoptimizer ir2-convert-setter ((object value) node block name offset lowtag)
  (let ((value-tn (lvar-tn node block value)))
    (vop set-slot node block (lvar-tn node block object) value-tn
         name offset lowtag)
    (move-lvar-result node block (list value-tn) (node-lvar node))))

;;; FIXME: Isn't there a name for this which looks less like a typo?
;;; (The name IR2-CONVERT-SETTER is used for something else, just above.)
(defoptimizer ir2-convert-setfer ((value object) node block name offset lowtag)
  (let ((value-tn (lvar-tn node block value)))
    (vop set-slot node block (lvar-tn node block object) value-tn
         name offset lowtag)
    (move-lvar-result node block (list value-tn) (node-lvar node))))

#!+compare-and-swap-vops
(defoptimizer ir2-convert-casser
    ((object old new) node block name offset lowtag)
  (let* ((lvar (node-lvar node))
         (locs (lvar-result-tns lvar (list *backend-t-primitive-type*)))
         (res (first locs)))
    (vop compare-and-swap-slot node block
         (lvar-tn node block object)
         (lvar-tn node block old)
         (lvar-tn node block new)
         name offset lowtag
         res)
    (move-lvar-result node block locs lvar)))

(defun emit-inits (node block name object lowtag instance-length inits args)
  #!+interleaved-raw-slots (declare (ignore instance-length))
  #!-raw-instance-init-vops
  (declare (ignore instance-length))
  (let ((unbound-marker-tn nil)
        (funcallable-instance-tramp-tn nil))
    (dolist (init inits)
      (let ((kind (car init))
            (slot (cdr init)))
        (case kind
          (:slot
           ;; FIXME: with #!+interleaved-raw-slots the only reason INIT-SLOT
           ;; and its raw variants exist is to avoid an extra MOVE -
           ;; setters are expected to return something, but INITers don't.
           ;; It would probably produce better code by not assuming that
           ;; setters return a value, because as things are, if you call
           ;; 8 setters in a row, then you probably produce 7 extraneous moves,
           ;; because not all of them can deliver a value to the final result.
           (let* ((raw-type (pop slot))
                  (arg (pop args))
                  (arg-tn (lvar-tn node block arg)))
             (macrolet
                 ((make-case (&optional rsd-list)
                    `(ecase raw-type
                       ((t)
                        (vop init-slot node block object arg-tn
                             name (+ sb!vm:instance-slots-offset slot) lowtag))
                       ,@(map 'list
                          (lambda (rsd)
                            `(,(sb!kernel::raw-slot-data-raw-type rsd)
                              (vop ,(sb!kernel::raw-slot-data-init-vop rsd)
                                   node block object arg-tn
                                   #!-interleaved-raw-slots instance-length
                                   slot)))
                          (symbol-value rsd-list)))))
               (make-case #!+raw-instance-init-vops
                          sb!kernel::*raw-slot-data*))))
          (:dd
           (vop init-slot node block object
                (emit-constant (sb!kernel::dd-layout-or-lose slot))
                name sb!vm:instance-slots-offset lowtag))
          (otherwise
           (vop init-slot node block object
                (ecase kind
                  (:arg
                   (aver args)
                   (lvar-tn node block (pop args)))
                  (:unbound
                   (or unbound-marker-tn
                       (setf unbound-marker-tn
                             (let ((tn (make-restricted-tn
                                        nil
                                        (sc-number-or-lose 'sb!vm::any-reg))))
                               (vop make-unbound-marker node block tn)
                               tn))))
                  (:null
                   (emit-constant nil))
                  (:funcallable-instance-tramp
                   (or funcallable-instance-tramp-tn
                       (setf funcallable-instance-tramp-tn
                             (let ((tn (make-restricted-tn
                                        nil
                                        (sc-number-or-lose 'sb!vm::any-reg))))
                               (vop make-funcallable-instance-tramp node block tn)
                               tn)))))
                name slot lowtag))))))
  (unless (null args)
    (bug "Leftover args: ~S" args)))

(defun emit-fixed-alloc (node block name words type lowtag result lvar)
  (let ((stack-allocate-p (and lvar (lvar-dynamic-extent lvar))))
    (when stack-allocate-p
      (vop current-stack-pointer node block
           (ir2-lvar-stack-pointer (lvar-info lvar))))
    (vop fixed-alloc node block name words type lowtag stack-allocate-p result)))

(defoptimizer ir2-convert-fixed-allocation
              ((&rest args) node block name words type lowtag inits)
  (let* ((lvar (node-lvar node))
         (locs (lvar-result-tns lvar (list *backend-t-primitive-type*)))
         (result (first locs)))
    (emit-fixed-alloc node block name words type lowtag result lvar)
    (emit-inits node block name result lowtag words inits args)
    (move-lvar-result node block locs lvar)))

(defoptimizer ir2-convert-variable-allocation
              ((extra &rest args) node block name words type lowtag inits)
  (let* ((lvar (node-lvar node))
         (locs (lvar-result-tns lvar (list *backend-t-primitive-type*)))
         (result (first locs)))
    (if (constant-lvar-p extra)
        (let ((words (+ (lvar-value extra) words)))
          (emit-fixed-alloc node block name words type lowtag result lvar))
        (vop var-alloc node block (lvar-tn node block extra) name words
             type lowtag result))
    (emit-inits node block name result lowtag nil inits args)
    (move-lvar-result node block locs lvar)))

(defoptimizer ir2-convert-structure-allocation
    ((dd slot-specs &rest args) node block name words type lowtag inits)
  (declare (ignore inits))
  (let* ((lvar (node-lvar node))
         (locs (lvar-result-tns lvar (list *backend-t-primitive-type*)))
         (result (first locs)))
    (aver (constant-lvar-p dd))
    (aver (constant-lvar-p slot-specs))
    (let* ((c-dd (lvar-value dd))
           (c-slot-specs (lvar-value slot-specs))
           (words (+ (sb!kernel::dd-instance-length c-dd) words)))
      (emit-fixed-alloc node block name words type lowtag result lvar)
      (emit-inits node block name result lowtag words `((:dd . ,c-dd) ,@c-slot-specs) args)
      (move-lvar-result node block locs lvar))))

(defoptimizer (initialize-vector ir2-convert)
    ((vector &rest initial-contents) node block)
  (let* ((vector-ctype (lvar-type vector))
         (elt-ctype (if (array-type-p vector-ctype)
                        (array-type-specialized-element-type vector-ctype)
                        (bug "Unknow vector type in IR2 conversion for ~S."
                             'initialize-vector)))
         (saetp (find-saetp-by-ctype elt-ctype))
         (lvar (node-lvar node))
         (locs (lvar-result-tns lvar (list (primitive-type vector-ctype))))
         (result (first locs))
         (elt-ptype (primitive-type elt-ctype))
         (tmp (make-normal-tn elt-ptype)))
    (emit-move node block (lvar-tn node block vector) result)
    (flet ((compute-setter ()
             (macrolet
                 ((frob ()
                    (let ((*package* (find-package :sb!vm))
                          (clauses nil))
                      (map nil (lambda (s)
                                 (when (sb!vm:saetp-specifier s)
                                   (push
                                    `(,(sb!vm:saetp-typecode s)
                                       (lambda (index tn)
                                         #!+x86-64
                                         (vop ,(symbolicate "DATA-VECTOR-SET-WITH-OFFSET/"
                                                            (sb!vm:saetp-primitive-type-name s)
                                                            "-C")
                                              node block result tn index 0 tn)
                                         #!+x86
                                         (vop ,(symbolicate "DATA-VECTOR-SET-WITH-OFFSET/"
                                                            (sb!vm:saetp-primitive-type-name s))
                                              node block result index tn 0 tn)
                                         #!-(or x86 x86-64)
                                         (vop ,(symbolicate "DATA-VECTOR-SET/"
                                                            (sb!vm:saetp-primitive-type-name s))
                                              node block result index tn tn)))
                                    clauses)))
                           sb!vm:*specialized-array-element-type-properties*)
                      `(ecase (sb!vm:saetp-typecode saetp)
                         ,@(nreverse clauses)))))
               (frob)))
           (tnify (index)
             #!-x86-64
             (emit-constant index)
             #!+x86-64
             index))
      (let ((setter (compute-setter))
            (length (length initial-contents)))
        (dotimes (i length)
          (emit-move node block (lvar-tn node block (pop initial-contents)) tmp)
          (funcall setter (tnify i) tmp))))
    (move-lvar-result node block locs lvar)))

;;; :SET-TRANS (in objdef.lisp !DEFINE-PRIMITIVE-OBJECT) doesn't quite
;;; cut it for symbols, where under certain compilation options
;;; (e.g. #!+SB-THREAD) we have to do something complicated, rather
;;; than simply set the slot.  So we build the IR2 converting function
;;; by hand.  -- CSR, 2003-05-08
(let ((fun-info (fun-info-or-lose '%set-symbol-value)))
  (setf (fun-info-ir2-convert fun-info)
        (lambda (node block)
          (let ((args (basic-combination-args node)))
            (destructuring-bind (symbol value) args
              (let ((value-tn (lvar-tn node block value)))
                (vop set node block
                     (lvar-tn node block symbol) value-tn)
                (move-lvar-result
                 node block (list value-tn) (node-lvar node))))))))

;;; Stack allocation optimizers per platform support
#!+stack-allocatable-vectors
(progn
  (defoptimizer (allocate-vector stack-allocate-result)
      ((type length words) node dx)
    (declare (ignorable type) (ignore length))
    (and
     ;; Can't put unboxed data on the stack unless we scavenge it
     ;; conservatively.
     #!-c-stack-is-control-stack
     (constant-lvar-p type)
     #!-c-stack-is-control-stack
     (member (lvar-value type)
             '#.(list (sb!vm:saetp-typecode (find-saetp 't))
                      (sb!vm:saetp-typecode (find-saetp 'fixnum))))
     (or (eq dx :always-dynamic)
         (zerop (policy node safety))
         ;; a vector object should fit in one page -- otherwise it might go past
         ;; stack guard pages.
         (values-subtypep (lvar-derived-type words)
                          (load-time-value
                           (specifier-type `(integer 0 ,(- (/ sb!vm::*backend-page-bytes*
                                                              sb!vm:n-word-bytes)
                                                           sb!vm:vector-data-offset))))))))

  (defoptimizer (allocate-vector ltn-annotate) ((type length words) call ltn-policy)
    (declare (ignore type length words))
    (vectorish-ltn-annotate-helper call ltn-policy
                                   'sb!vm::allocate-vector-on-stack
                                   'sb!vm::allocate-vector-on-heap))

  (defun vectorish-ltn-annotate-helper (call ltn-policy dx-template &optional not-dx-template)
    (let* ((args (basic-combination-args call))
           (template-name (if (awhen (node-lvar call)
                                (lvar-dynamic-extent it))
                              dx-template
                              not-dx-template))
           (template (and template-name
                          (template-or-lose template-name))))
      (dolist (arg args)
        (setf (lvar-info arg)
              (make-ir2-lvar (primitive-type (lvar-type arg)))))
      (unless (and template
                   (is-ok-template-use template call (ltn-policy-safe-p ltn-policy)))
        (ltn-default-call call)
        (return-from vectorish-ltn-annotate-helper (values)))
      (setf (basic-combination-info call) template)
      (setf (node-tail-p call) nil)

      (dolist (arg args)
        (annotate-1-value-lvar arg)))))

;;; ...lists
#!+stack-allocatable-lists
(progn
  (defoptimizer (list stack-allocate-result) ((&rest args) node dx)
    (declare (ignore dx))
    (not (null args)))
  (defoptimizer (list* stack-allocate-result) ((&rest args) node dx)
    (declare (ignore dx))
    (not (null (rest args))))
  (defoptimizer (%listify-rest-args stack-allocate-result) ((&rest args) node dx)
    (declare (ignore args dx))
    t))

;;; ...conses
#!+stack-allocatable-fixed-objects
(progn
  (defoptimizer (cons stack-allocate-result) ((&rest args) node dx)
    (declare (ignore args dx))
    t)
  (defoptimizer (%make-complex stack-allocate-result) ((&rest args) node dx)
    (declare (ignore args dx))
    t))

;;; MAKE-LIST optimizations
#!+x86-64
(progn
  (defoptimizer (%make-list stack-allocate-result) ((length element) node dx)
    (declare (ignore element))
    (or (eq dx :always-dynamic)
        (zerop (policy node safety))
        ;; At most one page (this is more paranoid than %listify-rest-args).
        ;; Really what you want to do is decrement the stack pointer by one page
        ;; at a time, filling in CDR pointers downward. Then this restriction
        ;; could be removed, because allocation would never miss the guard page
        ;; if it tries to consume too much stack space.
        (values-subtypep (lvar-derived-type length)
                         (load-time-value
                          (specifier-type `(integer 0 ,(/ sb!vm::*backend-page-bytes*
                                                          sb!vm:n-word-bytes 2)))))))
  (defoptimizer (%make-list ltn-annotate) ((length element) call ltn-policy)
    (declare (ignore length element))
    (vectorish-ltn-annotate-helper call ltn-policy
                                   'sb!vm::allocate-list-on-stack)))

