;;;; This file contains utilities used for creating and manipulating
;;;; TNs, and some other more assorted IR2 utilities.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;; The component that is currently being compiled. TNs are allocated
;;; in this component.
(defvar *component-being-compiled*)

;;; DO-PACKED-TNS (TN-Var Component [Result]) Declaration* Form*
;;;
;;; Iterate over all packed TNs allocated in COMPONENT.
(defmacro do-packed-tns ((tn component &optional result) &body body)
  (let ((n-component (gensym)))
    `(let ((,n-component (component-info ,component)))
       (do ((,tn (ir2-component-normal-tns ,n-component) (tn-next ,tn)))
           ((null ,tn))
         ,@body)
       (do ((,tn (ir2-component-restricted-tns ,n-component) (tn-next ,tn)))
           ((null ,tn))
         ,@body)
       (do ((,tn (ir2-component-wired-tns ,n-component) (tn-next ,tn)))
           ((null ,tn)
            ,result)
         ,@body))))

(defun set-ir2-physenv-live-tns (value instance)
  (setf (ir2-physenv-live-tns instance) value))

(defun set-ir2-physenv-debug-live-tns (value instance)
  (setf (ir2-physenv-debug-live-tns instance) value))

(defun set-ir2-component-alias-tns (value instance)
  (setf (ir2-component-alias-tns instance) value))

(defun set-ir2-component-normal-tns (value instance)
  (setf (ir2-component-normal-tns instance) value))

(defun set-ir2-component-restricted-tns (value instance)
  (setf (ir2-component-restricted-tns instance) value))

(defun set-ir2-component-wired-tns (value instance)
  (setf (ir2-component-wired-tns instance) value))

;;; Remove all TNs with no references from the lists of unpacked TNs.
;;; We null out the OFFSET so that nobody will mistake deleted wired
;;; TNs for properly packed TNs. We mark non-deleted alias TNs so that
;;; aliased TNs aren't considered to be unreferenced.
(defun delete-unreferenced-tns (component)
  (let* ((2comp (component-info component))
         (aliases (make-array (1+ (ir2-component-global-tn-counter 2comp))
                              :element-type 'bit :initial-element 0)))
    (labels ((delete-some (getter setter)
               (let ((prev nil))
                 (do ((tn (funcall getter 2comp) (tn-next tn)))
                     ((null tn))
                   (cond
                    ((or (used-p tn)
                         (and (eq (tn-kind tn) :specified-save)
                              (used-p (tn-save-tn tn))))
                     (setq prev tn))
                    (t
                     (delete-1 tn prev setter))))))
             (used-p (tn)
               (or (tn-reads tn) (tn-writes tn)
                   (member (tn-kind tn) '(:component :environment))
                   (not (zerop (sbit aliases (tn-number tn))))))
             (delete-1 (tn prev setter)
               (if prev
                   (setf (tn-next prev) (tn-next tn))
                   (funcall setter (tn-next tn) 2comp))
               (setf (tn-offset tn) nil)
               (case (tn-kind tn)
                 (:environment
                  (clear-live tn
                              #'ir2-physenv-live-tns
                              #'set-ir2-physenv-live-tns))
                 (:debug-environment
                  (clear-live tn
                              #'ir2-physenv-debug-live-tns
                              #'set-ir2-physenv-debug-live-tns))))
             (clear-live (tn getter setter)
               (let ((env (physenv-info (tn-physenv tn))))
                 (funcall setter (delete tn (funcall getter env)) env))))
      (declare (inline used-p delete-some delete-1 clear-live))
      (delete-some #'ir2-component-alias-tns
                   #'set-ir2-component-alias-tns)
      (do ((tn (ir2-component-alias-tns 2comp) (tn-next tn)))
          ((null tn))
        (setf (sbit aliases (tn-number (tn-save-tn tn))) 1))
      (delete-some #'ir2-component-normal-tns
                   #'set-ir2-component-normal-tns)
      (delete-some #'ir2-component-restricted-tns
                   #'set-ir2-component-restricted-tns)
      (delete-some #'ir2-component-wired-tns
                   #'set-ir2-component-wired-tns)))
  (values))

;;;; TN creation

;;; Create a packed TN of the specified primitive-type in the
;;; *COMPONENT-BEING-COMPILED*. We use the SCs from the primitive type
;;; to determine which SCs it can be packed in.
(defun make-normal-tn (type)
  (declare (type primitive-type type))
  (let* ((component (component-info *component-being-compiled*))
         (res (make-tn (incf (ir2-component-global-tn-counter component))
                       :normal type nil)))
    (push-in tn-next res (ir2-component-normal-tns component))
    res))

;;; Create a normal packed TN with representation indicated by SCN.
(defun make-representation-tn (ptype scn)
  (declare (type primitive-type ptype) (type sc-number scn))
  (let* ((component (component-info *component-being-compiled*))
         (res (make-tn (incf (ir2-component-global-tn-counter component))
                       :normal ptype
                       (svref *backend-sc-numbers* scn))))
    (push-in tn-next res (ir2-component-normal-tns component))
    res))

;;; Create a TN wired to a particular location in an SC. We set the Offset
;;; and FSC to record where it goes, and then put it on the current component's
;;; Wired-TNs list. Ptype is the TN's primitive-type, which may be NIL in VOP
;;; temporaries.
(defun make-wired-tn (ptype scn offset)
  (declare (type (or primitive-type null) ptype)
           (type sc-number scn) (type unsigned-byte offset))
  (let* ((component (component-info *component-being-compiled*))
         (res (make-tn (incf (ir2-component-global-tn-counter component))
                       :normal ptype
                       (svref *backend-sc-numbers* scn))))
    (setf (tn-offset res) offset)
    (push-in tn-next res (ir2-component-wired-tns component))
    res))

;;; Create a packed TN restricted to the SC with number SCN. Ptype is as
;;; for MAKE-WIRED-TN.
(defun make-restricted-tn (ptype scn)
  (declare (type (or primitive-type null) ptype) (type sc-number scn))
  (let* ((component (component-info *component-being-compiled*))
         (res (make-tn (incf (ir2-component-global-tn-counter component))
                       :normal ptype
                       (svref *backend-sc-numbers* scn))))
    (push-in tn-next res (ir2-component-restricted-tns component))
    res))

;;; Make TN be live throughout PHYSENV. Return TN. In the DEBUG case,
;;; the TN is treated normally in blocks in the environment which
;;; reference the TN, allowing targeting to/from the TN. This results
;;; in move efficient code, but may result in the TN sometimes not
;;; being live when you want it.
(defun physenv-live-tn (tn physenv)
  (declare (type tn tn) (type physenv physenv))
  (aver (eq (tn-kind tn) :normal))
  (setf (tn-kind tn) :environment)
  (setf (tn-physenv tn) physenv)
  (push tn (ir2-physenv-live-tns (physenv-info physenv)))
  tn)
(defun physenv-debug-live-tn (tn physenv)
  (declare (type tn tn) (type physenv physenv))
  (aver (eq (tn-kind tn) :normal))
  (setf (tn-kind tn) :debug-environment)
  (setf (tn-physenv tn) physenv)
  (push tn (ir2-physenv-debug-live-tns (physenv-info physenv)))
  tn)

;;; Make TN be live throughout the current component. Return TN.
(defun component-live-tn (tn)
  (declare (type tn tn))
  (aver (eq (tn-kind tn) :normal))
  (setf (tn-kind tn) :component)
  (push tn (ir2-component-component-tns (component-info
                                         *component-being-compiled*)))
  tn)

;;; Specify that SAVE be used as the save location for TN. TN is returned.
(defun specify-save-tn (tn save)
  (declare (type tn tn save))
  (aver (eq (tn-kind save) :normal))
  (aver (and (not (tn-save-tn tn)) (not (tn-save-tn save))))
  (setf (tn-kind save) :specified-save)
  (setf (tn-save-tn tn) save)
  (setf (tn-save-tn save) tn)
  (push save
        (ir2-component-specified-save-tns
         (component-info *component-being-compiled*)))
  tn)

;;; Create a constant TN. The implementation dependent
;;; IMMEDIATE-CONSTANT-SC function is used to determine whether the
;;; constant has an immediate representation.
(defun make-constant-tn (constant)
  (declare (type constant constant))
  (let* ((component (component-info *component-being-compiled*))
         (immed (immediate-constant-sc (constant-value constant)))
         (sc (svref *backend-sc-numbers*
                    (or immed (sc-number-or-lose 'constant))))
         (res (make-tn 0 :constant (primitive-type (leaf-type constant)) sc)))
    (unless immed
      (let ((constants (ir2-component-constants component)))
        (setf (tn-offset res) (fill-pointer constants))
        (vector-push-extend constant constants)))
    (push-in tn-next res (ir2-component-constant-tns component))
    (setf (tn-leaf res) constant)
    res))

(defun make-load-time-value-tn (handle type)
  (let* ((component (component-info *component-being-compiled*))
         (sc (svref *backend-sc-numbers*
                    (sc-number-or-lose 'constant)))
         (res (make-tn 0 :constant (primitive-type type) sc))
         (constants (ir2-component-constants component)))
    (setf (tn-offset res) (fill-pointer constants))
    (vector-push-extend (cons :load-time-value handle) constants)
    (push-in tn-next res (ir2-component-constant-tns component))
    res))

;;; Make a TN that aliases TN for use in local call argument passing.
(defun make-alias-tn (tn)
  (declare (type tn tn))
  (let* ((component (component-info *component-being-compiled*))
         (res (make-tn (incf (ir2-component-global-tn-counter component))
                       :alias (tn-primitive-type tn) nil)))
    (setf (tn-save-tn res) tn)
    (push-in tn-next res
             (ir2-component-alias-tns component))
    res))

;;; Return a load-time constant TN with the specified KIND and INFO.
;;; If the desired CONSTANTS entry already exists, then reuse it,
;;; otherwise allocate a new load-time constant slot.
(defun make-load-time-constant-tn (kind info)
  (declare (type keyword kind))
  (let* ((component (component-info *component-being-compiled*))
         (res (make-tn 0
                       :constant
                       *backend-t-primitive-type*
                       (svref *backend-sc-numbers*
                              (sc-number-or-lose 'constant))))
         (constants (ir2-component-constants component)))

    (do ((i 0 (1+ i)))
        ((= i (length constants))
         (setf (tn-offset res) i)
         (vector-push-extend (cons kind info) constants))
      (let ((entry (aref constants i)))
        (when (and (consp entry)
                   (eq (car entry) kind)
                   (or (eq (cdr entry) info)
                       (and (consp info)
                            (equal (cdr entry) info))))
          (setf (tn-offset res) i)
          (return))))

    (push-in tn-next res (ir2-component-constant-tns component))
    res))

;;;; TN referencing

;;; Make a TN-REF that references TN and return it. WRITE-P should be
;;; true if this is a write reference, otherwise false. All we do
;;; other than calling the constructor is add the reference to the
;;; TN's references.
(defun reference-tn (tn write-p)
  (declare (type tn tn) (type boolean write-p))
  (let ((res (make-tn-ref tn write-p)))
    (if write-p
        (push-in tn-ref-next res (tn-writes tn))
        (push-in tn-ref-next res (tn-reads tn)))
    res))

;;; Make TN-REFS to reference each TN in TNs, linked together by
;;; TN-REF-ACROSS. WRITE-P is the WRITE-P value for the refs. MORE is
;;; stuck in the TN-REF-ACROSS of the ref for the last TN, or returned
;;; as the result if there are no TNs.
(defun reference-tn-list (tns write-p &optional more)
  (declare (list tns) (type boolean write-p) (type (or tn-ref null) more))
  (if tns
      (let* ((first (reference-tn (first tns) write-p))
             (prev first))
        (dolist (tn (rest tns))
          (let ((res (reference-tn tn write-p)))
            (setf (tn-ref-across prev) res)
            (setq prev res)))
        (setf (tn-ref-across prev) more)
        first)
      more))

;;; Remove Ref from the references for its associated TN.
(defun delete-tn-ref (ref)
  (declare (type tn-ref ref))
  (if (tn-ref-write-p ref)
      (deletef-in tn-ref-next (tn-writes (tn-ref-tn ref)) ref)
      (deletef-in tn-ref-next (tn-reads (tn-ref-tn ref)) ref))
  (values))

;;; Do stuff to change the TN referenced by Ref. We remove Ref from its
;;; old TN's refs, add ref to TN's refs, and set the TN-REF-TN.
(defun change-tn-ref-tn (ref tn)
  (declare (type tn-ref ref) (type tn tn))
  (delete-tn-ref ref)
  (setf (tn-ref-tn ref) tn)
  (if (tn-ref-write-p ref)
      (push-in tn-ref-next ref (tn-writes tn))
      (push-in tn-ref-next ref (tn-reads tn)))
  (values))

;;;; miscellaneous utilities

;;; Emit a move-like template determined at run-time, with X as the
;;; argument and Y as the result. Useful for move, coerce and
;;; type-check templates. If supplied, then insert before VOP,
;;; otherwise insert at then end of the block. Returns the last VOP
;;; inserted.
(defun emit-move-template (node block template x y &optional before)
  (declare (type node node) (type ir2-block block)
           (type template template) (type tn x y))
  (let ((arg (reference-tn x nil))
        (result (reference-tn y t)))
    (multiple-value-bind (first last)
        (funcall (template-emit-function template) node block template arg
                 result)
      (insert-vop-sequence first last block before)
      last)))

;;; like EMIT-MOVE-TEMPLATE, except that we pass in INFO args too
(defun emit-load-template (node block template x y info &optional before)
  (declare (type node node) (type ir2-block block)
           (type template template) (type tn x y))
  (let ((arg (reference-tn x nil))
        (result (reference-tn y t)))
    (multiple-value-bind (first last)
        (funcall (template-emit-function template) node block template arg
                 result info)
      (insert-vop-sequence first last block before)
      last)))

;;; like EMIT-MOVE-TEMPLATE, except that the VOP takes two args
(defun emit-move-arg-template (node block template x f y &optional before)
  (declare (type node node) (type ir2-block block)
           (type template template) (type tn x f y))
  (let ((x-ref (reference-tn x nil))
        (f-ref (reference-tn f nil))
        (y-ref (reference-tn y t)))
    (setf (tn-ref-across x-ref) f-ref)
    (multiple-value-bind (first last)
        (funcall (template-emit-function template) node block template x-ref
                 y-ref)
      (insert-vop-sequence first last block before)
      last)))

;;; like EMIT-MOVE-TEMPLATE, except that the VOP takes no args
(defun emit-context-template (node block template y &optional before)
  (declare (type node node) (type ir2-block block)
           (type template template) (type tn y))
  (let ((y-ref (reference-tn y t)))
    (multiple-value-bind (first last)
        (funcall (template-emit-function template) node block template nil
                 y-ref)
      (insert-vop-sequence first last block before)
      last)))

;;; Return the label marking the start of Block, assigning one if necessary.
(defun block-label (block)
  (declare (type cblock block))
  (let ((2block (block-info block)))
    (or (ir2-block-%label 2block)
        (setf (ir2-block-%label 2block) (gen-label)))))

;;; Return true if Block is emitted immediately after the block ended by Node.
(defun drop-thru-p (node block)
  (declare (type node node) (type cblock block))
  (let ((next-block (ir2-block-next (block-info (node-block node)))))
    (aver (eq node (block-last (node-block node))))
    (eq next-block (block-info block))))

;;; Link a list of VOPs from First to Last into Block, Before the specified
;;; VOP. If Before is NIL, insert at the end.
(defun insert-vop-sequence (first last block before)
  (declare (type vop first last) (type ir2-block block)
           (type (or vop null) before))
  (if before
      (let ((prev (vop-prev before)))
        (setf (vop-prev first) prev)
        (if prev
            (setf (vop-next prev) first)
            (setf (ir2-block-start-vop block) first))
        (setf (vop-next last) before)
        (setf (vop-prev before) last))
      (let ((current (ir2-block-last-vop block)))
        (setf (vop-prev first) current)
        (setf (ir2-block-last-vop block) last)
        (if current
            (setf (vop-next current) first)
            (setf (ir2-block-start-vop block) first))))
  (values))

;;; Delete all of the TN-REFs associated with VOP and remove VOP from the IR2.
(defun delete-vop (vop)
  (declare (type vop vop))
  (do ((ref (vop-refs vop) (tn-ref-next-ref ref)))
      ((null ref))
    (delete-tn-ref ref))

  (let ((prev (vop-prev vop))
        (next (vop-next vop))
        (block (vop-block vop)))
    (if prev
        (setf (vop-next prev) next)
        (setf (ir2-block-start-vop block) next))
    (if next
        (setf (vop-prev next) prev)
        (setf (ir2-block-last-vop block) prev)))

  (values))

;;; Return a list of N normal TNs of the specified primitive type.
(defun make-n-tns (n ptype)
  (declare (type unsigned-byte n) (type primitive-type ptype))
  (loop repeat n
        collect (make-normal-tn ptype)))

;;; Return true if X and Y are packed in the same location, false otherwise.
;;; This is false if either operand is constant.
(defun location= (x y)
  (declare (type tn x y))
  (and (eq (sc-sb (tn-sc x)) (sc-sb (tn-sc y)))
       (eql (tn-offset x) (tn-offset y))
       (not (or (eq (tn-kind x) :constant)
                (eq (tn-kind y) :constant)))))

;;; Return the value of an immediate constant TN.
(defun tn-value (tn)
  (declare (type tn tn))
  (aver (eq (tn-kind tn) :constant))
  (constant-value (tn-leaf tn)))

(defun immediate-tn-p (tn)
  (declare (type tn tn))
  (let ((leaf (tn-leaf tn)))
    ;; Leaves with KIND :CONSTANT can have NIL as the leaf if they
    ;; represent load time values.
    (and leaf
         (eq (tn-kind tn) :constant)
         (eq (immediate-constant-sc (constant-value leaf))
             (sc-number-or-lose 'sb!vm::immediate)))))

;;; Force TN to be allocated in a SC that doesn't need to be saved: an
;;; unbounded non-save-p SC. We don't actually make it a real "restricted" TN,
;;; but since we change the SC to an unbounded one, we should always succeed in
;;; packing it in that SC.
(defun force-tn-to-stack (tn)
  (declare (type tn tn))
  (let ((sc (tn-sc tn)))
    (unless (and (not (sc-save-p sc))
                 (eq (sb-kind (sc-sb sc)) :unbounded))
      (dolist (alt (sc-alternate-scs sc)
                   (error "SC ~S has no :UNBOUNDED :SAVE-P NIL alternate SC."
                          (sc-name sc)))
        (when (and (not (sc-save-p alt))
                   (eq (sb-kind (sc-sb alt)) :unbounded))
          (setf (tn-sc tn) alt)
          (return)))))
  (values))

