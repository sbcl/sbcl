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

(in-package "SB-C")

;;; The component that is currently being compiled. TNs are allocated
;;; in this component.
(defvar *component-being-compiled*)

;;; DO-PACKED-TNS (TN-Var Component [Result]) Form*
;;;
;;; Iterate over all packed TNs allocated in COMPONENT.
(defmacro do-packed-tns ((tn component &optional result) &body body)
  (with-unique-names (n-component tns more-tns outer inner)
    `(prog* ((,n-component (component-info ,component))
             (,tn       (ir2-component-normal-tns ,n-component))
             (,tns      (ir2-component-restricted-tns ,n-component))
             (,more-tns (ir2-component-wired-tns ,n-component)))
       (when ,tn (go ,inner))
       ,outer (when (eq ,tns :done) (return ,result))
              (shiftf ,tn ,tns ,more-tns :done)
              (unless ,tn (go ,outer))
       ,inner (progn ,@body)
              (if (setq ,tn (tn-next ,tn)) (go ,inner) (go ,outer)))))

(defun set-ir2-environment-live-tns (value instance)
  (setf (ir2-environment-live-tns instance) value))

(defun set-ir2-environment-debug-live-tns (value instance)
  (setf (ir2-environment-debug-live-tns instance) value))

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
               (and (neq (tn-kind tn) :unused)
                    (or (tn-reads tn) (tn-writes tn)
                        (member (tn-kind tn) '(:component :environment))
                        (not (zerop (sbit aliases (tn-number tn)))))))
             (delete-1 (tn prev setter)
               (if prev
                   (setf (tn-next prev) (tn-next tn))
                   (funcall setter (tn-next tn) 2comp))
               (setf (tn-offset tn) nil)
               (case (tn-kind tn)
                 (:environment
                  (clear-live tn
                              #'ir2-environment-live-tns
                              #'set-ir2-environment-live-tns))
                 (:debug-environment
                  (clear-live tn
                              #'ir2-environment-debug-live-tns
                              #'set-ir2-environment-debug-live-tns))))
             (clear-live (tn getter setter)
               (let ((env (environment-info (tn-environment tn))))
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
(defun make-normal-tn (primitive-type &optional (type *universal-type*))
  (declare (type primitive-type primitive-type)
           (type ctype type))
  (let* ((component (component-info *component-being-compiled*))
         (res (make-tn (incf (ir2-component-global-tn-counter component))
                       :normal primitive-type nil)))
    (setf (tn-type res) (single-value-type type))
    (push-in tn-next res (ir2-component-normal-tns component))
    res))

;;; Create a normal packed TN with representation indicated by SCN.
(defun make-representation-tn (ptype scn &optional (type *universal-type*))
  (declare (type primitive-type ptype) (type sc-number scn))
  (let* ((component (component-info *component-being-compiled*))
         (res (make-tn (incf (ir2-component-global-tn-counter component))
                       :normal ptype
                       (svref *backend-sc-numbers* scn))))
    (setf (tn-type res) (single-value-type type))
    (push-in tn-next res (ir2-component-normal-tns component))
    res))

;;; Create a TN wired to a particular location in an SC. We set the Offset
;;; and FSC to record where it goes, and then put it on the current component's
;;; Wired-TNs list. Ptype is the TN's primitive-type, which may be NIL in VOP
;;; temporaries.
(defun make-wired-tn (ptype scn offset &optional (type *universal-type*))
  (declare (type (or primitive-type null) ptype)
           (type sc-number scn) (type unsigned-byte offset))
  (let* ((component (component-info *component-being-compiled*))
         (res (make-tn (incf (ir2-component-global-tn-counter component))
                       :normal ptype
                       (svref *backend-sc-numbers* scn))))
    (setf (tn-offset res) offset)
    (when ptype
      (setf (tn-type res) (if type
                              (single-value-type type)
                              *universal-type*)))
    (push-in tn-next res (ir2-component-wired-tns component))
    res))
(defun sb-vm::make-wired-tn* (prim-type-name scn offset)
  (make-wired-tn (primitive-type-or-lose prim-type-name) scn offset))

;;; Create a packed TN restricted to the SC with number SCN. Ptype is as
;;; for MAKE-WIRED-TN.
(defun make-restricted-tn (ptype scn &optional (type *universal-type*))
  (declare (type (or primitive-type null) ptype) (type sc-number scn))
  (let* ((component (component-info *component-being-compiled*))
         (res (make-tn (incf (ir2-component-global-tn-counter component))
                       :normal ptype
                       (svref *backend-sc-numbers* scn))))
    (when ptype
      (setf (tn-type res) (if type
                              (single-value-type type)
                              *universal-type*)))
    (push-in tn-next res (ir2-component-restricted-tns component))
    res))

(defun make-unused-tn ()
  (make-tn (incf (ir2-component-global-tn-counter (component-info *component-being-compiled*)))
           :unused nil nil))

;;; Make TN be live throughout ENV. Return TN. In the DEBUG case, the
;;; TN is treated normally in blocks in the environment which
;;; reference the TN, allowing targeting to/from the TN. This results
;;; in move efficient code, but may result in the TN sometimes not
;;; being live when you want it.
(defun environment-live-tn (tn env)
  (declare (type tn tn) (type environment env))
  (aver (eq (tn-kind tn) :normal))
  (setf (tn-kind tn) :environment)
  (setf (tn-environment tn) env)
  (push tn (ir2-environment-live-tns (environment-info env)))
  tn)
(defun environment-debug-live-tn (tn env)
  (declare (type tn tn) (type environment env))
  (aver (eq (tn-kind tn) :normal))
  (setf (tn-kind tn) :debug-environment)
  (setf (tn-environment tn) env)
  (push tn (ir2-environment-debug-live-tns (environment-info env)))
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
#-fp-and-pc-standard-save
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

;;; Create a constant TN. The backend dependent
;;; IMMEDIATE-CONSTANT-SC function is used to determine whether the
;;; constant has an immediate representation.
;;; FIXME: this can create multiple boxed TNs (consuming more than one
;;; slot in the boxed constant vector) for a given leaf when called by
;;; EMIT-MOVES-AND-COERCIONS. That's wasteful.
(defun make-constant-tn (constant &optional force-boxed)
  (declare (type constant constant))
  (or (and (tn-p (leaf-info constant))
           (leaf-info constant))
      (multiple-value-bind (immed null-offset)
          (immediate-constant-sc (constant-value constant))
        ;; currently NULL-OFFSET is used only on ARM64
        (if null-offset
            (let ((tn (component-live-tn
                       (make-wired-tn (primitive-type (leaf-type constant))
                                      immed
                                      null-offset
                                      (specifier-type 'null)))))
              (setf (tn-leaf tn) constant
                    (leaf-info constant) tn))
            (let* ((boxed (or (not immed)
                              (boxed-immediate-sc-p immed)))
                   (component (component-info *component-being-compiled*))
                   ;; If a constant has either an immediate or boxed
                   ;; representation (e.g. double-float) postpone the SC
                   ;; choice until SELECT-REPRESENTATIONS.
                   (sc (cond (boxed
                              (if immed
                                  (svref *backend-sc-numbers* immed)
                                  (sc-or-lose 'constant)))
                             (force-boxed
                              (setf immed nil)
                              (sc-or-lose 'constant))))
                   (res (make-tn 0 :constant (primitive-type (leaf-type constant)) sc)))
              ;; Objects of type SYMBOL can be immediate but they still go in the constants
              ;; because liveness depends on pointer tracing without looking at code-fixups.
              (when (and sc
                         (or (not immed)
                             #+(or immobile-space permgen)
                             (let ((val (constant-value constant)))
                               (or (and (symbolp val) (not (sb-vm:static-symbol-p val)))
                                   (typep val 'layout))))
                         #+(or arm64 x86-64)
                         (not (eql (constant-value constant) 0f0)))
                (let ((constants (ir2-component-constants component)))
                  (setf (tn-offset res)
                        (vector-push-extend constant constants))))
              (when sc
                (setf (leaf-info constant) res))
              (push-in tn-next res (ir2-component-constant-tns component))
              (setf (tn-type res) (leaf-type constant))
              (setf (tn-leaf res) constant)
              res)))))

;;; Extracted from above
(defun constant-sc (constant)
  (if (leaf-info constant)
      (tn-sc (leaf-info constant))
      (multiple-value-bind (immed null-offset)
          (immediate-constant-sc (constant-value constant))
        (if null-offset
            immed
            (let ((boxed (or (not immed)
                             (boxed-immediate-sc-p immed))))
              (cond (boxed
                     (if immed
                         (svref *backend-sc-numbers* immed)
                         (sc-or-lose 'constant)))
                    (t
                     (sc-or-lose 'constant))))))))

(defun make-load-time-value-tn (handle type)
  (let* ((component (component-info *component-being-compiled*))
         (sc (svref *backend-sc-numbers*
                    sb-vm:constant-sc-number))
         (res (make-tn 0 :constant (primitive-type type) sc))
         (constants (ir2-component-constants component)))
    (setf (tn-offset res) (fill-pointer constants)
          (tn-type res) type)
    ;; The third list element served no purpose as far as I can discern.
    ;; Perhaps it was for debugging?
    (vector-push-extend (list :load-time-value handle #|res|#) constants)
    (push-in tn-next res (ir2-component-constant-tns component))
    res))

;;; Make a TN that aliases TN for use in local call argument passing.
(defun make-alias-tn (tn)
  (declare (type tn tn))
  (let* ((component (component-info *component-being-compiled*))
         (res (make-tn (incf (ir2-component-global-tn-counter component))
                       :alias (tn-primitive-type tn) nil)))
    (setf (tn-save-tn res) tn
          (tn-type res) (tn-type tn))
    (setf (tn-vertex tn) :alias)
    (push-in tn-next res
             (ir2-component-alias-tns component))
    res))

;;; Return a load-time constant TN with the specified KIND and INFO.
;;; If the desired CONSTANTS entry already exists, then reuse it,
;;; otherwise allocate a new load-time constant slot.
;;; FIXME: this function deserves a comment about why it creates a new TN even
;;; when KIND + INFO are already found. It's as if we need to maintain a 1:1
;;; relation between TN and TN-REF for a subset of constants, but it's unclear
;;; why that should be necessary. The shape of this algorithm suggests more than
;;; mere oversight, and that it was a deliberate choice.
(defun make-load-time-constant-tn (kind info)
  (declare (type keyword kind))
  (let* ((component (component-info *component-being-compiled*))
         (res (make-tn 0
                       :constant
                       *backend-t-primitive-type*
                       (svref *backend-sc-numbers* sb-vm:constant-sc-number)))
         (constants (ir2-component-constants component)))
    (setf (tn-type res) *universal-type*)
    (do ((i 1 (1+ i)))
        ((= i (length constants))
         (setf (tn-offset res) i)
         ;; The third list element served no purpose as far as I can discern.
         ;; Perhaps it was for debugging?
         (vector-push-extend (list kind info #|res|#) constants))
      (let ((entry (aref constants i)))
        (when (and (consp entry)
                   (eq (car entry) kind)
                   (or (eq (cadr entry) info)
                       (and (consp info)
                            (equal (cadr entry) info))))
          (setf (tn-offset res) i)
          (return))))

    (push-in tn-next res (ir2-component-constant-tns component))
    res))

;;;; TN referencing

(defmacro link-tn-ref (write-p tn ref)
  `(cond (,write-p
          (let ((w (tn-writes ,tn)))
            (when w
              (setf (tn-ref-prev w) ,ref))
            (setf (tn-ref-next ,ref) w
                  (tn-writes ,tn) ,ref)))
         (t
          (let ((r (tn-reads ,tn)))
            (when r
              (setf (tn-ref-prev r) ,ref))
            (setf (tn-ref-next ,ref) r
                  (tn-reads ,tn) ,ref)))))

;;; Make a TN-REF that references TN and return it. WRITE-P should be
;;; true if this is a write reference, otherwise false. All we do
;;; other than calling the constructor is add the reference to the
;;; TN's references.
(defun reference-tn (tn write-p)
  (declare (type tn tn) (type boolean write-p))
  (let ((ref (make-tn-ref tn write-p)))
    (unless (eql (tn-kind tn) :unused)
      (when (tn-primitive-type tn)
        (aver (setf (tn-ref-type ref) (tn-type tn))))
      (link-tn-ref write-p tn ref))
    ref))

(defun reference-tn-refs (refs write-p)
  (when refs
    (let* ((first (reference-tn (tn-ref-tn refs) write-p))
           (prev first))
      (setf (tn-ref-type first) (tn-ref-type refs))
      (loop for tn-ref = (tn-ref-across refs) then (tn-ref-across tn-ref)
            while tn-ref
            do
            (let ((ref (reference-tn (tn-ref-tn tn-ref) write-p)))
              (setf (tn-ref-across prev) ref
                    (tn-ref-type ref) (tn-ref-type tn-ref))
              (setq prev ref)))
      first)))

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

;;; Copy the tn-ref-type of the TNs.
(defun reference-tn-ref-list (tn-refs write-p &optional more)
  (declare (list tn-refs) (type boolean write-p) (type (or tn-ref null) more))
  (if tn-refs
      (let* ((first (reference-tn (tn-ref-tn (first tn-refs)) write-p))
             (prev first))
        (setf (tn-ref-type first) (tn-ref-type (first tn-refs)))
        (dolist (tn-ref (rest tn-refs))
          (let ((res (reference-tn (tn-ref-tn tn-ref) write-p)))
            (setf (tn-ref-across prev) res
                  (tn-ref-type res) (tn-ref-type tn-ref))
            (setq prev res)))
        (setf (tn-ref-across prev) more)
        first)
      more))

;;; Remove Ref from the references for its associated TN.
(defun delete-tn-ref (ref)
  (declare (type tn-ref ref))
  (let ((tn (tn-ref-tn ref))
        (prev (tn-ref-prev ref))
        (next (tn-ref-next ref)))
    (cond ((tn-ref-write-p ref)
           (if prev
               (setf (tn-ref-next prev) next)
               (setf (tn-writes tn) next)))
          (t
           (if prev
               (setf (tn-ref-next prev) next)
               (setf (tn-reads tn) next))))
    (when next
      (setf (tn-ref-prev next) prev))
    (setf (tn-ref-prev ref) nil)))

;;; Do stuff to change the TN referenced by Ref. We remove Ref from its
;;; old TN's refs, add ref to TN's refs, and set the TN-REF-TN.
(defun change-tn-ref-tn (ref tn)
  (declare (type tn-ref ref) (type tn tn))
  (delete-tn-ref ref)
  (setf (tn-ref-tn ref) tn)
  (link-tn-ref (tn-ref-write-p ref) tn ref)
  nil)

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
    (emit-and-insert-vop node block template arg result before)))

;;; like EMIT-MOVE-TEMPLATE, except that we pass in INFO args too
(defun emit-load-template (node block template x y info &optional before)
  (declare (type node node) (type ir2-block block)
           (type template template) (type tn x y))
  (let ((arg (reference-tn x nil))
        (result (reference-tn y t)))
    (emit-and-insert-vop node block template arg result before info)))

;;; like EMIT-MOVE-TEMPLATE, except that the VOP takes two args
(defun emit-move-arg-template (node block template x f y &optional before)
  (declare (type node node) (type ir2-block block)
           (type template template) (type tn x f y))
  (let ((x-ref (reference-tn x nil))
        (f-ref (reference-tn f nil))
        (y-ref (reference-tn y t)))
    (setf (tn-ref-across x-ref) f-ref)
    (emit-and-insert-vop node block template x-ref y-ref before)))

;;; like EMIT-MOVE-TEMPLATE, except that the VOP takes no args
(defun emit-context-template (node block template y &optional before)
  (declare (type node node) (type ir2-block block)
           (type template template) (type tn y))
  (let ((y-ref (reference-tn y t)))
    (emit-and-insert-vop node block template nil y-ref before)))

;;; Return the label marking the start of Block, assigning one if necessary.
(defun block-label (block)
  (declare (type cblock block))
  (let ((2block (block-info block)))
    (or (ir2-block-%label 2block)
        (setf (ir2-block-%label 2block) (gen-label "basic block")))))
(defun block-trampoline (block)
  (declare (type cblock block))
  (let ((2block (block-info block)))
    (or (ir2-block-%trampoline-label 2block)
        (setf (ir2-block-%trampoline-label 2block) (gen-label "trampoline")))))

;;; Return true if Block is emitted immediately after the block ended by Node.
(defun drop-thru-p (node block)
  (declare (type node node) (type cblock block))
  (let ((next-block (ir2-block-next (block-info (node-block node)))))
    (aver (eq node (block-last (node-block node))))
    (eq next-block (block-info block))))
(defun register-drop-thru (block)
  (declare (type cblock block))
  (let ((2block (block-info block)))
    (setf (ir2-block-dropped-thru-to 2block) t))
  nil)

;;; Insert a VOP into BLOCK, before the specified
;;; BEFORE VOP. If BEFORE is NIL, insert at the end.
(defun insert-vop (vop block before)
  (declare (type vop vop) (type ir2-block block)
           (type (or vop null) before))
  (if before
      (let ((prev (vop-prev before)))
        (setf (vop-prev vop) prev)
        (if prev
            (setf (vop-next prev) vop)
            (setf (ir2-block-start-vop block) vop))
        (setf (vop-next vop) before)
        (setf (vop-prev before) vop))
      (let ((current (ir2-block-last-vop block)))
        (setf (vop-prev vop) current)
        (setf (ir2-block-last-vop block) vop)
        (if current
            (setf (vop-next current) vop)
            (setf (ir2-block-start-vop block) vop))))
  (values))

(defun emit-and-insert-vop (node block template arg result before
                            &optional info)
  (let ((vop (emit-vop node block template arg result info)))
    (insert-vop vop block before)
    vop))

;;; Delete all of the TN-REFs associated with VOP and remove VOP from the IR2.
(defun delete-vop (vop)
  (declare (type vop vop))
  (do ((ref (vop-refs vop) (tn-ref-next-ref ref)))
      ((null ref))
    (unless (eql (tn-kind (tn-ref-tn ref)) :unused)
      (delete-tn-ref ref)))
  (let ((prev (vop-prev vop))
        (next (vop-next vop))
        (block (vop-block vop)))
    (if prev
        (setf (vop-next prev) next)
        (setf (ir2-block-start-vop block) next))
    (if next
        (setf (vop-prev next) prev)
        (setf (ir2-block-last-vop block) prev)))

  nil)

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

(defun constant-tn-p (tn)
  (declare (type tn tn))
  (let ((leaf (tn-leaf tn)))
    ;; Leaves with KIND :CONSTANT can have NIL as the leaf if they
    ;; represent load time values.
    (and leaf
         (eq (tn-kind tn) :constant))))

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

