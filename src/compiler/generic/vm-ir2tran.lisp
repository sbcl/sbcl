;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

(def-alloc '%make-structure-instance 1 :structure-alloc
           sb-vm:instance-widetag sb-vm:instance-pointer-lowtag
           nil)
(def-alloc '%make-instance/mixed 1 :var-alloc
           sb-vm:instance-widetag sb-vm:instance-pointer-lowtag nil)

(defoptimizer (%make-structure-instance stack-allocate-result)
    ((defstruct-description &rest args))
  (aver (constant-lvar-p defstruct-description))
  ;; A structure instance can be stack-allocated if it has no raw
  ;; slots, or if we're on a target with a conservatively-scavenged
  ;; stack.  We have no reader conditional for stack conservation, but
  ;; it turns out that the only time stack conservation is in play is
  ;; C-STACK-IS-CONTROL-STACK (otherwise, the C stack is the
  ;; number stack, and we precisely-scavenge the control stack).
  (or #+c-stack-is-control-stack t
      (not (dd-has-raw-slot-p (lvar-value defstruct-description)))))

(defoptimizer (%make-instance stack-allocate-result) ((n) node)
  t)
#+c-stack-is-control-stack
(defoptimizer (%make-instance/mixed stack-allocate-result) ((n) node)
  t)
(defoptimizer (%make-funcallable-instance stack-allocate-result) ((n) node)
  t)

(defoptimizer ir2-convert-reffer ((object) node block name offset lowtag)
  (let* ((lvar (node-lvar node))
         (locs (lvar-result-tns lvar (list (if lvar
                                               (lvar-type lvar)
                                               *universal-type*))))
         (res (first locs)))
    (vop slot node block (lvar-tn node block object)
         name offset lowtag res)
    (move-lvar-result node block locs lvar)))

(defoptimizer ir2-convert-setter ((object value) node block name offset lowtag)
  (let ((value-tn (lvar-tn node block value)))
    (vop set-slot node block (lvar-tn node block object) (:lvar value value-tn)
         name offset lowtag)
    (move-lvar-result node block (list value-tn) (node-lvar node))))

;;; FIXME: Isn't there a name for this which looks less like a typo?
;;; (The name IR2-CONVERT-SETTER is used for something else, just above.)
(defoptimizer ir2-convert-setfer ((value object) node block name offset lowtag)
  (let ((value-tn (lvar-tn node block value)))
    (vop set-slot node block (lvar-tn node block object) value-tn
         name offset lowtag)
    (move-lvar-result node block (list value-tn) (node-lvar node))))

#+compare-and-swap-vops
(defoptimizer ir2-convert-casser
    ((object old new) node block name offset lowtag)
  (let* ((lvar (node-lvar node))
         (locs (lvar-result-tns lvar (list *universal-type*)))
         (res (first locs)))
    (vop compare-and-swap-slot node block
         (lvar-tn node block object)
         (:lvar old (lvar-tn node block old))
         (:lvar new (lvar-tn node block new))
         name offset lowtag
         res)
    (move-lvar-result node block locs lvar)))

(defun emit-inits (node block object lowtag inits args)
  (let ((unbound-marker-tn nil)
        (dx-p (node-stack-allocate-p node)))
    (flet ((zero-init-p (x)
             ;; dynamic-space is already zeroed
             (and (not dx-p)
                  (constant-lvar-p x)
                  (eql (lvar-value x) 0))))
     (dolist (init inits)
       (let ((kind (car init))
             (slot (cdr init)))
         (case kind
           (:slot
            (let ((raw-type (pop slot))
                  (arg (pop args)))
              (unless (and (or (eq raw-type t)
                               (eq raw-type 'word)) ;; can be made to handle floats
                           (zero-init-p arg))
                (let ((arg-tn (lvar-tn node block arg)))
                  (macrolet
                      ((make-case (&aux (rsd-list sb-kernel::*raw-slot-data*))
                         `(ecase raw-type
                            ((t)
                             (vop set-slot node block object arg-tn
                                  :allocator (+ sb-vm:instance-slots-offset slot) lowtag))
                            ,@(map 'list
                               (lambda (rsd)
                                 `(,(sb-kernel::raw-slot-data-raw-type rsd)
                                   (vop ,(sb-kernel::raw-slot-data-writer-name rsd)
                                        node block object (emit-constant slot) arg-tn)))
                               rsd-list))))
                    (make-case))))))
           ;; compact header passes the layout to EMIT-FIXED-ALLOC
           ;; which generates the instruction to set the header.
           #-compact-instance-header
           (:dd
            (vop set-slot node block object
                 (emit-constant (sb-kernel::dd-layout-or-lose slot))
                 :allocator sb-vm:instance-slots-offset lowtag))
           (otherwise
            (if (and (eq kind :arg)
                     (zero-init-p (car args)))
                (pop args)
                (vop set-slot node block object
                     (ecase kind
                       (:arg
                        (aver args)
                        (lvar-tn node block (pop args)))
                       (:unbound
                        ;; SLOT should be the word index to alter, but with structure
                        ;; instances, SLOT is a cons whose car is the raw-slot-type
                        ;; since BOXED-COMBINATION-REF-P expects that #'cddr is the
                        ;; slot index.
                        (when (listp slot)
                          (setq slot (+ (cdr slot) sb-vm:instance-slots-offset)))
                        (or unbound-marker-tn
                            (setf unbound-marker-tn
                                  (let ((tn (make-restricted-tn
                                             nil sb-vm:any-reg-sc-number)))
                                    (vop make-unbound-marker node block tn)
                                    tn))))
                       (:null
                        (emit-constant nil)))
                     :allocator slot lowtag))))))))
  (unless (null args)
    (bug "Leftover args: ~S" args))
  (ir2-block-last-vop block))

(defun unbound-marker-tn-p (tn)
  (let ((writes (tn-writes tn)))
    (and writes
         (not (tn-ref-next writes)) ; is never changed
         (let ((vop (tn-ref-vop writes)))
           (and vop (eq (vop-name vop) 'make-unbound-marker))))))

(defun emit-fixed-alloc (node block name words type lowtag result)
  (if (node-stack-allocate-p node)
      (vop fixed-alloc-to-stack node block name words type lowtag t result)
      (vop fixed-alloc node block name words type lowtag nil result)))

(defun link-allocator-to-inits (allocator-vop last-init-vop node)
  (declare (ignorable allocator-vop last-init-vop node))
  ;; This can easily enough be changed to any set of platforms for which it is
  ;; desirable to assign structure slots within pseudo-atomic in a constructor
  #+x86-64
  (unless (or (node-stack-allocate-p node) (eq last-init-vop allocator-vop))
    ;; extra codegen info is OK
    (setf (vop-codegen-info allocator-vop)
          (append (vop-codegen-info allocator-vop) (list last-init-vop)))))

(defoptimizer ir2-convert-fixed-allocation
              ((&rest args) node block name words type lowtag inits)
  (let* ((lvar (the (not null) (node-lvar node)))
         (locs (lvar-result-tns lvar (list *universal-type*)))
         (result (first locs)))
    (emit-fixed-alloc node block name words type lowtag result)
    (let ((allocator-vop (ir2-block-last-vop block))
          (last-init-vop (emit-inits node block result lowtag inits args)))
      (link-allocator-to-inits allocator-vop last-init-vop node))
    (move-lvar-result node block locs lvar)))

(defoptimizer ir2-convert-variable-allocation
              ((extra &rest args) node block name words type lowtag inits)
  (let* ((lvar (the (not null) (node-lvar node)))
         (locs (lvar-result-tns lvar (list *universal-type*)))
         (result (first locs)))
    (if (constant-lvar-p extra)
        (let ((words (+ (lvar-value extra) words)))
          (emit-fixed-alloc node block name words type lowtag result))
        (vop var-alloc node block (lvar-tn node block extra) name words
             type lowtag (node-stack-allocate-p node) result))
    (let ((allocator-vop (ir2-block-last-vop block))
          (last-init-vop (emit-inits node block result lowtag inits args)))
      (link-allocator-to-inits allocator-vop last-init-vop node))
    (move-lvar-result node block locs lvar)))

(defoptimizer ir2-convert-structure-allocation
    ((dd slot-specs &rest args) node block name words type lowtag)
  (let* ((lvar (the (not null) (node-lvar node)))
         (locs (lvar-result-tns lvar (list *universal-type*)))
         (result (first locs)))
    (aver (and (constant-lvar-p dd) (constant-lvar-p slot-specs) (= words 1)))
    (aver (= type sb-vm:instance-widetag))
    (let* ((c-dd (lvar-value dd))
           (c-slot-specs (lvar-value slot-specs))
           (words (+ (dd-length c-dd) words)))
      (let ((metadata #+compact-instance-header
                      (let ((layout (sb-kernel::dd-layout-or-lose c-dd)))
                        (emit-constant layout)
                        layout)
                      #-compact-instance-header
                      c-dd))
        (emit-fixed-alloc node block name words metadata lowtag result))
      (let ((allocator-vop (ir2-block-last-vop block))
            (last-init-vop
             (emit-inits node block result lowtag
                         `(#-compact-instance-header (:dd . ,c-dd) ,@c-slot-specs) args)))
        (link-allocator-to-inits allocator-vop last-init-vop node))
      (move-lvar-result node block locs lvar))))

(defoptimizer (initialize-vector ir2-convert)
    ((vector &rest initial-contents) node block)
  (let* ((vector-ctype (lvar-type vector))
         (elt-ctype (if (array-type-p vector-ctype)
                        (array-type-specialized-element-type vector-ctype)
                        (bug "Unknown vector type in IR2 conversion for ~S."
                             'initialize-vector)))
         (bit-vector-p (type= elt-ctype (specifier-type 'bit)))
         (simple-vector-p (type= elt-ctype (specifier-type 't)))
         (saetp (find-saetp-by-ctype elt-ctype))
         (lvar (node-lvar node))
         (locs (lvar-result-tns lvar (list vector-ctype)))
         (result (first locs))
         (elt-ptype (primitive-type elt-ctype))
         (tmp (make-normal-tn elt-ptype)))
    (declare (ignorable bit-vector-p simple-vector-p))
    (emit-move node block (lvar-tn node block vector) result)
    (flet ((compute-setter ()
             ;; Such cringe. I had no idea why all the "-C" vops were mandatory.
             ;; Too bad we can't let the backend decide how it would like to do things.
             ;; Not to mention, this code is confusing because RESULT is the argument,
             ;; and TN - the value to store - is the result, and an argument.
             ;; Also note that the constant-index vops want the operands to the
             ;; VOP macro as (VECTOR VALUE INDEX OFFSET) + (RESULT)
             ;; but the non-constant want (VECTOR INDEX VALUE OFFSET) + (RESULT).
             ;; They could totally have been made the same.
             (macrolet
                 ((frob ()
                    `(ecase (sb-vm:saetp-typecode saetp)
                       ,@(map 'list
                          (lambda (s &aux (ptype (sb-vm:saetp-primitive-type-name s))
                                          (*package* (find-package "SB-VM")))
                            `(,(sb-vm:saetp-typecode s)
                              (lambda (index tn)
                                #+x86-64
                                ,(if (eq ptype 'simple-bit-vector) ; no "-C" setter exists
                                     `(vop ,(symbolicate "DATA-VECTOR-SET-WITH-OFFSET/" ptype)
                                           node block result index tn 0)
                                     `(vop ,(symbolicate "DATA-VECTOR-SET-WITH-OFFSET/" ptype "-C")
                                           node block result tn index 0))
                                #+x86
                                (vop ,(symbolicate "DATA-VECTOR-SET-WITH-OFFSET/" ptype)
                                     node block result index tn 0)
                                #-(or x86 x86-64)
                                (vop ,(symbolicate "DATA-VECTOR-SET/" ptype)
                                     node block result index tn))))
                          (remove nil sb-vm:*specialized-array-element-type-properties*
                                  :key #'sb-vm:saetp-specifier)))))
               (frob)))
           (tnify (index)
             #-x86-64
             (emit-constant index)
             #+x86-64
             (if bit-vector-p ; moar cringe
                 (emit-constant index)
                 index)))
      (let ((setter (compute-setter))
            (length (length initial-contents))
            (dx-p (node-stack-allocate-p node))
            (character (eq (primitive-type-name elt-ptype)
                           'character)))
        (dotimes (i length)
          (let ((value (pop initial-contents)))
            ;; dynamic-space is already zeroed
            (unless (and (not dx-p)
                         (constant-lvar-p value)
                         (if character
                             (eql (char-code (lvar-value value)) 0)
                             (eql (lvar-value value) 0)))
              ;; With SIMPLE-BIT-VECTOR, prefer to pass a constant TN if we can, as it emits
              ;; better code (at least on x86-64) by using the constant to discern between
              ;; the BTS or BTR opcode. However, a new suboptimality comes from that,
              ;; which is that by not passing a LOCATION= TN for the output, the final MOVE
              ;; in the setter thinks that it has to do something.
              ;; Nonetheless it's far better than it was. In all other scenarios, don't pass
              ;; a constant TN, because we don't know that generated code is better.
              (cond #+x86-64 ; still moar cringe
                    ((and (or bit-vector-p simple-vector-p) (constant-lvar-p value)
                          ;; check for constant named-ness to not
                          ;; trigger load form processing.
                          (legal-immediate-constant-p (nth-value 1 (lvar-value value))))
                     (funcall setter (tnify i) (emit-constant (lvar-value value))))
                    (t
                     ;; FIXME: for simple-vector, fixnums should get stored via an ANY-REG
                     ;; so that data-vector-set doesn't emit a store barrier.
                     ;; (TMP is a descriptor-reg because ELT-CTYPE is T)
                     ;; Or just fix this optimizer - as commented above - to somehow elide
                     ;; the barrier for all elements.
                     (emit-move node block (lvar-tn node block value) tmp)
                     (funcall setter (tnify i) tmp))))))))
    (move-lvar-result node block locs lvar)))

;;; An array header for simple non-unidimensional arrays is a fixed alloc,
;;; because the rank has to be known.
;;; (There are no compile-time optimizations for unknown rank arrays)
;;; WIDETAG may have ORed into it 1 bit for +ARRAY-FILL-POINTER-P+
(defoptimizer (make-array-header* ir2-convert) ((widetag &rest args) node block)
  (let ((n-args (length args)))
    ;; Remove the widetag lvar
    (pop (basic-combination-args node))
    (ir2-convert-fixed-allocation
     node block 'make-array
     ;; Each argument fills in one slot of the array header.
     ;; Add one word for the primitive object's header word.
     (1+ n-args)
     (lvar-value widetag)
     sb-vm:other-pointer-lowtag
     (loop for i from 1 to n-args collect `(:arg . ,i)))))

;;; Stack allocation optimizers per platform support
(defoptimizer (make-array-header* stack-allocate-result) ((&rest args))
  t)
(defoptimizer (allocate-vector stack-allocate-result)
    ((#+ubsan poisoned type length words) node)
  (and
   ;; Can't put unboxed data on the stack unless we scavenge it
   ;; conservatively.
   #-c-stack-is-control-stack
   (constant-lvar-p type)
   #-c-stack-is-control-stack
   (member (lvar-value type)
           '#.(list (sb-vm:saetp-typecode (find-saetp 't))
                    (sb-vm:saetp-typecode (find-saetp 'fixnum))))))
(defoptimizer (allocate-vector ltn-annotate)
    ((#+ubsan poisoned type length words) call ltn-policy)
  (vectorish-ltn-annotate-helper call ltn-policy
                                 (if (sb-c:msan-unpoison sb-c:*compilation*)
                                     'sb-vm::allocate-vector-on-stack+msan-unpoison
                                     'sb-vm::allocate-vector-on-stack)
                                 (cond #-x86-64
                                       ((sb-vm::system-tlab-p 0 call)
                                        'sb-vm::sys-allocate-vector-on-heap)
                                       (t
                                        'sb-vm::allocate-vector-on-heap))))

(defun make-vector-check-overflow-p (node)
  (not (or (zerop (policy node safety))
           ;; If the vector object fits in one page, no overflow checking is needed since it will hit the guard page.
           (let ((words #-ubsan (third (combination-args node))
                        #+ubsan (fourth (combination-args node))))
             (values-subtypep (lvar-derived-type words)
                              (specifier-type
                               `(integer 0 ,(- (/ +backend-page-bytes+ sb-vm:n-word-bytes)
                                               sb-vm:vector-data-offset))))))))

(defun vectorish-ltn-annotate-helper (call ltn-policy dx-template not-dx-template)
  (let* ((args (basic-combination-args call))
         (template-name (if (node-stack-allocate-p call)
                            dx-template
                            not-dx-template))
         (template (template-or-lose template-name)))
    (dolist (arg args)
      (setf (lvar-info arg)
            (make-ir2-lvar (primitive-type (lvar-type arg)))))
    (aver (is-ok-template-use template call (ltn-policy-safe-p ltn-policy)))
    (setf (basic-combination-info call) template)
    (setf (node-tail-p call) nil)
    (dolist (arg args)
      (annotate-1-value-lvar arg))))

;;; ...lists

(defoptimizer (list stack-allocate-result) ((&rest args))
  (not (null args)))
(defoptimizer (list* stack-allocate-result) ((&rest args))
  (not (null (rest args))))
(defoptimizer (%listify-rest-args stack-allocate-result) ((&rest args))
  t)

;;; ...conses
(defoptimizer (cons stack-allocate-result) ((&rest args))
  t)
(defoptimizer (%make-complex stack-allocate-result) ((&rest args))
  t)

(defoptimizer (unaligned-dx-cons stack-allocate-result) ((car))
  t)

;;; MAKE-LIST optimizations
#+x86-64
(progn
  (defoptimizer (%make-list stack-allocate-result) ((length element) node)
    t)
  (defoptimizer (%make-list ltn-annotate) ((length element) call ltn-policy)
    (vectorish-ltn-annotate-helper call ltn-policy
                                   'sb-vm::allocate-list-on-stack
                                   'sb-vm::allocate-list-on-heap)))

(defun make-list-check-overflow-p (node)
  (not (or (zerop (policy node safety))
           ;; If the list object fits in one page, no overflow checking is needed since it will hit the guard page.
           (let ((length (first (combination-args node))))
             (values-subtypep (lvar-derived-type length)
                              (specifier-type
                               `(integer 0 ,(- (/ +backend-page-bytes+ sb-vm:n-word-bytes)
                                               sb-vm:vector-data-offset))))))))

;;; Return the vop that wrote the TN referenced by TN-REF,
;;; but look through MOVEs.
(defun producer-vop (tn-ref)
  (let ((vop (tn-ref-vop (tn-writes (tn-ref-tn tn-ref)))))
    (if (neq (vop-name vop) 'move)
        vop
        (tn-ref-vop (tn-writes (tn-ref-tn (vop-args vop)))))))

(defun elide-zero-fill (vop)
  (let* ((writer (producer-vop (vop-args vop)))
         ;; Take the last of the info arguments
         ;; in case WORDS is also an info argument.
         (last (car (last (vop-codegen-info vop))))
         (value
          (if last
              (the (or sb-vm:word
                       (member :trap :unbound :safe-default :unsafe-default))
                   last)
              (return-from elide-zero-fill))) ; :INITIAL-ELEMENT NIL
         (elidep
          (ecase (vop-name writer)
            (sb-vm::allocate-vector-on-heap
             (member value '(0 :safe-default :unsafe-default)))
            ((sb-vm::allocate-vector-on-stack
              sb-vm::allocate-vector-on-stack+msan-unpoison)
             ;; For most specialized vectors, any random bits can
             ;; be regarded as a :SAFE-DEFAULT. If :UNSAFE-DEFAULT, then random
             ;; bits are OK (even for SIMPLE-VECTOR if not using precise gencgc).
             (eq value :unsafe-default)))))
    (when elidep ; change it to a MOVE
      (let ((new (emit-and-insert-vop (vop-node vop) (vop-block vop)
                                      (template-or-lose 'move)
                                      (reference-tn (tn-ref-tn (vop-args vop)) nil)
                                      (reference-tn (tn-ref-tn (vop-results vop)) t)
                                      vop)))
        (delete-vop vop)
        new))))

(in-package "SB-VM")
;;; Return a list of parameters with which to call MAKE-ARRAY-HEADER*
;;; given the mandatory slots for a simple array of rank 0 or > 1.
(defun make-array-header-inits (storage n-elements dimensions)
  (let ((primitive-obj (load-time-value (primitive-object 'array) t)))
    (nconc (loop with slots = (primitive-object-slots primitive-obj)
                 for i from 0 below (1- (length slots))
                 collect
                 (let ((slot (svref slots i)))
                     (ecase (slot-name slot)
                       (data           storage)
                       (fill-pointer   n-elements)
                       (elements       n-elements)
                       (displacement   0)
                       (displaced-p    nil)
                       (displaced-from nil))))
           dimensions)))

;;; This order is used by SB-C::TRANSFORM-MAKE-ARRAY-VECTOR
;;; The slot formerly known as FILL-POINTER-P is 1 bit in the header now.
(assert (equal (map 'list #'slot-name
                    (primitive-object-slots (primitive-object 'array)))
               '(fill-pointer elements data
                 displacement displaced-p displaced-from dimensions)))

(define-vop (touch-object)
  (:translate touch-object)
  (:args (object))
  (:ignore object)
  (:policy :fast-safe)
  (:arg-types t)
  (:generator 0))

#+(or x86 x86-64)
(define-vop ()
  (:translate touch-object-identity)
  (:args (object :scs (descriptor-reg any-reg sap-reg
                       signed-reg unsigned-reg)
                 :target res))
  (:policy :fast-safe)
  (:results (res :scs (descriptor-reg)))
  (:generator 0
    (move res object)))

;;; By pure coincidence, almost all the architectures that lack a +-MODFX vop
;;; have an instruction named ADD with the same argument order.
;;; MIPS needs to use ADDU to avoid trapping on overflow.
#+(or arm mips ppc sparc)
(define-vop (+-modfx)
  (:translate +-modfx)
  (:policy :fast-safe)
  (:args (x :scs (any-reg)) (y :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:results (res :scs (any-reg)))
  (:result-types fixnum)
  (:generator 1 (inst #+mips addu #-mips add res x y)))
