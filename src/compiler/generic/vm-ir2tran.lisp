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

(defoptimizer (%make-structure-instance stack-allocate-result) ((&rest args))
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
  (let ((unbound-marker-tn nil)
        (funcallable-instance-tramp-tn nil))
    (dolist (init inits)
      (let ((kind (car init))
            (slot (cdr init)))
        (case kind
          (:slot
           (let ((raw-type (pop slot))
                 (arg-tn (lvar-tn node block (pop args))))
             (macrolet ((make-case ()
                          `(ecase raw-type
                             ((t)
                              (vop set-slot node block object arg-tn
                                   name (+ sb!vm:instance-slots-offset slot) lowtag))
                             ,@(mapcar (lambda (rsd)
                                         `(,(sb!kernel::raw-slot-data-raw-type rsd)
                                            (vop ,(sb!kernel::raw-slot-data-init-vop rsd)
                                                 node block
                                                 object arg-tn instance-length slot)))
                                       #!+raw-instance-init-vops
                                       sb!kernel::*raw-slot-data-list*
                                       #!-raw-instance-init-vops
                                       nil))))
               (make-case))))
          (:dd
           (vop set-slot node block object
                (emit-constant (sb!kernel::dd-layout-or-lose slot))
                name sb!vm:instance-slots-offset lowtag))
          (otherwise
           (vop set-slot node block object
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

;;; :SET-TRANS (in objdef.lisp DEFINE-PRIMITIVE-OBJECT) doesn't quite
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
