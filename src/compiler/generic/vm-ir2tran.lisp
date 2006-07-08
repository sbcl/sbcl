;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

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

(defun emit-inits (node block name result lowtag inits args)
  (let ((unbound-marker-tn nil))
    (dolist (init inits)
      (let ((kind (car init))
            (slot (cdr init)))
        (vop set-slot node block result
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
                (emit-constant nil)))
             name slot lowtag))))
  (aver (null args)))

(defun emit-fixed-alloc (node block name words type lowtag result)
  (vop fixed-alloc node block name words type lowtag result))

(defoptimizer ir2-convert-fixed-allocation
              ((&rest args) node block name words type lowtag inits)
  (let* ((lvar (node-lvar node))
         (locs (lvar-result-tns lvar
                                        (list *backend-t-primitive-type*)))
         (result (first locs)))
    (emit-fixed-alloc node block name words type lowtag result)
    (emit-inits node block name result lowtag inits args)
    (move-lvar-result node block locs lvar)))

(defoptimizer ir2-convert-variable-allocation
              ((extra &rest args) node block name words type lowtag inits)
  (let* ((lvar (node-lvar node))
         (locs (lvar-result-tns lvar
                                        (list *backend-t-primitive-type*)))
         (result (first locs)))
    (if (constant-lvar-p extra)
        (let ((words (+ (lvar-value extra) words)))
          (emit-fixed-alloc node block name words type lowtag result))
        (vop var-alloc node block (lvar-tn node block extra) name words
             type lowtag result))
    (emit-inits node block name result lowtag inits args)
    (move-lvar-result node block locs lvar)))

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
