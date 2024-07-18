;;;; implementation-independent facilities used for defining the
;;;; compiler's interface to the VM in a given implementation

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;; Return the template having the specified name, or die trying.
(defun template-or-lose (x)
  (the template
       (or (gethash x *backend-template-names*)
           (error "~S is not a defined template." x))))

;;; Return the SC structure, SB structure or SC number corresponding
;;; to a name, or die trying.
(defun sc-or-lose (x)
  (the storage-class
       (or (gethash x *backend-sc-names*)
           (error "~S is not a defined storage class." x))))
(defun sb-or-lose (x)
  (the storage-base
       (dovector (sb *backend-sbs*
                     (error "~S is not a defined storage base." x))
         (when (eq (sb-name sb) x)
           (return sb)))))

(defun sc-number-or-lose (x)
  (the sc-number (sc-number (sc-or-lose x))))

;;;; side effect classes

(!def-boolean-attribute vop
  any)

;;;; move/coerce definition

;;; Compute at compiler load time the costs for moving between all SCs that
;;; can be loaded from FROM-SC and to TO-SC given a base move cost Cost.
(defun compute-move-costs (from-sc to-sc cost)
  (declare (type storage-class from-sc to-sc) (type index cost))
  (let ((to-scn (sc-number to-sc))
        (from-costs (sc-load-costs from-sc)))
    (dolist (dest-sc (cons to-sc (sc-alternate-scs to-sc)))
      (let ((vec (sc-move-costs dest-sc))
            (dest-costs (sc-load-costs dest-sc)))
        (setf (svref vec (sc-number from-sc)) cost)
        (dolist (sc (append (sc-alternate-scs from-sc)
                            (sc-constant-scs from-sc)))
          (let* ((scn (sc-number sc))
                 (total (+ (svref from-costs scn)
                           (svref dest-costs to-scn)
                           cost))
                 (old (svref vec scn)))
            (unless (and old (< old total))
              (setf (svref vec scn) total))))))))

;;;; primitive type definition

;;; Return the primitive type corresponding to the specified name, or
;;; die trying.
(defun primitive-type-or-lose (name)
  (the primitive-type
       (or (gethash name *backend-primitive-type-names*)
           (error "~S is not a defined primitive type." name))))

;;; Return true if SC is either one of PTYPE's SC's, or one of those
;;; SC's alternate or constant SCs.
(defun sc-allowed-by-primitive-type (sc ptype)
  (declare (type storage-class sc) (type primitive-type ptype))
  (let ((scn (sc-number sc)))
    (dolist (allowed (primitive-type-scs ptype) nil)
      (when (eql allowed scn)
        (return t))
      (let ((allowed-sc (svref *backend-sc-numbers* allowed)))
        (when (or (member sc (sc-alternate-scs allowed-sc))
                  (member sc (sc-constant-scs allowed-sc)))
          (return t))))))

;;;; generation of emit functions

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; We need the EVAL-WHEN because EMIT-VOP (below)
  ;; uses #.MAX-VOP-TN-REFS, not just MAX-VOP-TN-REFS.
  ;; -- AL 20010218
  ;;
  ;; See also the description of VOP-INFO-TARGETS. -- APD, 2002-01-30
  (defconstant max-vop-tn-refs 256))

;;; Emit a VOP for TEMPLATE. Arguments:
;;; NODE Node for source context.
;;; BLOCK IR2-BLOCK that we place the VOP in.
;;; TEMPLATE: VOP template
;;; ARGS Head of argument TN-REF list.
;;; RESULT Head of result TN-REF list.
;;; INFO If INFO-ARG-COUNT is non-zero, then a list of the magic arguments.
;;;
;;; Return the emitted vop
(defun emit-vop (node block template args results &optional info)
  (let* ((vop (make-vop block node template args results))
         (num-args (vop-info-num-args template))
         (last-arg (1- num-args))
         (num-results (vop-info-num-results template))
         (num-operands (+ num-args num-results))
         (last-result (1- num-operands))
         (temps (vop-info-temps template))
         ;; Can't have more temps than registers in the CPU.
         ;; 64 is totally reasonable
         (refs (make-array (+ (* 2 (the (mod 64) (length temps)))
                              num-operands)))
         (ref-ordering (vop-info-ref-ordering template)))
    (declare (dynamic-extent refs))
    (declare (type vop vop)
             (type (integer 0 #.max-vop-tn-refs)
                   num-args num-results num-operands)
             (type (integer -1 #.(1- max-vop-tn-refs)) last-arg last-result))
    (when (vop-info-gc-barrier template)
      (setf info (append info (list (vop-info-gc-barrier template)))))
    (setf (vop-codegen-info vop) info)

    ;; Inputs
    (do ((index 0 (1+ index))
         (ref args (and ref (tn-ref-across ref))))
        ((= index num-args))
      (setf (svref refs index) ref))
    ;; Outputs
    (do ((index num-args (1+ index))
         (ref results (and ref (tn-ref-across ref))))
        ((= index num-operands))
      (setf (svref refs index) ref))
    ;; Temporaries
    (when temps
      (let ((index num-operands)
            (prev nil))
        (dotimes (i (length temps))
          (let* ((temp (aref temps i))
                 (tn (if (logbitp 0 temp)
                         (make-wired-tn
                          nil
                          (ldb (byte sb-vm:sc-number-bits 1) temp)
                          (ash temp (- (1+ sb-vm:sc-number-bits))))
                         (make-restricted-tn nil (ash temp -1))))
                 (write-ref (reference-tn tn t)))
                     ;; KLUDGE: These formulas must be consistent with
                     ;; those in COMPUTE-REF-ORDERING, and this is
                     ;; currently maintained by hand. -- WHN
                     ;; 2002-01-30, paraphrasing APD
            (setf (aref refs index) (reference-tn tn nil))
            (setf (aref refs (1+ index)) write-ref)
            (if prev
                (setf (tn-ref-across prev) write-ref)
                (setf (vop-temps vop) write-ref))
            (setf prev write-ref)
            (incf index 2)))))

    (let ((prev nil))
      (flet ((add-ref (ref)
               (setf (tn-ref-vop ref) vop)
               (setf (tn-ref-next-ref ref) prev)
               (setf prev ref)))
        (declare (inline add-ref))
        (dotimes (i (length ref-ordering))
          (let* ((index (aref ref-ordering i))
                 (ref (aref refs index)))
            (if (or (= index last-arg) (= index last-result))
                (do ((ref ref (tn-ref-across ref)))
                    ((null ref))
                  (add-ref ref))
                (add-ref ref)))))
      (setf (vop-refs vop) prev))

    (let ((targets (vop-info-targets template)))
      (when targets
        (dotimes (i (length targets))
          (let ((target (aref targets i)))
            (sb-regalloc:target-if-desirable
                    (aref refs (ldb (byte 8 8) target))
                    (aref refs (ldb (byte 8 0) target)))))))
    vop))

;;;; function translation stuff

;;; Add Template into List, removing any old template with the same name.
;;; We also maintain the increasing cost ordering.
(defun adjoin-template (template list)
  (declare (type template template) (list list))
  (stable-sort (sort (cons template
                           (remove (template-name template) list
                                   :key #'template-name))
                     #'string<
                     :key #'template-name)
               #'<=
               :key #'template-cost))

;;; Return a function type specifier describing TEMPLATE's type computed
;;; from the operand type restrictions.
(declaim (inline template-conditional-p))
(defun template-conditional-p (template)
  (declare (type template template))
  (let ((rtypes (template-result-types template)))
    (or (eq rtypes :conditional)
        (eq (car rtypes) :conditional))))

(defun template-type-specifier (template)
  (declare (type template template))
  (flet ((convert (types more-types)
           (flet ((frob (x)
                    (if (eq x '*)
                        t
                        (ecase (first x)
                          (:or `(or ,@(mapcar #'primitive-type-specifier
                                              (rest x))))
                          (:constant `(constant-arg ,(cdr x)))))))
             `(,@(mapcar #'frob types)
               ,@(when more-types
                   `(&rest ,(frob more-types)))))))
    (let* ((args (convert (template-arg-types template)
                          (template-more-args-type template)))
           (result-restr (template-result-types template))
           (results (if (template-conditional-p template)
                        '(boolean)
                        (convert result-restr
                                 (template-more-results-type template)))))
      `(function ,args
                 (values ,@results
                         ,@(unless (template-more-results-type template)
                             '(&optional)))))))

(defun template-translates-arg-p (function argument type)
  (let ((primitive-type (primitive-type (specifier-type type))))
    (loop for template in (fun-info-templates (info :function :info function))
          for arg-type = (nth argument (template-arg-types template))
          thereis (and (consp arg-type)
                       (memq primitive-type (cdr arg-type))))))
