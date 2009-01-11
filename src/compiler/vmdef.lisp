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

(in-package "SB!C")

;;; Return the template having the specified name, or die trying.
(defun template-or-lose (x)
  (the template
       (or (gethash x *backend-template-names*)
           (error "~S is not a defined template." x))))

;;; Return the SC structure, SB structure or SC number corresponding
;;; to a name, or die trying.
(defun sc-or-lose (x)
  (the sc
       (or (gethash x *backend-sc-names*)
           (error "~S is not a defined storage class." x))))
(defun sb-or-lose (x)
  (the sb
       (or (gethash x *backend-sb-names*)
           (error "~S is not a defined storage base." x))))
(defun sc-number-or-lose (x)
  (the sc-number (sc-number (sc-or-lose x))))

;;; This is like the non-meta versions, except we go for the
;;; meta-compile-time info. These should not be used after load time,
;;; since compiling the compiler changes the definitions.
(defun meta-sc-or-lose (x)
  (the sc
       (or (gethash x *backend-meta-sc-names*)
           (error "~S is not a defined storage class." x))))
(defun meta-sb-or-lose (x)
  (the sb
       (or (gethash x *backend-meta-sb-names*)
           (error "~S is not a defined storage base." x))))
(defun meta-sc-number-or-lose (x)
  (the sc-number (sc-number (meta-sc-or-lose x))))

;;;; side effect classes

(!def-boolean-attribute vop
  any)

;;;; move/coerce definition

;;; Compute at compiler load time the costs for moving between all SCs that
;;; can be loaded from FROM-SC and to TO-SC given a base move cost Cost.
(defun compute-move-costs (from-sc to-sc cost)
  (declare (type sc from-sc to-sc) (type index cost))
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
  (declare (type sc sc) (type primitive-type ptype))
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
  ;; We need the EVAL-WHEN because %EMIT-GENERIC-VOP (below)
  ;; uses #.MAX-VOP-TN-REFS, not just MAX-VOP-TN-REFS.
  ;; -- AL 20010218
  ;;
  ;; See also the description of VOP-INFO-TARGETS. -- APD, 2002-01-30
  (def!constant max-vop-tn-refs 256))

;;; FIXME: This is a remarkably eccentric way of implementing what
;;; would appear to be by nature a closure.  A closure isn't any more
;;; threadsafe than this special variable implementation, but at least
;;; it's more idiomatic, and one could imagine closing over an
;;; extensible pool to make a thread-safe implementation.
(declaim (type (simple-vector #.max-vop-tn-refs) *vop-tn-refs*))
(defvar *vop-tn-refs* (make-array max-vop-tn-refs :initial-element nil))

(def!constant sc-bits (integer-length (1- sc-number-limit)))

(defun emit-generic-vop (node block template args results &optional info)
  (%emit-generic-vop node block template args results info))

(defun %emit-generic-vop (node block template args results info)
  (let* ((vop (make-vop block node template args results))
         (num-args (vop-info-num-args template))
         (last-arg (1- num-args))
         (num-results (vop-info-num-results template))
         (num-operands (+ num-args num-results))
         (last-result (1- num-operands))
         (ref-ordering (vop-info-ref-ordering template)))
    (declare (type vop vop)
             (type (integer 0 #.max-vop-tn-refs)
                   num-args num-results num-operands)
             (type (integer -1 #.(1- max-vop-tn-refs)) last-arg last-result))
    (setf (vop-codegen-info vop) info)
    (unwind-protect
         (let ((refs *vop-tn-refs*))
           (declare (type (simple-vector #.max-vop-tn-refs) refs))
           (do ((index 0 (1+ index))
                (ref args (and ref (tn-ref-across ref))))
               ((= index num-args))
             (setf (svref refs index) ref))
           (do ((index num-args (1+ index))
                (ref results (and ref (tn-ref-across ref))))
               ((= index num-operands))
             (setf (svref refs index) ref))
           (let ((temps (vop-info-temps template)))
             (when temps
               (let ((index num-operands)
                     (prev nil))
                 (dotimes (i (length temps))
                   (let* ((temp (aref temps i))
                          (tn (if (logbitp 0 temp)
                                  (make-wired-tn nil
                                                 (ldb (byte sc-bits 1) temp)
                                                 (ash temp (- (1+ sc-bits))))
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
                     (incf index 2))))))
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
                   (target-if-desirable
                    (aref refs (ldb (byte 8 8) target))
                    (aref refs (ldb (byte 8 0) target)))))))
           (values vop vop))
      (fill *vop-tn-refs* nil))))

;;;; function translation stuff

;;; Add Template into List, removing any old template with the same name.
;;; We also maintain the increasing cost ordering.
(defun adjoin-template (template list)
  (declare (type template template) (list list))
  (sort (cons template
              (remove (template-name template) list
                      :key #'template-name))
        #'<=
        :key #'template-cost))

;;; Return a function type specifier describing TEMPLATE's type computed
;;; from the operand type restrictions.
(defun template-type-specifier (template)
  (declare (type template template))
  (flet ((convert (types more-types)
           (flet ((frob (x)
                    (if (eq x '*)
                        t
                        (ecase (first x)
                          (:or `(or ,@(mapcar #'primitive-type-specifier
                                              (rest x))))
                          (:constant `(constant-arg ,(third x)))))))
             `(,@(mapcar #'frob types)
               ,@(when more-types
                   `(&rest ,(frob more-types)))))))
    (let* ((args (convert (template-arg-types template)
                          (template-more-args-type template)))
           (result-restr (template-result-types template))
           (results (if (template-conditional-p template)
                        '(boolean)
                        (convert result-restr
                                 (cond ((template-more-results-type template))
                                       ((/= (length result-restr) 1) '*)
                                       (t nil))))))
      `(function ,args
                 ,(if (= (length results) 1)
                      (first results)
                      `(values ,@results))))))

#!-sb-fluid (declaim (inline template-conditional-p))
(defun template-conditional-p (template)
  (declare (type template template))
  (let ((rtypes (template-result-types template)))
    (or (eq rtypes :conditional)
        (eq (car rtypes) :conditional))))
