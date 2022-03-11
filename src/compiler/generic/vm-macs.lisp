;;;; some macros and constants that are object-format-specific or are
;;;; used for defining the object format

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;;; other miscellaneous stuff

;;; This returns a form that returns a dual-word aligned number of bytes when
;;; given a number of words.
;;;
;;; FIXME: should be a function
;;; FIXME: should be called PAD-DATA-BLOCK-SIZE
(defmacro pad-data-block (words)
  `(logandc2 (+ (ash ,words word-shift) lowtag-mask) lowtag-mask))

;;;; primitive object definition stuff

(defun remove-keywords (options keywords)
  (cond ((null options) nil)
        ((member (car options) keywords)
         (remove-keywords (cddr options) keywords))
        (t
         (list* (car options) (cadr options)
                (remove-keywords (cddr options) keywords)))))

(defstruct (primitive-object (:copier nil))
  (name nil :type symbol :read-only t)
  (widetag nil :type symbol :read-only t)
  (lowtag nil :type symbol :read-only t)
  (length 0 :type fixnum :read-only t)
  (variable-length-p nil :type boolean :read-only t)
  (slots #() :type vector :read-only t))

(defun slot-offset (slot) (car slot))
(defun slot-name (slot) (cadr slot))
(defun slot-special (slot) (getf (cddr slot) :special))

(declaim (freeze-type primitive-object))

(define-load-time-global *primitive-objects* nil)
(defun primitive-object (name)
  (find name *primitive-objects* :key #'primitive-object-name))
(defun primitive-object-slot (obj name)
  (find name (primitive-object-slots obj):key #'slot-name))

(defun !%define-primitive-object (primobj)
  (let ((name (primitive-object-name primobj)))
    (setf *primitive-objects*
          (cons primobj
                (remove name *primitive-objects*
                        :key #'primitive-object-name :test #'eq)))
    name))

(defvar *!late-primitive-object-forms* nil)

(defmacro define-primitive-object
          ((name &key lowtag widetag alloc-trans (type t)
                      (size (symbolicate name "-SIZE")))
           &rest slot-specs)
  (collect ((slots) (specials) (constants) (forms) (inits))
    (let ((offset (if widetag 1 0))
          (variable-length-p nil))
      (dolist (spec slot-specs)
        (when variable-length-p
          (error "No more slots can follow a :rest-p slot."))
        (destructuring-bind
            (slot-name &rest options
                       &key rest-p (length (if rest-p 0 1))
                       ((:type slot-type) t) init
                       (ref-known nil ref-known-p) ref-trans
                       (set-known nil set-known-p) set-trans
                       cas-trans
                       special
                       pointer
                       &allow-other-keys)
            (if (atom spec) (list spec) spec)
          (declare (ignorable pointer))
          (slots (list* offset slot-name
                        (remove-keywords
                         options
                         `(#+sb-xc :c-type :rest-p ,@(if (= length 1) '(:length))))))
          (let ((offset-sym (symbolicate name "-" slot-name
                                         (if rest-p "-OFFSET" "-SLOT"))))
            (constants
             `(progn (defconstant ,offset-sym ,offset)
                     (setf (info :variable :kind ',offset-sym) :constant)))
            (when special
              (specials `(progn
                           (defvar ,special)
                           (setf (info :variable :always-bound ',special)
                                 :always-bound)))))
          (when ref-trans
            (when ref-known-p
              (forms `(defknown ,ref-trans (,type) ,slot-type ,ref-known)))
            (forms `(def-reffer ',ref-trans ,offset ,lowtag)))
          (when set-trans
            (when set-known-p
              (forms `(defknown ,set-trans
                                ,(if (listp set-trans)
                                     (list slot-type type)
                                     (list type slot-type))
                                ,slot-type
                        ,set-known)))
            (forms `(def-setter ',set-trans ,offset ,lowtag)))
          (when cas-trans
            (when rest-p
              (error ":REST-P and :CAS-TRANS incompatible."))
            (forms
             `(progn
                (defknown ,cas-trans (,type ,slot-type ,slot-type)
                    ,slot-type ())
                #+compare-and-swap-vops
                (def-casser ',cas-trans ,offset ,lowtag))))
          (when init
            (inits (cons init offset)))
          (when rest-p
            (setf variable-length-p t))
          (incf offset length)))
      (unless variable-length-p
        (constants `(defconstant ,size ,offset)))
      (when alloc-trans
        (forms `(def-alloc ',alloc-trans ,offset
                  ,(if variable-length-p :var-alloc :fixed-alloc)
                  ,widetag
                  ,lowtag ',(inits))))
      `(progn
         (setf (info :type :source-location ',name) (source-location))
         (!%define-primitive-object
            (make-primitive-object :name ',name
                                   :widetag ',widetag
                                   :lowtag ',lowtag
                                   :slots ,(coerce (slots) 'vector)
                                   :length ,offset
                                   :variable-length-p ,variable-length-p))
         ,@(constants)
         ,@(specials)
         (setf *!late-primitive-object-forms*
               (append *!late-primitive-object-forms*
                       ',(forms)))))))

;;; We want small SC-NUMBERs for SCs whose numbers are frequently
;;; embedded into machine code. We therefore fix the numbers for the
;;; four (i.e two bits) most frequently embedded SCs (empirically
;;; determined) and assign the rest sequentially.
(defmacro !define-storage-classes (&rest classes)
  (let* ((fixed-numbers '((descriptor-reg . 0)
                          (any-reg        . 1)
                          (signed-reg     . 2)
                          (constant       . 3)))
         (index (length fixed-numbers)))
    (flet ((process-class (class-spec)
             (destructuring-bind (sc-name sb-name &rest args) class-spec
               (let* ((sc-number (or (cdr (assoc sc-name fixed-numbers))
                                     (1- (incf index))))
                      (constant-name (symbolicate sc-name "-SC-NUMBER")))
                 `((!define-storage-class ,sc-name ,sc-number
                     ,sb-name ,@args)
                   (defconstant ,constant-name ,sc-number))))))
      `(progn ,@(mapcan #'process-class classes)))))

;;;; some general constant definitions

;;; The maximum number of storage classes and offsets within a given
;;; storage class. Applies to all backends.
(defconstant sc-number-limit 62)
(defconstant sc-number-bits (integer-length (1- sc-number-limit)))
(deftype sb-c:sc-number () `(integer 0 (,sc-number-limit)))

(defconstant sc-offset-limit (ash 1 21))
(defconstant sc-offset-bits (integer-length (1- sc-offset-limit)))
(deftype sc-offset () `(integer 0 (,sc-offset-limit)))

(defconstant finite-sc-offset-limit
  #-(or sparc) 32
  #+(or sparc) 64)
(defconstant finite-sc-offset-bits
  (integer-length (1- finite-sc-offset-limit)))
(deftype finite-sc-offset () `(integer 0 (,finite-sc-offset-limit)))
(deftype finite-sc-offset-map () `(unsigned-byte ,finite-sc-offset-limit))

;;;; stuff for defining reffers and setters

(in-package "SB-C")

(defun def-reffer (name offset lowtag)
  (let ((fun-info (fun-info-or-lose name)))
    (setf (fun-info-ir2-convert fun-info)
          (lambda (node block)
            (ir2-convert-reffer node block name offset lowtag))))
  name)

(defun def-setter (name offset lowtag)
  (let ((fun-info (fun-info-or-lose name)))
    (setf (fun-info-ir2-convert fun-info)
          (if (listp name)
              (lambda (node block)
                (ir2-convert-setfer node block name offset lowtag))
              (lambda (node block)
                (ir2-convert-setter node block name offset lowtag)))))
  name)

(defun def-alloc (name words allocation-style header lowtag inits)
  (let ((info (fun-info-or-lose name)))
    (setf (fun-info-ir2-convert info)
          (ecase allocation-style
            (:var-alloc
             (lambda (node block)
                (ir2-convert-variable-allocation node block name words header
                                                 lowtag inits)))
            (:fixed-alloc
             (lambda (node block)
               (ir2-convert-fixed-allocation node block name words header
                                             lowtag inits)))
            (:structure-alloc
             (lambda (node block)
               (ir2-convert-structure-allocation node block name words header
                                                 lowtag inits))))))
  name)

#+compare-and-swap-vops ; same as IR2-CONVERT-CASSER
(defun def-casser (name offset lowtag)
  (let ((fun-info (fun-info-or-lose name)))
    (setf (fun-info-ir2-convert fun-info)
          (lambda (node block)
            (ir2-convert-casser node block name offset lowtag)))))

;;; Modular functions
