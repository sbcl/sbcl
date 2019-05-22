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

(defstruct (prim-object-slot
             (:constructor make-slot (name rest-p offset special options))
             (:copier nil)
             (:conc-name slot-))
  (name nil :type symbol :read-only t)
  (rest-p nil :type (member t nil) :read-only t)
  (offset 0 :type fixnum :read-only t)
  (options nil :type list :read-only t)
  ;; On some targets (e.g. x86-64) slots of the thread structure are
  ;; referenced as special variables, this slot holds the name of that variable.
  (special nil :type symbol :read-only t))

(defstruct (primitive-object (:copier nil))
  (name nil :type symbol :read-only t)
  (widetag nil :type symbol :read-only t)
  (lowtag nil :type symbol :read-only t)
  (options nil :type list :read-only t)
  (slots nil :type list :read-only t)
  (length 0 :type fixnum :read-only t)
  (variable-length-p nil :type (member t nil) :read-only t))

(declaim (freeze-type prim-object-slot primitive-object))

(define-load-time-global *primitive-objects* nil)

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
          #-alpha
          (declare (ignorable pointer))
          #+alpha
          (when pointer
            ;; Pointer values on ALPHA are 64 bits wide, and
            ;; double-word aligned.  We may also wish to have such a
            ;; mode for other 64-bit hardware outside of any defined
            ;; 32-on-64 ABI (which would presumably have 32-bit
            ;; pointers in the first place, obviating the alignment
            ;; and size requirements).
            (unless rest-p
              (setf length 2))
            (when (oddp offset)
              (incf offset)))
          (slots `(make-slot ',slot-name ,rest-p ,offset ',special
                             ',(remove-keywords options '(:rest-p :length))))
          (let ((offset-sym (symbolicate name "-" slot-name
                                         (if rest-p "-OFFSET" "-SLOT"))))
            (constants `(defconstant ,offset-sym ,offset))
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
                                   :slots (list ,@(slots))
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
(def!type sb-c::sc-number () `(integer 0 (,sc-number-limit)))

(defconstant sc-offset-limit (ash 1 21))
(defconstant sc-offset-bits (integer-length (1- sc-offset-limit)))
(deftype sc-offset () `(integer 0 (,sc-offset-limit)))

(defconstant finite-sc-offset-limit
  #-(or sparc alpha hppa) 32
  #+(or sparc alpha hppa) 64)
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

;;; For a documentation, see CUT-TO-WIDTH.

(defstruct (modular-class (:copier nil))
  ;; hash: name -> { :GOOD | optimizer | ({modular-fun-info}*)}
  (funs (make-hash-table :test 'eq))
  ;; hash: modular-variant -> (prototype width)
  ;;
  ;; FIXME: Reimplement with generic function names of kind
  ;; (MODULAR-VERSION prototype width)
  (versions (make-hash-table :test 'eq))
  ;; list of increasing widths + signedps
  (widths nil))
(define-load-time-global *untagged-unsigned-modular-class* (make-modular-class))
(define-load-time-global *untagged-signed-modular-class* (make-modular-class))
(define-load-time-global *tagged-modular-class* (make-modular-class))
(defun find-modular-class (kind signedp)
  (ecase kind
    (:untagged
     (ecase signedp
       ((nil) *untagged-unsigned-modular-class*)
       ((t) *untagged-signed-modular-class*)))
    (:tagged
     (aver signedp)
     *tagged-modular-class*)))

(defstruct (modular-fun-info (:copier nil))
  (name (missing-arg) :type symbol)
  (width (missing-arg) :type (integer 0))
  (signedp (missing-arg) :type boolean)
  (lambda-list (missing-arg) :type list)
  (prototype (missing-arg) :type symbol))

(defun find-modular-version (fun-name kind signedp width)
  (let ((infos (gethash fun-name (modular-class-funs (find-modular-class kind signedp)))))
    (if (listp infos)
        (find-if (lambda (mfi)
                   (aver (eq (modular-fun-info-signedp mfi) signedp))
                   (>= (modular-fun-info-width mfi) width))
                 infos)
        infos)))

;;; Return (VALUES prototype-name width)
(defun modular-version-info (name kind signedp)
  (values-list (gethash name (modular-class-versions (find-modular-class kind signedp)))))

(defun %define-modular-fun (name lambda-list prototype kind signedp width)
  (let* ((class (find-modular-class kind signedp))
         (funs (modular-class-funs class))
         (versions (modular-class-versions class))
         (infos (the list (gethash prototype funs)))
         (info (find-if (lambda (mfi)
                          (and (eq (modular-fun-info-signedp mfi) signedp)
                               (= (modular-fun-info-width mfi) width)))
                        infos)))
    (if info
        (unless (and (eq name (modular-fun-info-name info))
                     (= (length lambda-list)
                        (length (modular-fun-info-lambda-list info))))
          (setf (modular-fun-info-name info) name)
          (style-warn "Redefining modular version ~S of ~S for ~
                       ~:[un~;~]signed width ~S."
                      name prototype signedp width))
        (setf (gethash prototype funs)
              (merge 'list
                     (list (make-modular-fun-info :name name
                                                  :width width
                                                  :signedp signedp
                                                  :lambda-list lambda-list
                                                  :prototype prototype))
                     infos
                     #'< :key #'modular-fun-info-width)
              (gethash name versions)
              (list prototype width)))
    (setf (modular-class-widths class)
          (merge 'list (list (cons width signedp)) (modular-class-widths class)
                 #'< :key #'car))))

(defun %check-modular-fun-macro-arguments
    (name kind &optional (lambda-list nil lambda-list-p))
  (check-type name symbol)
  (check-type kind (member :untagged :tagged))
  (when lambda-list-p
    (dolist (arg lambda-list)
      (when (member arg sb-xc:lambda-list-keywords)
        (error "Lambda list keyword ~S is not supported for modular ~
                function lambda lists." arg)))))

(defmacro define-modular-fun (name lambda-list prototype kind signedp width)
  (%check-modular-fun-macro-arguments name kind lambda-list)
  (check-type prototype symbol)
  (check-type width unsigned-byte)
  `(progn
     (%define-modular-fun ',name ',lambda-list ',prototype ',kind ',signedp ,width)
     (defknown ,name ,(mapcar (constantly 'integer) lambda-list)
               (,(ecase signedp
                   ((nil) 'unsigned-byte)
                   ((t) 'signed-byte))
                 ,width)
               (foldable flushable movable)
               :derive-type (make-modular-fun-type-deriver
                             ',prototype ',kind ,width ',signedp))))

(defun %define-good-modular-fun (name kind signedp)
  (setf (gethash name (modular-class-funs (find-modular-class kind signedp))) :good)
  name)

(defmacro define-good-modular-fun (name kind signedp)
  (%check-modular-fun-macro-arguments name kind)
  `(%define-good-modular-fun ',name ',kind ',signedp))

(defmacro define-modular-fun-optimizer
    (name ((&rest lambda-list) kind signedp &key (width (gensym "WIDTH")))
     &body body)
  (%check-modular-fun-macro-arguments name kind lambda-list)
  (with-unique-names (call args)
    `(setf (gethash ',name (modular-class-funs (find-modular-class ',kind ',signedp)))
           (lambda (,call ,width)
             (declare (type basic-combination ,call)
                      (type (integer 0) ,width))
             (let ((,args (basic-combination-args ,call)))
               (when (= (length ,args) ,(length lambda-list))
                 (destructuring-bind ,lambda-list ,args
                   (declare (type lvar ,@lambda-list))
                   ,@body)))))))
