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

(in-package "SB!VM")

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

(def!struct (prim-object-slot
             (:constructor make-slot (name docs rest-p offset special options))
             (:make-load-form-fun just-dump-it-normally)
             (:conc-name slot-))
  (name nil :type symbol :read-only t)
  (docs nil :type (or null simple-string) :read-only t)
  (rest-p nil :type (member t nil) :read-only t)
  (offset 0 :type fixnum :read-only t)
  (options nil :type list :read-only t)
  ;; On some targets (e.g. x86-64) slots of the thread structure are
  ;; referenced as special variables, this slot holds the name of that variable.
  (special nil :type symbol :read-only t))

(def!struct (primitive-object (:make-load-form-fun just-dump-it-normally))
  (name nil :type symbol :read-only t)
  (widetag nil :type symbol :read-only t)
  (lowtag nil :type symbol :read-only t)
  (options nil :type list :read-only t)
  (slots nil :type list :read-only t)
  (size 0 :type fixnum :read-only t)
  (variable-length-p nil :type (member t nil) :read-only t))

(declaim (freeze-type prim-object-slot primitive-object))
(defvar *primitive-objects* nil)

(defun !%define-primitive-object (primobj)
  (let ((name (primitive-object-name primobj)))
    (setf *primitive-objects*
          (cons primobj
                (remove name *primitive-objects*
                        :key #'primitive-object-name :test #'eq)))
    name))

(defvar *!late-primitive-object-forms* nil)

(defmacro !define-primitive-object
          ((name &key lowtag widetag alloc-trans (type t))
           &rest slot-specs)
  (collect ((slots) (specials) (constants) (forms) (inits))
    (let ((offset (if widetag 1 0))
          (variable-length-p nil))
      (dolist (spec slot-specs)
        (when variable-length-p
          (error "No more slots can follow a :rest-p slot."))
        (destructuring-bind
            (slot-name &rest options
                       &key docs rest-p (length (if rest-p 0 1))
                       ((:type slot-type) t) init
                       (ref-known nil ref-known-p) ref-trans
                       (set-known nil set-known-p) set-trans
                       cas-trans
                       special
                       pointer
                       &allow-other-keys)
            (if (atom spec) (list spec) spec)
          #!-alpha
          (declare (ignorable pointer))
          #!+alpha
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
          (slots (make-slot slot-name docs rest-p offset special
                            (remove-keywords options
                                             '(:docs :rest-p :length))))
          (let ((offset-sym (symbolicate name "-" slot-name
                                         (if rest-p "-OFFSET" "-SLOT"))))
            (constants `(def!constant ,offset-sym ,offset
                          ,@(when docs (list docs))))
            (when special
              (specials `(defvar ,special))))
          (when ref-trans
            (when ref-known-p
              (forms `(defknown ,ref-trans (,type) ,slot-type ,ref-known)))
            (forms `(def-reffer ,ref-trans ,offset ,lowtag)))
          (when set-trans
            (when set-known-p
              (forms `(defknown ,set-trans
                                ,(if (listp set-trans)
                                     (list slot-type type)
                                     (list type slot-type))
                                ,slot-type
                        ,set-known)))
            (forms `(def-setter ,set-trans ,offset ,lowtag)))
          (when cas-trans
            (when rest-p
              (error ":REST-P and :CAS-TRANS incompatible."))
            (forms
             `(progn
                (defknown ,cas-trans (,type ,slot-type ,slot-type)
                    ,slot-type ())
                #!+compare-and-swap-vops
                (def-casser ,cas-trans ,offset ,lowtag))))
          (when init
            (inits (cons init offset)))
          (when rest-p
            (setf variable-length-p t))
          (incf offset length)))
      (unless variable-length-p
        (constants `(def!constant ,(symbolicate name "-SIZE") ,offset)))
      (when alloc-trans
        (forms `(def-alloc ,alloc-trans ,offset
                  ,(if variable-length-p :var-alloc :fixed-alloc)
                  ,widetag
                  ,lowtag ',(inits))))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (setf (info :type :source-location ',name) (source-location))
           (!%define-primitive-object
            ',(make-primitive-object :name name
                                     :widetag widetag
                                     :lowtag lowtag
                                     :slots (slots)
                                     :size offset
                                     :variable-length-p variable-length-p))
           ,@(constants)
           ,@(specials))
         (setf *!late-primitive-object-forms*
               (append *!late-primitive-object-forms*
                       ',(forms)))))))

;;;; stuff for defining reffers and setters

(in-package "SB!C")

(defmacro def-reffer (name offset lowtag)
  `(%def-reffer ',name ,offset ,lowtag))
(defmacro def-setter (name offset lowtag)
  `(%def-setter ',name ,offset ,lowtag))
(defmacro def-alloc (name words alloc-style header lowtag inits)
  `(%def-alloc ',name ,words ,alloc-style ,header ,lowtag ,inits))
#!+compare-and-swap-vops
(defmacro def-casser (name offset lowtag)
  `(%def-casser ',name ,offset ,lowtag))
;;; KLUDGE: The %DEF-FOO functions used to implement the macros here
;;; are defined later in another file, since they use structure slot
;;; setters defined later, and we can't have physical forward
;;; references to structure slot setters because ANSI in its wisdom
;;; allows the xc host CL to implement structure slot setters as SETF
;;; expanders instead of SETF functions. -- WHN 2002-02-09

;;;; some general constant definitions

;;; FIXME: SC-NUMBER-LIMIT should probably be exported from SB!C
;;; or SB!VM so that we don't need to do this extra IN-PACKAGE.
(in-package "SB!C")

;;; the maximum number of SCs in any implementation
(def!constant sc-number-limit 62)

;;; Modular functions

;;; For a documentation, see CUT-TO-WIDTH.

(defstruct modular-class
  ;; hash: name -> { :GOOD | optimizer | ({modular-fun-info}*)}
  (funs (make-hash-table :test 'eq))
  ;; hash: modular-variant -> (prototype width)
  ;;
  ;; FIXME: Reimplement with generic function names of kind
  ;; (MODULAR-VERSION prototype width)
  (versions (make-hash-table :test 'eq))
  ;; list of increasing widths + signedps
  (widths nil))
(defvar *untagged-unsigned-modular-class* (make-modular-class))
(defvar *untagged-signed-modular-class* (make-modular-class))
(defvar *tagged-modular-class* (make-modular-class))
(defun find-modular-class (kind signedp)
  (ecase kind
    (:untagged
     (ecase signedp
       ((nil) *untagged-unsigned-modular-class*)
       ((t) *untagged-signed-modular-class*)))
    (:tagged
     (aver signedp)
     *tagged-modular-class*)))

(defstruct modular-fun-info
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

(defmacro define-modular-fun (name lambda-list prototype kind signedp width)
  (check-type name symbol)
  (check-type prototype symbol)
  (check-type kind (member :untagged :tagged))
  (check-type width unsigned-byte)
  (dolist (arg lambda-list)
    (when (member arg sb!xc:lambda-list-keywords)
      (error "Lambda list keyword ~S is not supported for ~
              modular function lambda lists." arg)))
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
  (check-type name symbol)
  (check-type kind (member :untagged :tagged))
  `(%define-good-modular-fun ',name ',kind ',signedp))

(defmacro define-modular-fun-optimizer
    (name ((&rest lambda-list) kind signedp &key (width (gensym "WIDTH")))
     &body body)
  (check-type name symbol)
  (check-type kind (member :untagged :tagged))
  (dolist (arg lambda-list)
    (when (member arg sb!xc:lambda-list-keywords)
      (error "Lambda list keyword ~S is not supported for ~
              modular function lambda lists." arg)))
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
