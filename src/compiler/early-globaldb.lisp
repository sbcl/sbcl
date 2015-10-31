;;;; This file contains stuff that was split out from 'globaldb.lisp'
;;;; to satisfy build-order constraints.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;; Given the presence of docstrings and source locations,
;;; this logic arguably belongs to the runtime kernel, not the compiler,
;;; but such nuance isn't hugely important.
(in-package "SB!C")

(declaim (ftype (function (t t t) (values t t &optional)) info)
         (ftype (function (t t t) (values t &optional)) clear-info)
         (ftype (function (t t t t) (values t &optional)) (setf info)))

;;; (:FUNCTION :TYPE) information is extracted through a wrapper.
;;; The globaldb representation is not necessarily literally a CTYPE.
#-sb-xc-host
(declaim (ftype (function (t) (values ctype boolean &optional))
                proclaimed-ftype))

;;; At run time, we represent the type of a piece of INFO in the globaldb
;;; by a small integer between 1 and 63.  [0 is reserved for internal use.]
;;; CLISP, and maybe others, need EVAL-WHEN because without it, the constant
;;; is not seen by the "#." expression a few lines down.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant info-number-bits 6))
(def!type info-number () `(unsigned-byte ,info-number-bits))

;;; A map from info-number to its META-INFO object.
;;; The reverse mapping is obtained by reading the META-INFO.
(declaim (type (simple-vector #.(ash 1 info-number-bits)) *info-types*))
(!defglobal *info-types*
            (make-array (ash 1 info-number-bits) :initial-element nil))

(defstruct (meta-info
            (:constructor
             !make-meta-info (number category kind type-spec
                              type-checker validate-function default))
            (:copier nil))
  ;; a number that uniquely identifies this object
  (number nil :type info-number :read-only t)
  ;; 2-part key to this piece of metainfo
  (category nil :type keyword :read-only t)
  (kind nil :type keyword :read-only t)
  ;; a type specifier which info of this type must satisfy
  (type-spec nil :type t :read-only t)
  ;; Two functions called by (SETF INFO) before calling SET-INFO-VALUE.
  ;; 1. A function that type-checks its argument and returns it,
  ;;    or signals an error.
  ;;    Some Lisps trip over their shoelaces trying to assert that
  ;;    a function is (function (t) t). Our code is fine though.
  (type-checker nil :type #+sb-xc-host function #-sb-xc-host (sfunction (t) t)
                :read-only t)
  ;; 2. a function of two arguments, a name and new-value, which performs
  ;;    any other checks and/or side-effects including signaling an error.
  (validate-function nil :type (or function null) :read-only t)
  ;; If FUNCTIONP, then a function called when there is no information of
  ;; this type. If not FUNCTIONP, then any object serving as a default.
  (default nil))

(declaim (freeze-type meta-info))

(defconstant +info-metainfo-type-num+ 0)

;; Refer to info-vector.lisp for the meaning of this constant.
(defconstant +no-auxilliary-key+ 0)

;;; SYMBOL-INFO is a primitive object accessor defined in 'objdef.lisp'
;;; But in the host Lisp, there is no such thing as a symbol-info slot.
;;; Instead, symbol-info is kept in the host symbol's plist.
#+sb-xc-host
(defmacro symbol-info-vector (symbol) `(get ,symbol :sb-xc-globaldb-info))

;; Perform the equivalent of (GET-INFO-VALUE KIND +INFO-METAINFO-TYPE-NUM+)
;; but skipping the defaulting logic.
;; Return zero or more META-INFOs that match on KIND, which is usually
;; - though not always - a unique identifier for the (:TYPE :KIND) pair.
;; Note that bypassing of defaults is critical for bootstrapping,
;; since INFO is used to retrieve its own META-INFO at system-build time.
(defmacro !get-meta-infos (kind)
  `(let* ((info-vector (symbol-info-vector ,kind))
          (index (if info-vector
                     (packed-info-value-index info-vector +no-auxilliary-key+
                                              +info-metainfo-type-num+))))
     (if index (svref info-vector index))))

;; (UNSIGNED-BYTE 16) is an arbitrarily generous limit on the number of
;; cells in an info-vector. Most vectors have a fewer than a handful of things,
;; and performance would need to be re-thought if more than about a dozen
;; cells were in use. (It would want to become hash-based probably)
(declaim (ftype (function (simple-vector (or (eql 0) symbol) info-number)
                          (or null (unsigned-byte 16)))
                packed-info-value-index))

;; Return the META-INFO object for CATEGORY and KIND, signaling an error
;; if not found and ERRORP is non-nil. Note that the two-level logical hierarchy
;; of CATEGORY + KIND is physically grouped by KIND first, then CATEGORY.
;; e.g. Searching for (:SETF :EXPANDER) searches for :EXPANDER and finds
;;   (#<:CAS :EXPANDER, 44> #<:SETF :EXPANDER, 43> #<:TYPE :EXPANDER, 25>)
;; from which one is picked. This is slightly faster than searching first by
;; CATEGORY, because in the case of :FUNCTION there would be ~11 things to sift
;; through, whereas typically no more than 3 or 4 items have the same KIND.
;;
(defun meta-info (category kind &optional (errorp t))
  (or (let ((metadata (!get-meta-infos kind)))
        (cond ((listp metadata) ; conveniently handles NIL
               (dolist (info metadata nil) ; FIND is slower :-(
                 (when (eq (meta-info-category (truly-the meta-info info))
                           category)
                   (return info))))
              ((eq (meta-info-category (truly-the meta-info metadata)) category)
               metadata)))
      ;; !GET-META-INFOS enforces that KIND is a symbol, therefore
      ;; if a metaobject was found, CATEGORY was necessarily a symbol too.
      ;; Otherwise, if the caller wants no error to be signaled on missing info,
      ;; we must nevertheless enforce that CATEGORY was actually a symbol.
      (if errorp
          (error "(~S ~S) is not a defined info type." category kind)
          (progn (the symbol category) nil)))) ; THE is for type-safety

;;; Compiler macros for INFO functions.
;;;
;;; These are defined ASAP so that when building the cross-compiler, all calls
;;; occurring after compilation of "globaldb" (for known constant meta-info)
;;; are transformed; and when executing the cross-compiler, *all* inlineable
;;; calls for known constant meta-info are transformed;
;;; and when running target code, calls with legal constants for the first two
;;; arguments are transformed.
(macrolet ((def (name lambda-list form)
             (assert (and (member 'category lambda-list)
                          (member 'kind lambda-list)))
             `(define-compiler-macro ,name ,(append '(&whole .whole.) lambda-list)
                (if (and (keywordp category) (keywordp kind))
                    ;; In the target Lisp, it's a STYLE-WARNING if this macro
                    ;; defers to a full call to #'INFO.
                    ;; If the cross-compilation host, if any info-type is
                    ;; defined, then it's an error not to find the meta-info.
                    ;; If no info-types are defined, silently defer.
                    (let ((meta-info
                           (and #+sb-xc-host (find-if #'identity *info-types*)
                                (meta-info category kind #-sb-xc-host nil))))
                      (if meta-info
                          ,form
                          (progn
                            #-sb-xc-host
                            (style-warn "(INFO ~S ~S) will fail at runtime."
                                        category kind)
                            .whole.)))
                    .whole.))))

  (def info (category kind name)
    `(truly-the (values ,(meta-info-type-spec meta-info) boolean)
                (get-info-value ,name ,(meta-info-number meta-info))))

  (def (setf info) (new-value category kind name)
    (let* (#+sb-xc-host (sb!xc:*gensym-counter* sb!xc:*gensym-counter*)
           (tin (meta-info-number meta-info)) ; info-type id number
           (type-spec (meta-info-type-spec meta-info))
           (new (make-symbol "NEW"))
           (check
            (when (meta-info-validate-function meta-info)
              ;; is (or ... null), but non-null at macroexpansion time
              ;; implies non-null at runtime.
              `(truly-the function
                (meta-info-validate-function
                 (truly-the meta-info (svref *info-types* ,tin)))))))
      `(let ((,new ,new-value))
         ;; enforce type-correctness regardless of enclosing policy
         (let ((,new (locally (declare (optimize (safety 3)))
                       (the ,type-spec ,new))))
           ,@(when check
               `((funcall ,check ,name ,new)))
           (set-info-value ,name ,tin ,new)))))

  (def clear-info (category kind name)
    `(clear-info-values ,name '(,(meta-info-number meta-info)))))

;; Perform the approximate equivalent operations of retrieving
;; (INFO :CATEGORY :KIND NAME), but if no info is found, invoke CREATION-FORM
;; to produce an object that becomes the value for that piece of info, storing
;; and returning it. The entire sequence behaves atomically but with a proviso:
;; the creation form's result may be discarded, and another object returned
;; instead (presumably) from another thread's execution of the creation form.
;; If constructing the object has either non-trivial cost, or deleterious
;; side-effects from making and discarding its result, do NOT use this macro.
;; A mutex-guarded table would probably be more appropriate in such cases.
;;
(defmacro get-info-value-initializing (category kind name creation-form)
  (let ((proc (make-symbol "THUNK")))
    `(dx-flet ((,proc () ,creation-form))
       (%get-info-value-initializing
        ,(if (and (keywordp category) (keywordp kind))
             (meta-info-number (meta-info category kind))
             `(meta-info-number (meta-info ,category ,kind)))
        ,name #',proc))))
