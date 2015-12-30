;;;; machine-independent disassembler

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!DISASSEM")

;;; types and defaults

(def!constant label-column-width 7)

(deftype text-width () '(integer 0 1000))
(deftype alignment () '(integer 0 64))
(deftype offset () '(signed-byte 24))
(deftype address () '(unsigned-byte #.sb!vm:n-word-bits))
(deftype disassem-length () '(unsigned-byte 24))
(deftype column () '(integer 0 1000))

(def!constant max-filtered-value-index 32)
(deftype filtered-value-index ()
  `(integer 0 (,max-filtered-value-index)))
(deftype filtered-value-vector ()
  `(simple-array t (,max-filtered-value-index)))

;;;; disassembly parameters

;; With a few tweaks, you can use a running SBCL as a cross-assembler
;; and disassembler for other supported backends,
;; if that backend has been converted to use a distinct ASM package.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter sb!assem::*backend-instruction-set-package*
    (find-package #.(sb-cold::backend-asm-package-name))))

(defvar *disassem-inst-space* nil)

;;; minimum alignment of instructions, in bytes
(defvar *disassem-inst-alignment-bytes* sb!vm:n-word-bytes)
(declaim (type alignment *disassem-inst-alignment-bytes*))

;; How many columns of output to allow for the address preceding each line.
;; If NIL, use the minimum possible width for the disassembly range.
;; If 0, do not print addresses.
(defvar *disassem-location-column-width* nil)
(declaim (type (or null text-width) *disassem-location-column-width*))

;;; the width of the column in which instruction-names are printed. A
;;; value of zero gives the effect of not aligning the arguments at
;;; all.
(defvar *disassem-opcode-column-width* 0)
(declaim (type text-width *disassem-opcode-column-width*))

;;; the width of the column in which instruction-bytes are printed. A
;;; value of zero disables the printing of instruction bytes.
(defvar *disassem-inst-column-width* 16
  #!+sb-doc
  "The width of instruction bytes.")
(declaim (type text-width *disassem-inst-column-width*))

(defvar *disassem-note-column* (+ 45 *disassem-inst-column-width*)
  #!+sb-doc
  "The column in which end-of-line comments for notes are started.")

;;;; A DCHUNK contains the bits we look at to decode an
;;;; instruction.
;;;; I tried to keep this abstract so that if using integers > the machine
;;;; word size conses too much, it can be changed to use bit-vectors or
;;;; something.
;;;;
;;;; KLUDGE: It's not clear that using bit-vectors would be any more efficient.
;;;; Perhaps the abstraction could go away. -- WHN 19991124

#!-sb-fluid
(declaim (inline dchunk-or dchunk-and dchunk-clear dchunk-not
                 dchunk-make-mask dchunk-make-field
                 sap-ref-dchunk
                 dchunk-extract
                 dchunk=
                 dchunk-count-bits))

(def!constant dchunk-bits #.sb!vm:n-word-bits)

(deftype dchunk ()
  `(unsigned-byte ,dchunk-bits))
(deftype dchunk-index ()
  `(integer 0 ,dchunk-bits))

(def!constant dchunk-zero 0)
(def!constant dchunk-one #.(1- (expt 2 sb!vm:n-word-bits)))

(defun dchunk-extract (from pos)
  (declare (type dchunk from))
  (the dchunk (ldb pos (the dchunk from))))

(defmacro dchunk-copy (x)
  `(the dchunk ,x))

(defun dchunk-or (to from)
  (declare (type dchunk to from))
  (the dchunk (logior to from)))
(defun dchunk-and (to from)
  (declare (type dchunk to from))
  (the dchunk (logand to from)))
(defun dchunk-clear (to from)
  (declare (type dchunk to from))
  (the dchunk (logandc2 to from)))
(defun dchunk-not (from)
  (declare (type dchunk from))
  (the dchunk (logand dchunk-one (lognot from))))

(defmacro dchunk-andf (to from)
  `(setf ,to (dchunk-and ,to ,from)))
(defmacro dchunk-orf (to from)
  `(setf ,to (dchunk-or ,to ,from)))
(defmacro dchunk-clearf (to from)
  `(setf ,to (dchunk-clear ,to ,from)))

(defun dchunk-make-mask (pos)
  (the dchunk (mask-field pos -1)))
(defun dchunk-make-field (pos value)
  (the dchunk (dpb value pos 0)))

(defmacro make-dchunk (value)
  `(the dchunk ,value))

#-sb-xc-host ;; FIXME: function belongs in 'target-disassem'
(defun sap-ref-dchunk (sap byte-offset byte-order)
  (declare (type sb!sys:system-area-pointer sap)
           (type offset byte-offset)
           (muffle-conditions compiler-note) ; returns possible bignum
           (optimize (speed 3) (safety 0)))
  (the dchunk
       (ecase dchunk-bits
         (32 (if (eq byte-order :big-endian)
                 (+ (ash (sb!sys:sap-ref-8 sap byte-offset) 24)
                    (ash (sb!sys:sap-ref-8 sap (+ 1 byte-offset)) 16)
                    (ash (sb!sys:sap-ref-8 sap (+ 2 byte-offset)) 8)
                    (sb!sys:sap-ref-8 sap (+ 3 byte-offset)))
                 (+ (sb!sys:sap-ref-8 sap byte-offset)
                    (ash (sb!sys:sap-ref-8 sap (+ 1 byte-offset)) 8)
                    (ash (sb!sys:sap-ref-8 sap (+ 2 byte-offset)) 16)
                    (ash (sb!sys:sap-ref-8 sap (+ 3 byte-offset)) 24))))
         (64 (if (eq byte-order :big-endian)
                 (+ (ash (sb!sys:sap-ref-8 sap byte-offset) 56)
                    (ash (sb!sys:sap-ref-8 sap (+ 1 byte-offset)) 48)
                    (ash (sb!sys:sap-ref-8 sap (+ 2 byte-offset)) 40)
                    (ash (sb!sys:sap-ref-8 sap (+ 3 byte-offset)) 32)
                    (ash (sb!sys:sap-ref-8 sap (+ 4 byte-offset)) 24)
                    (ash (sb!sys:sap-ref-8 sap (+ 5 byte-offset)) 16)
                    (ash (sb!sys:sap-ref-8 sap (+ 6 byte-offset)) 8)
                    (sb!sys:sap-ref-8 sap (+ 7 byte-offset)))
                 (+ (sb!sys:sap-ref-8 sap byte-offset)
                    (ash (sb!sys:sap-ref-8 sap (+ 1 byte-offset)) 8)
                    (ash (sb!sys:sap-ref-8 sap (+ 2 byte-offset)) 16)
                    (ash (sb!sys:sap-ref-8 sap (+ 3 byte-offset)) 24)
                    (ash (sb!sys:sap-ref-8 sap (+ 4 byte-offset)) 32)
                    (ash (sb!sys:sap-ref-8 sap (+ 5 byte-offset)) 40)
                    (ash (sb!sys:sap-ref-8 sap (+ 6 byte-offset)) 48)
                    (ash (sb!sys:sap-ref-8 sap (+ 7 byte-offset)) 56)))))))

(defun dchunk-corrected-extract (from pos unit-bits byte-order)
  (declare (type dchunk from))
  (if (eq byte-order :big-endian)
      (ldb (byte (byte-size pos)
                 (+ (byte-position pos) (- dchunk-bits unit-bits)))
           (the dchunk from))
      (ldb pos (the dchunk from))))

(defmacro dchunk-insertf (place pos value)
  `(setf ,place (the dchunk (dpb ,value ,pos (the dchunk,place)))))

(defun dchunk= (x y)
  (declare (type dchunk x y))
  (= x y))
(defmacro dchunk-zerop (x)
  `(dchunk= ,x dchunk-zero))

(defun dchunk-strict-superset-p (sup sub)
  (and (zerop (logandc2 sub sup))
       (not (zerop (logandc2 sup sub)))))

(defun dchunk-count-bits (x)
  (declare (type dchunk x))
  (logcount x))

(defstruct (instruction (:conc-name inst-)
                        (:constructor
                         make-instruction (name
                                           format-name
                                           print-name
                                           length
                                           mask id
                                           printer
                                           labeller prefilter control))
                        (:copier nil))
  (name nil :type (or symbol string))
  (format-name nil :type (or symbol string))

  (mask dchunk-zero :type dchunk)       ; bits in the inst that are constant
  (id dchunk-zero :type dchunk)         ; value of those constant bits

  (length 0 :type disassem-length)               ; in bytes

  (print-name nil :type symbol)

  ;; disassembly functions
  (prefilter nil :type (or null function))
  (labeller nil :type (or null function))
  (printer (missing-arg) :type (or null function))
  (control nil :type (or null function))

  ;; instructions that are the same as this instruction but with more
  ;; constraints
  (specializers nil :type list))
(def!method print-object ((inst instruction) stream)
  (print-unreadable-object (inst stream :type t :identity t)
    (format stream "~A(~A)" (inst-name inst) (inst-format-name inst))))

;;;; an instruction space holds all known machine instructions in a
;;;; form that can be easily searched

(defstruct (inst-space (:conc-name ispace-)
                       (:copier nil))
  (valid-mask dchunk-zero :type dchunk) ; applies to *children*
  (choices nil :type list))
(def!method print-object ((ispace inst-space) stream)
  (print-unreadable-object (ispace stream :type t :identity t)))

;;; now that we've defined the structure, we can declaim the type of
;;; the variable:
(declaim (type (or null inst-space) *disassem-inst-space*))

(defstruct (inst-space-choice (:conc-name ischoice-)
                              (:copier nil))
  (common-id dchunk-zero :type dchunk)  ; applies to *parent's* mask
  (subspace (missing-arg) :type (or inst-space instruction)))

;;;; only used during compilation of the instructions for a backend
;;;;
;;;; FIXME: If only used then, isn't there some way we could do
;;;; EVAL-WHEN tricks to keep this stuff from appearing in the target
;;;; system?

(defvar *disassem-inst-formats* (make-hash-table))
(defvar *disassem-arg-types* nil)

;;; The expander of DEFINE-INSTRUCTION has side-effects - if it finds a hit
;;; in the interned-sexpr table, then the compiler thinks that it does not
;;; have to emit a corresponding DEFUN. Depending on how you restart a
;;; compile, this is a lie, so just wipe the table clean.
(defmacro !begin-instruction-definitions ()
  '(eval-when (:compile-toplevel :execute)
     (makunbound (intern "INTERNED-SEXPRS"
                         sb!assem::*backend-instruction-set-package*))))

;;; FIXME: If GEN-ARG-FORMS used canonical temps vars, this would reduce to EQUAL.
(defun equal-mod-gensyms (a b)
  (aver (and (eq (car a) 'let*) (eq (car b) 'let*)))
  (let ((bindings-a (mapcar #'car (second a)))
        (bindings-b (mapcar #'car (second b))))
    (named-let recurse ((a a) (b b))
      (etypecase a
        (null (null b))
        (list (and (listp b) (recurse (car a) (car b)) (recurse (cdr a) (cdr b))))
        (symbol (or (eq a b)
                    (and (symbolp b)
                         ;; Care is needed, as printers use uninterned symbols
                         ;; in lieu of strings. It must be 1950 all over again.
                         (not (symbol-package a))
                         (not (symbol-package b))
                         (or (string= a b)
                             (let ((p (posq a bindings-a)))
                               (and p (eq (nth p bindings-b) b)))))))
        ((or number character) (eql a b))
        (vector (and (vectorp b) (every #'recurse a b)))))))

;;; Previously there were complicated checker functions which tried to attempt to
;;; decide, given two FUNSTATEs, whether all their args were similarly used,
;;; where "similarity" required that the prefilter and such be identical.
;;; Instead we can just look at two sexprs and decide whether they act the same,
;;; which is of course impossible in general; however, for this purpose,
;;; if sexprs are EQUAL disregarding variations in gensyms, then their code
;;; can be folded. If we miss (don't fold) things that act the same, it's ok.
;;; N.B.: This definition of equivalence is admissible because there can be
;;; no "interesting" non-null lexical environment. While it could be non-null,
;;; it can't matter, because our auto-generated code can't depend on the lexenv.
(defun generate-function (kind forms funstate skeleton)
  (let* ((package sb!assem::*backend-instruction-set-package*)
         (table-name (intern "INTERNED-SEXPRS" package))
         (table (if (boundp table-name)
                    (symbol-value table-name)
                    (set table-name
                         (list (list :printer) (list :prefilter) (list :labeller)))))
         (sub-table (assq kind table))
         (bindings (make-arg-temp-bindings funstate))
         (guts `(let* ,bindings ,@forms))
         (found (assoc guts (cdr sub-table) :test #'equal-mod-gensyms)))
    (if found
        (values (cdr found) nil)
        (let ((name (intern (concatenate 'string "INST-" (string kind) "-"
                                         (write-to-string (length sub-table)))
                            package)))
          (push (cons guts name) (cdr sub-table))
          (values name `(defun ,name ,@(subst guts :body (cdr skeleton))))))))

(defstruct (arg (:copier nil)
                (:predicate nil)
                (:constructor %make-arg (name))
                (:constructor standard-make-arg) ; only so #S readmacro works
                (:print-object
                 (lambda (self stream)
                   (if *print-readably*
                       (call-next-method)
                       (print-unreadable-object (self stream :type t)
                         (format stream
                                 "~A ~:[~;+~]~:S~@[=~S~]~@[ filt=~S~]~
~@[ lbl=~S~]~@[ prt=~S~]"
                                 (arg-name self)
                                 (arg-sign-extend-p self)
                                 (arg-fields self)
                                 (arg-value self)
                                 (arg-prefilter self)
                                 (arg-use-label self)
                                 (arg-printer self)))))))
  (name nil :type symbol)
  (fields nil :type list)

  (value nil :type (or list integer))
  (sign-extend-p nil :type (member t nil))

  ;; functions to use
  (printer nil)
  (prefilter nil)
  (use-label nil))

(defstruct (instruction-format (:conc-name format-)
                               (:constructor make-inst-format
                                             (name length default-printer args))
                               (:copier nil))
  (name nil)
  (args nil :type list)

  (length 0 :type disassem-length)               ; in bytes

  (default-printer nil :type list))

;;; A FUNSTATE holds the state of any arguments used in a disassembly
;;; function. It is a 2-level alist. The outer list maps each ARG to
;;; a list of styles in which that arg can be rendered.
;;; Each rendering is named by a keyword (the key to the inner alist),
;;; and is represented as a list of temp vars and values for them.
(defun make-funstate (args) (mapcar #'list args))

(defun arg-position (arg funstate)
  ;;; The THE form is to assert that ARG is found.
  (the filtered-value-index (position arg funstate :key #'car)))

(defun arg-or-lose (name funstate)
  (or (car (assoc name funstate :key #'arg-name :test #'eq))
      (pd-error "unknown argument ~S" name)))

;;;; Since we can't include some values in compiled output as they are
;;;; (notably functions), we sometimes use a VALSRC structure to keep
;;;; track of the source from which they were derived.

(defstruct (valsrc (:constructor %make-valsrc)
                   (:copier nil))
  (value nil)
  (source nil))

(defun make-valsrc (value source)
  (cond ((equal value source)
         source)
        ((and (listp value) (eq (car value) 'function))
         value)
        (t
         (%make-valsrc :value value :source source))))

;;; machinery to provide more meaningful error messages during compilation
(defvar *current-instruction-flavor* nil)
(defun pd-error (fmt &rest args)
  (if *current-instruction-flavor*
      (error "~@<in printer-definition for ~S(~S): ~3I~:_~?~:>"
             (car *current-instruction-flavor*)
             (cdr *current-instruction-flavor*)
             fmt args)
      (apply #'error fmt args)))

;;; FIXME:
;;;  1. This should become a utility in SB!INT.
;;;  2. Arrays and structures and maybe other things are
;;;     self-evaluating too.
(defun self-evaluating-p (x)
  (typecase x
    (null t)
    (keyword t)
    (symbol (eq x t))
    (cons nil)
    (t t)))

(defun maybe-quote (evalp form)
  (if (or evalp (self-evaluating-p form)) form `',form))

;;; Detect things that obviously don't need wrapping, like
;;; variable-refs and #'function.
(defun doesnt-need-wrapping-p (form)
  (or (symbolp form)
      (and (listp form)
           (eq (car form) 'function)
           (symbolp (cadr form)))))

(defun make-wrapper (form arg-name funargs prefix)
  (if (and (listp form)
           (eq (car form) 'function))
      ;; a function def
      (let ((wrapper-name (symbolicate prefix "-" arg-name "-WRAPPER"))
            (wrapper-args (make-gensym-list (length funargs))))
        (values `#',wrapper-name
                `(defun ,wrapper-name ,wrapper-args
                   (funcall ,form ,@wrapper-args))))
      ;; something else
      (let ((wrapper-name (symbolicate "*" prefix "-" arg-name "-WRAPPER*")))
        (values wrapper-name `(defparameter ,wrapper-name ,form)))))

(defun filter-overrides (overrides evalp)
  (mapcar (lambda (override)
            (list* (car override) (cadr override)
                   (munge-fun-refs (cddr override) evalp)))
          overrides))

(defparameter *arg-fun-params*
  '((:printer . (value stream dstate))
    (:use-label . (value dstate))
    (:prefilter . (value dstate))))

(defun munge-fun-refs (params evalp &optional wrap-defs-p (prefix ""))
  (let ((params (copy-list params)))
    (do ((tail params (cdr tail))
         (wrapper-defs nil))
        ((null tail)
         (values params (nreverse wrapper-defs)))
      (let ((fun-arg (assoc (car tail) *arg-fun-params*)))
        (when fun-arg
          (let* ((fun-form (cadr tail))
                 (quoted-fun-form `',fun-form))
            (when (and wrap-defs-p (not (doesnt-need-wrapping-p fun-form)))
              (multiple-value-bind (access-form wrapper-def-form)
                  (make-wrapper fun-form (car fun-arg) (cdr fun-arg) prefix)
                (setf quoted-fun-form `',access-form)
                (push wrapper-def-form wrapper-defs)))
            (if evalp
                (setf (cadr tail)
                      `(make-valsrc ,fun-form ,quoted-fun-form))
                (setf (cadr tail)
                      fun-form))))))))

(defun gen-args-def-form (overrides format-form &optional (evalp t))
  (let ((args-var (gensym)))
    `(let ((,args-var (copy-list (format-args ,format-form))))
       ,@(mapcar (lambda (override)
                   (update-args-form args-var
                                     `',(car override)
                                     (and (cdr override)
                                          (cons :value (cdr override)))
                                     evalp))
                 overrides)
       ,args-var)))

(defun gen-printer-def-forms-def-form (base-name
                                       def
                                       &optional
                                       (evalp t))
  (declare (type symbol base-name))
  (destructuring-bind
      (format-name
       (&rest field-defs)
       &optional (printer-form :default)
       &key ((:print-name print-name-form) `',base-name) control)
      def
    (let ((format-var (gensym))
          (field-defs (filter-overrides field-defs evalp)))
      `(let* ((*current-instruction-flavor* ',(cons base-name format-name))
              (,format-var (format-or-lose ',format-name))
              (args ,(gen-args-def-form field-defs format-var evalp)))
         (multiple-value-bind (printer-fun printer-defun)
             (find-printer-fun ,(if (eq printer-form :default)
                                     `(format-default-printer ,format-var)
                                     (maybe-quote evalp printer-form))
                               args)
           (multiple-value-bind (labeller-fun labeller-defun)
               (find-labeller-fun args)
             (multiple-value-bind (prefilter-fun prefilter-defun)
                 (find-prefilter-fun args)
               (multiple-value-bind (mask id)
                   (compute-mask-id args)
                 (values
                  `(make-instruction ',',base-name
                                     ',',format-name
                                     ,',print-name-form
                                     ,(format-length ,format-var)
                                     ,mask
                                     ,id
                                     ,(and printer-fun `#',printer-fun)
                                     ,(and labeller-fun `#',labeller-fun)
                                     ,(and prefilter-fun `#',prefilter-fun)
                                     ,',control)
                  `(progn
                     ,@(and printer-defun (list printer-defun))
                     ,@(and labeller-defun (list labeller-defun))
                     ,@(and prefilter-defun (list prefilter-defun))))
                 ))))))))

(defun update-args-form (var name-form descrip-forms evalp)
  `(setf ,var
         ,(if evalp
              `(modify-or-add-arg ,name-form ,var ,@descrip-forms)
              `(apply #'modify-or-add-arg ,name-form ,var ',descrip-forms))))

(defun format-or-lose (name)
  (or (gethash name *disassem-inst-formats*)
      (pd-error "unknown instruction format ~S" name)))

;;; FIXME: needed only at build-the-system time, not in running system
;;; and FIXME: better syntax would allow inheriting the length to avoid
;;; re-stating it needlessly in some derived formats. Perhaps:
;;; (DEFINE-INSTRUCTION-FORMAT NAME (:bits N [more-format-keys]*) &rest fields)
;;;
(defmacro define-instruction-format ((format-name length-in-bits
                                      &key default-printer include)
                                     &rest arg-specs)
  #!+sb-doc
  "DEFINE-INSTRUCTION-FORMAT (Name Length {Format-Key Value}*) Arg-Def*
  Define an instruction format NAME for the disassembler's use. LENGTH is
  the length of the format in bits.
  Possible FORMAT-KEYs:

  :INCLUDE other-format-name
      Inherit all arguments and properties of the given format. Any
      arguments defined in the current format definition will either modify
      the copy of an existing argument (keeping in the same order with
      respect to when prefilters are called), if it has the same name as
      one, or be added to the end.
  :DEFAULT-PRINTER printer-list
      Use the given PRINTER-LIST as a format to print any instructions of
      this format when they don't specify something else.

  Each ARG-DEF defines one argument in the format, and is of the form
    (Arg-Name {Arg-Key Value}*)

  Possible ARG-KEYs (the values are evaluated unless otherwise specified):

  :FIELDS byte-spec-list
      The argument takes values from these fields in the instruction. If
      the list is of length one, then the corresponding value is supplied by
      itself; otherwise it is a list of the values. The list may be NIL.
  :FIELD byte-spec
      The same as :FIELDS (list byte-spec).

  :VALUE value
      If the argument only has one field, this is the value it should have,
      otherwise it's a list of the values of the individual fields. This can
      be overridden in an instruction-definition or a format definition
      including this one by specifying another, or NIL to indicate that it's
      variable.

  :SIGN-EXTEND boolean
      If non-NIL, the raw value of this argument is sign-extended,
      immediately after being extracted from the instruction (before any
      prefilters are run, for instance). If the argument has multiple
      fields, they are all sign-extended.

  :TYPE arg-type-name
      Inherit any properties of the given argument type.

  :PREFILTER function
      A function which is called (along with all other prefilters, in the
      order that their arguments appear in the instruction-format) before
      any printing is done, to filter the raw value. Any uses of READ-SUFFIX
      must be done inside a prefilter.

  :PRINTER function-string-or-vector
      A function, string, or vector which is used to print this argument.

  :USE-LABEL
      If non-NIL, the value of this argument is used as an address, and if
      that address occurs inside the disassembled code, it is replaced by a
      label. If this is a function, it is called to filter the value."
  (let ((length-var (gensym)) ; are lengths ever non-constant? probably not.
        (inherited-args
         (if include
             (copy-list (format-args (format-or-lose include)))))
        added-args readers all-wrapper-defs)
    (dolist (arg-spec arg-specs)
      (let ((arg-name (car arg-spec)))
        (multiple-value-bind (props wrapper-defs)
            (munge-fun-refs (cdr arg-spec) t t
                            (symbolicate format-name '- arg-name))
          (setf all-wrapper-defs (nconc wrapper-defs all-wrapper-defs))
          (let ((reader (getf props :reader)))
            (when reader
              (setq readers (list* #!-sb-fluid `(declaim (inline ,reader))
                                   `(defun ,reader (dchunk dstate)
                                      (declare (ignorable dchunk dstate))
                                      (arg-access-macro ,arg-name ,format-name
                                                        dchunk dstate))
                                   readers))
              (remf props :reader))) ; ok because MUNGEing copied the plist
          (let ((cell (member arg-name inherited-args
                              :key (lambda (x)
                                     (arg-name (if (listp x) (second x) x))))))
            (cond ((not cell)
                   (push `(make-arg ',arg-name ,length-var ,@props)
                         added-args))
                  (props ; do nothing if no alterations
                   (rplaca cell
                           `(copy-arg ,(car cell) ,length-var ,@props))))))))
    `(progn
       ,@all-wrapper-defs
       (eval-when (:compile-toplevel :execute)
         (let ((,length-var ,length-in-bits))
           (setf (gethash ',format-name *disassem-inst-formats*)
                 (make-inst-format ',format-name (bits-to-bytes ,length-var)
                                   ,(maybe-quote t default-printer)
                                   (list ,@inherited-args
                                         ,@(nreverse added-args))))))
       ,@readers)))

(defun make-arg (name format-length-bits &rest properties)
  (apply #'modify-arg (%make-arg name) format-length-bits properties))

(defun copy-arg (arg format-length-bits &rest properties)
  (apply #'modify-arg (copy-structure arg) format-length-bits properties))

;;; FIXME: probably needed only at build-the-system time, not in
;;; final target system
(defun modify-or-add-arg (arg-name args &rest properties)
  (declare (dynamic-extent properties))
  (when (get-properties properties '(:field :fields))
    (error "~@<in arg ~S: ~3I~:_~
          can't specify fields except using DEFINE-INSTRUCTION-FORMAT~:>"
           arg-name))
  (let* ((cell (member arg-name args :key #'arg-name))
         (arg (if cell
                  (setf (car cell) (copy-structure (car cell)))
                  (let ((arg (%make-arg arg-name)))
                    (setf args (nconc args (list arg)))
                    arg))))
    (apply #'modify-arg arg nil properties)
    args))

(defun modify-arg (arg format-length
                   &key   (value nil value-p)
                          (type nil type-p)
                          (prefilter nil prefilter-p)
                          (printer nil printer-p)
                          (sign-extend nil sign-extend-p)
                          (use-label nil use-label-p)
                          (field nil field-p)
                          (fields nil fields-p))
  (when field-p
    (if fields-p
        (error ":FIELD and :FIELDS are mutually exclusive")
        (setf fields (list field) fields-p t)))
  (when type-p
    (set-arg-from-type arg type *disassem-arg-types*))
  (when value-p
    (setf (arg-value arg) value))
  (when prefilter-p
    (setf (arg-prefilter arg) prefilter))
  (when sign-extend-p
    (setf (arg-sign-extend-p arg) sign-extend))
  (when printer-p
    (setf (arg-printer arg) printer))
  (when use-label-p
    (setf (arg-use-label arg) use-label))
  (when fields-p
    (setf (arg-fields arg)
          (mapcar (lambda (bytespec)
                    (when (> (+ (byte-position bytespec) (byte-size bytespec))
                             format-length)
                      (error "~@<in arg ~S: ~3I~:_~
                                   The field ~S doesn't fit in an ~
                                   instruction-format ~W bits wide.~:>"
                             (arg-name arg) bytespec format-length))
                    (correct-dchunk-bytespec-for-endianness
                     bytespec format-length sb!c:*backend-byte-order*))
                  fields)))
  arg)

;; Generate a sexpr to extract ARG-NAME of FORMAT-NAME using CHUNK and DSTATE.
;; The first two arguments to this macro are not runtime-evaluated.
(defmacro arg-access-macro (arg-name format-name chunk dstate)
  (let* ((funstate (make-funstate (format-args (format-or-lose format-name))))
         (arg (arg-or-lose arg-name funstate))
         (arg-val-form (arg-value-form arg funstate :numeric)))
    `(flet ((local-filtered-value (offset)
              (declare (type filtered-value-index offset))
              (aref (dstate-filtered-values ,dstate) offset))
            (local-extract (bytespec)
              (dchunk-extract ,chunk bytespec)))
       (declare (ignorable #'local-filtered-value #'local-extract)
                (inline local-filtered-value local-extract))
       (let* ,(make-arg-temp-bindings funstate) ,arg-val-form))))

(defun arg-value-form (arg funstate
                       &optional
                       (rendering :final)
                       (allow-multiple-p (neq rendering :numeric)))
  (let ((forms (gen-arg-forms arg rendering funstate)))
    (when (and (not allow-multiple-p)
               (listp forms)
               (/= (length forms) 1))
      (pd-error "~S must not have multiple values." arg))
    (maybe-listify forms)))

(defun correct-dchunk-bytespec-for-endianness (bs unit-bits byte-order)
  (if (eq byte-order :big-endian)
      (byte (byte-size bs) (+ (byte-position bs) (- dchunk-bits unit-bits)))
      bs))

(defun make-arg-temp-bindings (funstate)
  (let ((bindings nil))
    ;; Prefilters have to be called in the correct order, so reverse FUNSTATE
    ;; because we're using PUSH in the inner loop.
    (dolist (arg-cell (reverse funstate) bindings)
      ;; These sublists are "backwards", so PUSH ends up being correct.
      (dolist (rendering (cdr arg-cell))
        (let* ((binding (cdr rendering))
               (vars (car binding))
               (vals (cdr binding)))
          (if (listp vars)
              (mapc (lambda (var val) (push `(,var ,val) bindings)) vars vals)
              (push `(,vars ,vals) bindings)))))))

;;; Return the form(s) that should be evaluated to render ARG in the chosen
;;; RENDERING style, which is one of :RAW, :SIGN-EXTENDED, :FILTERING,
;;; :FILTERED, :NUMERIC, and :FINAL. Each rendering depends on the preceding
;;; one, so asking for :FINAL will implicitly compute all renderings.
(defun gen-arg-forms (arg rendering funstate)
  (let* ((arg-cell (assq arg funstate))
         (rendering-temps (cdr (assq rendering (cdr arg-cell))))
         (vars (car rendering-temps))
         (forms (cdr rendering-temps)))
    (unless forms
      (multiple-value-bind (new-forms single-value-p)
          (%gen-arg-forms arg rendering funstate)
        (setq forms new-forms
              vars (cond ((or single-value-p (atom forms))
                          (if (symbolp forms) vars (gensym)))
                         ((every #'symbolp forms)
                          ;; just use the same as the forms
                          nil)
                         (t
                          (make-gensym-list (length forms)))))
        (push (list* rendering vars forms) (cdr arg-cell))))
    (or vars forms)))

(defun maybe-listify (forms)
  (cond ((atom forms)
         forms)
        ((/= (length forms) 1)
         `(list ,@forms))
        (t
         (car forms))))

(defun set-arg-from-type (arg type-name table)
  (let ((type-arg (find type-name table :key #'arg-name)))
    (when (null type-arg)
      (pd-error "unknown argument type: ~S" type-name))
    (setf (arg-printer arg) (arg-printer type-arg))
    (setf (arg-prefilter arg) (arg-prefilter type-arg))
    (setf (arg-sign-extend-p arg) (arg-sign-extend-p type-arg))
    (setf (arg-use-label arg) (arg-use-label type-arg))))

;;; DEFINE-ARG-TYPE Name {Key Value}*
;;;
;;; Define a disassembler argument type NAME (which can then be referenced in
;;; another argument definition using the :TYPE argument). &KEY args are:
;;;
;;;  :SIGN-EXTEND boolean
;;;     If non-NIL, the raw value of this argument is sign-extended.
;;;
;;;  :TYPE arg-type-name
;;;     Inherit any properties of given arg-type.
;;;
;;; :PREFILTER function
;;;     A function which is called (along with all other prefilters,
;;;     in the order that their arguments appear in the instruction-
;;;     format) before any printing is done, to filter the raw value.
;;;     Any uses of READ-SUFFIX must be done inside a prefilter.
;;;
;;; :PRINTER function-string-or-vector
;;;     A function, string, or vector which is used to print an argument of
;;;     this type.
;;;
;;; :USE-LABEL
;;;     If non-NIL, the value of an argument of this type is used as
;;;     an address, and if that address occurs inside the disassembled
;;;     code, it is replaced by a label. If this is a function, it is
;;;     called to filter the value.
(defmacro define-arg-type (name &rest args
                           &key sign-extend type prefilter printer use-label)
  (declare (ignore sign-extend type prefilter printer use-label))
  (multiple-value-bind (args wrapper-defs)
      (munge-fun-refs args t t name)
    `(progn
       ,@wrapper-defs
       (eval-when (:compile-toplevel :execute)
         (setq *disassem-arg-types*
               (delete ',name *disassem-arg-types* :key #'arg-name))
         (push (modify-arg (%make-arg ',name) nil ,@args) *disassem-arg-types*))
       ',name)))

(defun %gen-arg-forms (arg rendering funstate)
  (declare (type arg arg) (type list funstate))
  (ecase rendering
    (:raw ; just extract the bits
     (mapcar (lambda (bytespec)
               `(the (unsigned-byte ,(byte-size bytespec))
                     (local-extract ',bytespec)))
             (arg-fields arg)))
    (:sign-extended ; sign-extend, or not
     (let ((raw-forms (gen-arg-forms arg :raw funstate)))
       (if (and (arg-sign-extend-p arg) (listp raw-forms))
           (mapcar (lambda (form field)
                     `(the (signed-byte ,(byte-size field))
                           (sign-extend ,form ,(byte-size field))))
                   raw-forms
                   (arg-fields arg))
           raw-forms)))
    (:filtering ; pass the sign-extended arg to the filter function
     ;; The prefilter is not required to be side-effect-free -
     ;; e.g. it might touch DSTATE-CUR-OFFS - so it stores :FILTERING values
     ;; into :FILTERED values, which can be repeatedly accessed as needed.
     (let ((sign-extended-forms (gen-arg-forms arg :sign-extended funstate))
           (pf (arg-prefilter arg)))
       (if pf
           (values `(local-filter ,(maybe-listify sign-extended-forms)
                                  ,(source-form pf))
                   t)
           (values sign-extended-forms nil))))
    (:filtered ; extract from the prefiltered value vector
     (let ((pf (arg-prefilter arg)))
       (if pf
           (values `(local-filtered-value ,(arg-position arg funstate)) t)
           (gen-arg-forms arg :sign-extended funstate))))
    (:numeric ; pass the filtered value to the label adjuster, or not
     (let ((filtered-forms (gen-arg-forms arg :filtered funstate))
           (use-label (arg-use-label arg)))
       ;; use-label = T means that the prefiltered value is already an address,
       ;; otherwise non-nil means a function to call, and NIL means not a label.
       ;; So only the middle case needs to call ADJUST-LABEL.
       (if (and use-label (neq use-label t))
           `((adjust-label ,(maybe-listify filtered-forms)
                           ,(source-form use-label)))
           filtered-forms)))
    (:final ; if arg is not a label, return numeric value, otherwise a string
     (let ((numeric-forms (gen-arg-forms arg :numeric funstate))
           (use-label (arg-use-label arg)))
       (cond ((not use-label) numeric-forms)
             ((and (eq use-label t) (listp numeric-forms) (cdr numeric-forms))
              (pd-error "cannot label multi-field ~S without a labeller" arg))
             (t `((lookup-label ,(maybe-listify numeric-forms)))))))))

;;; Returns a version of THING suitable for including in an evaluable
;;; position in some form.
(defun source-form (thing)
  (cond ((valsrc-p thing)
         (valsrc-source thing))
        ((functionp thing)
         (pd-error
          "can't dump functions, so function ref form must be quoted: ~S"
          thing))
        ((self-evaluating-p thing)
         thing)
        ((eq (car thing) 'function)
         thing)
        (t
         `',thing)))

;;; Return anything but a VALSRC structure.
(defun value-or-source (thing)
  (if (valsrc-p thing)
      (valsrc-value thing)
      thing))

(defun find-printer-fun (printer-source args)
  (unless printer-source
    (return-from find-printer-fun (values nil nil)))
  (let ((source (preprocess-printer printer-source args))
        (funstate (make-funstate args)))
   (generate-function
    :printer
    (compile-printer-list source funstate)
    funstate
    '(lambda (chunk inst stream dstate)
       (declare (type dchunk chunk)
                (type instruction inst)
                (type stream stream)
                (type disassem-state dstate))
       (macrolet ((local-format-arg (arg fmt)
                    `(funcall (formatter ,fmt) stream ,arg)))
         (flet ((local-tab-to-arg-column ()
                  (tab (dstate-argument-column dstate) stream))
                (local-print-name ()
                  (princ (inst-print-name inst) stream))
                (local-write-char (ch)
                  (write-char ch stream))
                (local-princ (thing)
                  (princ thing stream))
                (local-princ16 (thing)
                  (princ16 thing stream))
                (local-call-arg-printer (arg printer)
                  (funcall printer arg stream dstate))
                (local-call-global-printer (fun)
                  (funcall fun chunk inst stream dstate))
                (local-filtered-value (offset)
                  (declare (type filtered-value-index offset))
                  (aref (dstate-filtered-values dstate) offset))
                (local-extract (bytespec)
                  (dchunk-extract chunk bytespec))
                (lookup-label (lab)
                  (or (gethash lab (dstate-label-hash dstate))
                      lab))
                (adjust-label (val adjust-fun)
                  (funcall adjust-fun val dstate)))
           (declare (ignorable #'local-tab-to-arg-column
                               #'local-print-name
                               #'local-princ #'local-princ16
                               #'local-write-char
                               #'local-call-arg-printer
                               #'local-call-global-printer
                               #'local-extract
                               #'local-filtered-value
                               #'lookup-label #'adjust-label)
                    (inline local-tab-to-arg-column
                            local-princ local-princ16
                            local-call-arg-printer local-call-global-printer
                            local-filtered-value local-extract
                            lookup-label adjust-label))
           :body))))))

(defun preprocess-test (subj form args)
  (multiple-value-bind (subj test)
      (if (and (consp form) (symbolp (car form)) (not (keywordp (car form))))
          (values (car form) (cdr form))
          (values subj form))
    (let ((key (if (consp test) (car test) test))
          (body (if (consp test) (cdr test) nil)))
      (case key
        (:constant
         (if (null body)
             ;; If no supplied constant values, just any constant is ok,
             ;; just see whether there's some constant value in the arg.
             (not
              (null
               (arg-value
                (or (find subj args :key #'arg-name)
                    (pd-error "unknown argument ~S" subj)))))
             ;; Otherwise, defer to run-time.
             form))
        ((:or :and :not)
         (sharing-cons
          form
          subj
          (sharing-cons
           test
           key
           (sharing-mapcar
            (lambda (sub-test)
              (preprocess-test subj sub-test args))
            body))))
        (t form)))))

(defun preprocess-conditionals (printer args)
  (if (atom printer)
      printer
      (case (car printer)
        (:unless
         (preprocess-conditionals
          `(:cond ((:not ,(nth 1 printer)) ,@(nthcdr 2 printer)))
          args))
        (:when
         (preprocess-conditionals `(:cond (,(cdr printer))) args))
        (:if
         (preprocess-conditionals
          `(:cond (,(nth 1 printer) ,(nth 2 printer))
                  (t ,(nth 3 printer)))
          args))
        (:cond
         (sharing-cons
          printer
          :cond
          (sharing-mapcar
           (lambda (clause)
             (let ((filtered-body
                    (sharing-mapcar
                     (lambda (sub-printer)
                       (preprocess-conditionals sub-printer args))
                     (cdr clause))))
               (sharing-cons
                clause
                (preprocess-test (find-first-field-name filtered-body)
                                 (car clause)
                                 args)
                filtered-body)))
           (cdr printer))))
        (quote printer)
        (t
         (sharing-mapcar
          (lambda (sub-printer)
            (preprocess-conditionals sub-printer args))
          printer)))))

;;; Return a version of the disassembly-template PRINTER with
;;; compile-time tests (e.g. :constant without a value), and any
;;; :CHOOSE operators resolved properly for the args ARGS.
;;;
;;; (:CHOOSE Sub*) simply returns the first Sub in which every field
;;; reference refers to a valid arg.
(defun preprocess-printer (printer args)
  (preprocess-conditionals (preprocess-chooses printer args) args))

;;; Return the first non-keyword symbol in a depth-first search of TREE.
(defun find-first-field-name (tree)
  (cond ((null tree)
         nil)
        ((and (symbolp tree) (not (keywordp tree)))
         tree)
        ((atom tree)
         nil)
        ((eq (car tree) 'quote)
         nil)
        (t
         (or (find-first-field-name (car tree))
             (find-first-field-name (cdr tree))))))

(defun preprocess-chooses (printer args)
  (cond ((atom printer)
         printer)
        ((eq (car printer) :choose)
         (pick-printer-choice (cdr printer) args))
        (t
         (sharing-mapcar (lambda (sub) (preprocess-chooses sub args))
                         printer))))

;;;; some simple functions that help avoid consing when we're just
;;;; recursively filtering things that usually don't change

(defun sharing-cons (old-cons car cdr)
  #!+sb-doc
  "If CAR is eq to the car of OLD-CONS and CDR is eq to the CDR, return
  OLD-CONS, otherwise return (cons CAR CDR)."
  (if (and (eq car (car old-cons)) (eq cdr (cdr old-cons)))
      old-cons
      (cons car cdr)))

(defun sharing-mapcar (fun list)
  (declare (type function fun))
  #!+sb-doc
  "A simple (one list arg) mapcar that avoids consing up a new list
  as long as the results of calling FUN on the elements of LIST are
  eq to the original."
  (and list
       (sharing-cons list
                     (funcall fun (car list))
                     (sharing-mapcar fun (cdr list)))))

(defun all-arg-refs-relevant-p (printer args)
  (cond ((or (null printer) (keywordp printer) (eq printer t))
         t)
        ((symbolp printer)
         (find printer args :key #'arg-name))
        ((listp printer)
         (every (lambda (x) (all-arg-refs-relevant-p x args))
                printer))
        (t t)))

(defun pick-printer-choice (choices args)
  (dolist (choice choices
           (pd-error "no suitable choice found in ~S" choices))
    (when (all-arg-refs-relevant-p choice args)
      (return choice))))

(defun compile-printer-list (sources funstate)
  (unless (null sources)
    ;; Coalesce adjacent symbols/strings, and convert to strings if possible,
    ;; since they require less consing to write.
    (do ((el (car sources) (car sources))
         (names nil (cons (strip-quote el) names)))
        ((not (string-or-qsym-p el))
         (when names
           ;; concatenate adjacent strings and symbols
           (let ((string
                  (apply #'concatenate
                         'string
                         (mapcar #'string (nreverse names)))))
             ;; WTF? Everything else using INST-PRINT-NAME writes a string.
             (push (if (some #'alpha-char-p string)
                       `',(make-symbol string) ; Preserve casifying output.
                       string)
                   sources))))
      (pop sources))
    (cons (compile-printer-body (car sources) funstate)
          (compile-printer-list (cdr sources) funstate))))

(defun compile-printer-body (source funstate)
  (cond ((null source)
         nil)
        ((eq source :name)
         `(local-print-name))
        ((eq source :tab)
         `(local-tab-to-arg-column))
        ((keywordp source)
         (pd-error "unknown printer element: ~S" source))
        ((symbolp source)
         (compile-print source funstate))
        ((atom source)
         `(local-princ ',source))
        ((eq (car source) :using)
         (unless (or (stringp (cadr source))
                     (and (listp (cadr source))
                          (eq (caadr source) 'function)))
           (pd-error "The first arg to :USING must be a string or #'function."))
         (compile-print (caddr source) funstate
                        (make-valsrc (eval (cadr source)) (cadr source))))
        ((eq (car source) :plus-integer)
         ;; prints the given field proceed with a + or a -
         (let ((form
                (arg-value-form (arg-or-lose (cadr source) funstate)
                                funstate
                                :numeric)))
           `(progn
              (when (>= ,form 0)
                (local-write-char #\+))
              (local-princ ,form))))
        ((eq (car source) 'quote)
         `(local-princ ,source))
        ((eq (car source) 'function)
         `(local-call-global-printer ,source))
        ((eq (car source) :cond)
         `(cond ,@(mapcar (lambda (clause)
                            `(,(compile-test (find-first-field-name
                                              (cdr clause))
                                             (car clause)
                                             funstate)
                              ,@(compile-printer-list (cdr clause)
                                                      funstate)))
                          (cdr source))))
        ;; :IF, :UNLESS, and :WHEN are replaced by :COND during preprocessing
        (t
         `(progn ,@(compile-printer-list source funstate)))))

(defun compile-print (arg-name funstate &optional printer)
  (let* ((arg (arg-or-lose arg-name funstate))
         (printer (or printer (arg-printer arg)))
         (printer-val (value-or-source printer))
         (printer-src (source-form printer)))
    (cond ((stringp printer-val)
           `(local-format-arg ,(arg-value-form arg funstate) ,printer-val))
          ((vectorp printer-val)
           `(local-princ
             (aref ,printer-src
                   ,(arg-value-form arg funstate :numeric))))
          ((or (functionp printer-val)
               (and (consp printer-val) (eq (car printer-val) 'function)))
           `(local-call-arg-printer ,(arg-value-form arg funstate)
                                    ,printer-src))
          ((or (null printer-val) (eq printer-val t))
           `(,(if (arg-use-label arg) 'local-princ16 'local-princ)
             ,(arg-value-form arg funstate)))
          (t
           (pd-error "illegal printer: ~S" printer-src)))))

(defun string-or-qsym-p (thing)
  (or (stringp thing)
      (and (consp thing)
           (eq (car thing) 'quote)
           (or (stringp (cadr thing))
               (symbolp (cadr thing))))))

(defun strip-quote (thing)
  (if (and (consp thing) (eq (car thing) 'quote))
      (cadr thing)
      thing))

(defun compare-fields-form (val-form-1 val-form-2)
  (flet ((listify-fields (fields)
           (cond ((symbolp fields) fields)
                 ((every #'constantp fields) `',fields)
                 (t `(list ,@fields)))))
    (cond ((or (symbolp val-form-1) (symbolp val-form-2))
           `(equal ,(listify-fields val-form-1)
                   ,(listify-fields val-form-2)))
          (t
           `(and ,@(mapcar (lambda (v1 v2) `(= ,v1 ,v2))
                           val-form-1 val-form-2))))))

(defun compile-test (subj test funstate)
  (when (and (consp test) (symbolp (car test)) (not (keywordp (car test))))
    (setf subj (car test)
          test (cdr test)))
  (let ((key (if (consp test) (car test) test))
        (body (if (consp test) (cdr test) nil)))
    (cond ((null key)
           nil)
          ((eq key t)
           t)
          ((eq key :constant)
           (let* ((arg (arg-or-lose subj funstate))
                  (fields (arg-fields arg))
                  (consts body))
             (when (not (= (length fields) (length consts)))
               (pd-error "The number of constants doesn't match number of ~
                          fields in: (~S :constant~{ ~S~})"
                         subj body))
             (compare-fields-form (gen-arg-forms arg :numeric funstate)
                                  consts)))
          ((eq key :positive)
           `(> ,(arg-value-form (arg-or-lose subj funstate) funstate :numeric)
               0))
          ((eq key :negative)
           `(< ,(arg-value-form (arg-or-lose subj funstate) funstate :numeric)
               0))
          ((eq key :same-as)
           (let ((arg1 (arg-or-lose subj funstate))
                 (arg2 (arg-or-lose (car body) funstate)))
             (unless (and (= (length (arg-fields arg1))
                             (length (arg-fields arg2)))
                          (every (lambda (bs1 bs2)
                                   (= (byte-size bs1) (byte-size bs2)))
                                 (arg-fields arg1)
                                 (arg-fields arg2)))
               (pd-error "can't compare differently sized fields: ~
                          (~S :same-as ~S)" subj (car body)))
             (compare-fields-form (gen-arg-forms arg1 :numeric funstate)
                                  (gen-arg-forms arg2 :numeric funstate))))
          ((eq key :or)
           `(or ,@(mapcar (lambda (sub) (compile-test subj sub funstate))
                          body)))
          ((eq key :and)
           `(and ,@(mapcar (lambda (sub) (compile-test subj sub funstate))
                           body)))
          ((eq key :not)
           `(not ,(compile-test subj (car body) funstate)))
          ((and (consp key) (null body))
           (compile-test subj key funstate))
          (t
           (pd-error "bogus test-form: ~S" test)))))

(defun find-labeller-fun (args)
  (unless (some #'arg-use-label args)
    (return-from find-labeller-fun (values nil nil)))
  (let ((funstate (make-funstate args))
        (labels-form 'labels))
    (dolist (arg args)
      (when (arg-use-label arg)
        (setf labels-form
              `(let ((labels ,labels-form)
                     (addr ,(arg-value-form arg funstate :numeric nil)))
                 ;; if labeler didn't return an integer, it isn't a label
                 (if (or (not (integerp addr)) (assoc addr labels))
                     labels
                     (cons (cons addr nil) labels))))))
    (generate-function
            :labeller
            (list labels-form)
            funstate
            '(lambda (chunk labels dstate)
               (declare (type list labels)
                        (type dchunk chunk)
                        (type disassem-state dstate))
               (flet ((local-filtered-value (offset)
                        (declare (type filtered-value-index offset))
                        (aref (dstate-filtered-values dstate) offset))
                      (local-extract (bytespec)
                        (dchunk-extract chunk bytespec))
                      (adjust-label (val adjust-fun)
                        (funcall adjust-fun val dstate)))
                 (declare (ignorable #'local-filtered-value #'local-extract
                                     #'adjust-label)
                          (inline local-filtered-value local-extract
                                  adjust-label))
                 :body)))))

(defun find-prefilter-fun (args)
  (unless (some #'arg-prefilter args)
    (return-from find-prefilter-fun (values nil nil)))
  (let* ((funstate (make-funstate args))
         (forms
          (mapcan (lambda (arg &aux (pf (arg-prefilter arg)))
                    (when pf
                      (list `(setf (local-filtered-value ,(arg-position arg funstate))
                                   ,(maybe-listify
                                     (gen-arg-forms arg :filtering funstate))))))
                  args)))
    (generate-function
            :prefilter
            forms
            funstate
            '(lambda (chunk dstate)
               (declare (type dchunk chunk)
                        (type disassem-state dstate))
               (flet (((setf local-filtered-value) (value offset)
                        (declare (type filtered-value-index offset))
                        (setf (aref (dstate-filtered-values dstate) offset)
                              value))
                      (local-filter (value filter)
                        (funcall filter value dstate))
                      (local-extract (bytespec)
                        (dchunk-extract chunk bytespec)))
                (declare (ignorable #'local-filter #'local-extract)
                         (inline (setf local-filtered-value)
                                 local-filter local-extract))
                :body)))))

(defun compute-mask-id (args)
  (let ((mask dchunk-zero)
        (id dchunk-zero))
    (dolist (arg args (values mask id))
      (let ((av (arg-value arg)))
        (when av
          (do ((fields (arg-fields arg) (cdr fields))
               (values (if (atom av) (list av) av) (cdr values)))
              ((null fields))
            (let ((field-mask (dchunk-make-mask (car fields))))
              (when (/= (dchunk-and mask field-mask) dchunk-zero)
                (pd-error "The field ~S in arg ~S overlaps some other field."
                          (car fields)
                          (arg-name arg)))
              (dchunk-insertf id (car fields) (car values))
              (dchunk-orf mask field-mask))))))))

#!-sb-fluid (declaim (inline bytes-to-bits))
(declaim (maybe-inline sign-extend aligned-p align tab tab0))

(defun bytes-to-bits (bytes)
  (declare (type disassem-length bytes))
  (* bytes sb!vm:n-byte-bits))

(defun bits-to-bytes (bits)
  (declare (type disassem-length bits))
  (multiple-value-bind (bytes rbits)
      (truncate bits sb!vm:n-byte-bits)
    (when (not (zerop rbits))
      (error "~W bits is not a byte-multiple." bits))
    bytes))

(defun sign-extend (int size)
  (declare (type integer int)
           (type (integer 0 128) size))
  (if (logbitp (1- size) int)
      (dpb int (byte size 0) -1)
      int))

;;; Is ADDRESS aligned on a SIZE byte boundary?
(defun aligned-p (address size)
  (declare (type address address)
           (type alignment size))
  (zerop (logand (1- size) address)))

;;; Return ADDRESS aligned *upward* to a SIZE byte boundary.
(defun align (address size)
  (declare (type address address)
           (type alignment size))
  (logandc1 (1- size) (+ (1- size) address)))

(defun tab (column stream)
  (funcall (formatter "~V,1t") stream column)
  nil)
(defun tab0 (column stream)
  (funcall (formatter "~V,0t") stream column)
  nil)

(defun princ16 (value stream)
  (write value :stream stream :radix t :base 16 :escape nil))

(defun read-signed-suffix (length dstate)
  (declare (type (member 8 16 32 64) length)
           (type disassem-state dstate)
           (optimize (speed 3) (safety 0)))
  (sign-extend (read-suffix length dstate) length))

(defstruct (storage-info (:copier nil))
  (groups nil :type list)               ; alist of (name . location-group)
  (debug-vars #() :type vector))

(defstruct (segment (:conc-name seg-)
                    (:constructor %make-segment)
                    (:copier nil))
  (sap-maker (missing-arg)
             :type (function () sb!sys:system-area-pointer))
  ;; Length in bytes of the range of memory covered by this segment.
  (length 0 :type disassem-length)
  ;; Length of the memory range excluding any trailing untagged data.
  ;; Defaults to 'length' but could be shorter.
  ;; FIXME: can opcodes-length really be shorter? Nothing ever alters it.
  (opcodes-length 0 :type disassem-length)
  (virtual-location 0 :type address)
  (storage-info nil :type (or null storage-info))
  ;; KLUDGE: CODE-COMPONENT is not a type the host understands
  #-sb-xc-host (code nil :type (or null sb!kernel:code-component))
  (unboxed-data-range nil :type (or null (cons fixnum fixnum)))
  (hooks nil :type list))

;;; All state during disassembly. We store some seemingly redundant
;;; information so that we can allow garbage collect during disassembly and
;;; not get tripped up by a code block being moved...
(defstruct (disassem-state (:conc-name dstate-)
                           (:constructor %make-dstate)
                           (:copier nil))
  ;; offset of current pos in segment
  (cur-offs 0 :type offset)
  ;; offset of next position
  (next-offs 0 :type offset)
  ;; a sap pointing to our segment
  (segment-sap nil :type (or null sb!sys:system-area-pointer))
  ;; the current segment
  (segment nil :type (or null segment))
  ;; what to align to in most cases
  (alignment sb!vm:n-word-bytes :type alignment)
  (byte-order :little-endian
              :type (member :big-endian :little-endian))
  ;; for user code to hang stuff off of
  (properties nil :type list)
  ;; for user code to hang stuff off of, cleared each time after a
  ;; non-prefix instruction is processed
  (inst-properties nil :type list)
  (filtered-values (make-array max-filtered-value-index)
                   :type filtered-value-vector)
  ;; used for prettifying printing
  (addr-print-len nil :type (or null (integer 0 20)))
  (argument-column 0 :type column)
  ;; to make output look nicer
  (output-state :beginning
                :type (member :beginning
                              :block-boundary
                              nil))

  ;; alist of (address . label-number)
  (labels nil :type list)
  ;; same as LABELS slot data, but in a different form
  (label-hash (make-hash-table) :type hash-table)
  ;; list of function
  (fun-hooks nil :type list)

  ;; alist of (address . label-number), popped as it's used
  (cur-labels nil :type list)
  ;; OFFS-HOOKs, popped as they're used
  (cur-offs-hooks nil :type list)

  ;; for the current location
  (notes nil :type list)

  ;; currently active source variables
  (current-valid-locations nil :type (or null (vector bit))))
(def!method print-object ((dstate disassem-state) stream)
  (print-unreadable-object (dstate stream :type t)
    (format stream
            "+~W~@[ in ~S~]"
            (dstate-cur-offs dstate)
            (dstate-segment dstate))))

;;; Return the absolute address of the current instruction in DSTATE.
(defun dstate-cur-addr (dstate)
  (the address (+ (seg-virtual-location (dstate-segment dstate))
                  (dstate-cur-offs dstate))))

;;; Return the absolute address of the next instruction in DSTATE.
(defun dstate-next-addr (dstate)
  (the address (+ (seg-virtual-location (dstate-segment dstate))
                  (dstate-next-offs dstate))))

;;; Get the value of the property called NAME in DSTATE. Also SETF'able.
;;;
;;; KLUDGE: The associated run-time machinery for this is in
;;; target-disassem.lisp (much later). This is here just to make sure
;;; it's defined before it's used. -- WHN ca. 19990701
(defmacro dstate-get-prop (dstate name)
  `(getf (dstate-properties ,dstate) ,name))

;;; Push NAME on the list of instruction properties in DSTATE.
(defun dstate-put-inst-prop (dstate name)
  (push name (dstate-inst-properties dstate)))

;;; Return non-NIL if NAME is on the list of instruction properties in
;;; DSTATE.
(defun dstate-get-inst-prop (dstate name)
  (member name (dstate-inst-properties dstate) :test #'eq))
