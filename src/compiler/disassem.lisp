;;;; machine-independent disassembler

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-DISASSEM")

;;; types and defaults

(deftype text-width () '(integer 0 1000))
(deftype alignment () '(integer 0 64))
(deftype offset () 'fixnum)
(deftype address () 'word)
(deftype disassem-length () '(and unsigned-byte fixnum))
(deftype column () '(integer 0 1000))

(defconstant max-filtered-value-index 32)
(deftype filtered-value-index ()
  `(integer 0 (,max-filtered-value-index)))
(deftype filtered-value-vector ()
  `(simple-array t (,max-filtered-value-index)))

;;;; disassembly parameters

;; With a few tweaks, you can use a running SBCL as a cross-assembler
;; and disassembler for other supported backends,
;; if that backend has been converted to use a distinct ASM package.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter sb-assem::*backend-instruction-set-package*
    (find-package #.(sb-cold::backend-asm-package-name))))

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
  "The width of instruction bytes.")
(declaim (type text-width *disassem-inst-column-width*))

(defvar *disassem-note-column* (+ 45 *disassem-inst-column-width*)
  "The column in which end-of-line comments for notes are started.")

;;;; A DCHUNK contains the bits we look at to decode an
;;;; instruction.
;;;; I tried to keep this abstract so that if using integers > the machine
;;;; word size conses too much, it can be changed to use bit-vectors or
;;;; something.
;;;;
;;;; KLUDGE: It's not clear that using bit-vectors would be any more efficient.
;;;; Perhaps the abstraction could go away. -- WHN 19991124

(declaim (inline dchunk-or dchunk-and dchunk-clear dchunk-not
                 dchunk-make-mask dchunk-make-field
                 dchunk-extract
                 dchunk=
                 dchunk-count-bits))

;;; For variable-length instruction sets, such as x86, it is better to
;;; define the dchunk size to be the smallest number of bits necessary
;;; and sufficient to decode any instruction format, if that quantity
;;; of bits is small enough to avoid bignum consing.
;;; Ideally this constant would go in the 'insts' file for the architecture,
;;; but there's really no easy way to do that at present.
(defconstant dchunk-bits
  #+x86-64 56
  #+ppc64 32
  #-(or x86-64 ppc64) sb-vm:n-word-bits)

(deftype dchunk ()
  `(unsigned-byte ,dchunk-bits))
(deftype dchunk-index ()
  `(integer 0 ,dchunk-bits))

(defconstant dchunk-zero 0)
(defconstant dchunk-one (ldb (byte dchunk-bits 0) -1))

(defun dchunk-extract (chunk byte-spec)
  (declare (type dchunk chunk))
  (the dchunk (ldb byte-spec (the dchunk chunk))))

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

(defstruct (arg (:constructor %make-arg (name))
                (:copier nil)
                (:predicate nil))
  (name nil :type symbol :read-only t)
  (fields nil :type list)

  (value nil :type (or list integer))
  (sign-extend-p nil :type boolean)

  ;; functions to use
  (printer nil :type (or null function vector))
  (prefilter nil :type (or null function))
  (use-label nil :type (or boolean function)))

(defstruct (instruction-format (:conc-name format-)
                               (:constructor make-inst-format
                                             (name length default-printer args))
                               (:copier nil)
                               #-sb-xc-host :no-constructor-defun)
  (name nil :read-only t)
  (args nil :type list :read-only t)

  (length 0 :type disassem-length :read-only t)               ; in bytes

  (default-printer nil :type list :read-only t))
(declaim (freeze-type arg instruction-format))

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

;;; machinery to provide more meaningful error messages during compilation
(defvar *current-instruction-flavor*)
(defun pd-error (fmt &rest args)
  (if (boundp '*current-instruction-flavor*)
      (error "~{A printer ~D~}: ~?" *current-instruction-flavor* fmt args)
      (apply #'error fmt args)))

(defun format-or-lose (name)
  (or (get name 'inst-format)
      (pd-error "unknown instruction format ~S" name)))

;;; Return a modified copy of ARG that has property values changed
;;; depending on whether it is being used at compile-time or load-time.
;;; This is to avoid evaluating #'FOO references at compile-time
;;; while allowing compile-time manipulation of byte specifiers.
(defun massage-arg (spec when)
  (ecase when
    (:compile
     ;; At compile-time we get a restricted view of the DEFINE-ARG-TYPE args,
     ;; just enough to macroexpand :READER definitions. :TYPE and :SIGN-EXTEND
     ;; are as specified, but :PREFILTER, :LABELLER, and :PRINTER are not
     ;; compile-time evaluated.
     (loop for (indicator val) on (cdr spec) by #'cddr
           nconc (case indicator
                   (:sign-extend ; Only a literal T or NIL is allowed
                    (list indicator (the boolean val)))
                   (:prefilter
                    ;; #'ERROR is a placeholder for any compile-time non-nil
                    ;; value. If nil, it must be literally nil, not 'NIL.
                    (list indicator (if val #'error nil)))
                   ((:field :fields :type)
                    (list indicator val)))))
    (:eval
     (loop for (indicator raw-val) on (cdr spec) by #'cddr
           ;; Use NAMED-LAMBDAs to enhance debuggability,
           for val = (if (typep raw-val '(cons (eql lambda)))
                         `(named-lambda ,(format nil "~A.~A" (car spec) indicator)
                                        ,@(cdr raw-val))
                         raw-val)
           nconc (case indicator
                   (:reader nil) ; drop it
                   (:prefilter ; Enforce compile-time-determined not-nullness.
                    (list indicator (if val `(the (not null) ,val) nil)))
                   (t (list indicator val)))))))

(defmacro define-instruction-format ((format-name length-in-bits
                                      &key default-printer include)
                                     &body arg-specs)
  #+sb-xc-host (declare (ignore default-printer))
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

  If ARG-NAME is an integer it is the same as (#.(gensym) :value arg-name ...).

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
  `(progn
     (eval-when (:compile-toplevel)
       (%def-inst-format
        ',format-name ',include ,length-in-bits nil
        ,@(mapcar (lambda (arg) `(list ',(car arg) ,@(massage-arg arg :compile)))
                  arg-specs)))
     #-sb-xc-host ; Host doesn't execute any stuff that comes with
     (progn       ; format definitions, including dchunk readers
      ,@(mapcan
         (lambda (arg-spec)
           (awhen (getf (cdr arg-spec) :reader)
            `((defun ,it (dchunk dstate)
                (declare (ignorable dchunk dstate))
                (flet ((local-filtered-value (offset)
                         (declare (type filtered-value-index offset))
                         (aref (dstate-filtered-values dstate) offset))
                       (local-extract (bytespec)
                         (dchunk-extract dchunk bytespec)))
                  (declare (ignorable #'local-filtered-value #'local-extract)
                           (inline local-filtered-value local-extract))
                  ;; Delay ARG-FORM-VALUE call until after compile-time-too
                  ;; processing of !%DEF-INSTRUCTION-FORMAT has happened.
                  (macrolet
                      ((reader ()
                         (let* ((format-args
                                 (format-args (format-or-lose ',format-name)))
                                (arg (find ',(car arg-spec) format-args
                                           :key #'arg-name))
                                (funstate (make-funstate format-args))
                                (*!temp-var-counter* 0)
                                (expr (arg-value-form arg funstate :numeric)))
                           `(let* ,(make-arg-temp-bindings funstate) ,expr))))
                    (reader)))))))
         arg-specs)
      (%def-inst-format
       ',format-name ',include ,length-in-bits ,default-printer
       ,@(mapcar (lambda (arg) `(list ',(car arg) ,@(massage-arg arg :eval)))
                 arg-specs)))))

(defun %def-inst-format (name inherit length printer &rest arg-specs)
  (declare (inline make-inst-format))
  (let ((args (if inherit (copy-list (format-args (format-or-lose inherit)))))
        (seen))
    (dolist (arg-spec arg-specs)
      (let* ((arg-name (car arg-spec))
             (properties (cdr arg-spec)))
        (when (integerp arg-name)
          (setf properties (list* :value arg-name
                                  properties)
                arg-name (gensym)))
        (let ((cell (member arg-name args :key #'arg-name)))
          (aver (not (memq arg-name seen)))
          (push arg-name seen)
          (cond ((not cell)
                 (setq args (nconc args (list (apply #'modify-arg (%make-arg arg-name)
                                                     length properties)))))
                (properties
                 (rplaca cell (apply #'modify-arg (copy-structure (car cell))
                                     length properties)))))))
    (setf (get name 'inst-format)
          (make-inst-format name (bits-to-bytes length) printer args))))

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
    (let ((type-arg (or (get type 'arg-type)
                        (pd-error "unknown argument type: ~S" type))))
      (setf (arg-printer arg) (arg-printer type-arg))
      (setf (arg-prefilter arg) (arg-prefilter type-arg))
      (setf (arg-sign-extend-p arg) (arg-sign-extend-p type-arg))
      (setf (arg-use-label arg) (arg-use-label type-arg))))
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
                     bytespec format-length sb-c:*backend-byte-order*))
                  fields)))
  arg)

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
          ;; We can end up here with VARS = NIL, and VALS = an atom.
          ;; As the spec says, MAPC "should be prepared to signal an error
          ;; ... if any list is not a proper list"
          ;; We don't err in that situation because we check for ENDP of the
          ;; lists from left to right. However, at least one implementation
          ;; does rigorously use ENDP on both lists on each iteration.
          (cond ((not vars))
                ((listp vars)
                 (mapc (lambda (var val) (push `(,var ,val) bindings)) vars vals))
                (t
                 (push `(,vars ,vals) bindings))))))))

;;; Return the form(s) that should be evaluated to render ARG in the chosen
;;; RENDERING style, which is one of :RAW, :SIGN-EXTENDED,
;;; :FILTERED, :NUMERIC, and :FINAL. Each rendering depends on the preceding
;;; one, so asking for :FINAL will implicitly compute all renderings.
(defvar *!temp-var-counter*)
(defun gen-arg-forms (arg rendering funstate)
  (labels ((tempvars (n)
             (if (plusp n)
                 (cons (package-symbolicate
                        #.(find-package "SB-DISASSEM") ".T" (incf *!temp-var-counter*))
                       (tempvars (1- n))))))
    (let* ((arg-cell (assq arg funstate))
           (rendering-temps (cdr (assq rendering (cdr arg-cell))))
           (vars (car rendering-temps))
           (forms (cdr rendering-temps)))
      (unless forms
        (multiple-value-bind (new-forms single-value-p)
            (%gen-arg-forms arg rendering funstate)
          (setq forms new-forms
                vars (cond ((or single-value-p (atom forms))
                            (if (symbolp forms) vars (car (tempvars 1))))
                           ((every #'symbolp forms)
                            ;; just use the same as the forms
                            nil)
                           (t
                            (tempvars (length forms)))))
          (push (list* rendering vars forms) (cdr arg-cell))))
      (or vars forms))))

(defun maybe-listify (forms)
  (cond ((atom forms)
         forms)
        ((/= (length forms) 1)
         `(list ,@forms))
        (t
         (car forms))))

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
                           &key ((:type inherit))
                                sign-extend prefilter printer use-label)
  (declare (ignore sign-extend prefilter printer use-label))
  ;; FIXME: this should be an *unevaluated* macro arg (named :INHERIT)
  (aver (typep inherit '(or null (cons (eql quote) (cons symbol null)))))
  (let ((pair (cons name (loop for (ind val) on args by #'cddr
                               unless (eq ind :type)
                               nconc (list ind val)))))
    `(progn
       (eval-when (:compile-toplevel :execute)
         (%def-arg-type ',name ,inherit ,@(massage-arg pair :compile)))
       #-sb-xc-host ; Host doesn't need the real definition.
       (%def-arg-type ',name ,inherit ,@(massage-arg pair :eval)))))

(defun %def-arg-type (name inherit &rest properties)
  (setf (get name 'arg-type)
        (apply 'modify-arg (%make-arg name) nil
               (nconc (when inherit (list :type inherit)) properties))))

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
           `((adjust-label ,(maybe-listify filtered-forms) ,use-label))
           filtered-forms)))
    (:final ; if arg is not a label, return numeric value, otherwise a string
     (let ((numeric-forms (gen-arg-forms arg :numeric funstate)))
       (if (arg-use-label arg)
           `((lookup-label ,(maybe-listify numeric-forms)))
           numeric-forms)))))

(declaim (inline bytes-to-bits))

(defun bytes-to-bits (bytes)
  (declare (type disassem-length bytes))
  (* bytes sb-vm:n-byte-bits))

(defun bits-to-bytes (bits)
  (declare (type disassem-length bits))
  (multiple-value-bind (bytes rbits)
      (truncate bits sb-vm:n-byte-bits)
    (when (not (zerop rbits))
      (error "~W bits is not a byte-multiple." bits))
    bytes))

(defun sign-extend (int size)
  (declare (type integer int)
           (type (integer 0 128) size))
  (if (logbitp (1- size) int)
      (dpb int (byte size 0) -1)
      int))
