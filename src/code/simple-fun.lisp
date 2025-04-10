;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;;; Generalizing over SIMPLE-FUN, CLOSURE, and FUNCALLABLE-INSTANCEs

;;; Underlying SIMPLE-FUN
(defun %fun-fun (function)
  (declare (function function))
  (case (%fun-pointer-widetag function)
    (#.sb-vm:closure-widetag
     (%closure-fun function))
    (#.sb-vm:funcallable-instance-widetag
     (%fun-fun (%funcallable-instance-fun function)))
    (t function)))

(define-load-time-global **closure-extra-values**
    (make-hash-table :test 'eq :weakness :key :synchronized t))

;;; This is the bit pattern ORed into the haader word with no further shifting.
(defconstant closure-extra-data-indicator #x800000)
;;                                              -- widetag
;;                                          ---- length (masked to #x7FFF)
(eval-when (:compile-toplevel)
  ;; Ensure no overlap of the packed fields
  (aver (zerop (logand (ash sb-vm:short-header-max-words sb-vm:n-widetag-bits)
                       closure-extra-data-indicator))))

(declaim (inline get-closure-length))
(defun get-closure-length (f)
  (logand (ash (function-header-word f) (- sb-vm:n-widetag-bits))
          sb-vm:short-header-max-words))

(defmacro closure-len->nvalues (length)
  `(- ,length (1- sb-vm:closure-info-offset)))

;; Return T if and only if CLOSURE has extra values that are physically
;; in a padding slot of the object and not in the external hash-table.
(defun closure-has-extra-values-slot-p (closure)
  (declare (closure closure))
  (and (logtest (function-header-word closure) closure-extra-data-indicator)
       (evenp (get-closure-length closure))))

(macrolet ((new-closure (nvalues)
             ;; argument is the number of INFO words
             #-(or arm64 ppc64 x86 x86-64)
             `(sb-c::maybe-with-system-tlab (closure)
               (sb-vm::%alloc-closure ,nvalues (%closure-fun closure)))
             #+(or arm64 ppc64 x86 x86-64)
             `(with-pinned-objects ((%closure-fun closure))
                ;; %CLOSURE-CALLEE manifests as a fixnum which remains
                ;; valid across GC due to %CLOSURE-FUN being pinned
                ;; until after the new closure is made.
                (sb-c::maybe-with-system-tlab (closure)
                 (sb-vm::%alloc-closure ,nvalues (sb-vm::%closure-callee closure)))))
           (copy-slots (has-extra-data)
             `(do ((j 0 (1+ j)))
                  ((>= j nvalues)
                   (with-pinned-objects (copy)
                     (let ((sap (sap+ (int-sap (get-lisp-obj-address copy))
                                      (- sb-vm:fun-pointer-lowtag))))
                       (declare (ignorable sap))
                       ,@(when has-extra-data
                           `((setf (sap-ref-word sap 0)
                                   (logior (function-header-word copy)
                                           closure-extra-data-indicator))))
                       #+compact-instance-header ; copy the layout
                       (setf (sap-ref-32 sap 4) ; ASSUMPTION: little-endian
                             (logior (get-lisp-obj-address ,(find-layout 'function)))))))
                (%closure-index-set copy j (%closure-index-ref closure j)))))

  ;; This is factored out because of a cutting-edge implementation
  ;; of tracing wrappers that I'm trying to finish.
  (defun copy-closure (closure)
    (declare (closure closure))
    (let* ((nvalues (closure-len->nvalues
                     (get-closure-length (truly-the function closure))))
           (copy (new-closure nvalues)))
      (copy-slots nil)
      copy))

  ;;; Assign CLOSURE a new name and/or docstring in VALUES, and return the
  ;;; closure. If PERMIT-COPY is true, this function may return a copy of CLOSURE
  ;;; to avoid using a global hashtable. We prefer to store the name in the
  ;;; closure itself, which is possible in two cases:
  ;;; (1) if the closure has a padding slot due to alignment constraints,
  ;;;     the padding slot holds the name.
  ;;; (2) if it doesn't, but the caller does not care about identity,
  ;;;     then a copy of the closure with extra slots reduces to case (1).
  (defun set-closure-extra-values (closure permit-copy data)
    (declare (closure closure))
    (let* ((len (get-closure-length (truly-the function closure)))
           (nvalues (closure-len->nvalues len))
           (has-padding-slot (evenp len))
           (extendedp (logtest (function-header-word closure)
                               closure-extra-data-indicator)))
      (when (and (not extendedp) (not has-padding-slot) permit-copy)
        (let ((copy (new-closure (1+ nvalues))))
          (with-pinned-objects (copy)
            (copy-slots t)
            ;; We copy only if there was no padding, which means that adding 1 slot
            ;; physically adds 2 slots. You might think that the added data go in the
            ;; first new slot, followed by padding. Nope! Take an example of a
            ;; closure header specifying 3 words, which we increase to 4 words,
            ;; which is 5 with the header, which is 6 aligned to a doubleword:
            ;;   word 0: 2030008300800435
            ;;   word 1: 0000001001A0B280             ] words the closure's fun
            ;;   word 2: 00000000203AA39F = #<thing1> ] "should" use given
            ;;   word 3: 00000000203AA51F = #<thing2> ] 4 payload slots.
            ;;   word 4: 0000000000000000 = 0         ]
            ;;   word 5: 00000010019E2B9F = NAME    <-- name goes here
            ;; CLOSURE-EXTRA-VALUES always reads 1 word past the stated payload size
            ;; regardless of whether the closure really captured the implied
            ;; number of closure values.
            (%closure-index-set copy (1+ nvalues) data)
            (return-from set-closure-extra-values copy))))
      (unless (with-pinned-objects (closure)
                (unless extendedp ; Set the header bit
                  (setf (sap-ref-word (int-sap (get-lisp-obj-address closure))
                                      (- sb-vm:fun-pointer-lowtag))
                        (logior (function-header-word closure)
                                closure-extra-data-indicator)))
                (when has-padding-slot
                  (%closure-index-set closure nvalues data)
                  t))
        (if (unbound-marker-p data)
            (remhash closure **closure-extra-values**)
            (setf (gethash closure **closure-extra-values**) data))))
    closure))

(defun pack-closure-extra-values (name docstring)
    ;; Return one of:
    ;;   (a) Just a name => NAME
    ;;   (b) Just a docstring => DOCSTRING
    ;;   (c) Both => (NAME . DOCSTRING)
    ;; Always choose to store both if (a) or (b) would be ambiguous.
    ;; The reason for carefully distinguishing between NIL and unbound is that
    ;; if exactly one part is present, the generalized accessor for the other
    ;; part should report whatever the underlying simple-fun does.
    ;; i.e. setting docstring does not cause (%FUN-NAME CLOSURE) to be NIL.
  (cond ((or (and (not (unbound-marker-p name))
                  (not (unbound-marker-p docstring)))
             (typep name '(or string null (cons t (or string null)))))
         (cons name docstring))
        ((unbound-marker-p docstring) name)
        (t docstring)))

(defun closure-extra-values (closure &aux (unbound (make-unbound-marker)))
    ;; Return name and documentation of closure as two values.
    ;; Either or both can be the unbound marker.
    ;; Following the convention established by SET-CLOSURE-EXTRA-VALUES,
    ;; an even-length payload takes the last slot for the name,
    ;; and an odd-length payload uses the global hashtable.
  (declare (closure closure))
  (if (logtest (function-header-word closure) closure-extra-data-indicator)
      (let* ((len (get-closure-length closure))
             ;; See examples in doc/internals-notes/closure
             (data (if (evenp len) ; has padding slot
                       (%closure-index-ref closure (closure-len->nvalues len))
                       ; No padding slot
                       (gethash closure **closure-extra-values** 0))))
            (typecase data
             ((eql 0) (values unbound unbound))
             ((cons t (or string null (satisfies unbound-marker-p)))
              (values (car data) (cdr data)))
             ;; NIL represents an explicit docstring of NIL, not the name.
             ((or string null) (values unbound data))
             (t (values data unbound))))
      (values unbound unbound)))

(defconstant +closure-name-index+ 0)
(defconstant +closure-doc-index+ 1)
;; Preserve absence of value in the other extra "slot" when setting either.
(defun set-closure-name (closure permit-copy name)
  (set-closure-extra-values
   closure permit-copy
   (pack-closure-extra-values
    name (nth-value +closure-doc-index+ (closure-extra-values closure)))))

;;; a SETFable function to return the associated debug name for FUN
;;; (i.e., the third value returned from CL:FUNCTION-LAMBDA-EXPRESSION),
;;; or NIL if there's none
(defun %fun-name (function)
  (case (%fun-pointer-widetag function)
    (#.sb-vm:closure-widetag
     (let ((val (nth-value +closure-name-index+ (closure-extra-values function))))
       (unless (unbound-marker-p val)
         (return-from %fun-name val))))
    (#.sb-vm:funcallable-instance-widetag
     (typecase (truly-the funcallable-instance function)
       (generic-function
        (return-from %fun-name
          (sb-mop:generic-function-name function)))
       (interpreted-function
        (return-from %fun-name
          #+sb-eval (sb-eval:interpreted-function-debug-name function)
          #+sb-fasteval
          (let ((name (sb-interpreter:proto-fn-name (sb-interpreter:fun-proto-fn function))))
            (unless (eql name 0)
              name)))))))
  (%simple-fun-name (%fun-fun function)))

(defun (setf %fun-name) (new-value function)
  (case (%fun-pointer-widetag function)
    (#.sb-vm:closure-widetag
     (set-closure-name (truly-the closure function) nil new-value))
    (#.sb-vm:simple-fun-widetag
     (setf (%simple-fun-name function) new-value))
    (t
     (typecase (truly-the funcallable-instance function)
       (generic-function
        (setf (sb-mop:generic-function-name function) new-value))
       (interpreted-function
        #+sb-eval
        (setf (sb-eval:interpreted-function-debug-name function) new-value)
        #+sb-fasteval
        (setf (sb-interpreter:proto-fn-name (sb-interpreter:fun-proto-fn function))
              new-value)))))
  new-value)

(defun %fun-lambda-list (function)
  (typecase function
    (interpreted-function
     #+sb-fasteval
     (sb-interpreter:proto-fn-pretty-arglist (sb-interpreter:fun-proto-fn function))
     #+sb-eval
     (sb-eval:interpreted-function-debug-lambda-list function))
    (t
     (%simple-fun-arglist (%fun-fun function)))))

(defun (setf %fun-lambda-list) (new-value function)
  (typecase function
    (interpreted-function
     #+sb-fasteval
     (setf (sb-interpreter:proto-fn-pretty-arglist (sb-interpreter:fun-proto-fn function))
           new-value)
     #+sb-eval
     (setf (sb-eval:interpreted-function-debug-lambda-list function) new-value))
    ;; FIXME: Eliding general funcallable-instances for now.
    ((or simple-fun closure)
     (setf (%simple-fun-arglist (%fun-fun function)) new-value)))
  new-value)

(macrolet ((index (slot)
             `(+ (* sb-vm:code-slots-per-simple-fun (%simple-fun-index fun))
                 (get-dsd-index sb-c::compiled-debug-info rest)
                 ,slot))
           (def (accessor slot)
             `(progn
                (defun (setf ,accessor) (newval fun)
                  (declare (simple-fun fun))
                  (let ((di (%code-debug-info (fun-code-header fun))))
                    (aver (sb-c::compiled-debug-info-p di))
                    (setf (%instance-ref di (index ,slot)) newval)))
                (defun ,accessor (fun)
                  (let ((di (%code-debug-info (fun-code-header fun))))
                    ;; metadataless functions return NIL for all these slots
                    (when (sb-c::compiled-debug-info-p di)
                      (%instance-ref di (index ,slot))))))))
  (def %simple-fun-name    sb-vm:simple-fun-name-slot)
  (def %simple-fun-arglist sb-vm:simple-fun-arglist-slot)
  (def %simple-fun-source  sb-vm:simple-fun-source-slot)
  (def %simple-fun-info    sb-vm:simple-fun-info-slot))

(defun %simple-fun-type (fun)
  (let* ((info (%simple-fun-info fun))
         (internal-type (typecase info
                          ((cons t simple-vector) (car info)) ; (type . xref)
                          ((not simple-vector) info))))
    ;; For backward-compatibility we expand SFUNCTION -> FUNCTION.
    (if (and (listp internal-type) (eq (car internal-type) 'sfunction))
        (values (sb-ext:typexpand-1 internal-type))
        internal-type)))

(defun %simple-fun-xrefs (fun)
  (let ((info (%simple-fun-info fun)))
    (typecase info
      ((cons t simple-vector) (cdr info))
      (simple-vector info))))

(defun %fun-ftype (function)
  (typecase function
    #+sb-fasteval
    ;; Obtain a list of the right shape, usually with T for each
    ;; arg type, but respecting local declarations if any.
    (interpreted-function (sb-interpreter:%fun-ftype function))
    (t (%simple-fun-type (%fun-fun function)))))

(defun sb-c::ftype-from-definition (name)
  (let ((function (fboundp name)))
    (cond
      ((not function)
       (specifier-type 'function))
      ((and (symbolp name) (macro-function name))
       ;; this seems to be called often on macros
       ;; what's the right answer ???
       ;; (warn "ftype-from-fdefn called on macro ~s" name)
       (specifier-type '(function (t t) t)))
      (t
        ;; Never signal the PARSE-UNKNOWN-TYPE condition.
        ;; This affects 2 regression tests, both very contrived:
        ;;  - in defstruct.impure ASSERT-ERROR (BUG127--FOO (MAKE-BUG127-E :FOO 3))
        ;;  - in compiler.impure at :IDENTIFY-SUSPECT-VOPS
        (let* ((ftype (%fun-ftype function))
               (context
                (sb-kernel::make-type-context
                 ftype nil sb-kernel::+type-parse-signal-inhibit+)))
          (declare (dynamic-extent context))
          (values (sb-kernel::basic-parse-typespec ftype context)))))))

;;; Return the lambda expression for SIMPLE-FUN if compiled to memory
;;; and rentention of forms was enabled via the EVAL-STORE-SOURCE-FORM policy
;;; (as is the default).
(defun %simple-fun-lexpr (simple-fun)
  (declare (simple-fun simple-fun))
  (let ((source (%simple-fun-source simple-fun)))
    (typecase source
      ((cons t string) (car source))
      ((not string) source))))

(defun %simple-fun-doc (simple-fun)
  (declare (simple-fun simple-fun))
  (let ((source (%simple-fun-source simple-fun)))
    (typecase source
      ((cons t string) (cdr source))
      (string source))))

(defun (setf %simple-fun-doc) (doc simple-fun)
  (declare (type (or null string) doc)
           (simple-fun simple-fun))
  (setf (%simple-fun-source simple-fun)
        (let ((form (%simple-fun-lexpr simple-fun)))
          (if (and form doc) (cons form doc) (or form doc))))
  doc)

;;; Return the number of bytes to subtract from the untagged address of SIMPLE-FUN
;;; to obtain the untagged address of its code component.
;;; See also CODE-FROM-FUNCTION.
(declaim (inline %fun-code-offset))
(defun %fun-code-offset (simple-fun)
  (declare (type simple-fun simple-fun))
  ;; A fun header can't really point backwards this many words, but it's ok-
  ;; the point is to mask out the layout pointer bits (if compact-instance-header).
  ;; The largest representable code size (in words) is ~22 bits for 32-bit words.
  ;; It would be possible to optimize out one shift here for 32-bit headers:
  ;;   #bxxxxxxx..xxxxxxxx00101010 (header widetag = #x2A)
  ;; so we don't need to right shift out all 8 widetag bits and then
  ;; left-shift by 2 (= word-shift). We could just right-shift 6, and mask.
  (ash (ldb (byte 24 sb-vm:n-widetag-bits) (function-header-word simple-fun))
       sb-vm:word-shift))

;;;; CODE-COMPONENT

;;; software mark bits on pages of code require that all assignments to
;;; header slots go through the CODE-HEADER-SET vop,
;;; which is slightly different from the general soft card mark implementation
;;; for historical reasons.
(defun (setf %code-debug-info) (newval code)
  (setf (code-header-ref code sb-vm:code-debug-info-slot) newval)
  newval)

(defun (setf sb-vm::%code-fixups) (newval code)
  (setf (code-header-ref code sb-vm::code-fixups-slot) newval)
  newval)

#+(or sparc ppc64)
(defun code-trailer-ref (code offset)
  (with-pinned-objects (code)
    (sap-ref-32 (int-sap (get-lisp-obj-address code))
                (+ (code-object-size code) offset (- sb-vm:other-pointer-lowtag)))))

;;; The last 'uint16' in the object holds the trailer length (see 'src/runtime/code.h')
(declaim (inline code-trailer-len))
(defun code-trailer-len (code-obj)
  (let ((word (code-trailer-ref code-obj -4)))
    ;; TRAILER-REF returns 4-byte quantities. Extract a two-byte quantity.
    #+little-endian (ldb (byte 16 16) word)
    #+big-endian    (ldb (byte 16  0) word)))

;;; The fun-table-count is a uint16_t immediately preceding the trailer length
;;; containing two subfields:
;;;  11 bits for the number of simple-funs in the code component
;;;   5 bits for the number of pad bytes added to align the fun-offset-table
;;; See also code_n_funs() in code.h
(macrolet ((code-fun-table-count (code-obj)
             `(if (eql (code-trailer-len ,code-obj) 0)
                  0
                  (let ((word (code-trailer-ref ,code-obj -4)))
                    ;; TRAILER-REF returns 4-byte quantities. Extract a two-byte quantity.
                    #+little-endian (ldb (byte 16  0) word)
                    #+big-endian    (ldb (byte 16 16) word)))))

;;; Return the number of simple-funs in CODE-OBJ
;;; Keep in sync with C function code_n_funs()
(defun code-n-entries (code-obj)
  (declare (type code-component code-obj))
  (ash (code-fun-table-count code-obj) -5))

;;; Subtract from %CODE-CODE-SIZE the number of trailing data bytes which aren't
;;; machine instructions. It's generally ok to use TEXT-SIZE or CODE-SIZE if searching
;;; for a function that encloses a given program counter, since the PC won't be
;;; in the data range. But all the same, it looks nicer in disassemblies to avoid
;;; examining bytes that aren't instructions.
;;; NOTE: this definition of 'code-text-size' is almost but not exactly identical
;;;       to the one in 'code.h'. This logic subtracts the padding amount,
;;;       which is essential to getting the proper disassembly length for the
;;;       final simple-fun if instructions have variable length (as on x86).
;;;       The C definition doesn't really care about that minor detail
;;;       so it might overreport the size by the padding amount.
(defun %code-text-size (code-obj)
  (- (%code-code-size code-obj)
     (code-trailer-len code-obj)
     ;; Subtract between 0 and 31 bytes of padding
     (logand (code-fun-table-count code-obj) #x1f)))
) ; end MACROLET

;;; Return the offset in bytes from (CODE-INSTRUCTIONS CODE-OBJ)
;;; to its FUN-INDEXth function.
(declaim (inline %code-fun-offset))
(defun %code-fun-offset (code-obj fun-index)
  (declare ((unsigned-byte 11) fun-index))
  (code-trailer-ref code-obj (* -4 (+ fun-index 2))))

(defun %code-entry-point (code-obj fun-index)
  (declare (type (unsigned-byte 16) fun-index))
  ;; FIXME: it should probably be an error to pass FUN-INDEX too large
  (when (< fun-index (code-n-entries code-obj))
    (truly-the function
      (values (%primitive sb-c:compute-fun code-obj
                          (%code-fun-offset code-obj fun-index))))))

;;; Return the 0-based index of SIMPLE-FUN within its code component.
;;; Computed via binary search.
(defun %simple-fun-index (simple-fun)
  (let* ((code (fun-code-header simple-fun))
         (n-entries (code-n-entries code)))
    (if (eql n-entries 1)
        0
        (let* ((offset (the (unsigned-byte 24)
                            (- (%fun-code-offset simple-fun)
                               (ash (code-header-words code) sb-vm:word-shift))))
               (min 0)
               (max (1- n-entries)))
          (declare ((unsigned-byte 16) min max))
          (loop
             (let* ((index (floor (+ min max) 2))
                    (guess (%code-fun-offset code index)))
               (cond ((< guess offset) (setq min (1+ index)))
                     ((> guess offset) (setq max (1- index)))
                     (t (return index)))
               (aver (<= min max))))))))

;;; Return the starting address of FUN's code as a SAP.
;;; FUN must be pinned.
(declaim (inline sb-vm:simple-fun-entry-sap))
(defun sb-vm:simple-fun-entry-sap (fun)
  #-(or x86 x86-64)
  (int-sap (+ (get-lisp-obj-address fun)
              (- sb-vm:fun-pointer-lowtag)
              (ash sb-vm:simple-fun-insts-offset sb-vm:word-shift)))
  ;; The preceding case would actually work, but I'm anticipating a change
  ;; in which simple-fun headers are all contiguous in their code component,
  ;; followed by all the machine instructions for all the simple-funs.
  ;; If that change is done, then you must indirect through the SELF pointer
  ;; in order to get the correct starting address.
  ;; (Such change would probably be confined to x86[-64])
  #+(or x86 x86-64)
  (sap-ref-sap (int-sap (- (get-lisp-obj-address fun) sb-vm:fun-pointer-lowtag))
               (ash sb-vm:simple-fun-self-slot sb-vm:word-shift)))

;;; Return the simple-fun within CODE whose entrypoint is ADDRESS,
;;; or NIL if ADDRESS does not point to any function in CODE.
(defun sb-vm::%simple-fun-from-entrypoint (code address)
  (let ((min 0) (max (code-n-entries code)) (sap (int-sap address)))
    (declare ((unsigned-byte 16) min max))
    (unless (zerop max)
      (decf max)
      ;; Don't need to pin CODE here because it must have been pinned
      ;; by the caller, or else ADDRESS as supplied could be bogus already.
      (loop
        (when (< max min) (return nil))
        (let* ((index (floor (+ min max) 2))
               (fun (%code-entry-point code index))
               (guess (sb-vm:simple-fun-entry-sap fun)))
          (cond ((sap< guess sap) (setq min (1+ index)))
                ((sap> guess sap) (setq max (1- index)))
                (t (return fun))))))))

;;; Return the number of bytes of instructions in SIMPLE-FUN,
;;; i.e. to the distance to the next simple-fun or end of code component.
;;; If INDEX is specified, it is used to quickly find the next simple-fun.
;;; Otherwise the code object is scanned to determine SIMPLE-FUN's index.
(defun %simple-fun-text-len (simple-fun &optional index)
  (declare (simple-fun simple-fun))
  (let* ((code (fun-code-header simple-fun))
         (max-index (1- (code-n-entries code))))
      (- (cond ((eq simple-fun (%code-entry-point code max-index))
                (if index
                    (aver (= index max-index))
                    (setq index max-index))
                (%code-text-size code))
               (t
                (if index
                    (aver (eq (%code-entry-point code index) simple-fun))
                    (setq index (%simple-fun-index simple-fun)))
                (%code-fun-offset code (1+ index))))
         (%code-fun-offset code index)
         (ash sb-vm:simple-fun-insts-offset sb-vm:word-shift))))

(defun code-n-unboxed-data-bytes (code-obj)
  ;; If the number of boxed words (from the header) is not the same as
  ;; the displacement backwards from the first simple-fun to the header,
  ;; then there are unboxed constants between the end of the boxed constants
  ;; and the first simple-fun.
  (let ((f (%code-entry-point code-obj 0)))
    (or (and f
             (- (%fun-code-offset f)
                (ash (code-header-words code-obj) sb-vm:word-shift)))
        0)))

;;; Set (SYMBOL-FUNCTION SYMBOL) to a closure that signals an error,
;;; preventing funcall/apply of macros and special operators.
(defun sb-c::install-guard-function (symbol fun-name)
  ;; (SETF SYMBOL-FUNCTION) goes out of its way to disallow this closure,
  ;; but we can trivially replicate its low-level effect.
  (let ((closure
         (set-closure-name
          (lambda (&rest args)
           (declare (ignore args))
           ;; don't capture the SYMBOL argument of INSTALL-GUARD-FUNCTION.
           ;; It's redundant with FUN-NAME, which provides more information.
           ;; No need to use SPECIAL-OPERATOR-P to see what it is.
           (let ((kind (first fun-name)))
           ;; ANSI specification of FUNCALL says that this should be
           ;; an error of type UNDEFINED-FUNCTION, not just SIMPLE-ERROR.
           ;; SPECIAL-FORM-FUNCTION is a subtype of UNDEFINED-FUNCTION.
             (error (if (eq kind :special) 'special-form-function 'undefined-function)
                    :name (second fun-name))))
          t
          fun-name)))
    (fset symbol closure)))

;;;; Iterating over closure values

(defmacro do-closure-values ((value closure &key include-extra-values) &body body)
  (with-unique-names (i nclosure)
    `(let ((,nclosure ,closure))
       (declare (closure ,nclosure))
       (dotimes (,i (- (+ (get-closure-length ,nclosure)
                          1
                          ,@(and include-extra-values
                                 `((if (closure-has-extra-values-slot-p ,nclosure)
                                       1
                                       0))))
                       sb-vm:closure-info-offset))
         (let ((,value (%closure-index-ref ,nclosure ,i)))
           ,@body)))))

(defun %closure-values (closure)
  (declare (closure closure))
  (let (values)
    (do-closure-values (elt closure)
      (push elt values))
    values)) ; no need to reverse - this has no promised iteration order

;;; This is like FIND-IF, except that we do it on a compiled closure's
;;; environment.
(defun find-if-in-closure (test closure)
  (declare (closure closure))
  (do-closure-values (value closure)
    (when (funcall test value)
      (return value))))
