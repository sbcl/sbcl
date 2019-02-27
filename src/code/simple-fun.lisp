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
  ;; It's too bad that TYPECASE isn't able to generate equivalent code.
  (case (fun-subtype function)
    (#.sb-vm:closure-widetag
     (%closure-fun function))
    (#.sb-vm:funcallable-instance-widetag
     (%fun-fun (%funcallable-instance-fun function)))
    (t function)))

(define-load-time-global **closure-extra-values**
    (make-hash-table :test 'eq :weakness :key :synchronized t))

(defconstant closure-extra-data-indicator #x8000)
(eval-when (:compile-toplevel)
  (aver (zerop (logand sb-vm:short-header-max-words
                       closure-extra-data-indicator))))

(declaim (inline get-closure-length))
(defun get-closure-length (f)
  (logand (fun-header-data f) sb-vm:short-header-max-words))

(macrolet ((extendedp-bit ()
             ;; The closure header is updated using sap-ref-n.
             ;; Why not use SET-HEADER-DATA???
             (ash closure-extra-data-indicator sb-vm:n-widetag-bits))
           (%closure-index-set (closure index val)
             ;; Use the identical convention as %CLOSURE-INDEX-REF for the index.
             ;; There are no closure slot setters, and in fact SLOT-SET
             ;; does not exist in a variant that takes a non-constant index.
             `(setf (sap-ref-lispobj (int-sap (get-lisp-obj-address ,closure))
                                     (+ (ash sb-vm:closure-info-offset sb-vm:word-shift)
                                        (ash ,index sb-vm:word-shift)
                                        (- sb-vm:fun-pointer-lowtag)))
                    ,val))
           (closure-header-word (closure)
             `(sap-ref-word (int-sap (get-lisp-obj-address ,closure))
                            (- sb-vm:fun-pointer-lowtag))))

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
    (let ((payload-len (get-closure-length (truly-the function closure)))
          (extendedp (logtest (fun-header-data closure) closure-extra-data-indicator)))
      (when (and (not extendedp) permit-copy (oddp payload-len))
        ;; PAYLOAD-LEN includes the trampoline, so the number of slots we would
        ;; pass to %COPY-CLOSURE is 1 less than that, were it not for
        ;; the fact that we actually want to create 1 additional slot.
        ;; So in effect, asking for PAYLOAD-LEN does exactly the right thing.
        (let ((copy #-(or x86 x86-64)
                    (sb-vm::%copy-closure payload-len (%closure-fun closure))
                    #+(or x86 x86-64)
                    (with-pinned-objects ((%closure-fun closure))
                      ;; %CLOSURE-CALLEE manifests as a fixnum which remains
                      ;; valid across GC due to %CLOSURE-FUN being pinned
                      ;; until after the new closure is made.
                      (sb-vm::%copy-closure payload-len
                                            (sb-vm::%closure-callee closure)))))
          (with-pinned-objects (copy)
            (loop with sap = (int-sap (get-lisp-obj-address copy))
                  for i from 0 below (1- payload-len)
                  for ofs from (- (ash 2 sb-vm:word-shift) sb-vm:fun-pointer-lowtag)
                  by sb-vm:n-word-bytes
                  do (setf (sap-ref-lispobj sap ofs) (%closure-index-ref closure i)))
            (setf (closure-header-word copy) ; Update the header
                  ;; Closure copy lost its high header bits, so OR them in again.
                  (logior #+(and immobile-space 64-bit sb-thread)
                          (sap-int (sb-vm::current-thread-offset-sap
                                    sb-vm::thread-function-layout-slot))
                          #+(and immobile-space 64-bit (not sb-thread))
                          (get-lisp-obj-address sb-vm:function-layout)
                          (extendedp-bit)
                          (closure-header-word copy)))
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
            (%closure-index-set copy payload-len data)
            (return-from set-closure-extra-values copy))))
      (unless (with-pinned-objects (closure)
                (unless extendedp ; Set the header bit
                  (setf (closure-header-word closure)
                        (logior (extendedp-bit) (closure-header-word closure))))
                (when (evenp payload-len)
                  (%closure-index-set closure (1- payload-len) data)
                  t))
        (setf (gethash closure **closure-extra-values**) data)))
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
  (let ((header-data (fun-header-data closure)))
      (if (not (logtest closure-extra-data-indicator header-data))
          (values unbound unbound)
          (let* ((len (logand header-data sb-vm:short-header-max-words))
                 (data
                  ;; GET-CLOSURE-LENGTH counts the 'fun' slot in the length,
                  ;; but %CLOSURE-INDEX-REF starts indexing from the value slots.
                  (if (oddp len)
                      (gethash closure **closure-extra-values** 0)
                      (%closure-index-ref closure (1- len)))))
            ;; There can be a concurrency glitch, because the 'extended' flag is
            ;; toggled to indicate that extra data are present before the data
            ;; are written; so there's a window where NAME looks like 0. That could
            ;; be fixed in the setter, or caught here, but it shouldn't matter.
            (typecase data
             ((cons t (or string null (satisfies unbound-marker-p)))
              (values (car data) (cdr data)))
             ;; NIL represents an explicit docstring of NIL, not the name.
             ((or string null) (values unbound data))
             (t (values data unbound)))))))

;; Return T if and only if CLOSURE has extra values that are physically
;; in the object, not in the external hash-table.
(defun closure-has-extra-values-slot-p (closure)
  (declare (closure closure))
  (let ((header-data (fun-header-data closure)))
    (and (logtest closure-extra-data-indicator header-data)
         (evenp (logand header-data sb-vm:short-header-max-words)))))

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
  (case (fun-subtype function)
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
  (case (fun-subtype function)
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

;;; Extract the type from the function header FUNC.
(defun %simple-fun-type (func)
  (let ((internal-type (sb-vm::%%simple-fun-type func)))
    ;; For backward-compatibility we expand SFUNCTION -> FUNCTION.
    (if (and (listp internal-type) (eq (car internal-type) 'sfunction))
        (sb-ext:typexpand-1 internal-type)
        internal-type)))

(defun %fun-type (function)
  (typecase function
    #+sb-fasteval
    ;; Obtain a list of the right shape, usually with T for each
    ;; arg type, but respecting local declarations if any.
    (interpreted-function (sb-interpreter:%fun-type function))
    (t (%simple-fun-type (%fun-fun function)))))

;;; A FUN-SRC structure appears in %SIMPLE-FUN-INFO of any function for
;;; which a source form is retained via COMPILE or LOAD and for which it was
;;; required to store all three of these pieces of data.
(defstruct (fun-src (:constructor make-fun-src (form doc xrefs))
                    (:predicate nil)
                    (:copier nil))
  form
  doc
  xrefs)
;;; Assign %SIMPLE-FUN-INFO given the three possible things that
;;; we stash there.
(defun set-simple-fun-info (fun form doc xrefs)
  (setf (%simple-fun-info fun)
        (if form
            ;; If form starts with a string, we can't store it by itself
            ;; because it's confusable with (CONS STRING *)
            ;; Lambda expressions start with LAMBDA, obviously,
            ;; so this really shouldn't happen. Just being defensive here.
            (if (or doc xrefs (typep form '(cons string)))
                (make-fun-src form doc xrefs)
                form)
            (if (and doc xrefs)
                (cons doc xrefs)
                (or doc xrefs)))))

;;; Define readers for parts of SIMPLE-FUN-INFO, which holds:
;;;  - a string if documentation only,
;;;  - a SIMPLE-VECTOR if xrefs only
;;;  - a (CONS STRING SIMPLE-VECTOR) if both
;;;  - a CONS headed by LAMBDA if a source form only
;;;  - a FUN-SRC if other combinations of the above
;;;  - or NIL
(macrolet ((def (name info-part if-simple-vector if-string if-struct)
             `(defun ,name (simple-fun)
                (declare (simple-fun simple-fun))
                (let ((info (%simple-fun-info simple-fun)))
                  (typecase info
                    ;; (CONS (NOT STRING)) implies neither doc nor xref present
                    (list (if (stringp (car info)) (,info-part info)))
                    (simple-vector ,if-simple-vector)
                    (string ,if-string)
                    (fun-src ,if-struct)
                    (t (bug "bogus INFO for ~S: ~S" simple-fun info)))))))
  (def %simple-fun-doc   car nil info (fun-src-doc info))
  (def %simple-fun-xrefs cdr info nil (fun-src-xrefs info)))

;;; Return the lambda expression for SIMPLE-FUN if compiled to memory
;;; and rentention of forms was enabled via the EVAL-STORE-SOURCE-FORM policy
;;; (as is the default).
(defun %simple-fun-lexpr (simple-fun)
  (declare (simple-fun simple-fun))
  (let ((info (%simple-fun-info simple-fun)))
    (typecase info
      (fun-src (fun-src-form info))
      ((cons (not string)) info))))

(defun (setf %simple-fun-doc) (doc simple-fun)
  (declare (type (or null string) doc)
           (simple-fun simple-fun))
  (let ((info (%simple-fun-info simple-fun)))
    (setf (%simple-fun-info simple-fun)
          (typecase info
            ((or null string) doc)
            (simple-vector
             (if doc (cons doc info) info))
            ((cons string)
             (if doc (rplaca info doc) (cdr info)))
            (fun-src
             (setf (fun-src-doc info) doc)
             info)
            ((cons (not string))
             (if doc (make-fun-src info doc nil) info))
            (t
             (bug "bogus INFO for ~S: ~S" simple-fun info)))))
  doc)

(defun %simple-fun-next (simple-fun) ; DO NOT USE IN NEW CODE
  (%code-entry-point (fun-code-header simple-fun)
                     (1+ (%simple-fun-index simple-fun))))

;;; Return the number of bytes to subtract from the untagged address of SIMPLE-FUN
;;; to obtain the untagged address of its code component.
;;; Not to be confused with SIMPLE-FUN-CODE-OFFSET which is a constant.
;;; See also CODE-FROM-FUNCTION.
(declaim (inline %fun-code-offset))
(defun %fun-code-offset (simple-fun)
  (declare (type simple-fun simple-fun))
  ;; A fun header can't really point backwards this many words, but it's ok-
  ;; the point is to mask out the layout pointer bits (if compact-instance-header).
  ;; The largest representable code size (in words) is ~22 bits for 32-bit words.
  (ash (ldb (byte 24 0) (fun-header-data simple-fun)) sb-vm:word-shift))

;;;; CODE-COMPONENT

(defun %code-entry-points (code-obj) ; DO NOT USE IN NEW CODE
  (%code-entry-point code-obj 0))

(declaim (inline code-obj-is-filler-p))
(defun code-obj-is-filler-p (code-obj)
  ;; See also HOLE-P in the allocator (same thing but using SAPs)
  ;; and filler_obj_p() in the C code
  (eql (sb-vm::%code-boxed-size code-obj) 0))

#+(or sparc alpha hppa ppc64)
(defun code-trailer-ref (code offset)
  (with-pinned-objects (code)
    (sap-ref-32 (int-sap (get-lisp-obj-address code))
                (+ (code-object-size code) offset (- sb-vm:other-pointer-lowtag)))))

;;; The last 'uint16' in the object holds the trailer length (see 'src/runtime/code.h')
;;; but do not attempt to read it if the object is a filler.
(declaim (inline code-trailer-len))
(defun code-trailer-len (code-obj)
  (if (code-obj-is-filler-p code-obj)
      0
      (let ((word (code-trailer-ref code-obj -4)))
        ;; TRAILER-REF returns 4-byte quantities. Extract a two-byte quantity.
        #+little-endian (ldb (byte 16 16) word)
        #+big-endian    (ldb (byte 16  0) word))))

;;; The fun-table-count is a uint16_t immediately preceding the trailer length
;;; containing two subfields:
;;;  12 bits for the number of simple-funs in the code component
;;;   4 bits for the number of pad bytes added to align the fun-offset-table
(declaim (inline code-fun-table-count))
(defun code-fun-table-count (code-obj)
  (if (eql (code-trailer-len code-obj) 0)
      0
      (let ((word (code-trailer-ref code-obj -4)))
        ;; TRAILER-REF returns 4-byte quantities. Extract a two-byte quantity.
        #+little-endian (ldb (byte 16  0) word)
        #+big-endian    (ldb (byte 16 16) word))))

;;; Return the number of simple-funs in CODE-OBJ
;;; Keep in sync with C function code_n_funs()
(defun code-n-entries (code-obj)
  (declare (type code-component code-obj))
  (ash (code-fun-table-count code-obj) -4))

;;; Return the offset in bytes from (CODE-INSTRUCTIONS CODE-OBJ)
;;; to its FUN-INDEXth function.
(declaim (inline %code-fun-offset))
(defun %code-fun-offset (code-obj fun-index)
  (declare ((unsigned-byte 12) fun-index))
  (code-trailer-ref code-obj (* -4 (+ fun-index 2))))

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
(declaim (inline %code-text-size))
(defun %code-text-size (code-obj)
  (- (%code-code-size code-obj)
     (code-trailer-len code-obj)
     ;; Subtract between 0 and 15 bytes of padding
     (logand (code-fun-table-count code-obj) #xf)))

(defun %code-entry-point (code-obj fun-index)
  (declare (type (unsigned-byte 16) fun-index))
  (when (< fun-index (code-n-entries code-obj))
    (truly-the function
      (values (%primitive sb-c:compute-fun code-obj
                          (%code-fun-offset code-obj fun-index))))))

(defun code-entry-points (code-obj) ; FIXME: obsolete
  (let ((a (make-array (code-n-entries code-obj))))
    (dotimes (i (length a) a)
      (setf (aref a i) (%code-entry-point code-obj i)))))

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
              (ash sb-vm:simple-fun-code-offset sb-vm:word-shift)))
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
         (ash sb-vm:simple-fun-code-offset sb-vm:word-shift))))

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
           ;; ANSI specification of FUNCALL says that this should be
           ;; an error of type UNDEFINED-FUNCTION, not just SIMPLE-ERROR.
           ;; SPECIAL-FORM-FUNCTION is a subtype of UNDEFINED-FUNCTION.
           (error (if (special-operator-p symbol)
                      'special-form-function
                      'undefined-function)
                  :name symbol))
          t
          fun-name)))
    ;; In most cases, install the guard closure in the usual way.
    #-immobile-code (setf (fdefn-fun (find-or-create-fdefn symbol)) closure)

    ;; Do something slightly different for immobile code: fmakunbound, causing the FUN
    ;; slot to become NIL, and RAW-ADDR to contain a call instruction; then overwrite
    ;; NIL with the above closure. This is better than assigning a closure, because
    ;; assigning a closure into an fdefn generally conses a new closure trampoline.
    ;; (The CALL goes to undefined tramp which pops the stack to deduce the fdefn)
    #+immobile-code
    (let ((fdefn (find-or-create-fdefn symbol)))
      (fdefn-makunbound fdefn)
      (%primitive sb-vm::set-fdefn-fun ; This invokes TOUCH-GC-CARD
                  fdefn closure
                  (sap-ref-word (int-sap (get-lisp-obj-address fdefn))
                                (- (ash sb-vm:fdefn-raw-addr-slot sb-vm:word-shift)
                                   sb-vm:other-pointer-lowtag))))))

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

(in-package "SB-C")

;;; This is target-only code, so doesn't belong in 'debug-info.lisp'
(flet ((unpack-tlf-num+offset (integer &aux (bytepos 0))
         (flet ((unpack-1 ()
                  (let ((shift 0) (acc 0))
                    (declare (notinline sb-kernel:%ldb)) ; lp#1573398
                    (loop
                     (let ((byte (ldb (byte 8 bytepos) integer)))
                       (incf bytepos 8)
                       (setf acc (logior acc (ash (logand byte #x7f) shift)))
                       (if (logtest byte #x80)
                           (incf shift 7)
                           (return acc)))))))
           (let ((v1 (unpack-1))
                 (v2 (unpack-1)))
             (values (if (eql v1 0) nil (1- v1))
                     (if (eql v2 0) nil (1- v2)))))))
  (defun compiled-debug-info-tlf-number (cdi)
    (nth-value 0 (unpack-tlf-num+offset
                  (compiled-debug-info-tlf-num+offset cdi))))

  (defun compiled-debug-info-char-offset (cdi)
    (nth-value 1 (unpack-tlf-num+offset
                  (compiled-debug-info-tlf-num+offset cdi)))))
