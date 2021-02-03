;;;; FOP definitions

(in-package "SB-FASL")

;;; Bind STACK-VAR and PTR-VAR to the start of a subsequence of
;;; the fop stack of length COUNT, then execute BODY.
;;; Within the body, FOP-STACK-REF is used in lieu of SVREF
;;; to elide bounds checking.
(defmacro with-fop-stack (((stack-var &optional stack-expr) ptr-var count)
                          &body body)
  `(macrolet ((fop-stack-ref (i)
                `(locally
                     #-sb-xc-host
                     (declare (optimize (sb-c::insert-array-bounds-checks 0)))
                   (svref ,',stack-var (truly-the index ,i)))))
     (let* (,@(when stack-expr
                (list `(,stack-var (the simple-vector ,stack-expr))))
            (,ptr-var (truly-the index (fop-stack-pop-n ,stack-var ,count))))
       ,@body)))

;;; Define NAME as a fasl operation, with op-code FOP-CODE.
;;; PUSHP describes what the body does to the fop stack:
;;;   T   - The result of the body is pushed on the fop stack.
;;;   NIL - The result of the body is discarded.
;;; In either case, the body is permitted to pop the stack.
;;;
(defmacro define-fop (fop-code &rest stuff)
  (multiple-value-bind (name allowp operands stack-args pushp forms)
      (let ((allowp (if (eq (car stuff) :not-host)
                        (progn (pop stuff) (or #-sb-xc-host t))
                        t)))
        (destructuring-bind ((name &optional arglist (pushp t)) . forms) stuff
          (aver (member pushp '(nil t)))
          (multiple-value-bind (operands stack-args)
              (if (atom (car arglist))
                  (values nil arglist)
                  (ecase (caar arglist)
                    (:operands (values (cdar arglist) (cdr arglist)))))
            (assert (<= (length operands) 3))
            (values name allowp operands stack-args pushp forms))))
    `(progn
       (defun ,name (.fasl-input. ,@operands)
         (declare (ignorable .fasl-input.))
         ,@(if allowp
               `((macrolet
                   ((fasl-input () '(truly-the fasl-input .fasl-input.))
                    (fasl-input-stream () '(%fasl-input-stream (fasl-input)))
                    (operand-stack () '(%fasl-input-stack (fasl-input)))
                    (skip-until () '(%fasl-input-skip-until (fasl-input))))
                  ,@(if (null stack-args)
                        forms
                        (with-unique-names (stack ptr)
                         `((with-fop-stack ((,stack (operand-stack))
                                            ,ptr ,(length stack-args))
                             (multiple-value-bind ,stack-args
                                 (values ,@(loop for i below (length stack-args)
                                                 collect `(fop-stack-ref (+ ,ptr ,i))))
                               ,@forms)))))))
               `((declare (ignore ,@operands))
                 (error ,(format nil "Not-host fop invoked: ~A" name)))))
       (!%define-fop ',name ,fop-code ,(length operands) ,(if pushp 1 0)))))

(defun !%define-fop (name opcode n-operands pushp)
  (declare (type (mod 4) n-operands))
  (let ((function (svref **fop-funs** opcode)))
    (when (functionp function)
      (let ((oname (nth-value 2 (function-lambda-expression function))))
        (when (and oname (not (eq oname name)))
          (error "fop ~S with opcode ~D conflicts with fop ~S."
                 name opcode oname))))
    (let ((existing-opcode (get name 'opcode)))
      (when (and existing-opcode (/= existing-opcode opcode))
        (error "multiple codes for fop name ~S: ~D and ~D"
               name opcode existing-opcode)))
    (setf (get name 'opcode) opcode
          (svref **fop-funs** opcode) (symbol-function name)
          (aref (car **fop-signatures**) opcode) n-operands
          (sbit (cdr **fop-signatures**) opcode) pushp))
  name)

;;; Compatibity macros that allow some fops to share the identical
;;; body between genesis and the target code.
#-sb-xc-host
(progn
  (defmacro cold-cons (x y) `(cons ,x ,y))
  (defmacro number-to-core (x) x)
  (defmacro make-character-descriptor (x) `(code-char ,x)))

;;; helper functions for reading string values from FASL files: sort
;;; of like READ-SEQUENCE specialized for files of (UNSIGNED-BYTE 8),
;;; with an automatic conversion from (UNSIGNED-BYTE 8) into CHARACTER
;;; for each element read

;;; Variation 1: character string, transfer elements of type (unsigned-byte 8)
;;: [Can we eliminate this one?]
(defun read-string-as-bytes (stream string &optional (length (length string)))
  (declare (type (simple-array character (*)) string)
           (type index length)
           (optimize speed))
  (with-fast-read-byte ((unsigned-byte 8) stream)
    (dotimes (i length)
      (setf (aref string i)
            (sb-xc:code-char (fast-read-byte)))))
  string)
;;; Variation 2: base-string, transfer elements of type (unsigned-byte 8)
(defun read-base-string-as-bytes (stream string &optional (length (length string)))
  (declare (type (simple-array base-char (*)) string)
           (type index length)
           (optimize speed))
  (with-fast-read-byte ((unsigned-byte 8) stream)
    (dotimes (i length)
      (setf (aref string i)
            (sb-xc:code-char (fast-read-byte)))))
  string)
;;; Variation 3: character-string, transfer elements of type varint
(defun read-char-string-as-varints
    (stream string &optional (length (length string)))
  (declare (type (simple-array character (*)) string)
           (type index length)
           (optimize speed))
  (with-fast-read-byte ((unsigned-byte 8) stream)
    ;; OAOO violation- This repeats code in DEFINE-READ-VAR-INTEGER in 'debug-var-io'
    ;; but there isn't a good expansion of that macro that would operate on a stream
    ;; (which is ok in itself) but also that would entail only a single wrapping of
    ;; WITH-FAST-READ-BYTE for all work.
    ;; i.e. we don't want to update the stream slots after each varint.
    (flet ((read-varint ()
             (loop for shift :of-type (integer 0 28) from 0 by 7 ; position in integer
                   for octet = (fast-read-byte)
                   for accum :of-type (mod #.char-code-limit)
                     = (logand octet #x7F)
                     then (logior (ash (logand octet #x7F) shift) accum)
                   unless (logbitp 7 octet) return accum)))
      (dotimes (i length)
        (setf (aref string i)
              (sb-xc:code-char (read-varint))))))
  string)

;;;; miscellaneous fops

(define-fop 0 (fop-nop () nil))
(define-fop 1 (fop-pop (x) nil) (push-fop-table x (fasl-input)))
(define-fop 2 (fop-empty-list) nil)
(define-fop 3 (fop-truth) t)
(define-fop 4 (fop-push ((:operands index)))
  (ref-fop-table (fasl-input) index))
(define-fop 5 (fop-move-to-table (x))
  (push-fop-table x (fasl-input))
  x)

(define-fop 66 :not-host (fop-misc-trap)
  (make-unbound-marker))

(define-fop 76 (fop-character ((:operands char-code)))
  (make-character-descriptor char-code))

;; %MAKE-INSTANCE does not exist on the host.
(define-fop 48 :not-host (fop-struct ((:operands size) layout))
  (let ((res (%make-instance size)) ; number of words excluding header
        ;; Discount the layout from number of user-visible words.
        (n-data-words (- size sb-vm:instance-data-start)))
    (setf (%instance-layout res) layout)
    (with-fop-stack ((stack (operand-stack)) ptr n-data-words)
      (declare (type index ptr))
      (let ((bitmap (layout-bitmap layout)))
        ;; Values on the stack are in the same order as in the structure itself.
        (do ((i sb-vm:instance-data-start (1+ i)))
            ((>= i size))
          (declare (type index i))
          (let ((val (fop-stack-ref ptr)))
            (if (logbitp i bitmap)
                (setf (%instance-ref res i) val)
                (setf (%raw-instance-ref/word res i) val))
            (incf ptr)))))
    res))

;;; Symbol-like entities
(define-fop 49 :not-host (fop-debug-name-marker ((:operands kind)))
  (ecase kind
   (1 sb-c::*debug-name-sharp*)
   (2 sb-c::*debug-name-ellipsis*)))

(define-fop 45 :not-host (fop-layout ((:operands depthoid flags length)
                                       name bitmap inherits))
  (decf depthoid) ; was bumped by 1 since non-stack args can't encode negatives
  (sb-kernel::load-layout name depthoid inherits length bitmap flags))

;; Allocate a CLOS object. This is used when the compiler detects that
;; MAKE-LOAD-FORM returned a simple use of MAKE-LOAD-FORM-SAVING-SLOTS,
;; or possibly a hand-written equivalent (however unlikely).
(define-fop 68 :not-host (fop-allocate-instance (name) nil)
  (let ((instance (allocate-instance (find-class (the symbol name)))))
    (push-fop-table instance (fasl-input))))

;; Fill in object slots as dictated by the second return value from
;; MAKE-LOAD-FORM-SAVING-SLOTS.
(define-fop 69 :not-host (fop-set-slot-values ((:operands n-slots) slot-names obj) nil)
  (let* ((stack (operand-stack))
         (ptr (fop-stack-pop-n stack n-slots)))
      (dotimes (i n-slots)
        (let ((val (svref stack (+ ptr i)))
              (slot-name (pop slot-names)))
          (if (unbound-marker-p val)
              ;; SLOT-MAKUNBOUND-USING-CLASS might do something nonstandard.
              (slot-makunbound obj slot-name)
              (setf (slot-value obj slot-name) val))))))

(define-fop 64 (fop-end-group () nil)
  (throw 'fasl-group-end t))

(define-fop 62 (fop-verify-table-size ((:operands expected-index)) nil)
  (unless (= (svref (%fasl-input-table (fasl-input)) 0) expected-index)
    (bug "fasl table of improper size")))
(define-fop 63 (fop-verify-empty-stack () nil)
  (unless (fop-stack-empty-p (operand-stack))
    (bug "fasl stack not empty when it should be")))

;;;; fops for loading symbols

(defstruct (undefined-package (:copier nil))
  (error nil :read-only t))
(declaim (freeze-type undefined-package))

;;; Cold load has its own implementation of all symbol fops,
;;; but we have to execute define-fop now to assign their numbers.
;;;
;;; Any symbols created by the loader must have their SYMBOL-HASH computed.
;;; This is a requirement for the CASE macro to work. When code is compiled
;;; to memory, symbols in the expansion are subject to SXHASH, so all is well.
;;; When loaded, even uninterned symbols need a hash.
;;; Interned symbols automatically get a precomputed hash.
(labels #+sb-xc-host ()
        #-sb-xc-host
        ((read-symbol-name (length+flag fasl-input)
           (let* ((namelen (ash (the fixnum length+flag) -1))
                  (base-p (logand length+flag 1))
                  (elt-type (if (eql base-p 1) 'base-char 'character))
                  (buffer (%fasl-input-name-buffer fasl-input))
                  (string (the string (svref buffer base-p))))
             (when (< (length string) namelen) ; grow
               (setf string (make-string namelen :element-type elt-type)
                     (svref buffer base-p) string))
             (funcall (if (eql base-p 1)
                          'read-base-string-as-bytes
                          'read-char-string-as-varints)
                      (%fasl-input-stream fasl-input) string namelen)
             (values string namelen elt-type)))
         (aux-fop-intern (length+flag package fasl-input)
           (multiple-value-bind (name length elt-type)
               (read-symbol-name length+flag fasl-input)
             (if (undefined-package-p package)
                 (error 'simple-package-error
                        :format-control "Error finding package for symbol ~s:~% ~a"
                        :format-arguments
                        (list (subseq name 0 length)
                              (undefined-package-error package)))
                 (push-fop-table (%intern name length package elt-type t)
                                 fasl-input))))
         (ensure-hashed (symbol)
           ;; ENSURE-SYMBOL-HASH when vop-translated is flushable since it is
           ;; conceptually just a slot reader, however its actual effect is to fill in
           ;; the hash if absent, so it's not quite flushable when called expressly
           ;; to fill in the slot. In this case we need a full call to ENSURE-SYMBOL-HASH
           ;; to ensure the side-effect happens.
           ;; Careful if changing this again. There'a regression test thank goodness.
           (declare (notinline ensure-symbol-hash))
           (ensure-symbol-hash symbol)
           symbol))

  (define-fop 77 :not-host (fop-lisp-symbol-save ((:operands length+flag)))
    (aux-fop-intern length+flag *cl-package* (fasl-input)))
  (define-fop 78 :not-host (fop-keyword-symbol-save ((:operands length+flag)))
    (aux-fop-intern length+flag *keyword-package* (fasl-input)))
  (define-fop 79 :not-host (fop-symbol-in-package-save ((:operands length+flag pkg-index)))
    (aux-fop-intern length+flag (ref-fop-table (fasl-input) pkg-index) (fasl-input)))

  (define-fop 80 :not-host (fop-uninterned-symbol-save ((:operands length+flag)))
    (multiple-value-bind (name len) (read-symbol-name length+flag (fasl-input))
      (push-fop-table (ensure-hashed (make-symbol (subseq name 0 len)))
                      (fasl-input))))

  (define-fop 81 :not-host (fop-copy-symbol-save ((:operands table-index)))
    (push-fop-table (ensure-hashed
                     (copy-symbol (ref-fop-table (fasl-input) table-index)))
                    (fasl-input))))

(define-fop 82 (fop-package (pkg-designator))
  (find-undeleted-package-or-lose pkg-designator))

(define-fop 83 :not-host (fop-named-package-save ((:operands length)) nil)
  (let ((package-name (make-string length)))
    (read-char-string-as-varints (fasl-input-stream) package-name)
    (push-fop-table
     (handler-case (find-undeleted-package-or-lose package-name)
       (simple-package-error (c)
         (make-undefined-package :error (princ-to-string c))))
     (fasl-input))))

;;;; fops for loading numbers

;;; Load a signed integer LENGTH bytes long from FASL-INPUT-STREAM.
(defun load-s-integer (length fasl-input-stream)
  (declare (fixnum length)
           (optimize speed)
           #-sb-xc-host (muffle-conditions compiler-note))
  (with-fast-read-byte ((unsigned-byte 8) fasl-input-stream)
    (do* ((index length (1- index))
          (byte 0 (fast-read-byte))
          (result 0 (+ result (ash byte bits)))
          (bits 0 (+ bits 8)))
         ((= index 0)
          (if (logbitp 7 byte)          ; look at sign bit
              (- result (ash 1 bits))
              result))
      (declare (fixnum index byte bits)))))

(define-fop 36 (fop-integer ((:operands n-bytes)))
  (number-to-core (load-s-integer n-bytes (fasl-input-stream))))

(define-fop 33 :not-host (fop-word-pointer)
  (with-fast-read-byte ((unsigned-byte 8) (fasl-input-stream))
    (int-sap (fast-read-u-integer #.sb-vm:n-word-bytes))))

(define-fop 34 (fop-word-integer)
  (with-fast-read-byte ((unsigned-byte 8) (fasl-input-stream))
    (number-to-core (fast-read-s-integer #.sb-vm:n-word-bytes))))

(define-fop 35 (fop-byte-integer)
  ;; FIXME: WITH-FAST-READ-BYTE for exactly 1 byte is not really faster/better
  ;; than regular READ-BYTE. The expansion of READ-ARG corroborates this claim.
  (with-fast-read-byte ((unsigned-byte 8) (fasl-input-stream))
    (number-to-core (fast-read-s-integer 1))))

;; There's a long tail to the distribution of FOP-BYTE-INTEGER uses,
;; but these 4 seem to account for about half of them.
(define-fop 37 (fop-int-const0) (number-to-core 0))
(define-fop 38 (fop-int-const1) (number-to-core 1))
(define-fop 39 (fop-int-const2) (number-to-core 2))
(define-fop 40 (fop-int-const-neg1) (number-to-core -1))

(define-fop 70 (fop-ratio (num den))
  #+sb-xc-host (number-pair-to-core num den sb-vm:ratio-widetag)
  #-sb-xc-host (%make-ratio num den))

(define-fop 71 (fop-complex (realpart imagpart))
  #+sb-xc-host (number-pair-to-core realpart imagpart sb-vm:complex-widetag)
  #-sb-xc-host (%make-complex realpart imagpart))

(macrolet ((fast-read-single-float ()
             '(make-single-float (fast-read-s-integer 4)))
           (fast-read-double-float ()
             '(let ((lo (fast-read-u-integer 4)))
               (make-double-float (fast-read-s-integer 4) lo))))
  (macrolet ((define-complex-fop (opcode name type)
               (let ((reader (symbolicate "FAST-READ-" type)))
                 `(define-fop ,opcode (,name)
                    (with-fast-read-byte ((unsigned-byte 8) (fasl-input-stream))
                      (number-to-core (complex (,reader) (,reader)))))))
             (define-float-fop (opcode name type)
               (let ((reader (symbolicate "FAST-READ-" type)))
                 `(define-fop ,opcode (,name)
                    (with-fast-read-byte ((unsigned-byte 8) (fasl-input-stream))
                      (number-to-core (,reader)))))))
    (define-complex-fop 72 fop-complex-single-float single-float)
    (define-complex-fop 73 fop-complex-double-float double-float)
    #+long-float
    (define-complex-fop 67 fop-complex-long-float long-float)
    (define-float-fop 46 fop-single-float single-float)
    (define-float-fop 47 fop-double-float double-float)
    #+long-float
    (define-float-fop 52 fop-long-float long-float)))

#+sb-simd-pack
(define-fop 88 :not-host (fop-simd-pack)
  (with-fast-read-byte ((unsigned-byte 8) (fasl-input-stream))
    (let ((tag (fast-read-s-integer 8)))
      (cond #+sb-simd-pack-256
            ((logbitp 2 tag)
             (%make-simd-pack-256 (logand tag #b11)
                                  (fast-read-u-integer 8)
                                  (fast-read-u-integer 8)
                                  (fast-read-u-integer 8)
                                  (fast-read-u-integer 8)))
            (t
             (%make-simd-pack tag
                              (fast-read-u-integer 8)
                              (fast-read-u-integer 8)))))))

;;;; loading lists

(defun fop-list (fasl-input n &aux (stack (%fasl-input-stack fasl-input)))
  (declare (type index n)
           (optimize (speed 3)))
  (with-fop-stack ((stack) ptr n)
    (do* ((i (+ ptr n) (1- i))
          (res () (cold-cons (fop-stack-ref i) res)))
         ((= i ptr) res)
      (declare (type index i)))))
(defun fop-list* (fasl-input n &aux (stack (%fasl-input-stack fasl-input)))
  (declare (type index n)
           (optimize (speed 3)))
  (with-fop-stack ((stack) ptr (1+ n))
    (do* ((i (+ ptr n) (1- i))
          (res (fop-stack-ref (+ ptr n))
               (cold-cons (fop-stack-ref i) res)))
         ((= i ptr) res)
      (declare (type index i)))))

;;;; fops for loading arrays

(define-fop 100 :not-host (fop-base-string ((:operands length)))
  (logically-readonlyize
   (read-base-string-as-bytes (fasl-input-stream)
                              (make-string length :element-type 'base-char))))

(define-fop 101 :not-host (fop-character-string ((:operands length)))
  (logically-readonlyize
   (read-char-string-as-varints (fasl-input-stream) (make-string length))))

(define-fop 92 (fop-vector ((:operands size)))
  (if (zerop size)
      #()
      (let ((res (make-array size))
            (stack (operand-stack)))
        (declare (fixnum size))
        (let ((ptr (fop-stack-pop-n stack size)))
          (replace res stack :start2 ptr))
        (logically-readonlyize res))))

;; No MAKE-ARRAY-HEADER on host
(define-fop 89 :not-host (fop-array ((:operands rank) vec))
  (let ((length (length vec))
        (res (make-array-header sb-vm:simple-array-widetag rank)))
    (declare (simple-array vec)
             (type (unsigned-byte #.(- sb-vm:n-word-bits sb-vm:n-widetag-bits)) rank))
    (set-array-header res vec length nil 0 (fop-list (fasl-input) rank) nil t)
    res))

(defglobal **saetp-bits-per-length**
    (let ((array (make-array (1+ sb-vm:widetag-mask)
                             :element-type '(unsigned-byte 8)
                             :initial-element 255)))
      (loop for saetp across sb-vm:*specialized-array-element-type-properties*
            do
            (setf (aref array (sb-vm:saetp-typecode saetp))
                  (sb-vm:saetp-n-bits saetp)))
      array)
    "255 means bad entry.")
(declaim (type (simple-array (unsigned-byte 8) (#.(1+ sb-vm:widetag-mask)))
               **saetp-bits-per-length**))

(define-fop 43 (fop-spec-vector  ((:operands length)))
  (let* ((widetag (read-byte-arg (fasl-input-stream)))
         (bits-per-length (aref **saetp-bits-per-length** widetag))
         (bits (progn (aver (< bits-per-length 255))
                      (* length bits-per-length)))
         (bytes (ceiling bits sb-vm:n-byte-bits))
         (words (ceiling bytes sb-vm:n-word-bytes))
         (vector
          (progn (aver (/= widetag sb-vm:simple-vector-widetag))
                 (logically-readonlyize
                  (allocate-vector widetag length words)))))
    (declare (type index length bytes words)
             (type word bits))
    (read-n-bytes (fasl-input-stream) vector 0 bytes)
    vector))

(define-fop 53 (fop-eval (expr)) ; This seems to be unused
  (if (skip-until)
      expr
      (eval expr)))

(define-fop 54 (fop-eval-for-effect (expr) nil) ; This seems to be unused
  (unless (skip-until)
    (eval expr))
  nil)

(defun fop-funcall* (argc stack skipping)
  (with-fop-stack ((stack) ptr (1+ argc))
    (unless skipping
      (do ((i (+ ptr argc))
           (args))
          ((= i ptr) (apply (fop-stack-ref i) args))
        (declare (type index i))
        (push (fop-stack-ref i) args)
        (decf i)))))

(define-fop 55 (fop-funcall ((:operands n)))
  (fop-funcall* n (operand-stack) (skip-until)))
(define-fop 56 (fop-funcall-for-effect ((:operands n)) nil)
  (fop-funcall* n (operand-stack) (skip-until)))

;;; For LOAD-TIME-VALUE which is used for MAKE-LOAD-FORM
(define-fop 57 (fop-funcall-no-skip ((:operands n)))
  (fop-funcall* n (operand-stack) nil))

;;;; fops for fixing up circularities

(define-fop 11 (fop-rplaca ((:operands tbl-slot idx) val) nil)
  (let ((obj (ref-fop-table (fasl-input) tbl-slot)))
    (setf (car (nthcdr idx obj)) val)))

(define-fop 12 (fop-rplacd ((:operands tbl-slot idx) val) nil)
  (let ((obj (ref-fop-table (fasl-input) tbl-slot)))
    (setf (cdr (nthcdr idx obj)) val)))

(define-fop 13 (fop-svset ((:operands tbl-slot idx) val) nil)
  (setf (svref (ref-fop-table (fasl-input) tbl-slot) idx) val))

(define-fop 14 :not-host (fop-structset ((:operands tbl-slot idx) val) nil)
  (setf (%instance-ref (ref-fop-table (fasl-input) tbl-slot) idx) val))

(define-fop 15 (fop-nthcdr ((:operands n) obj))
  (nthcdr n obj))

;;;; fops for loading functions

;;; (In CMU CL there was a FOP-CODE-FORMAT (47) which was
;;; conventionally placed at the beginning of each fasl file to test
;;; for compatibility between the fasl file and the CMU CL which
;;; loaded it. In SBCL, this functionality has been replaced by
;;; putting the implementation and version in required fields in the
;;; fasl file header.)

(define-fop 16 :not-host (fop-load-code ((:operands header n-code-bytes n-fixups)))
  (let* ((n-named-calls (read-unsigned-byte-32-arg (fasl-input-stream)))
         (n-boxed-words (ash header -1))
         (n-constants (- n-boxed-words sb-vm:code-constants-offset)))
    ;; stack has (at least) N-CONSTANTS words plus debug-info
    (with-fop-stack ((stack (operand-stack)) ptr (1+ n-constants))
      (let* ((debug-info-index (+ ptr n-constants))
             (n-boxed-words (+ sb-vm:code-constants-offset n-constants))
             (code (sb-c:allocate-code-object
                    (if (oddp header) :immobile :dynamic)
                    n-named-calls
                    (align-up n-boxed-words sb-c::code-boxed-words-align)
                    n-code-bytes)))
        (with-pinned-objects (code)
          ;; * DO * NOT * SEPARATE * THESE * STEPS *
          ;; For a full explanation, refer to the comment above MAKE-CORE-COMPONENT
          ;; concerning the corresponding use therein of WITH-PINNED-OBJECTS etc.
          #-darwin-jit
          (read-n-bytes (fasl-input-stream) (code-instructions code) 0 n-code-bytes)
          #+darwin-jit
          (let ((buf (make-array n-code-bytes :element-type '(unsigned-byte 8))))
            (read-n-bytes (fasl-input-stream) buf 0 n-code-bytes)
            (with-pinned-objects (buf)
              (sb-vm::jit-memcpy (code-instructions code) (vector-sap buf) n-code-bytes)))
          ;; Serial# shares a word with the jump-table word count,
          ;; so we can't assign serial# until after all raw bytes are copied in.
          (sb-c::assign-code-serialno code)
          (sb-thread:barrier (:write))
          ;; Assign debug-info last. A code object that has no debug-info will never
          ;; have its fun table accessed in conservative_root_p() or pin_object().
          (setf (%code-debug-info code) (svref stack debug-info-index))
          ;; Boxed constants can be assigned only after figuring out where the range
          ;; of implicitly tagged words is, which requires knowing how many functions
          ;; are in the code component, which requires reading the code trailer.
          (let* ((fdefns-start (sb-impl::code-fdefns-start-index code))
                 (fdefns-end (1- (+ fdefns-start n-named-calls)))) ; inclusive bound
            (loop for i of-type index from sb-vm:code-constants-offset
                  for j of-type index from ptr below debug-info-index
                  do (let ((constant (svref stack j)))
                       (if (<= fdefns-start i fdefns-end)
                           (sb-c::set-code-fdefn code i constant)
                           (setf (code-header-ref code i) constant)))))
          ;; Now apply fixups. The fixups to perform are popped from the fasl stack.
          (sb-c::apply-fasl-fixups stack code n-fixups))
        #-sb-xc-host
        (when (typep (code-header-ref code (1- n-boxed-words))
                     '(cons (eql sb-c::coverage-map)))
          ;; Record this in the global list of coverage-instrumented code.
          (atomic-push (make-weak-pointer code) (cdr *code-coverage-info*)))
        code))))

;; this gets you an #<fdefn> object, not the result of (FDEFINITION x)
;; cold-loader uses COLD-FDEFINITION-OBJECT instead.
(define-fop 17 :not-host (fop-fdefn (name))
  (awhen (deprecated-thing-p 'function name) ; returns the stage of deprecation
    (pushnew (list* it name :function)
             (%fasl-input-deprecated-stuff (fasl-input)) :test 'equal))
  (find-or-create-fdefn name))

(define-fop 18 :not-host (fop-known-fun (name))
  (%coerce-name-to-fun name))

;; This FOP is only encountered in cross-compiled files for cold-load.
(define-fop 74 :not-host (fop-fset (name fn) nil)
  ;; Ordinary, not-for-cold-load code shouldn't need to mess with this
  ;; at all, since it's only used as part of the conspiracy between
  ;; the cross-compiler and GENESIS to statically link FDEFINITIONs
  ;; for cold init.
  (warn "~@<FOP-FSET seen in ordinary load (not cold load) -- quite
strange! ~ If you didn't do something strange to cause this, please
report it as a ~ bug.~:@>")
  ;; Unlike CMU CL, we don't treat this as a no-op in ordinary code.
  ;; If the user (or, more likely, developer) is trying to reload
  ;; compiled-for-cold-load code into a warm SBCL, we'll do a warm
  ;; assignment. (This is partly for abstract tidiness, since the warm
  ;; assignment is the closest analogy to what happens at cold load,
  ;; and partly because otherwise our compiled-for-cold-load code will
  ;; fail, since in SBCL things like compiled-for-cold-load %DEFUN
  ;; depend more strongly than in CMU CL on FOP-FSET actually doing
  ;; something.)
  (setf (fdefinition name) fn))

;;; Modify a slot of the code boxed constants.
(define-fop 19 (fop-alter-code ((:operands index) code value) nil)
  (flet (#+sb-xc-host
         ((setf code-header-ref) (value code index)
            (write-wordindexed code index value)))
    (setf (code-header-ref code index) value)
    (values)))

(define-fop 20 :not-host (fop-fun-entry ((:operands fun-index) code-object))
  (%code-entry-point code-object fun-index))

;;;; assemblerish fops

(define-fop 21 (fop-assembler-code)
  (error "cannot load assembler code except at cold load"))

;;; FOPs needed for implementing an IF operator in a FASL

;;; Skip until a FOP-MAYBE-STOP-SKIPPING with the same POSITION is
;;; executed. While skipping, we execute most FOPs normally, except
;;; for ones that a) funcall/eval b) start skipping. This needs to
;;; be done to ensure that the fop table gets populated correctly
;;; regardless of the execution path.
(define-fop 6 (fop-skip ((:operands position)) nil)
  (unless (skip-until)
    (setf (skip-until) position))
  (values))

;;; As before, but only start skipping if the top of the FOP stack is NIL.
(define-fop 7 (fop-skip-if-false ((:operands position) condition) nil)
  (unless (or condition (skip-until))
    (setf (skip-until) position))
  (values))

;;; If skipping, pop the top of the stack and discard it. Needed for
;;; ensuring that the stack stays balanced when skipping.
(define-fop 8 (fop-drop-if-skipping () nil)
  (when (skip-until)
    (fop-stack-pop-n (operand-stack) 1))
  (values))

;;; If skipping, push a dummy value on the stack. Needed for
;;; ensuring that the stack stays balanced when skipping.
(define-fop 9 (fop-push-nil-if-skipping () nil)
  (when (skip-until)
    (push-fop-stack nil (fasl-input)))
  (values))

;;; Stop skipping if the top of the stack matches SKIP-UNTIL
(define-fop 10 (fop-maybe-stop-skipping ((:operands label)) nil)
  (when (eql (skip-until) label)
    (setf (skip-until) nil))
  (values))

;;; Primordial layouts.
(macrolet ((frob (&rest specs)
             `(progn
                (defun known-layout-fop (name)
                  (case name
                    ,@(mapcar (lambda (spec) `((,(cadr spec)) ,(car spec)))
                              specs)))
                ,@(mapcar (lambda (spec)
                            `(define-fop ,(car spec)
                                          (,(symbolicate "FOP-LAYOUT-OF-"
                                                         (cadr spec)))
                               ,(find-layout (cadr spec))))
                          specs))))
  (frob (#x68 t)
        (#x69 structure-object)
        (#x6a condition)
        (#x6b definition-source-location)
        (#x6c sb-c::debug-info)
        (#x6d sb-c::compiled-debug-info)
        (#x6e sb-c::debug-source)
        (#x6f defstruct-description)
        (#x70 defstruct-slot-description)
        (#x71 sb-c::debug-fun)
        (#x72 sb-c::compiled-debug-fun)
        (#x73 sb-c::compiled-debug-fun-optional)
        (#x74 sb-c::compiled-debug-fun-more)
        (#x75 sb-c::compiled-debug-fun-external)
        (#x76 sb-c::compiled-debug-fun-toplevel)
        (#x77 sb-c::compiled-debug-fun-cleanup)))
