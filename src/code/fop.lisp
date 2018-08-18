;;;; FOP definitions

(in-package "SB!FASL")

;;; Bind STACK-VAR and PTR-VAR to the start of a subsequence of
;;; the fop stack of length COUNT, then execute BODY.
;;; Within the body, FOP-STACK-REF is used in lieu of SVREF
;;; to elide bounds checking.
(defmacro with-fop-stack (((stack-var &optional stack-expr) ptr-var count)
                          &body body)
  `(macrolet ((fop-stack-ref (i)
                `(locally
                     #-sb-xc-host
                     (declare (optimize (sb!c::insert-array-bounds-checks 0)))
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
(defmacro !define-fop (fop-code &rest stuff)
  (multiple-value-bind (name allowp operands stack-args pushp forms)
      (let ((allowp (if (eq (car stuff) :not-host)
                        (progn (pop stuff) (or #+sb-xc t))
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

;;; a helper function for reading string values from FASL files: sort
;;; of like READ-SEQUENCE specialized for files of (UNSIGNED-BYTE 8),
;;; with an automatic conversion from (UNSIGNED-BYTE 8) into CHARACTER
;;; for each element read
(defun read-string-as-bytes (stream string &optional (length (length string)))
  (declare (type (simple-array character (*)) string)
           (type index length)
           (optimize speed))
  (with-fast-read-byte ((unsigned-byte 8) stream)
    (dotimes (i length)
      (setf (aref string i)
            (sb!xc:code-char (fast-read-byte)))))
  string)
(defun read-base-string-as-bytes (stream string &optional (length (length string)))
  (declare (type (simple-array base-char (*)) string)
           (type index length)
           (optimize speed))
  (with-fast-read-byte ((unsigned-byte 8) stream)
    (dotimes (i length)
      (setf (aref string i)
            (sb!xc:code-char (fast-read-byte)))))
  string)
#!+(and sb-unicode (host-feature sb-xc))
(defun read-string-as-unsigned-byte-32
    (stream string &optional (length (length string)))
  (declare (type (simple-array character (*)) string)
           (type index length)
           (optimize speed))
  (with-fast-read-byte ((unsigned-byte 8) stream)
    (dotimes (i length)
      (setf (aref string i)
            (sb!xc:code-char (fast-read-u-integer 4)))))
  string)

;;;; miscellaneous fops

(!define-fop 0 (fop-nop () nil))
(!define-fop 1 (fop-pop (x) nil) (push-fop-table x (fasl-input)))
(!define-fop 2 (fop-empty-list) nil)
(!define-fop 3 (fop-truth) t)
(!define-fop 4 (fop-push ((:operands index)))
  (ref-fop-table (fasl-input) index))
(!define-fop 9 (fop-move-to-table (x))
  (push-fop-table x (fasl-input))
  x)

(!define-fop 66 :not-host (fop-misc-trap)
  (make-unbound-marker))

(!define-fop 76 (fop-character ((:operands char-code)))
  (code-char char-code))

;; %MAKE-INSTANCE does not exist on the host.
(!define-fop 48 :not-host (fop-struct ((:operands size) layout))
  (let ((res (%make-instance size)) ; number of words excluding header
        ;; Discount the layout from number of user-visible words.
        (n-data-words (- size sb!vm:instance-data-start)))
    (setf (%instance-layout res) layout)
    (with-fop-stack ((stack (operand-stack)) ptr n-data-words)
      (declare (type index ptr))
      (let ((bitmap (layout-bitmap layout)))
        ;; Values on the stack are in the same order as in the structure itself.
        (do ((i sb!vm:instance-data-start (1+ i)))
            ((>= i size))
          (declare (type index i))
          (let ((val (fop-stack-ref ptr)))
            (if (logbitp i bitmap)
                (setf (%instance-ref res i) val)
                (setf (%raw-instance-ref/word res i) val))
            (incf ptr)))))
    res))

(!define-fop 45 :not-host (fop-layout (name inherits depthoid length bitmap))
  (let ((flags
         (case (and (>= (length (the simple-vector inherits)) 2)
                    (elt inherits 1))
           (#.(find-layout 'structure-object) +structure-layout-flag+)
           (#.(find-layout 'condition) +condition-layout-flag+)
           ;; DUMP-LAYOUT avers that +pcl-object-flag+ layouts are never dumped
           (t 0))))
    (find-and-init-or-check-layout name length flags inherits depthoid bitmap)))

;; Allocate a CLOS object. This is used when the compiler detects that
;; MAKE-LOAD-FORM returned a simple use of MAKE-LOAD-FORM-SAVING-SLOTS,
;; or possibly a hand-written equivalent (however unlikely).
(!define-fop 68 :not-host (fop-allocate-instance (name) nil)
  (let ((instance (allocate-instance (find-class (the symbol name)))))
    (push-fop-table instance (fasl-input))))

;; Fill in object slots as dictated by the second return value from
;; MAKE-LOAD-FORM-SAVING-SLOTS.
;; This wants a 'count' as the first item in the SLOT-NAMES argument
;; rather than using read-arg because many calls of this might share
;; the list, which must be constructed into the fop-table no matter what.
(!define-fop 69 :not-host (fop-set-slot-values (slot-names obj) nil)
  (let* ((n-slots (pop slot-names))
         (stack (operand-stack))
         (ptr (fop-stack-pop-n stack n-slots)))
      (dotimes (i n-slots)
        (let ((val (svref stack (+ ptr i)))
              (slot-name (pop slot-names)))
          (if (unbound-marker-p val)
              ;; SLOT-MAKUNBOUND-USING-CLASS might do something nonstandard.
              (slot-makunbound obj slot-name)
              (setf (slot-value obj slot-name) val))))))

(!define-fop 64 (fop-end-group () nil)
  (/show0 "THROWing FASL-GROUP-END")
  (throw 'fasl-group-end t))

;;; We used to have FOP-NORMAL-LOAD as 81 and FOP-MAYBE-COLD-LOAD as
;;; 82 until GENESIS learned how to work with host symbols and
;;; packages directly instead of piggybacking on the host code.

(!define-fop 62 (fop-verify-table-size () nil)
  (let ((expected-index (read-word-arg (fasl-input-stream))))
    (unless (= (svref (%fasl-input-table (fasl-input)) 0) expected-index)
      (bug "fasl table of improper size"))))
(!define-fop 63 (fop-verify-empty-stack () nil)
  (unless (fop-stack-empty-p (operand-stack))
    (bug "fasl stack not empty when it should be")))

;;;; fops for loading symbols

(defstruct (undefined-package (:copier nil))
  (error nil :read-only t))
(declaim (freeze-type undefined-package))

;; Cold load has its own implementation of all symbol fops,
;; but we have to execute define-fop now to assign their numbers.
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
                          'read-string-as-unsigned-byte-32)
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
                 (push-fop-table (without-package-locks
                                  (%intern name length package elt-type))
                                 fasl-input))))
         ;; Symbol-hash is usually computed lazily and memoized into a symbol.
         ;; Laziness slightly improves the speed of allocation.
         ;; But when loading fasls, the time spent in the loader totally swamps
         ;; any time savings of not precomputing symbol-hash.
         ;; INTERN hashes everything anyway, so let's be consistent
         ;; and precompute the hashes of uninterned symbols too.
         (ensure-hashed (symbol)
           (ensure-symbol-hash symbol)
           symbol))

  #-sb-xc-host (declare (inline ensure-hashed))
  (!define-fop 80 :not-host (fop-lisp-symbol-save ((:operands length+flag)))
    (aux-fop-intern length+flag *cl-package* (fasl-input)))
  (!define-fop 84 :not-host (fop-keyword-symbol-save ((:operands length+flag)))
    (aux-fop-intern length+flag *keyword-package* (fasl-input)))
  (!define-fop #xF0 :not-host (fop-symbol-in-package-save ((:operands length+flag pkg-index)))
    (aux-fop-intern length+flag (ref-fop-table (fasl-input) pkg-index) (fasl-input)))

  (!define-fop 96 :not-host (fop-uninterned-symbol-save ((:operands length+flag)))
    (multiple-value-bind (name len) (read-symbol-name length+flag (fasl-input))
      (push-fop-table (ensure-hashed (make-symbol (subseq name 0 len)))
                      (fasl-input))))

  (!define-fop 104 :not-host (fop-copy-symbol-save ((:operands table-index)))
    (push-fop-table (ensure-hashed
                     (copy-symbol (ref-fop-table (fasl-input) table-index)))
                    (fasl-input))))

(!define-fop 44 (fop-package (pkg-designator))
  (find-undeleted-package-or-lose pkg-designator))

(!define-fop 156 :not-host (fop-named-package-save ((:operands length)) nil)
  (let ((package-name (make-string length)))
    #!-sb-unicode (read-string-as-bytes (fasl-input-stream) package-name)
    #!+sb-unicode (read-string-as-unsigned-byte-32 (fasl-input-stream) package-name)
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

(!define-fop 36 (fop-integer ((:operands n-bytes)))
  (load-s-integer n-bytes (fasl-input-stream)))

(!define-fop 34 (fop-word-integer)
  (with-fast-read-byte ((unsigned-byte 8) (fasl-input-stream))
    (fast-read-s-integer #.sb!vm:n-word-bytes)))

(!define-fop 35 (fop-byte-integer)
  ;; FIXME: WITH-FAST-READ-BYTE for exactly 1 byte is not really faster/better
  ;; than regular READ-BYTE. The expansion of READ-ARG corroborates this claim.
  (with-fast-read-byte ((unsigned-byte 8) (fasl-input-stream))
    (fast-read-s-integer 1)))

;; No %MAKE-RATIO on host
(!define-fop 70 :not-host (fop-ratio (num den)) (%make-ratio num den))

;; No %MAKE-COMPLEX on host
(!define-fop 71 :not-host (fop-complex (realpart imagpart))
  (%make-complex realpart imagpart))

(macrolet ((fast-read-single-float ()
             '(make-single-float (fast-read-s-integer 4)))
           (fast-read-double-float ()
             '(let ((lo (fast-read-u-integer 4)))
               (make-double-float (fast-read-s-integer 4) lo))))
  (macrolet ((define-complex-fop (opcode name type)
               (let ((reader (symbolicate "FAST-READ-" type)))
                 `(!define-fop ,opcode (,name)
                      (with-fast-read-byte ((unsigned-byte 8) (fasl-input-stream))
                        (complex (,reader) (,reader))))))
             (define-float-fop (opcode name type)
               (let ((reader (symbolicate "FAST-READ-" type)))
                 `(!define-fop ,opcode (,name)
                    (with-fast-read-byte ((unsigned-byte 8) (fasl-input-stream))
                      (,reader))))))
    (define-complex-fop 72 fop-complex-single-float single-float)
    (define-complex-fop 73 fop-complex-double-float double-float)
    #!+long-float
    (define-complex-fop 67 fop-complex-long-float long-float)
    (define-float-fop 46 fop-single-float single-float)
    (define-float-fop 47 fop-double-float double-float)
    #!+long-float
    (define-float-fop 52 fop-long-float long-float)))

#!+sb-simd-pack
(!define-fop 88 :not-host (fop-simd-pack)
  (with-fast-read-byte ((unsigned-byte 8) (fasl-input-stream))
    (%make-simd-pack (fast-read-s-integer 8)
                     (fast-read-u-integer 8)
                     (fast-read-u-integer 8))))

;;;; loading lists

(defun fop-list-from-stack (stack n)
  ;; N is 0-255 when called from FOP-LIST,
  ;; but it is as large as ARRAY-RANK-LIMIT in FOP-ARRAY.
  (declare (type (unsigned-byte 16) n)
           (optimize (speed 3)))
  (with-fop-stack ((stack) ptr n)
    (do* ((i (+ ptr n) (1- i))
          (res () (cons (fop-stack-ref i) res)))
         ((= i ptr) res)
      (declare (type index i)))))

(!define-fop 33 (fop-list)
  (fop-list-from-stack (operand-stack) (read-byte-arg (fasl-input-stream))))
(!define-fop 16 (fop-list*)
  ;; N is the number of cons cells (0 is ok)
  (let ((n (read-byte-arg (fasl-input-stream))))
    (with-fop-stack ((stack (operand-stack)) ptr (1+ n))
      (do* ((i (+ ptr n) (1- i))
            (res (fop-stack-ref (+ ptr n))
                 (cons (fop-stack-ref i) res)))
           ((= i ptr) res)
        (declare (type index i))))))

(macrolet ((frob (name op fun n)
             (let ((args (make-gensym-list n)))
               `(!define-fop ,op (,name ,args) (,fun ,@args)))))

  (frob fop-list-1 17 list 1)
  (frob fop-list-2 18 list 2)
  (frob fop-list-3 19 list 3)
  (frob fop-list-4 20 list 4)
  (frob fop-list-5 21 list 5)
  (frob fop-list-6 22 list 6)
  (frob fop-list-7 23 list 7)
  (frob fop-list-8 24 list 8)

  (frob fop-list*-1 25 list* 2)
  (frob fop-list*-2 26 list* 3)
  (frob fop-list*-3 27 list* 4)
  (frob fop-list*-4 28 list* 5)
  (frob fop-list*-5 29 list* 6)
  (frob fop-list*-6 30 list* 7)
  (frob fop-list*-7 31 list* 8)
  (frob fop-list*-8 32 list* 9))

;;;; fops for loading arrays

(!define-fop 100 :not-host (fop-base-string ((:operands length)))
  (logically-readonlyize
   (read-base-string-as-bytes (fasl-input-stream)
                              (make-string length :element-type 'base-char))))

;; FIXME: can save space by UTF-8 encoding, or use 1 bit to indicate pure ASCII
;; in the fasl even though the result will be a non-base string.
#!+sb-unicode
(!define-fop 160 :not-host (fop-character-string ((:operands length)))
  (logically-readonlyize
   (read-string-as-unsigned-byte-32 (fasl-input-stream)
                                    (make-string length))))

(!define-fop 92 (fop-vector ((:operands size)))
  (if (zerop size)
      #()
      (let ((res (make-array size))
            (stack (operand-stack)))
        (declare (fixnum size))
        (let ((ptr (fop-stack-pop-n stack size)))
          (replace res stack :start2 ptr))
        (logically-readonlyize res))))

;; No MAKE-ARRAY-HEADER on host
(!define-fop 89 :not-host (fop-array (vec))
  (let* ((rank (read-word-arg (fasl-input-stream)))
         (length (length vec))
         (res (make-array-header sb!vm:simple-array-widetag rank)))
    (declare (simple-array vec)
             (type (unsigned-byte #.(- sb!vm:n-word-bits sb!vm:n-widetag-bits)) rank))
    (set-array-header res vec length nil 0
                      (fop-list-from-stack (operand-stack) rank)
                      nil t)
    res))

(defglobal **saetp-bits-per-length**
    (let ((array (make-array (1+ sb!vm:widetag-mask)
                             :element-type '(unsigned-byte 8)
                             :initial-element 255)))
      (loop for saetp across sb!vm:*specialized-array-element-type-properties*
            do
            (setf (aref array (sb!vm:saetp-typecode saetp))
                  (sb!vm:saetp-n-bits saetp)))
      array)
    "255 means bad entry.")
(declaim (type (simple-array (unsigned-byte 8) (#.(1+ sb!vm:widetag-mask)))
               **saetp-bits-per-length**))

;; No ALLOCATE-VECTOR on host (nor READ-N-BYTES)
(!define-fop 43 :not-host (fop-spec-vector)
  (let* ((length (read-word-arg (fasl-input-stream)))
         (widetag (read-byte-arg (fasl-input-stream)))
         (bits-per-length (aref **saetp-bits-per-length** widetag))
         (bits (progn (aver (< bits-per-length 255))
                      (* length bits-per-length)))
         (bytes (ceiling bits sb!vm:n-byte-bits))
         (words (ceiling bytes sb!vm:n-word-bytes))
         (vector (if (and (= widetag sb!vm:simple-vector-widetag)
                          (= words 0))
                     #()
                     (logically-readonlyize
                      (allocate-vector widetag length words)))))
    (declare (type index length bytes words)
             (type word bits))
    (read-n-bytes (fasl-input-stream) vector 0 bytes)
    vector))

(!define-fop 53 (fop-eval (expr)) ; This seems to be unused
  (if (skip-until)
      expr
      (eval expr)))

(!define-fop 54 (fop-eval-for-effect (expr) nil) ; This seems to be unused
  (unless (skip-until)
    (eval expr))
  nil)

(defun fop-funcall* (input-stream stack skipping)
 (let ((argc (read-byte-arg input-stream)))
   (with-fop-stack ((stack) ptr (1+ argc))
     (unless skipping
       (do ((i (+ ptr argc))
            (args))
           ((= i ptr) (apply (fop-stack-ref i) args))
         (declare (type index i))
         (push (fop-stack-ref i) args)
         (decf i))))))

;; FIXME: there should be a syntax to share these identical bodies
(!define-fop 55 (fop-funcall)
  (fop-funcall* (fasl-input-stream) (operand-stack) (skip-until)))
(!define-fop 56 (fop-funcall-for-effect () nil)
  (fop-funcall* (fasl-input-stream) (operand-stack) (skip-until)))

;;; For LOAD-TIME-VALUE which is used for MAKE-LOAD-FORM
(!define-fop 57 (fop-funcall-no-skip)
  (fop-funcall* (fasl-input-stream) (operand-stack) nil))

;;;; fops for fixing up circularities

(!define-fop 200 (fop-rplaca (val) nil)
  (let ((obj (ref-fop-table (fasl-input) (read-word-arg (fasl-input-stream))))
        (idx (read-word-arg (fasl-input-stream))))
    (setf (car (nthcdr idx obj)) val)))

(!define-fop 201 (fop-rplacd (val) nil)
  (let ((obj (ref-fop-table (fasl-input) (read-word-arg (fasl-input-stream))))
        (idx (read-word-arg (fasl-input-stream))))
    (setf (cdr (nthcdr idx obj)) val)))

(!define-fop 202 (fop-svset (val) nil)
  (let* ((obi (read-word-arg (fasl-input-stream)))
         (obj (ref-fop-table (fasl-input) obi))
         (idx (read-word-arg (fasl-input-stream))))
    (if (%instancep obj) ; suspicious. should have been FOP-STRUCTSET
        #+sb-xc (setf (%instance-ref obj idx) val)
        #-sb-xc (bug "%instance-set?")
        (setf (svref obj idx) val))))

(!define-fop 204 :not-host (fop-structset (val) nil)
  (setf (%instance-ref (ref-fop-table (fasl-input)
                                      (read-word-arg (fasl-input-stream)))
                       (read-word-arg (fasl-input-stream)))
        val))

;;; In the original CMUCL code, this actually explicitly declared PUSHP
;;; to be T, even though that's what it defaults to in DEFINE-FOP.
(!define-fop 203 (fop-nthcdr (obj))
  (nthcdr (read-word-arg (fasl-input-stream)) obj))

;;;; fops for loading functions

;;; (In CMU CL there was a FOP-CODE-FORMAT (47) which was
;;; conventionally placed at the beginning of each fasl file to test
;;; for compatibility between the fasl file and the CMU CL which
;;; loaded it. In SBCL, this functionality has been replaced by
;;; putting the implementation and version in required fields in the
;;; fasl file header.)

;; Cold-load calls COLD-LOAD-CODE instead
(!define-fop #xE0 :not-host (fop-load-code ((:operands immobile-p
                                                       n-boxed-words
                                                       n-code-bytes)))
  (let ((n-constants (- n-boxed-words sb!vm:code-constants-offset)))
    ;; stack has (at least) N-CONSTANTS words plus debug-info
    (with-fop-stack ((stack (operand-stack)) ptr (1+ n-constants))
      (let* ((debug-info-index (+ ptr n-constants))
             (n-boxed-words (+ sb!vm:code-constants-offset n-constants))
             (code (sb!c:allocate-code-object
                    (if (eql immobile-p 1) :immobile :dynamic)
                    (align-up n-boxed-words sb!c::code-boxed-words-align)
                    n-code-bytes)))
        (setf (%code-debug-info code) (svref stack debug-info-index))
        (loop for i of-type index from sb!vm:code-constants-offset
              for j of-type index from ptr below debug-info-index
              do (setf (code-header-ref code i) (svref stack j)))
        (with-pinned-objects (code)
          (read-n-bytes (fasl-input-stream) (code-instructions code) 0 n-code-bytes)
          (sb!c::apply-fasl-fixups stack code))
        #-sb-xc-host
        (when (typep (code-header-ref code (1- n-boxed-words))
                     '(cons (eql sb!c::coverage-map)))
          ;; Record this in the global list of coverage-instrumented code.
          (atomic-push (make-weak-pointer code) (cdr *code-coverage-info*)))
        code))))

;; this gets you an #<fdefn> object, not the result of (FDEFINITION x)
;; cold-loader uses COLD-FDEFINITION-OBJECT instead.
(!define-fop 60 :not-host (fop-fdefn (name))
  (awhen (deprecated-thing-p 'function name) ; returns the stage of deprecation
    (pushnew (list* it name :function)
             (%fasl-input-deprecated-stuff (fasl-input)) :test 'equal))
  (find-or-create-fdefn name))

(!define-fop 65 :not-host (fop-known-fun (name))
  (%coerce-name-to-fun name))

;;; Modify a slot in a CONSTANTS object.
(!define-fop 140 :not-host (fop-alter-code ((:operands index) code value) nil)
  (setf (code-header-ref code index) value)
  (values))

(!define-fop #xFC :not-host (fop-fun-entry ((:operands fun-index)
                                            code-object name arglist type info))
  (let ((fun (%code-entry-point code-object fun-index)))
    (setf (%simple-fun-name fun) name)
    (setf (%simple-fun-arglist fun) arglist)
    (setf (%simple-fun-type fun) type)
    (setf (%simple-fun-info fun) info)
    fun))

;;;; Some Dylan FOPs used to live here. By 1 November 1998 the code
;;;; was sufficiently stale that the functions it called were no
;;;; longer defined, so I (William Harold Newman) deleted it.
;;;;
;;;; In case someone in the future is trying to make sense of FOP layout,
;;;; it might be worth recording that the Dylan FOPs were
;;;;    100 FOP-DYLAN-SYMBOL-SAVE
;;;;    101 FOP-SMALL-DYLAN-SYMBOL-SAVE
;;;;    102 FOP-DYLAN-KEYWORD-SAVE
;;;;    103 FOP-SMALL-DYLAN-KEYWORD-SAVE
;;;;    104 FOP-DYLAN-VARINFO-VALUE

;;;; assemblerish fops

(!define-fop 144 (fop-assembler-code)
  (error "cannot load assembler code except at cold load"))

;;; FOPs needed for implementing an IF operator in a FASL

;;; Skip until a FOP-MAYBE-STOP-SKIPPING with the same POSITION is
;;; executed. While skipping, we execute most FOPs normally, except
;;; for ones that a) funcall/eval b) start skipping. This needs to
;;; be done to ensure that the fop table gets populated correctly
;;; regardless of the execution path.
(!define-fop 151 (fop-skip (position) nil)
  (unless (skip-until)
    (setf (skip-until) position))
  (values))

;;; As before, but only start skipping if the top of the FOP stack is NIL.
(!define-fop 152 (fop-skip-if-false (position condition) nil)
  (unless (or condition (skip-until))
    (setf (skip-until) position))
  (values))

;;; If skipping, pop the top of the stack and discard it. Needed for
;;; ensuring that the stack stays balanced when skipping.
(!define-fop 153 (fop-drop-if-skipping () nil)
  (when (skip-until)
    (fop-stack-pop-n (operand-stack) 1))
  (values))

;;; If skipping, push a dummy value on the stack. Needed for
;;; ensuring that the stack stays balanced when skipping.
(!define-fop 154 (fop-push-nil-if-skipping () nil)
  (when (skip-until)
    (push-fop-stack nil (fasl-input)))
  (values))

;;; Stop skipping if the top of the stack matches SKIP-UNTIL
(!define-fop 155 (fop-maybe-stop-skipping (label) nil)
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
                            `(!define-fop ,(car spec)
                                          (,(symbolicate "FOP-LAYOUT-OF-"
                                                         (cadr spec)))
                                          (find-layout ',(cadr spec))))
                          specs))))
  (frob (#x6c t)
        (#x6d structure-object)
        (#x6e condition)
        (#x6f definition-source-location)
        (#x70 sb!c::debug-fun)
        (#x71 sb!c::compiled-debug-fun)
        (#x72 sb!c::debug-info)
        (#x73 sb!c::compiled-debug-info)
        (#x74 sb!c::debug-source)
        (#x75 defstruct-description)
        (#x76 defstruct-slot-description)))
