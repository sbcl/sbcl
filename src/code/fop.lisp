;;;; FOP definitions

(in-package "SB!FASL")

;;; Sometimes we want to skip over any FOPs with side-effects (like
;;; function calls) while executing other FOPs. *SKIP-UNTIL* will
;;; either contain the position where the skipping will stop, or
;;; NIL if we're executing normally.
(defvar *skip-until* nil)

;;; Bind STACK-VAR and PTR-VAR to the start of a subsequence of
;;; the fop stack of length COUNT, then execute BODY.
;;; Within the body, FOP-STACK-REF is used in lieu of SVREF
;;; to elide bounds checking.
(defmacro with-fop-stack ((stack-var ptr-var count) &body body)
  `(multiple-value-bind (,stack-var ,ptr-var)
       (truly-the (values simple-vector index) (fop-stack-pop-n ,count))
     (macrolet ((fop-stack-ref (i)
                  `(locally
                       #-sb-xc-host
                       (declare (optimize (sb!c::insert-array-bounds-checks 0)))
                     (svref ,',stack-var ,i))))
       ,@body)))

;;; Define NAME as a fasl operation, with op-code FOP-CODE. PUSHP
;;; describes what the body does to the fop stack:
;;;   T
;;;     The body might pop the fop stack. The result of the body is
;;;     pushed on the fop stack.
;;;   NIL
;;;     The body might pop the fop stack. The result of the body is
;;;     discarded.
;;;
;;; I think the macro syntax would be aesthetically more pleasing as
;;;   (DEFINE-FOP code (name &OPTIONAL (args) (pushp t)) . body)
;;;
(defmacro define-fop ((name fop-code &optional arglist (pushp t)) &body forms)
  (aver (member pushp '(nil t)))
  (binding* (((operands stack-args)
              (if (consp (car arglist))
                  (ecase (caar arglist)
                    (:operands (values (cdar arglist) (cdr arglist))))
                  (values nil arglist)))
             (guts (if pushp `((push-fop-stack (progn ,@forms))) forms)))
    (assert (<= (length operands) 2))
    `(progn
       (defun ,name ,operands
         ,@(if (null stack-args)
               guts
               (with-unique-names (stack ptr)
                 `((with-fop-stack (,stack ,ptr ,(length stack-args))
                     (multiple-value-bind ,stack-args
                         (values ,@(loop for i below (length stack-args)
                                         collect `(fop-stack-ref (+ ,ptr ,i))))
                       ,@guts))))))
       (%define-fop ',name ,fop-code ,(length operands)))))

(defun %define-fop (name code n-operands)
  (declare (type (mod 3) n-operands)) ; 0, 1, or 2 are allowed
  (let ((n-slots (expt 4 n-operands)))
    (unless (zerop (mod code n-slots))
      (error "Opcode for fop ~S must be a multiple of ~D" name n-slots))
    (loop for opcode from code below (+ code n-slots)
          when (functionp (svref *fop-funs* opcode))
          do (let ((oname (svref *fop-names* opcode)))
               (when (and oname (not (eq oname name)))
                 (error "fop ~S with opcode ~D conflicts with fop ~S."
                        name opcode oname))))
    (let ((existing-opcode (get name 'opcode)))
      (when (and existing-opcode (/= existing-opcode code))
        (error "multiple codes for fop name ~S: ~D and ~D"
               name code existing-opcode)))
    (setf (get name 'opcode) code)
    ;; The low 2 bits of the opcode comprise the length modifier if there is
    ;; exactly one operand. Such opcodes are aligned in blocks of 4.
    ;; 2-operand fops occupy 16 slots in a reserved range of the function table.
    (loop for opcode from code below (+ code n-slots)
          do (setf (svref *fop-names* opcode) name
                   (svref *fop-funs* opcode) (symbol-function name)
                   (sbit *fop-argp* (ash opcode -2)) (signum n-operands))))
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
#!+sb-unicode
(defun read-string-as-unsigned-byte-32
    (stream string &optional (length (length string)))
  (declare (type (simple-array character (*)) string)
           (type index length)
           (optimize speed))
  #+sb-xc-host (bug "READ-STRING-AS-UNSIGNED-BYTE-32 called")
  (with-fast-read-byte ((unsigned-byte 8) stream)
    (dotimes (i length)
      (setf (aref string i)
            (sb!xc:code-char (fast-read-u-integer 4)))))
  string)

;;;; miscellaneous fops

;;; Setting this variable causes execution of a FOP-NOP4 to produce
;;; output to *DEBUG-IO*. This can be handy when trying to follow the
;;; progress of FASL loading.
#!+sb-show
(defvar *show-fop-nop4-p* nil)

;;; CMU CL had a single no-op fop, FOP-NOP, with fop code 0. Since 0
;;; occurs disproportionately often in fasl files for other reasons,
;;; FOP-NOP is less than ideal for writing human-readable patterns
;;; into fasl files for debugging purposes. There's no shortage of
;;; unused fop codes, so we add this second NOP, which reads 4
;;; arbitrary bytes and discards them.
(define-fop (fop-nop4 137 () nil)
  (let ((arg (read-arg 4)))
    (declare (ignorable arg))
    #!+sb-show
    (when *show-fop-nop4-p*
      (format *debug-io* "~&/FOP-NOP4 ARG=~W=#X~X~%" arg arg))))

(define-fop (fop-nop 0 () nil))
(define-fop (fop-pop 1 (x) nil) (push-fop-table x))
(define-fop (fop-empty-list 2) nil)
(define-fop (fop-truth 3) t)
(define-fop (fop-push 4 ((:operands index))) (ref-fop-table index))

;;; CMU CL had FOP-POP-FOR-EFFECT as fop 65, but it was never used and seemed
;;; to have no possible use.
(define-fop (fop-misc-trap 66)
  #+sb-xc-host ; since xc host doesn't know how to compile %PRIMITIVE
  (error "FOP-MISC-TRAP can't be defined without %PRIMITIVE.")
  #-sb-xc-host
  (%primitive sb!c:make-unbound-marker))

(define-fop (fop-character 76 ((:operands char-code)))
  (code-char char-code))

(define-fop (fop-struct 48 ((:operands size) layout))
  (let* ((res (%make-instance size)) ; number of words excluding header
         ;; Compute count of elements to pop from stack, sans layout.
         ;; If instance-data-start is 0, then size is the count,
         ;; otherwise subtract 1 because the layout consumes a slot.
         (n-data-words (- size sb!vm:instance-data-start)))
    (declare (type index size))
    (with-fop-stack (stack ptr n-data-words)
      (let ((ptr (+ ptr n-data-words)))
        (declare (type index ptr))
        (setf (%instance-layout res) layout)
        #!-interleaved-raw-slots
        (let* ((nuntagged (layout-n-untagged-slots layout))
               (ntagged (- size nuntagged)))
          (dotimes (n (1- ntagged))
            (declare (type index n))
            (setf (%instance-ref res (1+ n)) (fop-stack-ref (decf ptr))))
          (dotimes (n nuntagged)
            (declare (type index n))
            (setf (%raw-instance-ref/word res (- nuntagged n 1))
                  (fop-stack-ref (decf ptr)))))
        #!+interleaved-raw-slots
        (let ((metadata (layout-untagged-bitmap layout)))
          (do ((i sb!vm:instance-data-start (1+ i)))
              ((>= i size))
            (declare (type index i))
            (let ((val (fop-stack-ref (decf ptr))))
              (if (logbitp i metadata)
                  (setf (%raw-instance-ref/word res i) val)
                  (setf (%instance-ref res i) val)))))))
    res))

(define-fop (fop-layout 45 (name inherits depthoid length metadata))
  (find-and-init-or-check-layout name length inherits depthoid metadata))

;; Allocate a CLOS object. This is used when the compiler detects that
;; MAKE-LOAD-FORM returned a simple use of MAKE-LOAD-FORM-SAVING-SLOTS,
;; or possibly a hand-written equivalent (however unlikely).
(define-fop (fop-allocate-instance 68 (name) nil)
  (let ((instance (allocate-instance (find-class (the symbol name)))))
    (push-fop-table instance)
    instance))

;; Fill in object slots as dictated by the second return value from
;; MAKE-LOAD-FORM-SAVING-SLOTS.
;; This wants a 'count' as the first item in the SLOT-NAMES argument
;; rather than using read-arg because many calls of this might share
;; the list, which must be constructed into the fop-table no matter what.
(define-fop (fop-initialize-instance 69 (slot-names obj) nil)
  (let ((n-slots (pop slot-names)))
    (multiple-value-bind (stack ptr) (fop-stack-pop-n n-slots)
      (dotimes (i n-slots)
        (let ((val (svref stack (+ ptr i)))
              (slot-name (pop slot-names)))
          (if (eq val 'sb!pcl::..slot-unbound..)
              ;; SLOT-MAKUNBOUND-USING-CLASS might do something nonstandard.
              (slot-makunbound obj slot-name)
              ;; FIXME: the DEFSETF for this isn't defined until warm load
              (sb!pcl::set-slot-value obj slot-name val)))))))

(define-fop (fop-end-group 64 () nil)
  (/show0 "THROWing FASL-GROUP-END")
  (throw 'fasl-group-end t))

;;; We used to have FOP-NORMAL-LOAD as 81 and FOP-MAYBE-COLD-LOAD as
;;; 82 until GENESIS learned how to work with host symbols and
;;; packages directly instead of piggybacking on the host code.

(define-fop (fop-verify-table-size 62 () nil)
  (let ((expected-index (read-word-arg)))
    (unless (= (get-fop-table-index) expected-index)
      (bug "fasl table of improper size"))))
(define-fop (fop-verify-empty-stack 63 () nil)
  (unless (fop-stack-empty-p)
    (bug "fasl stack not empty when it should be")))

;;;; fops for loading symbols

(defstruct (undefined-package
            (:copier nil))
  error)

(declaim (freeze-type undefined-package))

(defun aux-fop-intern (size package)
  (declare (optimize speed))
  (let ((buffer (make-string size)))
    #+sb-xc-host
    (read-string-as-bytes *fasl-input-stream* buffer size)
    #-sb-xc-host
    (progn
      #!+sb-unicode
      (read-string-as-unsigned-byte-32 *fasl-input-stream* buffer size)
      #!-sb-unicode
      (read-string-as-bytes *fasl-input-stream* buffer size))
    (if (undefined-package-p package)
        (error 'simple-package-error
               :format-control "Error finding package for symbol ~s:~% ~a"
               :format-arguments
               (list (subseq buffer 0 size)
                     (undefined-package-error package)))
        (push-fop-table (without-package-locks
                          (intern* buffer
                                   size
                                   package
                                   :no-copy t))))))

(define-fop (fop-lisp-symbol-save        80 ((:operands namelen)))
  (aux-fop-intern namelen *cl-package*))
(define-fop (fop-keyword-symbol-save     84 ((:operands namelen)))
  (aux-fop-intern namelen *keyword-package*))

;; But srsly? Most of the space is wasted by UCS4 encoding of ASCII.
;; An extra word per symbol for the package is nothing by comparison.
  ;; FIXME: Because we don't have FOP-SYMBOL-SAVE any more, an
  ;; enormous number of symbols will fall through to this case,
  ;; probably resulting in bloated fasl files. A new
  ;; FOP-SYMBOL-IN-LAST-PACKAGE-SAVE/FOP-SMALL-SYMBOL-IN-LAST-PACKAGE-SAVE
  ;; cloned fop pair could undo some of this bloat.
(define-fop (fop-symbol-in-package-save #xF0 ((:operands pkg-index namelen)))
  (aux-fop-intern namelen (ref-fop-table pkg-index)))

(define-fop (fop-uninterned-symbol-save 96 ((:operands namelen)))
  (let ((res (make-string namelen)))
    #!-sb-unicode
    (read-string-as-bytes *fasl-input-stream* res)
    #!+sb-unicode
    (read-string-as-unsigned-byte-32 *fasl-input-stream* res)
    (push-fop-table (make-symbol res))))

(define-fop (fop-package 44 (pkg-designator))
  (find-undeleted-package-or-lose pkg-designator))

(define-fop (fop-named-package-save 156 ((:operands length)) nil)
  (let ((package-name (make-string length)))
    #+sb-xc-host
    (read-string-as-bytes *fasl-input-stream* package-name)
    #-sb-xc-host
    (progn
      #!-sb-unicode
      (read-string-as-bytes *fasl-input-stream* package-name)
      #!+sb-unicode
      (read-string-as-unsigned-byte-32 *fasl-input-stream* package-name))
    (push-fop-table
     (handler-case (find-undeleted-package-or-lose package-name)
       (simple-package-error (c)
         (make-undefined-package :error (princ-to-string c)))))))

;;;; fops for loading numbers

;;; Load a signed integer LENGTH bytes long from *FASL-INPUT-STREAM*.
(defun load-s-integer (length)
  (declare (fixnum length)
           (optimize speed))
  (with-fast-read-byte ((unsigned-byte 8) *fasl-input-stream*)
    (do* ((index length (1- index))
          (byte 0 (fast-read-byte))
          (result 0 (+ result (ash byte bits)))
          (bits 0 (+ bits 8)))
         ((= index 0)
          (if (logbitp 7 byte)          ; look at sign bit
              (- result (ash 1 bits))
              result))
      (declare (fixnum index byte bits)))))

(define-fop (fop-integer 36 ((:operands n-bytes)))
  (load-s-integer n-bytes))

(define-fop (fop-word-integer 34)
  (with-fast-read-byte ((unsigned-byte 8) *fasl-input-stream*)
    (fast-read-s-integer #.sb!vm:n-word-bytes)))

(define-fop (fop-byte-integer 35)
  (with-fast-read-byte ((unsigned-byte 8) *fasl-input-stream*)
    (fast-read-s-integer 1)))

(define-fop (fop-ratio 70 (num den)) (%make-ratio num den))

(define-fop (fop-complex 71 (realpart imagpart))
  (%make-complex realpart imagpart))

(macrolet ((fast-read-single-float ()
             '(make-single-float (fast-read-s-integer 4)))
           (fast-read-double-float ()
             '(let ((lo (fast-read-u-integer 4)))
               (make-double-float (fast-read-s-integer 4) lo))))
  (macrolet ((define-complex-fop (name fop-code type)
               (let ((reader (symbolicate "FAST-READ-" type)))
                 `(define-fop (,name ,fop-code)
                      (with-fast-read-byte ((unsigned-byte 8) *fasl-input-stream*)
                        (complex (,reader) (,reader))))))
             (define-float-fop (name fop-code type)
               (let ((reader (symbolicate "FAST-READ-" type)))
                 `(define-fop (,name ,fop-code)
                    (with-fast-read-byte ((unsigned-byte 8) *fasl-input-stream*)
                      (,reader))))))
    (define-complex-fop fop-complex-single-float 72 single-float)
    (define-complex-fop fop-complex-double-float 73 double-float)
    #!+long-float
    (define-complex-fop fop-complex-long-float 67 long-float)
    (define-float-fop fop-single-float 46 single-float)
    (define-float-fop fop-double-float 47 double-float)
    #!+long-float
    (define-float-fop fop-long-float 52 long-float)))

#!+sb-simd-pack
(define-fop (fop-simd-pack 88)
  (with-fast-read-byte ((unsigned-byte 8) *fasl-input-stream*)
    (%make-simd-pack (fast-read-s-integer 8)
                     (fast-read-u-integer 8)
                     (fast-read-u-integer 8))))

;;;; loading lists

(defun fop-list-from-stack (n)
  ;; N is 0-255 when called from FOP-LIST,
  ;; but it is as large as ARRAY-RANK-LIMIT in FOP-ARRAY.
  (declare (type (unsigned-byte 16) n)
           (optimize (speed 3)))
  (with-fop-stack (stack ptr n)
    (do* ((i (+ ptr n) (1- i))
          (res () (cons (fop-stack-ref i) res)))
         ((= i ptr) res)
      (declare (type index i)))))

(define-fop (fop-list 33) (fop-list-from-stack (read-byte-arg)))
(define-fop (fop-list* 16)
  (let ((n (read-byte-arg))) ; N is the number of cons cells (0 is ok)
    (with-fop-stack (stack ptr (1+ n))
      (do* ((i (+ ptr n) (1- i))
            (res (fop-stack-ref (+ ptr n))
                 (cons (fop-stack-ref i) res)))
           ((= i ptr) res)
        (declare (type index i))))))

(macrolet ((frob (name op fun n)
             (let ((args (make-gensym-list n)))
               `(define-fop (,name ,op ,args) (,fun ,@args)))))

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

(define-fop (fop-base-string 100 ((:operands length)))
  (let ((res (make-string length :element-type 'base-char)))
    (read-base-string-as-bytes *fasl-input-stream* res)
    res))

#!+sb-unicode
;; FIXME: can save space by UTF-8 encoding, or use 1 bit to indicate pure ASCII
;; in the fasl even though the result will be a non-base string.
(progn
  #+sb-xc-host
  (define-fop (fop-character-string 160 ((:operands length)))
    (bug "CHARACTER-STRING FOP encountered"))

  #-sb-xc-host
  (define-fop (fop-character-string 160 ((:operands length)))
    (let ((res (make-string length)))
      (read-string-as-unsigned-byte-32 *fasl-input-stream* res)
      res)))

(define-fop (fop-vector 92 ((:operands size)))
  (let ((res (make-array size)))
    (declare (fixnum size))
    (unless (zerop size)
      (multiple-value-bind (stack ptr) (fop-stack-pop-n size)
        (replace res stack :start2 ptr)))
    res))

(define-fop (fop-array 89 (vec))
  (let* ((rank (read-word-arg))
         (length (length vec))
         (res (make-array-header sb!vm:simple-array-widetag rank)))
    (declare (simple-array vec)
             (type (unsigned-byte #.(- sb!vm:n-word-bits sb!vm:n-widetag-bits)) rank))
    (set-array-header res vec length nil 0 (fop-list-from-stack rank) nil t)
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
    #!+sb-doc
    "255 means bad entry.")
(declaim (type (simple-array (unsigned-byte 8) (#.(1+ sb!vm:widetag-mask)))
               **saetp-bits-per-length**))

(define-fop (fop-spec-vector 43)
  (let* ((length (read-word-arg))
         (widetag (read-byte-arg))
         (bits-per-length (aref **saetp-bits-per-length** widetag))
         (bits (progn (aver (< bits-per-length 255))
                      (* length bits-per-length)))
         (bytes (ceiling bits sb!vm:n-byte-bits))
         (words (ceiling bytes sb!vm:n-word-bytes))
         (vector (allocate-vector widetag length words)))
    (declare (type index length bytes words)
             (type word bits))
    (read-n-bytes *fasl-input-stream* vector 0 bytes)
    vector))

(define-fop (fop-eval 53 (expr)) ; This seems to be unused
  (if *skip-until*
      expr
      (eval expr)))

(define-fop (fop-eval-for-effect 54 (expr) nil) ; This seems to be unused
  (unless *skip-until*
    (eval expr))
  nil)

(defun fop-funcall* ()
 (let ((argc (read-byte-arg)))
   (with-fop-stack (stack ptr (1+ argc))
     (unless *skip-until*
       (do ((i (+ ptr argc))
            (args))
           ((= i ptr) (apply (fop-stack-ref i) args))
         (declare (type index i))
         (push (fop-stack-ref i) args)
         (decf i))))))

(define-fop (fop-funcall 55) (fop-funcall*))
(define-fop (fop-funcall-for-effect 56 () nil) (fop-funcall*))

;;;; fops for fixing up circularities

(define-fop (fop-rplaca 200 (val) nil)
  (let ((obj (ref-fop-table (read-word-arg)))
        (idx (read-word-arg)))
    (setf (car (nthcdr idx obj)) val)))

(define-fop (fop-rplacd 201 (val) nil)
  (let ((obj (ref-fop-table (read-word-arg)))
        (idx (read-word-arg)))
    (setf (cdr (nthcdr idx obj)) val)))

(define-fop (fop-svset 202 (val) nil)
  (let* ((obi (read-word-arg))
         (obj (ref-fop-table obi))
         (idx (read-word-arg)))
    (if (%instancep obj)
        (setf (%instance-ref obj idx) val)
        (setf (svref obj idx) val))))

(define-fop (fop-structset 204 (val) nil)
  (setf (%instance-ref (ref-fop-table (read-word-arg))
                       (read-word-arg))
        val))

;;; In the original CMUCL code, this actually explicitly declared PUSHP
;;; to be T, even though that's what it defaults to in DEFINE-FOP.
(define-fop (fop-nthcdr 203 (obj))
  (nthcdr (read-word-arg) obj))

;;;; fops for loading functions

;;; (In CMU CL there was a FOP-CODE-FORMAT (47) which was
;;; conventionally placed at the beginning of each fasl file to test
;;; for compatibility between the fasl file and the CMU CL which
;;; loaded it. In SBCL, this functionality has been replaced by
;;; putting the implementation and version in required fields in the
;;; fasl file header.)

(define-fop (fop-code #xE0 ((:operands n-boxed n-inst-bytes)))
  (load-code n-boxed n-inst-bytes))

(define-fop (fop-fdefn 60 (name))
  (find-or-create-fdefn name))

(define-fop (fop-known-fun 65 (name))
  (%coerce-name-to-fun name))

#!-(or x86 x86-64)
(define-fop (fop-sanctify-for-execution 61 (component))
  (sb!vm:sanctify-for-execution component)
  component)

(define-fop (fop-fset 74 (name fn) nil)
  ;; Ordinary, not-for-cold-load code shouldn't need to mess with this
  ;; at all, since it's only used as part of the conspiracy between
  ;; the cross-compiler and GENESIS to statically link FDEFINITIONs
  ;; for cold init.
  (warn "~@<FOP-FSET seen in ordinary load (not cold load) -- quite strange! ~
If you didn't do something strange to cause this, please report it as a ~
bug.~:@>")
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

(define-fop (fop-note-debug-source 174 (debug-source) nil)
  (warn "~@<FOP-NOTE-DEBUG-SOURCE seen in ordinary load (not cold load) -- ~
very strange!  If you didn't do something to cause this, please report it as ~
a bug.~@:>")
  ;; as with COLD-FSET above, we are going to be lenient with coming
  ;; across this fop in a warm SBCL.
  (setf (sb!c::debug-source-compiled debug-source) (get-universal-time)
        (sb!c::debug-source-created debug-source)
        (file-write-date (sb!c::debug-source-namestring debug-source))))

;;; Modify a slot in a CONSTANTS object.
(define-fop (fop-alter-code 140 ((:operands index) code value) nil)
  (setf (code-header-ref code index) value)
  (values))

(define-fop (fop-fun-entry 139 (code-object name arglist type info))
  #+sb-xc-host ; since xc host doesn't know how to compile %PRIMITIVE
  (error "FOP-FUN-ENTRY can't be defined without %PRIMITIVE.")
  #-sb-xc-host
  (let ((offset (read-word-arg)))
    (declare (type index offset))
    (unless (zerop (logand offset sb!vm:lowtag-mask))
      (bug "unaligned function object, offset = #X~X" offset))
    (let ((fun (%primitive sb!c:compute-fun code-object offset)))
      (setf (%simple-fun-self fun) fun)
      (setf (%simple-fun-next fun) (%code-entry-points code-object))
      (setf (%code-entry-points code-object) fun)
      (setf (%simple-fun-name fun) name)
      (setf (%simple-fun-arglist fun) arglist)
      (setf (%simple-fun-type fun) type)
      (setf (%simple-fun-info fun) info)
      fun)))

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

(define-fop (fop-assembler-code 144)
  (error "cannot load assembler code except at cold load"))

(define-fop (fop-assembler-routine 145)
  (error "cannot load assembler code except at cold load"))

(define-fop (fop-symbol-tls-fixup 146 (code-object kind symbol))
  (sb!vm:fixup-code-object code-object
                           (read-word-arg)
                           (ensure-symbol-tls-index symbol)
                           kind)
  code-object)

(define-fop (fop-foreign-fixup 147 (code-object kind))
  (let* ((len (read-byte-arg))
         (sym (make-string len :element-type 'base-char)))
    (read-n-bytes *fasl-input-stream* sym 0 len)
    (sb!vm:fixup-code-object code-object
                             (read-word-arg)
                             (foreign-symbol-address sym)
                             kind)
    code-object))

(define-fop (fop-assembler-fixup 148 (code-object kind routine))
    (multiple-value-bind (value found) (gethash routine *assembler-routines*)
      (unless found
        (error "undefined assembler routine: ~S" routine))
      (sb!vm:fixup-code-object code-object (read-word-arg) value kind))
    code-object)

(define-fop (fop-code-object-fixup 149 (code-object kind))
    ;; Note: We don't have to worry about GC moving the code-object after
    ;; the GET-LISP-OBJ-ADDRESS and before that value is deposited, because
    ;; we can only use code-object fixups when code-objects don't move.
    (sb!vm:fixup-code-object code-object (read-word-arg)
                             (get-lisp-obj-address code-object) kind)
    code-object)

#!+linkage-table
(define-fop (fop-foreign-dataref-fixup 150 (code-object kind))
  (let* ((len (read-byte-arg))
         (sym (make-string len :element-type 'base-char)))
    (read-n-bytes *fasl-input-stream* sym 0 len)
    (sb!vm:fixup-code-object code-object
                             (read-word-arg)
                             (foreign-symbol-address sym t)
                             kind)
    code-object))

;;; FOPs needed for implementing an IF operator in a FASL

;;; Skip until a FOP-MAYBE-STOP-SKIPPING with the same POSITION is
;;; executed. While skipping, we execute most FOPs normally, except
;;; for ones that a) funcall/eval b) start skipping. This needs to
;;; be done to ensure that the fop table gets populated correctly
;;; regardless of the execution path.
(define-fop (fop-skip 151 (position) nil)
  (unless *skip-until*
    (setf *skip-until* position))
  (values))

;;; As before, but only start skipping if the top of the FOP stack is NIL.
(define-fop (fop-skip-if-false 152 (position condition) nil)
  (unless (or condition *skip-until*)
    (setf *skip-until* position))
  (values))

;;; If skipping, pop the top of the stack and discard it. Needed for
;;; ensuring that the stack stays balanced when skipping.
(define-fop (fop-drop-if-skipping 153 () nil)
  (when *skip-until*
    (fop-stack-pop-n 1))
  (values))

;;; If skipping, push a dummy value on the stack. Needed for
;;; ensuring that the stack stays balanced when skipping.
(define-fop (fop-push-nil-if-skipping 154 () nil)
  (when *skip-until*
    (push-fop-stack nil))
  (values))

;;; Stop skipping if the top of the stack matches *SKIP-UNTIL*
(define-fop (fop-maybe-stop-skipping 155 (label) nil)
  (when (eql *skip-until* label)
    (setf *skip-until* nil))
  (values))
