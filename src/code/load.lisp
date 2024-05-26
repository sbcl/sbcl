;;;; parts of the loader which make sense in the cross-compilation
;;;; host (and which are useful in the host, because they're used by
;;;; GENESIS)
;;;;
;;;; based on the CMU CL load.lisp code, written by Skef Wholey and
;;;; Rob Maclachlan

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-FASL")


;;;; various constants and essentially-constants

;;; a string which appears at the start of a fasl file header
;;;
;;; This value is used to identify fasl files. Even though this is not
;;; declared as a constant (because ANSI Common Lisp has no facility
;;; for declaring values which are constant under EQUAL but not EQL),
;;; obviously you shouldn't mess with it lightly. If you do set a new
;;; value for some reason, keep these things in mind:
;;; * To avoid confusion with the similar but incompatible CMU CL
;;;   fasl file format, the value should not be "FASL FILE", which
;;;   is what CMU CL used for the same purpose.
;;; * Since its presence at the head of a file is used by LOAD to
;;;   decide whether a file is to be fasloaded or just loaded
;;;   ordinarily (as source), the value should be something which
;;;   can't legally appear at the head of a Lisp source file.
;;; * The value should not contain any line-terminating characters,
;;;   because they're hard to express portably and because the LOAD
;;;   code might reasonably use READ-LINE to get the value to compare
;;;   against.
(defglobal *fasl-header-string-start-string* "# FASL")

;;; a list of SB-XC:*FEATURES* flags which affect binary compatibility,
;;; i.e. which must be the same between the SBCL which compiled the code
;;; and the SBCL which executes the code. This is a property of SBCL executables
;;; in the abstract, not of this particular SBCL executable,
;;; so any flag in this list may or may not be present
;;; in the *FEATURES* list of this particular build.
(defglobal *features-potentially-affecting-fasl-format*
    (append '(:sb-thread :sb-unicode :cheneygc :gencgc :msan :sb-safepoint)))

;;; the code for a character which terminates a fasl file header
(defconstant +fasl-header-string-stop-char-code+ 255)

;;; This value should be incremented when the system changes in such a
;;; way that it will no longer work reliably with old fasl files. In
;;; practice, I (WHN) have often forgotten to increment it for CVS
;;; versions which break binary compatibility. But it certainly should
;;; be incremented for release versions which break binary
;;; compatibility.
(defconstant +fasl-file-version+ 78)
;;; (description of versions before 0.9.0.1 deleted in 0.9.17)
;;; 56: (2005-05-22) Something between 0.9.0.1 and 0.9.0.14. My money is
;;;     on 0.9.0.6 (MORE CASE CONSISTENCY).
;;; 57: (2005-06-12) Raw slot rearrangement in 0.9.1.38
;;; 58: (2005-08-16) Multiple incompatible changes between 0.9.3 and 0.9.3.60
;;; 59: (2005-09-18) METAOBJECT implementation, removal of INSTANCE and
;;;     FUNCALLABLE-INSTANCE classes.
;;; 60: (2005-10-24) Bumped for 0.9.6
;;; 61: (2005-11-06) Improved source location recording added extra parameters
;;;     to multiple %DEFMUMBLE functions.
;;; 62: (2005-12-30) Make the count of FASL header counted strings
;;;     a 32-bit value also on 64-bit platforms.
;;; 63: (2006-01-27) Shuffle storage classes around to reduce the error
;;;     trap information size on RISCy platforms.
;;; 64: (2006-03-24) New calling convention for unknown-values on x86 and
;;;     x86-64.  Also (belatedly) PPC/gencgc, including :gencgc on FPAFF.
;;; 65: (2006-04-11) Package locking interface changed.
;;; 66: (2006-05-13) Fopcompiler
;;; 67: (2006-07-25) Reports on #lisp about 0.9.13 fasls being invalid on
;;;     0.9.14.something
;;; 68: (2006-08-14) changed number of arguments of LOAD-DEFMETHOD
;;; 69: (2006-08-17) changed validity of various initargs for methods
;;; 70: (2006-09-13) changes to *PSEUDO-ATOMIC* on x86 and x86-64
;;; 71: (2006-11-19) CLOS calling convention changes
;;; 72: (2006-12-05) Added slot to the primitive function type
;;; 73: (2007-04-13) Changed a hash function
;;; 74: (2007-06-05) UNWIND-TO-FRAME-AND-CALL
;;; 75: (2007-08-06) FD-STREAM layout changes
;;; 76: (2007-10-05) MUTEX layout changes
;;; 77: (2007-11-08) Essentially obsolete fasl-file-version, fasls are now
;;;     considered compatible only when the version numbers of the compiling
;;;     SBCL instance is exactly the same as the one of the loading instance.
;;;     Further fasl-file-version bumps should only be done for real changes
;;;     in the fasl format, not for changes in function/macro signatures or
;;;     lisp data structures.
;;; 78: (2010-04-02) Add FOP-{SMALL-,}NAMED-PACKAGE, remove FOP-NORMAL-LOAD
;;;     and FOP-MAYBE-COLD-LOAD.

;;; the conventional file extension for our fasl files
;;; FIXME this should be (DEFCONSTANT-EQX +FASL-FILE-TYPE+ "fasl" #'EQUAL),
;;; but renaming the variable would harm 'asdf-dependency-grovel' and other
;;; random 3rd-party libraries. However, we can't keep the name and make it
;;; constant, because the compiler warns about asterisks on constants.
;;; So we keep the asterisks and make it defglobal.
(declaim (type simple-string *fasl-file-type*))
(defglobal *fasl-file-type* "fasl")


;;;; internal state variables

(defvar *load-depth* 0
  "the current number of recursive LOADs")
(declaim (type index *load-depth*))

;;;; miscellaneous load utilities

(defun make-fop-vector (size)
  (declare (type index size))
  (let ((vector (make-array size)))
    (setf (aref vector 0) 0)
    vector))

;;; a holder for the FASL file we're reading from
(defstruct (fasl-input (:conc-name %fasl-input-)
                       (:constructor make-fasl-input (stream print))
                       (:predicate nil)
                       (:copier nil))
  (stream nil :type ansi-stream :read-only t)
  (table (make-fop-vector 1000) :type simple-vector)
  (stack (make-fop-vector 100) :type simple-vector)
  (name-buffer (vector (make-string  1 :element-type 'character)
                       (make-string 31 :element-type 'base-char)))
  (print nil :type boolean)
  ;; We keep track of partial source info for the input in case
  ;; loading gets interrupted.
  (partial-source-info nil :type (or null sb-c::debug-source)))
(declaim (freeze-type fasl-input))

;;; Lisp assembler routines are named by Lisp symbols, not strings,
;;; and so can be compared by EQ.
(define-load-time-global *assembler-routines* nil)
;;; word vector in static space of addresses of asm routines
#+immobile-space (define-load-time-global *asm-routine-vector* nil)
#-sb-xc-host (declaim (code-component *assembler-routines*))

;;; Output the current number of semicolons after a fresh-line.
;;; FIXME: non-mnemonic name
(defun load-fresh-line ()
  (fresh-line)
  (let ((semicolons ";;;;;;;;;;;;;;;;"))
    (do ((count *load-depth* (- count (length semicolons))))
        ((< count (length semicolons))
         (write-string semicolons *standard-output* :end count))
      (declare (fixnum count))
      (write-string semicolons))
    (write-char #\space)))

;;; If VERBOSE, output (to *STANDARD-OUTPUT*) a message about how
;;; we're loading from STREAM-WE-ARE-LOADING-FROM.
(defun maybe-announce-load (stream-we-are-loading-from verbose)
  (when verbose
    (load-fresh-line)
    (let ((name #-sb-xc-host (file-name stream-we-are-loading-from)
                #+sb-xc-host nil))
      (if name
          (format t "loading ~S~%" name)
          (format t "loading stuff from ~S~%" stream-we-are-loading-from)))))

;;;; utilities for reading from fasl files

;;; This expands into code to read an N-byte unsigned integer using
;;; FAST-READ-BYTE.
(defmacro fast-read-u-integer (n)
  (let (bytes)
    `(let ,(loop for i from 0 below n
                 collect (let ((name (gensym "B")))
                           (push name bytes)
                           `(,name ,(if (zerop i)
                                        `(fast-read-byte)
                                        `(ash (fast-read-byte) ,(* i 8))))))
       (logior ,@bytes))))

;;; like FAST-READ-U-INTEGER, but the size may be determined at run time
(defmacro fast-read-var-u-integer (n)
  (let ((n-pos (gensym))
        (n-res (gensym))
        (n-cnt (gensym)))
    `(do ((,n-pos 8 (+ ,n-pos 8))
          (,n-cnt (1- ,n) (1- ,n-cnt))
          (,n-res
           (fast-read-byte)
           (dpb (fast-read-byte) (byte 8 ,n-pos) ,n-res)))
         ((zerop ,n-cnt) ,n-res)
       (declare (type index ,n-pos ,n-cnt)))))

;;; FIXME: why do all of these reading functions and macros declare
;;; (SPEED 0)?  was there some bug in the compiler which has since
;;; been fixed?  --njf, 2004-09-08
;;; Afaict, the (SPEED 0) declarations in here avoid code bloat,
;;; by counteracting the INLINE declaration on the local definition
;;; of FAST-READ-BYTE. At least, that is the effect, and it seems
;;; reasonable. Pretty much the INLINE declaration is probably
;;; the thing that deserves to go away.

;;; Read a signed integer.
(defmacro fast-read-s-integer (n)
  (declare (optimize (speed 0)))
  (let ((n-last (gensym)))
    (do ((res `(let ((,n-last (fast-read-byte)))
                 (if (zerop (logand ,n-last #x80))
                     ,n-last
                     (logior ,n-last #x-100)))
              `(logior (fast-read-byte)
                       (ash (the (signed-byte ,(* cnt 8)) ,res) 8)))
         (cnt 1 (1+ cnt)))
        ((>= cnt n) res))))

;;; Read an N-byte unsigned integer from FASL-INPUT-STREAM.
(defmacro read-arg (n fasl-input-stream)
  (if (= n 1)
      `(the (unsigned-byte 8) (read-byte ,fasl-input-stream))
      `(with-fast-read-byte ((unsigned-byte 8) ,fasl-input-stream)
         (fast-read-u-integer ,n))))

(declaim (inline read-byte-arg))
(defun read-byte-arg (stream)
  (declare (optimize (speed 0)))
  (read-arg 1 stream))

(defun read-word-arg (stream)
  (declare (optimize (speed 0)))
  (read-arg #.sb-vm:n-word-bytes stream))

(defun read-unsigned-byte-32-arg (stream)
  (declare (optimize (speed 0)))
  (read-arg 4 stream))

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
            (code-char (fast-read-byte)))))
  string)
;;; Variation 2: base-string, transfer elements of type (unsigned-byte 8)
(defun read-base-string-as-bytes (stream string &optional (length (length string)))
  (declare (type (simple-array base-char (*)) string)
           (type index length)
           (optimize speed))
  (with-fast-read-byte ((unsigned-byte 8) stream)
    (dotimes (i length)
      (setf (aref string i)
            (code-char (fast-read-byte)))))
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
              (code-char (read-varint))))))
  string)


;;;; the fop table

;;; The table is implemented as a simple-vector indexed by the table
;;; offset. The offset is kept in at index 0 of the vector.
;;;
;;; FOPs use the table to save stuff, other FOPs refer to the table by
;;; direct indexes via REF-FOP-TABLE.

(declaim (inline ref-fop-table))
(defun ref-fop-table (fasl-input index)
  (svref (%fasl-input-table fasl-input) (1+ (the index index))))

(defun push-fop-table (thing fasl-input) ; and return THING
  (let* ((table (%fasl-input-table fasl-input))
         (index (+ (the index (aref table 0)) 1)))
    (declare (fixnum index)
             (simple-vector table))
    (when (eql index (length table))
      (setf table (grow-fop-vector table index)
            (%fasl-input-table fasl-input) table))
    (setf (aref table 0) index
          (aref table index) thing)))

;;; These two routines are used for both the stack and the table.
(defun grow-fop-vector (old-vector old-size)
  (declare (simple-vector old-vector)
           (type index old-size))
  (let* ((new-size (* old-size 2))
         (new-vector (make-array new-size)))
    (declare (fixnum new-size)
             (simple-vector new-vector old-vector))
    (replace new-vector old-vector)
    (nuke-fop-vector old-vector)
    new-vector))

(defun nuke-fop-vector (vector)
  (declare (simple-vector vector)
           (ignorable vector)
           (optimize speed))
  ;; Make sure we don't keep any garbage.
  ;; NOTE: for the work-in-progress concurrent GC, it is better *NOT* to 0-fill
  ;; if the the deletion barrier is enabled.
  (fill vector 0))


;;;; the fop stack

(declaim (inline fop-stack-empty-p))
(defun fop-stack-empty-p (stack)
  (eql 0 (svref stack 0)))

;; Ensure that N arguments can be popped from the FOP stack.
;; Return the stack and the pointer to the first argument.
;; Update the new top-of-stack to reflect that all N have been popped.
(defun fop-stack-pop-n (stack n)
  (declare (type index n))
  (let* ((top (the index (svref stack 0)))
         (new-top (- top n)))
    (if (minusp new-top) ; 0 is ok at this point
        (error "FOP stack underflow")
        (progn (setf (svref stack 0) new-top)
               (1+ new-top)))))

(defun push-fop-stack (value fasl-input)
  (let* ((stack (%fasl-input-stack fasl-input))
         (next (1+ (the index (svref stack 0)))))
    (declare (type index next))
    (when (eql (length stack) next)
      (setf stack (grow-fop-vector stack next)
            (%fasl-input-stack fasl-input) stack))
    (setf (svref stack 0) next
          (svref stack next) value)))

;;; Bind STACK-VAR and PTR-VAR to the start of a subsequence of
;;; the fop stack of length COUNT, then execute BODY.
;;; Within the body, FOP-STACK-REF is used in lieu of SVREF
;;; to elide bounds checking.
(defmacro with-fop-stack (((stack-var &optional stack-expr) ptr-var count)
                          &body body)
  `(macrolet ((fop-stack-ref (i)
                `(locally
                     #-sb-xc-host
                     (declare (optimize (sb-c:insert-array-bounds-checks 0)))
                   (svref ,',stack-var (truly-the index ,i)))))
     (let* (,@(when stack-expr
                (list `(,stack-var (the simple-vector ,stack-expr))))
            (,ptr-var (truly-the index (fop-stack-pop-n ,stack-var ,count))))
       ,@body)))


;;;; the FOP database

;; The bottom 5 bits of the opcodes above 128 encode an implicit operand.
(defconstant n-ordinary-fops 128)

;;; a vector indexed by a FaslOP that yields a function which performs
;;; the operation. Most functions take 0 arguments - they only manipulate
;;; the fop stack. But if the fop is defined to receive an argument (or two)
;;; then loader's main loop is responsible for supplying it.
(defglobal **fop-funs** (make-array n-ordinary-fops :initial-element 0))
(declaim (type (simple-vector #.n-ordinary-fops) **fop-funs**))

;;; Two arrays indicate fop function signature.
;;; The first array indicates how many integer operands follow the opcode.
;;; The second tells whether the fop wants its result pushed on the stack.
(declaim (type (cons (simple-array (mod 4) (#.n-ordinary-fops))
                     (simple-bit-vector #.n-ordinary-fops))
               **fop-signatures**))
(defglobal **fop-signatures**
    (cons (make-array n-ordinary-fops :element-type '(mod 4) :initial-element 0)
          (make-array n-ordinary-fops :element-type 'bit :initial-element 0)))


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
                    (operand-stack () '(%fasl-input-stack (fasl-input))))
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

(defglobal *fop-name-to-opcode* (make-hash-table))
(defun !%define-fop (name opcode n-operands pushp)
  (declare (type (mod 4) n-operands))
  (let ((function (svref **fop-funs** opcode)))
    (when (functionp function)
      (let ((oname (nth-value 2 (function-lambda-expression function))))
        (when (and oname (not (eq oname name)))
          (cerror "Define it anyway"
                  "fop ~S with opcode ~D conflicts with fop ~S."
                 name opcode oname))))
    (let ((existing-opcode (get name 'opcode)))
      (when (and existing-opcode (/= existing-opcode opcode))
        (error "multiple codes for fop name ~S: ~D and ~D"
               name opcode existing-opcode)))
    (setf (gethash name *fop-name-to-opcode*) opcode
          (svref **fop-funs** opcode) (symbol-function name)
          (aref (car **fop-signatures**) opcode) n-operands
          (sbit (cdr **fop-signatures**) opcode) pushp))
  name)


;;;; Conditions signalled on invalid fasls (wrong fasl version, etc),
;;;; so that user code (esp. ASDF) can reasonably handle attempts to
;;;; load such fasls by recompiling them, etc. For simplicity's sake
;;;; make only condition INVALID-FASL part of the public interface,
;;;; and keep the guts internal.

(define-condition invalid-fasl (error)
  ((stream :reader invalid-fasl-stream :initarg :stream)
   (expected :reader invalid-fasl-expected :initarg :expected))
  (:report
   (lambda (condition stream)
     (format stream "~S is an invalid fasl file."
             (invalid-fasl-stream condition)))))

(define-condition invalid-fasl-header (invalid-fasl)
  ((byte :reader invalid-fasl-byte :initarg :byte)
   (byte-nr :reader invalid-fasl-byte-nr :initarg :byte-nr))
  (:report
   (lambda (condition stream)
     (format stream "~@<~S contains an illegal byte in the FASL header at ~
                     position ~A: Expected ~A, got ~A.~:@>"
             (invalid-fasl-stream condition)
             (invalid-fasl-byte-nr condition)
             (invalid-fasl-expected condition)
             (invalid-fasl-byte condition)))))

(define-condition invalid-fasl-version (invalid-fasl)
  ((version :reader invalid-fasl-version :initarg :version))
  (:report
   (lambda (condition stream)
     (format stream "~@<~S is a fasl file compiled with SBCL ~W, and ~
                      can't be loaded into SBCL ~W.~:@>"
             (invalid-fasl-stream condition)
             (invalid-fasl-version condition)
             (invalid-fasl-expected condition)))))

(define-condition invalid-fasl-implementation (invalid-fasl)
  ((implementation :reader invalid-fasl-implementation
                   :initarg :implementation))
  (:report
   (lambda (condition stream)
     (format stream "~S was compiled for implementation ~A, but this is a ~A."
             (invalid-fasl-stream condition)
             (invalid-fasl-implementation condition)
             (invalid-fasl-expected condition)))))

(define-condition invalid-fasl-features (invalid-fasl)
  ((features :reader invalid-fasl-features :initarg :features))
  (:report
   (lambda (condition stream)
     (format stream "~@<incompatible features ~A ~_in fasl file ~S: ~2I~_~
                     Runtime expects ~A~:>"
             (invalid-fasl-features condition)
             (invalid-fasl-stream condition)
             (invalid-fasl-expected condition)))))

;;; Skips past the shebang line on stream, if any.
(defun maybe-skip-shebang-line (stream)
  (let ((p (file-position stream)))
    (when p
      (flet ((next () (read-byte stream nil)))
        (unwind-protect
             (when (and (eq (next) (char-code #\#))
                        (eq (next) (char-code #\!)))
               (setf p nil)
               (loop for x = (next)
                     until (or (not x) (eq x (char-code #\newline)))))
          (when p
            (file-position stream p)))))))

;;; Return a string representing symbols in *FEATURES-POTENTIALLY-AFFECTING-FASL-FORMAT*
;;; which are present in a particular compilation.
(defun compute-features-affecting-fasl-format ()
  (let ((list (sort (copy-list (intersection *features-potentially-affecting-fasl-format*
                                             sb-xc:*features*))
                    #'string< :key #'symbol-name)))
    ;; Stringify the subset of *FEATURES* that affect fasl format.
    ;; A list would be the natural representation choice for this, but a string
    ;; is convenient for and a requirement for writing to and reading from fasls
    ;; at this stage of the loading. WITH-STANDARD-IO-SYNTAX and WRITE-TO-STRING
    ;; would work, but this is simple enough to do by hand.
    (%with-output-to-string (stream)
      (let ((delimiter #\())
        (dolist (symbol list)
          (write-char delimiter stream)
          (write-string (string symbol) stream)
          (setq delimiter #\Space)))
      (write-char #\) stream))))

#-sb-xc-host
(eval-when (:compile-toplevel)
  (let ((string (compute-features-affecting-fasl-format)))
    (assert (and (> (length string) 2)
                 (not (find #\newline string))
                 (not (find #\# string))
                 (not (search ".." string))))))

;;;; LOAD-AS-FASL
;;;;
;;;; Note: LOAD-AS-FASL is used not only by LOAD, but also (with
;;;; suitable modification of the fop table) in GENESIS. Therefore,
;;;; it's needed not only in the target Lisp, but also in the
;;;; cross-compilation host.

;;; a helper function for LOAD-FASL-GROUP
;;;
;;; Return true if we successfully read a FASL header from the stream, or NIL
;;; if EOF was hit before anything except the optional shebang line was read.
;;; Signal an error if we encounter garbage.
(defun check-fasl-header (stream)
  (maybe-skip-shebang-line stream)
  (let ((byte (read-byte stream nil))
        (results))
    (when byte
      ;; Read and validate constant string prefix in fasl header.
      (let* ((fhsss *fasl-header-string-start-string*)
             (fhsss-length (length fhsss)))
        (unless (= byte (char-code (schar fhsss 0)))
          (error 'invalid-fasl-header
                 :stream stream
                 :byte-nr 0
                 :byte byte
                 :expected (char-code (schar fhsss 0))))
        (do ((byte (read-byte stream) (read-byte stream))
             (count 1 (1+ count)))
            ((= byte +fasl-header-string-stop-char-code+)
             t)
          (declare (fixnum byte count))
          (when (and (< count fhsss-length)
                     (not (eql byte (char-code (schar fhsss count)))))
            (error 'invalid-fasl-header
                   :stream stream
                   :byte-nr count
                   :byte byte
                   :expected (char-code (schar fhsss count))))))
      ;; Read and validate version-specific compatibility stuff.
      (flet ((string-from-stream ()
               (let* ((length (read-unsigned-byte-32-arg stream))
                      (result (make-string length)))
                 (read-string-as-bytes stream result)
                 (push result results)
                 result))
             (unsuffix (s)
               (if (and (> (length s) 4)
                        (string= s "-WIP" :start1 (- (length s) 4)))
                   (subseq s 0 (- (length s) 4))
                   s)))
        ;; Read and validate implementation and version.
        (let ((implementation (string-from-stream))
              (expected-implementation +backend-fasl-file-implementation+))
          (unless (string= expected-implementation implementation)
            (error 'invalid-fasl-implementation
                   :stream stream
                   ;; This slot used to hold a symbol. Now it's a string.
                   ;; I don't think anyone should care, but if they do,
                   ;; then this needs a call to KEYWORDICATE.
                   :implementation implementation
                   :expected expected-implementation)))
        (let* ((fasl-version (read-word-arg stream))
               (sbcl-version (if (<= fasl-version 76)
                                 "1.0.11.18"
                                 (string-from-stream)))
               (expected-version (lisp-implementation-version)))
          (push fasl-version results)
          (unless (string= (unsuffix expected-version) (unsuffix sbcl-version))
            (restart-case
                (error 'invalid-fasl-version
                       :stream stream
                       :version sbcl-version
                       :expected expected-version)
              (continue () :report "Load the fasl file anyway"))))
        ;; Read and validate *FEATURES* which affect binary compatibility.
        (let ((faff-in-this-file (string-from-stream))
              (expected (compute-features-affecting-fasl-format)))
          (unless (string= faff-in-this-file expected)
            (error 'invalid-fasl-features
                   :stream stream
                   :expected expected
                   :features faff-in-this-file)))
        ;; success
        (nreverse results)))))

;; Setting this variable gives you a trace of fops as they are loaded and
;; executed.
(defvar *show-fops-p* nil)

;;; Return byte, function, pushp, n-operands, arg1, arg2, arg3
(defun decode-fop (fasl-input &aux (stream (%fasl-input-stream fasl-input)))
  (with-fast-read-byte ((unsigned-byte 8) stream)
    (flet ((read-varint ()
             (let ((accumulator 0)
                   (shift 0))
               (declare (fixnum shift) (type word accumulator))
               (loop
                 (let ((octet (fast-read-byte)))
                   (setq accumulator (logior accumulator (ash (logand octet #x7F) shift)))
                   (incf shift 7)
                   (unless (logbitp 7 octet) (return accumulator)))))))
      (let ((byte (fast-read-byte)))
        (if (< byte n-ordinary-fops)
            (let ((n-operands (aref (car **fop-signatures**) byte)))
              (values byte
                      (svref **fop-funs** byte)
                      (plusp (sbit (cdr **fop-signatures**) byte))
                      n-operands
                      (when (>= n-operands 1) (read-varint))
                      (when (>= n-operands 2) (read-varint))
                      (when (>= n-operands 3) (read-varint))))
            (let* ((operand (logand byte #x7f))
                   (nconses (logand operand #b1111)))
              (aver (not (logtest #b1100000 operand)))
              ;; Decode as per TERMINATE-[UN]DOTTED-LIST in src/compiler/dump
              (values byte
                      (if (logbitp 4 operand) #'fop-list* #'fop-list)
                      t
                      1
                      (if (zerop nconses) (+ (read-varint) 16) nconses)
                      nil
                      nil)))))))

(defstruct fasl-group
  (fun-names))

(defvar *current-fasl-group*)
;;;
;;; a helper function for LOAD-AS-FASL
;;;
;;; Return true if we successfully load a group from the stream, or
;;; NIL if EOF was encountered while trying to read from the stream.
;;; Dispatch to the right function for each fop.
(defun load-fasl-group (fasl-input)
  (let ((stream (%fasl-input-stream fasl-input))
        (trace *show-fops-p*))
    (unless (check-fasl-header stream)
      (return-from load-fasl-group))
    (let ((*current-fasl-group* (make-fasl-group)))
      (catch 'fasl-group-end
        (setf (svref (%fasl-input-table fasl-input) 0) 0)
        (loop
         (binding* ((pos (when trace (file-position stream)))
                    ((byte function pushp n-operands arg1 arg2 arg3)
                     (decode-fop fasl-input))
                    (result
                     (if (functionp function)
                         (case n-operands
                           (0 (funcall function fasl-input))
                           (1 (funcall function fasl-input arg1))
                           (2 (funcall function fasl-input arg1 arg2))
                           (3 (funcall function fasl-input arg1 arg2 arg3)))
                         (error "corrupt fasl file: FOP code #x~x" byte))))
           (when pushp
             (push-fop-stack result fasl-input))
           (when trace
             ;; show file pos prior to decoding the fop,
             ;; table and stack ptrs *after* executing it
             (format *trace-output* "~&~6x : [~D,~D] ~2,'0x~v@{ ~x~}"
                     pos
                     (svref (%fasl-input-table fasl-input) 0) ; table pointer
                     (svref (%fasl-input-stack fasl-input) 0) ; stack pointer
                     byte
                     n-operands arg1 arg2 arg3)
             (when (functionp function)
               (format *trace-output* " ~35t~(~a~)" (%fun-name function))
               (case (%fun-name function)
                 (fop-push (format *trace-output* " ~(~A~)" (ref-fop-table fasl-input arg1)))
                 (fop-word-integer (format *trace-output* " ~V,'0X" (* 2 sb-vm:n-word-bytes) result))
                 (fop-byte-integer (format *trace-output* " ~2,'0X" result))
                 (fop-integer (format *trace-output* " ~X" result)))))))))))

(defun load-as-fasl (stream verbose print)
  (when (zerop (file-length stream))
    (error "attempt to load an empty FASL file:~%  ~S" (namestring stream)))
  (maybe-announce-load stream verbose)
  (let ((fasl-input (make-fasl-input stream print)))
    (with-loader-package-names
      (unwind-protect
           (loop while (load-fasl-group fasl-input))
        ;; Nuke the table and stack to avoid keeping garbage on
        ;; conservatively collected platforms.
        (nuke-fop-vector (%fasl-input-table fasl-input))
        (nuke-fop-vector (%fasl-input-stack fasl-input)))))
  t)


;;; Compatibity macros that allow some fops to share the identical
;;; body between genesis and the target code.
#-sb-xc-host
(progn
  (defmacro cold-cons (x y) `(cons ,x ,y))
  (defmacro number-to-core (x) x)
  (defmacro make-character-descriptor (x) `(code-char ,x)))

;;;; Actual FOP definitions:

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
  (let ((res (sb-kernel::%new-instance* layout size)) ; number of words excluding header
        ;; Discount the layout from number of user-visible words.
        (n-data-words (- size sb-vm:instance-data-start)))
    (with-fop-stack ((stack (operand-stack)) ptr n-data-words)
      (declare (type index ptr))
      ;; Values on the stack are in the same order as in the structure itself.
      (do-layout-bitmap (i taggedp layout size)
        (let ((val (fop-stack-ref ptr)))
          (if taggedp
              (%instance-set res i val)
              (%raw-instance-set/word res i val)))
        (incf ptr)))
    res))

(define-fop 45 :not-host (fop-layout ((:operands depthoid flags length)
                                       name bitmap inherits))
  (decf depthoid) ; was bumped by 1 since non-stack args can't encode negatives
  (sb-kernel::load-layout name depthoid inherits length bitmap flags))

;;; This is dumped when the compiler detects that MAKE-LOAD-FORM
;;; returned a simple use of MAKE-LOAD-FORM-SAVING-SLOTS, or possibly
;;; a hand-written equivalent (however unlikely).
(define-fop 68 :not-host (fop-instance ((:operands n-slots) name))
  (let* ((instance (allocate-instance (find-class (the symbol name))))
         (stack (operand-stack))
         (ptr (fop-stack-pop-n stack (* 2 n-slots))))
    (dotimes (i n-slots)
      (let* ((index (+ ptr (* 2 i)))
             (value (svref stack index))
             (slot-name (svref stack (1+ index))))
        (if (unbound-marker-p value)
            ;; SLOT-MAKUNBOUND-USING-CLASS might do something nonstandard.
            (slot-makunbound instance slot-name)
            (setf (slot-value instance slot-name) value))))
    instance))

(define-fop 64 (fop-end-group ((:operands table-size)) nil)
  (unless (= (svref (%fasl-input-table (fasl-input)) 0) table-size)
    (bug "fasl table of improper size"))
  (unless (fop-stack-empty-p (operand-stack))
    (bug "fasl stack not empty when it should be"))
  (throw 'fasl-group-end t))

;;;; fops for loading symbols

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
         (aux-fop-intern (length+flag package inherited fasl-input)
           (multiple-value-bind (name length elt-type)
               (read-symbol-name length+flag fasl-input)
             (push-fop-table (%intern name length package elt-type t inherited)
                             fasl-input))))

  (define-fop 77 :not-host (fop-lisp-symbol-save ((:operands length+flag)))
    (aux-fop-intern length+flag *cl-package* t (fasl-input)))
  (define-fop 78 :not-host (fop-keyword-symbol-save ((:operands length+flag)))
    (aux-fop-intern length+flag *keyword-package* t (fasl-input)))
  (define-fop 79 :not-host (fop-symbol-in-package-save ((:operands length+flag pkg-index)))
    (aux-fop-intern length+flag (ref-fop-table (fasl-input) pkg-index) t (fasl-input)))
  (define-fop 84 :not-host (fop-symbol-in-package-internal-save ((:operands length+flag pkg-index)))
    (aux-fop-intern length+flag (ref-fop-table (fasl-input) pkg-index) nil (fasl-input)))

  (define-fop 80 :not-host (fop-uninterned-symbol-save ((:operands length+flag)))
    (multiple-value-bind (name len) (read-symbol-name length+flag (fasl-input))
      (push-fop-table (make-symbol (subseq name 0 len))
                      (fasl-input))))

  (define-fop 81 :not-host (fop-copy-symbol-save ((:operands table-index)))
    (push-fop-table (copy-symbol (ref-fop-table (fasl-input) table-index))
                    (fasl-input))))

(define-fop 82 (fop-package (pkg-designator))
  (find-undeleted-package-or-lose pkg-designator))

(define-fop 83 :not-host (fop-named-package-save ((:operands length)) nil)
  (let ((package-name (make-string length)))
    (read-char-string-as-varints (fasl-input-stream) package-name)
    (push-fop-table (find-or-maybe-make-deferred-package package-name)
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
  #+sb-xc-host (number-pair-to-core realpart imagpart sb-vm:complex-rational-widetag)
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
            ((logbitp 6 tag)
             (%make-simd-pack-256 (logand tag #b00111111)
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

(define-fop 43 (fop-spec-vector  ((:operands length)))
  (let* ((widetag (read-byte-arg (fasl-input-stream)))
         (bits (* length (sb-vm::simple-array-widetag->bits-per-elt widetag)))
         (bytes (ceiling bits sb-vm:n-byte-bits))
         (words (ceiling bytes sb-vm:n-word-bytes))
         (vector (logically-readonlyize
                  (allocate-vector #+(and (not sb-xc-host) ubsan) nil
                                   widetag length words))))
    (declare (type index length bytes words)
             (type word bits))
    (read-n-bytes (fasl-input-stream) vector 0 bytes)
    vector))

(defun fop-funcall* (argc stack)
  (with-fop-stack ((stack) ptr (1+ argc))
    (if (zerop argc)
        (funcall (fop-stack-ref ptr))
        (do ((i (+ ptr argc))
             (args))
            ((= i ptr) (apply (fop-stack-ref i) args))
          (declare (type index i))
          (push (fop-stack-ref i) args)
          (decf i)))))

(define-fop 55 (fop-funcall ((:operands n)))
  (fop-funcall* n (operand-stack)))
(define-fop 56 (fop-funcall-for-effect ((:operands n)) nil)
  (fop-funcall* n (operand-stack)))

;;;; fops for fixing up circularities

(define-fop 11 (fop-rplaca ((:operands tbl-slot index) value) nil)
  (let ((obj (ref-fop-table (fasl-input) tbl-slot)))
    (setf (car (nthcdr index obj)) value)))

(define-fop 12 (fop-rplacd ((:operands tbl-slot index) value) nil)
  (let ((obj (ref-fop-table (fasl-input) tbl-slot)))
    (setf (cdr (nthcdr index obj)) value)))

(define-fop 13 (fop-svset ((:operands tbl-slot index) value) nil)
  (setf (svref (ref-fop-table (fasl-input) tbl-slot) index) value))

(define-fop 14 :not-host (fop-structset ((:operands tbl-slot index) value) nil)
  (%instance-set (ref-fop-table (fasl-input) tbl-slot) index value))

(define-fop 15 :not-host (fop-slotset ((:operands tbl-slot index) value slot-name) nil)
  index
  (setf (slot-value (ref-fop-table (fasl-input) tbl-slot) slot-name) value))

(define-fop 16 (fop-nthcdr ((:operands n) obj))
  (nthcdr n obj))

;;;; fops for loading functions

;;; (In CMU CL there was a FOP-CODE-FORMAT (47) which was
;;; conventionally placed at the beginning of each fasl file to test
;;; for compatibility between the fasl file and the CMU CL which
;;; loaded it. In SBCL, this functionality has been replaced by
;;; putting the implementation and version in required fields in the
;;; fasl file header.)

;;; Caution: don't try to "test" WITH-WRITABLE-CODE-INSTRUCTIONS in copy-in/out mode
;;; on any architecture where fixup application cares what the address of the code actually is.
;;; This means x86 is disqualified. You're just wasting your time if you try, as I did.
(defmacro with-writable-code-instructions ((code-var total-nwords debug-info-var
                                            n-fdefns n-funs)
                                           &key copy fixup)
  (declare (ignorable n-fdefns))
  ;; N-FDEFNS is important for PPC64, slightly important for X86-64, not important for
  ;; any others, and doesn't even have a place to store it if lispwords are 32 bits.
  ;; The :DEDUPLICATED-FDEFNS test in compiler-2.pure asserts that the value is valid
  (let ((body
         ;; The following operations need the code pinned:
         ;; 1. copying into code-instructions (a SAP)
         ;; 2. apply-core-fixups and sanctify-for-execution
         ;; A very specific store order is necessary to allow using uninitialized memory
         ;; pages for code. Storing of the debug-info slot must occur between steps 1 and 2.
         ;; Note that this does not have to take care to ensure atomicity
         ;; of the store to the final word of unboxed data. Even if BYTE-BLT were
         ;; interrupted in between the store of any individual byte, this code
         ;; is GC-safe because we no longer need to know where simple-funs are embedded
         ;; within the object to trace pointers. We *do* need to know where the funs
         ;; are when transporting the object, but it's pinned within the body forms.
         `(,copy
           (sb-c::code-header/trailer-adjust ,code-var ,total-nwords ,n-fdefns)
           ;; Check that the code trailer matches our expectation on number of embedded simple-funs
           (aver (= (code-n-entries ,code-var) ,n-funs))
           ;; Until debug-info is assigned, it is illegal to create a simple-fun pointer
           ;; into this object, because the C code assumes that the fun table is in an
           ;; invalid/incomplete state (i.e. can't be read) until the code has debug-info.
           ;; That is, C code can't deal with an interior code pointer until the fun-table
           ;; is valid. This store must occur prior to calling %CODE-ENTRY-POINT, and
           ;; applying fixups calls %CODE-ENTRY-POINT, so we have to do this before that.
           (setf (%code-debug-info ,code-var) ,debug-info-var)
           ,fixup)))
    #+darwin-jit
    `(with-pinned-objects (,code-var ,debug-info-var)
       ;; DEBUG-INFO is pinned so that after assigning it into the temporary
       ;; block of memory, the off-heap word which is invisible to GC remains valid.
       (let* ((temp-copy (alien-funcall (extern-alien "duplicate_codeblob_offheap"
                                                      (function unsigned unsigned))
                                        (get-lisp-obj-address ,code-var)))
              (aligned (+ temp-copy (logand temp-copy sb-vm:n-word-bytes))))
         ;; Rebind CODE-VAR to the replica, then execute BODY
         (let ((,code-var (%make-lisp-obj (logior aligned sb-vm:other-pointer-lowtag)))) ,@body)
         ;; Copy back, and fixup the simple-funs in the managed object
         (alien-funcall (extern-alien "jit_copy_code_insts" (function void unsigned unsigned))
                        (get-lisp-obj-address ,code-var)
                        temp-copy)))
    #-darwin-jit
    `(with-pinned-objects (,code-var) ,@body)))

(define-load-time-global *show-new-code* nil)
#+sb-xc-host
(defun possibly-log-new-code (object reason)
  (declare (ignore reason))
  object)
#-sb-xc-host
(defun possibly-log-new-code (object reason &aux (show *show-new-code*))
  (when show
    (let ((size (code-object-size object))
          (fmt "~&New code(~Db,~A): ~A~%")
          (file "jit-code.txt")
          (*print-pretty* nil))
      ;; DISASSEMBLE is for limited debugging only.
      ;; It may write garbled output if multiple threads
      ;; I tried WITH-OPEN-STREAM during cold-init and got:
      ;;   "vicious metacircle:  The computation of an effective method of
      ;;    #<STREAM-FUNCTION COMMON-LISP:CLOSE (2)> for arguments of types
      ;;    (#<STRUCTURE-CLASS SB-SYS:FD-STREAM>) uses the effective method
      ;; being computed."
      ;; so just leave the stream open. Or we could call the fd-stream misc routine.
      (if (or (eq show 'disassemble) (streamp show))
          (let ((f (if (streamp show)
                       show
                       (prog1
                           (setq *show-new-code*
                                 (open file :direction :output
                                       :if-exists :append :if-does-not-exist :create))
                         (format t "~&; Logging code allocation to ~S~%" file)))))
            (format f fmt size reason object)
            (disassemble object :stream f)
            (terpri f)
            (force-output f))
          (format *trace-output* fmt (code-object-size object) reason object))))
  object)

;;; Unpack an integer from DUMP-FIXUPs.
(declaim (inline !unpack-fixup-info))
(defun !unpack-fixup-info (packed-info) ; Return (VALUES offset kind flavor-id data)
  (values (ash packed-info -16)
          (aref +fixup-kinds+ (ldb (byte 4 0) packed-info))
          (ldb (byte 4 4) packed-info)
          (ldb (byte 8 8) packed-info)))

(define-fop 17 :not-host (fop-load-code ((:operands header n-code-bytes n-fixup-elts)))
  ;; The stack looks like:
  ;; ... | constant0 constant1 ... constantN | DEBUG-INFO | FIXUPS-ITEMS ....   ||
  ;;     | <--------- n-constants ---------> |            | <-- n-fixup-elts -> ||
  (let* ((n-simple-funs (read-unsigned-byte-32-arg (fasl-input-stream)))
         (n-fdefns (read-unsigned-byte-32-arg (fasl-input-stream)))
         (n-boxed-words (ash header -1))
         (n-constants (- n-boxed-words sb-vm:code-constants-offset))
         (stack-elts-consumed (+ n-constants 1 n-fixup-elts)))
    (with-fop-stack ((stack (operand-stack)) ptr stack-elts-consumed)
      ;; We've already ensured that all FDEFNs the code uses exist.
      ;; This happened by virtue of calling fop-fdefn for each.
      (loop for stack-index from (+ ptr (* n-simple-funs sb-vm:code-slots-per-simple-fun))
            repeat n-fdefns
            do (aver (typep (svref stack stack-index) 'fdefn)))
      (binding* (((code total-nwords)
                  (sb-c:allocate-code-object
                   (if (oddp header) :immobile :dynamic)
                   (align-up n-boxed-words sb-c::code-boxed-words-align)
                   n-code-bytes))
                 (real-code code)
                 (debug-info (svref stack (+ ptr n-constants))))
        (with-writable-code-instructions
            (code total-nwords debug-info n-fdefns n-simple-funs)
          :copy (read-n-bytes (fasl-input-stream) (code-instructions code) 0 n-code-bytes)
          :fixup (sb-c::apply-fasl-fixups code stack (+ ptr (1+ n-constants)) n-fixup-elts real-code))
        ;; Don't need the code pinned from here on
        (setf (sb-c::debug-info-source (%code-debug-info code))
              (%fasl-input-partial-source-info (fasl-input)))
        (let ((stack-index (+ ptr (* n-simple-funs sb-vm:code-slots-per-simple-fun))))
          ;; This is the moral equivalent of a warning from /usr/bin/ld
          ;; that "gets() is dangerous." You're informed by both the compiler and linker.
          (dotimes (i n-fdefns)
            (let ((name (fdefn-name (svref stack (+ stack-index i)))))
              (when (deprecated-thing-p 'function name)
                (format *error-output* "~&; While loading ~S:" (sb-c::debug-info-name debug-info))
                (check-deprecated-thing 'function name)))))
        ;; Boxed constants can be assigned only after figuring out where the range
        ;; of implicitly tagged words is, which requires knowing how many functions
        ;; are in the code component, which requires reading the code trailer.
        #+darwin-jit (sb-c::assign-code-constants code (subseq stack ptr (+ ptr n-constants)))
        #-darwin-jit
        (let* ((header-index sb-vm:code-constants-offset)
               (stack-index ptr))
            (declare (type index header-index stack-index))
            (dotimes (n (* n-simple-funs sb-vm:code-slots-per-simple-fun))
              (setf (code-header-ref code header-index) (svref stack stack-index))
              (incf header-index)
              (incf stack-index))
            (dotimes (i n-fdefns)
              (sb-c::set-code-fdefn code header-index (svref stack stack-index))
              (incf header-index)
              (incf stack-index))
            (do () ((>= header-index n-boxed-words))
              (setf (code-header-ref code header-index) (svref stack stack-index))
              (incf header-index)
              (incf stack-index)))
        (when (typep (code-header-ref code (1- n-boxed-words))
                     '(cons (eql sb-c::coverage-map)))
          ;; Record this in the global list of coverage-instrumented code.
          (atomic-push (make-weak-pointer code) (cdr *code-coverage-info*)))
        (possibly-log-new-code code "load")))))

;; this gets you an #<fdefn> object, not the result of (FDEFINITION x)
;; cold-loader uses COLD-FDEFINITION-OBJECT instead.
(define-fop 18 :not-host (fop-fdefn (name))
  (find-or-create-fdefn name))

(define-fop 19 :not-host (fop-known-fun (name))
  (%coerce-name-to-fun name))

#+arm64
(define-fop 23 :not-host (fop-tls-index (name))
  (ash (ensure-symbol-tls-index name) (- sb-vm:n-fixnum-tag-bits)))

;;; This FOP is only encountered in cross-compiled FASLs for cold load,
;;; and is a no-op except in cold load. A developer may want to load a
;;; cross-compiled FASL into a running system, and this FOP doesn't
;;; have to do anything, as the system can load top level forms and
;;; will define the function normally.
(define-fop 74 :not-host (fop-fset (name fn) nil)
  (declare (ignore name fn)))

;;; Like FOP-FSET, but for method definitions.
(define-fop 75 :not-host (fop-mset (name qualifiers specializer fn) nil)
  (declare (ignore name qualifiers specializer fn)))

;;; Modify a slot of the code boxed constants.
(define-fop 20 (fop-alter-code ((:operands index) code value) nil)
  (flet (#+sb-xc-host
         ((setf code-header-ref) (value code index)
            (write-wordindexed code index value)))
    (setf (code-header-ref code index) value)
    (values)))

;;; Set the named constant value in the boxed constants, setting up
;;; backpatching information if the symbol is not yet bound. Forward
;;; references can occur at load time when non-top-level components
;;; containing named constant references get loaded before the top
;;; level form defining the constant gets loaded. This can happen when
;;; top level lambdas get merged.
#-sb-xc-host
(defun named-constant-set (code index name)
  (cond ((boundp name)
         (setf (code-header-ref code index) (symbol-global-value name)))
        (t
         (push (lambda (value)
                 (setf (code-header-ref code index) value))
               (info :variable :forward-references name)))))

(define-fop 121 :not-host (fop-named-constant-set ((:operands index) name code) nil)
  (named-constant-set code index name))

(define-fop 21 (fop-fun-entry ((:operands fun-index) code-object))
  (let ((fun (%code-entry-point code-object fun-index)))
    (when (%fasl-input-print (fasl-input))
      (load-fresh-line)
      (format t "~S loaded" fun))
    fun))

;;;; assemblerish fops

(define-fop 22 (fop-assembler-code)
  (error "cannot load assembler code except at cold load"))

;;;; fops for debug info

(define-fop 124 (fop-note-partial-source-info (namestring created plist) nil)
  (setf (%fasl-input-partial-source-info (fasl-input))
        (sb-c::make-debug-source :namestring namestring
                                 :created created
                                 :plist plist))
  (values))

(define-fop 125 :not-host (fop-note-full-calls (alist) nil)
  (sb-c::accumulate-full-calls alist)
  (values))

;;;; fops for code coverage

(define-fop 120 :not-host (fop-record-code-coverage (namestring paths) nil)
  (setf (gethash namestring (car *code-coverage-info*))
        (mapcar #'list paths))
  (values))

;;; Primordial layouts.
(macrolet ((frob (&rest specs)
             `(progn
                (defun known-layout-fop (name)
                  (case name
                    ,@(mapcar (lambda (spec) `((,(cadr spec)) ,(car spec)))
                              specs)))
                ,@(mapcar (lambda (spec)
                            `(define-fop ,(car spec) :not-host
                               (,(symbolicate "FOP-LAYOUT-OF-"
                                              (cadr spec)))
                               ,(find-layout (cadr spec))))
                          specs))))
  (frob (#x68 t)
        (#x69 structure-object)
        (#x6a condition)
        (#x6b sb-c::definition-source-location)
        (#x6c sb-c::debug-info)
        (#x6d sb-c::compiled-debug-info)
        (#x6e sb-c::debug-source)
        (#x6f defstruct-description)
        (#x70 defstruct-slot-description)))


;;;; stuff for debugging/tuning by collecting statistics on FOPs (?)

#|
(defvar *fop-counts* (make-array 256 :initial-element 0))
(defvar *fop-times* (make-array 256 :initial-element 0))
(defvar *print-fops* nil)

(defun clear-counts ()
  (fill (the simple-vector *fop-counts*) 0)
  (fill (the simple-vector *fop-times*) 0)
  t)

(defun analyze-counts ()
  (let ((counts ())
        (total-count 0)
        (times ())
        (total-time 0))
    (macrolet ((breakdown (lvar tvar vec)
                 `(progn
                   (dotimes (i 255)
                     (declare (fixnum i))
                     (let ((n (svref ,vec i)))
                       (push (cons (%fun-name (svref **fop-funs** i)) n) ,lvar)
                       (incf ,tvar n)))
                   (setq ,lvar (subseq (sort ,lvar (lambda (x y)
                                                     (> (cdr x) (cdr y))))
                                       0 10)))))

      (breakdown counts total-count *fop-counts*)
      (breakdown times total-time *fop-times*)
      (format t "Total fop count is ~D~%" total-count)
      (dolist (c counts)
        (format t "~30S: ~4D~%" (car c) (cdr c)))
      (format t "~%Total fop time is ~D~%" (/ (float total-time) 60.0))
      (dolist (m times)
        (format t "~30S: ~6,2F~%" (car m) (/ (float (cdr m)) 60.0))))))
|#
