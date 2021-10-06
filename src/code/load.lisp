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
                       (:constructor make-fasl-input (stream))
                       (:predicate nil)
                       (:copier nil))
  (stream nil :type ansi-stream :read-only t)
  (table (make-fop-vector 1000) :type simple-vector)
  (stack (make-fop-vector 100) :type simple-vector)
  (name-buffer (vector (make-string  1 :element-type 'character)
                       (make-string 31 :element-type 'base-char)))
  (deprecated-stuff nil :type list)
  ;; Sometimes we want to skip over any FOPs with side-effects (like
  ;; function calls) while executing other FOPs. SKIP-UNTIL will
  ;; either contain the position where the skipping will stop, or
  ;; NIL if we're executing normally.
  (skip-until nil :type (or null fixnum)))
(declaim (freeze-type fasl-input))

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
           #-gencgc (ignore vector)
           (optimize speed))
  ;; Make sure we don't keep any garbage.
  #+gencgc
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; The bottom 5 bits of the opcodes above 128 encode an implicit operand.
  (defconstant n-ordinary-fops 128))

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

;;;
;;; a helper function for LOAD-AS-FASL
;;;
;;; Return true if we successfully load a group from the stream, or
;;; NIL if EOF was encountered while trying to read from the stream.
;;; Dispatch to the right function for each fop.
;;;
;;; When true, PRINT causes most tlf-equivalent forms to print their primary value.
;;; This differs from loading of Lisp source, which prints all values of
;;; only truly-toplevel forms.  This is permissible per CLHS -
;;;  "If print is true, load incrementally prints information to standard
;;;   output showing the progress of the loading process. [...]
;;;   For a compiled file, what is printed might not reflect precisely the
;;;   contents of the source file, but some information is generally printed."
;;;
(defun load-fasl-group (fasl-input print)
  (declare (ignorable print))
  (let ((stream (%fasl-input-stream fasl-input))
        (trace *show-fops-p*))
    (unless (check-fasl-header stream)
      (return-from load-fasl-group))
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
             (format *trace-output* " ~35t~(~a~)" (%fun-name function))))
         (let ((stack (%fasl-input-stack fasl-input)))
           (declare (ignorable stack)) ; not used in xc-host
           (when (and (eq byte #.(get 'fop-funcall-for-effect 'opcode))
                      (fop-stack-empty-p stack)) ; (presumed) end of TLF
             (awhen (%fasl-input-deprecated-stuff fasl-input)
               ;; Delaying this message rather than printing it
               ;; in fop-fdefn makes it more informative (usually).
               (setf (%fasl-input-deprecated-stuff fasl-input) nil)
               (loader-deprecation-warn
                it
                (and (eq (svref stack 1) 'sb-impl::%defun) (svref stack 2))))
             (when print
               (load-fresh-line)
               (prin1 result)))))))))

;; This is the moral equivalent of a warning from /usr/bin/ld that
;; "gets() is dangerous." You're informed by both the compiler and linker.
(defun loader-deprecation-warn (stuff whence)
  ;; Stuff is a list: ((<state> name . category) ...)
  ;; For now we only deal with category = :FUNCTION so we ignore it.
  (let ((warning-class
         ;; We're only going to warn once (per toplevel form),
         ;; so pick the most stern warning applicable.
         (if (every (lambda (x) (eq (car x) :early)) stuff)
             'simple-style-warning 'simple-warning)))
    (warn warning-class
          :format-control "Reference to deprecated function~P ~S~@[ from ~S~]"
          :format-arguments
          (list (length stuff) (mapcar #'second stuff) whence))))

(defun load-as-fasl (stream verbose print)
  (when (zerop (file-length stream))
    (error "attempt to load an empty FASL file:~%  ~S" (namestring stream)))
  (maybe-announce-load stream verbose)
  (let ((fasl-input (make-fasl-input stream)))
    (unwind-protect
         (loop while (load-fasl-group fasl-input print))
      ;; Nuke the table and stack to avoid keeping garbage on
      ;; conservatively collected platforms.
      (nuke-fop-vector (%fasl-input-table fasl-input))
      (nuke-fop-vector (%fasl-input-stack fasl-input))))
  t)


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

