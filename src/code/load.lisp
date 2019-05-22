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

;;;; miscellaneous load utilities

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

#-sb-fluid (declaim (inline read-byte))

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

;; FIXME: on x86-64, these functions exceed 600, 900, and 1200 bytes of code
;; respectively. Either don't inline them, or make a "really" fast inline case
;; that punts if inapplicable. e.g. if the fast-read-byte buffer will not be
;; refilled, then SAP-REF-WORD could work to read 8 bytes.
;; But this would only be feasible on machines that are little-endian
;; and that allow unaligned reads. (like x86)
(declaim (inline read-byte-arg read-word-arg))
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
                 result)))
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
               (expected-version (sb-xc:lisp-implementation-version)))
          (push fasl-version results)
          (unless (string= expected-version sbcl-version)
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

(declaim (notinline read-byte)) ; Why is it even *declaimed* inline above?

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

