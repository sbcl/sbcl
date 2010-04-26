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

(in-package "SB!FASL")

;;;; There looks to be an exciting amount of state being modified
;;;; here: certainly enough that I (dan, 2003.1.22) don't want to mess
;;;; around deciding how to thread-safetify it.  So we use a Big Lock.
;;;; Because this code is mutually recursive with the compiler, we use
;;;; the **WORLD-LOCK**.

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

#!-sb-fluid (declaim (inline read-byte))

;;; FIXME: why do all of these reading functions and macros declare
;;; (SPEED 0)?  was there some bug in the compiler which has since
;;; been fixed?  --njf, 2004-09-08

;;; This expands into code to read an N-byte unsigned integer using
;;; FAST-READ-BYTE.
(defmacro fast-read-u-integer (n)
  (declare (optimize (speed 0)))
  (do ((res '(fast-read-byte)
            `(logior (fast-read-byte)
                     (ash ,res 8)))
       (cnt 1 (1+ cnt)))
      ((>= cnt n) res)))

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

;;; Read an N-byte unsigned integer from the *FASL-INPUT-STREAM*.
(defmacro read-arg (n)
  (declare (optimize (speed 0)))
  (if (= n 1)
      `(the (unsigned-byte 8) (read-byte *fasl-input-stream*))
      `(prepare-for-fast-read-byte *fasl-input-stream*
         (prog1
          (fast-read-u-integer ,n)
          (done-with-fast-read-byte)))))

(declaim (inline read-byte-arg read-halfword-arg read-word-arg))
(defun read-byte-arg ()
  (declare (optimize (speed 0)))
  (read-arg 1))

(defun read-halfword-arg ()
  (declare (optimize (speed 0)))
  (read-arg #.(/ sb!vm:n-word-bytes 2)))

(defun read-word-arg ()
  (declare (optimize (speed 0)))
  (read-arg #.sb!vm:n-word-bytes))

(defun read-unsigned-byte-32-arg ()
  (declare (optimize (speed 0)))
  (read-arg 4))


;;;; the fop table

;;; The table is implemented as a simple-vector indexed by the table
;;; offset. We may need to have several, since LOAD can be called
;;; recursively.

;;; a list of free fop tables for the fasloader
;;;
;;; FIXME: Is it really a win to have this permanently bound?
;;; Couldn't we just bind it on entry to LOAD-AS-FASL?
(defvar *free-fop-tables* (list (make-array 1000)))

;;; the current fop table
(defvar *current-fop-table*)
(declaim (simple-vector *current-fop-table*))

;;; the length of the current fop table
(defvar *current-fop-table-size*)
(declaim (type index *current-fop-table-size*))

;;; the index in the fop-table of the next entry to be used
(defvar *current-fop-table-index*)
(declaim (type index *current-fop-table-index*))

(defun grow-fop-table ()
  (let* ((new-size (* *current-fop-table-size* 2))
         (new-table (make-array new-size)))
    (declare (fixnum new-size) (simple-vector new-table))
    (replace new-table (the simple-vector *current-fop-table*))
    (setq *current-fop-table* new-table)
    (setq *current-fop-table-size* new-size)))

(defmacro push-fop-table (thing)
  (let ((n-index (gensym)))
    `(let ((,n-index *current-fop-table-index*))
       (declare (fixnum ,n-index))
       (when (= ,n-index (the fixnum *current-fop-table-size*))
         (grow-fop-table))
       (setq *current-fop-table-index* (1+ ,n-index))
       (setf (svref *current-fop-table* ,n-index) ,thing))))

;;;; the fop stack

;;; (This is to be bound by LOAD to an adjustable (VECTOR T) with
;;; FILL-POINTER, for use as a stack with VECTOR-PUSH-EXTEND.)
(defvar *fop-stack*)
(declaim (type (vector t) *fop-stack*))

;;; Cache information about the fop stack in local variables. Define a
;;; local macro to pop from the stack. Push the result of evaluation
;;; if PUSHP.
(defmacro with-fop-stack (pushp &body forms)
  (aver (member pushp '(nil t :nope)))
  (with-unique-names (fop-stack)
    `(let ((,fop-stack *fop-stack*))
       (declare (type (vector t) ,fop-stack)
                (ignorable ,fop-stack))
       (macrolet ((pop-stack ()
                    `(vector-pop ,',fop-stack))
                  (push-stack (value)
                    `(vector-push-extend ,value ,',fop-stack))
                  (call-with-popped-args (fun n)
                    `(%call-with-popped-args ,fun ,n ,',fop-stack)))
         ,(if pushp
              `(vector-push-extend (progn ,@forms) ,fop-stack)
              `(progn ,@forms))))))

;;; Call FUN with N arguments popped from STACK.
(defmacro %call-with-popped-args (fun n stack)
  ;; N's integer value must be known at macroexpansion time.
  (declare (type index n))
  (with-unique-names (n-stack old-length new-length)
    (let ((argtmps (make-gensym-list n)))
      `(let* ((,n-stack ,stack)
              (,old-length (fill-pointer ,n-stack))
              (,new-length (- ,old-length ,n))
              ,@(loop for i from 0 below n collecting
                      `(,(nth i argtmps)
                        (aref ,n-stack (+ ,new-length ,i)))))
        (declare (type (vector t) ,n-stack))
        (setf (fill-pointer ,n-stack) ,new-length)
        ;; (For some applications it might be appropriate to FILL the
        ;; popped area with NIL here, to avoid holding onto garbage. For
        ;; sbcl-0.8.7.something, though, it shouldn't matter, because
        ;; we're using this only to pop stuff off *FOP-STACK*, and the
        ;; entire *FOP-STACK* can be GCed as soon as LOAD returns.)
        (,fun ,@argtmps)))))

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
  ((potential-features :reader invalid-fasl-potential-features
                       :initarg :potential-features)
   (features :reader invalid-fasl-features :initarg :features))
  (:report
   (lambda (condition stream)
     (format stream "~@<incompatible ~S in fasl file ~S: ~2I~_~
                     Of features affecting binary compatibility, ~4I~_~S~2I~_~
                     the fasl has ~4I~_~A,~2I~_~
                     while the runtime expects ~4I~_~A.~:>"
             '*features*
             (invalid-fasl-stream condition)
             (invalid-fasl-potential-features condition)
             (invalid-fasl-features condition)
             (invalid-fasl-expected condition)))))

;;; Skips past the shebang line on stream, if any.
(defun maybe-skip-shebang-line (stream)
  (let ((p (file-position stream)))
    (flet ((next () (read-byte stream nil)))
      (unwind-protect
           (when (and (eq (next) (char-code #\#))
                      (eq (next) (char-code #\!)))
             (setf p nil)
             (loop for x = (next)
                   until (or (not x) (eq x (char-code #\newline)))))
        (when p
          (file-position stream p))))
    t))

;;; Returns T if the stream is a binary input stream with a FASL header.
(defun fasl-header-p (stream &key errorp)
  (let ((p (file-position stream)))
    (unwind-protect
         (let* ((header *fasl-header-string-start-string*)
                (buffer (make-array (length header) :element-type '(unsigned-byte 8)))
                (n 0))
           (flet ((scan ()
                    (maybe-skip-shebang-line stream)
                    (setf n (read-sequence buffer stream))))
             (if errorp
                 (scan)
                 (or (ignore-errors (scan))
                     ;; no a binary input stream
                     (return-from fasl-header-p nil))))
           (if (mismatch buffer header
                         :test #'(lambda (code char) (= code (char-code char))))
               ;; Immediate EOF is valid -- we want to match what
               ;; CHECK-FASL-HEADER does...
               (or (zerop n)
                   (when errorp
                     (error 'fasl-header-missing
                            :stream stream
                            :fhsss buffer
                            :expected header)))
               t))
      (file-position stream p))))

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
  (let ((byte (read-byte stream nil)))
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
               (let* ((length (read-unsigned-byte-32-arg))
                      (result (make-string length)))
                 (read-string-as-bytes stream result)
                 result)))
        ;; Read and validate implementation and version.
        (let ((implementation (keywordicate (string-from-stream)))
              (expected-implementation +backend-fasl-file-implementation+))
          (unless (string= expected-implementation implementation)
            (error 'invalid-fasl-implementation
                   :stream stream
                   :implementation implementation
                   :expected expected-implementation)))
        (let* ((fasl-version (read-word-arg))
               (sbcl-version (if (<= fasl-version 76)
                                 "1.0.11.18"
                                 (string-from-stream)))
               (expected-version (sb!xc:lisp-implementation-version)))
          (unless (string= expected-version sbcl-version)
            (restart-case
                (error 'invalid-fasl-version
                       :stream stream
                       :version sbcl-version
                       :expected expected-version)
              (continue () :report "Load the fasl file anyway"))))
        ;; Read and validate *FEATURES* which affect binary compatibility.
        (let ((faff-in-this-file (string-from-stream)))
          (unless (string= faff-in-this-file *features-affecting-fasl-format*)
            (error 'invalid-fasl-features
                   :stream stream
                   :potential-features *features-potentially-affecting-fasl-format*
                   :expected *features-affecting-fasl-format*
                   :features faff-in-this-file)))
        ;; success
        t))))

;; Setting this variable gives you a trace of fops as they are loaded and
;; executed.
#!+sb-show
(defvar *show-fops-p* nil)

;; buffer for loading symbols
(defvar *fasl-symbol-buffer*)
(declaim (simple-string *fasl-symbol-buffer*))

;;;
;;; a helper function for LOAD-AS-FASL
;;;
;;; Return true if we successfully load a group from the stream, or
;;; NIL if EOF was encountered while trying to read from the stream.
;;; Dispatch to the right function for each fop.
(defun load-fasl-group (stream)
  (when (check-fasl-header stream)
    (catch 'fasl-group-end
      (let ((*current-fop-table-index* 0)
            (*skip-until* nil))
        (declare (special *skip-until*))
        (loop
          (let ((byte (read-byte stream)))
            ;; Do some debugging output.
            #!+sb-show
            (when *show-fops-p*
              (let* ((stack *fop-stack*)
                     (ptr (1- (fill-pointer *fop-stack*))))
                (fresh-line *trace-output*)
                ;; The FOP operations are stack based, so it's sorta
                ;; logical to display the operand before the operator.
                ;; ("reverse Polish notation")
                (unless (= ptr -1)
                  (write-char #\space *trace-output*)
                  (prin1 (aref stack ptr) *trace-output*)
                  (terpri *trace-output*))
                ;; Display the operator.
                (format *trace-output*
                        "~&~S (#X~X at ~D) (~S)~%"
                        (aref *fop-names* byte)
                        byte
                        (1- (file-position stream))
                        (svref *fop-funs* byte))))

            ;; Actually execute the fop.
            (funcall (the function (svref *fop-funs* byte)))))))))

(defun load-as-fasl (stream verbose print)
  ;; KLUDGE: ANSI says it's good to do something with the :PRINT
  ;; argument to LOAD when we're fasloading a file, but currently we
  ;; don't. (CMU CL did, but implemented it in a non-ANSI way, and I
  ;; just disabled that instead of rewriting it.) -- WHN 20000131
  (declare (ignore print))
  (when (zerop (file-length stream))
    (error "attempt to load an empty FASL file:~%  ~S" (namestring stream)))
  (maybe-announce-load stream verbose)
  (with-world-lock ()
    (let* ((*fasl-input-stream* stream)
           (*fasl-symbol-buffer* (make-string 100))
           (*current-fop-table* (or (pop *free-fop-tables*) (make-array 1000)))
           (*current-fop-table-size* (length *current-fop-table*))
           (*fop-stack* (make-array 100 :fill-pointer 0 :adjustable t)))
      (unwind-protect
           (loop while (load-fasl-group stream))
        (push *current-fop-table* *free-fop-tables*)
        ;; NIL out the table, so that we don't hold onto garbage.
        ;;
        ;; FIXME: Could we just get rid of the free fop table pool so
        ;; that this would go away?
        (fill *current-fop-table* nil))))
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
                       (push (cons (svref *fop-names* i) n) ,lvar)
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

