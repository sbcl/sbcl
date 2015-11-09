;;;; SBCL-specific parts of the condition system, i.e. parts which
;;;; don't duplicate/clobber functionality already provided by the
;;;; cross-compilation host Common Lisp

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;; a utility for SIGNAL, ERROR, CERROR, WARN, COMPILER-NOTIFY and
;;; INVOKE-DEBUGGER: Parse the hairy argument conventions into a
;;; single argument that's directly usable by all the other routines.
(defun coerce-to-condition (datum arguments default-type fun-name)
  (declare (explicit-check))
  (cond ((typep datum 'condition)
         (when (and arguments (not (eq fun-name 'cerror)))
           (cerror "Ignore the additional arguments."
                   'simple-type-error
                   :datum arguments
                   :expected-type 'null
                   :format-control "You may not supply additional arguments ~
                                    when giving ~S to ~S."
                   :format-arguments (list datum fun-name)))
         datum)
        ((or (stringp datum) (functionp datum))
         (make-condition default-type
                         :format-control datum
                         :format-arguments arguments))
        (t
         (apply #'make-condition datum arguments))))

(define-condition layout-invalid (type-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream
             "~@<invalid structure layout: ~
              ~2I~_A test for class ~4I~_~S ~
              ~2I~_was passed the obsolete instance ~4I~_~S~:>"
             (classoid-proper-name (type-error-expected-type condition))
             (type-error-datum condition)))))

(define-condition case-failure (type-error)
  ((name :reader case-failure-name :initarg :name)
   (possibilities :reader case-failure-possibilities :initarg :possibilities))
  (:report
   (lambda (condition stream)
     (let ((*print-escape* t))
       (format stream "~@<~S fell through ~S expression.~@[ ~
                      ~:_Wanted one of (~/pprint-fill/).~]~:>"
               (type-error-datum condition)
               (case-failure-name condition)
               (case-failure-possibilities condition))))))

(define-condition compiled-program-error (program-error)
  ((message :initarg :message :reader program-error-message)
   (source :initarg :source :reader program-error-source))
  (:report (lambda (condition stream)
             (format stream "Execution of a form compiled with errors.~%~
                             Form:~%  ~A~%~
                             Compile-time error:~%  ~A"
                       (program-error-source condition)
                       (program-error-message condition)))))

(define-condition interpreted-program-error
    (program-error encapsulated-condition)
  ;; Unlike COMPILED-PROGRAM-ERROR, we don't need to dump these, so
  ;; storing the original condition and form is OK.
  ((form :initarg :form :reader program-error-form))
  (:report (lambda (condition stream)
             (format stream "~&Evaluation of~%  ~S~%~
                             caused error:~%  ~A~%"
                     (program-error-form condition)
                     (encapsulated-condition condition)))))

(define-condition simple-control-error (simple-condition control-error) ())
(define-condition simple-file-error    (simple-condition file-error)    ())
(define-condition simple-program-error (simple-condition program-error) ())
(define-condition simple-stream-error  (simple-condition stream-error)  ())
(define-condition simple-parse-error   (simple-condition parse-error)   ())

(define-condition character-coding-error (error)
  ((external-format :initarg :external-format :reader character-coding-error-external-format)))
(define-condition character-encoding-error (character-coding-error)
  ((code :initarg :code :reader character-encoding-error-code)))
(define-condition character-decoding-error (character-coding-error)
  ((octets :initarg :octets :reader character-decoding-error-octets)))
(define-condition stream-encoding-error (stream-error character-encoding-error)
  ()
  (:report
   (lambda (c s)
     (let ((stream (stream-error-stream c))
           (code (character-encoding-error-code c)))
       (format s "~@<~S stream encoding error on ~S: ~2I~_~
                  the character with code ~D cannot be encoded.~@:>"
               (character-coding-error-external-format c)
               stream
               code)))))
(define-condition stream-decoding-error (stream-error character-decoding-error)
  ()
  (:report
   (lambda (c s)
     (let ((stream (stream-error-stream c))
           (octets (character-decoding-error-octets c)))
       (format s "~@<~S stream decoding error on ~S: ~2I~_~
                  the octet sequence ~S cannot be decoded.~@:>"
               (character-coding-error-external-format c)
               stream
               octets)))))

(define-condition c-string-encoding-error (character-encoding-error)
  ()
  (:report
   (lambda (c s)
     (format s "~@<~S c-string encoding error: ~2I~_~
                  the character with code ~D cannot be encoded.~@:>"
               (character-coding-error-external-format c)
               (character-encoding-error-code c)))))

(define-condition c-string-decoding-error (character-decoding-error)
  ()
  (:report
   (lambda (c s)
     (format s "~@<~S c-string decoding error: ~2I~_~
                  the octet sequence ~S cannot be decoded.~@:>"
             (character-coding-error-external-format c)
             (character-decoding-error-octets c)))))

(define-condition control-stack-exhausted (storage-condition)
  ()
  (:report
    (lambda (condition stream)
      (declare (ignore condition))
      (format stream
              ;; no pretty-printing, because that would use a lot of stack.
              "Control stack exhausted (no more space for function call frames).
This is probably due to heavily nested or infinitely recursive function
calls, or a tail call that SBCL cannot or has not optimized away.

PROCEED WITH CAUTION."))))

(define-condition binding-stack-exhausted (storage-condition)
  ()
  (:report
    (lambda (condition stream)
      (declare (ignore condition))
      (format stream
              ;; no pretty-printing, because that would use a lot of stack.
              "Binding stack exhausted.

PROCEED WITH CAUTION."))))

(define-condition alien-stack-exhausted (storage-condition)
  ()
  (:report
    (lambda (condition stream)
      (declare (ignore condition))
      (format stream
              ;; no pretty-printing, because that would use a lot of stack.
              "Alien stack exhausted.

PROCEED WITH CAUTION."))))

(define-condition heap-exhausted-error (storage-condition)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (declare (special *heap-exhausted-error-available-bytes*
                       *heap-exhausted-error-requested-bytes*))
     ;; See comments in interr.lisp -- there is a method to this madness.
     (if (and (boundp '*heap-exhausted-error-available-bytes*)
              (boundp '*heap-exhausted-error-requested-bytes*))
         (format stream
                 ;; no pretty-printing, because that will use a lot of heap.
                 "Heap exhausted (no more space for allocation).
~D bytes available, ~D requested.

PROCEED WITH CAUTION."
                 *heap-exhausted-error-available-bytes*
                 *heap-exhausted-error-requested-bytes*)
         (format stream
                 "A ~S condition without bindings for heap statistics.  (If
you did not expect to see this message, please report it."
                 'heap-exhausted-error)))))

(define-condition system-condition (condition)
  ((address :initarg :address :reader system-condition-address :initform nil)
   (context :initarg :context :reader system-condition-context :initform nil)))

(define-condition memory-fault-error (system-condition error) ()
  (:report
   (lambda (condition stream)
     (format stream "Unhandled memory fault at #x~X."
             (system-condition-address condition)))))

(define-condition breakpoint-error (system-condition error) ()
  (:report
   (lambda (condition stream)
     (format stream "Unhandled breakpoint/trap at #x~X."
             (system-condition-address condition)))))

(define-condition interactive-interrupt (system-condition serious-condition) ()
  (:report
   (lambda (condition stream)
     (format stream "Interactive interrupt at #x~X."
             (system-condition-address condition)))))
