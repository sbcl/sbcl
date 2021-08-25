;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-FORMAT")

(defstruct (format-directive (:copier nil)
                             (:constructor %make-directive
                                           (string start end params bits function))
                             (:conc-name directive-))
  (string (missing-arg) :type simple-string :read-only t)
  (start (missing-arg) :type (and unsigned-byte fixnum) :read-only t)
  (end (missing-arg) :type (and unsigned-byte fixnum) :read-only t)
  (bits nil :type (unsigned-byte 9) :read-only t) ; colon, atsign, char
  ;; for early binding to the function in "~/pkg:fun/" directives
  (function nil :type symbol :read-only t)
  (params nil :type list :read-only t))
(declaim (freeze-type format-directive))

(defun make-format-directive (string start end params colon atsign char symbol)
  (let ((code (char-code char)))
    (%make-directive string start end params
                     (logior (if colon  #x100 0)
                             (if atsign #x080 0)
                             (if (< code 128) code 0))
                     symbol)))

(defglobal *format-directive-expanders* (make-array 128 :initial-element nil))
(declaim (type (simple-vector 128)
               *format-directive-expanders*))

(defun %print-format-error (condition stream)
  (format stream
          "~:[~*~;error in ~S: ~]~?~@[~%  ~A~%  ~V@T^~@[~V@T^~]~]"
          (format-error-print-banner condition)
          'format
          (format-error-complaint condition)
          (format-error-args condition)
          (format-error-control-string condition)
          (format-error-offset condition)
          (format-error-second-relative condition)))

(defvar *default-format-error-control-string* nil)
(defvar *default-format-error-offset* nil)

(define-condition format-error (error reference-condition)
  ((complaint :reader format-error-complaint :initarg :complaint)
   (args :reader format-error-args :initarg :args :initform nil)
   (control-string :reader format-error-control-string
                   :initarg :control-string
                   :initform *default-format-error-control-string*)
   (offset :reader format-error-offset :initarg :offset
           :initform *default-format-error-offset*)
   (second-relative :reader format-error-second-relative
                    :initarg :second-relative :initform nil)
   (print-banner :reader format-error-print-banner :initarg :print-banner
                 :initform t))
  (:report %print-format-error)
  (:default-initargs :references nil))

(defun format-error* (complaint args &rest initargs &key &allow-other-keys)
  (apply #'error 'format-error :complaint complaint :args args initargs))

(defun format-error (complaint &rest args)
  (format-error* complaint args))

(defun format-error-at* (control-string offset complaint args
                         &rest initargs &key &allow-other-keys)
  (apply #'error 'format-error
         :complaint complaint :args args
         :control-string (or control-string *default-format-error-control-string*)
         :offset (or offset *default-format-error-offset*)
         initargs))

(defun format-error-at (control-string offset complaint &rest args)
  (format-error-at* control-string offset complaint args))



(declaim (inline directive-colonp directive-atsignp))
(defun directive-colonp (x) (logbitp 8 (directive-bits x)))
(defun directive-atsignp (x) (logbitp 7 (directive-bits x)))
(declaim (inline directive-code directive-character))
(defun directive-code (x) (logand (directive-bits x) #x7F))
(defun directive-character (x) (code-char (directive-code x)))
;;; This works even if directive char is invalid, where -CHARACTER
;;; would return (code-char 0)
(defun directive-char-name (x)
  (let ((byte (directive-code x)))
    (char-name (if (eql byte 0)
                   ;; extract the character from the string
                   (char (directive-string x) (1- (directive-end x)))
                   (code-char byte)))))

(defun check-modifier (modifier-name value)
  (when value
    (let ((modifiers (ensure-list modifier-name)))
      (format-error "The ~{~A~^ and the ~} modifier~P cannot be used ~
                     ~:*~[~;~;simultaneously ~]with this directive."
                    modifiers (length modifiers)))))

;;; FMT-CONTROL is a structure with a nonstandard metaclass.
;;; The compile-time representation of that object is an ordinary defstruct
;;; which of course works on any cross-compiler host.
(def!struct (fmt-control-proxy (:constructor make-fmt-control-proxy
                                   (string symbols)))
  string symbols)
(!set-load-form-method fmt-control-proxy (:xc :target)
  (lambda (self env)
    (declare (ignore env))
    `(make-fmt-control ,(fmt-control-proxy-string self)
                       ',(fmt-control-proxy-symbols self))))
