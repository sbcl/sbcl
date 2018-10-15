;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!FORMAT")

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


(defstruct (format-directive (:copier nil))
  (string (missing-arg) :type simple-string :read-only t)
  (start (missing-arg) :type (and unsigned-byte fixnum) :read-only t)
  (end (missing-arg) :type (and unsigned-byte fixnum) :read-only t)
  ;; CHARACTER, COLONP, ATSIGNP could be packed into one integer
  (character (missing-arg) :type character :read-only t)
  ;; for early binding to the function in "~/pkg:fun/" directives
  (function nil :type symbol :read-only t)
  (colonp nil :type (member t nil) :read-only t)
  (atsignp nil :type (member t nil) :read-only t)
  (params nil :type list :read-only t))
(declaim (freeze-type format-directive))

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
