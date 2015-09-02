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

(defstruct format-directive
  (string (missing-arg) :type simple-string)
  (start (missing-arg) :type (and unsigned-byte fixnum))
  (end (missing-arg) :type (and unsigned-byte fixnum))
  (character (missing-arg) :type character)
  (colonp nil :type (member t nil))
  (atsignp nil :type (member t nil))
  (params nil :type list))
(defmethod print-object ((x format-directive) stream)
  (print-unreadable-object (x stream)
    (write-string (format-directive-string x)
                  stream
                  :start (format-directive-start x)
                  :end (format-directive-end x))))
