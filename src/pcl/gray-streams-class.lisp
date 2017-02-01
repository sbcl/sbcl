;;;; class definitions for the SBCL Gray streams implementation, based on the
;;;; CMU CL Gray streams implementation, based on the stream-definition-by-user
;;;; proposal by David N. Gray

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is in the public domain and is provided with absolutely no
;;;; warranty. See the COPYING and CREDITS files for more information.

(in-package "SB-GRAY")

(defclass fundamental-stream (standard-object stream)
  ((open-p :initform t :accessor stream-open-p))
  (:documentation "Base class for all Gray streams."))

;;; Define the stream classes.
(defclass fundamental-input-stream (fundamental-stream) nil
  (:documentation "Superclass of all Gray input streams."))

(defclass fundamental-output-stream (fundamental-stream) nil
  (:documentation "Superclass of all Gray output streams."))

(defclass fundamental-character-stream (fundamental-stream) nil
  (:documentation
   "Superclass of all Gray streams whose element-type is a subtype of character."))

(defclass fundamental-binary-stream (fundamental-stream) nil
  (:documentation "Superclass of all Gray streams whose element-type
is a subtype of unsigned-byte or signed-byte."))

(defclass fundamental-character-input-stream
    (fundamental-input-stream fundamental-character-stream) nil
  (:documentation "Superclass of all Gray input streams whose element-type
is a subtype of character."))

(defclass fundamental-character-output-stream
    (fundamental-output-stream fundamental-character-stream) nil
  (:documentation "Superclass of all Gray output streams whose element-type
is a subtype of character."))

(defclass fundamental-binary-input-stream
    (fundamental-input-stream fundamental-binary-stream) nil
  (:documentation "Superclass of all Gray input streams whose element-type
is a subtype of unsigned-byte or signed-byte."))

(defclass fundamental-binary-output-stream
    (fundamental-output-stream fundamental-binary-stream) nil
  (:documentation "Superclass of all Gray output streams whose element-type
is a subtype of unsigned-byte or signed-byte."))

;;; This is not in the Gray stream proposal, so it is left here
;;; as example code.
;;;
;;; example character input and output streams
#|
(defclass character-output-stream (fundamental-character-output-stream)
  ((lisp-stream :initarg :lisp-stream
                :accessor character-output-stream-lisp-stream)))

(defclass character-input-stream (fundamental-character-input-stream)
  ((lisp-stream :initarg :lisp-stream
                :accessor character-input-stream-lisp-stream)))
|#
