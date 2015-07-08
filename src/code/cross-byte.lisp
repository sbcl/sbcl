;;;; cross-compile-time-only replacements for byte-specifier
;;;; machinery.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!INT")

;; Inlining these allows type inference to work.
(declaim (inline sb!xc:dpb sb!xc:ldb sb!xc:mask-field))

(defun sb!xc:byte (size position)
  (cons size position))

(defun sb!xc:byte-size (cross-byte)
  (car cross-byte))

(defun sb!xc:byte-position (cross-byte)
  (cdr cross-byte))

(defun uncross-byte (cross-byte)
  (cl:byte (sb!xc:byte-size cross-byte) (sb!xc:byte-position cross-byte)))

(defun sb!xc:ldb (cross-byte int)
  (cl:ldb (uncross-byte cross-byte) int))

(defun sb!xc:ldb-test (cross-byte int)
  (cl:ldb-test (uncross-byte cross-byte) int))

(defun sb!xc:dpb (new cross-byte int)
  (cl:dpb new (uncross-byte cross-byte) int))

(defun sb!xc:mask-field (cross-byte int)
  (cl:mask-field (uncross-byte cross-byte) int))

(defun sb!xc:deposit-field (new cross-byte int)
  (cl:deposit-field new (uncross-byte cross-byte) int))

(declaim (ftype function bug))
(define-setf-expander sb!xc:ldb (cross-byte int &environment env)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-expansion int env)
    (when (cdr stores)
      (bug "SETF SB!XC:LDB too hairy!"))
    (let ((btemp (gensym))
          (store (gensym)))
      (values (cons btemp temps)
              (cons cross-byte vals)
              (list store)
              `(let ((,(car stores) (cl:dpb ,store (uncross-byte ,btemp) ,access-form)))
                ,store-form
                ,store)
              `(cl:ldb (uncross-byte ,btemp) ,access-form)))))

(define-setf-expander sb!xc:mask-field (cross-byte int &environment env)
    (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-expansion int env)
    (when (cdr stores)
      (bug "SETF SB!XC:MASK-FIELD too hairy!"))
    (let ((btemp (gensym))
          (store (gensym)))
      (values (cons btemp temps)
              (cons cross-byte vals)
              (list store)
              `(let ((,(car stores) (cl:deposit-field ,store (uncross-byte ,btemp) ,access-form)))
                ,store-form
                ,store)
              `(cl:mask-field (uncross-byte ,btemp) ,access-form)))))
