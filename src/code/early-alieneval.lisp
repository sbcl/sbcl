;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!ALIEN")

(defvar *alien-type-classes* (make-hash-table :test 'eq))

(defvar *new-auxiliary-types* nil)

;;; the list of record types that have already been unparsed. This is
;;; used to keep from outputting the slots again if the same structure
;;; shows up twice.
(defvar *record-types-already-unparsed*)

;;; not documented in CMU CL:-(
;;;
;;; reverse engineering observations:
;;;   * seems to be set when translating return values
;;;   * seems to enable the translation of (VALUES), which is the
;;;     Lisp idiom for C's return type "void" (which is likely
;;;     why it's set when when translating return values)
(defvar *values-type-okay* nil)

(defvar *default-c-string-external-format* nil)

;;; Frame pointer, program counter conses. In each thread it's bound
;;; locally or not bound at all.
(defvar *saved-fp-and-pcs*)

#!+:c-stack-is-control-stack
(declaim (inline invoke-with-saved-fp-and-pc))
#!+:c-stack-is-control-stack
(defun invoke-with-saved-fp-and-pc (fn)
  (declare #-sb-xc-host (muffle-conditions compiler-note)
           (optimize (speed 3)))
  (let* ((fp-and-pc (cons (sb!kernel:%caller-frame)
                          (sap-int (sb!kernel:%caller-pc)))))
    (declare (truly-dynamic-extent fp-and-pc))
    (let ((*saved-fp-and-pcs* (if (boundp '*saved-fp-and-pcs*)
                                  (cons fp-and-pc *saved-fp-and-pcs*)
                                  (list fp-and-pc))))
      (declare (truly-dynamic-extent *saved-fp-and-pcs*))
      (funcall fn))))

(defun find-saved-fp-and-pc (fp)
  (when (boundp '*saved-fp-and-pcs*)
    (dolist (x *saved-fp-and-pcs*)
      (when (#!+:stack-grows-downward-not-upward
             sap>
             #!-:stack-grows-downward-not-upward
             sap<
             (int-sap (sb!kernel:get-lisp-obj-address (car x))) fp)
        (return (values (car x) (cdr x)))))))

