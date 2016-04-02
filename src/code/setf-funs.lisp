;;;; stuff to automatically generate SETF functions for all the standard
;;;; functions that are currently implemented with SETF macros

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

;;; Fix unknown types in globaldb
(let ((l nil))
  (do-all-symbols (s)
    (when (and (fboundp s) (not (macro-function s)))
      (let ((ftype (info :function :type s)))
        (when (contains-unknown-type-p ftype)
          (setf (info :function :type s)
                (specifier-type (type-specifier ftype)))
          (push s l)))))
  (let ((*print-pretty* nil)
        (*print-length* nil))
    (format t "~&; Fixed ftypes: ~S~%" (sort l #'string<))))

(eval-when (:compile-toplevel :execute)

(defun compute-one-setter (name type)
  (let ((args (second type)))
    (cond
     ((null (intersection args lambda-list-keywords))
      (let ((res (type-specifier
                  (single-value-type
                   (values-specifier-type (third type)))))
            (arglist (cons 'newval (sb-kernel:%fun-lambda-list
                                    (symbol-function name)))))
        `(locally
          (declare (muffle-conditions
                    ;; Expect SETF macro + function warnings.
                    (and style-warning
                         ;; Expect none of these,
                         ;; but just to make sure, show them.
                         (not sb-c:inlining-dependency-failure))))
          (defun (setf ,name) ,arglist
            (declare ,@(mapcar (lambda (arg type) `(type ,type ,arg))
                               arglist (cons res args)))
            (setf (,name ,@(rest arglist)) ,(first arglist))))))
     (t
      (warn "hairy SETF expander for function ~S" name)
      nil))))

;;; FIXME: should probably become MACROLET, but inline functions
;;; within a macrolet capture the whole macrolet, which is dumb.
(defmacro define-setters (packages &rest ignore)
  (collect ((res))
    (dolist (pkg packages)
      (do-external-symbols (sym pkg)
        (when (and (fboundp sym)
                   (eq (info :function :kind sym) :function)
                   (info :setf :expander sym)
                   (not (memq sym ignore)))
          (res sym))))
    `(progn
      ,@(mapcan
         (lambda (sym)
           (let ((type (type-specifier (proclaimed-ftype sym))))
             (aver (consp type))
             (list
              #-sb-fluid `(declaim (inline (setf ,sym)))
              (compute-one-setter sym type))))
         (sort (res) #'string<)))))

) ; EVAL-WHEN

(define-setters ("COMMON-LISP")
  ;; Semantically silly...
  getf apply ldb mask-field logbitp values
  ;; Hairy lambda list
  get subseq
  ;; Have explicit redundant definitions...
  bit sbit aref gethash)
