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

(in-package "SB!KERNEL")

(eval-when (:compile-toplevel :execute)

(defun compute-one-setter (name type)
  (let* ((args (second type))
         (res (type-specifier
               (single-value-type
                (values-specifier-type (third type)))))
         (arglist (make-gensym-list (1+ (length args)))))
    (cond
     ((null (intersection args sb!xc:lambda-list-keywords))
      `(defun (setf ,name) ,arglist
         (declare ,@(mapcar (lambda (arg type)
                              `(type ,type ,arg))
                            arglist
                            (cons res args)))
         (setf (,name ,@(rest arglist)) ,(first arglist))))
     (t
      (warn "hairy SETF expander for function ~S" name)
      nil))))

;;; FIXME: should probably become MACROLET
;;; [But can't until we fix the "lexical environment too hairy" warning.
;;;  And this environment isn't too hairy so it's especially annoying]
(sb!xc:defmacro define-setters (packages &rest ignore)
  (collect ((res))
    (dolist (pkg packages)
      (do-external-symbols (sym pkg)
        (when (and (fboundp sym)
                   (eq (info :function :kind sym) :function)
                   (or (info :setf :inverse sym)
                       (info :setf :expander sym))
                   ;; Use STRING= because (NEQ 'LDB 'SB!XC:LDB) etc.
                   (not (member sym ignore :test #'string=)))
          (res sym))))
    `(progn
      ,@(mapcan
         (lambda (sym)
           (let ((type (type-specifier (proclaimed-ftype sym))))
             (aver (consp type))
             (list
              #!-sb-fluid `(declaim (inline (setf ,sym)))
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
