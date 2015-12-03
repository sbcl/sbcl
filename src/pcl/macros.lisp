;;;; macros, global variable definitions, and other miscellaneous support stuff
;;;; used by the rest of the PCL subsystem

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is derived from software originally released by Xerox
;;;; Corporation. Copyright and release statements follow. Later modifications
;;;; to the software are in the public domain and are provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for more
;;;; information.

;;;; copyright information from original PCL sources:
;;;;
;;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;;; All rights reserved.
;;;;
;;;; Use and copying of this software and preparation of derivative works based
;;;; upon this software are permitted. Any distribution of this software or
;;;; derivative works must comply with all applicable United States export
;;;; control laws.
;;;;
;;;; This software is made available AS IS, and Xerox Corporation makes no
;;;; warranty about the software, its performance or its conformity to any
;;;; specification.

(in-package "SB-PCL")

(/show "starting pcl/macros.lisp")

(declaim (declaration
          ;; These nonstandard declarations seem to be used privately
          ;; within PCL itself to pass information around, so we can't
          ;; just delete them.
          %class
          ;; This declaration may also be used within PCL to pass
          ;; information around, I'm not sure. -- WHN 2000-12-30
          %variable-rebinding))

(/show "done with DECLAIM DECLARATION")

(defun get-declaration (name declarations &optional default)
  (dolist (d declarations default)
    (dolist (form (cdr d))
      (when (and (consp form) (eq (car form) name))
        (return-from get-declaration (cdr form))))))

(/show "pcl/macros.lisp 85")

(/show "pcl/macros.lisp 101")

(defmacro dolist-carefully ((var list improper-list-handler) &body body)
  `(let ((,var nil)
         (.dolist-carefully. ,list))
     (loop (when (null .dolist-carefully.) (return nil))
           (if (consp .dolist-carefully.)
               (progn
                 (setq ,var (pop .dolist-carefully.))
                 ,@body)
               (,improper-list-handler)))))

;;;; FIND-CLASS
;;;;
;;;; This is documented in the CLOS specification.

(/show "pcl/macros.lisp 119")

(declaim (inline legal-class-name-p))
(defun legal-class-name-p (x)
  (symbolp x))

(defvar *create-classes-from-internal-structure-definitions-p* t)

(declaim (ftype function ensure-non-standard-class))
(defun find-class-from-cell (symbol cell &optional (errorp t))
  (or (when cell
        (or (classoid-cell-pcl-class cell)
            (when *create-classes-from-internal-structure-definitions-p*
              (let ((classoid (classoid-cell-classoid cell)))
                (when (and classoid
                           (or (condition-classoid-p classoid)
                               (defstruct-classoid-p classoid)))
                  (ensure-non-standard-class symbol classoid))))))
      (cond ((null errorp) nil)
            ((legal-class-name-p symbol)
             (error "There is no class named ~
                     ~/sb-impl::print-symbol-with-prefix/." symbol))
            (t
             (error "~S is not a legal class name." symbol)))))

(defun find-class (symbol &optional (errorp t) environment)
  (declare (ignore environment) (explicit-check))
  (find-class-from-cell symbol
                        (find-classoid-cell symbol)
                        errorp))


(/show "pcl/macros.lisp 187")

(define-compiler-macro find-class (&whole form
                                   symbol &optional (errorp t) environment)
  (declare (ignore environment))
  (if (and (constantp symbol)
           (legal-class-name-p (setf symbol (constant-form-value symbol)))
           (constantp errorp)
           (member **boot-state** '(braid complete)))
      (let ((errorp (not (null (constant-form-value errorp))))
            (cell (make-symbol "CLASSOID-CELL")))
        `(let ((,cell (load-time-value (find-classoid-cell ',symbol :create t))))
           (or (classoid-cell-pcl-class ,cell)
               ,(if errorp
                    `(find-class-from-cell ',symbol ,cell t)
                    `(when (classoid-cell-classoid ,cell)
                       (find-class-from-cell ',symbol ,cell nil))))))
      form))

(declaim (ftype function update-ctors))
(defun (setf find-class) (new-value name &optional errorp environment)
  (declare (ignore errorp environment))
  (cond ((legal-class-name-p name)
         (with-single-package-locked-error
             (:symbol name "Using ~A as the class-name argument in ~
                           (SETF FIND-CLASS)"))
         (with-world-lock ()
           (let ((cell (find-classoid-cell name :create new-value)))
             (cond (new-value
                    (setf (classoid-cell-pcl-class cell) new-value)
                    (when (eq **boot-state** 'complete)
                      (let ((classoid (class-classoid new-value)))
                        (setf (find-classoid name) classoid))))
                   (cell
                    (%clear-classoid name cell)))
             (when (or (eq **boot-state** 'complete)
                       (eq **boot-state** 'braid))
               (update-ctors 'setf-find-class :class new-value :name name))
             new-value)))
        (t
         (error "~S is not a legal class name." name))))

(/show "pcl/macros.lisp 241")

(defmacro function-funcall (form &rest args)
  `(funcall (the function ,form) ,@args))

(defmacro function-apply (form &rest args)
  `(apply (the function ,form) ,@args))

(/show "pcl/macros.lisp 249")

(defun get-setf-fun-name (name)
  `(setf ,name))

;;;; support for useful hashing of PCL instances

;; FIXME: do something address-based, so not to depend on the ordinary PRNG.
;; Probably mix the object address with the output of a fast and simple LCG,
;; as is done for CTYPEs.
(defvar *instance-hash-code-random-state* (make-random-state))
(defun get-instance-hash-code ()
  ;; ANSI SXHASH wants us to make a good-faith effort to produce
  ;; hash-codes that are well distributed within the range of
  ;; non-negative fixnums, and this RANDOM operation does that, unlike
  ;; the sbcl<=0.8.16 implementation of this operation as
  ;; (INCF COUNTER).
  ;;
  ;; Hopefully there was no virtue to the old counter implementation
  ;; that I am insufficiently insightful to insee. -- WHN 2004-10-28
  (random most-positive-fixnum
          *instance-hash-code-random-state*))


(/show "finished with pcl/macros.lisp")
