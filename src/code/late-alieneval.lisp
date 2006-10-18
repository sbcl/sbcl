;;;; This file contains parts of the ALIEN implementation that
;;;; are not part of the compiler, but depend on it.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!ALIEN")

(defmacro make-alien (type &optional size &environment env)
  #!+sb-doc
  "Allocate an alien of type TYPE and return an alien pointer to it. If SIZE
is supplied, how it is interpreted depends on TYPE. If TYPE is an array type,
SIZE is used as the first dimension for the allocated array. If TYPE is not an
array, then SIZE is the number of elements to allocate. The memory is
allocated using ``malloc'', so it can be passed to foreign functions which use
``free''."
  (let ((alien-type (if (alien-type-p type)
                        type
                        (parse-alien-type type env))))
    (multiple-value-bind (size-expr element-type)
        (if (alien-array-type-p alien-type)
            (let ((dims (alien-array-type-dimensions alien-type)))
              (cond
                (size
                 (unless dims
                   (error
                    "cannot override the size of zero-dimensional arrays"))
                 (when (constantp size)
                   (setf alien-type (copy-alien-array-type alien-type))
                   (setf (alien-array-type-dimensions alien-type)
                         (cons (constant-form-value size) (cdr dims)))))
                (dims
                 (setf size (car dims)))
                (t
                 (setf size 1)))
              (values `(* ,size ,@(cdr dims))
                      (alien-array-type-element-type alien-type)))
            (values (or size 1) alien-type))
      (let ((bits (alien-type-bits element-type))
            (alignment (alien-type-alignment element-type)))
        (unless bits
          (error "The size of ~S is unknown."
                 (unparse-alien-type element-type)))
        (unless alignment
          (error "The alignment of ~S is unknown."
                 (unparse-alien-type element-type)))
        (let ((alloc-form `(%sap-alien (%make-alien (* ,(align-offset bits alignment)
                                                       ,size-expr))
                                       ',(make-alien-pointer-type :to alien-type))))
          (if (sb!c:policy env (> speed 1))
              alloc-form
              `(locally (declare (muffle-conditions compiler-note))
                 ,alloc-form)))))))
