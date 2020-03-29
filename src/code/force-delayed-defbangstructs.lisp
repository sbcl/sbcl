;;;; Once all the cross-compiler DEFSTRUCT machinery has been set up,
;;;; we can feed the stored DEF!STRUCT argument lists to it. (This
;;;; goes in its own source file, instead of in the same file as the
;;;; DEFSTRUCT machinery, because it's tidier and more maintainable
;;;; than adding EVAL-WHEN :COMPILE wrappers to anything that it might
;;;; need.)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

(macrolet ((do-delayed-structs ()
             `(progn
                ,@(mapcan (lambda (x)
                            (list `(in-package ,(delayed-defstruct-package x))
                                  `(sb-xc:defstruct ,@(delayed-defstruct-args x))))
                          (reverse *delayed-defstructs*)))))
  (do-delayed-structs)
  (makunbound '*delayed-defstructs*))
