;;;; ALIEN-related type system stuff, done later
;;;; than other type system stuff because it depends on the definition
;;;; of the ALIEN-VALUE target structure type

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

(sb-xc:defstruct (alien-value (:copier nil) (:constructor %sap-alien (sap type)))
  (sap nil :type sb-sys:system-area-pointer)
  (type nil :type sb-alien::alien-type))
(proclaim '(freeze-type alien-value))

;;; KLUDGE: This !DEFINE-SUPERCLASSES gets executed much later than the
;;; others (toplevel form time instead of cold load init time) because
;;; ALIEN-VALUE itself is a structure which isn't defined until fairly
;;; late.
(!define-superclasses alien ((alien-value)) progn)
