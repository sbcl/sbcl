;;;; cross-compile-time-only replacements for BOOLE machinery.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!INT")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant sb!xc:boole-clr 0)
  (defconstant sb!xc:boole-set 1)
  (defconstant sb!xc:boole-1   2)
  (defconstant sb!xc:boole-2   3)
  (defconstant sb!xc:boole-c1  4)
  (defconstant sb!xc:boole-c2  5)
  (defconstant sb!xc:boole-and 6)
  (defconstant sb!xc:boole-ior 7)
  (defconstant sb!xc:boole-xor 8)
  (defconstant sb!xc:boole-eqv 9)
  (defconstant sb!xc:boole-nand  10)
  (defconstant sb!xc:boole-nor   11)
  (defconstant sb!xc:boole-andc1 12)
  (defconstant sb!xc:boole-andc2 13)
  (defconstant sb!xc:boole-orc1  14)
  (defconstant sb!xc:boole-orc2  15))

(defun sb!xc:boole (boole num1 num2)
  (cl:boole (uncross-boole boole) num1 num2))

(defun uncross-boole (boole)
  (case boole
    (#.sb!xc:boole-clr cl:boole-clr)
    (#.sb!xc:boole-set cl:boole-set)
    (#.sb!xc:boole-1 cl:boole-1)
    (#.sb!xc:boole-2 cl:boole-2)
    (#.sb!xc:boole-c1 cl:boole-c1)
    (#.sb!xc:boole-c2 cl:boole-c2)
    (#.sb!xc:boole-and cl:boole-and)
    (#.sb!xc:boole-ior cl:boole-ior)
    (#.sb!xc:boole-xor cl:boole-xor)
    (#.sb!xc:boole-eqv cl:boole-eqv)
    (#.sb!xc:boole-nand cl:boole-nand)
    (#.sb!xc:boole-nor cl:boole-nor)
    (#.sb!xc:boole-andc1 cl:boole-andc1)
    (#.sb!xc:boole-andc2 cl:boole-andc2)
    (#.sb!xc:boole-orc1 cl:boole-orc1)
    (#.sb!xc:boole-orc2 cl:boole-orc2)))
