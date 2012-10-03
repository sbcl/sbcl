;;;; This file contains some parameterizations of various VM
;;;; attributes for the ARM.  This file is separate from other stuff so
;;;; that it can be compiled and loaded earlier.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;; number of bits per word where a word holds one lisp descriptor
(def!constant n-word-bits 32)

;;; the natural width of a machine word (as seen in e.g. register width,
;;; address space)
(def!constant n-machine-word-bits 32)

;;; number of bits per byte where a byte is the smallest addressable
;;; object
(def!constant n-byte-bits 8)


;;;; Where to put the different spaces.

;;; On non-gencgc we need large dynamic and static spaces for PURIFY
#!-gencgc
(progn
  (def!constant read-only-space-start #x04000000)
  (def!constant read-only-space-end   #x07ff8000)
  (def!constant static-space-start    #x08000000)
  (def!constant static-space-end      #x097fff00)

  (def!constant linkage-table-space-start #x0a000000)
  (def!constant linkage-table-space-end   #x0b000000))

(def!constant linkage-table-entry-size 16)

#!+linux
(progn
  #!-gencgc
  (progn
    (def!constant dynamic-0-space-start #x4f000000)
    (def!constant dynamic-0-space-end   #x66fff000)
    (def!constant dynamic-1-space-start #x67000000)
    (def!constant dynamic-1-space-end   #x7efff000)))


;;;; Static symbols.


;;; These symbols are loaded into static space directly after NIL so
;;; that the system can compute their address by adding a constant
;;; amount to NIL.
;;;
;;; The fdefn objects for the static functions are loaded into static
;;; space directly after the static symbols.  That way, the raw-addr
;;; can be loaded directly out of them by indirecting relative to NIL.
;;;
(defparameter *static-symbols*
  (append
   *common-static-symbols*
   *c-callable-static-symbols*))

(defparameter *static-funs*
  '())


;;;; Assembler parameters:

;;; The number of bits per element in the assemblers code vector.
;;;
(defparameter *assembly-unit-length* 8)
