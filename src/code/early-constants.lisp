;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

(defconstant-eqx lambda-list-keywords
    '(&allow-other-keys
      &aux
      &body
      &environment
      &key
      &more
      &optional
      &rest
      &whole)
  #'equal
  "A list of symbols used as lambda list keywords in SBCL.")

;;; The BOOLE function dispaches to any logic operation depending on
;;; the value of an argument. Presently, legal selector values are [0..15].
;;; BOOLE is open coded for calls with any of the constants declared below.

(defconstant boole-clr 0
  "Boole function op, makes BOOLE return 0.")

(defconstant boole-set 1
  "Boole function op, makes BOOLE return -1.")

(defconstant boole-1   2
  "Boole function op, makes BOOLE return integer1.")

(defconstant boole-2   3
  "Boole function op, makes BOOLE return integer2.")

(defconstant boole-c1  4
  "Boole function op, makes BOOLE return complement of integer1.")

(defconstant boole-c2  5
  "Boole function op, makes BOOLE return complement of integer2.")

(defconstant boole-and 6
  "Boole function op, makes BOOLE return logand of integer1 and integer2.")

(defconstant boole-ior 7
  "Boole function op, makes BOOLE return logior of integer1 and integer2.")

(defconstant boole-xor 8
  "Boole function op, makes BOOLE return logxor of integer1 and integer2.")

(defconstant boole-eqv 9
  "Boole function op, makes BOOLE return logeqv of integer1 and integer2.")

(defconstant boole-nand  10
  "Boole function op, makes BOOLE return log nand of integer1 and integer2.")

(defconstant boole-nor   11
  "Boole function op, makes BOOLE return lognor of integer1 and integer2.")

(defconstant boole-andc1 12
  "Boole function op, makes BOOLE return logandc1 of integer1 and integer2.")

(defconstant boole-andc2 13
  "Boole function op, makes BOOLE return logandc2 of integer1 and integer2.")

(defconstant boole-orc1  14
  "Boole function op, makes BOOLE return logorc1 of integer1 and integer2.")

(defconstant boole-orc2  15
  "Boole function op, makes BOOLE return logorc2 of integer1 and integer2.")
