;;; -*- lisp -*-

;;; This code is in the public domain.

;;; The cmucl implementation of simple-streams was done by Paul Foley,
;;; who placed the code in the public domain.  Sbcl port by Rudi
;;; Schlatte.

;;; This is just for compatibility with Franz demo code
(defpackage "EXCL"
  (:use "SB-SIMPLE-STREAM")
  (:import-from "SB-SIMPLE-STREAM"
	"BUFFER" "BUFFPOS" "BUFFER-PTR"
	"OUT-BUFFER" "MAX-OUT-POS"
	"INPUT-HANDLE" "OUTPUT-HANDLE"
	"MELDED-STREAM"
	"J-READ-CHARS"))

(use-package "SB-SIMPLE-STREAMS")

(provide :iodefs)

