;;; -*- Package: ALPHA -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;;
;;; **********************************************************************
;;;
;;; Stuff to handle allocation of stuff we don't want to do inline.
;;;
;;; Written by William Lott.
;;;

(in-package "SB!VM")


;;; Given that the pseudo-atomic sequence is so short, there is
;;; nothing that qualifies.  But we want to keep the file around
;;; in case we decide to add something later.

