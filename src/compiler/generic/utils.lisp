;;;; utility functions needed by the back end to generate code

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

(file-comment
  "$Header$")

(defun fixnumize (num)
  #!+sb-doc
  "Make a fixnum out of NUM. (i.e. shift by two bits if it will fit.)"
  (if (<= #x-20000000 num #x1fffffff)
      (ash num 2)
      (error "~D is too big for a fixnum." num)))

;;;; routines for dealing with static symbols

(defun static-symbol-p (symbol)
  (or (null symbol)
      (and (member symbol *static-symbols*) t)))

(defun static-symbol-offset (symbol)
  #!+sb-doc
  "the byte offset of the static symbol SYMBOL"
  (if symbol
      (let ((posn (position symbol *static-symbols*)))
	(unless posn (error "~S is not a static symbol." symbol))
	(+ (* posn (pad-data-block symbol-size))
	   (pad-data-block (1- symbol-size))
	   other-pointer-type
	   (- list-pointer-type)))
      0))

(defun offset-static-symbol (offset)
  #!+sb-doc
  "Given a byte offset, OFFSET, return the appropriate static symbol."
  (if (zerop offset)
      nil
      (multiple-value-bind (n rem)
	  (truncate (+ offset list-pointer-type (- other-pointer-type)
		       (- (pad-data-block (1- symbol-size))))
		    (pad-data-block symbol-size))
	(unless (and (zerop rem) (<= 0 n (1- (length *static-symbols*))))
	  (error "The byte offset ~D is not valid." offset))
	(elt *static-symbols* n))))

(defun static-function-offset (name)
  #!+sb-doc
  "Return the (byte) offset from NIL to the start of the fdefn object
   for the static function NAME."
  (let ((static-syms (length *static-symbols*))
	(static-function-index (position name *static-functions*)))
    (unless static-function-index
      (error "~S isn't a static function." name))
    (+ (* static-syms (pad-data-block symbol-size))
       (pad-data-block (1- symbol-size))
       (- list-pointer-type)
       (* static-function-index (pad-data-block fdefn-size))
       (* fdefn-raw-addr-slot word-bytes))))
