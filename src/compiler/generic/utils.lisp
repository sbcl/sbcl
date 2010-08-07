;;;; utility functions and macros needed by the back end to generate
;;;; code

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;; Make a fixnum out of NUM. (I.e. shift by two bits if it will fit.)
(defun fixnumize (num)
  (if (fixnump num)
      (ash num (1- n-lowtag-bits))
      (error "~W is too big for a fixnum." num)))

;;; Determining whether a constant offset fits in an addressing mode.
#!+(or x86 x86-64)
(defun foldable-constant-offset-p (element-size lowtag data-offset offset)
  (if (< element-size n-byte-bits)
      nil
      (multiple-value-bind (min max)
          (sb!impl::displacement-bounds lowtag element-size data-offset)
        (<= min offset max))))


;;;; routines for dealing with static symbols

(defun static-symbol-p (symbol)
  (or (null symbol)
      (and (member symbol *static-symbols*) t)))

;;; the byte offset of the static symbol SYMBOL
(defun static-symbol-offset (symbol)
  (if symbol
      (let ((posn (position symbol *static-symbols*)))
        (unless posn (error "~S is not a static symbol." symbol))
        (+ (* posn (pad-data-block symbol-size))
           (pad-data-block (1- symbol-size))
           other-pointer-lowtag
           (- list-pointer-lowtag)))
      0))

;;; Given a byte offset, OFFSET, return the appropriate static symbol.
(defun offset-static-symbol (offset)
  (if (zerop offset)
      nil
      (multiple-value-bind (n rem)
          (truncate (+ offset list-pointer-lowtag (- other-pointer-lowtag)
                       (- (pad-data-block (1- symbol-size))))
                    (pad-data-block symbol-size))
        (unless (and (zerop rem) (<= 0 n (1- (length *static-symbols*))))
          (error "The byte offset ~W is not valid." offset))
        (elt *static-symbols* n))))

;;; Return the (byte) offset from NIL to the start of the fdefn object
;;; for the static function NAME.
(defun static-fdefn-offset (name)
  (let ((static-syms (length *static-symbols*))
        (static-fun-index (position name *static-funs*)))
    (unless static-fun-index
      (error "~S isn't a static function." name))
    (+ (* static-syms (pad-data-block symbol-size))
       (pad-data-block (1- symbol-size))
       (- list-pointer-lowtag)
       (* static-fun-index (pad-data-block fdefn-size))
       other-pointer-lowtag)))

;;; Return the (byte) offset from NIL to the raw-addr slot of the
;;; fdefn object for the static function NAME.
(defun static-fun-offset (name)
  (+ (static-fdefn-offset name)
     (- other-pointer-lowtag)
     (* fdefn-raw-addr-slot n-word-bytes)))

;;; Various error-code generating helpers
(defvar *adjustable-vectors* nil)

(defmacro with-adjustable-vector ((var) &rest body)
  `(let ((,var (or (pop *adjustable-vectors*)
                   (make-array 16
                               :element-type '(unsigned-byte 8)
                               :fill-pointer 0
                               :adjustable t))))
     (declare (type (vector (unsigned-byte 8) 16) ,var))
     (setf (fill-pointer ,var) 0)
     (unwind-protect
         (progn
           ,@body)
       (push ,var *adjustable-vectors*))))
