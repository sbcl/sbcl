;;;; Lisp-side allocation (used currently only for direct allocation
;;;; to static space).

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

#!-sb-fluid (declaim (inline store-word))
(defun store-word (word base &optional (offset 0) (lowtag 0))
  (declare (type (unsigned-byte #.sb!vm:n-word-bits) word base offset)
           (type (unsigned-byte #.n-lowtag-bits) lowtag))
  (setf (sap-ref-word (int-sap base) (- (ash offset word-shift) lowtag)) word))

(defun allocate-static-vector (widetag length words)
  (declare (type (unsigned-byte #.n-widetag-bits) widetag)
           (type (unsigned-byte #.n-word-bits) words)
           (type index length))
  ;; WITHOUT-GCING implies WITHOUT-INTERRUPTS
  (or
   (without-gcing
     (let* ((pointer *static-space-free-pointer*) ; in words
            (free (* pointer n-word-bytes))
            (vector (logior free other-pointer-lowtag)) ; in bytes, yay
            ;; rounded to dual word boundary
            (nwords (logandc2 (+ lowtag-mask (+ words vector-data-offset 1))
                              lowtag-mask))
            (new-pointer (+ *static-space-free-pointer* nwords))
            (new-free (* new-pointer n-word-bytes)))
       (when (> static-space-end new-free)
         (store-word widetag
                     vector 0 other-pointer-lowtag)
         (store-word (ash length word-shift)
                     vector vector-length-slot other-pointer-lowtag)
         (store-word 0 new-free)
         (setf *static-space-free-pointer* new-pointer)
         (%make-lisp-obj vector))))
   (error 'simple-storage-condition
          :format-control "Not enough memory left in static space to ~
                           allocate vector.")))

