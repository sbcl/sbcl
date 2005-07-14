;;;; (See the comments at the head of the file compiler-extras.lisp.)

(in-package "SB-IMPL")

(declaim (optimize (speed 3) (space 1)))

;;; FIXME: should DEFUN REPLACE in terms of same expansion as
;;; DEFTRANSFORM
#+nil
(defun replace (..)
  (cond ((and (typep seq1 'simple-vector)
              (typep seq2 'simple-vector))
         (%replace-vector-vector ..))
        ((and (typep seq1 'simple-string)
              (typep seq2 'simple-string))
         (%replace-vector-vector ..))
        (t
         ..)))

