;;;; a hack to suppress array specialization when building under the
;;;; cross-compiler

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;; It's hard to dump specialized vectors portably, because ANSI
;;; doesn't guarantee much about what specialized vectors exist.
;;; Thus, if we do
;;;   (MAKE-ARRAY 10 :ELEMENT-TYPE '(UNSIGNED-BYTE 4))
;;; in the cross-compilation host, we could easily end up with a
;;; vector of (UNSIGNED-BYTE 8) or of T, and the dumped result would
;;; reflect this.
;;;
;;; To reduce the prominence of this issue in cross-compilation, we
;;; can use these types, which expands into a specialized vector type when
;;; building the cross-compiler, and a SIMPLE-VECTOR otherwise.
(deftype specializable (type)
  #+sb-xc-host (declare (ignore type))
  #+sb-xc-host t
  #-sb-xc-host type)
(deftype specializable-vector (element-type)
  `(array (specializable ,element-type) 1))

;;; MAKE-SPECIALIZABLE-ARRAY is MAKE-ARRAY, except that in the interests of
;;; being able to dump the result without worrying about nonportable
;;; dependences on what kinds of specialized vectors actually exist in the
;;; cross-compilation host, any :ELEMENT-TYPE argument is discarded when
;;; running under the cross-compilation host ANSI Common Lisp.
#+sb-xc-host
(defun make-specializable-array (dimensions
                                 &rest rest
                                 &key (element-type t)
                                 &allow-other-keys)
  (apply #'make-array
         dimensions
         (if (eq element-type t)
           rest
           (do ((reversed-modified-rest nil))
               ((null rest) (nreverse reversed-modified-rest))
             (let ((first (pop rest))
                   (second (pop rest)))
               (when (eq first :element-type)
                 (setf second t))
               (push first reversed-modified-rest)
               (push second reversed-modified-rest))))))
#-sb-xc-host
(declaim #!-sb-fluid (inline make-specializable-array))
#-sb-xc-host
(defun make-specializable-array (&rest rest) (apply #'make-array rest))
