;;;; PCOUNTERs
;;;;
;;;; a PCOUNTER is used to represent an unsigned integer quantity which
;;;; can grow bigger than a fixnum, but typically does so, if at all,
;;;; in many small steps, where we don't want to cons on every step.
;;;; Such quantities typically arise in profiling, e.g. 
;;;; total system consing, time spent in a profiled function, and
;;;; bytes consed in a profiled function are all examples of such
;;;; quantities. The name is an abbreviation for "Profiling COUNTER".
;;;;
;;;; It's not one of my more brilliant names, if you have a better
;;;; suggestion, I might be interested. -- WHN 2001-06-22

(in-package "SB!IMPL")

;;;; basic PCOUNTER stuff

(/show0 "pcounter.lisp 16")

(defstruct (pcounter (:copier nil))
  (integer 0 :type unsigned-byte)
  (fixnum 0 :type (and fixnum unsigned-byte)))

(/show0 "pcounter.lisp 22")

(declaim (maybe-inline incf-pcounter))
(defun incf-pcounter (pcounter delta)
  (aver (typep delta 'unsigned-byte))
  (let ((sum (+ (pcounter-fixnum pcounter) delta)))
    (cond ((typep sum 'fixnum)
	   (setf (pcounter-fixnum pcounter) sum))
	  (t
	   (incf (pcounter-integer pcounter) sum)
	   (setf (pcounter-fixnum pcounter) 0))))
  pcounter)

(/show0 "pcounter.lisp 36")

;;;(declaim (inline pcounter->integer)) ; FIXME: maybe inline when more stable
(defun pcounter->integer (pcounter)
  (+ (pcounter-integer pcounter)
     (pcounter-fixnum pcounter)))

;;;; operations on (OR PCOUNTER FIXNUM)
;;;;
;;;; When we don't want to cons a PCOUNTER unless we're forced to, we
;;;; start with a FIXNUM counter and only create a PCOUNTER if the
;;;; FIXNUM overflows.

(/show0 "pcounter.lisp 50")

(declaim (inline %incf-pcounter-or-fixnum))
(defun %incf-pcounter-or-fixnum (x delta)
  (etypecase x
    (fixnum
     (let ((sum (+ x delta)))
       (if (typep sum 'fixnum)
	   sum
	   (make-pcounter :integer sum))))
    (pcounter
     (incf-pcounter x delta))))
  
(define-modify-macro incf-pcounter-or-fixnum (delta) %incf-pcounter-or-fixnum)

(/show0 "pcounter.lisp 64")

;;; Trade off space for execution time by handling the common fast
;;; (TYPEP DELTA 'FIXNUM) case inline and only calling generic
;;; arithmetic as a last resort.
(defmacro fastbig-incf-pcounter-or-fixnum (x delta)
  (let ((delta-sym (gensym "DELTA")))
    `(let ((,delta-sym ,delta))
       (aver (typep ,delta-sym 'unsigned-byte))
       ;;(declare (type unsigned-byte ,delta-sym))
       (if (typep ,delta-sym 'fixnum)
	   (incf-pcounter-or-fixnum ,x ,delta)
	   (incf-pcounter-or-fixnum ,x ,delta)))))

(/show0 "pcounter.lisp 80")

(declaim (maybe-inline pcounter-or-fixnum->integer))
(defun pcounter-or-fixnum->integer (x)
  (etypecase x
    (fixnum x)
    (pcounter (pcounter->integer x))))

(/show0 "pcounter.lisp end")
