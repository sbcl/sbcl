;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(defun featurep (x)
  #!+sb-doc
  "If X is an atom, see whether it is present in *FEATURES*. Also
  handle arbitrary combinations of atoms using NOT, AND, OR."
  (if (consp x)
    (case (car x)
      ((:not not)
       (if (cddr x)
	 (error "too many subexpressions in feature expression: ~S" x)
	 (not (featurep (cadr x)))))
      ((:and and) (every #'featurep (cdr x)))
      ((:or or) (some #'featurep (cdr x)))
      (t
       (error "unknown operator in feature expression: ~S." x)))
    (not (null (memq x *features*)))))

;;; Given a list of keyword substitutions `(,OLD ,NEW), and a
;;; &KEY-argument-list-style list of alternating keywords and
;;; arbitrary values, return a new &KEY-argument-list-style list with
;;; all substitutions applied to it.
;;;
;;; Note: If efficiency mattered, we could do less consing. (But if
;;; efficiency mattered, why would we be using &KEY arguments at
;;; all, much less renaming &KEY arguments?)
;;;
;;; KLUDGE: It would probably be good to get rid of this. -- WHN 19991201
(defun rename-key-args (rename-list key-args)
  (declare (type list rename-list key-args))
  ;; Walk through RENAME-LIST modifying RESULT as per each element in
  ;; RENAME-LIST.
  (do ((result (copy-list key-args))) ; may be modified below
      ((null rename-list) result)
    (destructuring-bind (old new) (pop rename-list)
      ;; ANSI says &KEY arg names aren't necessarily KEYWORDs.
      (declare (type symbol old new))
      ;; Walk through RESULT renaming any OLD key argument to NEW.
      (do ((in-result result (cddr in-result)))
	  ((null in-result))
	(declare (type list in-result))
	(when (eq (car in-result) old)
	  (setf (car in-result) new))))))

;;; ANSI Common Lisp's READ-SEQUENCE function, unlike most of the
;;; other ANSI input functions, is defined to communicate end of file
;;; status with its return value, not by signalling. This is not the
;;; behavior we usually want. This is a wrapper which give the
;;; behavior we usually want, causing READ-SEQUENCE to communicate
;;; end-of-file status by signalling.
(defun read-sequence-or-die (sequence stream &key start end)
  ;; implementation using READ-SEQUENCE
  #-no-ansi-read-sequence
  (let ((read-end (read-sequence sequence
				 stream
				 :start start
				 :end end)))
    (unless (= read-end end)
      (error 'end-of-file :stream stream))
    (values))
  ;; workaround for broken READ-SEQUENCE
  #+no-ansi-read-sequence
  (progn
    (aver (<= start end))
    (let ((etype (stream-element-type stream)))
    (cond ((equal etype '(unsigned-byte 8))
	   (do ((i start (1+ i)))
	       ((>= i end)
		(values))
	     (setf (aref sequence i)
		   (read-byte stream))))
	  (t (error "unsupported element type ~S" etype))))))
