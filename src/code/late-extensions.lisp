;;;; various extensions (including SB-INT "internal extensions")
;;;; available both in the cross-compilation host Lisp and in the
;;;; target SBCL, but which can't be defined on the target until until
;;;; some significant amount of machinery (e.g. error-handling) is
;;;; defined

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;; Is X a list for which LENGTH is meaningful, i.e. a list which is
;;; not improper and which is not circular?
(defun list-with-length-p (x)
  (values (ignore-errors (list-length x))))

;;; not used in 0.7.8, but possibly useful for defensive programming
;;; in e.g. (COERCE ... 'VECTOR)
;;;(defun list-length-or-die (x)
;;;  (or (list-length x)
;;;      ;; not clear how to do this best:
;;;      ;;   * Should this be a TYPE-ERROR? Colloquially that'd make
;;;      ;;     lots of sense, but since I'm not sure how to express
;;;      ;;     "noncircular list" as a Lisp type expression, coding
;;;      ;;     it seems awkward.
;;;      ;;   * Should the ERROR object include the offending value?
;;;      ;;     Ordinarily that's helpful, but if the user doesn't have
;;;      ;;     his printer set up to deal with cyclicity, we might not
;;;      ;;     be doing him a favor by printing the object here.
;;;      ;; -- WHN 2002-10-19
;;;      (error "can't calculate length of cyclic list")))

;;; This is used in constructing arg lists for debugger printing,
;;; and when needing to print unbound slots in PCL.
(defstruct (unprintable-object
            (:constructor make-unprintable-object (string))
            (:print-object (lambda (x s)
                             (print-unreadable-object (x s)
                               (write-string (unprintable-object-string x) s))))
            (:copier nil))
  string)
