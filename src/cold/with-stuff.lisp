;;;; code to tweak compilation environment, used to set up
;;;; for different phases of cross-compilation

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-COLD")

;;; a helper macro for WITH-ADDITIONAL-NICKNAME and WITHOUT-SOME-NICKNAME
(defmacro with-given-nicknames ((package-designator nicknames) &body body)
  (let ((p (gensym "P"))
        (n (gensym "N"))
        (o (gensym "O")))
    `(let* ((,p ,package-designator) ; PACKAGE-DESIGNATOR, evaluated only once
            (,n ,nicknames) ; NICKNAMES, evaluated only once
            (,o (package-nicknames ,p))) ; old package nicknames
       (rename-package-carefully ,p (package-name ,p) ,n)
       (unwind-protect
           (progn ,@body)
         (unless (nicknames= ,n (package-nicknames ,p))
           ;; This probably didn't happen on purpose, and it's not clear anyway
           ;; what we should do when it did happen, so die noisily:
           (error "package nicknames changed within WITH-GIVEN-NICKNAMES: ~
                   expected ~S, found ~S"
                  ,n
                  (package-nicknames ,p)))
         (rename-package-carefully ,p (package-name ,p) ,o)))))

;;; Execute BODY with NICKNAME added as a nickname for PACKAGE-DESIGNATOR.
(defmacro with-additional-nickname ((package-designator nickname) &body body)
  (let ((p (gensym "P"))
        (n (gensym "N")))
    `(let* ((,p ,package-designator) ; PACKAGE-DESIGNATOR, evaluated only once
            (,n ,nickname)) ; NICKNAME, evaluated only once
       (if (find-package ,n)
         (error "~S is already a package name." ,n)
         (with-given-nicknames (,p (cons ,n (package-nicknames ,p)))
           ,@body)))))

;;; Execute BODY with NICKNAME removed as a nickname for PACKAGE-DESIGNATOR.
(defmacro without-given-nickname ((package-designator nickname) &body body)
  (let ((p (gensym "P"))
        (n (gensym "N"))
        (o (gensym "O")))
    `(let* ((,p ,package-designator) ; PACKAGE-DESIGNATOR, evaluated only once
            (,n ,nickname) ; NICKNAME, evaluated only once
            (,o (package-nicknames ,p))) ; old package nicknames
       (if (find ,n ,o :test #'string=)
         (with-given-nicknames (,p (remove ,n ,o :test #'string=))
           ,@body)
         (error "~S is not a nickname for ~S." ,n ,p)))))

;;; a helper function for WITH-NICKNAME: Are two collections of package
;;; nicknames the same?
(defun nicknames= (x y)
  (equal (sort (mapcar #'string x) #'string<)
         (sort (mapcar #'string y) #'string<)))
(compile 'nicknames=)

;;; helper functions for WITH-ADDITIONAL-NICKNAMES and WITHOUT-GIVEN-NICKNAMES
(defun %with-additional-nickname (package-designator nickname body-fn)
  (declare (type function body-fn))
  (with-additional-nickname (package-designator nickname)
    (funcall body-fn)))
(defun %without-given-nickname (package-designator nickname body-fn)
  (declare (type function body-fn))
  (without-given-nickname (package-designator nickname)
    (funcall body-fn)))
(defun %multi-nickname-magic (nd-list single-nn-fn body-fn)
  (declare (type function single-nn-fn))
  (labels ((multi-nd (nd-list body-fn) ; multiple nickname descriptors
             (declare (type function body-fn))
             (if (null nd-list)
               (funcall body-fn)
               (single-nd (first nd-list)
                          (lambda ()
                            (multi-nd (rest nd-list) body-fn)))))
           (single-nd (nd body-fn) ; single nickname descriptor
             (destructuring-bind (package-descriptor nickname-list) nd
               (multi-nn package-descriptor nickname-list body-fn)))
           (multi-nn (nn-list package-descriptor body-fn) ; multiple nicknames
             (declare (type function body-fn))
             (if (null nn-list)
               (funcall body-fn)
               (funcall single-nn-fn
                        (first nn-list)
                        package-descriptor
                        (lambda ()
                          (multi-nn package-descriptor
                                    (rest nn-list)
                                    body-fn))))))
    (multi-nd nd-list body-fn)))
(compile '%with-additional-nickname)
(compile '%without-given-nickname)
(compile '%multi-nickname-magic)

;;; This is like WITH-ADDITIONAL-NICKNAME and WITHOUT-GIVEN-NICKNAMES,
;;; except working on arbitrary lists of nickname descriptors instead
;;; of single nickname/package pairs.
;;;
;;; A nickname descriptor is a list of the form
;;;   PACKAGE-DESIGNATOR NICKNAME*
(defmacro with-additional-nicknames (nickname-descriptor-list &body body)
  `(%multi-nickname-magic ,nickname-descriptor-list
                          #'%with-additional-nickname
                          (lambda () ,@body)))
(defmacro without-given-nicknames (nickname-descriptor-list &body body)
  `(%multi-nickname-magic ,nickname-descriptor-list
                          #'%without-additional-nickname
                          (lambda () ,@body)))
