(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "assertoid.lisp")
  (use-package "ASSERTOID"))

;;; bug 254: compiler falure
(defpackage :bug254 (:use :cl))
(in-package :bug254)
(declaim (optimize (safety 3) (debug 2) (speed 2) (space 1)))
(defstruct foo
  (uhw2 nil :type (or package null)))
(macrolet ((defprojection (variant &key lexpr eexpr)
             (let ()
               `(defmethod uu ((foo foo))
                  (let ((uhw2 (foo.uhw2 bar)))
                    (let ()
                      (u-flunt uhw2
                               (baz (funcall ,lexpr south east 1)))))))))
  (defprojection h
      :lexpr (lambda (south east sched)
               (flet ((bd (x) (bref x sched)))
                 (let ((avecname (gafp)))
                   (declare (type (vector t) avecname))
                   (multiple-value-prog1
                       (progn
                         (setf (avec.count avecname) (length rest))
                         (setf (aref avecname 0) (bd (h south)))
                         (setf (aref avecname 1) (bd (h east)))
                         (stub avecname))
                     (paip avecname)))))
      :eexpr (lambda (south east))))
(delete-package :bug254)

;;; bug 255
(defpackage :bug255 (:use :cl))
(in-package :bug255)
(declaim (optimize (safety 3) (debug 2) (speed 2) (space 1)))
(defvar *1*)
(defvar *2*)
(defstruct v a b)
(defstruct w)
(defstruct yam (v nil :type (or v null)))
(defstruct un u)
(defstruct (bod (:include un)) bo)
(defstruct (bad (:include bod)) ba)
(declaim (ftype (function ((or w bad) (or w bad)) (values)) %ufm))
(defun %ufm (base bound) (froj base bound *1*) (values))
(declaim (ftype (function ((vector t)) (or w bad)) %pu))
(defun %pu (pds) *2*)
(defun uu (yam)
  (let ((v (yam-v az)))
    (%ufm v
          (flet ((project (x) (frob x 0)))
            (let ((avecname *1*))
              (multiple-value-prog1
                  (progn (%pu avecname))
                (frob)))))))
(delete-package :bug255)

;;; bug 148
(defpackage :bug148 (:use :cl))
(in-package :bug148)

(defvar *thing*)
(defvar *zoom*)
(defstruct foo bar bletch)
(defun %zeep ()
  (labels ((kidify1 (kid)
             )
           (kid-frob (kid)
             (if *thing*
                 (setf sweptm
                       (m+ (frobnicate kid)
                           sweptm))
                 (kidify1 kid))))
    (declare (inline kid-frob))
    (map nil
         #'kid-frob
         (the simple-vector (foo-bar perd)))))

(declaim (optimize (safety 3) (speed 2) (space 1)))
(defvar *foo*)
(defvar *bar*)
(defun u-b-sra (x r ad0 &optional ad1 &rest ad-list)
  (labels ((c.frob (c0)
             (let ()
               (when *foo*
                 (vector-push-extend c0 *bar*))))
           (ad.frob (ad)
             (if *foo*
                 (map nil #'ad.frob (the (vector t) *bar*))
                 (dolist (b *bar*)
                   (c.frob b)))))
    (declare (inline c.frob ad.frob))   ; 'til DYNAMIC-EXTENT
    (ad.frob ad0)))

(defun bug148-3 (ad0)
  (declare (special *foo* *bar*))
  (declare (optimize (safety 3) (speed 2) (space 1)))
  (labels ((c.frob ())
           (ad.frob (ad)
             (if *foo*
                 (mapc #'ad.frob *bar*)
                 (dolist (b *bar*)
                   (c.frob)))))
    (declare (inline c.frob ad.frob))
    (ad.frob ad0)))

(defun bug148-4 (ad0)
  (declare (optimize (safety 3) (speed 2) (space 1) (debug 1)))
  (labels ((c.frob (x)
             (* 7 x))
           (ad.frob (ad)
             (loop for b in ad
                   collect (c.frob b))))
    (declare (inline c.frob ad.frob))
    (list (the list ad0)
          (funcall (if (listp ad0) #'ad.frob #'print) ad0)
          (funcall (if (listp ad0) #'ad.frob #'print) (reverse ad0)))))

(assert (equal (eval '(bug148-4 '(1 2 3)))
               '((1 2 3) (7 14 21) (21 14 7))))

(delete-package :bug148)

;;; bug 258
(defpackage :bug258 (:use :cl))
(in-package :bug258)

(defun u-b-sra (ad0)
  (declare (special *foo* *bar*))
  (declare (optimize (safety 3) (speed 2) (space 1) (debug 1)))
  (labels ((c.frob (x)
             (1- x))
           (ad.frob (ad)
             (mapcar #'c.frob ad)))
    (declare (inline c.frob ad.frob))
    (list (the list ad0)
          (funcall (if (listp ad0) #'ad.frob #'print) ad0)
          (funcall (if (listp ad0) #'ad.frob #'print) (reverse ad0)))))

(assert (equal (u-b-sra '(4 9 7))
               '((4 9 7) (3 8 6) (6 8 3))))

(delete-package :bug258)

(in-package :cl-user)

;;;
(defun bug233a (x)
  (declare (optimize (speed 2) (safety 3)))
  (let ((y 0d0))
    (values
     (the double-float x)
     (setq y (+ x 1d0))
     (setq x 3d0)
     (funcall (eval ''list) y (+ y 2d0) (* y 3d0)))))
(assert (raises-error? (bug233a 4) type-error))

;;; compiler failure
(defun bug145b (x)
  (declare (type (double-float -0d0) x))
  (declare (optimize speed))
  (+ x (sqrt (log (random 1d0)))))

(sb-ext:quit :unix-status 104)
