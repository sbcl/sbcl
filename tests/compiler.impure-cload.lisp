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

(sb-ext:quit :unix-status 104)
