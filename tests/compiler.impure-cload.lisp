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

;;; compiler failures reported by Paul Dietz: inaccurate dealing with
;;; BLOCK-LAST in CONSTANT-FOLD-CALL and DO-NODES
(defun #:foo (a b c d)
  (declare (type (integer -1 1000655) b)
           (optimize (speed 3) (safety 1) (debug 1)))
  (- (logior
      (abs (- (+ b (logandc1 -473949 (max 5165 (abs (logandc1 a 250775)))))))
      (logcount (logeqv (max (logxor (abs c) -1) 0) -4)))
     d))

(defun #:foo (a d)
  (declare (type (integer -8507 26755) a)
           (type (integer -393314538 2084485) d)
           (optimize (speed 3) (safety 1) (debug 1)))
  (gcd
   (if (= 0 a) 10 (abs -1))
   (logxor -1
           (min -7580
                (max (logand a 31365125) d)))))

;;; compiler failure "NIL is not of type LVAR"
(defun #:foo (x)
  (progn (truly-the integer x)
         (1+ x)))

;;; bug 291 reported by Nikodemus Siivola (modified version)
(defstruct line
  (%chars ""))
(defun update-window-imag (line)
  (tagbody
   TOP
     (if (null line)
         (go DONE)
         (go TOP))
   DONE
     (unless (eq current the-sentinel)
       (let* ((cc (car current))
              (old-line (dis-line-line cc)))
         (if (eq old-line line)
             (do ((chars (line-%chars line) nil))
                 (())
               (let* ()
                 (multiple-value-call
                     #'(lambda (&optional g2740 g2741 &rest g2742)
                         (declare (ignore g2742))
                         (catch 'foo
                           (values (setq string g2740) (setq underhang g2741))))
                   (foo)))
               (setf (dis-line-old-chars cc) chars)))))))

;;; and similar cases found by Paul Dietz
(defun #:foo (a b c)
  (declare (optimize (speed 0) (safety 3) (debug 3)))
  (FLET ((%F11 ()
           (BLOCK B6
             (LET ((V2 B))
               (IF (LDB-TEST (BYTE 27 14) V2)
                   (LET ((V6
                          (FLET ((%F7 ()
                                   B))
                            -1)))
                     (RETURN-FROM B6 V2))
                   C)))))
    A))
(defun #:foo (a b c)
  (declare (optimize (speed 0) (safety 3) (debug 3)))
  (FLET ((%F15 ()
           (BLOCK B8
             (LET ((V5 B))
               (MIN A (RETURN-FROM B8 C))))))
    C))

;;; bug 292, reported by Paul Dietz
(defun #:foo (C)
  (DECLARE (TYPE (INTEGER -5945502333 12668542) C)
           (OPTIMIZE (SPEED 3)))
  (LET ((V2 (* C 12)))
    (- (MAX (IF (/= 109335113 V2) -26479 V2)
            (DEPOSIT-FIELD 311
                           (BYTE 14 28)
                           (MIN (MAX 521326 C) -51))))))

;;; zombie variables, arising from constraints
(defun #:foo (A B)
  (DECLARE (TYPE (INTEGER -40945116 24028306) B)
           (OPTIMIZE (SPEED 3)))
  (LET ((V5 (MIN 31883 (LOGCOUNT A))))
    (IF (/= B V5) (IF (EQL 122911784 V5) -43765 1487) B)))

;;; let-conversion of a function into deleted one
(defun #:foo (a c)
  (declare (type (integer -883 1566) a)
           (type (integer -1 0) c)
           (optimize (speed 3) (safety 1) (debug 1)))
  (flet ((%f8 () c))
    (flet ((%f5 ()
             (if (< c a)
                 (return-from %f5 (if (= -4857 a) (%f8) (%f8)))
                 c)))
      (if (<= 11 c) (%f5) c))))

;;; two bugs: "aggressive" deletion of optional entries and problems
;;; of FIND-RESULT-TYPE in dealing with deleted code; reported by
;;; Nikodemus Siivola (simplified version)
(defun lisp-error-error-handler (condition)
  (invoke-debugger condition)
  (handler-bind ()
    (unwind-protect
         (with-simple-restart
             (continue "return to hemlock's debug loop.")
           (invoke-debugger condition))
      (device))))

;;;
(defun #:foo ()
  (labels ((foo (x)
             (return-from foo x)
             (block u
               (labels ((bar (x &optional (y (return-from u)))
                          (list x y (apply #'bar (fee)))))
                 (list (bar 1) (bar 1 2))))
             (1+ x)))
    #'foo))

(defun #:foo (b c)
  (declare (type (integer 0 1) b) (optimize (speed 3)))
  (flet ((%f2 () (lognor (block b5 138) c)))
    (if (not (or (= -67399 b) b))
        (deposit-field (%f2) (byte 11 8) -3)
        c)))


(sb-ext:quit :unix-status 104)
