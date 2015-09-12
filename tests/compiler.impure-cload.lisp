(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "assertoid.lisp")
  (load "compiler-test-util.lisp")
  (load "test-util.lisp")
  (use-package "ASSERTOID"))

;;; bug 254: compiler falure
(defpackage :bug254 (:use :cl))
(in-package :bug254)
(declaim (optimize (safety 3) (debug 2) (speed 2) (space 1)))
(defstruct foo
  (uhw2 nil :type (or package null)))
(macrolet ((defprojection (variant &key lexpr eexpr)
             (declare (ignore variant eexpr))
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
(in-package :cl-user)
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
(defun %pu (pds) (declare (ignore pds)) *2*)
(defun uu (yam)
  (declare (ignore yam))
  (let ((v (yam-v az)))
    (%ufm v
          (flet ((project (x) (frob x 0)))
            (let ((avecname *1*))
              (multiple-value-prog1
                  (progn (%pu avecname))
                (frob)))))))
(in-package :cl-user)
(delete-package :bug255)

;;; bug 148
(defpackage :bug148 (:use :cl))
(in-package :bug148)

(defvar *thing*)
(defvar *zoom*)
(defstruct foo bar bletch)
(defun %zeep ()
  (labels ((kidify1 (kid)
             (declare (ignore kid))
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
             (declare (ignorable ad))
             (if *foo*
                 (mapc #'ad.frob *bar*)
                 (dolist (b *bar*)
                   (declare (ignore b))
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

(in-package :cl-user)
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

(in-package :cl-user)
(delete-package :bug258)

;;;
(defun bug233a (x)
  (declare (optimize (speed 2) (safety 3)))
  (let ((y 0d0))
    (values
     (the double-float x)
     (setq y (+ x 1d0))
     (setq x 3d0)
     (funcall (eval ''list) y (+ y 2d0) (* y 3d0)))))
(assert-error (bug233a 4) type-error)

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

(defun #:foo (a b c)
  (declare (type (integer -5498929 389890) a)
           (type (integer -5029571274946 48793670) b)
           (type (integer 9221496 260169518304) c)
           (ignorable a b c)
           (optimize (speed 3) (safety 1) (debug 1)))
  (- (mod 1020122 (min -49 -420))
     (logandc1
      (block b2 (mod c (min -49 (if t (return-from b2 1582) b))))
      (labels ((%f14 ()
                 (mod a (max 76 8))))
        b))))

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

;;; bug 214: compiler failure
(defun bug214a1 ()
  (declare (optimize (sb-ext:inhibit-warnings 0) (compilation-speed 2)))
  (flet ((foo (&key (x :vx x-p)) (list x x-p)))
    (foo :x 2)))

(defun bug214a2 ()
  (declare (optimize (sb-ext:inhibit-warnings 0) (compilation-speed 2)))
  (lambda (x) (declare (fixnum x)) (if (< x 0) 0 (1- x))))

;;; this one was reported by rydis on #lisp
(defun 214b (n)
  (declare (fixnum n))
  (declare (optimize (speed 2) (space 3)))
  (dotimes (k n)
    (princ k)))

;;; bug reported by Brian Downing: incorrect detection of MV-LET
(DEFUN #:failure-testcase (SESSION)
  (LABELS ((CONTINUATION-1 ()
             (PROGN
               (IF (foobar-1 SESSION)
                   (CONTINUATION-2))
               (LET ((CONTINUATION-3
                      #'(LAMBDA ()
                          (MULTIPLE-VALUE-CALL #'CONTINUATION-2
                            (CONTINUATION-1)))))
                 (foobar-2 CONTINUATION-3))))
           (CONTINUATION-2 (&REST OTHER-1)
             (DECLARE (IGNORE OTHER-1))))
    (continuation-1)))

;;; reported by antifuchs/bdowning/etc on #lisp: ITERATE failure on
;;; (iter (for i in '(1 2 3)) (+ i 50))
(defun values-producer () (values 1 2 3 4 5 6 7))

(defun values-consumer (fn)
  (let (a b c d e f g h)
    (multiple-value-bind (aa bb cc dd ee ff gg hh) (funcall fn)
      (setq a aa)
      (setq b bb)
      (setq c cc)
      (setq d dd)
      (setq e ee)
      (setq f ff)
      (setq g gg)
      (setq h hh)
      (values a b c d e f g h))))

(let ((list (multiple-value-list (values-consumer #'values-producer))))
  (assert (= (length list) 8))
  (assert (null (nth 7 list))))

;;; failed on Alpha prior to sbcl-0.8.10.30
(defun lotso-values ()
  (values 0 1 2 3 4 5 6 7 8 9
          0 1 2 3 4 5 6 7 8 9
          0 1 2 3 4 5 6 7 8 9
          0 1 2 3 4 5 6 7 8 9
          0 1 2 3 4 5 6 7 8 9
          0 1 2 3 4 5 6 7 8 9
          0 1 2 3 4 5 6 7 8 9
          0 1 2 3 4 5 6 7 8 9
          0 1 2 3 4 5 6 7 8 9
          0 1 2 3 4 5 6 7 8 9))

;;; bug 313: source transforms were "lisp-1"
(defun srctran-lisp1-1 (cadr) (if (functionp cadr) (funcall cadr 1) nil))
(assert (eql (funcall (eval #'srctran-lisp1-1) #'identity) 1))
(without-package-locks
   ;; this be a nasal demon, but test anyways
   (defvar caar))
(defun srctran-lisp1-2 (caar) (funcall (sb-ext:truly-the function caar) 1))
(assert (eql (funcall (eval #'srctran-lisp1-2) #'identity) 1))

;;; partial bug 262: reference of deleted CTRAN (in RETURN-FROM)
;;; during inline expansion. Bug report by Peter Denno, simplified
;;; test case by David Wragg.
(defun bug262-return-from (x &aux (y nil))
  (declare (ignore y))
  (labels ((foo-a (z) (return-from bug262-return-from z))
           (foo-b (z) (foo-a z)))
    (declare (inline foo-a))
    (foo-a x)))

;;; broken inference of an upper bound of an iteration variable,
;;; reported by Rajat Datta.
(defun isieve (num)
  (let ((vec (make-array num :initial-element 0))
        (acc 0))
    (do ((i 2 (+ i 1)))
        ((>= i num) 'done)
      (when (= (svref vec i) 0)
        (do ((j (* i i) (+ j i)))
            ((>= j num) 'done)
          (setf (svref vec j) 1))
        (incf acc)))
    acc))

(assert (= (isieve 46349) 4792))

;;; COERCE should not be constant-folded (reported by Nikodemus
;;; Siivola)
(let ((f (gensym)))
  (setf (fdefinition f) (lambda (x) x))
  (let ((g (compile nil `(lambda () (coerce ',f 'function)))))
    (setf (fdefinition f) (lambda (x) (1+ x)))
    (assert (eq (funcall g) (fdefinition f)))))

(let ((x (coerce '(1 11) 'vector)))
  (incf (aref x 0))
  (assert (equalp x #(2 11))))

;;; and BIT-* too (reported by Paul F. Dietz)
(loop with v1 = #*0011
      and  v2 = #*0101
      for f in '(bit-and bit-andc1 bit-andc2 bit-eqv
                 bit-ior bit-nand bit-nor bit-not
                 bit-orc1 bit-orc2 bit-xor
                 )
      for form = `(lambda ()
                    (let ((v (,f ,v1 ,v2)))
                      (setf (aref v 0) (- 1 (aref v 0)))
                      (aref v 0)))
      for compiled-res = (funcall (compile nil form))
      for real-res = (- 1 (aref (funcall f v1 v2) 0))
      do (assert (equal compiled-res real-res)))
(let* ((v #*0011)
       (form `(lambda ()
                (let ((v (bit-not ,v)))
                  (setf (aref v 0) (- 1 (aref v 0)))
                  (aref v 0))))
       (compiled-res (funcall (compile nil form)))
       (real-res (- 1 (aref (funcall (eval #'bit-not) v) 0))))
  (assert (equal compiled-res real-res)))

;; bug reported on sbcl-devel by Hannu Koivisto on 2005-08-10
(defvar *hannu-trap* nil)
(progv '(*hannu-trap*) '()
  (setq *hannu-trap* t))
(assert (not *hannu-trap*))

;;; bug reported on sbcl-help by Vasile Rotaru
(let* ((initial-size (expt 2 16))
       (prime-table (make-array initial-size
                                :element-type 'integer))
       (first-primes #(5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71
       73
                      79 83 89 97 101 103 107 109 113 127 131 137 139 149
                      151 157 163 167 173 179 181 191 193 197 199 211 223
                      227 229 233 239 241 251 257 263 269 271 277 281))
       (count 0)
       (increment 2))

  (defun largest-prime-so-far ()
    (aref prime-table (1- count)))
  (defun add-prime (prime)
    (setf (aref prime-table count) prime) (incf count))
  (defun init-table ()
    (map 'nil #'add-prime first-primes))
  (defun next-candidate (candidate)
    (prog1 (+ candidate increment)
      (ecase increment
        (2 (setf increment 4))
        (4 (setf increment 2)))))
  (defun prime-p (n)
    (let ((sqrt-n (truncate (sqrt n))))
      (dotimes (i count)
        (let ((prime (aref prime-table i)))
          (when (> prime sqrt-n)
            (return-from prime-p t))
          (when (zerop (mod n prime))
            (return-from prime-p nil))))
      (error "~&prime-table too small: ~A ~A~%" n
      (largest-prime-so-far))))
  (defun generate-primes (required)
    (do ((candidate (next-candidate (largest-prime-so-far))
                    (next-candidate candidate)))
        ((> candidate required))
      (when (prime-p candidate)
        (add-prime candidate))))
  ;;
  (init-table))

;;; Bug in the fopcompiler's handling of LOCALLY pre-0.9.14.8

(defvar *a* 1)

(setf *a*
      (locally
          (declare)
        2))

;;; Bug in the interaction of BIND-SENTINEL and UNBIND-TO-HERE, as
;;; used by PROGV.

(defvar *foo-1* nil)
(defvar *foo-2* nil)

(defun foo ()
  (declare (optimize (debug 2)))
  (let ((*foo-1* nil))
    (progv
        (list '*foo-2*)
        (list nil)
      (write-line "foo-2"))
    (write-line "foo-1"))
  (write-line "foo-0"))

(foo)

;;; LOAD-TIME-VALUE smartness
(defun load-time-value-type-derivation-test-1 ()
  (ctu:compiler-derived-type (load-time-value (cons 'foo 0))))
(defun load-time-value-type-derivation-test-2 ()
  (ctu:compiler-derived-type (load-time-value (+ (or *print-length* 0) 10))))
(defun load-time-value-auto-read-only-p ()
  (load-time-value (random most-positive-fixnum)))
(defun load-time-value-boring ()
  (load-time-value (cons t t)))
(test-util:with-test (:name (load-time-value :type-smartness/cload))
  (assert (eq 'cons (load-time-value-type-derivation-test-1)))
  (assert (equal '(integer 10) (load-time-value-type-derivation-test-2)))
  (assert (not (ctu:find-value-cell-values #'load-time-value-auto-read-only-p)))
  (assert (ctu:find-value-cell-values #'load-time-value-boring)))

(defun regression-1.0.29.54 ()
  (logior (1+ most-positive-fixnum)
          (load-time-value (the fixnum (eval 1)) t)))

(test-util:with-test (:name :regression-1.0.29.54)
  (assert (= (+ most-positive-fixnum 2) (regression-1.0.29.54)))
  (assert (eq 42
              (funcall (compile nil
                                `(lambda ()
                                   (load-time-value (values 42))))))))

(defun mv-call-regression-1.0.43.57-foo (a c d x y)
  (values a c d x y))
(defun mv-call-regression-1.0.43.57-bar (a b c d)
  (declare (number a b c d))
  (values a b c d))
(defun mv-call-regression-1.0.43.57-quux (a sxx sxy syy)
  (multiple-value-call #'mv-call-regression-1.0.43.57-foo
    (mv-call-regression-1.0.43.57-bar sxx sxy sxy syy)
    a))
(test-util:with-test (:name :mv-call-regression-1.0.43.57)
  ;; This used to signal a bogus argument-count error.
  (mv-call-regression-1.0.43.57-quux 1s0 10s0 1s0 10s0))

(defun etypecase-failure-test (x)
  (etypecase x
    (bignum 'b)
    (character 'c)
    ((or fixnum float) 'f)))
(locally
    (declare (muffle-conditions style-warning))
  (defun ecase-failure-test (x)
    (ecase x ((a b c) 1) ((b c d) 2) ((e d f) 3) ((c g h i) 4))))

(test-util:with-test (:name :case-failures)
  (assert (equal (handler-case (etypecase-failure-test 'hi)
                   (sb-kernel:case-failure (c)
                     (sb-kernel::case-failure-possibilities c)))
                 ;; In order as originally written
                 '(bignum character (or fixnum float))))
  (assert (equal (handler-case (ecase-failure-test 'hi)
                   (sb-kernel:case-failure (c)
                     (sb-kernel::case-failure-possibilities c)))
                 ;; In order as originally written, and no dups.
                 '(a b c d e f g h i))))

(defparameter *circular-cons-with-a-vector-cdr*
  #1=#((a .  #1#)))
