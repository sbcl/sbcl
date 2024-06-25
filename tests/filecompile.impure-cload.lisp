;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

;;; COMPILE-FILE-LINE and COMPILE-FILE-POSITION

(macrolet ((line () `(multiple-value-call 'cons (compile-file-line))))
  (defun more-foo (x)
    (if x
        (format nil "Great! ~D" (line)) ; <-- this is line 17
        (format nil "Yikes ~D" (line)))))

(declaim (inline thing))
(defun thing ()
  (format nil "failed to frob a knob at line #~D"
          (compile-file-line))) ; <-- this is line 23

(defmacro more-randomness ()
  '(progn
    (let ()
      (thing))))

(macrolet ()
  (progn
    (defun bork (x)
      (flet ()
        (if x
            (locally (declare (notinline thing))
              (more-randomness))
            (progn (more-randomness))))))) ; <-- this is line 37

(defun compile-file-pos-sharp-dot (x)
  (list #.(format nil "Foo line ~D" (compile-file-line)) ; line #40
        x))

(defun compile-file-pos-eval-in-macro ()
  (macrolet ((macro (x)
               (format nil "hi ~A at ~D" x
                       (compile-file-line)))) ; line #46
    (macro "there")))

(with-test (:name :compile-file-line)
  (assert (string= (more-foo t) "Great! (17 . 32)"))
  (assert (string= (more-foo nil) "Yikes (18 . 31)"))
  (assert (string= (bork t) "failed to frob a knob at line #23"))
  (assert (string= (bork nil) "failed to frob a knob at line #37"))
  (assert (string= (car (compile-file-pos-sharp-dot nil))
                    "Foo line 40"))
  (assert (string= (compile-file-pos-eval-in-macro)
                    "hi there at 46")))

(eval-when (:compile-toplevel)
  (let ((stream (sb-c::source-info-stream sb-c::*source-info*)))
    (assert (pathname stream))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character
   #\# #\@
   (lambda (stream char arg)
     (declare (ignore char arg) (optimize (speed 0)))
     ;; return the column where the '#' was
     `'(,(- (stream-line-column stream) 2)))))

(defun foo-char-macro () (list #@
                          #@))

(with-test (:name :compile-file-stream-line-column)
  (assert (equal (foo-char-macro) '((31) (26)))))

(defglobal *form-grid-coordinates* (make-array 3 :fill-pointer 0))

(defmacro annotate-me (&whole thing &body body)
  (multiple-value-bind (start end) (sb-int:form-source-bounds thing)
    (vector-push-extend (vector start end) *form-grid-coordinates*)
    `(progn ,@body)))

(defun kickme (x y)
  (declare (integer x y))
  "foo"
  (progn
    (annotate-me (+ (annotate-me (progn (* x
;;; ^ This left paranthesis is at line 88 column 4
;;;                 ^ and this one is line 88 column 20
                                         5)))
                  (annotate-me (logcount y)))
               )))
;;;             ^ this right parenthesis is at line 93 column 16

;;; Hint: move around in Emacs to visually confirm that this output
;;; looks right, then plug the numbers into the test below.
#+nil(format t "~&According to compiler: ~S~%" *form-grid-coordinates*)

(with-test (:name :form-grid-coords-golden-test)
  (flet ((assert-line/col-is (index start end)
           (let ((actual (aref *form-grid-coordinates* index)))
             (assert (equal start (aref actual 0)))
             (assert (equal end (aref actual 1))))))
    (assert-line/col-is 0 '(88 . 4) '(93 . 16))
    ;; Somehow we can't figure out where annotation 1 ends precisely
    ;; but we do see that newlines occurred.
    (assert-line/col-is 1 '(88 . 20) '(92 . -1))
    (assert-line/col-is 2 '(92 . 18) '(92 . 44))))
