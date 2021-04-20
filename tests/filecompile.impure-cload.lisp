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
