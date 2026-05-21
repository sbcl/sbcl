;;; Consider the following definitions in which we assign F2 and F3
;;; the function binding of F1.
;;; Even if we were to agree that tracing of F2 and F3 should happen
;;; (which I think is wrong in itself), then what about SB-DEBUG::*TRACED-FUNS* ?
;;; It doesn't know that F2 and F3 are traced, which seems like a metadata bug.
;;; So then when we UNTRACE F1, we don't know that we're supposed to UNTRACE
;;; F2 and F3, so they continue to print the tracing output.
;;; And aside from that, when F2 or F3 would get invoked, the tracing output
;;; would suggest that F1 was called by name, which doesn't make a ton of sense,
;;; because first of all tracing is supposed to print the name of what was
;;; actually called - which coincidentally is bound to the same function object
;;; as some other global name - and secondly we just explicitly UNTRACEd the very
;;; name that it said was called.
;;; A reasonable solution to these bugs is to remove the tracing encapsulation
;;; when assigning SYMBOL-FUNCTION or FDEFINITION. Granted there are other
;;; encapsulations (profiling, e.g.) but this entire area is unspecified.

(defparameter *count* 0)
(defun f1 () (incf *count*))

(test-util:with-test (:name (:print-object-function-name :untraced))
  ;; The SEARCH allows for SB-KERNEL:INTERPRETED-FUNCTION as well.
  (assert (search "FUNCTION F1>" (prin1-to-string #'f1))))

(defvar *f1/base* #'f1)
(trace f1)
(defvar *f1/traced* #'f1)
(setf (symbol-function 'f2) #'f1)
(setf (fdefinition 'f3) #'f1)
(defun f4 ())
(trace f4)
(defun f5 ())
(trace f5)
(setf (symbol-function 'f4) #'f1)
(setf (fdefinition 'f5) #'f1)

(test-util:with-test (:name :symbol-function-vs-fdefinition)
  (assert (not (eq *f1/base* *f1/traced*)))
  (assert (eq (symbol-function 'f1) (fdefinition 'f1))))

(test-util:with-test (:name :strip-encap)
  (let ((s (with-output-to-string (*trace-output*)
             (f1))))
    (assert (search "F1 returned 1" s)))
  (let ((s (with-output-to-string (*trace-output*)
             (f2))))
    (assert (= (length s) 0)))
  (let ((s (with-output-to-string (*trace-output*)
             (f3))))
    (assert (= (length s) 0)))
  (let ((s (with-output-to-string (*trace-output*)
             (f4))))
    (assert (search "F4 returned 4" s)))
  (let ((s (with-output-to-string (*trace-output*)
             (f5))))
    (assert (search "F5 returned 5" s))))

(test-util:with-test (:name (:printed-function-name :traced))
  (assert (search "FUNCTION F1 ENCAPSULATED>"
                  (prin1-to-string (symbol-function 'f1))))
  (assert (search "FUNCTION F1 ENCAPSULATED>"
                  (prin1-to-string (fdefinition 'f1))))
  (let ((sub (if (typep #'f2 'sb-kernel:interpreted-function)
                 "FUNCTION F1"
                 "FUNCTION F1 {")))
    (dolist (name '(f2 f3))
      (assert (search sub (prin1-to-string (symbol-function name)))
              () "(SYMBOL-FUNCTION ~S) printed as ~S"
              name (prin1-to-string (symbol-function name)))
      (assert (search sub (prin1-to-string (fdefinition name)))
              () "(FDEFINITION ~S) printed as ~S"
              name (prin1-to-string (fdefinition name)))))
  (dolist (name '(f4 f5))
    (assert (search "FUNCTION F1 ENCAPSULATED {"
                    (prin1-to-string (symbol-function name)))
            () "(SYMBOL-FUNCTION ~S) printed as ~S"
            name (prin1-to-string (symbol-function name)))
    (assert (search "FUNCTION F1 ENCAPSULATED {"
                    (prin1-to-string (fdefinition name)))
            () "(FDEFINITION ~S) printed as ~S"
            name (prin1-to-string (fdefinition name)))))

(defvar *foo*)
;; this should keep the tracing encapsulation
(defun f1 () (setq *foo* 'invoked))
(test-util:with-test (:name :no-strip-encap)
  (let ((s (with-output-to-string (*trace-output*)
             (f1))))
    (assert (search "F1 returned INVOKED" s))))


;;;; Same thing for generic functions

(defparameter *count* 0)
(defgeneric g1 ()
  (:method ()
    (incf *count*)))
(defvar *g1/base* #'g1)
(trace g1)
(defvar *g1/traced* #'g1)
(setf (symbol-function 'g2) #'g1)
(setf (fdefinition 'g3) #'g1)
(defun g4 ())
(trace g4)
(defun g5 ())
(trace g5)
(setf (symbol-function 'g4) #'g1)
(setf (fdefinition 'g5) #'g1)

(test-util:with-test (:name (:symbol-function-vs-fdefinition :generic-function))
  (assert (eq *g1/base* *g1/traced*))
  (assert (eq (symbol-function 'g1) (fdefinition 'g1))))

(test-util:with-test (:name (:strip-encap :generic-function))
  (let ((s (with-output-to-string (*trace-output*)
             (g1))))
    (assert (search "G1 returned 1" s)))
  (let ((s (with-output-to-string (*trace-output*)
             (f2))))
    (assert (= (length s) 0)))
  (let ((s (with-output-to-string (*trace-output*)
             (f3))))
    (assert (= (length s) 0)))
  (let ((s (with-output-to-string (*trace-output*)
             (f4))))
    (assert (search "F4 returned 4" s)))
  (let ((s (with-output-to-string (*trace-output*)
             (f5))))
    (assert (search "F5 returned 5" s))))

(defvar *foo*)
;; this should keep the tracing encapsulation
(defgeneric g1 ()
  (:method ()
    (setq *foo* 'invoked)))
(test-util:with-test (:name (:no-strip-encap :generic-function))
  (let ((s (with-output-to-string (*trace-output*)
             (g1))))
    (assert (search "G1 returned INVOKED" s))))
