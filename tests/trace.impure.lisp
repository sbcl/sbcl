
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
(trace f1)
(setf (symbol-function 'f2) #'f1)
(setf (fdefinition 'f3) #'f1)

(test-util:with-test (:name :strip-encap)
  (let ((s (with-output-to-string (*trace-output*)
             (f1))))
    (assert (search "F1 returned 1" s)))
  (let ((s (with-output-to-string (*trace-output*)
             (f2))))
    (assert (= (length s) 0)))
  (let ((s (with-output-to-string (*trace-output*)
             (f3))))
    (assert (= (length s) 0))))

(defvar *foo*)
;; this should keep the tracing encapsulation
(defun f1 () (setq *foo* 'invoked))
(test-util:with-test (:name :no-strip-encap)
  (let ((s (with-output-to-string (*trace-output*)
             (f1))))
    (assert (search "F1 returned INVOKED" s))))
