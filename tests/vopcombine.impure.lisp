
;;; MACHINE-VERSION did the wrong thing after rev 1a0c5f225a
;;; but it will cease to be a good test because SB-SYS:*MACHINE-VERSION*
;;; should be a defglobal, but we don't allows defglobals (not thread-locally
;;; bindable) variables to have unbound-marker as their value, so
;;; I don't know what I'll change. But regardless there was another
;;; inefficiency in that SB-SYS:*MACHINE-VERSION* is allowed to be a non-simple
;;; string, while fndb says that #'MACHINE-VERSION returns a SIMPLE-STRING.
;;; After I fix that discrepancy there should be no type-check at all
;;; in the quick case of (MACHINE-VERSION). So this test is a self-contained
;;; replica of what the broken code did, whether or not I change that.
(defvar *some-string*)
(defun compute-the-string ()
  (format nil "~A a ~A" "here's" "string"))

(declaim (ftype (function () string) cached-get-a-string))
(defun cached-get-a-string ()
  (unless (boundp '*some-string*)
    (setf *some-string* (compute-the-string)))
  *some-string*)
(compile 'cached-get-a-string)

(with-test (:name :boundp+symbol-value-labeled-blocks)
  (assert (string= (cached-get-a-string) "here's a string")))
