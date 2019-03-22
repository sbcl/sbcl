;;;; tests related to Lisp streams

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

(in-package :cl-user)

;;; The unread and clear-input functions on input streams need to
;;; sneak past the old CMU CL encapsulation. As explained by DTC in
;;; the checkin message for his CMU CL patch ca. April 2001,
;;;   These streams encapsulate other input streams which may
;;;   have an input buffer so they need to call unread-char
;;;   and clear-input on the encapsulated stream rather than
;;;   directly calling the encapsulated streams misc method
;;;   as the misc methods are below the layer of the input buffer.
;;;
;;; The code below tests only UNREAD-CHAR. It would be nice to test
;;; CLEAR-INPUT too, but I'm not sure how to do it cleanly and
;;; portably in a noninteractive test. -- WHN 2001-05-05
(defparameter *scratch-file-name* (scratch-file-name))
(defvar *scratch-file-stream*)
(dolist (scratch-file-length '(1 ; everyone's favorite corner case
                               200123)) ; hopefully much bigger than buffer
  ; (format t "/SCRATCH-FILE-LENGTH=~W~%" scratch-file-length)
  (with-open-file (s *scratch-file-name* :direction :output)
    (dotimes (i scratch-file-length)
      (write-char #\x s)))
  (dolist (wrap-named-stream-fn
           ;; All kinds of wrapped input streams have the same issue.
           (list (lambda (wrapped-stream-name)
                   (make-synonym-stream wrapped-stream-name))
                 (lambda (wrapped-stream-name)
                   (make-two-way-stream (symbol-value wrapped-stream-name)
                                        *standard-output*))
                 (lambda (wrapped-stream-name)
                   (make-concatenated-stream (symbol-value wrapped-stream-name)
                                             (make-string-input-stream "")))))
    ; (format t "/WRAP-NAMED-STREAM-FN=~S~%" wrap-named-stream-fn)
    (with-open-file (*scratch-file-stream* *scratch-file-name*
                                           :direction :input)
      (let ((ss (funcall wrap-named-stream-fn '*scratch-file-stream*)))
        (flet ((expect (thing-expected)
                 (let ((thing-found (read-char ss nil nil)))
                   (unless (eql thing-found thing-expected)
                     (error "expected ~S, found ~S"
                            thing-expected thing-found)))))
          (dotimes (i scratch-file-length)
            (expect #\x)
            (unread-char #\y ss)
            (expect #\y)
            (unread-char #\z ss)
            (expect #\z))
          (expect nil))))) ; i.e. end of file
  (delete-file *scratch-file-name*))

(with-open-file (s *scratch-file-name* :direction :output)
  (format s "1234~%"))
(assert
 (string=
  (with-open-file (s *scratch-file-name* :direction :input)
    (let* ((b (make-string 10)))
      (peek-char nil s)
      (read-sequence b s)
      b))
  (format nil "1234")
  :end1 4))
(delete-file *scratch-file-name*)
