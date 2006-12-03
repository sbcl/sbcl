;;;; various RUN-PROGRAM tests with side effects

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

(cl:in-package :cl-user)

;; In addition to definitions lower down the impurity we're avoiding
;; is the sigchld handler that RUN-PROGRAM sets up, which interfers
;; with the manual unix process control done by the test framework
;; (sometimes the handler will manage to WAIT3 a process before
;; run-tests WAITPIDs it).

(with-test (:name :run-program-cat-1)
  (let* ((process (sb-ext:run-program "/bin/cat" '() :wait nil
                                      :output :stream :input :stream))
         (out (process-input process))
         (in (process-output process)))
    (unwind-protect
         (loop for i from 0 to 255 do
              (write-byte i out)
              (force-output out)
              (assert (= (read-byte in) i)))
      (process-close process))))

;;; Test driving an external program (cat) through pipes wrapped in
;;; composite streams.

(require :sb-posix)

(defun make-pipe ()
  (multiple-value-bind (in out) (sb-posix:pipe)
    (let ((input (sb-sys:make-fd-stream in
                                        :input t
                                        :external-format :ascii
                                        :buffering :none :name "in"))
          (output (sb-sys:make-fd-stream out
                                         :output t
                                         :external-format :ascii
                                         :buffering :none :name "out")))
      (make-two-way-stream input output))))

(defparameter *cat-in-pipe* (make-pipe))
(defparameter *cat-in* (make-synonym-stream '*cat-in-pipe*))
(defparameter *cat-out-pipe* (make-pipe))
(defparameter *cat-out* (make-synonym-stream '*cat-out-pipe*))

(with-test (:name :run-program-cat-2)
  (let ((cat (run-program "/bin/cat" nil :input *cat-in* :output *cat-out*
                          :wait nil)))
    (dolist (test '("This is a test!"
                    "This is another test!"
                    "This is the last test...."))
      (write-line test *cat-in*)
      (assert (equal test (read-line *cat-out*))))
    (process-close cat)))

;;; The above test used to use ed, but there were buffering issues: on some platforms
;;; buffering of stdin and stdout depends on their TTYness, and ed isn't sufficiently
;;; agressive about flushing them. So, here's another test using :PTY.

(defparameter *tmpfile* "run-program-ed-test.tmp")

(with-open-file (f *tmpfile*
                   :direction :output
                   :if-exists :supersede)
  (write-line "bar" f))

(defparameter *ed*
  (run-program "/bin/ed" (list *tmpfile*) :wait nil :pty t))

(defparameter *ed-pipe* (make-two-way-stream (process-pty *ed*) (process-pty *ed*)))
(defparameter *ed-in* (make-synonym-stream '*ed-pipe*))
(defparameter *ed-out* (make-synonym-stream '*ed-pipe*))

(defun read-linish (stream)
  (with-output-to-string (s)
    (loop for c = (read-char stream)
       while (and c (not (eq #\newline c)) (not (eq #\return c)))
       do (write-char c s))))

(defun assert-ed (command response)
  (when command
    (write-line command *ed-in*)
    (force-output *ed-in*))
  (let ((got (read-linish *ed-out*)))
    (unless (equal response got)
      (error "wanted ~S from ed, got ~S" response got)))
  *ed*)

(unwind-protect
     (with-test (:name :run-program-ed)
       (assert-ed nil "4")
       (assert-ed ".s/bar/baz/g" "")
       (assert-ed "w" "4")
       (assert-ed "q" "")
       (process-wait *ed*)
       (with-open-file (f *tmpfile*)
         (assert (equal "baz" (read-line f)))))
  (delete-file *tmpfile*))
