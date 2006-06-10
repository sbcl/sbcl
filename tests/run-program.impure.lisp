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

;; Actually there's no real side-effect here. (But see below.) The
;; impurity we're avoiding is the sigchld handler that RUN-PROGRAM
;; sets up, which interfers with the manual unix process control done
;; by the test framework (sometimes the handler will manage to WAIT3 a
;; process before run-tests WAITPIDs it).

(let* ((process (sb-ext:run-program "/bin/cat" '() :wait nil
                                    :output :stream :input :stream))
       (out (process-input process))
       (in (process-output process)))
  (unwind-protect
       (loop for i from 0 to 255 do
             (write-byte i out)
             (force-output out)
             (assert (= (read-byte in) i)))
    (process-close process)))

;;; Test driving an external program (ed) through pipes wrapped in
;;; composite streams.

(require :sb-posix)

(defvar *tmpfile* "run-program-ed-test.tmp")

(with-open-file (f *tmpfile*
                   :direction :output
                   :if-exists :supersede)
  (write-line "bar" f))

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

(defvar *in-pipe* (make-pipe))
(defvar *in* (make-synonym-stream '*in-pipe*))
(defvar *out-pipe* (make-pipe))
(defvar *out* (make-synonym-stream '*out-pipe*))

(defvar *ed*
  (run-program "/bin/ed" (list *tmpfile*) :input *in* :output *out* :wait nil))

(defun real-input (stream)
  (two-way-stream-input-stream (symbol-value (synonym-stream-symbol stream))))

(defun magic-read-line (stream)
  ;; KLUDGE 1: The otherwise out :buffering :none is worth nothing,
  (let ((input (real-input stream)))
    (with-output-to-string (s)
      ;; KLUDGE 2: Something funny going on with buffering, as plain
      ;; READ-CHAR will hang here waiting for the newline, also
      ;; -NO-HANG will return too early without the sleep.
      ;;
      ;; Shoot me now. --NS 2006-06-09
      (loop for c = (progn (sleep 0.2) (read-char-no-hang input))
         while (and c (not (eq #\newline c)))
         do (write-char c s)))))

(defun assert-ed (command response)
  (when command
    (write-line command *in*)
    (force-output *in*))
  (let ((got (magic-read-line *out*)))
    (unless (equal response got)
      (error "wanted ~S from ed, got ~S" response got)))
  *ed*)

(assert-ed nil "4")
(assert-ed ".s/bar/baz/g" "")
(assert-ed "w" "4")
(assert-ed "q" "")
(process-wait *ed*)

(with-open-file (f *tmpfile*)
  (assert (equal "baz" (read-line f))))

;(delete-file *tmp*)
