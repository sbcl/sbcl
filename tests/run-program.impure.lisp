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

(with-test (:name :run-program-cat-2 :skipped-on '(not :sb-thread))
  ;; Tests that reading from a FIFO is interruptible.
  (let* ((process (sb-ext:run-program "/bin/cat" '()
                                      :wait nil
                                      :output :stream :input :stream))
         (in (process-input process))
         (out (process-output process))
         (sem (sb-thread:make-semaphore))
         (state :init)
         (writer (sb-thread:make-thread (lambda ()
                                          (sb-thread:wait-on-semaphore sem)
                                          (setf state :sleep)
                                          (sleep 2)
                                          (setf state :write)
                                          (write-line "OK" in)
                                          (finish-output in))))
         (timeout nil)
         (got nil)
         (unwind nil))
    (sb-thread:signal-semaphore sem)
    (handler-case
        (with-timeout 0.1
          (unwind-protect
               (setf got (read-line out))
            (setf unwind state)))
      (timeout ()
        (setf timeout t)))
    (assert (not got))
    (assert timeout)
    (assert (eq unwind :sleep))
    (sb-thread:join-thread writer)
    (assert (equal "OK" (read-line out)))))

(defclass buffer-stream (sb-gray:fundamental-binary-input-stream sb-gray:fundamental-binary-output-stream)
  ((buffer :initform (make-array 128
                                :element-type '(unsigned-byte 8)
                                :adjustable t
                                :fill-pointer 0))
   (mark :initform 0)))

(defmethod stream-element-type ((stream buffer-stream))
  '(unsigned-byte 8))

(defmethod sb-gray:stream-read-sequence ((stream buffer-stream) seq &optional (start 0) end)
  (let* ((buffer (slot-value stream 'buffer))
         (end (or end (length seq)))
         (mark (slot-value stream 'mark))
         (fill-pointer (fill-pointer buffer))
         (new-mark (+ mark (min fill-pointer (- end start)))))
    (setf (slot-value stream 'mark) new-mark)
    (replace seq buffer
             :start1 start :end1 end
             :start2 mark :end2 fill-pointer)
    (min end (+ start (- fill-pointer mark)))))

(defmethod sb-gray:stream-write-sequence ((stream buffer-stream) seq &optional (start 0) end)
  (let* ((buffer (slot-value stream 'buffer))
         (end (or end (length seq)))
         (fill-pointer (fill-pointer buffer))
         (new-fill (min (array-total-size buffer) (+ fill-pointer (- end start)))))
    (setf (fill-pointer buffer) new-fill)
    (replace buffer seq
             :start1 fill-pointer
             :start2 start :end2 end)
    seq))

(with-test (:name :run-program-cat-3)
  ;; User-defined binary input and output streams.
  (let ((in (make-instance 'buffer-stream))
        (out (make-instance 'buffer-stream))
        (data #(0 1 2 3 4 5 6 7 8 9 10 11 12)))
    (write-sequence data in)
    (let ((process (sb-ext:run-program "/bin/cat" '() :wait t :output out :input in))
          (buf (make-array (length data))))
      (assert (= 13 (read-sequence buf out)))
      (assert (= 0 (read-sequence (make-array 8) out)))
      (assert (equalp buf data)))))

(with-test (:name :run-program-cat-4)
  ;; Null broadcast stream as output
  (let* ((process (sb-ext:run-program "/bin/cat" '() :wait nil
                                      :output (make-broadcast-stream)
                                      :input :stream))
         (in (process-input process)))
    (unwind-protect
         (progn
           (write-string "foobar" in)
           (close in)
           (process-wait process))
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

(with-test (:name :run-program-cat-5)
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
          while (and c (not (eq #\newline c)))
             ;; Some eds like to send \r\n
          do (unless (eq #\return c)
               (write-char c s)))))

(defun assert-ed (command response)
  (when command
    (write-line command *ed-in*)
    (force-output *ed-in*))
  (when response
    (let ((got (read-linish *ed-out*)))
      (unless (equal response got)
        (error "wanted '~A' from ed, got '~A'" response got))))
  *ed*)

(unwind-protect
     (with-test (:name :run-program-ed)
       (assert-ed nil "4")
       (assert-ed ".s/bar/baz/g" nil)
       (assert-ed "w" "4")
       (assert-ed "q" nil)
       (process-wait *ed*)
       (with-open-file (f *tmpfile*)
         (assert (equal "baz" (read-line f)))))
  (delete-file *tmpfile*))

;; Around 1.0.12 there was a regression when :INPUT or :OUTPUT was a
;; pathname designator.  Since these use the same code, it should
;; suffice to test just :INPUT.
(let ((file))
  (unwind-protect
       (progn (with-open-file (f "run-program-test.tmp" :direction :output)
                (setf file (truename f))
                (write-line "Foo" f))
                  (assert (run-program "cat" ()
                                       :input file :output t
                                       :search t :wait t)))
    (when file
      (delete-file file))))

;;; This used to crash on Darwin and trigger recursive lock errors on
;;; every platform.
(with-test (:name (:run-program :stress))
  ;; Do it a hundred times in batches of 10 so that with a low limit
  ;; of the number of processes the test can have a chance to pass.
  (loop
   repeat 10 do
   (map nil
        #'sb-ext:process-wait
        (loop repeat 10
              collect
              (sb-ext:run-program "/bin/echo" '
                                  ("It would be nice if this didn't crash.")
                                  :wait nil :output nil)))))

(with-test (:name (:run-program :pty-stream))
  (assert (equal "OK"
                 (subseq
                  (with-output-to-string (s)
                    (assert (= 42 (process-exit-code
                                   (run-program "/bin/sh" '("-c" "echo OK; exit 42") :wait t
                                                :pty s))))
                    s)
                  0
                  2))))

;; Check whether RUN-PROGRAM puts its child process into the foreground
;; when stdin is inherited. If it fails to do so we will receive a SIGTTIN.
;;
;; We can't check for the signal itself since run-program.c resets the
;; forked process' signal mask to defaults. But the default is `stop'
;; of which we can be notified asynchronously by providing a status hook.
(with-test (:name (:run-program :inherit-stdin))
  (let (stopped)
    (flet ((status-hook (proc)
             (case (sb-ext:process-status proc)
               (:stopped (setf stopped t)))))
      (let ((proc (sb-ext:run-program "/bin/ed" nil :search nil :wait nil
                                      :input t :output t
                                      :status-hook #'status-hook)))
        ;; Give the program a generous time to generate the SIGTTIN.
        ;; If it hasn't done so after that time we can consider it
        ;; to be working (i.e. waiting for input without generating SIGTTIN).
        (sleep 0.5)
        ;; either way we have to signal it to terminate
        (process-kill proc sb-posix:sigterm)
        (process-close proc)
        (assert (not stopped))))))


;; Check that in when you do run-program with :wait t that causes
;; encoding error, it does not affect the following run-program
(with-test (:name (:run-program :clean-exit-after-encoding-error))
  (let ((had-error-p nil))
    (flet ((barf (&optional (format :default))
             (with-output-to-string (stream)
               (run-program "/usr/bin/perl"
                            '("-e" "print \"\\x20\\xfe\\xff\\x0a\"")
                            :output stream
                            :external-format format)))
           (no-barf ()
             (with-output-to-string (stream)
               (run-program "/bin/echo"
                            '("This is a test")
                            :output stream))))
      (handler-case
          (barf :utf-8)
        (error ()
          (setq had-error-p t)))
      (assert had-error-p)
      ;; now run the harmless program
      (setq had-error-p nil)
      (handler-case
          (no-barf)
        (error ()
          (setq had-error-p t)))
      (assert (not had-error-p)))))

