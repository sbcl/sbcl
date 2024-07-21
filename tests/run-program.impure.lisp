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

(defun bin-pwd-ignoring-result ()
  (let ((initially-open-fds (directory "/proc/self/fd/*" :resolve-symlinks nil)))
    (sb-ext:run-program "pwd" nil :search t :input :stream :output :stream :wait nil)
    (length initially-open-fds)))

(with-test (:name (run-program :autoclose-streams)
            :broken-on :sbcl  ;; not reliable enough
            :skipped-on (not :linux))
  (let ((n-initially-open-fds (bin-pwd-ignoring-result)))
    (gc)
    (sb-sys:scrub-control-stack) ; Make sure we're not referencing the #<process>
    (gc) ; now nothing should reference the streams
    (assert (= (length (directory "/proc/self/fd/*" :resolve-symlinks nil))
               n-initially-open-fds))))

;; In addition to definitions lower down the impurity we're avoiding
;; is the sigchld handler that RUN-PROGRAM sets up, which interfers
;; with the manual unix process control done by the test framework
;; (sometimes the handler will manage to WAIT3 a process before
;; run-tests WAITPIDs it).

(with-test (:name (run-program :cat 1))
  (let* ((process (run-program "cat" '() :wait nil
                               :search t :output :stream :input :stream))
         (out (process-input process))
         (in (process-output process)))
    (unwind-protect
         (loop for i from 0 to 255 do
              (write-byte i out)
              (force-output out)
              (assert (= (read-byte in) i)))
      (process-close process))))

(with-test (:name (run-program :cat 2)
                  :skipped-on (or (not :sb-thread) :win32))
  ;; Tests that reading from a FIFO is interruptible.
  (let* ((process (run-program "cat" '() :search t
                               :wait nil :output :stream :input :stream))
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

(with-test (:name (run-program :cat 3))
  ;; User-defined binary input and output streams.
  (let ((in (make-instance 'buffer-stream))
        (out (make-instance 'buffer-stream))
        (data #(0 1 2 3 4 5 6 7 8 9 10 11 12)))
    (write-sequence data in)
    (let ((process (run-program "cat" '()
                                :search t
                                :wait t
                                :output out :input in))
          (buf (make-array (length data))))
      (declare (ignore process))
      (assert (= 13 (read-sequence buf out)))
      (assert (= 0 (read-sequence (make-array 8) out)))
      (assert (equalp buf data)))))

(with-test (:name (run-program :cat 4))
  ;; Null broadcast stream as output
  (let* ((process (run-program "cat" '() :wait nil
                               :search t
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

#-win32
(progn
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
  (defparameter *cat-out* (make-synonym-stream '*cat-out-pipe*)))

(with-test (:name (run-program :cat 5) :fails-on :win32)
  (let ((cat (run-program "cat" nil :search t :input *cat-in* :output *cat-out*
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

#-win32
(unless (probe-file "/bin/ed") (push :no-bin-ed-installed *features*))

#-win32
(progn
  (defparameter *tmpfile* (scratch-file-name))

  (with-test (:name (run-program :/bin/ed) :skipped-on :no-bin-ed-installed)
    (with-open-file (f *tmpfile*
                       :direction :output
                       :if-exists :supersede)
      (write-line "bar" f))
    (unwind-protect
         (let* ((ed (run-program "/bin/ed" (list *tmpfile*) :wait nil :pty t))
                (ed-in (process-pty ed))
                (ed-out (process-pty ed)))
           (labels ((read-linish (stream)
                      (with-output-to-string (s)
                        (loop for c = (read-char stream)
                              while (and c (not (eq #\newline c)))
                              ;; Some eds like to send \r\n
                              do (unless (eq #\return c)
                                   (write-char c s)))))
                    (assert-ed (command response)
                      (when command
                        (write-line command ed-in)
                        (force-output ed-in))
                      (when response
                        (let ((got (read-linish ed-out)))
                          (unless (equal response got)
                            (error "wanted '~A' from ed, got '~A'" response got))))
                      ed))
             (assert-ed nil "4")
             (assert-ed ".s/bar/baz/g" nil)
             (assert-ed "w" "4")
             (assert-ed "q" nil)
             (process-wait ed)
             (with-open-file (f *tmpfile*)
               (assert (equal "baz" (read-line f))))))
      (delete-file *tmpfile*)))) ;; #-win32

;; Around 1.0.12 there was a regression when :INPUT or :OUTPUT was a
;; pathname designator.  Since these use the same code, it should
;; suffice to test just :INPUT.
(with-test (:name (run-program :input :output pathname))
  (with-scratch-file (file)
    (with-open-file (f file :direction :output)
      (setf file (truename file))
      (write-line "Foo" f))
    (assert (run-program "cat" ()
                         :input file :output t
                         :search t :wait t))))

;;; This used to crash on Darwin and trigger recursive lock errors on
;;; every platform.
;;; ...and now it triggers recursive lock errors on safepoint + linux.
(with-test (:name (run-program :stress) :skipped-on (:and :sb-safepoint :linux))
  ;; Do it a hundred times in batches of 10 so that with a low limit
  ;; of the number of processes the test can have a chance to pass.
  ;;
  ;; If #+sb-thread, then make this test even more brutal by calling
  ;; RUN-PROGRAM in new threads. This is neither good nor bad as far as
  ;; total run time, but good in that it excercises RUN-PROGRAM
  ;; from other than the main thread.
  (flet ((start-run ()
           (run-program "echo"
                        '("It would be nice if this didn't crash.")
                        :search t :wait nil :output nil)))
    (dotimes (i 10)
      (mapc #'process-wait
            #+sb-thread (mapcar #'sb-thread:join-thread
                                (loop repeat 10
                                      collect (sb-thread:make-thread #'start-run)))
            #-sb-thread (loop repeat 10 collect (start-run))))))

(with-test (:name (run-program :pty-stream)
            :fails-on :win32
            :broken-on :darwin)
  (let (process
        stream)
    (assert (search "OK"
                   (handler-bind
                       ((timeout (lambda (c)
                                   c
                                   (format t "~a ~a~%" process
                                           (when stream
                                             (get-output-stream-string stream))))))
                     (with-timeout 60
                       (with-output-to-string (s)
                         (setf stream s)
                         (setf process
                               (run-program "/bin/sh" '("-c" "echo OK; exit 42") :pty s
                                                                                 :wait nil))
                         (process-wait process)
                         (assert (= (process-exit-code process) 42))
                         s)))))))

;; Check whether RUN-PROGRAM puts its child process into the foreground
;; when stdin is inherited. If it fails to do so we will receive a SIGTTIN.
;;
;; We can't check for the signal itself since run-program.c resets the
;; forked process' signal mask to defaults. But the default is `stop'
;; of which we can be notified asynchronously by providing a status hook.
(with-test (:name (run-program :inherit-stdin) :fails-on :win32
                  :skipped-on :no-bin-ed-installed)
  (let (stopped)
    (flet ((status-hook (proc)
             (case (process-status proc)
               (:stopped (setf stopped t)))))
      (let ((proc (run-program "/bin/ed" nil :search nil :wait nil
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
(with-test (:name (run-program :clean-exit-after-encoding-error)
                  :fails-on :win32)
  (let ((had-error-p nil))
    (flet ((barf (&optional (format :default))
             (with-output-to-string (stream)
               (run-program "perl"
                            '("-e" "print \"\\x20\\xfe\\xff\\x0a\"")
                            :search t
                            :output stream
                            :external-format format)))
           (no-barf ()
             (with-output-to-string (stream)
               (run-program "echo"
                            '("This is a test")
                            :search t
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

(with-test (:name (run-program :no-such-thing))
  (assert (search "Couldn't execute"
                  (handler-case
                      (progn (run-program "no-such-program-we-hope" '()) nil)
                    (error (e)
                      (princ-to-string e))))))

(with-test (:name (run-program :not-executable))
  (assert (search "Couldn't execute"
                  (handler-case
                      (progn (run-program "run-program.impure.lisp" '()) nil)
                    (error (e)
                      (princ-to-string e))))))

#-win32
(with-test (:name (run-program :if-input-does-not-exist))
  (let ((file (pathname (sb-posix:mktemp "rpXXXXXX"))))
    (when (and (boundp 'run-tests::*allowed-inputs*)
               ;; If the permitted inputs are :ANY then leave it be
               (listp (symbol-value 'run-tests::*allowed-inputs*)))
      (push (namestring file) (symbol-value 'run-tests::*allowed-inputs*)))
    (assert (null (run-program "cat" '() :search t :input file)))
    (assert (null (run-program "cat" '() :search t :output #.(or *compile-file-truename*
                                                                 *load-truename*)
                                      :if-output-exists nil)))))


(with-test (:name (run-program :set-directory))
  (let* ((directory #-win32 "/"
                    #+win32 "c:\\")
         (out (process-output
               (run-program #-win32 "/bin/sh"
                            #-win32 '("-c" "pwd")
                            #+win32 "cmd.exe"
                            #+win32 '("/c" "cd")
                            :output :stream
                            :directory directory
                            :search t))))
    (assert
     (equal directory
            (string-right-trim '(#\Return) (read-line out))))))

(with-test (:name (run-program :directory-nil))
  (run-program #-win32 "/bin/sh"
               #-win32 '("-c" "pwd")
               #+win32 "cmd.exe"
               #+win32 '("/c" "cd")
               :directory nil
               :search t))

(with-test (:name (run-program :bad-options))
  (assert-error
   (run-program #-win32 "/bin/sh"
                #-win32 '("-c" "pwd")
                #+win32 "cmd.exe"
                #+win32 '("/c" "cd")
                :search t
                :output :bad)))

(with-test (:name (run-program :stop+continue) :skipped-on :win32)
  (let ((process (run-program "cat" '() :search t :input :stream :wait nil)))
    (flet ((kill-and-check-status (signal expected-status)
             (process-kill process signal)
             (loop :repeat 2000
                :when (eq (process-status process) expected-status)
                :do (return)
                :do (sleep 1/100)
                :finally (error "~@<Process ~A did not change its ~
                                 status to ~S within 20 seconds.~@:>"
                                process expected-status))))
      (kill-and-check-status sb-posix:sigstop :stopped)
      (kill-and-check-status sb-posix:sigcont :running)
      (kill-and-check-status sb-posix:sigkill :signaled)
      (process-wait process)
      (assert (not (process-alive-p process))))))

#-win32
(with-test (:name (run-program :stop+continue :posix-kill))
  (let ((process (run-program "cat" '() :search t :input :stream :wait nil)))
    (flet ((kill-and-check-status (signal expected-status)
             (sb-posix:kill (process-pid process) signal)
             (loop :repeat 2000
                :when (eq (process-status process) expected-status)
                :do (return)
                :do (sleep 1/100)
                :finally (error "~@<Process ~A did not change its ~
                                 status to ~S within 20 seconds.~@:>"
                                process expected-status))))
      (kill-and-check-status sb-posix:sigstop :stopped)
      (kill-and-check-status sb-posix:sigcont :running)
      (kill-and-check-status sb-posix:sigkill :signaled)
      (process-wait process)
      (assert (not (process-alive-p process))))))

(defun malloc ()
  (let ((x (sb-alien:make-alien int 10000)))
    (sb-alien:free-alien x)
    1))

(with-test (:name (run-program :malloc-deadlock)
            :broken-on :sb-safepoint
            :skipped-on (or :ubsan (not :sb-thread) :win32))
  (let* (stop
         (delay-between-gc
          (or #+freebsd
              (let* ((p (run-program "sysctl" '("hw.model") :search t :output :stream))
                     (output (process-output p))
                     (result (read-line output)))
                (close output)
                ;; With the default delay of 0 using FreeBSD on QEMU this test never
                ;; finished because the RUN-PROGRAM thread would never get scheduled
                ;; after its first entry into the loop.
                ;; It wasn't hung, it just wasn't getting CPU time. For me anyway.
                (when (search "QEMU" result)
                  .00000001)) ; 10 nanoseconds
              #+(and darwin arm64)
               0.01
              0))
         (threads (list*
                   (sb-thread:make-thread (lambda ()
                                            (loop until (progn (sb-thread:barrier (:read))
                                                               stop)
                                                  do
                                                  (sleep delay-between-gc)
                                                  (gc :full t))))
                   (loop repeat 3
                         collect
                         (sb-thread:make-thread
                          (lambda ()
                            (loop until (progn (sb-thread:barrier (:read))
                                               stop)
                                  do (malloc))))))))
    (loop with dot = (get-internal-real-time)
          with end = (+ dot (* internal-time-units-per-second 4))
          for time = (get-internal-real-time)
          while (> end time)
          do
          (when (> (- time dot)
                   (/ internal-time-units-per-second 10))
            (setf dot time)
            (write-char #\.)
            (finish-output))
          (with-output-to-string (str)
            (run-program "uname" ()
                         :output str
                         :search t
                         :wait t)))
    (setf stop t)
    (sb-thread:barrier (:write))
    (mapc #'sb-thread:join-thread threads)))

(with-test (:name (run-program :child-fd-leak)
            :skipped-on (or :openbsd :win32))
  (when (probe-file "/dev/fd")
    (with-open-file (stream "/dev/null")
      (let* ((fd (sb-sys:fd-stream-fd stream))
             (process (run-program "test" (list "-e" (format nil "/dev/fd/~a" fd))
                                   :search t)))
        (assert (not (zerop (process-exit-code process))))))))
