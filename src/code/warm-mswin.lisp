;;;; Windows API bindings not needed for cold initialization.
(in-package "SB-WIN32")

;;;; CreateProcess and surrounding data structures provide a way to implement
;;;; RUN-PROGRAM while using handles rather than file descriptors.

(define-alien-type process-information
    (struct process-information
      (process-handle handle)
      (thread-handle handle)
      (process-id dword)
      (thread-id dword)))

(define-alien-type startup-info
    (struct startup-info
      (cb dword)
      (reserved1 system-string)
      (desktop system-string)
      (title system-string)
      (x dword)
      (y dword)
      (x-size dword)
      (y-size dword)
      (x-chars dword)
      (y-chars dword)
      (fill-attribute dword)
      (flags dword)
      (show-window unsigned-short)
      (reserved2 unsigned-short)
      (reserved3 (* t))
      (stdin handle)
      (stdout handle)
      (stderr handle)))

(defconstant +startf-use-show-window+ #x001)
(defconstant +startf-use-std-handles+ #x100)

(defconstant +sw-hide+               0)
(defconstant +sw-show-normal+        1)
(defconstant +sw-show-minimized+     2)
(defconstant +sw-show-maximized+     3)
(defconstant +sw-show-no-activate+   4)
(defconstant +sw-show-min-no-active+ 7)
(defconstant +sw-show-na+            8)

(define-alien-routine ("CreateProcessW" create-process) lispbool
  (application-name system-string)
  (command-line system-string)
  (process-security-attributes (* t))
  (thread-security-attributes (* t))
  (inherit-handles-p lispbool)
  (creation-flags dword)
  (environment (* t))
  (current-directory system-string)
  (startup-info (* t))
  (process-information (* t)))

(defun search-path (partial-name)
  "Searh executable using the system path"
  (with-alien ((pathname-buffer pathname-buffer))
    (syscall (("SearchPath" t) dword
              system-string
              system-string
              system-string
              dword
              (* t)
              (* t))
             (and (plusp result)
                  (values (decode-system-string pathname-buffer) result))
             nil partial-name nil
             max_path (cast pathname-buffer (* char)) nil)))

(define-alien-routine ("GetProcessId" get-process-id) dword
  (process handle))

(define-alien-routine ("GetExitCodeProcess" get-exit-code-process) int
  (handle handle) (exit-code dword :out))

(define-alien-routine ("GetExitCodeThread" get-exit-code-thread) int
  (handle handle) (exit-code dword :out))

(define-alien-routine ("TerminateProcess" terminate-process) boolean
  (process handle)
  (exit-code uint))

(defmacro zero-alien (alien type)
  `(alien-funcall (extern-alien "memset" (function void system-area-pointer int unsigned))
                  (alien-sap ,alien) 0 (alien-size ,type :bytes)))

(defun mswin-spawn (program argv stdin stdout stderr searchp envp directory window preserve-handles)
  (let ((std-handles (multiple-value-list (get-std-handles)))
        (inheritp nil))
    (flet ((maybe-std-handle (arg)
             (let ((default (pop std-handles)))
               (case arg (-1 default) (otherwise (setf inheritp t) arg)))))
      (when preserve-handles
        (setf inheritp t)
        (loop for handle in preserve-handles
              do (setf (inheritable-handle-p handle) t)))
      (with-alien ((process-information process-information)
                   (startup-info startup-info))
        (zero-alien startup-info startup-info)
        (setf (slot startup-info 'cb) (alien-size startup-info :bytes)
              (slot startup-info 'stdin) (maybe-std-handle stdin)
              (slot startup-info 'stdout) (maybe-std-handle stdout)
              (slot startup-info 'stderr) (maybe-std-handle stderr)
              (slot startup-info 'reserved1) nil
              (slot startup-info 'reserved2) 0
              (slot startup-info 'reserved3) nil
              (slot startup-info 'show-window) (ecase window
                                                 (:hide +sw-hide+)
                                                 (:show-normal +sw-show-normal+)
                                                 (:show-maximized +sw-show-maximized+)
                                                 (:show-minimized +sw-show-minimized+)
                                                 (:show-no-activate +sw-show-no-activate+)
                                                 (:show-min-no-active +sw-show-min-no-active+)
                                                 (:show-na +sw-show-na+)
                                                 ((nil) 0))
              (slot startup-info 'flags) (logior (if inheritp +startf-use-std-handles+ 0)
                                                 (if window +startf-use-show-window+ 0)))

        (without-interrupts
          ;; KLUDGE: pass null image file name when searchp is true.
          ;; This way, file extension gets resolved by OS if omitted.
          (if (create-process (if searchp nil program)
                              argv
                              nil nil
                              inheritp 0 envp directory
                              (alien-sap startup-info)
                              (alien-sap process-information))
              (let ((child (slot process-information 'process-handle)))
                (close-handle (slot process-information 'thread-handle))
                (values (get-process-id child) child))
              -2))))))

(define-alien-routine ("SetConsoleCtrlHandler" set-console-ctrl-handler) int
  (callback (function (:stdcall int) int))
  (enable int))

(defun windows-console-control-handler (event-code)
  (case event-code
    (0
     (flet ((interrupt-it ()
              (let* ((context
                       (sb-di::nth-interrupt-context
                        (1- sb-kernel:*free-interrupt-context-index*)))
                     (pc (sb-vm:context-pc context)))
                (with-interrupts
                  (let ((int (make-condition
                              'interactive-interrupt
                              :context context
                              :address (sb-sys:sap-int pc))))
                    ;; First SIGNAL, so that handlers can run.
                    (signal int)
                    (%break 'sigint int))))))
       (sb-thread:interrupt-thread (sb-thread::foreground-thread)
                                   #'interrupt-it)
       t))))

(defvar *console-control-handler* #'windows-console-control-handler)
(defvar *console-control-enabled* nil)
(defvar *console-control-spec* nil)

(define-alien-callable alien-console-control-handler (:stdcall int)
    ((event-code int))
  (if (ignore-errors (funcall *console-control-handler* event-code)) 1 0))

(defun console-control-handler ()
  "Get or set windows console control handler.

Boolean value: use default handler (NIL) or ignore event (T).  Symbol
or function: designator for a function that receives an event code and
returns generalized boolean (false to fall back to other handlers,
true to stop searching)." *console-control-spec*)

(defun (setf console-control-handler) (new-handler)
  (etypecase new-handler
    (boolean
     (aver (plusp (set-console-ctrl-handler
                   (sap-alien (int-sap 0)
                              (function (:stdcall int) int))
                   (if new-handler 1 0))))
     (setf *console-control-enabled* nil))
    ((or symbol function)
     (setf *console-control-handler* new-handler)
     (aver (plusp (set-console-ctrl-handler (alien-callable-function 'alien-console-control-handler) 1)))))
  (setf *console-control-spec* new-handler))

(defun initialize-console-control-handler (&optional reset-to-default-p)
  (setf (console-control-handler)
        (if reset-to-default-p
            'windows-console-control-handler
            (console-control-handler))))

(initialize-console-control-handler t)
(pushnew 'initialize-console-control-handler sb-ext:*init-hooks*)

;;;; I/O copying for run-program
;;;; Anonymous pipes do not support overlapped I/O,
;;;; named pipes are to be used instead.

(define-alien-type overlapped
    (struct overlapped
            (internal (* ulong))
            (internal-high  (* ulong))
            (offset dword)
            (offset-high dword)
            (event handle)))

(define-alien-routine ("CreateNamedPipeW" create-named-pipe) handle
  (name system-string)
  (open-mode dword)
  (pipe-mode dword)
  (max-instances dword)
  (out-buffer-size dword)
  (in-buffer-size dword)
  (default-time-out dword)
  (security-attributes (* t)))

(define-alien-routine ("WaitForSingleObject" wait-for-single-object) dword
  (handle handle)
  (timeout dword))

(define-alien-routine ("CreateEventW" create-event) handle
  (security-attributes (* t))
  (manual-reset lispbool)
  (initial-state lispbool)
  (name system-string))

(define-alien-routine ("GetOverlappedResult" get-overlapped-result) lispbool
  (handle handle)
  (overlapped (* t))
  (bytes-transferred dword :out)
  (wait lispbool))

(defun maybe-win32-error (result)
  (when (minusp result)
    (win32-error ""))
  result)

(defglobal **run-program-pipe-counter** 0)
(declaim (type fixnum **run-program-pipe-counter**))

(defun make-named-pipe ()
  (let* ((name (format nil "\\\\.\\pipe\\SBCL-~a-~a"
                       (sb-unix:unix-getpid)
                       (sb-ext:atomic-incf **run-program-pipe-counter**)))
         (pipe (maybe-win32-error
                (create-named-pipe name
                                   (logior pipe-access-inbound file-flag-overlapped)
                                   pipe-type-byte
                                   1 0 0 0 nil))))
    (multiple-value-bind (fd error) (sb-unix:unix-open name sb-unix:o_wronly
                                                       0 :overlapped nil)
      (unless fd
        (win32-error "open" error))
      (values pipe fd))))

(define-alien-routine ("win32_wait_for_multiple_objects_or_signal"
                       wait-for-multiple-objects-or-signal)
    dword
  (handles (* handle))
  (count dword))

(defstruct io-copier
  pipe
  stream
  external-format
  buffer
  event
  overlapped)

(defconstant +copier-buffer+ 256)

(defun setup-copiers (copiers)
  (let ((result (make-array (length copiers))))
    (loop for copier in copiers
          for i from 0
          do
          (let ((overlapped (make-alien overlapped))
                (event (create-event nil t nil nil)))
           (setf (io-copier-event copier) event
                 (io-copier-overlapped copier) overlapped
                 (io-copier-buffer copier) (make-alien char +copier-buffer+)
                 (svref result i) copier)
           (zero-alien overlapped overlapped)
           (setf (slot overlapped 'event) event)))
    result))

(defun free-copier (copier)
  (close-handle (io-copier-pipe copier))
  (when (io-copier-event copier)
    (close-handle (io-copier-event copier)))
  (when (io-copier-overlapped copier)
    (free-alien (io-copier-overlapped copier)))
  (when (io-copier-buffer copier)
    (free-alien (io-copier-buffer copier))))

(defun win32-process-wait (process)
  (let ((handle (sb-impl::process-handle process))
        (copiers (sb-impl::process-copiers process)))
    (when handle
      (cond (copiers
             (unwind-protect
                  (with-alien ((events
                                ;; Should be enough for stdout, stderr, handle,
                                ;; and the signal event
                                (array handle 4)))
                    (let ((copiers (setup-copiers copiers))
                          (count (length copiers))
                          (lisp-buffer (make-array +copier-buffer+ :element-type '(unsigned-byte 8))))
                      (loop for i below count
                         do (setf (deref events i)
                                  (io-copier-event (svref copiers i))))
                      (setf (deref events count) handle)
                      (labels ((pending-or-error (operation
                                                  &optional (error (get-last-error)))
                                 (when (/= error error-io-pending)
                                   (win32-error operation error)))
                               (try-read (copier)
                                 (cond ((plusp
                                         (read-file (io-copier-pipe copier)
                                                    (io-copier-buffer copier)
                                                    +copier-buffer+
                                                    nil
                                                    (io-copier-overlapped copier)))
                                        (copy copier)
                                        (try-read copier))
                                       (t
                                        (let ((last-error (get-last-error)))
                                          (unless (= last-error error-broken-pipe)
                                            (pending-or-error "ReadFile" last-error))))))
                               (copy (copier)
                                 (let* ((stream (io-copier-stream copier))
                                        (element-type (stream-element-type stream)))
                                   (multiple-value-bind (finished count)
                                       (get-overlapped-result (io-copier-pipe copier)
                                                              (io-copier-overlapped copier) nil)
                                     (cond (finished
                                            (loop for i below count
                                               do (setf (aref lisp-buffer i)
                                                        (deref (io-copier-buffer copier) i)))
                                            (cond
                                              ((member element-type '(base-char character))
                                               (write-string
                                                (octets-to-string lisp-buffer
                                                                  :end count
                                                                  :external-format
                                                                  (io-copier-external-format copier))
                                                stream))
                                              (t
                                               (handler-bind
                                                   ((type-error
                                                     (lambda (c)
                                                       (error 'simple-type-error
                                                              :format-control
                                                              "Error using ~s for program output:~@
                                                             ~a"
                                                              :format-arguments
                                                              (list stream c)
                                                              :expected-type
                                                              (type-error-expected-type c)
                                                              :datum
                                                              (type-error-datum c)))))
                                                 (write-sequence lisp-buffer stream :end count)))))
                                           (t
                                            (let ((last-error (get-last-error)))
                                              (unless (= last-error error-broken-pipe)
                                                (pending-or-error "GetOverlappedResult" last-error)))))))))
                        (loop for copier across copiers
                           do (try-read copier))
                        (loop for event = (wait-for-multiple-objects-or-signal (cast events
                                                                                     (* handle))
                                                                               (1+ count))
                           do
                             (cond ((= event wait-timeout))
                                   ((< event count)
                                    (let ((copier (svref copiers event)))
                                      (copy copier)
                                      (try-read copier)))
                                   ((= event count) ;; HANDLE event
                                    (return))
                                   ((= event wait-failed)
                                    (win32-error "WaitForMultipleObjects")))))))
               (mapc #'free-copier copiers)))
            (t
             (do ()
                 ((= 0
                     (wait-object-or-signal handle))))))
      (sb-impl::get-processes-status-changes)))
  process)
