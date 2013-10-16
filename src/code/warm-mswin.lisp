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

(defconstant +startf-use-std-handles+ #x100)

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

(define-alien-routine ("GetExitCodeProcess" get-exit-code-process) int
  (handle handle) (exit-code dword :out))

(define-alien-routine ("GetExitCodeThread" get-exit-code-thread) int
  (handle handle) (exit-code dword :out))

(defun mswin-spawn (program argv stdin stdout stderr searchp envp waitp
                    directory)
  (let ((std-handles (multiple-value-list (get-std-handles)))
        (inheritp nil))
    (flet ((maybe-std-handle (arg)
             (let ((default (pop std-handles)))
               (case arg (-1 default) (otherwise (setf inheritp t) arg)))))
      (with-alien ((process-information process-information)
                   (startup-info startup-info))
        (sb-kernel:system-area-ub8-fill
         0 (alien-sap startup-info)
         0 (alien-size startup-info :bytes))
        (setf (slot startup-info 'cb) (alien-size startup-info :bytes)
              (slot startup-info 'stdin) (maybe-std-handle stdin)
              (slot startup-info 'stdout) (maybe-std-handle stdout)
              (slot startup-info 'stderr) (maybe-std-handle stderr)
              (slot startup-info 'reserved1) nil
              (slot startup-info 'reserved2) 0
              (slot startup-info 'reserved3) nil
              (slot startup-info 'flags) (if inheritp +startf-use-std-handles+ 0))
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
                (if waitp
                    (do () ((/= 1 (with-local-interrupts (wait-object-or-signal child)))
                            (multiple-value-bind (got code) (get-exit-code-process child)
                              (if got code -1))))
                    child))
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

(sb-alien::define-alien-callback *alien-console-control-handler* (:stdcall int)
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
     (aver (plusp (set-console-ctrl-handler *alien-console-control-handler* 1)))))
  (setf *console-control-spec* new-handler))

(defun initialize-console-control-handler (&optional reset-to-default-p)
  (setf (console-control-handler)
        (if reset-to-default-p
            'windows-console-control-handler
            (console-control-handler))))

(initialize-console-control-handler t)
(pushnew 'initialize-console-control-handler sb-ext:*init-hooks*)
