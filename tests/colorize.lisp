(defvar *no-color*
  (member "--no-color" *posix-argv* :test #'equal))

(defvar *color-error* nil)

(if *no-color*
  (defun %output-colored-text (text color &key bold)
    (declare (ignore text color bold)))
  (let ((file #-win32 "colorize-control-codes.lisp"
              #+win32 "colorize-windows-console.lisp"))
    (handler-case (load file)
      (error (c)
        (setf *color-error*
              (format nil "Error while loading ~a:~% ~a"
                      (enough-namestring file)
                      c))))))

(defun is-tty ()
  (let* ((stream (sb-impl::stream-output-stream  *standard-output*))
         (fd (and (sb-sys:fd-stream-p stream)
                  (sb-sys:fd-stream-fd stream))))
    (when (integerp fd)
      (plusp (sb-unix:unix-isatty fd)))))

(defun present-coloring-error (error)
  (format t "~a~%" error)
  (format t "Switching off colored output,~%~
                    it can be turned off by passing --no-color~%~%")
  (setf *no-color* t))

(defun output-colored-text (kind text
                            &key (align 20))
  (cond ((or (not (is-tty))
             *no-color*)
         (write-string text))
        (*color-error*
         (present-coloring-error *color-error*)
         (write-string text))
        (t
         (handler-case
             (case kind
               ((:unexpected-failure
                 :leftover-thread
                 :unhandled-error
                 :invalid-exit-status)
                (%output-colored-text text :red :bold t))
               ((:unexpected-success)
                (%output-colored-text text :green))
               (t
                (write-string text)))
           (error (c)
             (present-coloring-error
              (format nil "Error while printing colored text:~% ~a"
                      c))
             (write-string text)))))
  (write-string (make-string (max 0 (- align (length text)))
                             :initial-element #\Space)))
