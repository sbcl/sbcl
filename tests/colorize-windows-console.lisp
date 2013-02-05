
(defun output-handle ()
  (sb-win32::get-std-handle-or-null
   sb-win32::+std-output-handle+))

(sb-alien:define-alien-type nil
    (sb-alien:struct console-screen-buffer-info
            (size sb-alien:int)
            (cursor-position sb-alien:int)
            (attributes sb-alien:int)
            (window sb-win32:dword)
            (maximum-window-size sb-alien:int)))

(sb-alien:define-alien-routine
    ("SetConsoleTextAttribute" set-console-text-attribute)
  sb-alien:boolean
  (console sb-win32:handle)
  (attributes sb-alien:int))

(sb-alien:define-alien-routine
    ("GetConsoleScreenBufferInfo" get-console-screen-buffer-info)
  sb-alien:boolean
  (console-output sb-win32:handle)
  (info (* (sb-alien:struct console-screen-buffer-info))))

(defun get-attributes ()
  (sb-alien:with-alien ((info (sb-alien:struct console-screen-buffer-info)))
    (get-console-screen-buffer-info (output-handle)
                                    (sb-alien:addr info))
    (sb-alien:slot info 'attributes)))

(defun console-color (color)
  (ecase color
    (:red 4)
    (:green 2)))

(defun set-color (color)
  (set-console-text-attribute (output-handle) color))

(defun %output-colored-text (text color &key bold)
  (declare (ignore bold))
  (let ((current-attributes (get-attributes)))
    (unwind-protect
         (progn (set-color (console-color color))
                (write-string text)
                (finish-output))
      (set-color current-attributes))))
