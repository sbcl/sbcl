(defun ascii-color (color)
  (ecase color
    (:red 31)
    (:green 32)))

(defun %output-colored-text (text color &key bold)
  (format t "~c[~a~:[~;;1~]m~a~c[0m"
          #\Esc
          (ascii-color color)
          bold
          text #\Esc))
