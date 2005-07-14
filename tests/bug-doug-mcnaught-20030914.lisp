(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *readtable* (copy-readtable nil))  ; LOAD binds *readtable*...

  (set-macro-character #\] (get-macro-character #\)))

  (set-dispatch-macro-character #\# #\[
                                #'(lambda (s c n) (declare (ignore c))
                                    (let* ((type (if n `(unsigned-byte ,n)
                                                   '(unsigned-byte 8)))
                                           (list (read-delimited-list #\] s nil))
                                           (len (length list)))
                                      (make-array (list len)
                                                  :element-type type
                                                  :initial-contents list)))))

(defvar *bug-doug-mcnaught-20030914* '#4[1 2 3])
