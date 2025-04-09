(defun readprocmaps ()
  #+(and linux immobile-space) ; sb-vm: symbols may not exist?
  (let* ((space-start sb-vm:fixedobj-space-start)
         (space-end (+ sb-vm:fixedobj-space-size space-start))
         (state nil))
    (with-open-file (f "/proc/self/maps")
      (loop
       (let ((line (read-line f nil)))
         (assert line) ; better not run out of lines before passing the test
         (sb-int:binding*
             (((range-start dashpos) (parse-integer line :radix 16 :junk-allowed t))
              ((range-end spacepos)
               (progn (assert (char= (char line dashpos) #\-))
                      (parse-integer line :radix 16 :start (1+ dashpos) :junk-allowed t)))
              (perms
               (progn (assert (char= (char line spacepos) #\space))
                      (subseq line (incf spacepos) (+ spacepos 4)))))
           (case state
             ((nil)
              ;; look for a range that starts exactly at space-start
              (if (= range-start space-start)
                  (setq state :in-fixedobj-space)
                  ;; larger address means somehow we missed seeeing fixedobj space
                  (assert (< range-end space-start))))
             (:in-fixedobj-space
              (cond ((= range-end space-end)
                     (assert (string= perms "rwxp")) ; untouched pages must exist
                     (return))
                    (t
                     (assert (< range-end space-end))))))))))))

(with-test (:name :fixedobj-sized-correctly) (readprocmaps))
