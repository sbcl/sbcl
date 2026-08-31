
;; Semantically a "limit" is the top end of the thing you're allowed to have,
;; not an exclusive number. (think Speed limit 55)
;; So thanks Lisp for screwing up the English language like this.
(defconstant mvl (min 70 (1- multiple-values-limit))) ; don't use all of heap
(defun ret ()
  (values . #.(loop for i below mvl collect `(list ,i))))

(defun ret2 ()
  (values 1 2 3 4))

(defun j ()
  (let ((end (+ (get-internal-real-time)
                (* internal-time-units-per-second #-slow 1/2))))
    (loop until (>= (get-internal-real-time) end)
          do
          (ret2)
          (assert (not (mismatch (multiple-value-list (ret))
                                 '#.(loop for i below mvl collect (list i))
                                 :test #'equal))))))

(test-util:with-test (:name :async-mv-area-preservation)
  (schedule-timer (make-timer (lambda () (ret2))) 0.1 :repeat-interval 0.005)
  (j))
