(in-package :cl-user)

(require :sb-sprof)

;;; silly examples

(defun test-0 (n &optional (depth 0))
  (declare (optimize (debug 3)))
  (when (< depth n)
    (dotimes (i n)
      (test-0 n (1+ depth))
      (test-0 n (1+ depth)))))

(defun test ()
  (sb-sprof:with-profiling (:reset t :max-samples 1000 :report :graph)
    (test-0 7)))

(defun consalot ()
  (let ((junk '()))
    (loop repeat 10000 do
         (push (make-array 10) junk))
    junk))

(defun consing-test ()
  ;; 0.0001 chosen so that it breaks rather reliably when sprof does not
  ;; respect pseudo atomic.
  (sb-sprof:with-profiling (:reset t :sample-interval 0.0001 :report :graph :loop nil)
    (let ((target (+ (get-universal-time) 15)))
      (princ #\.)
      (force-output)
      (loop
         while (< (get-universal-time) target)
         do (consalot)))))

#-(or win32 darwin)                    ;not yet
(test)
#-(or win32 darwin)                    ;not yet
(consing-test)

;; For debugging purposes, print output for visual inspection to see if
;; the allocation sequence gets hit in the right places (i.e. not at all
;; in traditional builds, and everywhere if SB-SAFEPOINT-STRICTLY is
;; enabled.)
(disassemble #'consalot)
