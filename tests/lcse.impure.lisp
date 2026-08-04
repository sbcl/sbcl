(setq *evaluator-mode* :compile)

(defstruct s
  (x (make-array 3)))

;; Don't creash evaluating this.
;; The bug was that local common subexpression elimintation tried to add
;; a REUSE-VAR binding into the LET which bound * instead of just giving up.
(let (*)
  (lambda ()
    (let* ((s #.(make-s))
           (a (s-x s))
           (b (s-x s)))
      (values (aref a 0)
              (aref b 0)))))
