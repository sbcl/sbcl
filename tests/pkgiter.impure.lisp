;; lp# 2080387 but using the code of CSR, not the UB case
(make-package :som :use nil)
(setf *package* (find-package :som))
(cl:defparameter *mmm* cl:nil)
(cl:do-symbols (sym (cl:find-package :som))
  (cl:push sym *mmm*)
  (cl:export sym))

(cl:assert (cl:find '*mmm* *mmm*))
(cl:assert (cl:find 'sym *mmm*))
(cl:assert (cl:eq (cl:nth-value 1 (cl:find-symbol "*MMM*" "SOM"))
                  :external))
