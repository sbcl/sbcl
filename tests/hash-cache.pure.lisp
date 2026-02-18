(with-test (:name (:hash-cache :interrupt))
  (let* ((type1 (random-type 500))
         (type2 (random-type 500))
         (wanted (subtypep type1 type2)))
    (dotimes (i 50) ; 2.5 seconds (= 50 * .05) of run time
      (block foo
        (sb-ext:schedule-timer (sb-ext:make-timer
                                (lambda ()
                                  (assert (eq wanted (subtypep type1 type2)))
                                  (return-from foo)))
                               0.05)
        (loop
           (assert (eq wanted (subtypep type1 type2))))))))
