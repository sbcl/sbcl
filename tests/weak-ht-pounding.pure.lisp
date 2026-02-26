(defvar *ht* (make-hash-table :test #'eq :weakness :key
                              :synchronized t))

(sb-ext:defglobal *stop* nil)

(with-test (:name :weak-hash-concurrency
                  :skipped-on (:not :sb-thread))
  (let ((threads
         (loop repeat 12
               collect
           (sb-thread:make-thread
            (lambda ()
              (loop
                (setf (gethash (make-array 100) *ht*) 10)
                (when *stop* (return))))))))
    (sleep 1)
    (setq *stop* t)
    (mapc 'sb-thread:join-thread threads)))
