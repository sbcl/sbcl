
(defparameter *pkgname* "SOME-FINE-PACKAGE")
(unless (find-package *pkgname*)
  (make-package *pkgname*))

(defglobal *count-found* 0)
(declaim (fixnum *count-found*))

;;; We most certainly don't want to synchronize all calls to FIND-PACKAGE
;;; and FIND-SYMBOL with mutators of the package name graph.
;;; Unfortunately there is an interesting and not too far-fetched use which
;;; involves one thread looking up a package by its primary name while some
;;; other thread adds a nickname to that package.  This used to potentially
;;; fail in the looking-up thread because RENAME-PACKAGE started by deleting
;;; all names of the package being altered. That is no longer the case.
(defun renaming-experiment (n-trials n-threads)
  (let* ((trigger-sem (sb-thread:make-semaphore))
         (trial-completion-sem (sb-thread:make-semaphore))
         (threadfun
          (lambda ()
            (dotimes (i n-trials)
              (sb-thread:wait-on-semaphore trigger-sem)
              (when (find-package *pkgname*) (atomic-incf *count-found*))
              (sb-thread:signal-semaphore trial-completion-sem))))
         (threads
          (loop for i from 1 to n-threads
                collect (sb-thread:make-thread threadfun
                                               :name (format nil "thread ~d" i))))
         (wins 0))
    (dotimes (i n-trials)
      (setq *count-found* 0)
      (sb-thread:signal-semaphore trigger-sem n-threads)
      (rename-package *pkgname* *pkgname* '("SFP"))
      (sb-thread:wait-on-semaphore trial-completion-sem :n n-threads)
      (rename-package *pkgname* *pkgname* nil)
      ;; (format t "~&Trial ~d: ~d~%" (1+ i) *count-found*)
      (when (= *count-found* n-threads)
        (incf wins)))
    (mapc 'sb-thread:join-thread threads)
    wins))

(with-test (:name :renamed-package-does-not-disappear
            :skipped-on (:not :sb-thread))
  (let* ((n-trials 100)
         (wins (renaming-experiment n-trials 4)))
    ;; This was getting about 50% to 70% success prior to the fix.
    ;; (format t "~&Win percent: ~,,2f~%" (/ wins n-trials))
    (assert (= wins n-trials))))
