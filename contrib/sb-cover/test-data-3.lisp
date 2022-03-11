(declaim (optimize sb-c:store-coverage-data))

(defun test-1 ()
  (print '((1 3 1))))

(defun test-2 ()
  (assert (equal (test-1)
                 (list (list 1 3 1)))))

;;; This function failed to compile after git revision 96532df8969a9976
;;; because label positions became non-monotonically-increasing.
;;; DUMP-1-LOCATION failed with "The value -33 is not of type
;;; (UNSIGNED-BYTE 32) when binding SB-C::VALUE" where it tried to
;;; delta-encode the block locations.
(defun smoketest (code fn)
  (loop for i from sb-vm:code-constants-offset
        below (sb-kernel:code-header-words code)
        do (funcall fn (sb-kernel:code-header-ref code i))))
