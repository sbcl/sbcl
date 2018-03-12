;;;; Call counting extension to the statistical profiler
;;;;
;;;; Copyright (C) 2003 Gerd Moellmann <gerd.moellmann@t-online.de>
;;;; All rights reserved.

(in-package #:sb-sprof)


;;;; Call counting

;;; The following functions tell sb-sprof to do call count profiling
;;; for the named functions in addition to normal statistical
;;; profiling.  The benefit of this over using SB-PROFILE is that this
;;; encapsulation is a lot more lightweight, due to not needing to
;;; track cpu usage / consing. (For example, compiling asdf 20 times
;;; took 13s normally, 15s with call counting for all functions in
;;; SB-C, and 94s with SB-PROFILE profiling SB-C).

(defun profile-call-counts (&rest names)
  "Mark the functions named by NAMES as being subject to call counting
during statistical profiling. If a string is used as a name, it will
be interpreted as a package name. In this case call counting will be
done for all functions with names like X or (SETF X), where X is a symbol
with the package as its home package."
  (dolist (name names)
    (if (stringp name)
        (let ((package (find-package name)))
          (do-symbols (symbol package)
            (when (eql (symbol-package symbol) package)
              (dolist (function-name (list symbol (list 'setf symbol)))
                (profile-call-counts-for-function function-name)))))
        (profile-call-counts-for-function name))))

(defun profile-call-counts-for-function (function-name)
  (unless (gethash function-name *encapsulations*)
    (setf (gethash function-name *encapsulations*) nil)))

(defun unprofile-call-counts ()
  "Clear all call counting information. Call counting will be done for no
functions during statistical profiling."
  (clrhash *encapsulations*))

;;; Called when profiling is started to enable the call counting
;;; encapsulation. Wrap all the call counted functions
(defun enable-call-counting ()
  (maphash (lambda (k v)
             (declare (ignore v))
             (enable-call-counting-for-function k))
           *encapsulations*))

;;; Called when profiling is stopped to disable the encapsulation. Restore
;;; the original functions.
(defun disable-call-counting ()
  (maphash (lambda (k v)
             (when v
               (assert (cdr v))
               (without-package-locks
                 (setf (fdefinition k) (cdr v)))
               (setf (cdr v) nil)))
           *encapsulations*))

(defun enable-call-counting-for-function (function-name)
  (let ((info (gethash function-name *encapsulations*)))
    ;; We should never try to encapsulate an fdefn multiple times.
    (assert (or (null info)
                (null (cdr info))))
    (when (and (fboundp function-name)
               (or (not (symbolp function-name))
                   (and (not (special-operator-p function-name))
                        (not (macro-function function-name)))))
      (let* ((original-fun (fdefinition function-name))
             (info (cons 0 original-fun)))
        (setf (gethash function-name *encapsulations*) info)
        (without-package-locks
          (setf (fdefinition function-name)
                (sb-int:named-lambda call-counter (sb-int:&more more-context more-count)
                  (declare (optimize speed (safety 0)))
                  ;; 2^59 calls should be enough for anybody, and it
                  ;; allows using fixnum arithmetic on x86-64. 2^32
                  ;; isn't enough, so we can't do that on 32 bit platforms.
                  (incf (the (unsigned-byte 59)
                             (car info)))
                  (multiple-value-call original-fun
                    (sb-c:%more-arg-values more-context
                                           0
                                           more-count)))))))))
