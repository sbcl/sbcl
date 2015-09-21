;;;; Package (locking) related macros needed on the target before most
;;;; of the package machinery is available.
;;;;
;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;; Unbound outside package lock context, inside either list of
;;; packages for which locks are ignored, T when locks for
;;; all packages are ignored, and :invalid outside package-lock
;;; context. FIXME: This needs to be rebound for each thread.
(!defvar *ignored-package-locks* :invalid)

;; This proclamation avoids a ton of style warnings due to so many calls
;; that get cross-compiled prior to compiling "target-package.lisp"
(declaim (ftype (sfunction ((or symbol list) &optional (or string function) &rest t) t)
                assert-symbol-home-package-unlocked))

(defmacro with-single-package-locked-error ((&optional kind thing &rest format)
                                            &body body)
  #!-sb-package-locks (declare (ignore kind thing format))
  #!-sb-package-locks
  `(progn ,@body)
  #!+sb-package-locks
  (with-unique-names (topmost)
    `(progn
       ;; /show was fairly useless here because it printed "/foo-ing ~A"
       ;; without any clue as to what the interesting THING was.
       ;; It could be handy for debugging package locks in bootstrap code,
       ;; but if package locks work fine, it's just way too much noise.
       (/noshow0 ,(first format))
       (let ((,topmost nil))
         ;; We use assignment and conditional restoration instead of
         ;; dynamic binding because we want the ignored locks
         ;; to propagate to the topmost context.
         (when (eq :invalid *ignored-package-locks*)
           (setf *ignored-package-locks* nil
                 ,topmost t))
         (unwind-protect
              (progn
                ,@(ecase kind
                   (:symbol
                    `((assert-symbol-home-package-unlocked ,thing ,@format)))
                   (:package
                    `((assert-package-unlocked
                       (find-undeleted-package-or-lose ,thing) ,@format)))
                   ((nil)
                    `()))
                ,@body)
           (when ,topmost
             (setf *ignored-package-locks* :invalid)))))))

(defmacro without-package-locks (&body body)
  #!+sb-doc
  "Ignores all runtime package lock violations during the execution of
body. Body can begin with declarations."
  `(let (#!+sb-package-locks (*ignored-package-locks* t))
    ,@body))
