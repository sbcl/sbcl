;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is derived from software originally released by Xerox
;;;; Corporation. Copyright and release statements follow. Later modifications
;;;; to the software are in the public domain and are provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for more
;;;; information.

;;;; copyright information from original PCL sources:
;;;;
;;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;;; All rights reserved.
;;;;
;;;; Use and copying of this software and preparation of derivative works based
;;;; upon this software are permitted. Any distribution of this software or
;;;; derivative works must comply with all applicable United States export
;;;; control laws.
;;;;
;;;; This software is made available AS IS, and Xerox Corporation makes no
;;;; warranty about the software, its performance or its conformity to any
;;;; specification.

(in-package "SB-PCL")

;;;; The whole of this file is dead code as long as *optimize-cache-functions-p*
;;;; is true, which it currently _always_ is.


(defun emit-reader/writer-function (reader/writer 1-or-2-class class-slot-p)
  (values
   (ecase reader/writer
     (:reader (ecase 1-or-2-class
                (1 (if class-slot-p
                       (emit-reader/writer-macro :reader 1 t)
                       (emit-reader/writer-macro :reader 1 nil)))
                (2 (if class-slot-p
                       (emit-reader/writer-macro :reader 2 t)
                       (emit-reader/writer-macro :reader 2 nil)))))
     (:writer (ecase 1-or-2-class
                (1 (if class-slot-p
                       (emit-reader/writer-macro :writer 1 t)
                       (emit-reader/writer-macro :writer 1 nil)))
                (2 (if class-slot-p
                       (emit-reader/writer-macro :writer 2 t)
                       (emit-reader/writer-macro :writer 2 nil)))))
     (:boundp (ecase 1-or-2-class
                (1 (if class-slot-p
                       (emit-reader/writer-macro :boundp 1 t)
                       (emit-reader/writer-macro :boundp 1 nil)))
                (2 (if class-slot-p
                       (emit-reader/writer-macro :boundp 2 t)
                       (emit-reader/writer-macro :boundp 2 nil)))))
     (:writer (ecase 1-or-2-class
                (1 (if class-slot-p
                       (emit-reader/writer-macro :makunbound 1 t)
                       (emit-reader/writer-macro :makunbound 1 nil)))
                (2 (if class-slot-p
                       (emit-reader/writer-macro :makunbound 2 t)
                       (emit-reader/writer-macro :makunbound 2 nil))))))
   nil))

(defun emit-one-or-n-index-reader/writer-function
    (reader/writer cached-index-p class-slot-p)
  (values
   (ecase reader/writer
     (:reader (if cached-index-p
                  (if class-slot-p
                      (emit-one-or-n-index-reader/writer-macro :reader t t)
                      (emit-one-or-n-index-reader/writer-macro :reader t nil))
                  (if class-slot-p
                      (emit-one-or-n-index-reader/writer-macro :reader nil t)
                      (emit-one-or-n-index-reader/writer-macro :reader nil nil))))
     (:writer (if cached-index-p
                  (if class-slot-p
                      (emit-one-or-n-index-reader/writer-macro :writer t t)
                      (emit-one-or-n-index-reader/writer-macro :writer t nil))
                  (if class-slot-p
                      (emit-one-or-n-index-reader/writer-macro :writer nil t)
                      (emit-one-or-n-index-reader/writer-macro :writer nil nil))))
     (:boundp (if cached-index-p
                  (if class-slot-p
                      (emit-one-or-n-index-reader/writer-macro :boundp t t)
                      (emit-one-or-n-index-reader/writer-macro :boundp t nil))
                  (if class-slot-p
                      (emit-one-or-n-index-reader/writer-macro :boundp nil t)
                      (emit-one-or-n-index-reader/writer-macro :boundp nil nil))))
     (:makunbound (if cached-index-p
                      (if class-slot-p
                          (emit-one-or-n-index-reader/writer-macro :makunbound t t)
                          (emit-one-or-n-index-reader/writer-macro :makunbound t nil))
                      (if class-slot-p
                          (emit-one-or-n-index-reader/writer-macro :makunbound nil t)
                          (emit-one-or-n-index-reader/writer-macro :makunbound nil nil)))))
   nil))

(defun emit-checking-or-caching-function (cached-emf-p return-value-p metatypes applyp)
  (values (emit-checking-or-caching-function-preliminary
           cached-emf-p return-value-p metatypes applyp)
          t))

(defun emit-checking-or-caching-function-preliminary
    (cached-emf-p return-value-p metatypes applyp)
  (declare (ignore applyp))
  (if cached-emf-p
      (lambda (cache miss-fn)
        (declare (type function miss-fn))
        #'(lambda (&rest args)
            (declare #.*optimize-speed*)
            (with-dfun-wrappers (args metatypes)
              (dfun-wrappers invalid-wrapper-p)
              (apply miss-fn args)
              (if invalid-wrapper-p
                  (apply miss-fn args)
                  (multiple-value-bind (not-in-cache emf)
                      (probe-cache cache dfun-wrappers)
                    (if (eq emf not-in-cache)
                        (apply miss-fn args)
                        (if return-value-p
                            emf
                            (invoke-emf emf args))))))))
      (lambda (cache emf miss-fn)
        (declare (type function miss-fn))
        #'(lambda (&rest args)
            (declare #.*optimize-speed*)
            (with-dfun-wrappers (args metatypes)
              (dfun-wrappers invalid-wrapper-p)
              (apply miss-fn args)
              (if invalid-wrapper-p
                  (apply miss-fn args)
                  (let ((found-p (probe-cache cache dfun-wrappers)))
                    (if found-p
                        (invoke-emf emf args)
                        (if return-value-p
                            t
                            (apply miss-fn args))))))))))

(defun emit-default-only-function (metatypes applyp)
  (declare (ignore metatypes applyp))
  (values (lambda (emf)
            (lambda (&rest args)
              (invoke-emf emf args)))
          t))
