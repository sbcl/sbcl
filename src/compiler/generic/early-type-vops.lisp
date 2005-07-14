;;;; generic type testing and checking apparatus

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.
(in-package "SB!VM")

(defparameter *immediate-types*
  (list* unbound-marker-widetag character-widetag
         (when (= sb!vm::n-word-bits 64)
           (list single-float-widetag))))

(defparameter *fun-header-widetags*
  (list funcallable-instance-header-widetag
        simple-fun-header-widetag
        closure-header-widetag))

(defun canonicalize-headers (headers)
  (collect ((results))
    (let ((start nil)
          (prev nil)
          (delta (- other-immediate-1-lowtag other-immediate-0-lowtag)))
      (flet ((emit-test ()
               (results (if (= start prev)
                            start
                            (cons start prev)))))
        (dolist (header (sort headers #'<))
          (cond ((null start)
                 (setf start header)
                 (setf prev header))
                ((= header (+ prev delta))
                 (setf prev header))
                (t
                 (emit-test)
                 (setf start header)
                 (setf prev header))))
        (emit-test)))
    (results)))

(defmacro test-type (value target not-p
                     (&rest type-codes)
                     &rest other-args
                     &key &allow-other-keys)
  ;; Determine what interesting combinations we need to test for.
  (let* ((type-codes (mapcar #'eval type-codes))
         (fixnump (and (member even-fixnum-lowtag type-codes)
                       (member odd-fixnum-lowtag type-codes)
                       t))
         (lowtags (remove lowtag-limit type-codes :test #'<))
         (extended (remove lowtag-limit type-codes :test #'>))
         (immediates (intersection extended *immediate-types* :test #'eql))
         (headers (set-difference extended *immediate-types* :test #'eql))
         (function-p (if (intersection headers *fun-header-widetags*)
                         (if (subsetp headers *fun-header-widetags*)
                             t
                             (error "can't test for mix of function subtypes ~
                                     and normal header types"))
                         nil)))
    (unless type-codes
      (error "At least one type must be supplied for TEST-TYPE."))
    (cond
      (fixnump
       (when (remove-if (lambda (x)
                          (or (= x even-fixnum-lowtag)
                              (= x odd-fixnum-lowtag)))
                        lowtags)
         (error "can't mix fixnum testing with other lowtags"))
       (when function-p
         (error "can't mix fixnum testing with function subtype testing"))
       (cond
         ((and (= sb!vm:n-word-bits 64) immediates headers)
          `(%test-fixnum-immediate-and-headers ,value ,target ,not-p
                                               ,(car immediates)
                                               ',(canonicalize-headers
                                                  headers)
                                               ,@other-args))
         (immediates
          (if (= sb!vm:n-word-bits 64)
              `(%test-fixnum-and-immediate ,value ,target ,not-p
                                           ,(car immediates)
                                           ,@other-args)
              (error "can't mix fixnum testing with other immediates")))
         (headers
          `(%test-fixnum-and-headers ,value ,target ,not-p
                                     ',(canonicalize-headers headers)
                                     ,@other-args))
         (t
          `(%test-fixnum ,value ,target ,not-p
                         ,@other-args))))
      (immediates
       (cond
         (headers
          (if (= sb!vm:n-word-bits 64)
              `(%test-immediate-and-headers ,value ,target ,not-p
                                            ,(car immediates)
                                            ',(canonicalize-headers headers)
                                            ,@other-args)
              (error "can't mix testing of immediates with testing of headers")))
         (lowtags
          (error "can't mix testing of immediates with testing of lowtags"))
         ((cdr immediates)
          (error "can't test multiple immediates at the same time"))
         (t
          `(%test-immediate ,value ,target ,not-p ,(car immediates)
                            ,@other-args))))
      (lowtags
       (when (cdr lowtags)
         (error "can't test multiple lowtags at the same time"))
       (when headers
         (error "can't test non-fixnum lowtags and headers at the same time"))
       `(%test-lowtag ,value ,target ,not-p ,(car lowtags) ,@other-args))
      (headers
       `(%test-headers ,value ,target ,not-p ,function-p
         ',(canonicalize-headers headers)
         ,@other-args))
      (t
       (error "nothing to test?")))))

