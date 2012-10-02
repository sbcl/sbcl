;;;; miscellaneous VM definition noise for the ARM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")


;;;; register specs

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *register-names* (make-array 16 :initial-element nil)))

(macrolet ((defreg (name offset)
               (let ((offset-sym (symbolicate name "-OFFSET")))
                 `(eval-when (:compile-toplevel :load-toplevel :execute)
                   (def!constant ,offset-sym ,offset)
                   (setf (svref *register-names* ,offset-sym) ,(symbol-name name)))))

           (defregset (name &rest regs)
               `(eval-when (:compile-toplevel :load-toplevel :execute)
                 (defparameter ,name
                   (list ,@(mapcar #'(lambda (name)
                                       (symbolicate name "-OFFSET")) regs))))))

  (defreg r0 0)
  (defreg r1 1)
  (defreg r2 2)
  (defreg r3 3)
  (defreg r4 4)
  (defreg r5 5)
  (defreg r6 6)
  (defreg r7 7)
  (defreg r8 8)
  (defreg r9 9)
  (defreg null 10)
  (defreg fp 11)
  (defreg r12 12)
  (defreg sp 13)
  (defreg lr 14)
  (defreg pc 15) ;; Yes, the program counter.

  (defregset system-regs
      null fp sp lr pc)

  (defregset descriptor-regs
      r0 r1 r2 r3 r4 r5 r6)

  (defregset non-descriptor-regs
      r7 r8 r9 r12)

  ;; registers used to pass arguments
  ;;
  ;; the number of arguments/return values passed in registers
  (def!constant  register-arg-count 4)
  ;; names and offsets for registers used to pass arguments
  (defregset *register-arg-offsets*  r0 r1 r2 r3)
  (defparameter *register-arg-names* '(r0 r1 r2 r3)))


;;;; SB and SC definition:

(define-storage-base registers :finite :size 16)
(define-storage-base control-stack :unbounded :size 8)
(define-storage-base non-descriptor-stack :unbounded :size 0)
(define-storage-base constant :non-packed)
(define-storage-base immediate-constant :non-packed)
