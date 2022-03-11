;;;; tests of the compiler vm internal consistency intended to be
;;;; executed as soon as the cross-compiler is built.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(in-package "SB-VM")

(/show "beginning tests/vm.before-xc.lisp")

(flet ((yes (x)
         (assert
          (eql immediate-sc-number
               (immediate-constant-sc x))))
       (no (x)
         (assert
          (not (immediate-constant-sc x)))))
  ;; target fixnums can be dealt with as immediates; target bignums
  ;; can not.
  (yes #.most-positive-fixnum)
  (yes #.most-negative-fixnum)
  (no #.(1+ most-positive-fixnum))
  (no #.(1- most-negative-fixnum)))

;; Assert that DO-PACKED-TNS has unsurprising behavior if the body RETURNs.
;; This isn't a test in the problem domain of CL - it's of an internal macro,
;; and x86-64-specific not because of broken-ness, but because it uses
;; known random TNs to play with.
(in-package "SB-C")
#+x86-64
(dotimes (i (ash 1 6))
  (labels ((make-tns (n)
             (mapcar 'copy-structure
                     (subseq `(,sb-vm::rax-tn ,sb-vm::rbx-tn ,sb-vm::rcx-tn) 0 n)))
           (link (list)
             (when list
               (setf (sb-c::tn-next (car list)) (link (cdr list)))
               (car list))))
    (let* ((normal     (make-tns (ldb (byte 2 0) i)))
           (restricted (make-tns (ldb (byte 2 2) i)))
           (wired      (make-tns (ldb (byte 2 4) i)))
           (expect     (append normal restricted wired))
           (comp       (sb-c::make-empty-component))
           (ir2-comp   (sb-c::make-ir2-component)))
      (setf (sb-c:component-info comp) ir2-comp
            (sb-c::ir2-component-normal-tns ir2-comp) (link normal)
            (sb-c::ir2-component-restricted-tns ir2-comp) (link restricted)
            (sb-c::ir2-component-wired-tns ir2-comp) (link wired))
      (let* ((list)
             (result (sb-c::do-packed-tns (tn comp 42) (push tn list))))
        (assert (eq result 42))
        (assert (equal expect (nreverse list))))
      (let* ((n 0) (list)
             (result (sb-c::do-packed-tns (tn comp 'bar)
                       (push tn list)
                       (if (= (incf n) 4) (return 'foo)))))
        (assert (eq result (if (>= (length expect) 4) 'foo 'bar)))
        (assert (equal (subseq expect 0 (min 4 (length expect)))
                       (nreverse list)))))))

(/show "done with tests/vm.before-xc.lisp")
