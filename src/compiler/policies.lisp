;;;; aimed optimization qualities

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

(define-optimization-quality type-check
    (cond ((= safety 0) 0)
          ;; FIXME: It is duplicated in PROBABLE-TYPE-CHECK-P and in
          ;; some other places.

          ((and (<= speed safety)
                (<= space safety)
                (<= compilation-speed safety))
           3)
          (t 2))
  ("no" "maybe" "fast" "full"))

(define-optimization-quality check-tag-existence
    (cond ((= safety 0) 0)
          (t 3))
  ("no" "maybe" "yes" "yes"))

(define-optimization-quality let-convertion
    (if (<= debug speed) 3 0)
  ("off" "maybe" "on" "on"))

(define-optimization-quality verify-arg-count
    (if (zerop safety) 0 3)
  ("no" "maybe" "yes" "yes"))

(define-optimization-quality merge-tail-calls
    (if (or (> space debug)
            (> speed debug))
        3
        0)
  ("no" "maybe" "yes" "yes"))

(define-optimization-quality insert-debug-catch
    (if (> debug (max speed space))
        3
        0)
  ("no" "maybe" "yes" "yes"))

(define-optimization-quality recognize-self-calls
    (if (> (max speed space) debug)
        3
        0)
  ("no" "maybe" "yes" "yes"))

(define-optimization-quality stack-allocate-dynamic-extent
    (if (and (> (max speed space) (max debug safety))
	     (< safety 3))
	3
	0)
  ("no" "maybe" "yes" "yes"))

(define-optimization-quality float-accuracy
    3
  ("degraded" "full" "full" "full"))

(define-optimization-quality insert-step-conditions
    (if (> debug (max speed space))
        debug
        0)
  ("no" "no" "partial" "full"))
