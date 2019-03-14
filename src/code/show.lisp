;;;; some stuff for displaying information for debugging/experimenting
;;;; with the system, mostly conditionalized with #+SB-SHOW

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-INT")

;;;; various SB-SHOW-dependent forms
;;;;
;;;; In general, macros named /FOO
;;;;   * are for debugging/tracing
;;;;   * expand into nothing unless :SB-SHOW is in the target
;;;;     features list
;;;; Often, they also do nothing at runtime if */SHOW* is NIL, but
;;;; this is not always true for some very-low-level ones.
;;;;
;;;; (I follow the "/FOO for debugging/tracing expressions" naming
;;;; rule and several other naming conventions in all my Lisp
;;;; programming when possible, and then set Emacs to display comments
;;;; in one shade of blue, tracing expressions in another shade of
;;;; blue, and declarations and assertions in a yellowish shade, so
;;;; that it's easy to separate them from the "real code" which
;;;; actually does the work of the program. -- WHN 2001-05-07)

;;; Set this to NIL to suppress output from /SHOW-related forms.
(defparameter */show* t)

;;; shorthand for a common idiom in output statements used in
;;; debugging: (/SHOW "Case 2:" X Y) becomes something to the effect
;;; of (FORMAT .. "~&/Case 2: X=~S Y=~S~%" X Y), conditional on */SHOW*.
;;; We eschew FORMAT though because the directive interpreters
;;; might not have been set up yet.
(defmacro /show (&rest xlist)
  (declare (ignorable xlist))
  ;; CONCATENATE transformer causes CTYPEP ambiguity.
  ;; WRITE transformer bind *PRINT-PRETTY* which isn't defined yet.
  (declare (notinline concatenate write-to-string))
  #+sb-show
  `(when */show*
     (let ((.stream. *trace-output*))
       (fresh-line .stream.)
       ;; *trace-output* is initially unbuffered, so try to combine syscalls
       ;; by squishing the first few writes into one string, as opposed to
       ;; writing #\/, string, #\space, "expr=" as four separate operations.
       ,@(let ((preamble (if (and (stringp (car xlist)) (cdr xlist))
                             (concatenate 'string "/" (pop xlist) " ")
                             "/")))
           (mapcan
            (lambda (x)
              (prog1
                  (if (stringp x)
                      (list `(write-string ,(concatenate 'string preamble x)
                                           .stream.))
                      (list `(write-string
                              ,(concatenate 'string
                                            preamble
                                            (write-to-string x :pretty nil)
                                            "=")
                              .stream.)
                            `(write ,x :stream .stream.)))
                (setq preamble " ")))
            xlist))
       (terpri .stream.))))

;;; a disabled-at-compile-time /SHOW, implemented as a macro instead
;;; of a function so that leaving occasionally-useful /SHOWs in place
;;; but disabled incurs no run-time overhead and works even when the
;;; arguments can't be evaluated (e.g. because they're only meaningful
;;; in a debugging version of the system, or just due to bit rot..)
(defmacro /noshow (&rest rest)
  (declare (ignore rest)))

;;; a trivial version of /SHOW which only prints a constant string,
;;; implemented at a sufficiently low level that it can be used early
;;; in cold init
;;;
;;; Unlike the other /SHOW-related functions, this one doesn't test
;;; */SHOW* at runtime, because messing with special variables early
;;; in cold load is too much trouble to be worth it.
(defmacro /show0 (&rest string-designators)
  ;; We don't inline CONCATENATE, because some of the
  ;; machinery behind its optimizations isn't available in the
  ;; cross-compiler.
  (declare (notinline concatenate))
  (let ((s (apply #'concatenate
                  'simple-string
                  (mapcar #'string string-designators))))
    (declare (ignorable s)) ; (for when #-SB-SHOW)
    #+sb-xc-host `(/show ,s)
    ;; ensure that fprintf() receives only a SIMPLE-BASE-STRING
    #+(and sb-show (not sb-xc-host))
    `(%primitive print ,(concatenate 'simple-base-string "/" s))))
(defmacro /noshow0 (&rest rest)
  (declare (ignore rest)))
