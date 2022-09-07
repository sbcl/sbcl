(defun f1 (x) `(invoked-on ,x))
(compile 'f1)
(setf (symbol-function 'f2) (symbol-function 'f1))
(defun g (arg)
  (case arg
    (1 (f1 arg))
    (2 (f2 arg))
    (3 (f1 #'f1))))
#+immobile-code (sb-vm::statically-link-core)

(with-test (:name :static-linker-glitch)
  (assert (equal (g 3) `(invoked-on ,#'f1))))

#+nil ; This fails because ambigous references are not patched any more
(with-test (:name :static-unlinker)
  (let ((sb-c::*compile-to-memory-space* :immobile))
    (declare (muffle-conditions style-warning))
    (flet ((disassembly-lines (name)
             (split-string
              (with-output-to-string (s)
                (let ((sb-disassem:*disassem-location-column-width* 0))
                  (disassemble name :stream s)))
              #\newline))
           (expect (match lines)
             (assert (loop for line in lines
                           thereis (search match line)))))
      (compile 'h '(lambda (x) (1+ x)))
      (setf (symbol-function 'g) #'h (symbol-function 'f) #'h)
      (compile 'c '(lambda (x) (g x)))
      (compile 'd '(lambda (x) (f (g x))))
      ;; The FDEFN-FUN of F is same as that of G.
      ;; Statically linking D should not patch the fdefn calls into static calls
      ;; because it can't unambiguously be undone without storing additional data
      ;; about where patches were performed to begin with.
      (sb-vm::statically-link-core :callers '(c d))
      (let ((lines (disassembly-lines 'c)))
        (expect "#<FUNCTION H>" lines))
      (let ((lines (disassembly-lines 'd)))
        (expect "#<FUNCTION H>" lines))
      (setf (symbol-function 'g) #'+)
      (let ((lines (disassembly-lines 'c)))
        (expect "#<FDEFN G>" lines))
      (let ((lines (disassembly-lines 'd)))
        (expect "#<FDEFN G>" lines)
        (expect "#<FUNCTION H>" lines)))))
