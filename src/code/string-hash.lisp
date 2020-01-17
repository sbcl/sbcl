;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;;; hashing strings
;;;;
;;;; Note that this operation is used in compiler symbol table
;;;; lookups, so we'd like it to be fast.
;;;;
;;;; As of 2004-03-10, we implement the one-at-a-time algorithm
;;;; designed by Bob Jenkins (see
;;;; <http://burtleburtle.net/bob/hash/doobs.html> for some more
;;;; information).

#-sb-xc-host (declaim (inline %sxhash-simple-substring))
(defun %sxhash-simple-substring (string start end)
  ;; FIXME: As in MIX above, we wouldn't need (SAFETY 0) here if the
  ;; cross-compiler were smarter about ASH, but we need it for
  ;; sbcl-0.5.0m.  (probably no longer true?  We might need SAFETY 0
  ;; to elide some type checks, but then again if this is inlined in
  ;; all the critical places, we might not -- CSR, 2004-03-10)

  ;; Never decrease safety in the cross-compiler. It's not worth the headache
  ;; of tracking down insidious host/target compatibility bugs.
  #-sb-xc-host (declare (optimize (speed 3) (safety 0)))
  (macrolet ((guts ()
               `(loop for i of-type index from start below end do
                  (set-result (+ result (char-code (aref string i))))
                  (set-result (+ result (ash result 10)))
                  (set-result (logxor result (ash result -6)))))
             (set-result (form)
               `(setf result (ldb (byte #.sb-vm:n-word-bits 0) ,form))))
    (let ((result 238625159)) ; (logandc2 most-positive-fixnum (sxhash #\S)) on 32 bits
      (declare (type word result))
      ;; Avoid accessing elements of a (simple-array nil (*)).
      ;; The expansion of STRING-DISPATCH involves ETYPECASE,
      ;; so we can't simply omit one case. Therefore that macro
      ;; is unusable here.
      #-sb-xc-host (typecase string
                     (simple-base-string (guts))
                     ((simple-array character (*)) (guts)))

      ;; just do it, don't care about loop unswitching or simple-ness of the string.
      #+sb-xc-host (guts)

      (set-result (+ result (ash result 3)))
      (set-result (logxor result (ash result -11)))
      (set-result (logxor result (ash result 15)))
      (logand result sb-xc:most-positive-fixnum))))
;;; test:
;;;   (let ((ht (make-hash-table :test 'equal)))
;;;     (do-all-symbols (symbol)
;;;       (let* ((string (symbol-name symbol))
;;;           (hash (%sxhash-substring string)))
;;;      (if (gethash hash ht)
;;;          (unless (string= (gethash hash ht) string)
;;;            (format t "collision: ~S ~S~%" string (gethash hash ht)))
;;;          (setf (gethash hash ht) string))))
;;;     (format t "final count=~W~%" (hash-table-count ht)))

(defun %sxhash-simple-string (x)
  (declare (optimize speed))
  ;; Don't care if the host uses non-simple strings where SBCL would always
  ;; have had a simple-string, notably SYMBOL-NAME and PACKAGE-NAME.
  (declare (type #+sb-xc-host string #-sb-xc-host simple-string x))
  ;; KLUDGE: this FLET is a workaround (suggested by APD) for presence
  ;; of let conversion in the cross compiler, which otherwise causes
  ;; strongly suboptimal register allocation.
  (flet ((trick (x)
           (%sxhash-simple-substring x 0 (length x))))
    (declare (notinline trick))
    (trick x)))
