;;;; More Unicode functions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-UNICODE")

(eval-when (:compile-toplevel :execute)
  (defun read-from-file (namestring &key (enforce-single-expr t) build-dependent)
    (declare (notinline concatenate) (ignore build-dependent))
    (with-open-file
        (s (concatenate 'string (if (boundp 'cl-user::*generated-sources-root*)
                                    (symbol-value 'cl-user::*generated-sources-root*)
                                    "")
                        namestring))
      (let* ((result (read s))
             (eof-result (cons nil nil)))
        (unless enforce-single-expr
          (return-from read-from-file result))
        (unless (eq (read s nil eof-result) eof-result)
          (error "more than one expression in file ~S" namestring))
        result))))

;;; This macro produces a lookup table that uses half the storage of a hash-table
;;; and achieves around double the performance on this test:
;;;
;;; * (time (loop for i below char-code-limit count (numeric-value (code-char i))))
;;;
;;; x86-64-based mac:
;;; Old
;;; ===
;;;    0.029 seconds of real time
;;;    0.029656 seconds of total run time (0.029644 user, 0.000012 system)
;;;    103.45% CPU
;;;    71,169,208 processor cycles
;;; New
;;; ===
;;;    0.012 seconds of real time
;;;    0.012075 seconds of total run time (0.012061 user, 0.000014 system)
;;;    100.00% CPU
;;;    29,034,292 processor cycles
;;;
;;; arm-based mac:
;;; Old
;;; ===
;;;    0.051 seconds of real time
;;;    0.050890 seconds of total run time (0.050709 user, 0.000181 system)
;;;    100.00% CPU
;;; New
;;; ===
;;;    0.033 seconds of real time
;;;    0.033195 seconds of total run time (0.033104 user, 0.000091 system)
;;;    100.00% CPU

(defmacro find-in-perfect-hashmap (x filename value-type value-getter)
  (declare (ignorable value-type value-getter))
  (let ((pairs
         (remove-if (lambda (x) (>= (car x) char-code-limit))
                    (read-from-file filename))))
    #-sb-unicode `(cdr (assoc (char-code ,x) ',pairs)) ; exactly 3 pairs
    #+sb-unicode
    (let*
        ((mapped-chars (coerce (mapcar 'car pairs) '(array (unsigned-byte 32) (*))))
         (lexpr (sb-c:make-perfect-hash-lambda mapped-chars))
         ;; We need the lexpr at compile-time to build the key/value arrays
         ;; and run-time of course, where the expression is stuffed in as
         ;; a form headed by LAMBDA.
         (hasher (compile nil lexpr))
         (n (length mapped-chars))
         ;; This string is pasted in as though written literally in source,
         ;; therefore it gets relocated to read-only space in the core.
         (key-array (make-array n :element-type 'character))
         (value-array (make-array n :element-type value-type)))
    (dolist (pair pairs)
      (let* ((index (funcall hasher (car pair)))
             (value (funcall value-getter pair)))
        (aver (char= (char key-array index) (code-char 0)))
        (setf (char key-array index) (code-char (car pair))
              (aref value-array index)
              (if (eq value-type 'character) (code-char value) value))))
    `(let ((hash (,lexpr (char-code ,x))))
       ;; Remember: even though the mapping is dense (range is 0..N-1)
       ;; a key which was not in the mapping as specified to the hash function
       ;; generator can screw with the calculation, causing it to return a value
       ;; outside the expected range. So bounds check it and then confirm a hit.
       (when (and (< hash ,n) (char= (char ,key-array hash) ,x))
         (aref ,value-array hash))))))

;;; Functions not defined in make-host-2 did not get exported
(export '(numeric-value
          bidi-mirroring-glyph))

(defun numeric-value (character)
  "Returns the numeric value of CHARACTER or NIL if there is no such value.
Numeric value is the most general of the Unicode numeric properties.
The only constraint on the numeric value is that it be a rational number."
  (or (find-in-perfect-hashmap character "output/ucd/numerics.lisp-expr" t cdr)
      (digit-value character)))

;;; FIXME: why does #-sb-unicode want or need this?
;;; (Indeed the regression test for it is *disabled* so I reiterate - WHY?)
(defun bidi-mirroring-glyph (character)
  "Returns the mirror image of CHARACTER if it exists.
Otherwise, returns NIL."
  ;; This used to call MIRRORED-P before table lookup, but it's not faster to do so
  #+sb-unicode
  (find-in-perfect-hashmap character "output/ucd/bidi-mirrors.lisp-expr"
                           character second)
  #-sb-unicode
  (macrolet ((direct-map (&aux (a (make-array char-code-limit :element-type 'character)))
               (dolist (pair (read-from-file "output/ucd/bidi-mirrors.lisp-expr") a)
                 (let ((key (car pair)))
                   (when (< key char-code-limit)
                     (setf (char a key) (code-char (second pair))))))))
    (let ((answer (char (direct-map) (char-code character))))
      (unless (char= answer (code-char 0)) answer))))
