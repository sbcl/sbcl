;;;; cross float reader

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

(macrolet ((def (name bits-fun n-exponent-bits n-mantissa-bits)
             (let ((initial-exponent (1- (ash 1 (1- n-exponent-bits))))
                   (max-exponent (1- (ash 1 n-exponent-bits))))
             `(defun ,name (rational)
                (declare (type cl:rational rational))
                (let ((sign (if (cl:= (cl:signum rational) -1) 1 0))
                      (magnitude (cl:abs rational)))
                  (when (cl:= magnitude 0)
                    (return-from ,name (,bits-fun 0 0 0)))
                  (loop with dir = (if (cl:> magnitude 1) -1 1)
                        for exponent = ,initial-exponent then (cl:- exponent dir)
                        for mantissa = magnitude then (cl:* mantissa (cl:expt 2 dir))
                        until (and (cl:<= 1 mantissa) (cl:< mantissa 2))
                        ;; the calls to CL:ROUND in this FINALLY
                        ;; clause are the representation of the FPU
                        ;; rounding mode.
                        finally (let ((%mantissa (cl:round (cl:* (cl:1- mantissa) (cl:expt 2 ,n-mantissa-bits)))))
                                  (when (cl:= %mantissa (cl:expt 2 ,n-mantissa-bits))
                                    (incf exponent)
                                    (setf %mantissa 0))
                                  (when (cl:>= exponent ,max-exponent)
                                    (setf exponent ,max-exponent %mantissa 0))
                                  (when (cl:<= exponent 0)
                                    (setf %mantissa (cl:round (cl:* mantissa (cl:expt 2 (cl:+ -1 ,n-mantissa-bits exponent))))
                                          exponent 0))
                                  (return (,bits-fun sign exponent %mantissa)))))))))
  (def %single-float-rational sb-kernel::single-from-bits 8 23)
  (def %double-float-rational sb-kernel::double-from-bits 11 52))

(defun flonum-from-rational (rational format)
  (ecase format
    (single-float (%single-float-rational rational))
    (double-float (%double-float-rational rational))))

(defun parse-xfloat-math-file (stream table)
  ;; Ensure that we're reading the correct variant of the file
  ;; in case there is more than one set of floating-point formats.
  (assert (eq (read stream) :default))
  (let ((pkg (make-package "SB-FLOAT-GENIE" :use '("CL")))
        (line 0))
    (import 'make-single-float pkg)
    (import 'make-double-float pkg)
    (unwind-protect
         (dolist (expr (let ((*package* pkg)) (read stream)))
           (incf line)
           (destructuring-bind (fun args . values) expr
             (let* ((key (cons fun args))
                    (existsp (gethash key table)))
               (when existsp
                 (error "Line ~D of float cache: ~S is repeated" line key))
               (setf (gethash key table) values))))
      (delete-package pkg))))

(defun get-float-ops-cache (&aux (cache sb-cold::*math-ops-memoization*))
  (when (atom cache)
    (return-from get-float-ops-cache cache))
  (let ((table (car cache))
        (pathname))
    (when (= (hash-table-count table) 0)
      (with-open-file (stream (setq pathname (sb-cold::math-journal-pathname :input))
                              :if-does-not-exist nil)
        (when stream
          (parse-xfloat-math-file stream table)
          (setf (cdr cache) (hash-table-count table))
          (when cl:*compile-verbose*
            (format t "~&; Math journal: prefilled ~D entries from ~S~%"
                    (cdr cache) pathname)))))
    table))

(defun record-math-op (fun args &rest values)
  (let* ((cache sb-cold::*math-ops-memoization*)
         (table (if (atom cache) cache (car cache))))
    (setf (gethash (cons fun args) table) values))
  (values-list values))

;;; Disallow non-canonical symbols in the float math cache,
;;; or it gets very confusing as to whether the cache is dirty.
(defun canonical-math-op-args (expr)
  ;; Try to avoid consing a new list unless we have to.
  (labels ((check (expr)
             (cond ((consp expr) (and (check (car expr)) (check (cdr expr))))
                   ((symbolp expr) (eq (cl:symbol-package expr) *cl-package*))
                   (t)))
           (recons (expr)
             (cond ((consp expr) (cons (recons (car expr)) (recons (cdr expr))))
                   ((symbolp expr) (intern (string expr) *cl-package*))
                   (t expr))))
    (if (check expr) expr (recons expr))))

(defmacro with-memoized-math-op ((name key-expr) &body calculation)
  (assert (symbolp name))
  ;; In theory I could make this so that only a cache miss has to call SANIFY-MATH-OP-ARGS
  ;; so that in the frequently-occuring cases we do not have to make an extra pass over
  ;; the expression to determine whether was is canonical. But since only COERCE can have
  ;; a problem of non-canononical symbols, it's easiest to just always canonicalize
  ;; for COERCE, and nothing else.
  `(let ((fun ',(intern (string name) "CL"))
         (args ,(if (or (string= name 'coerce)
                        (string= name 'read-from-string))
                    `(canonical-math-op-args ,key-expr)
                    key-expr)))
     (multiple-value-bind (answer foundp)
         (dx-let ((cache-key (cons fun args)))
           (gethash cache-key (get-float-ops-cache)))
       (if foundp
           (values-list answer)
           (multiple-value-call #'record-math-op fun args (progn ,@calculation))))))

(defvar *read-default-float-format* 'single-float)

(defun sb-cold::read-target-float-from-string (string)
  (when *read-suppress*
    (return-from sb-cold::read-target-float-from-string nil))
  (multiple-value-bind (flonum nchars)
      (with-memoized-math-op (read-from-string (list *read-default-float-format* (copy-seq string)))
        (let* ((marker-pos
                 (position-if (lambda (x)
                                (member x '(#\E #\S #\F #\D #\L) :test #'char-equal))
                              string))
               (exp-marker (if (and marker-pos
                                    (char-not-equal (char string marker-pos) #\E))
                               (char-upcase (char string marker-pos))
                               (ecase *read-default-float-format*
                                 ((single-float short-float) #\F)
                                 ((double-float) #\D)
                                 ((long-float) #\L))))
               (significand (if marker-pos (subseq string 0 marker-pos) string))
               (dot-pos (position #\. significand))
               (integer (if (eql dot-pos 0) 0 (parse-integer significand :start 0 :end dot-pos)))
               (fraction (if (and dot-pos (cl:> (length significand) (1+ dot-pos)))
                             (cl:/ (parse-integer significand :start (1+ dot-pos))
                                   (cl:expt 10 (cl:- (length significand) (1+ dot-pos))))
                             0))
               (exponent (if marker-pos
                             (parse-integer string :start (1+ marker-pos))
                             0))
               (rational (cl:* (if (char= (char string 0) #\-)
                                   (cl:- integer fraction)
                                   (cl:+ integer fraction))
                               (cl:expt 10 exponent)))
               (format (ecase exp-marker
                         ((#\F #\S) 'single-float)
                         ((#\D #-long-float #\L) 'double-float)
                         #+long-float ((#\L) 'long-float))))
          ;; Since we are working with rationals, we must special-case
          ;; negative zero (which does not have a natural rational
          ;; representation: explicitly look for -0 string.
          (if (or (string= significand "-0.0")
                  (string= significand "-.0")
                  (and (or (string= significand "-0") (string= significand "-0."))
                       (or marker-pos (error "~S has integer syntax" string))))
              (ecase format
                (single-float (values #.(make-single-float (ash -1 31))
                                      (length string)))
                (double-float (values #.(%make-double-float (ash -1 63))
                                      (length string))))
              (let ((result (flonum-from-rational rational format)))
                (values result (length string))))))
    (declare (ignore nchars))
    flonum))
