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

(defvar *floating-point-number-buffer* (make-array 100 :element-type 'character))

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
         (args ,(if (string= name 'coerce)
                    `(canonical-math-op-args ,key-expr)
                    key-expr)))
     (multiple-value-bind (answer foundp)
         (dx-let ((cache-key (cons fun args)))
           (gethash cache-key (get-float-ops-cache)))
       (if foundp
           (values-list answer)
           (multiple-value-call #'record-math-op fun args (progn ,@calculation))))))

(defun sb-cold::read-target-float (stream char)
  (let ((buffer *floating-point-number-buffer*)
        (index -1)
        string)
    (loop (setq char (read-char stream))
          (cond ((or (digit-char-p char)
                     (member char '(#\+ #\- #\. #\D #\E #\F #\L #\S) :test #'char-equal))
                 (setf (aref buffer (incf index)) char))
                (t
                 (unread-char char stream)
                 (return))))
    (when *read-suppress*
      (return-from sb-cold::read-target-float nil))
    (setf string (subseq buffer 0 (1+ index)))
    (multiple-value-bind (flonum nchars)
        (with-memoized-math-op (read-from-string (list *read-default-float-format* string))
          (let* ((marker-pos
                   (position-if (lambda (x)
                                  (member x '(#\E #\S #\F #\D #\L) :test #'char-equal))
                                string))
                 (exp-marker (if (and marker-pos
                                      (char-not-equal (char string marker-pos) #\E))
                                 (char-upcase (char string marker-pos))
                                 (ecase cl:*read-default-float-format*
                                   ((cl:single-float cl:short-float) #\F)
                                   ((cl:double-float cl:long-float)  #\D))))
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
                           ((#\D #\L) 'double-float))))
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
      flonum)))
