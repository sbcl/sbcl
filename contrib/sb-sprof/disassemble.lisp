;;;; Disassembler integration for the statistical profiler
;;;;
;;;; Copyright (C) 2003 Gerd Moellmann <gerd.moellmann@t-online.de>
;;;; All rights reserved.

(in-package #:sb-sprof)

(defun sample-counts-by-pc (samples)
  (let* ((unique-pc-count-guess (isqrt (truncate (samples-index samples) 2)))
         (sample-counts-by-pc (make-hash-table :size unique-pc-count-guess)))
    (map-all-pc-locs (lambda (info pc-or-offset)
                       (let ((pc (sample-pc info pc-or-offset)))
                         (incf (gethash pc sample-counts-by-pc 0))))
                     samples)
    (values sample-counts-by-pc (samples-index samples))))

(declaim (ftype (sfunction (t hash-table) index) pc-sample-count))
(defun pc-sample-count (pc sample-counts-by-pc)
  (values (gethash pc sample-counts-by-pc 0)))

;;; Caches hash-tables returned by SAMPLE-COUNTS-BY-PC in a weak
;;; hash-table keyed on SAMPLES instances.
;;;
;;; To detect extensions of a SAMPLES instance for which there already
;;; is an entry in the cache, compare the current value of
;;; (samples-index SAMPLES) to the value for which the cache entry was
;;; created.
(defvar *cached-sample-counts*
  (make-hash-table :test #'eq :weakness :key :synchronized t))

(defun ensure-sample-counts (samples)
  (let ((table *cached-sample-counts*))
    (destructuring-bind (&optional counts-by-pc previous-index)
        (gethash samples table)
      (cond
        ((not (and counts-by-pc previous-index))
         (multiple-value-bind (counts-by-pc previous-index)
             (sample-counts-by-pc samples)
           (setf (gethash samples table) (list counts-by-pc previous-index))
           counts-by-pc))
        ((not (= (samples-index samples) previous-index))
         (remhash samples table)
         (ensure-sample-counts samples))
        (t
         counts-by-pc)))))

(defun add-disassembly-profile-note (chunk stream dstate)
  (declare (ignore chunk stream))
  (binding* ((samples *samples* :exit-if-null)
             (counts (ensure-sample-counts samples))
             (location (sb-disassem:dstate-cur-addr dstate))
             (count (pc-sample-count location counts)))
      (unless (zerop count)
        (let* ((total-count (samples-trace-count samples))
               (width (length (write-to-string total-count :base 10))))
          (sb-disassem::note (format nil "~VD/~VD samples"
                                     width count width total-count)
                             dstate)))))

(pushnew 'add-disassembly-profile-note sb-disassem::*default-dstate-hooks*)
