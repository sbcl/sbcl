;;;; Disassembler integration for the statistical profiler
;;;;
;;;; Copyright (C) 2003 Gerd Moellmann <gerd.moellmann@t-online.de>
;;;; All rights reserved.

(in-package #:sb-sprof)

(defun sample-pc-from-pc-or-offset (sample pc-or-offset)
  (etypecase sample
    ;; Assembly routines or foreign functions don't move around, so we've
    ;; stored a raw PC
    ((or sb-kernel:code-component string)
     pc-or-offset)
    ;; Lisp functions might move, so we've stored a offset from the
    ;; start of the code component.
    (sb-di::compiled-debug-fun
     (let* ((component (sb-di::compiled-debug-fun-component sample))
            (start-pc (code-start component)))
       (+ start-pc pc-or-offset)))))

(defun add-disassembly-profile-note (chunk stream dstate)
  (declare (ignore chunk stream))
  (when *samples*
    (let* ((location (+ (sb-disassem::seg-virtual-location
                         (sb-disassem:dstate-segment dstate))
                        (sb-disassem::dstate-cur-offs dstate)))
           (samples (loop with index = (samples-index *samples*)
                       for x from 0 below (- index 2) by 2
                       for last-sample = nil then sample
                       for sample = (aref (samples-vector *samples*) x)
                       for pc-or-offset = (aref (samples-vector *samples*)
                                                (1+ x))
                       when (and sample (eq last-sample 'trace-start))
                       count (= location
                                (sample-pc-from-pc-or-offset sample
                                                             pc-or-offset)))))
      (unless (zerop samples)
        (sb-disassem::note (format nil "~A/~A samples"
                                   samples (samples-trace-count *samples*))
                           dstate)))))

(pushnew 'add-disassembly-profile-note sb-disassem::*default-dstate-hooks*)
