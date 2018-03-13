;;;; Disassembler integration for the statistical profiler
;;;;
;;;; Copyright (C) 2003 Gerd Moellmann <gerd.moellmann@t-online.de>
;;;; All rights reserved.

(in-package #:sb-sprof)

(defun add-disassembly-profile-note (chunk stream dstate)
  (declare (ignore chunk stream))
  (when *samples*
    (let ((samples *samples*)
          (location (+ (sb-disassem::seg-virtual-location
                        (sb-disassem:dstate-segment dstate))
                       (sb-disassem::dstate-cur-offs dstate)))
          (hits 0))
      (declare (type index hits))
      (map-all-samples (lambda (debug-info pc-or-offset)
                         (when (= location (sample-pc debug-info pc-or-offset))
                           (incf hits)))
                       samples)
      (unless (zerop hits)
        (let* ((total-count (samples-trace-count samples))
               (width (length (write-to-string total-count :base 10))))
          (sb-disassem::note (format nil "~VD/~VD samples"
                                     width hits width total-count)
                             dstate))))))

(pushnew 'add-disassembly-profile-note sb-disassem::*default-dstate-hooks*)
