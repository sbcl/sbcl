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
    (let ((samples *samples*)
          (location (+ (sb-disassem::seg-virtual-location
                        (sb-disassem:dstate-segment dstate))
                       (sb-disassem::dstate-cur-offs dstate)))
          (hits 0))
      (declare (type index hits))
      (map-all-samples (lambda (debug-info pc-or-offset)
                         (when (= location (sample-pc-from-pc-or-offset
                                            debug-info pc-or-offset))
                           (incf hits)))
                       samples)
      (unless (zerop hits)
        (let* ((total-count (samples-trace-count samples))
               (width (length (write-to-string total-count :base 10))))
          (sb-disassem::note (format nil "~VD/~VD samples"
                                     width hits width total-count)
                             dstate))))))

(pushnew 'add-disassembly-profile-note sb-disassem::*default-dstate-hooks*)
