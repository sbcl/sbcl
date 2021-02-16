;;;; Package definition for the statistical profiler
;;;;
;;;; Copyright (C) 2003 Gerd Moellmann <gerd.moellmann@t-online.de>
;;;; All rights reserved.

(defpackage #:sb-sprof
  (:use #:cl #:sb-ext #:sb-unix #:sb-alien #:sb-sys #:sb-int)
  (:export
   ;; Recording
   #:start-sampling #:stop-sampling #:with-sampling
   #:map-traces
   #:sample-pc

   ;; Call counting
   #:profile-call-counts #:unprofile-call-counts

   ;; Reporting
   #:*report-sort-by* #:*report-sort-order*
   #:report

   ;; Interface
   #:*sample-interval* #:*max-samples*
   #:start-profiling #:stop-profiling #:with-profiling
   #:reset))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (sb-int:system-package-p (find-package "SB-SPROF")) t))
