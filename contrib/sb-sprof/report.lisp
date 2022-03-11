;;;; Reporting functions of the statistical profiler
;;;;
;;;; Copyright (C) 2003 Gerd Moellmann <gerd.moellmann@t-online.de>
;;;; All rights reserved.

(in-package #:sb-sprof)

(defconstant +alloc-region-size+
  #-gencgc
  sb-c:+backend-page-bytes+
  #+gencgc
  (max sb-vm:gencgc-alloc-granularity sb-vm:gencgc-page-bytes))

(deftype report-type ()
  '(member nil :flat :graph))

(deftype report-sort-key ()
  '(member :samples :cumulative-samples))

(deftype report-sort-order ()
  '(member :descending :ascending))


;;;; Reporting

(defun print-separator (&key (length 72) (char #\-))
  (format t "~&~V,,,V<~>~%" length char))

(defun samples-percent (call-graph count)
  (if (> count 0)
      (* 100.0 (/ count (call-graph-nsamples call-graph)))
      0))

(defun print-call-graph-header (call-graph)
  (let ((nsamples (call-graph-nsamples call-graph))
        (interval (call-graph-sample-interval call-graph))
        (ncycles (loop for v in (graph-vertices call-graph)
                    count (scc-p v))))
    (if (eq (call-graph-sampling-mode call-graph) :alloc)
        (format t "~2&Number of samples:     ~d~%~
                      Unique traces:         ~d~%~
                      Alloc interval:        ~a regions (approximately ~a kB)~%~
                      Total sampling amount: ~a regions (approximately ~a kB)"
                nsamples
                (call-graph-unique-trace-count call-graph)
                interval
                (truncate (* interval +alloc-region-size+) 1024)
                (* nsamples interval)
                (truncate (* nsamples interval +alloc-region-size+) 1024))
        (format t "~2&Number of samples:   ~d~%~
                      Sample interval:     ~f seconds~%~
                      Total sampling time: ~f seconds"
                nsamples
                interval
                (* nsamples interval)))
    (format t "~%Graph cycles:        ~d~%~
               Sampled threads:~%" ncycles)
    (loop for (thread bytes-used bytes-reserved buckets-used)
          in (call-graph-sampled-threads call-graph)
          do (format t "   ~a (~d/~d bytes, ~d hash buckets)~%"
                     thread bytes-used bytes-reserved buckets-used))
    (terpri)))

(declaim (type report-sort-key *report-sort-by*))
(defvar *report-sort-by* :samples
  "Method for sorting the flat report: either by :SAMPLES or by :CUMULATIVE-SAMPLES.")

(declaim (type report-sort-order  *report-sort-order*))
(defvar *report-sort-order* :descending
  "Order for sorting the flat report: either :DESCENDING or :ASCENDING.")

(defun make-node-comparator (key order)
  (declare (optimize speed))
  (let ((cmp (ecase order
               (:descending #'>)
               (:ascending #'<))))
    (multiple-value-bind (primary secondary)
        (ecase key
          (:samples (values #'node-count #'node-accrued-count))
          (:cumulative-samples (values #'node-accrued-count #'node-count)))
      (lambda (x y)
        (let ((px (the fixnum (funcall primary x)))
              (py (the fixnum (funcall primary y))))
          (if (/= px py)
              (funcall cmp px py)
              (let ((sx (the fixnum (funcall secondary x)))
                    (sy (the fixnum (funcall secondary y))))
                (if (/= sx sy)
                    (funcall cmp sx sy)
                    (in-caller-closure-p y x)))))))))

(defun print-flat (call-graph &key (stream *standard-output*) max
                                   min-percent (print-header t)
                                   (sort-by *report-sort-by*)
                                   (sort-order *report-sort-order*))
  (declare (type report-sort-order sort-order)
           (type report-sort-key sort-by))
  (let ((*standard-output* stream)
        (*print-pretty* nil)
        (total-count 0)
        (total-percent 0)
        (min-count (if min-percent
                       (round (* (/ min-percent 100.0)
                                 (call-graph-nsamples call-graph)))
                       0)))
    (when print-header
      (print-call-graph-header call-graph))
    (format t "~&           Self        Total        Cumul~%")
    (format t "~&  Nr  Count     %  Count     %  Count     %    Calls  Function~%")
    (print-separator)
    (let ((elsewhere-count (call-graph-elsewhere-count call-graph))
          (i 0)
          (nodes (stable-sort (copy-list (graph-vertices call-graph))
                              (make-node-comparator sort-by sort-order))))
      (dolist (node nodes)
        (when (or (and max (> (incf i) max))
                  (< (node-count node) min-count))
          (return))
        (let* ((count (node-count node))
               (percent (samples-percent call-graph count))
               (accrued-count (node-accrued-count node))
               (accrued-percent (samples-percent call-graph accrued-count)))
          (incf total-count count)
          (incf total-percent percent)
          (format t "~&~4d ~6d ~5,1f ~6d ~5,1f ~6d ~5,1f ~8@a  "
                  (incf i)
                  count
                  percent
                  accrued-count
                  accrued-percent
                  total-count
                  total-percent
                  (or (node-call-count node) "-"))
          (if (stringp (node-name node))
              (format t "~a~%" (node-name node))
              (format t "~s~%" (node-name node)))
          (finish-output)))
      (print-separator)
      (format t "~&     ~6d ~5,1f~36a elsewhere~%"
              elsewhere-count
              (samples-percent call-graph elsewhere-count)
              ""))))

(defun print-cycles (call-graph)
  (when (some #'cycle-p (graph-vertices call-graph))
    (format t "~&                            Cycle~%")
    (format t "~& Count     %                   Parts~%")
    (do-vertices (node call-graph)
      (when (cycle-p node)
        (flet ((print-info (indent index count percent name)
                 (format t "~&~6d ~5,1f ~11@t ~V@t  "
                         count percent indent)
                 (if (stringp name)
                     (format t "~a" name)
                     (format t "~s" name))
                 (format t " [~d]~%" index)))
          (print-separator)
          (format t "~&~6d ~5,1f                ~a...~%"
                  (node-count node)
                  (samples-percent call-graph (cycle-count node))
                  (node-name node))
          (dolist (v (vertex-scc-vertices node))
            (print-info 4 (node-index v) (node-count v)
                        (samples-percent call-graph (node-count v))
                        (node-name v))))))
    (print-separator)
    (format t "~2%")))

(defun print-graph (call-graph &key (stream *standard-output*)
                                 max min-percent)
  (let ((*standard-output* stream)
        (*print-pretty* nil))
    (print-call-graph-header call-graph)
    (print-cycles call-graph)
    (flet ((find-call (from to)
             (find to (node-edges from) :key #'call-vertex))
           (print-info (indent index count percent name)
             (format t "~&~6d ~5,1f ~11@t ~V@t  "
                     count percent indent)
             (if (stringp name)
                 (format t "~a" name)
                 (format t "~s" name))
             (format t " [~d]~%" index)))
      (format t "~&                               Callers~%")
      (format t "~&                 Total.     Function~%")
      (format t "~& Count     %  Count     %      Callees~%")
      (do-vertices (node call-graph (make-node-comparator
                                     :samples :descending))
        (print-separator)
        ;;
        ;; Print caller information.
        (dolist (caller (node-callers node))
          (let ((call (find-call caller node)))
            (print-info 4 (node-index caller)
                        (call-count call)
                        (samples-percent call-graph (call-count call))
                        (node-name caller))))
        ;; Print the node itself.
        (format t "~&~6d ~5,1f ~6d ~5,1f   "
                (node-count node)
                (samples-percent call-graph (node-count node))
                (node-accrued-count node)
                (samples-percent call-graph (node-accrued-count node)))
        (if (stringp (node-name node))
            (format t "~a" (node-name node))
            (format t "~s" (node-name node)))
        (format t " [~d]~%" (node-index node))
        ;; Print callees.
        (do-edges (call called node)
          (print-info 4 (node-index called)
                      (call-count call)
                      (samples-percent call-graph (call-count call))
                      (node-name called))))
      (print-separator)
      (format t "~2%")
      (print-flat call-graph :stream stream :max max
                  :min-percent min-percent :print-header nil))))

(defun report (&key (type :graph) max min-percent call-graph
                 ((:sort-by *report-sort-by*) *report-sort-by*)
                 ((:sort-order *report-sort-order*) *report-sort-order*)
                 (stream *standard-output*) ((:show-progress *show-progress*)))
  "Report statistical profiling results.  The following keyword
   args are recognized:

   :TYPE <type>
      Specifies the type of report to generate.  If :FLAT, show
      flat report, if :GRAPH show a call graph and a flat report.
      If nil, don't print out a report.

   :STREAM <stream>
      Specify a stream to print the report on.  Default is
      *STANDARD-OUTPUT*.

   :MAX <max>
      Don't show more than <max> entries in the flat report.

   :MIN-PERCENT <min-percent>
      Don't show functions taking less than <min-percent> of the
      total time in the flat report.

   :SORT-BY <column>
      If :SAMPLES, sort flat report by number of samples taken.
      If :CUMULATIVE-SAMPLES, sort flat report by cumulative number of samples
      taken (shows how much time each function spent on stack.) Default
      is *REPORT-SORT-BY*.

   :SORT-ORDER <order>
      If :DESCENDING, sort flat report in descending order. If :ASCENDING,
      sort flat report in ascending order. Default is *REPORT-SORT-ORDER*.

   :SHOW-PROGRESS <bool>
     If true, print progress messages while generating the call graph.

   :CALL-GRAPH <graph>
     Print a report from <graph> instead of the latest profiling
     results.

Value of this function is a CALL-GRAPH object representing the
resulting call-graph, or NIL if there are no samples (eg. right after
calling RESET.)

Profiling is stopped before the call graph is generated."
  (acond (*samples*
          (let ((graph (or call-graph (make-call-graph it most-positive-fixnum))))
           (ecase type
             (:flat
              (print-flat graph :stream stream :max max :min-percent min-percent))
             (:graph
              (print-graph graph :stream stream :max max :min-percent min-percent))
             ((nil)))
           graph))
         (t
          (format stream "~&; No samples to report.~%")
          nil)))
