;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;;; Due to a feature of genesis, symbols created as external in package-data-list
;;;; that are not referenced when cross-compiling do not make it into the core.
;;;; The benefit is that we automatically weed out junk. The drawback is that
;;;; symbols which want to be external might have to be exported "again".
;;;; On the whole, the benefit outweights the drawback, imho.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'sb-kernel::profile-deinit "SB-KERNEL"))

(in-package "SB-PROFILE")


;;;; COUNTER object
;;;;
;;;; Thread safe, and reasonably fast: in common case increment is just an
;;;; ATOMIC-INCF, in overflow case grab a lock and increment overflow counter.

(declaim (inline make-counter))
(defstruct (counter (:constructor make-counter) (:copier nil))
  (word 0 :type sb-vm:word)
  (overflow 0 :type unsigned-byte))
(declaim (freeze-type counter))

(defun incf-counter (counter delta)
  ;; When running multi-threaded we can easily get negative numbers for the
  ;; cons-counter. Don't count them at all.
  (when (plusp delta)
    (labels ((%incf-overflow (&optional (n 1))
               ;; Overflow-counter can run into bignums... so we need to loop
               ;; around CAS till the increment succeeds.
               (loop for old = (counter-overflow counter)
                     until (eq old (compare-and-swap (counter-overflow counter)
                                                     old (+ old n)))))
             (%incf (d)
               ;; Increment the word-sized counter. If it overflows, record the
               ;; overflow.
               (let ((prev (atomic-incf (counter-word counter) d)))
                 (when (< (logand most-positive-word (+ prev d)) prev)
                   (%incf-overflow)))))
      ;; DELTA can potentially be a bignum -- cut it down to word-size.
      (unless (typep delta 'sb-vm:word)
        (multiple-value-bind (n r) (truncate delta (1+ most-positive-word))
          (%incf-overflow n)
          (setf delta r)))
      ;; ATOMIC-INCF can at most handle SIGNED-WORD: if DELTA doesn't fit that,
      ;; DELTA/2 will.
      (if (typep delta 'sb-vm:signed-word)
          (%incf delta)
          ;; ...and if delta is still too big, split it into four parts: they
          ;; are guaranteed to fit into a signed word.
          (multiple-value-bind (n r) (truncate delta 2)
            (%incf n)
            (%incf n)
            (%incf r)))))
  counter)

(defun counter-count (counter)
  (+ (counter-word counter)
     (* (counter-overflow counter) (1+ most-positive-word))))

;;;; High resolution timer

(defconstant +ticks-per-second+ internal-time-units-per-second)

(declaim (inline get-internal-ticks))
(defun get-internal-ticks ()
  (get-internal-run-time))

;;;; global data structures

;;; We associate a PROFILE-INFO structure with each profiled function
;;; name. This holds the functions that we call to manipulate the
;;; closure which implements the encapsulation.
(defvar *profiled-fun-name->info*
  (make-hash-table
   ;; EQL testing isn't good enough for generalized function names
   ;; like (SETF FOO).
   :test 'equal
   :synchronized t))
(defstruct (profile-info (:copier nil))
  (name              (missing-arg) :read-only t)
  (encapsulated-fun  (missing-arg) :type function :read-only t)
  (encapsulation-fun (missing-arg) :type function :read-only t)
  (read-stats-fun    (missing-arg) :type function :read-only t)
  (clear-stats-fun   (missing-arg) :type function :read-only t))
(declaim (freeze-type profile-info))

;;; These variables are used to subtract out the time and consing for
;;; recursive and other dynamically nested profiled calls. The total
;;; resource consumed for each nested call is added into the
;;; appropriate variable. When the outer function returns, these
;;; amounts are subtracted from the total.
(declaim (counter *enclosed-ticks* *enclosed-consing*))
(defvar *enclosed-ticks*)
(defvar *enclosed-consing*)

;;; This variable is also used to subtract out time for nested
;;; profiled calls. The time inside the profile wrapper call --
;;; between its two calls to GET-INTERNAL-TICKS -- is accounted
;;; for by the *ENCLOSED-TIME* variable. However, there's also extra
;;; overhead involved, before we get to the first call to
;;; GET-INTERNAL-TICKS, and after we get to the second call. By
;;; keeping track of the count of enclosed profiled calls, we can try
;;; to compensate for that.
(declaim (counter *enclosed-profiles*))
(defvar *enclosed-profiles*)

(declaim (counter *enclosed-gc-run-time*))
(defvar *enclosed-gc-run-time*)

;;; the encapsulated function we're currently computing profiling data
;;; for, recorded so that we can detect the problem of
;;; PROFILE-computing machinery calling a function which has itself
;;; been PROFILEd
(defvar *computing-profiling-data-for*)

;;; the components of profiling overhead
(defstruct (overhead (:copier nil))
  ;; the number of ticks a bare function call takes. This is
  ;; factored into the other overheads, but not used for itself.
  (call (missing-arg) :type single-float :read-only t)
  ;; the number of ticks that will be charged to a profiled
  ;; function due to the profiling code
  (internal (missing-arg) :type single-float :read-only t)
  ;; the number of ticks of overhead for profiling that a single
  ;; profiled call adds to the total runtime for the program
  (total (missing-arg) :type single-float :read-only t))
(declaim (freeze-type overhead))
(defvar *overhead*)
(declaim (type overhead *overhead*))
(makunbound '*overhead*) ; in case we reload this file when tweaking

;;;; profile encapsulations

;;; Return a collection of closures over the same lexical context,
;;;   (VALUES ENCAPSULATION-FUN READ-STATS-FUN CLEAR-STATS-FUN).
;;;
;;; ENCAPSULATION-FUN is a plug-in replacement for ENCAPSULATED-FUN,
;;; which updates statistics whenever it's called.
;;;
;;; READ-STATS-FUN returns the statistics:
;;;   (VALUES COUNT TIME CONSING PROFILE).
;;; COUNT is the count of calls to ENCAPSULATION-FUN. TICKS is
;;; the total number of ticks spent in ENCAPSULATED-FUN.
;;; CONSING is the total consing of ENCAPSULATION-FUN. PROFILE is the
;;; number of calls to the profiled function, stored for the purposes
;;; of trying to estimate that part of profiling overhead which occurs
;;; outside the interval between the profile wrapper function's timer
;;; calls.
;;;
;;; CLEAR-STATS-FUN clears the statistics.
;;;
;;; (The reason for implementing this as coupled closures, with the
;;; counts built into the lexical environment, is that we hope this
;;; will minimize profiling overhead.)
(defun profile-encapsulation-lambdas ()
  (declare (muffle-conditions compiler-note))
  (let* ((count (make-counter))
         (ticks (make-counter))
         (consing (make-counter))
         (profiles (make-counter))
         (gc-run-time (make-counter)))
    (declare (counter count ticks consing profiles gc-run-time))
    (values
     ;; ENCAPSULATION-FUN
     (lambda (function &rest args)
       (declare (optimize speed safety)
                (function function))
       ;; Make sure that we're not recursing infinitely.
       (when (boundp '*computing-profiling-data-for*)
         (unprofile-all)                ; to avoid further recursion
         (error "~@<When computing profiling data for ~S, the profiled ~
                    function ~S was called. To get out of this infinite recursion, all ~
                    functions have been unprofiled. (Since the profiling system evidently ~
                    uses ~S in its computations, it looks as though it's a bad idea to ~
                    profile it.)~:@>"
                *computing-profiling-data-for* function function))
       (incf-counter count 1)
       (let ((dticks 0)
             (dconsing 0)
             (inner-enclosed-profiles 0)
             (dgc-run-time 0))
         (declare (truly-dynamic-extent dticks dconsing inner-enclosed-profiles))
         (unwind-protect
              (let ((start-ticks (get-internal-ticks))
                    (start-gc-run-time *gc-run-time*)
                    (*enclosed-ticks* (make-counter))
                    (*enclosed-consing* (make-counter))
                    (*enclosed-profiles* (make-counter))
                    (nbf0 *n-bytes-freed-or-purified*)
                    (dynamic-usage-0 (sb-kernel:dynamic-usage))
                    (*enclosed-gc-run-time* (make-counter)))
                (declare (dynamic-extent *enclosed-ticks* *enclosed-consing*
                                         *enclosed-profiles* *enclosed-gc-run-time*))
                (unwind-protect
                     (apply function args)
                  (let ((*computing-profiling-data-for* function)
                        (dynamic-usage-1 (sb-kernel:dynamic-usage)))
                    (setf dticks (- (get-internal-ticks) start-ticks)
                          dconsing (if (eql *n-bytes-freed-or-purified* nbf0)
                                       ;; common special case where we can avoid
                                       ;; bignum arithmetic
                                       (- dynamic-usage-1 dynamic-usage-0)
                                       ;; general case
                                       (- (get-bytes-consed) nbf0 dynamic-usage-0))
                          inner-enclosed-profiles (counter-count *enclosed-profiles*)
                          dgc-run-time (- *gc-run-time* start-gc-run-time))
                    (incf-counter ticks (- dticks (counter-count *enclosed-ticks*)))
                    (incf-counter gc-run-time (- dgc-run-time (counter-count *enclosed-gc-run-time*)))
                    (incf-counter consing (- dconsing (counter-count *enclosed-consing*)))
                    (incf-counter profiles inner-enclosed-profiles))))
           (when (boundp '*enclosed-ticks*)
             (incf-counter *enclosed-ticks* dticks)
             (incf-counter *enclosed-consing* dconsing)
             (incf-counter *enclosed-profiles* (1+ inner-enclosed-profiles))
             (incf-counter *enclosed-gc-run-time* dgc-run-time)))))
     ;; READ-STATS-FUN
     (lambda ()
       (values (counter-count count)
               (counter-count ticks)
               (counter-count consing)
               (counter-count profiles)
               (counter-count gc-run-time)))
     ;; CLEAR-STATS-FUN
     (lambda ()
       (setf count (make-counter)
             ticks (make-counter)
             consing (make-counter)
             profiles (make-counter)
             gc-run-time (make-counter))))))

;;;; interfaces

;;; A symbol or (SETF FOO) list names a function, a string names all
;;; the functions named by symbols in the named package.
(defun mapc-on-named-funs (function names)
  (dolist (name names)
    (etypecase name
      (symbol (funcall function name))
      (list
       (legal-fun-name-or-type-error name)
       ;; Then we map onto it.
       (funcall function name))
      (string (let ((package (find-undeleted-package-or-lose name)))
                (do-symbols (symbol package)
                  (when (eq (symbol-package symbol) package)
                    (when (and (fboundp symbol)
                               (not (macro-function symbol))
                               (not (special-operator-p symbol)))
                      (funcall function symbol))
                    (let ((setf-name `(setf ,symbol)))
                      (when (fboundp setf-name)
                        (funcall function setf-name)))))))))
  (values))

;;; Profile the named function, which should exist and not be profiled
;;; already.
(defun profile-1-unprofiled-fun (name)
  (let ((encapsulated-fun (fdefinition name)))
    (multiple-value-bind (encapsulation-fun read-stats-fun clear-stats-fun)
        (profile-encapsulation-lambdas)
      (without-package-locks
        (encapsulate name 'profile encapsulation-fun))
      (setf (gethash name *profiled-fun-name->info*)
            (make-profile-info :name name
                               :encapsulated-fun encapsulated-fun
                               :encapsulation-fun encapsulation-fun
                               :read-stats-fun read-stats-fun
                               :clear-stats-fun clear-stats-fun))
      (values))))

;;; Profile the named function. If already profiled, unprofile first.
(defun profile-1-fun (name)
  (cond ((fboundp name)
         (when (gethash name *profiled-fun-name->info*)
           (warn "~S is already profiled, so unprofiling it first." name)
           (unprofile-1-fun name))
         (profile-1-unprofiled-fun name))
        (t
         (warn "ignoring undefined function ~S" name)))
  (values))

;;; Unprofile the named function, if it is profiled.
(defun unprofile-1-fun (name)
  (let ((pinfo (gethash name *profiled-fun-name->info*)))
    (cond (pinfo
           (remhash name *profiled-fun-name->info*)
           (without-package-locks
             (unencapsulate name 'profile)))
          (t
           (warn "~S is not a profiled function." name))))
  (values))

(defmacro profile (&rest names)
  "PROFILE Name*

   If no names are supplied, return the list of profiled functions.

   If names are supplied, wrap profiling code around the named functions.
   As in TRACE, the names are not evaluated. A symbol names a function.
   A string names all the functions named by symbols in the named
   package. If a function is already profiled, then unprofile and
   reprofile (useful to notice function redefinition.)  If a name is
   undefined, then we give a warning and ignore it. See also
   UNPROFILE, REPORT and RESET."
  (if (null names)
      `(loop for k being each hash-key in *profiled-fun-name->info*
             collecting k)
      `(mapc-on-named-funs #'profile-1-fun ',names)))

(defmacro unprofile (&rest names)
  "Unwrap any profiling code around the named functions, or if no names
  are given, unprofile all profiled functions. A symbol names
  a function. A string names all the functions named by symbols in the
  named package. NAMES defaults to the list of names of all currently
  profiled functions."
  (if names
      `(mapc-on-named-funs #'unprofile-1-fun ',names)
      `(unprofile-all)))

(defun unprofile-all ()
  (dohash ((name profile-info) *profiled-fun-name->info*
           :locked t)
    (declare (ignore profile-info))
    (unprofile-1-fun name)))

(defun reset ()
  "Reset the counters for all profiled functions."
  (dohash ((name profile-info) *profiled-fun-name->info* :locked t)
    (declare (ignore name))
    (funcall (profile-info-clear-stats-fun profile-info))))

;;;; reporting results

(defstruct (time-info (:copier nil))
  name
  calls
  seconds
  consing
  gc-run-time)
(declaim (freeze-type time-info))

;;; Return our best guess for the run time in a function, subtracting
;;; out factors for profiling overhead. We subtract out the internal
;;; overhead for each call to this function, since the internal
;;; overhead is the part of the profiling overhead for a function that
;;; is charged to that function.
;;;
;;; We also subtract out a factor for each call to a profiled function
;;; within this profiled function. This factor is the total profiling
;;; overhead *minus the internal overhead*. We don't subtract out the
;;; internal overhead, since it was already subtracted when the nested
;;; profiled functions subtracted their running time from the time for
;;; the enclosing function.
(defun compensate-time (calls ticks profile)
  (let ((raw-compensated
         (- (/ (float ticks) (float +ticks-per-second+))
            (* (overhead-internal *overhead*) (float calls))
            (* (- (overhead-total *overhead*)
                  (overhead-internal *overhead*))
               (float profile)))))
    (max raw-compensated 0.0)))

(defun report (&key limit (print-no-call-list t))
  "Report results from profiling. The results are approximately
adjusted for profiling overhead. The compensation may be rather
inaccurate when bignums are involved in runtime calculation, as in a
very-long-running Lisp process.

If LIMIT is set to an integer, only the top LIMIT results are
reported. If PRINT-NO-CALL-LIST is T (the default) then a list of
uncalled profiled functions are listed."
  (unless (boundp '*overhead*)
    (setf *overhead*
          (compute-overhead)))
  (let ((time-info-list ())
        (no-call-name-list ()))
    (dohash ((name pinfo) *profiled-fun-name->info* :locked t)
      (multiple-value-bind (calls ticks consing profile gc-run-time)
          (funcall (profile-info-read-stats-fun pinfo))
        (if (zerop calls)
            (push name no-call-name-list)
            (push (make-time-info :name name
                                  :calls calls
                                  :seconds (compensate-time calls
                                                            ticks
                                                            profile)
                                  :consing consing
                                  :gc-run-time gc-run-time)
                  time-info-list))))

    (let ((times
           (sort time-info-list
                 #'>=
                 :key #'time-info-seconds)))
      (print-profile-table
       (if (and limit (> (length times) limit))
           (subseq times 0 limit)
           times)))

    (when (and print-no-call-list no-call-name-list)
      (format *trace-output*
              "~%These functions were not called:~%~{~<~%~:; ~S~>~}~%"
              (sort no-call-name-list #'string<
                    :key (lambda (name)
                           (symbol-name (fun-name-block-name name))))))

    (values)))


(defun print-profile-table (time-info-list)
  (let ((total-seconds 0.0)
        (total-consed 0)
        (total-calls 0)
        (total-gc-run-time 0)
        (seconds-width (length "seconds"))
        (consed-width (length "consed"))
        (calls-width (length "calls"))
        (sec/call-width 10)
        (gc-run-time-width (length "gc"))
        (name-width 6))
    (dolist (time-info time-info-list)
      (incf total-seconds (time-info-seconds time-info))
      (incf total-consed (time-info-consing time-info))
      (incf total-calls (time-info-calls time-info))
      (incf total-gc-run-time (time-info-gc-run-time time-info)))
    (setf seconds-width (max (length (format nil "~10,3F" total-seconds))
                             seconds-width)
          calls-width (max (length (format nil "~:D" total-calls))
                           calls-width)
          consed-width (max (length (format nil "~:D" total-consed))
                            consed-width)
          gc-run-time-width (max (length (format nil "~10,3F" (/ total-gc-run-time internal-time-units-per-second)))
                            gc-run-time-width))

    (flet ((dashes ()
             (dotimes (i (+ seconds-width consed-width calls-width
                            sec/call-width name-width
                            (* 5 3)))
               (write-char #\- *trace-output*))
             (terpri *trace-output*)))
      (format *trace-output* "~&~@{ ~v:@<~A~>~^|~}~%"
              seconds-width "seconds"
              (1+ gc-run-time-width) "gc"
              (1+ consed-width) "consed"
              (1+ calls-width) "calls"
              (1+ sec/call-width) "sec/call"
              (1+ name-width) "name")

      (dashes)

      (dolist (time-info time-info-list)
        (format *trace-output* "~v,3F | ~v,3F | ~v:D | ~v:D | ~10,6F | ~S~%"
                seconds-width (time-info-seconds time-info)
                gc-run-time-width (/ (time-info-gc-run-time time-info) internal-time-units-per-second)
                consed-width (time-info-consing time-info)
                calls-width (time-info-calls time-info)
                (/ (time-info-seconds time-info)
                   (float (time-info-calls time-info)))
                (time-info-name time-info)))

      (dashes)

      (format *trace-output* "~v,3F | ~v,3F | ~v:D | ~v:D |            | Total~%"
                seconds-width total-seconds
                gc-run-time-width (/ total-gc-run-time internal-time-units-per-second)
                consed-width total-consed
                calls-width total-calls)

      (format *trace-output*
              "~%estimated total profiling overhead: ~4,2F seconds~%"
              (* (overhead-total *overhead*) (float total-calls)))
      (format *trace-output*
              "~&overhead estimation parameters:~%  ~Ss/call, ~Ss total profiling, ~Ss internal profiling~%"
              (overhead-call *overhead*)
              (overhead-total *overhead*)
              (overhead-internal *overhead*)))))


;;;; overhead estimation

;;; We average the timing overhead over this many iterations.
;;;
;;; (This is a variable, not a constant, so that it can be set in
;;; .sbclrc if desired. Right now, that's an unsupported extension
;;; that I (WHN) use for my own experimentation, but it might
;;; become supported someday. Comments?)
(declaim (type unsigned-byte *timer-overhead-iterations*))
(defparameter *timer-overhead-iterations*
  500000)

;;; a dummy function that we profile to find profiling overhead
(declaim (notinline compute-overhead-aux))
(defun compute-overhead-aux (x)
  (declare (ignore x)))

;;; Return a newly computed OVERHEAD object.
(defun compute-overhead ()
  (format *debug-io* "~&measuring PROFILE overhead..")
  (flet ((frob ()
           (let ((start (get-internal-ticks))
                 (fun (symbol-function 'compute-overhead-aux)))
             (declare (type function fun))
             (dotimes (i *timer-overhead-iterations*)
               (funcall fun fun))
             (/ (float (- (get-internal-ticks) start))
                (float +ticks-per-second+)
                (float *timer-overhead-iterations*)))))
    (let (;; Measure unprofiled calls to estimate call overhead.
          (call-overhead (frob))
          total-overhead
          internal-overhead)
      ;; Measure profiled calls to estimate profiling overhead.
      (unwind-protect
          (progn
            (profile compute-overhead-aux)
            (setf total-overhead
                  (- (frob) call-overhead)))
        (let* ((pinfo (gethash 'compute-overhead-aux
                               *profiled-fun-name->info*))
               (read-stats-fun (profile-info-read-stats-fun pinfo))
               (time (nth-value 1 (funcall read-stats-fun))))
          (setf internal-overhead
                (/ (float time)
                   (float +ticks-per-second+)
                   (float *timer-overhead-iterations*))))
        (unprofile compute-overhead-aux))
      (prog1
          (make-overhead :call call-overhead
                         :total total-overhead
                         :internal internal-overhead)
        (format *debug-io* "done~%")))))

;;; It would be bad to compute *OVERHEAD*, save it into a .core file,
;;; then load the old *OVERHEAD* value from the .core file into a
;;; different machine running at a different speed. We avoid this by
;;; erasing *CALL-OVERHEAD* whenever we save a .core file.
(defun profile-deinit ()
  (without-package-locks
    (makunbound '*overhead*)))
