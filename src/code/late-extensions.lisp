;;;; various extensions (including SB-INT "internal extensions")
;;;; available both in the cross-compilation host Lisp and in the
;;;; target SBCL, but which can't be defined on the target until until
;;;; some significant amount of machinery (e.g. error-handling) is
;;;; defined

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;; Is X a list for which LENGTH is meaningful, i.e. a list which is
;;; not improper and which is not circular?
(defun list-with-length-p (x)
  (values (ignore-errors (list-length x))))

;;; not used in 0.7.8, but possibly useful for defensive programming
;;; in e.g. (COERCE ... 'VECTOR)
;;;(defun list-length-or-die (x)
;;;  (or (list-length x)
;;;      ;; not clear how to do this best:
;;;      ;;   * Should this be a TYPE-ERROR? Colloquially that'd make
;;;      ;;     lots of sense, but since I'm not sure how to express
;;;      ;;     "noncircular list" as a Lisp type expression, coding
;;;      ;;     it seems awkward.
;;;      ;;   * Should the ERROR object include the offending value?
;;;      ;;     Ordinarily that's helpful, but if the user doesn't have
;;;      ;;     his printer set up to deal with cyclicity, we might not
;;;      ;;     be doing him a favor by printing the object here.
;;;      ;; -- WHN 2002-10-19
;;;      (error "can't calculate length of cyclic list")))

;;; This is used in constructing arg lists for debugger printing,
;;; and when needing to print unbound slots in PCL.
(defstruct (unprintable-object
            (:constructor make-unprintable-object (string))
            (:print-object (lambda (x s)
                             (print-unreadable-object (x s)
                               (write-string (unprintable-object-string x) s))))
            (:copier nil))
  (string nil :read-only t))

;;; Used internally, but it would be nice to provide something
;;; like this for users as well.
;;;
;;; FIXME / IMPORTANT: On backends without interleaved raw slots,
;;  if the slot is raw, the address is correct only for
;;; instances of the specified class, not its subclasses!
(defmacro define-structure-slot-addressor (name &key structure slot)
  (let* ((dd (find-defstruct-description structure t))
         (slotd (or (and dd (find slot (dd-slots dd) :key #'dsd-name))
                    (error "Slot ~S not found in ~S." slot structure)))
         (index (dsd-index slotd))
         #!-interleaved-raw-slots (rsd (sb!kernel::dsd-raw-slot-data slotd)))
    `(progn
       (declaim (inline ,name))
       (defun ,name (instance)
         (declare (type ,structure instance) (optimize speed))
         (truly-the
          word
          (+ (get-lisp-obj-address instance)
             ,(+ (- sb!vm:instance-pointer-lowtag)
                 #!+interleaved-raw-slots
                 (* (+ sb!vm:instance-slots-offset index) sb!vm:n-word-bytes)
                 #!-interleaved-raw-slots
                 (* (if (not rsd)
                        (+ sb!vm:instance-slots-offset index)
                        (- (1+ (sb!kernel::dd-instance-length dd))
                           sb!vm:instance-slots-offset index
                           (1- (sb!kernel::raw-slot-data-n-words rsd))))
                    sb!vm:n-word-bytes))))))))

;;;; ATOMIC-INCF and ATOMIC-DECF

(defun expand-atomic-frob (name specified-place diff env
                           &aux (place (sb!xc:macroexpand specified-place env)))
  (declare (type (member atomic-incf atomic-decf) name))
  (flet ((invalid-place ()
           (error "Invalid first argument to ~S: ~S" name specified-place))
         (compute-newval (old) ; used only if no atomic inc vop
           `(logand (,(case name (atomic-incf '+) (atomic-decf '-)) ,old
                     (the sb!vm:signed-word ,diff)) sb!ext:most-positive-word))
         (compute-delta () ; used only with atomic inc vop
           `(logand ,(case name
                      (atomic-incf `(the sb!vm:signed-word ,diff))
                      (atomic-decf `(- (the sb!vm:signed-word ,diff))))
                    sb!ext:most-positive-word)))
    (declare (ignorable #'compute-newval #'compute-delta))
    (if (and (symbolp place)
             (eq (info :variable :kind place) :global)
             (type= (info :variable :type place) (specifier-type 'fixnum)))
        ;; Global can't be lexically rebound.
        (return-from expand-atomic-frob
          `(truly-the fixnum (,(case name
                                (atomic-incf '%atomic-inc-symbol-global-value)
                                (atomic-decf '%atomic-dec-symbol-global-value))
                              ',place (the fixnum ,diff)))))

    (unless (consp place)
      (invalid-place))
    (destructuring-bind (op &rest args) place
      ;; FIXME: The lexical environment should not be disregarded.
      ;; CL builtins can't be lexically rebound, but structure accessors can.
      (case op
        (aref
         (unless (singleton-p (cdr args))
           (invalid-place))
         (with-unique-names (array)
           `(let ((,array (the (simple-array word (*)) ,(car args))))
              #!+compare-and-swap-vops
              (%array-atomic-incf/word
               ,array
               (check-bound ,array (array-dimension ,array 0) ,(cadr args))
               ,(compute-delta))
              #!-compare-and-swap-vops
              ,(with-unique-names (index old-value)
                `(without-interrupts
                  (let* ((,index ,(cadr args))
                         (,old-value (aref ,array ,index)))
                    (setf (aref ,array ,index) ,(compute-newval old-value))
                    ,old-value))))))
        ((car cdr first rest)
         (when (cdr args)
           (invalid-place))
         `(truly-the
           fixnum
           (,(case op
              ((first car) (case name
                            (atomic-incf '%atomic-inc-car)
                            (atomic-decf '%atomic-dec-car)))
              ((rest cdr)  (case name
                            (atomic-incf '%atomic-inc-cdr)
                            (atomic-decf '%atomic-dec-cdr))))
             ,(car args) (the fixnum ,diff))))
        (t
         (when (or (cdr args)
         ;; Because accessor info is identical for the writer and reader
         ;; functions, without a SYMBOLP check this would erroneously allow
         ;;   (ATOMIC-INCF ((SETF STRUCT-SLOT) x))
                   (not (symbolp op))
                   (not (structure-instance-accessor-p op)))
             (invalid-place))
         (let* ((accessor-info (structure-instance-accessor-p op))
                (slotd (cdr accessor-info))
                (type (dsd-type slotd)))
           (unless (and (eq 'sb!vm:word (dsd-raw-type slotd))
                        (type= (specifier-type type) (specifier-type 'sb!vm:word)))
             (error "~S requires a slot of type (UNSIGNED-BYTE ~S), not ~S: ~S"
                    name sb!vm:n-word-bits type place))
           (when (dsd-read-only slotd)
             (error "Cannot use ~S with structure accessor for a read-only slot: ~S"
                    name place))
           #!+compare-and-swap-vops
           `(truly-the sb!vm:word
             (%raw-instance-atomic-incf/word
              (the ,(dd-name (car accessor-info)) ,@args)
              ,(dsd-index slotd)
              ,(compute-delta)))
           #!-compare-and-swap-vops
           (with-unique-names (structure old-value)
             `(without-interrupts
               (let* ((,structure ,@args)
                      (,old-value (,op ,structure)))
                 (setf (,op ,structure) ,(compute-newval old-value))
                 ,old-value)))))))))

(def!macro atomic-incf (&environment env place &optional (diff 1))
  #!+sb-doc
  #.(format nil
  "Atomically increments PLACE by DIFF, and returns the value of PLACE before
the increment.

PLACE must access one of the following:
 - a DEFSTRUCT slot with declared type (UNSIGNED-BYTE ~D~:*)
   or AREF of a (SIMPLE-ARRAY (UNSIGNED-BYTE ~D~:*) (*))
   The type SB-EXT:WORD can be used for these purposes.
 - CAR or CDR (respectively FIRST or REST) of a CONS.
 - a variable defined using DEFGLOBAL with a proclaimed type of FIXNUM.
Macroexpansion is performed on PLACE before expanding ATOMIC-INCF.

Incrementing is done using modular arithmetic,
which is well-defined over two different domains:
 - For structures and arrays, the operation accepts and produces
   an (UNSIGNED-BYTE ~D~:*), and DIFF must be of type (SIGNED-BYTE ~D).
   ATOMIC-INCF of #x~x by one results in #x0 being stored in PLACE.
 - For other places, the domain is FIXNUM, and DIFF must be a FIXNUM.
   ATOMIC-INCF of #x~x by one results in #x~x
   being stored in PLACE.

DIFF defaults to 1.

EXPERIMENTAL: Interface subject to change."
  sb!vm:n-word-bits most-positive-word
  sb!xc:most-positive-fixnum sb!xc:most-negative-fixnum)
  (expand-atomic-frob 'atomic-incf place diff env))

(defmacro atomic-decf (&environment env place &optional (diff 1))
  #!+sb-doc
  #.(format nil
  "Atomically decrements PLACE by DIFF, and returns the value of PLACE before
the decrement.

PLACE must access one of the following:
 - a DEFSTRUCT slot with declared type (UNSIGNED-BYTE ~D~:*)
   or AREF of a (SIMPLE-ARRAY (UNSIGNED-BYTE ~D~:*) (*))
   The type SB-EXT:WORD can be used for these purposes.
 - CAR or CDR (respectively FIRST or REST) of a CONS.
 - a variable defined using DEFGLOBAL with a proclaimed type of FIXNUM.
Macroexpansion is performed on PLACE before expanding ATOMIC-DECF.

Decrementing is done using modular arithmetic,
which is well-defined over two different domains:
 - For structures and arrays, the operation accepts and produces
   an (UNSIGNED-BYTE ~D~:*), and DIFF must be of type (SIGNED-BYTE ~D).
   ATOMIC-DECF of #x0 by one results in #x~x being stored in PLACE.
 - For other places, the domain is FIXNUM, and DIFF must be a FIXNUM.
   ATOMIC-DECF of #x~x by one results in #x~x
   being stored in PLACE.

DIFF defaults to 1.

EXPERIMENTAL: Interface subject to change."
  sb!vm:n-word-bits most-positive-word
  sb!xc:most-negative-fixnum sb!xc:most-positive-fixnum)
  (expand-atomic-frob 'atomic-decf place diff env))

;; Interpreter stubs for ATOMIC-INCF.
#!+compare-and-swap-vops
(progn
  ;; argument types are declared in vm-fndb
  (defun %array-atomic-incf/word (array index diff)
    (%array-atomic-incf/word array index diff))
  (defun %raw-instance-atomic-incf/word (instance index diff)
    (%raw-instance-atomic-incf/word instance index diff)))

(defun spin-loop-hint ()
  #!+sb-doc
  "Hints the processor that the current thread is spin-looping."
  (spin-loop-hint))

(defun call-hooks (kind hooks &key (on-error :error))
  (dolist (hook hooks)
    (handler-case
        (funcall hook)
      (serious-condition (c)
        (if (eq :warn on-error)
            (warn "Problem running ~A hook ~S:~%  ~A" kind hook c)
            (with-simple-restart (continue "Skip this ~A hook." kind)
              (error "Problem running ~A hook ~S:~%  ~A" kind hook c)))))))

;;;; DEFGLOBAL

(defmacro-mundanely defglobal (name value &optional (doc nil docp))
  #!+sb-doc
  "Defines NAME as a global variable that is always bound. VALUE is evaluated
and assigned to NAME both at compile- and load-time, but only if NAME is not
already bound.

Global variables share their values between all threads, and cannot be
locally bound, declared special, defined as constants, and neither bound
nor defined as symbol macros.

See also the declarations SB-EXT:GLOBAL and SB-EXT:ALWAYS-BOUND."
  (let ((boundp (make-symbol "BOUNDP")))
    `(progn
       (eval-when (:compile-toplevel)
         (let ((,boundp (boundp ',name)))
           (%compiler-defglobal ',name :always-bound
                                (unless ,boundp ,value) (not ,boundp))))
       (let ((,boundp (boundp ',name)))
         (%defglobal ',name (unless ,boundp ,value) ,boundp ',doc ,docp
                     (sb!c:source-location))))))

(defmacro-mundanely define-load-time-global (name value &optional (doc nil docp))
  #!+sb-doc
  "Defines NAME as a global variable that is always bound. VALUE is evaluated
and assigned to NAME at load-time, but only if NAME is not already bound.

Attempts to read NAME at compile-time will signal an UNBOUND-VARIABLE error
unless it has otherwise been assigned a value.

See also DEFGLOBAL which assigns the VALUE at compile-time too."
  (let ((boundp (make-symbol "BOUNDP")))
    `(progn
       (eval-when (:compile-toplevel)
         (%compiler-defglobal ',name :eventually nil nil))
       (let ((,boundp (boundp ',name)))
         (%defglobal ',name (unless ,boundp ,value) ,boundp ',doc ,docp
                     (sb!c:source-location))))))

(defun %compiler-defglobal (name always-boundp value assign-it-p)
  (sb!xc:proclaim `(global ,name))
  (when assign-it-p
    #-sb-xc-host
    (set-symbol-global-value name value)
    #+sb-xc-host
    (set name value))
  (sb!c::process-variable-declaration
   name 'always-bound
   ;; don't "weaken" the proclamation if it's in fact always bound now
   (if (eq (info :variable :always-bound name) :always-bound)
       :always-bound
       always-boundp)))

(defun %defglobal (name value boundp doc docp source-location)
  (%compiler-defglobal name :always-bound value (not boundp))
  (when docp
    (setf (fdocumentation name 'variable) doc))
  (when source-location
    (setf (info :source-location :variable name) source-location))
  name)

;;;; WAIT-FOR -- waiting on arbitrary conditions

#-sb-xc-host
(defun %%wait-for (test stop-sec stop-usec)
  (declare (function test))
  (labels ((try ()
             (declare (optimize (safety 0)))
             (awhen (funcall test)
               (return-from %%wait-for it)))
           (tick (sec usec)
             (declare (type fixnum sec usec))
             ;; TICK is microseconds
             (+ usec (* 1000000 sec)))
           (get-tick ()
             (multiple-value-call #'tick
               (decode-internal-time (get-internal-real-time)))))
    (let* ((timeout-tick (when stop-sec (tick stop-sec stop-usec)))
           (start (get-tick))
           ;; Rough estimate of how long a single attempt takes.
           (try-ticks (progn
                        (try) (try) (try)
                        (max 1 (truncate (- (get-tick) start) 3)))))
      ;; Scale sleeping between attempts:
      ;;
      ;; Start by sleeping for as many ticks as an average attempt
      ;; takes, then doubling for each attempt.
      ;;
      ;; Max out at 0.1 seconds, or the 2 x time of a single try,
      ;; whichever is longer -- with a hard cap of 10 seconds.
      ;;
      ;; FIXME: Maybe the API should have a :MAX-SLEEP argument?
      (loop with max-ticks = (max 100000 (min (* 2 try-ticks)
                                              (expt 10 7)))
            for scale of-type fixnum = 1
            then (let ((x (logand most-positive-fixnum (* 2 scale))))
                   (if (> scale x)
                       most-positive-fixnum
                       x))
            do (try)
               (let* ((now (get-tick))
                      (sleep-ticks (min (* try-ticks scale) max-ticks))
                      (sleep
                        (if timeout-tick
                            ;; If sleep would take us past the
                            ;; timeout, shorten it so it's just
                            ;; right.
                            (if (>= (+ now sleep-ticks) timeout-tick)
                                (- timeout-tick now)
                                sleep-ticks)
                            sleep-ticks)))
                 (declare (type fixnum sleep))
                 (cond ((plusp sleep)
                        ;; microseconds to seconds and nanoseconds
                        (multiple-value-bind (sec nsec)
                            (truncate (* 1000 sleep) (expt 10 9))
                          (with-interrupts
                            (sb!unix:nanosleep sec nsec))))
                       (t
                        (return-from %%wait-for nil))))))))

#-sb-xc-host
(defun %wait-for (test timeout)
  (declare (function test))
  (tagbody
   :restart
     (multiple-value-bind (to-sec to-usec stop-sec stop-usec deadlinep)
         (decode-timeout timeout)
       (declare (ignore to-sec to-usec))
       (return-from %wait-for
         (or (%%wait-for test stop-sec stop-usec)
             (when deadlinep
               (signal-deadline)
               (go :restart)))))))

(defmacro wait-for (test-form &key timeout)
  #!+sb-doc
  "Wait until TEST-FORM evaluates to true, then return its primary value.
If TIMEOUT is provided, waits at most approximately TIMEOUT seconds before
returning NIL.

If WITH-DEADLINE has been used to provide a global deadline, signals a
DEADLINE-TIMEOUT if TEST-FORM doesn't evaluate to true before the
deadline.

Experimental: subject to change without prior notice."
  `(dx-flet ((wait-for-test () (progn ,test-form)))
     (%wait-for #'wait-for-test ,timeout)))

(defmacro with-progressive-timeout ((name &key seconds)
                                    &body body)
  #!+sb-doc
  "Binds NAME as a local function for BODY. Each time #'NAME is called, it
returns SECONDS minus the time that has elapsed since BODY was entered, or
zero if more time than SECONDS has elapsed. If SECONDS is NIL, #'NAME
returns NIL each time."
  (with-unique-names (deadline time-left sec)
    `(let* ((,sec ,seconds)
            (,deadline
              (when ,sec
                (+ (get-internal-real-time)
                   (round (* ,seconds internal-time-units-per-second))))))
       (flet ((,name ()
                (when ,deadline
                  (let ((,time-left (- ,deadline (get-internal-real-time))))
                    (if (plusp ,time-left)
                        (* (coerce ,time-left 'single-float)
                           (load-time-value (/ 1.0f0 internal-time-units-per-second) t))
                        0)))))
         ,@body))))

(defmacro atomic-update (place update-fn &rest arguments &environment env)
  #!+sb-doc
  "Updates PLACE atomically to the value returned by calling function
designated by UPDATE-FN with ARGUMENTS and the previous value of PLACE.

PLACE may be read and UPDATE-FN evaluated and called multiple times before the
update succeeds: atomicity in this context means that the value of PLACE did
not change between the time it was read, and the time it was replaced with the
computed value.

PLACE can be any place supported by SB-EXT:COMPARE-AND-SWAP.

Examples:

  ;;; Conses T to the head of FOO-LIST.
  (defstruct foo list)
  (defvar *foo* (make-foo))
  (atomic-update (foo-list *foo*) #'cons t)

  (let ((x (cons :count 0)))
     (mapc #'sb-thread:join-thread
           (loop repeat 1000
                 collect (sb-thread:make-thread
                          (lambda ()
                            (loop repeat 1000
                                  do (atomic-update (cdr x) #'1+)
                                     (sleep 0.00001))))))
     ;; Guaranteed to be (:COUNT . 1000000) -- if you replace
     ;; atomic update with (INCF (CDR X)) above, the result becomes
     ;; unpredictable.
     x)
"
  (multiple-value-bind (vars vals old new cas-form read-form)
      (get-cas-expansion place env)
    `(let* (,@(mapcar 'list vars vals)
            (,old ,read-form))
       (loop for ,new = (funcall ,update-fn ,@arguments ,old)
             until (eq ,old (setf ,old ,cas-form))
             finally (return ,new)))))

(defmacro atomic-push (obj place &environment env)
  #!+sb-doc
  "Like PUSH, but atomic. PLACE may be read multiple times before
the operation completes -- the write does not occur until such time
that no other thread modified PLACE between the read and the write.

Works on all CASable places."
  (multiple-value-bind (vars vals old new cas-form read-form)
      (get-cas-expansion place env)
    `(let* (,@(mapcar 'list vars vals)
            (,old ,read-form)
            (,new (cons ,obj ,old)))
       (loop until (eq ,old (setf ,old ,cas-form))
             do (setf (cdr ,new) ,old)
             finally (return ,new)))))

(defmacro atomic-pop (place &environment env)
  #!+sb-doc
  "Like POP, but atomic. PLACE may be read multiple times before
the operation completes -- the write does not occur until such time
that no other thread modified PLACE between the read and the write.

Works on all CASable places."
  (multiple-value-bind (vars vals old new cas-form read-form)
      (get-cas-expansion place env)
    `(let* (,@(mapcar 'list vars vals))
       (loop for ,old = ,read-form
             for ,new = (cdr ,old)
             until (eq ,old (setf ,old ,cas-form))
             finally (return (car ,old))))))

(defun split-version-string (string)
  (loop with subversion and start = 0
        with end = (length string)
        when (setf (values subversion start)
                   (parse-integer string :start start :junk-allowed t))
        collect it
        while (and subversion
                   (< start end)
                   (char= (char string start) #\.))
        do (incf start)))

(defun version>= (x y)
  (unless (or x y)
    (return-from version>= t))
  (let ((head-x (or (first x) 0))
        (head-y (or (first y) 0)))
    (or (> head-x head-y)
        (and (= head-x head-y)
             (version>= (rest x) (rest y))))))

(defun assert-version->= (&rest subversions)
  #!+sb-doc
  "Asserts that the current SBCL is of version equal to or greater than
the version specified in the arguments.  A continuable error is signaled
otherwise.

The arguments specify a sequence of subversion numbers in big endian order.
They are compared lexicographically with the runtime version, and versions
are treated as though trailed by an unbounded number of 0s.

For example, (assert-version->= 1 1 4) asserts that the current SBCL is
version 1.1.4[.0.0...] or greater, and (assert-version->= 1) that it is
version 1[.0.0...] or greater."
  (let ((version (split-version-string (lisp-implementation-version))))
    (unless (version>= version subversions)
      (cerror "Disregard this version requirement."
              "SBCL ~A is too old for this program (version ~{~A~^.~} ~
               or later is required)."
              (lisp-implementation-version)
              subversions))))

;;; Signalling an error when trying to print an error condition is
;;; generally a PITA, so whatever the failure encountered when
;;; wondering about FILE-POSITION within a condition printer, 'tis
;;; better silently to give up than to try to complain.
(defun file-position-or-nil-for-error (stream &optional (pos nil posp))
  ;; Arguably FILE-POSITION shouldn't be signalling errors at all; but
  ;; "NIL if this cannot be determined" in the ANSI spec doesn't seem
  ;; absolutely unambiguously to prohibit errors when, e.g., STREAM
  ;; has been closed so that FILE-POSITION is a nonsense question. So
  ;; my (WHN) impression is that the conservative approach is to
  ;; IGNORE-ERRORS. (I encountered this failure from within a homebrew
  ;; defsystemish operation where the ERROR-STREAM had been CL:CLOSEd,
  ;; I think by nonlocally exiting through a WITH-OPEN-FILE, by the
  ;; time an error was reported.)
  (ignore-errors
   (if posp
       (file-position stream pos)
       (file-position stream))))

(defun stream-error-position-info (stream &optional position)
  (when (and (not position) (form-tracking-stream-p stream))
    (let ((line/col (line/col-from-charpos stream)))
      (return-from stream-error-position-info
        `((:line ,(car line/col))
          (:column ,(cdr line/col))
          ,@(let ((position (file-position-or-nil-for-error stream)))
              ;; FIXME: 1- is technically broken for multi-byte external
              ;; encodings, albeit bug-compatible with the broken code in
              ;; the general case (below) for non-form-tracking-streams.
              ;; i.e. If you position to this byte, it might not be the
              ;; first byte of any character.
              (when position `((:file-position ,(1- position)))))))))

  ;; Give up early for interactive streams and non-character stream.
  (when (or (ignore-errors (interactive-stream-p stream))
            (not (subtypep (ignore-errors (stream-element-type stream))
                           'character)))
    (return-from stream-error-position-info))

  (flet ((read-content (old-position position)
           "Read the content of STREAM into a buffer in order to count
lines and columns."
           (unless (and old-position position
                        (< position sb!xc:array-dimension-limit))
             (return-from read-content))
           (let ((content
                   (make-string position :element-type (stream-element-type stream))))
             (when (and (file-position-or-nil-for-error stream :start)
                        (eql position (ignore-errors (read-sequence content stream))))
               (file-position-or-nil-for-error stream old-position)
               content)))
         ;; Lines count from 1, columns from 0. It's stupid and
         ;; traditional.
         (line (string)
           (1+ (count #\Newline string)))
         (column (string position)
           (- position (or (position #\Newline string :from-end t) 0))))
   (let* ((stream-position (file-position-or-nil-for-error stream))
          (position (or position
                        ;; FILE-POSITION is the next character --
                        ;; error is at the previous one.
                        (and stream-position (plusp stream-position)
                             (1- stream-position))))
          (content (read-content stream-position position)))
     `(,@(when content `((:line ,(line content))
                         (:column ,(column content position))))
       ,@(when position `((:file-position ,position)))))))
