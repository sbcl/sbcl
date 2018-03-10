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
;;; FIXME: the reason this can't be defined adjacent to its friends
;;; PROPER-LIST-P and PROPER-LIST-OF-LENGTH-P is that HANDLER-BIND
;;; does not work in 'primordial-extensions'.
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

(defun spin-loop-hint ()
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

(sb!xc:defmacro defglobal (name value &optional (doc nil docp))
  "Defines NAME as a global variable that is always bound. VALUE is evaluated
and assigned to NAME both at compile- and load-time, but only if NAME is not
already bound.

Global variables share their values between all threads, and cannot be
locally bound, declared special, defined as constants, and neither bound
nor defined as symbol macros.

See also the declarations SB-EXT:GLOBAL and SB-EXT:ALWAYS-BOUND."
  ;; Maybe kill docstring, but only under the cross-compiler.
  #!+(and (not sb-doc) (host-feature sb-xc-host)) (setq doc nil)
  (let ((boundp (make-symbol "BOUNDP")))
    `(progn
       (eval-when (:compile-toplevel)
         (let ((,boundp (boundp ',name)))
           (%compiler-defglobal ',name :always-bound
                                (unless ,boundp ,value) (not ,boundp))))
       (let ((,boundp (boundp ',name)))
         (%defglobal ',name (unless ,boundp ,value) ,boundp ',doc ,docp
                     (sb!c:source-location))))))

(sb!xc:defmacro define-load-time-global (name value &optional (doc nil docp))
  "Defines NAME as a global variable that is always bound. VALUE is evaluated
and assigned to NAME at load-time, but only if NAME is not already bound.

Attempts to read NAME at compile-time will signal an UNBOUND-VARIABLE error
unless it has otherwise been assigned a value.

See also DEFGLOBAL which assigns the VALUE at compile-time too."
  ;; Maybe kill docstring, but only under the cross-compiler.
  #!+(and (not sb-doc) (host-feature sb-xc-host)) (setq doc nil)
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
