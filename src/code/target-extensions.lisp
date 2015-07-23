;;;; This file contains things for the extensions packages (SB-EXT and
;;;; also "internal extensions" SB-INT) which can't be built at
;;;; cross-compile time, and perhaps also some things which might as
;;;; well not be built at cross-compile time because they're not
;;;; needed then. Things which can't be built at cross-compile time
;;;; (e.g. because they need machinery which only exists inside SBCL's
;;;; implementation of the LISP package) do not belong in this file.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;;; variables initialization and shutdown sequences

;; (Most of the save-a-core functionality is defined later, in its
;; own file, but we'd like to have these symbols declared special
;; and initialized ASAP.)
(defvar *save-hooks* nil
  #!+sb-doc
  "This is a list of functions which are called in an unspecified
order before creating a saved core image. Unused by SBCL itself:
reserved for user and applications.")

(defvar *init-hooks* nil
  #!+sb-doc
  "This is a list of functions which are called in an unspecified
order when a saved core image starts up, after the system itself has
been initialized. Unused by SBCL itself: reserved for user and
applications.")

(defvar *exit-hooks* nil
  #!+sb-doc
  "This is a list of functions which are called in an unspecified
order when SBCL process exits. Unused by SBCL itself: reserved for
user and applications. Using (SB-EXT:EXIT :ABORT T), or calling
exit(3) directly will circumvent these hooks.")


;;; Binary search for simple vectors
(defun binary-search (value seq &key (key #'identity))
  (declare (simple-vector seq))
  (labels ((recurse (start end)
             (when (< start end)
               (let* ((i (+ start (truncate (- end start) 2)))
                      (elt (svref seq i))
                      (key-value (funcall key elt)))
                 (cond ((< value key-value)
                        (recurse start i))
                       ((> value key-value)
                        (recurse (1+ i) end))
                       (t
                        elt))))))
    (recurse 0 (length seq))))

(defun double-vector-binary-search (value vector)
  (declare (simple-vector vector)
           (optimize speed)
           (integer value))
  (labels ((recurse (start end)
             (declare (type index start end))
             (when (< start end)
               (let* ((i (+ start (truncate (- end start) 2)))
                      (elt (svref vector (truly-the index (* 2 i)))))
                 (declare (type integer elt)
                          (type index i))
                 (cond ((< value elt)
                        (recurse start i))
                       ((> value elt)
                        (recurse (1+ i) end))
                       (t
                        (svref vector (truly-the index (1+ (* 2 i))))))))))
    (recurse 0 (truncate (length vector) 2))))


;;; like LISTEN, but any whitespace in the input stream will be flushed
(defun listen-skip-whitespace (&optional (stream *standard-input*))
  (do ((char (read-char-no-hang stream nil nil nil)
             (read-char-no-hang stream nil nil nil)))
      ((null char) nil)
    (cond ((not (whitespace[1]p char))
           (unread-char char stream)
           (return t)))))

;;;; helpers for C library calls

;;; Signal a SIMPLE-CONDITION/ERROR condition associated with an ANSI C
;;; errno problem, arranging for the condition's print representation
;;; to be similar to the ANSI C perror(3) style.
(defun simple-perror (prefix-string
                      &key
                      (errno (get-errno))
                      (simple-error 'simple-error)
                      other-condition-args)
  (declare (type symbol simple-error))
  (aver (subtypep simple-error 'simple-condition))
  (aver (subtypep simple-error 'error))
  (apply #'error
         simple-error
         :format-control "~@<~A: ~2I~_~A~:>"
         :format-arguments (list prefix-string (strerror errno))
         other-condition-args))

;;; Constructing shortish strings one character at a time. More efficient then
;;; a string-stream, as can directly use simple-base-strings when applicable,
;;; and if the maximum size is know doesn't need to copy the result at all --
;;; but if the result is going to be HUGE, string-streams will win.
(defmacro with-push-char ((&key (element-type 'character) (initial-size 28)) &body body)
  (with-unique-names (string size pointer)
    `(let* ((,size ,initial-size)
            (,string (make-array ,size :element-type ',element-type))
            (,pointer 0))
       (declare (type (integer 0 ,sb!xc:array-dimension-limit) ,size)
                (type (integer 0 ,(1- sb!xc:array-dimension-limit)) ,pointer)
                (type (simple-array ,element-type (*)) ,string))
       (flet ((push-char (char)
                (declare (optimize (sb!c::insert-array-bounds-checks 0)))
                (when (= ,pointer ,size)
                  (let ((old ,string))
                    (setf ,size (* 2 (+ ,size 2))
                          ,string (make-array ,size :element-type ',element-type))
                    (replace ,string old)))
                (setf (char ,string ,pointer) char)
                (incf ,pointer))
              (get-pushed-string ()
                (let ((string ,string)
                      (size ,pointer))
                  (setf ,size 0
                        ,pointer 0
                        ,string ,(coerce "" `(simple-array ,element-type (*))))
                  ;; This is really local, so we can be destructive!
                  (%shrink-vector string size)
                  string)))
         ,@body))))

;;; The smallest power of two that is equal to or greater than X.
(defun power-of-two-ceiling (x)
  (declare (index x))
  (ash 1 (integer-length (1- x))))

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
