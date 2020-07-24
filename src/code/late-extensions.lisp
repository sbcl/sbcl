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

(in-package "SB-IMPL")

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
                        (< position array-dimension-limit))
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
