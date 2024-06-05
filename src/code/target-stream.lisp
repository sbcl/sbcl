;;;; os-independent stream functions requiring reader machinery

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;; In the interest of ``once and only once'' this macro contains the
;;; framework necessary to implement a peek-char function, which has
;;; two special-cases (one for gray streams and one for echo streams)
;;; in addition to the normal case.
;;;
;;; All arguments are forms which will be used for a specific purpose
;;; PEEK-TYPE - the current peek-type as defined by ANSI CL
;;; EOF-RESULT - the eof-value argument to peek-char
;;; CHAR-VAR - the variable which will be used to store the current character
;;; READ-FORM - the form which will be used to read a character
;;; EOF-VALUE - the result returned from READ-FORM when hitting eof
;;; UNREAD-FORM - ditto for unread-char
;;; SKIPPED-CHAR-FORM - the form to execute when skipping a character
;;; EOF-DETECTED-FORM - the form to execute when EOF has been detected
;;;                     (this will default to EOF-RESULT)
(defmacro generalized-peeking-mechanism
    (peek-type eof-value char-var read-form read-eof unread-form
     &optional (skipped-char-form nil) (eof-detected-form nil))
  `(let ((,char-var ,read-form))
    (cond ((eql ,char-var ,read-eof)
           ,(if eof-detected-form
                eof-detected-form
                eof-value))
          ((characterp ,peek-type)
           (do ((,char-var ,char-var ,read-form))
               ((or (eql ,char-var ,read-eof)
                    ;; CHAR= will type-check for us
                    (char= ,char-var ,peek-type))
                (cond ((eql ,char-var ,read-eof)
                       ,(if eof-detected-form
                            eof-detected-form
                            eof-value))
                      (t ,unread-form
                         ,char-var)))
             ,skipped-char-form))
          ((eql ,peek-type t)
           (do ((.readtable. *readtable*)
                (,char-var ,char-var ,read-form))
               ((or (eql ,char-var ,read-eof)
                    ;; whitespace[2]p will type-check for us
                    (not (whitespace[2]p ,char-var .readtable.)))
                (cond ((eql ,char-var ,read-eof)
                       ,(if eof-detected-form
                            eof-detected-form
                            eof-value))
                      (t ,unread-form
                         ,char-var)))
             ,skipped-char-form))
          ((null ,peek-type)
           ,unread-form
           ,char-var)
          (t
           (bug "Impossible case reached in PEEK-CHAR")))))

;;; rudi (2004-08-09): There was an inline declaration for read-char,
;;; unread-char, read-byte, listen here that was removed because these
;;; functions are redefined when simple-streams are loaded.

(declaim (inline ansi-stream-peek-char))
(defun ansi-stream-peek-char (peek-type stream eof-error-p eof-value
                              recursive-p)
  (cond ((ansi-stream-cin-buffer stream)
         (prepare-for-fast-read-char stream
           (declare (ignore %frc-method%))
           (case peek-type
             ((nil)
              (block nil
                (if (= %frc-index% +ansi-stream-in-buffer-length+)
                    (let ((index-or-nil (fast-read-char-refill %frc-stream% eof-error-p)))
                      (when (null index-or-nil)
                        (return eof-value))
                      (aref %frc-buffer% (truly-the (mod #.+ansi-stream-in-buffer-length+)
                                                    index-or-nil)))
                    (aref %frc-buffer% %frc-index%))))
             ((t)
              (let ((rt *readtable*))
                (prog1
                    (loop
                     (when (= %frc-index% +ansi-stream-in-buffer-length+)
                       (let ((index-or-nil (fast-read-char-refill %frc-stream% eof-error-p)))
                         (when (null index-or-nil)
                           (return eof-value))
                         (setf %frc-index% (truly-the (mod #.+ansi-stream-in-buffer-length+)
                                                      index-or-nil))))
                     (let ((char (aref %frc-buffer% %frc-index%)))
                       (if (whitespace[2]p char rt)
                           (incf %frc-index%)
                           (return char))))
                  (done-with-fast-read-char))))
             (t
              (prog1
                  (loop
                   (when (= %frc-index% +ansi-stream-in-buffer-length+)
                     (let ((index-or-nil (fast-read-char-refill %frc-stream% eof-error-p)))
                       (when (null index-or-nil)
                         (return eof-value))
                       (setf %frc-index% (truly-the (mod #.+ansi-stream-in-buffer-length+)
                                                    index-or-nil))))
                   (let ((char (aref %frc-buffer% %frc-index%)))
                     (if (char= char peek-type)
                         (return char)
                         (incf %frc-index%))))
                (done-with-fast-read-char))))))
        ((typep stream 'echo-stream)
         (echo-stream-peek-char stream peek-type eof-error-p eof-value))
        (t
         (generalized-peeking-mechanism
             peek-type eof-value char
             (ansi-stream-read-char stream eof-error-p :eof recursive-p)
             :eof
             (ansi-stream-unread-char char stream)))))

(defun peek-char (&optional (peek-type nil)
                            (stream *standard-input*)
                            (eof-error-p t)
                            eof-value
                            recursive-p)
  (declare (type (or character boolean) peek-type) (explicit-check))
  (stream-api-dispatch (stream :input)
    :simple (let ((char (s-%peek-char stream peek-type eof-error-p eof-value)))
              ;; simple-streams -%PEEK-CHAR always ignored RECURSIVE-P
              ;; so I removed it from the call.
              (if (eq char eof-value) char (the character char)))
    :native
        (ansi-stream-peek-char peek-type stream eof-error-p eof-value
                               recursive-p)
    :gray
        (let ((char
               (generalized-peeking-mechanism
                peek-type :eof char
                (if (null peek-type)
                    (stream-peek-char stream)
                    (stream-read-char stream))
                :eof
                (if (null peek-type)
                    ()
                    (stream-unread-char stream char))
                ()
                (eof-or-lose stream eof-error-p eof-value))))
          (if (eq char eof-value)
              char
              (the character char)))))

(defun echo-misc (stream operation arg1)
  (let* ((in (two-way-stream-input-stream stream))
         (out (two-way-stream-output-stream stream)))
    (stream-misc-case (operation)
      (:listen
       (if (ansi-stream-p in)
           (%ansi-stream-listen in)
           (stream-misc-dispatch in operation arg1)))
      (:unread (setf (echo-stream-unread-stuff stream) t)
               (unread-char arg1 in))
      (:element-type
       (let ((in-type (stream-element-type in))
             (out-type (stream-element-type out)))
         (if (equal in-type out-type)
             in-type
             `(and ,in-type ,out-type))))
      (:element-mode
       (let ((in-mode (stream-element-mode in))
             (out-mode (stream-element-mode out)))
         (when (equal in-mode out-mode)
           in-mode)))
      (:close
       (set-closed-flame stream))
      (t
       (or (if (ansi-stream-p in)
               (call-ansi-stream-misc in operation arg1)
               (stream-misc-dispatch in operation arg1))
           (if (ansi-stream-p out)
               (call-ansi-stream-misc out operation arg1)
               (stream-misc-dispatch out operation arg1)))))))

(defun echo-stream-peek-char (stream peek-type eof-error-p eof-value)
  (let* ((in (two-way-stream-input-stream stream))
         (out (two-way-stream-output-stream stream)))
       ;; returns peeked-char, eof-value, or errors end-of-file
       ;;
       ;; Note: This code could be moved into PEEK-CHAR if desired.
       ;; I am unsure whether this belongs with echo-streams because it is
       ;; echo-stream specific, or PEEK-CHAR because it is peeking code.
       ;; -- mrd 2002-11-18
       ;;
       ;; UNREAD-P indicates whether the next character on IN was one
       ;; that was previously unread.  In that case, we need to ensure
       ;; that the semantics for UNREAD-CHAR are held; the character
       ;; should not be echoed again.
       (let ((unread-p nil)
             ;; The first peek shouldn't touch the unread-stuff slot.
             (initial-peek-p t))
         (flet ((outfn (c)
                  (unless unread-p
                    (if (ansi-stream-p out)
                        (funcall (ansi-stream-cout out) out c)
                        ;; gray-stream
                        (stream-write-char out c))))
                (infn ()
                  (if initial-peek-p
                      (setf unread-p (echo-stream-unread-stuff stream))
                      (setf (echo-stream-unread-stuff stream) nil))
                  (setf initial-peek-p nil)
                  (read-char in eof-error-p :eof)))
           (generalized-peeking-mechanism
            peek-type eof-value char
            (infn)
            :eof
            (unread-char char in)
            (outfn char))))))

;;; Stop wasting space with unnecessarily preserved inline definitions.
;;; ANSI-STREAM-LISTEN, %ANSI-STREAM-LISTEN, ANSI-STREAM-CLEAR-INPUT
;;; are needed for sb-simple-streams.
;;; Maybe everything else can just get dropped entirely, the symbol and its
;;; function, instead of just the inline sexpr.
(dolist (name '(!ansi-stream-ftell
                ansi-stream-read-line ansi-stream-read-char
                ansi-stream-unread-char
                ansi-stream-read-char-no-hang
                ansi-stream-read-byte read-n-bytes
                read-char unread-char read-byte
                read-sequence/read-function write-sequence/write-function
                stream-element-mode))
  (clear-info :function :inlinep name)
  (clear-info :function :inlining-data name))
;;; Can all the ANSI- function names be removed now? Maybe?
(push '("SB-IMPL" ansi-stream-peek-char ansi-stream-unread-char)
      *!removable-symbols*)
;;; These two wants to get invoked by simple-streams but would get tree-shaken out
;;; were they not externalized.
(export '(sb-impl::in-stream-from-designator sb-impl::eof-or-lose)
        'sb-impl)
