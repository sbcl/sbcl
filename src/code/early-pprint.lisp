;;;; pretty printer stuff which has to be defined early (e.g. DEFMACROs)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-PRETTY")

;;;; user interface to the pretty printer

(defmacro pprint-logical-block ((stream-symbol
                                 object
                                 &rest keys
                                 &key (prefix nil prefixp)
                                      (per-line-prefix nil per-line-prefix-p)
                                      (suffix ""))
                                &body body)
  "Group some output into a logical block. STREAM-SYMBOL should be either a
   stream, T (for *TERMINAL-IO*), or NIL (for *STANDARD-OUTPUT*). The printer
   control variable *PRINT-LEVEL* is automatically handled."
  (let ((prefix (cond ((and prefixp per-line-prefix-p)
                       (error "cannot specify values for both PREFIX and PER-LINE-PREFIX."))
                      (prefixp prefix)
                      (per-line-prefix-p per-line-prefix)))
        (proc (make-symbol "PPRINT-BLOCK"))
        (list (and object (make-symbol "LIST")))
        (state (make-symbol "STATE"))
        (stream-var (case stream-symbol
                      ((nil) '*standard-output*)
                      ((t) '*terminal-io*)
                      (t stream-symbol)))
        (bindings))
    ;; This is not a function, but to the degree possible should have usual
    ;; evaluation order. No bothering with duplicated keyword args,
    ;; or :allow-other-keys nonsense.
    (unless (and (constantp prefix) (constantp suffix))
      (loop (multiple-value-bind (indicator value tail)
                (get-properties keys '(:prefix :per-line-prefix :suffix))
              (if (not indicator) (return))
              (setq keys (cddr tail))
              (unless (assoc indicator bindings :test 'string=) ; dup
                (let ((tmp (copy-symbol indicator)))
                  (setq bindings (nconc bindings (list (list tmp value))))
                  (if (eq indicator :suffix)
                      (setq suffix tmp)
                      (setq prefix tmp))))))
      (when object
        (let ((tmp (make-symbol "OBJ")))
          (setq bindings (acons tmp (list object) bindings) object tmp))))
    `(dx-flet ((,proc (,@(and list (list list)) ,state ,stream-var)
                 (declare (ignorable ,@(and list (list list))
                                      ,state ,stream-var))
                 (declare (disable-package-locks pprint-exit-if-list-exhausted
                                                 pprint-pop))
                 (macrolet ,(if object
                                `((pprint-exit-if-list-exhausted ()
                                    '(when (null ,list) (return-from ,proc)))
                                  (pprint-pop ()
                                    '(if (pprint-length-check ,list ,state)
                                         (pop ,list)
                                         (return-from ,proc))))
                                `((pprint-exit-if-list-exhausted ()
                                    '(return-from ,proc))
                                  (pprint-pop ()
                                    '(if (pprint-length-check* ,state)
                                         nil
                                         (return-from ,proc)))))
                   (declare (enable-package-locks pprint-exit-if-list-exhausted
                                                  pprint-pop))
                   ,@body)))
       (let ,bindings
         (call-logical-block-printer #',proc ,stream-symbol
                                     ,prefix ,per-line-prefix-p ,suffix
                                     ,@(if object (list object)))))))

(defmacro pprint-exit-if-list-exhausted ()
  "Cause the closest enclosing use of PPRINT-LOGICAL-BLOCK to return
   if its list argument is exhausted. Can only be used inside
   PPRINT-LOGICAL-BLOCK, and only when the LIST argument to
   PPRINT-LOGICAL-BLOCK is supplied."
  (error "PPRINT-EXIT-IF-LIST-EXHAUSTED must be lexically inside ~
          PPRINT-LOGICAL-BLOCK."))

(defmacro pprint-pop ()
  "Return the next element from LIST argument to the closest enclosing
   use of PPRINT-LOGICAL-BLOCK, automatically handling *PRINT-LENGTH*
   and *PRINT-CIRCLE*. Can only be used inside PPRINT-LOGICAL-BLOCK.
   If the LIST argument to PPRINT-LOGICAL-BLOCK was NIL, then nothing
   is popped, but the *PRINT-LENGTH* testing still happens."
  (error "PPRINT-POP must be lexically inside PPRINT-LOGICAL-BLOCK."))

;;;; pretty streams

;;; There are three different units for measuring character positions:
;;;  COLUMN - offset (if characters) from the start of the current line
;;;  INDEX  - index into the output buffer
;;;  POSN   - some position in the stream of characters cycling through
;;;           the output buffer
(deftype column ()
  '(and fixnum unsigned-byte))
(deftype posn () 'fixnum)

(defconstant initial-buffer-size 128)

(defconstant default-line-length 80)

;; We're allowed to DXify the pretty-stream used by PPRINT-LOGICAL-BLOCK.
;;   "pprint-logical-block and the pretty printing stream it creates have
;;    dynamic extent. The consequences are undefined if, outside of this
;;    extent, output is attempted to the pretty printing stream it creates."
;; However doing that is slightly dangerous since there are a zillion ways
;; for users to get a hold of the stream and stash it somewhere.
;; Anyway, just a thought... Maybe keep a small handful in a recyclable list?
(defstruct (pretty-stream (:include ansi-stream
                                    (cout #'pretty-out)
                                    (sout #'pretty-sout)
                                    (misc #'pretty-misc))
                          (:constructor make-pretty-stream
                                        (target
                                         &aux
                                         (line-length
                                          (or *print-right-margin*
                                              (sb-impl::line-length target)
                                              default-line-length))
                                         (buffer-start-column
                                          (or (sb-impl::charpos target) 0))))
                          (:copier nil))
  ;; Where the output is going to finally go.
  (target (missing-arg) :type stream :read-only t)
  ;; Line length we should format to. Cached here so we don't have to keep
  ;; extracting it from the target stream.
  (line-length (missing-arg) :type column :read-only t)
  ;; If non-nil, a function to call before performing OUT or SOUT
  (char-out-oneshot-hook nil :type (or null function))
  ;; A simple string holding all the text that has been output but not yet
  ;; printed.
  (buffer (make-string initial-buffer-size) :type (simple-array character (*)))
  ;; The index into BUFFER where more text should be put.
  (buffer-fill-pointer 0 :type index)
  ;; A list holding the positions in BUFFER of spaces which must not
  ;; be removed if they are at the end of a line.
  (buffer-significant-spaces nil :type list)
  ;; Whenever we output stuff from the buffer, we shift the remaining noise
  ;; over. This makes it difficult to keep references to locations in
  ;; the buffer. Therefore, we have to keep track of the total amount of
  ;; stuff that has been shifted out of the buffer.
  (buffer-offset 0 :type posn)
  ;; The column the first character in the buffer will appear in. Normally
  ;; zero, but if we end up with a very long line with no breaks in it we
  ;; might have to output part of it. Then this will no longer be zero.
  (buffer-start-column (missing-arg) :type column)
  ;; The line number we are currently on. Used for *PRINT-LINES*
  ;; abbreviations and to tell when sections have been split across
  ;; multiple lines.
  (line-number 0 :type index)
  ;; the value of *PRINT-LINES* captured at object creation time. We
  ;; use this, instead of the dynamic *PRINT-LINES*, to avoid
  ;; weirdness like
  ;;   (let ((*print-lines* 50))
  ;;     (pprint-logical-block ..
  ;;       (dotimes (i 10)
  ;;         (let ((*print-lines* 8))
  ;;           (print (aref possiblybigthings i) prettystream)))))
  ;; terminating the output of the entire logical blockafter 8 lines.
  (print-lines *print-lines* :type (or index null) :read-only t)
  ;; Stack of logical blocks in effect at the buffer start.
  (blocks (list (make-logical-block)) :type list)
  ;; Buffer holding the per-line prefix active at the buffer start.
  ;; Indentation is included in this. The length of this is stored
  ;; in the logical block stack.
  (prefix (make-string initial-buffer-size) :type (simple-array character (*)))
  ;; Buffer holding the total remaining suffix active at the buffer start.
  ;; The characters are right-justified in the buffer to make it easier
  ;; to output the buffer. The length is stored in the logical block
  ;; stack.
  (suffix (make-string initial-buffer-size) :type (simple-array character (*)))
  ;; Queue of pending operations. When empty, HEAD=TAIL=NIL. Otherwise,
  ;; TAIL holds the first (oldest) cons and HEAD holds the last (newest)
  ;; cons. Adding things to the queue is basically (setf (cdr head) (list
  ;; new)) and removing them is basically (pop tail) [except that care must
  ;; be taken to handle the empty queue case correctly.]
  (queue-tail nil :type list)
  (queue-head nil :type list)
  ;; Block-start queue entries in effect at the queue head.
  (pending-blocks nil :type list)
  (pending-blocks-length 0 :type index))
