;;;; Common Lisp pretty printer

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!PRETTY")

;;;; pretty streams

;;; There are three different units for measuring character positions:
;;;  COLUMN - offset (if characters) from the start of the current line
;;;  INDEX  - index into the output buffer
;;;  POSN   - some position in the stream of characters cycling through
;;;           the output buffer
(deftype column ()
  '(and fixnum unsigned-byte))
;;; The INDEX type is picked up from the kernel package.
(deftype posn ()
  'fixnum)

(defconstant initial-buffer-size 128)

(defconstant default-line-length 80)

(defstruct (pretty-stream (:include sb!kernel:ansi-stream
                                    (out #'pretty-out)
                                    (sout #'pretty-sout)
                                    (misc #'pretty-misc))
                          (:constructor make-pretty-stream (target))
                          (:copier nil))
  ;; Where the output is going to finally go.
  (target (missing-arg) :type stream)
  ;; Line length we should format to. Cached here so we don't have to keep
  ;; extracting it from the target stream.
  (line-length (or *print-right-margin*
                   (sb!impl::line-length target)
                   default-line-length)
               :type column)
  ;; A simple string holding all the text that has been output but not yet
  ;; printed.
  (buffer (make-string initial-buffer-size) :type (simple-array character (*)))
  ;; The index into BUFFER where more text should be put.
  (buffer-fill-pointer 0 :type index)
  ;; Whenever we output stuff from the buffer, we shift the remaining noise
  ;; over. This makes it difficult to keep references to locations in
  ;; the buffer. Therefore, we have to keep track of the total amount of
  ;; stuff that has been shifted out of the buffer.
  (buffer-offset 0 :type posn)
  ;; The column the first character in the buffer will appear in. Normally
  ;; zero, but if we end up with a very long line with no breaks in it we
  ;; might have to output part of it. Then this will no longer be zero.
  (buffer-start-column (or (sb!impl::charpos target) 0) :type column)
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
  (prefix (make-string initial-buffer-size) :type simple-string)
  ;; Buffer holding the total remaining suffix active at the buffer start.
  ;; The characters are right-justified in the buffer to make it easier
  ;; to output the buffer. The length is stored in the logical block
  ;; stack.
  (suffix (make-string initial-buffer-size) :type simple-string)
  ;; Queue of pending operations. When empty, HEAD=TAIL=NIL. Otherwise,
  ;; TAIL holds the first (oldest) cons and HEAD holds the last (newest)
  ;; cons. Adding things to the queue is basically (setf (cdr head) (list
  ;; new)) and removing them is basically (pop tail) [except that care must
  ;; be taken to handle the empty queue case correctly.]
  (queue-tail nil :type list)
  (queue-head nil :type list)
  ;; Block-start queue entries in effect at the queue head.
  (pending-blocks nil :type list))
(def!method print-object ((pstream pretty-stream) stream)
  ;; FIXME: CMU CL had #+NIL'ed out this code and done a hand-written
  ;; FORMAT hack instead. Make sure that this code actually works instead
  ;; of falling into infinite regress or something.
  (print-unreadable-object (pstream stream :type t :identity t)))

#!-sb-fluid (declaim (inline index-posn posn-index posn-column))
(defun index-posn (index stream)
  (declare (type index index) (type pretty-stream stream)
           (values posn))
  (+ index (pretty-stream-buffer-offset stream)))
(defun posn-index (posn stream)
  (declare (type posn posn) (type pretty-stream stream)
           (values index))
  (- posn (pretty-stream-buffer-offset stream)))
(defun posn-column (posn stream)
  (declare (type posn posn) (type pretty-stream stream)
           (values posn))
  (index-column (posn-index posn stream) stream))

;;; Is it OK to do pretty printing on this stream at this time?
(defun print-pretty-on-stream-p (stream)
  (and (pretty-stream-p stream)
       *print-pretty*))

;;;; stream interface routines

(defun pretty-out (stream char)
  (declare (type pretty-stream stream)
           (type character char))
  (cond ((char= char #\newline)
         (enqueue-newline stream :literal))
        (t
         (ensure-space-in-buffer stream 1)
         (let ((fill-pointer (pretty-stream-buffer-fill-pointer stream)))
           (setf (schar (pretty-stream-buffer stream) fill-pointer) char)
           (setf (pretty-stream-buffer-fill-pointer stream)
                 (1+ fill-pointer))))))

(defun pretty-sout (stream string start end)
  (declare (type pretty-stream stream)
           (type simple-string string)
           (type index start)
           (type (or index null) end))
  (let* ((end (or end (length string))))
    (unless (= start end)
      (sb!impl::string-dispatch (simple-base-string
                                 #!+sb-unicode
                                 (simple-array character (*)))
          string
        ;; For POSITION transform
        (declare (optimize (speed 2)))
        (let ((newline (position #\newline string :start start :end end)))
          (cond
            (newline
             (pretty-sout stream string start newline)
             (enqueue-newline stream :literal)
             (pretty-sout stream string (1+ newline) end))
            (t
             (let ((chars (- end start)))
               (loop
                  (let* ((available (ensure-space-in-buffer stream chars))
                         (count (min available chars))
                         (fill-pointer (pretty-stream-buffer-fill-pointer
                                        stream))
                         (new-fill-ptr (+ fill-pointer count)))
                    (if (typep string 'simple-base-string)
                        ;; FIXME: Reimplementing REPLACE, since it
                        ;; can't be inlined and we don't have a
                        ;; generic "simple-array -> simple-array"
                        ;; transform for it.
                        (loop for i from fill-pointer below new-fill-ptr
                              for j from start
                              with target = (pretty-stream-buffer stream)
                              do (setf (aref target i)
                                       (aref string j)))
                        (replace (pretty-stream-buffer stream)
                                 string
                                 :start1 fill-pointer :end1 new-fill-ptr
                                 :start2 start))
                    (setf (pretty-stream-buffer-fill-pointer stream)
                          new-fill-ptr)
                    (decf chars count)
                    (when (zerop count)
                      (return))
                    (incf start count)))))))))))

(defun pretty-misc (stream op &optional arg1 arg2)
  (declare (ignore stream op arg1 arg2)))

;;;; logical blocks

(defstruct (logical-block (:copier nil))
  ;; The column this logical block started in.
  (start-column 0 :type column)
  ;; The column the current section started in.
  (section-column 0 :type column)
  ;; The length of the per-line prefix. We can't move the indentation
  ;; left of this.
  (per-line-prefix-end 0 :type index)
  ;; The overall length of the prefix, including any indentation.
  (prefix-length 0 :type index)
  ;; The overall length of the suffix.
  (suffix-length 0 :type index)
  ;; The line number
  (section-start-line 0 :type index))

(defun really-start-logical-block (stream column prefix suffix)
  (let* ((blocks (pretty-stream-blocks stream))
         (prev-block (car blocks))
         (per-line-end (logical-block-per-line-prefix-end prev-block))
         (prefix-length (logical-block-prefix-length prev-block))
         (suffix-length (logical-block-suffix-length prev-block))
         (block (make-logical-block
                 :start-column column
                 :section-column column
                 :per-line-prefix-end per-line-end
                 :prefix-length prefix-length
                 :suffix-length suffix-length
                 :section-start-line (pretty-stream-line-number stream))))
    (setf (pretty-stream-blocks stream) (cons block blocks))
    (set-indentation stream column)
    (when prefix
      (setf (logical-block-per-line-prefix-end block) column)
      (replace (pretty-stream-prefix stream) prefix
               :start1 (- column (length prefix)) :end1 column))
    (when suffix
      (let* ((total-suffix (pretty-stream-suffix stream))
             (total-suffix-len (length total-suffix))
             (additional (length suffix))
             (new-suffix-len (+ suffix-length additional)))
        (when (> new-suffix-len total-suffix-len)
          (let ((new-total-suffix-len
                 (max (* total-suffix-len 2)
                      (+ suffix-length
                         (floor (* additional 5) 4)))))
            (setf total-suffix
                  (replace (make-string new-total-suffix-len) total-suffix
                           :start1 (- new-total-suffix-len suffix-length)
                           :start2 (- total-suffix-len suffix-length)))
            (setf total-suffix-len new-total-suffix-len)
            (setf (pretty-stream-suffix stream) total-suffix)))
        (replace total-suffix suffix
                 :start1 (- total-suffix-len new-suffix-len)
                 :end1 (- total-suffix-len suffix-length))
        (setf (logical-block-suffix-length block) new-suffix-len))))
  nil)

(defun set-indentation (stream column)
  (let* ((prefix (pretty-stream-prefix stream))
         (prefix-len (length prefix))
         (block (car (pretty-stream-blocks stream)))
         (current (logical-block-prefix-length block))
         (minimum (logical-block-per-line-prefix-end block))
         (column (max minimum column)))
    (when (> column prefix-len)
      (setf prefix
            (replace (make-string (max (* prefix-len 2)
                                       (+ prefix-len
                                          (floor (* (- column prefix-len) 5)
                                                 4))))
                     prefix
                     :end1 current))
      (setf (pretty-stream-prefix stream) prefix))
    (when (> column current)
      (fill prefix #\space :start current :end column))
    (setf (logical-block-prefix-length block) column)))

(defun really-end-logical-block (stream)
  (let* ((old (pop (pretty-stream-blocks stream)))
         (old-indent (logical-block-prefix-length old))
         (new (car (pretty-stream-blocks stream)))
         (new-indent (logical-block-prefix-length new)))
    (when (> new-indent old-indent)
      (fill (pretty-stream-prefix stream) #\space
            :start old-indent :end new-indent)))
  nil)

;;;; the pending operation queue

(defstruct (queued-op (:constructor nil)
                      (:copier nil))
  (posn 0 :type posn))

(defmacro enqueue (stream type &rest args)
  (let ((constructor (symbolicate "MAKE-" type)))
    (once-only ((stream stream)
                (entry `(,constructor :posn
                                      (index-posn
                                       (pretty-stream-buffer-fill-pointer
                                        ,stream)
                                       ,stream)
                                      ,@args))
                (op `(list ,entry))
                (head `(pretty-stream-queue-head ,stream)))
      `(progn
         (if ,head
             (setf (cdr ,head) ,op)
             (setf (pretty-stream-queue-tail ,stream) ,op))
         (setf (pretty-stream-queue-head ,stream) ,op)
         ,entry))))

(defstruct (section-start (:include queued-op)
                          (:constructor nil)
                          (:copier nil))
  (depth 0 :type index)
  (section-end nil :type (or null newline block-end)))

(defstruct (newline (:include section-start)
                    (:copier nil))
  (kind (missing-arg)
        :type (member :linear :fill :miser :literal :mandatory)))

(defun enqueue-newline (stream kind)
  (let* ((depth (length (pretty-stream-pending-blocks stream)))
         (newline (enqueue stream newline :kind kind :depth depth)))
    (dolist (entry (pretty-stream-queue-tail stream))
      (when (and (not (eq newline entry))
                 (section-start-p entry)
                 (null (section-start-section-end entry))
                 (<= depth (section-start-depth entry)))
        (setf (section-start-section-end entry) newline))))
  (maybe-output stream (or (eq kind :literal) (eq kind :mandatory))))

(defstruct (indentation (:include queued-op)
                        (:copier nil))
  (kind (missing-arg) :type (member :block :current))
  (amount 0 :type fixnum))

(defun enqueue-indent (stream kind amount)
  (enqueue stream indentation :kind kind :amount amount))

(defstruct (block-start (:include section-start)
                        (:copier nil))
  (block-end nil :type (or null block-end))
  (prefix nil :type (or null simple-string))
  (suffix nil :type (or null simple-string)))

(defun start-logical-block (stream prefix per-line-p suffix)
  ;; (In the PPRINT-LOGICAL-BLOCK form which calls us,
  ;; :PREFIX and :PER-LINE-PREFIX have hairy defaulting behavior,
  ;; and might end up being NIL.)
  (declare (type (or null string) prefix))
  ;; (But the defaulting behavior of PPRINT-LOGICAL-BLOCK :SUFFIX is
  ;; trivial, so it should always be a string.)
  (declare (type string suffix))
  (when prefix
    (unless (typep prefix 'simple-string)
      (setq prefix (coerce prefix '(simple-array character (*)))))
    (pretty-sout stream prefix 0 (length prefix)))
  (unless (typep suffix 'simple-string)
    (setq suffix (coerce suffix '(simple-array character (*)))))
  (let* ((pending-blocks (pretty-stream-pending-blocks stream))
         (start (enqueue stream block-start
                         :prefix (and per-line-p prefix)
                         :suffix suffix
                         :depth (length pending-blocks))))
    (setf (pretty-stream-pending-blocks stream)
          (cons start pending-blocks))))

(defstruct (block-end (:include queued-op)
                      (:copier nil))
  (suffix nil :type (or null simple-string)))

(defun end-logical-block (stream)
  (let* ((start (pop (pretty-stream-pending-blocks stream)))
         (suffix (block-start-suffix start))
         (end (enqueue stream block-end :suffix suffix)))
    (when suffix
      (pretty-sout stream suffix 0 (length suffix)))
    (setf (block-start-block-end start) end)))

(defstruct (tab (:include queued-op)
                (:copier nil))
  (sectionp nil :type (member t nil))
  (relativep nil :type (member t nil))
  (colnum 0 :type column)
  (colinc 0 :type column))

(defun enqueue-tab (stream kind colnum colinc)
  (multiple-value-bind (sectionp relativep)
      (ecase kind
        (:line (values nil nil))
        (:line-relative (values nil t))
        (:section (values t nil))
        (:section-relative (values t t)))
    (enqueue stream tab :sectionp sectionp :relativep relativep
             :colnum colnum :colinc colinc)))

;;;; tab support

(defun compute-tab-size (tab section-start column)
  (let* ((origin (if (tab-sectionp tab) section-start 0))
         (colnum (tab-colnum tab))
         (colinc (tab-colinc tab))
         (position (- column origin)))
    (cond ((tab-relativep tab)
           (unless (<= colinc 1)
             (let ((newposn (+ position colnum)))
               (let ((rem (rem newposn colinc)))
                 (unless (zerop rem)
                   (incf colnum (- colinc rem))))))
           colnum)
          ((< position colnum)
           (- colnum position))
          ((zerop colinc) 0)
          (t
           (- colinc
              (rem (- position colnum) colinc))))))

(defun index-column (index stream)
  (let ((column (pretty-stream-buffer-start-column stream))
        (section-start (logical-block-section-column
                        (first (pretty-stream-blocks stream))))
        (end-posn (index-posn index stream)))
    (dolist (op (pretty-stream-queue-tail stream))
      (when (>= (queued-op-posn op) end-posn)
        (return))
      (typecase op
        (tab
         (incf column
               (compute-tab-size op
                                 section-start
                                 (+ column
                                    (posn-index (tab-posn op)
                                                    stream)))))
        ((or newline block-start)
         (setf section-start
               (+ column (posn-index (queued-op-posn op)
                                         stream))))))
    (+ column index)))

(defun expand-tabs (stream through)
  (let ((insertions nil)
        (additional 0)
        (column (pretty-stream-buffer-start-column stream))
        (section-start (logical-block-section-column
                        (first (pretty-stream-blocks stream)))))
    (dolist (op (pretty-stream-queue-tail stream))
      (typecase op
        (tab
         (let* ((index (posn-index (tab-posn op) stream))
                (tabsize (compute-tab-size op
                                           section-start
                                           (+ column index))))
           (unless (zerop tabsize)
             (push (cons index tabsize) insertions)
             (incf additional tabsize)
             (incf column tabsize))))
        ((or newline block-start)
         (setf section-start
               (+ column (posn-index (queued-op-posn op) stream)))))
      (when (eq op through)
        (return)))
    (when insertions
      (let* ((fill-ptr (pretty-stream-buffer-fill-pointer stream))
             (new-fill-ptr (+ fill-ptr additional))
             (buffer (pretty-stream-buffer stream))
             (new-buffer buffer)
             (length (length buffer))
             (end fill-ptr))
        (when (> new-fill-ptr length)
          (let ((new-length (max (* length 2)
                                 (+ fill-ptr
                                    (floor (* additional 5) 4)))))
            (setf new-buffer (make-string new-length))
            (setf (pretty-stream-buffer stream) new-buffer)))
        (setf (pretty-stream-buffer-fill-pointer stream) new-fill-ptr)
        (decf (pretty-stream-buffer-offset stream) additional)
        (dolist (insertion insertions)
          (let* ((srcpos (car insertion))
                 (amount (cdr insertion))
                 (dstpos (+ srcpos additional)))
            (replace new-buffer buffer :start1 dstpos :start2 srcpos :end2 end)
            (fill new-buffer #\space :start (- dstpos amount) :end dstpos)
            (decf additional amount)
            (setf end srcpos)))
        (unless (eq new-buffer buffer)
          (replace new-buffer buffer :end1 end :end2 end))))))

;;;; stuff to do the actual outputting

(defun ensure-space-in-buffer (stream want)
  (declare (type pretty-stream stream)
           (type index want))
  (let* ((buffer (pretty-stream-buffer stream))
         (length (length buffer))
         (fill-ptr (pretty-stream-buffer-fill-pointer stream))
         (available (- length fill-ptr)))
    (cond ((plusp available)
           available)
          ((> fill-ptr (pretty-stream-line-length stream))
           (unless (maybe-output stream nil)
             (output-partial-line stream))
           (ensure-space-in-buffer stream want))
          (t
           (let* ((new-length (max (* length 2)
                                   (+ length
                                      (floor (* want 5) 4))))
                  (new-buffer (make-string new-length)))
             (setf (pretty-stream-buffer stream) new-buffer)
             (replace new-buffer buffer :end1 fill-ptr)
             (- new-length fill-ptr))))))

(defun maybe-output (stream force-newlines-p)
  (declare (type pretty-stream stream))
  (let ((tail (pretty-stream-queue-tail stream))
        (output-anything nil))
    (loop
      (unless tail
        (setf (pretty-stream-queue-head stream) nil)
        (return))
      (let ((next (pop tail)))
        (etypecase next
          (newline
           (when (ecase (newline-kind next)
                   ((:literal :mandatory :linear) t)
                   (:miser (misering-p stream))
                   (:fill
                    (or (misering-p stream)
                        (> (pretty-stream-line-number stream)
                           (logical-block-section-start-line
                            (first (pretty-stream-blocks stream))))
                        (ecase (fits-on-line-p stream
                                               (newline-section-end next)
                                               force-newlines-p)
                          ((t) nil)
                          ((nil) t)
                          (:dont-know
                           (return))))))
             (setf output-anything t)
             (output-line stream next)))
          (indentation
           (unless (misering-p stream)
             (set-indentation stream
                              (+ (ecase (indentation-kind next)
                                   (:block
                                    (logical-block-start-column
                                     (car (pretty-stream-blocks stream))))
                                   (:current
                                    (posn-column
                                     (indentation-posn next)
                                     stream)))
                                 (indentation-amount next)))))
          (block-start
           (ecase (fits-on-line-p stream (block-start-section-end next)
                                  force-newlines-p)
             ((t)
              ;; Just nuke the whole logical block and make it look
              ;; like one nice long literal.
              (let ((end (block-start-block-end next)))
                (expand-tabs stream end)
                (setf tail (cdr (member end tail)))))
             ((nil)
              (really-start-logical-block
               stream
               (posn-column (block-start-posn next) stream)
               (block-start-prefix next)
               (block-start-suffix next)))
             (:dont-know
              (return))))
          (block-end
           (really-end-logical-block stream))
          (tab
           (expand-tabs stream next))))
      (setf (pretty-stream-queue-tail stream) tail))
    output-anything))

(defun misering-p (stream)
  (declare (type pretty-stream stream))
  (and *print-miser-width*
       (<= (- (pretty-stream-line-length stream)
              (logical-block-start-column (car (pretty-stream-blocks stream))))
           *print-miser-width*)))

(defun fits-on-line-p (stream until force-newlines-p)
  (let ((available (pretty-stream-line-length stream)))
    (when (and (not *print-readably*)
               (pretty-stream-print-lines stream)
               (= (pretty-stream-print-lines stream)
                  (pretty-stream-line-number stream)))
      (decf available 3) ; for the `` ..''
      (decf available (logical-block-suffix-length
                       (car (pretty-stream-blocks stream)))))
    (cond (until
           (<= (posn-column (queued-op-posn until) stream) available))
          (force-newlines-p nil)
          ((> (index-column (pretty-stream-buffer-fill-pointer stream) stream)
              available)
           nil)
          (t
           :dont-know))))

(defun output-line (stream until)
  (declare (type pretty-stream stream)
           (type newline until))
  (let* ((target (pretty-stream-target stream))
         (buffer (pretty-stream-buffer stream))
         (kind (newline-kind until))
         (literal-p (eq kind :literal))
         (amount-to-consume (posn-index (newline-posn until) stream))
         (amount-to-print
          (if literal-p
              amount-to-consume
              (let ((last-non-blank
                     (position #\space buffer :end amount-to-consume
                               :from-end t :test #'char/=)))
                (if last-non-blank
                    (1+ last-non-blank)
                    0)))))
    (write-string buffer target :end amount-to-print)
    (let ((line-number (pretty-stream-line-number stream)))
      (incf line-number)
      (when (and (not *print-readably*)
                 (pretty-stream-print-lines stream)
                 (>= line-number (pretty-stream-print-lines stream)))
        (write-string " .." target)
        (let ((suffix-length (logical-block-suffix-length
                              (car (pretty-stream-blocks stream)))))
          (unless (zerop suffix-length)
            (let* ((suffix (pretty-stream-suffix stream))
                   (len (length suffix)))
              (write-string suffix target
                            :start (- len suffix-length)
                            :end len))))
        (throw 'line-limit-abbreviation-happened t))
      (setf (pretty-stream-line-number stream) line-number)
      (write-char #\newline target)
      (setf (pretty-stream-buffer-start-column stream) 0)
      (let* ((fill-ptr (pretty-stream-buffer-fill-pointer stream))
             (block (first (pretty-stream-blocks stream)))
             (prefix-len
              (if literal-p
                  (logical-block-per-line-prefix-end block)
                  (logical-block-prefix-length block)))
             (shift (- amount-to-consume prefix-len))
             (new-fill-ptr (- fill-ptr shift))
             (new-buffer buffer)
             (buffer-length (length buffer)))
        (when (> new-fill-ptr buffer-length)
          (setf new-buffer
                (make-string (max (* buffer-length 2)
                                  (+ buffer-length
                                     (floor (* (- new-fill-ptr buffer-length)
                                               5)
                                            4)))))
          (setf (pretty-stream-buffer stream) new-buffer))
        (replace new-buffer buffer
                 :start1 prefix-len :start2 amount-to-consume :end2 fill-ptr)
        (replace new-buffer (pretty-stream-prefix stream)
                 :end1 prefix-len)
        (setf (pretty-stream-buffer-fill-pointer stream) new-fill-ptr)
        (incf (pretty-stream-buffer-offset stream) shift)
        (unless literal-p
          (setf (logical-block-section-column block) prefix-len)
          (setf (logical-block-section-start-line block) line-number))))))

(defun output-partial-line (stream)
  (let* ((fill-ptr (pretty-stream-buffer-fill-pointer stream))
         (tail (pretty-stream-queue-tail stream))
         (count
          (if tail
              (posn-index (queued-op-posn (car tail)) stream)
              fill-ptr))
         (new-fill-ptr (- fill-ptr count))
         (buffer (pretty-stream-buffer stream)))
    (when (zerop count)
      (error "Output-partial-line called when nothing can be output."))
    (write-string buffer (pretty-stream-target stream)
                  :start 0 :end count)
    (incf (pretty-stream-buffer-start-column stream) count)
    (replace buffer buffer :end1 new-fill-ptr :start2 count :end2 fill-ptr)
    (setf (pretty-stream-buffer-fill-pointer stream) new-fill-ptr)
    (incf (pretty-stream-buffer-offset stream) count)))

(defun force-pretty-output (stream)
  (maybe-output stream nil)
  (expand-tabs stream nil)
  (write-string (pretty-stream-buffer stream)
                (pretty-stream-target stream)
                :end (pretty-stream-buffer-fill-pointer stream)))

;;;; user interface to the pretty printer

(defun pprint-newline (kind &optional stream)
  #!+sb-doc
  "Output a conditional newline to STREAM (which defaults to
   *STANDARD-OUTPUT*) if it is a pretty-printing stream, and do
   nothing if not. KIND can be one of:
     :LINEAR - A line break is inserted if and only if the immediatly
        containing section cannot be printed on one line.
     :MISER - Same as LINEAR, but only if ``miser-style'' is in effect.
        (See *PRINT-MISER-WIDTH*.)
     :FILL - A line break is inserted if and only if either:
       (a) the following section cannot be printed on the end of the
           current line,
       (b) the preceding section was not printed on a single line, or
       (c) the immediately containing section cannot be printed on one
           line and miser-style is in effect.
     :MANDATORY - A line break is always inserted.
   When a line break is inserted by any type of conditional newline, any
   blanks that immediately precede the conditional newline are ommitted
   from the output and indentation is introduced at the beginning of the
   next line. (See PPRINT-INDENT.)"
  (declare (type (member :linear :miser :fill :mandatory) kind)
           (type (or stream (member t nil)) stream)
           (values null))
  (let ((stream (case stream
                  ((t) *terminal-io*)
                  ((nil) *standard-output*)
                  (t stream))))
    (when (print-pretty-on-stream-p stream)
      (enqueue-newline stream kind)))
  nil)

(defun pprint-indent (relative-to n &optional stream)
  #!+sb-doc
  "Specify the indentation to use in the current logical block if
STREAM \(which defaults to *STANDARD-OUTPUT*) is a pretty-printing
stream and do nothing if not. (See PPRINT-LOGICAL-BLOCK.) N is the
indentation to use (in ems, the width of an ``m'') and RELATIVE-TO can
be either:

     :BLOCK - Indent relative to the column the current logical block
        started on.

     :CURRENT - Indent relative to the current column.

The new indentation value does not take effect until the following
line break."
  (declare (type (member :block :current) relative-to)
           (type real n)
           (type (or stream (member t nil)) stream)
           (values null))
  (let ((stream (case stream
                  ((t) *terminal-io*)
                  ((nil) *standard-output*)
                  (t stream))))
    (when (print-pretty-on-stream-p stream)
      (enqueue-indent stream relative-to (truncate n))))
  nil)

(defun pprint-tab (kind colnum colinc &optional stream)
  #!+sb-doc
  "If STREAM (which defaults to *STANDARD-OUTPUT*) is a pretty-printing
   stream, perform tabbing based on KIND, otherwise do nothing. KIND can
   be one of:
     :LINE - Tab to column COLNUM. If already past COLNUM tab to the next
       multiple of COLINC.
     :SECTION - Same as :LINE, but count from the start of the current
       section, not the start of the line.
     :LINE-RELATIVE - Output COLNUM spaces, then tab to the next multiple of
       COLINC.
     :SECTION-RELATIVE - Same as :LINE-RELATIVE, but count from the start
       of the current section, not the start of the line."
  (declare (type (member :line :section :line-relative :section-relative) kind)
           (type unsigned-byte colnum colinc)
           (type (or stream (member t nil)) stream)
           (values null))
  (let ((stream (case stream
                  ((t) *terminal-io*)
                  ((nil) *standard-output*)
                  (t stream))))
    (when (print-pretty-on-stream-p stream)
      (enqueue-tab stream kind colnum colinc)))
  nil)

(defun pprint-fill (stream list &optional (colon? t) atsign?)
  #!+sb-doc
  "Output LIST to STREAM putting :FILL conditional newlines between each
   element. If COLON? is NIL (defaults to T), then no parens are printed
   around the output. ATSIGN? is ignored (but allowed so that PPRINT-FILL
   can be used with the ~/.../ format directive."
  (declare (ignore atsign?))
  (pprint-logical-block (stream list
                                :prefix (if colon? "(" "")
                                :suffix (if colon? ")" ""))
    (pprint-exit-if-list-exhausted)
    (loop
      (output-object (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (write-char #\space stream)
      (pprint-newline :fill stream))))

(defun pprint-linear (stream list &optional (colon? t) atsign?)
  #!+sb-doc
  "Output LIST to STREAM putting :LINEAR conditional newlines between each
   element. If COLON? is NIL (defaults to T), then no parens are printed
   around the output. ATSIGN? is ignored (but allowed so that PPRINT-LINEAR
   can be used with the ~/.../ format directive."
  (declare (ignore atsign?))
  (pprint-logical-block (stream list
                                :prefix (if colon? "(" "")
                                :suffix (if colon? ")" ""))
    (pprint-exit-if-list-exhausted)
    (loop
      (output-object (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (write-char #\space stream)
      (pprint-newline :linear stream))))

(defun pprint-tabular (stream list &optional (colon? t) atsign? tabsize)
  #!+sb-doc
  "Output LIST to STREAM tabbing to the next column that is an even multiple
   of TABSIZE (which defaults to 16) between each element. :FILL style
   conditional newlines are also output between each element. If COLON? is
   NIL (defaults to T), then no parens are printed around the output.
   ATSIGN? is ignored (but allowed so that PPRINT-TABULAR can be used with
   the ~/.../ format directive."
  (declare (ignore atsign?))
  (pprint-logical-block (stream list
                                :prefix (if colon? "(" "")
                                :suffix (if colon? ")" ""))
    (pprint-exit-if-list-exhausted)
    (loop
      (output-object (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (write-char #\space stream)
      (pprint-tab :section-relative 0 (or tabsize 16) stream)
      (pprint-newline :fill stream))))

;;;; pprint-dispatch tables

(defvar *standard-pprint-dispatch-table*)
(defvar *initial-pprint-dispatch-table*)
(defvar *building-initial-table* nil)

(defstruct (pprint-dispatch-entry (:copier nil))
  ;; the type specifier for this entry
  (type (missing-arg) :type t)
  ;; a function to test to see whether an object is of this time.
  ;; Pretty must just (LAMBDA (OBJ) (TYPEP OBJECT TYPE)) except that
  ;; we handle the CONS type specially so that (CONS (MEMBER FOO))
  ;; works. We don't bother computing this for entries in the CONS
  ;; hash table, because we don't need it.
  (test-fn nil :type (or function null))
  ;; the priority for this guy
  (priority 0 :type real)
  ;; T iff one of the original entries.
  (initial-p *building-initial-table* :type (member t nil))
  ;; and the associated function
  (fun (missing-arg) :type callable))
(def!method print-object ((entry pprint-dispatch-entry) stream)
  (print-unreadable-object (entry stream :type t)
    (format stream "type=~S, priority=~S~@[ [initial]~]"
            (pprint-dispatch-entry-type entry)
            (pprint-dispatch-entry-priority entry)
            (pprint-dispatch-entry-initial-p entry))))

(defun cons-type-specifier-p (spec)
  (and (consp spec)
       (eq (car spec) 'cons)
       (cdr spec)
       (null (cddr spec))
       (let ((car (cadr spec)))
         (and (consp car)
              (let ((carcar (car car)))
                (or (eq carcar 'member)
                    (eq carcar 'eql)))
              (cdr car)
              (null (cddr car))))))

(defun entry< (e1 e2)
  (declare (type pprint-dispatch-entry e1 e2))
  (if (pprint-dispatch-entry-initial-p e1)
      (if (pprint-dispatch-entry-initial-p e2)
          (< (pprint-dispatch-entry-priority e1)
             (pprint-dispatch-entry-priority e2))
          t)
      (if (pprint-dispatch-entry-initial-p e2)
          nil
          (< (pprint-dispatch-entry-priority e1)
             (pprint-dispatch-entry-priority e2)))))

(macrolet ((frob (name x)
             `(cons ',x (named-lambda ,(symbolicate "PPRINT-DISPATCH-" name) (object)
                            ,x))))
  (defvar *precompiled-pprint-dispatch-funs*
    (list (frob array (typep object 'array))
          (frob function-call (and (consp object)
                                    (symbolp (car object))
                                    (fboundp (car object))))
          (frob cons (typep object 'cons)))))

(defun compute-test-fn (type)
  (let ((was-cons nil))
    (labels ((compute-test-expr (type object)
               (if (listp type)
                   (case (car type)
                     (cons
                      (setq was-cons t)
                      (destructuring-bind
                          (&optional (car nil car-p) (cdr nil cdr-p))
                          (cdr type)
                        `(and (consp ,object)
                              ,@(when car-p
                                  `(,(compute-test-expr
                                      car `(car ,object))))
                              ,@(when cdr-p
                                  `(,(compute-test-expr
                                      cdr `(cdr ,object)))))))
                     (not
                      (destructuring-bind (type) (cdr type)
                        `(not ,(compute-test-expr type object))))
                     (and
                      `(and ,@(mapcar (lambda (type)
                                        (compute-test-expr type object))
                                      (cdr type))))
                     (or
                      `(or ,@(mapcar (lambda (type)
                                       (compute-test-expr type object))
                                     (cdr type))))
                     (t
                      `(typep ,object ',type)))
                   `(typep ,object ',type))))
      (let ((expr (compute-test-expr type 'object)))
        (cond ((cdr (assoc expr *precompiled-pprint-dispatch-funs*
                           :test #'equal)))
              (t
               (let ((name (symbolicate "PPRINT-DISPATCH-"
                                        (if (symbolp type)
                                            type
                                            (write-to-string type
                                                             :escape t
                                                             :pretty nil
                                                             :readably nil)))))
                 (compile nil `(named-lambda ,name (object)
                                 ,expr)))))))))

(defun copy-pprint-dispatch (&optional (table *print-pprint-dispatch*))
  (declare (type (or pprint-dispatch-table null) table))
  (let* ((orig (or table *initial-pprint-dispatch-table*))
         (new (make-pprint-dispatch-table
               :entries (copy-list (pprint-dispatch-table-entries orig))))
         (new-cons-entries (pprint-dispatch-table-cons-entries new)))
    (maphash (lambda (key value)
               (setf (gethash key new-cons-entries) value))
             (pprint-dispatch-table-cons-entries orig))
    new))

(defun pprint-dispatch (object &optional (table *print-pprint-dispatch*))
  (declare (type (or pprint-dispatch-table null) table))
  (let* ((table (or table *initial-pprint-dispatch-table*))
         (cons-entry
          (and (consp object)
               (gethash (car object)
                        (pprint-dispatch-table-cons-entries table))))
         (entry
          (dolist (entry (pprint-dispatch-table-entries table) cons-entry)
            (when (and cons-entry
                       (entry< entry cons-entry))
              (return cons-entry))
            (when (funcall (pprint-dispatch-entry-test-fn entry) object)
              (return entry)))))
    (if entry
        (values (pprint-dispatch-entry-fun entry) t)
        (values (lambda (stream object)
                  (output-ugly-object object stream))
                nil))))

(defun assert-not-standard-pprint-dispatch-table (pprint-dispatch operation)
  (when (eq pprint-dispatch *standard-pprint-dispatch-table*)
    (cerror "Frob it anyway!" 'standard-pprint-dispatch-table-modified-error
            :operation operation)))

(defun set-pprint-dispatch (type function &optional
                            (priority 0) (table *print-pprint-dispatch*))
  (declare (type (or null callable) function)
           (type real priority)
           (type pprint-dispatch-table table))
  (/show0 "entering SET-PPRINT-DISPATCH, TYPE=...")
  (/hexstr type)
  (assert-not-standard-pprint-dispatch-table table 'set-pprint-dispatch)
  (if function
      (if (cons-type-specifier-p type)
          (setf (gethash (second (second type))
                         (pprint-dispatch-table-cons-entries table))
                (make-pprint-dispatch-entry :type type
                                            :priority priority
                                            :fun function))
          (let ((list (delete type (pprint-dispatch-table-entries table)
                              :key #'pprint-dispatch-entry-type
                              :test #'equal))
                (entry (make-pprint-dispatch-entry
                        :type type
                        :test-fn (compute-test-fn type)
                        :priority priority
                        :fun function)))
            (do ((prev nil next)
                 (next list (cdr next)))
                ((null next)
                 (if prev
                     (setf (cdr prev) (list entry))
                     (setf list (list entry))))
              (when (entry< (car next) entry)
                (if prev
                    (setf (cdr prev) (cons entry next))
                    (setf list (cons entry next)))
                (return)))
            (setf (pprint-dispatch-table-entries table) list)))
      (if (cons-type-specifier-p type)
          (remhash (second (second type))
                   (pprint-dispatch-table-cons-entries table))
          (setf (pprint-dispatch-table-entries table)
                (delete type (pprint-dispatch-table-entries table)
                        :key #'pprint-dispatch-entry-type
                        :test #'equal))))
  (/show0 "about to return NIL from SET-PPRINT-DISPATCH")
  nil)

;;;; standard pretty-printing routines

(defun pprint-array (stream array)
  (cond ((or (and (null *print-array*) (null *print-readably*))
             (stringp array)
             (bit-vector-p array))
         (output-ugly-object array stream))
        ((and *print-readably*
              (not (array-readably-printable-p array)))
         (let ((*print-readably* nil))
           (error 'print-not-readable :object array)))
        ((vectorp array)
         (pprint-vector stream array))
        (t
         (pprint-multi-dim-array stream array))))

(defun pprint-vector (stream vector)
  (pprint-logical-block (stream nil :prefix "#(" :suffix ")")
    (dotimes (i (length vector))
      (unless (zerop i)
        (format stream " ~:_"))
      (pprint-pop)
      (output-object (aref vector i) stream))))

(defun pprint-multi-dim-array (stream array)
  (funcall (formatter "#~DA") stream (array-rank array))
  (with-array-data ((data array) (start) (end))
    (declare (ignore end))
    (labels ((output-guts (stream index dimensions)
               (if (null dimensions)
                   (output-object (aref data index) stream)
                   (pprint-logical-block
                       (stream nil :prefix "(" :suffix ")")
                     (let ((dim (car dimensions)))
                       (unless (zerop dim)
                         (let* ((dims (cdr dimensions))
                                (index index)
                                (step (reduce #'* dims))
                                (count 0))
                           (loop
                             (pprint-pop)
                             (output-guts stream index dims)
                             (when (= (incf count) dim)
                               (return))
                             (write-char #\space stream)
                             (pprint-newline (if dims :linear :fill)
                                             stream)
                             (incf index step)))))))))
      (output-guts stream start (array-dimensions array)))))

(defun pprint-lambda-list (stream lambda-list &rest noise)
  (declare (ignore noise))
  (when (and (consp lambda-list)
             (member (car lambda-list) *backq-tokens*))
    ;; if this thing looks like a backquoty thing, then we don't want
    ;; to destructure it, we want to output it straight away.  [ this
    ;; is the exception to the normal processing: if we did this
    ;; generally we would find lambda lists such as (FUNCTION FOO)
    ;; being printed as #'FOO ]  -- CSR, 2003-12-07
    (output-object lambda-list stream)
    (return-from pprint-lambda-list nil))
  (pprint-logical-block (stream lambda-list :prefix "(" :suffix ")")
    (let ((state :required)
          (first t))
      (loop
        (pprint-exit-if-list-exhausted)
        (unless first
          (write-char #\space stream))
        (let ((arg (pprint-pop)))
          (unless first
            (case arg
              (&optional
               (setf state :optional)
               (pprint-newline :linear stream))
              ((&rest &body)
               (setf state :required)
               (pprint-newline :linear stream))
              (&key
               (setf state :key)
               (pprint-newline :linear stream))
              (&aux
               (setf state :optional)
               (pprint-newline :linear stream))
              (t
               (pprint-newline :fill stream))))
          (ecase state
            (:required
             (pprint-lambda-list stream arg))
            ((:optional :key)
             (pprint-logical-block
                 (stream arg :prefix "(" :suffix ")")
               (pprint-exit-if-list-exhausted)
               (if (eq state :key)
                   (pprint-logical-block
                       (stream (pprint-pop) :prefix "(" :suffix ")")
                     (pprint-exit-if-list-exhausted)
                     (output-object (pprint-pop) stream)
                     (pprint-exit-if-list-exhausted)
                     (write-char #\space stream)
                     (pprint-newline :fill stream)
                     (pprint-lambda-list stream (pprint-pop))
                     (loop
                       (pprint-exit-if-list-exhausted)
                       (write-char #\space stream)
                       (pprint-newline :fill stream)
                       (output-object (pprint-pop) stream)))
                   (pprint-lambda-list stream (pprint-pop)))
               (loop
                 (pprint-exit-if-list-exhausted)
                 (write-char #\space stream)
                 (pprint-newline :linear stream)
                 (output-object (pprint-pop) stream))))))
        (setf first nil)))))

(defun pprint-lambda (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter
            ;; KLUDGE: This format string, and other format strings which also
            ;; refer to SB!PRETTY, rely on the current SBCL not-quite-ANSI
            ;; behavior of FORMATTER in order to make code which survives the
            ;; transition when SB!PRETTY is renamed to SB-PRETTY after cold
            ;; init. (ANSI says that the FORMATTER functions should be
            ;; equivalent to the format string, but the SBCL FORMATTER
            ;; functions contain references to package objects, not package
            ;; names, so they keep right on going if the packages are renamed.)
            ;; If our FORMATTER behavior is ever made more compliant, the code
            ;; here will have to change. -- WHN 19991207
            "~:<~^~W~^~3I ~:_~/SB!PRETTY:PPRINT-LAMBDA-LIST/~1I~@{ ~_~W~}~:>")
           stream
           list))

(defun pprint-block (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter "~:<~^~W~^~3I ~:_~W~1I~@{ ~_~W~}~:>") stream list))

(defun pprint-flet (stream list &rest noise)
  (declare (ignore noise))
  (if (and (consp list)
           (consp (cdr list))
           (cddr list))
      (funcall (formatter
                "~:<~^~W~^ ~@_~:<~@{~:<~^~W~^~3I ~:_~/SB!PRETTY:PPRINT-LAMBDA-LIST/~1I~:@_~@{~W~^ ~_~}~:>~^ ~_~}~:>~1I~@:_~@{~W~^ ~_~}~:>")
               stream
               list)
      ;; for printing function names like (flet foo)
      (pprint-logical-block (stream list :prefix "(" :suffix ")")
        (pprint-exit-if-list-exhausted)
        (write (pprint-pop) :stream stream)
        (loop
           (pprint-exit-if-list-exhausted)
           (write-char #\space stream)
           (write (pprint-pop) :stream stream)))))

(defun pprint-let (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter "~:<~^~W~^ ~@_~:<~@{~:<~^~W~@{ ~_~W~}~:>~^ ~_~}~:>~1I~:@_~@{~W~^ ~_~}~:>")
           stream
           list))

(defun pprint-progn (stream list &rest noise)
  (declare (ignore noise))
  (pprint-linear stream list))

(defun pprint-progv (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter "~:<~^~W~^~3I ~_~W~^ ~_~W~^~1I~@{ ~_~W~}~:>")
           stream list))

(defun pprint-prog2 (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter "~:<~^~W~^~3I ~:_~W~^ ~_~W~^~1I~@{ ~_~W~}~:>")
           stream list))

(defvar *pprint-quote-with-syntactic-sugar* t)

(defun pprint-quote (stream list &rest noise)
  (declare (ignore noise))
  (if (and (consp list)
           (consp (cdr list))
           (null (cddr list))
           *pprint-quote-with-syntactic-sugar*)
      (case (car list)
        (function
         (write-string "#'" stream)
         (output-object (cadr list) stream))
        (quote
         (write-char #\' stream)
         (output-object (cadr list) stream))
        (t
         (pprint-fill stream list)))
      (pprint-fill stream list)))

(defun pprint-declare (stream list &rest noise)
  (declare (ignore noise))
  ;; Make sure to print (DECLARE (FUNCTION F)) not (DECLARE #'A).
  (let ((*pprint-quote-with-syntactic-sugar* nil))
    (pprint-spread-fun-call stream list)))

;;; Try to print every variable-value pair on one line; if that doesn't
;;; work print the value indented by 2 spaces:
;;;
;;;      (setq foo bar
;;;            quux xoo)
;;;  vs.
;;;      (setf foo
;;;              (long form ...)
;;;            quux xoo)
(defun pprint-setq (stream list &rest noise)
  (declare (ignore noise))
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    (pprint-exit-if-list-exhausted)
    (output-object (pprint-pop) stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\space stream)
    (pprint-newline :miser stream)
    (pprint-logical-block (stream (cdr list) :prefix "" :suffix "")
      (loop
       (pprint-indent :block 2 stream)
       (output-object (pprint-pop) stream)
       (pprint-exit-if-list-exhausted)
       (write-char #\space stream)
       (pprint-newline :fill stream)
       (pprint-indent :block 0 stream)
       (output-object (pprint-pop) stream)
       (pprint-exit-if-list-exhausted)
       (write-char #\space stream)
       (pprint-newline :mandatory stream)))))

;;; FIXME: could become SB!XC:DEFMACRO wrapped in EVAL-WHEN (COMPILE EVAL)
(defmacro pprint-tagbody-guts (stream)
  `(loop
     (pprint-exit-if-list-exhausted)
     (write-char #\space ,stream)
     (let ((form-or-tag (pprint-pop)))
       (pprint-indent :block
                      (if (atom form-or-tag) 0 1)
                      ,stream)
       (pprint-newline :linear ,stream)
       (output-object form-or-tag ,stream))))

(defun pprint-tagbody (stream list &rest noise)
  (declare (ignore noise))
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    (pprint-exit-if-list-exhausted)
    (output-object (pprint-pop) stream)
    (pprint-tagbody-guts stream)))

(defun pprint-case (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter
            "~:<~^~W~^ ~3I~:_~W~1I~@{ ~_~:<~^~:/SB!PRETTY:PPRINT-FILL/~^~@{ ~_~W~}~:>~}~:>")
           stream
           list))

(defun pprint-defun (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter
            "~:<~^~W~^ ~@_~:I~W~^ ~:_~/SB!PRETTY:PPRINT-LAMBDA-LIST/~1I~@{ ~_~W~}~:>")
           stream
           list))

(defun pprint-defmethod (stream list &rest noise)
  (declare (ignore noise))
  (if (consp (third list))
      (pprint-defun stream list)
      (funcall (formatter
                "~:<~^~W~^ ~@_~:I~W~^ ~W~^ ~:_~/SB!PRETTY:PPRINT-LAMBDA-LIST/~1I~@{ ~_~W~}~:>")
               stream
               list)))

(defun pprint-defpackage (stream list &rest noise)
  (declare (ignore noise))
  (funcall  (formatter
             "~:<~W~^ ~3I~:_~W~^~1I~@{~:@_~:<~^~W~^ ~:I~@_~@{~W~^ ~_~}~:>~}~:>")
            stream
            list))

(defun pprint-destructuring-bind (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter
            "~:<~^~W~^~3I ~_~:/SB!PRETTY:PPRINT-LAMBDA-LIST/~^ ~_~W~^~1I~@{ ~_~W~}~:>")
           stream list))

(defun pprint-do (stream list &rest noise)
  (declare (ignore noise))
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    (pprint-exit-if-list-exhausted)
    (output-object (pprint-pop) stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\space stream)
    (pprint-indent :current 0 stream)
    (funcall (formatter "~:<~@{~:<~^~W~^ ~@_~:I~W~@{ ~_~W~}~:>~^~:@_~}~:>")
             stream
             (pprint-pop))
    (pprint-exit-if-list-exhausted)
    (write-char #\space stream)
    (pprint-newline :linear stream)
    (pprint-linear stream (pprint-pop))
    (pprint-tagbody-guts stream)))

(defun pprint-dolist (stream list &rest noise)
  (declare (ignore noise))
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    (pprint-exit-if-list-exhausted)
    (output-object (pprint-pop) stream)
    (pprint-exit-if-list-exhausted)
    (pprint-indent :block 3 stream)
    (write-char #\space stream)
    (pprint-newline :fill stream)
    (funcall (formatter "~:<~^~W~^ ~:_~:I~W~@{ ~_~W~}~:>")
             stream
             (pprint-pop))
    (pprint-tagbody-guts stream)))

(defun pprint-typecase (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter
            "~:<~^~W~^ ~3I~:_~W~1I~@{ ~_~:<~^~W~^~@{ ~_~W~}~:>~}~:>")
           stream
           list))

(defun pprint-prog (stream list &rest noise)
  (declare (ignore noise))
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    (pprint-exit-if-list-exhausted)
    (output-object (pprint-pop) stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\space stream)
    (pprint-newline :miser stream)
    (pprint-fill stream (pprint-pop))
    (pprint-tagbody-guts stream)))

;;; Each clause in this list will get its own line.
(defvar *loop-seperating-clauses*
  '(:and
    :with :for
    :initially :finally
    :do :doing
    :collect :collecting
    :append :appending
    :nconc :nconcing
    :count :counting
    :sum :summing
    :maximize :maximizing
    :minimize :minimizing
    :if :when :unless :end
    :for :while :until :repeat :always :never :thereis
    ))

(defun pprint-extended-loop (stream list)
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
    (output-object (pprint-pop) stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\space stream)
    (pprint-indent :current 0 stream)
    (output-object (pprint-pop) stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\space stream)
    (loop for thing = (pprint-pop)
          when (and (symbolp thing)
                    (member thing  *loop-seperating-clauses* :test #'string=))
          do (pprint-newline :mandatory stream)
          do (output-object thing stream)
          do (pprint-exit-if-list-exhausted)
          do (write-char #\space stream))))

(defun pprint-loop (stream list &rest noise)
  (declare (ignore noise))
  (destructuring-bind (loop-symbol . clauses) list
    (declare (ignore loop-symbol))
    (if (or (null clauses) (consp (car clauses)))
        (pprint-spread-fun-call stream list)
        (pprint-extended-loop stream list))))

(defun pprint-if (stream list &rest noise)
  (declare (ignore noise))
  ;; Indent after the ``predicate'' form, and the ``then'' form.
  (funcall (formatter "~:<~^~W~^ ~:I~W~^ ~:@_~@{~W~^ ~:@_~}~:>")
           stream
           list))

(defun pprint-fun-call (stream list &rest noise)
  (declare (ignore noise))
  (funcall (formatter "~:<~^~W~^ ~:_~:I~@{~W~^ ~:_~}~:>")
           stream
           list))

(defun pprint-spread-fun-call (stream list &rest noise)
  (declare (ignore noise))
  ;; Similiar to PPRINT-FUN-CALL but emit a mandatory newline after
  ;; each parameter. I.e. spread out each parameter on its own line.
  (funcall (formatter "~:<~^~W~^ ~:_~:I~@{~W~^ ~:@_~}~:>")
           stream
           list))

(defun pprint-data-list (stream list &rest noise)
  (declare (ignore noise))
  (pprint-fill stream list))

;;; Returns an Emacs-style indent spec: an integer N, meaning indent
;;; the first N arguments specially then indent any further arguments
;;; like a body.
(defun macro-indentation (name)
  (labels ((proper-list-p (list)
             (not (nth-value 1 (ignore-errors (list-length list)))))
           (macro-arglist (name)
             (%simple-fun-arglist (macro-function name)))
           (clean-arglist (arglist)
             "Remove &whole, &enviroment, and &aux elements from ARGLIST."
             (cond ((null arglist) '())
                   ((member (car arglist) '(&whole &environment))
                    (clean-arglist (cddr arglist)))
                   ((eq (car arglist) '&aux)
                    '())
                   (t (cons (car arglist) (clean-arglist (cdr arglist)))))))
    (let ((arglist (macro-arglist name)))
      (if (proper-list-p arglist)       ; guard against dotted arglists
          (position '&body (remove '&optional (clean-arglist arglist)))
          nil))))

;;; Pretty-Print macros by looking where &BODY appears in a macro's
;;; lambda-list.
(defun pprint-macro-call (stream list &rest noise)
  (declare (ignore noise))
  (let ((indentation (and (car list) (macro-indentation (car list)))))
    (unless indentation
      (return-from pprint-macro-call
        (pprint-fun-call stream list)))
    (pprint-logical-block (stream list :prefix "(" :suffix ")")
      (output-object (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (write-char #\space stream)
      (loop for indent from 0 below indentation do
            (cond
              ;; Place the very first argument next to the macro name
              ((zerop indent)
               (output-object (pprint-pop) stream)
               (pprint-exit-if-list-exhausted))
              ;; Indent any other non-body argument by the same
              ;; amount. It's what Emacs seems to do, too.
              (t
               (pprint-indent :block 3 stream)
               (pprint-newline :mandatory stream)
               (output-object (pprint-pop) stream)
               (pprint-exit-if-list-exhausted))))
      ;; Indent back for the body.
      (pprint-indent :block 1 stream)
      (pprint-newline :mandatory stream)
      (loop
       (output-object (pprint-pop) stream)
       (pprint-exit-if-list-exhausted)
       (pprint-newline :mandatory stream)))))

;;;; the interface seen by regular (ugly) printer and initialization routines

;;; OUTPUT-PRETTY-OBJECT is called by OUTPUT-OBJECT when
;;; *PRINT-PRETTY* is true.
(defun output-pretty-object (object stream)
  (multiple-value-bind (fun pretty) (pprint-dispatch object)
    (if pretty
        (with-pretty-stream (stream)
          (funcall fun stream object))
        ;; No point in consing up a pretty stream if we are not using pretty
        ;; printing the object after all.
        (output-ugly-object object stream))))

(defun mboundp (name)
  (and (fboundp name) (macro-function name) t))

(defun !pprint-cold-init ()
  (/show0 "entering !PPRINT-COLD-INIT")
  ;; Kludge: We set *STANDARD-PP-D-TABLE* to a new table even though
  ;; it's going to be set to a copy of *INITIAL-PP-D-T* below because
  ;; it's used in WITH-STANDARD-IO-SYNTAX, and condition reportery
  ;; possibly performed in the following extent may use W-S-IO-SYNTAX.
  (setf *standard-pprint-dispatch-table* (make-pprint-dispatch-table))
  (setf *initial-pprint-dispatch-table*  (make-pprint-dispatch-table))
  (let ((*print-pprint-dispatch* *initial-pprint-dispatch-table*)
        (*building-initial-table* t))
    (/show0 "doing SET-PPRINT-DISPATCH for regular types")
    (set-pprint-dispatch 'array #'pprint-array)
    (set-pprint-dispatch '(cons (and symbol (satisfies mboundp)))
                         #'pprint-macro-call -1)
    (set-pprint-dispatch '(cons (and symbol (satisfies fboundp)))
                         #'pprint-fun-call -1)
    (set-pprint-dispatch '(cons symbol)
                         #'pprint-data-list -2)
    (set-pprint-dispatch 'cons #'pprint-fill -2)
    ;; cons cells with interesting things for the car
    (/show0 "doing SET-PPRINT-DISPATCH for CONS with interesting CAR")

    (dolist (magic-form '((lambda pprint-lambda)
                          (declare pprint-declare)

                          ;; special forms
                          (block pprint-block)
                          (catch pprint-block)
                          (eval-when pprint-block)
                          (flet pprint-flet)
                          (function pprint-quote)
                          (if pprint-if)
                          (labels pprint-flet)
                          (let pprint-let)
                          (let* pprint-let)
                          (locally pprint-progn)
                          (macrolet pprint-flet)
                          (multiple-value-call pprint-block)
                          (multiple-value-prog1 pprint-block)
                          (progn pprint-progn)
                          (progv pprint-progv)
                          (quote pprint-quote)
                          (return-from pprint-block)
                          (setq pprint-setq)
                          (symbol-macrolet pprint-let)
                          (tagbody pprint-tagbody)
                          (throw pprint-block)
                          (unwind-protect pprint-block)

                          ;; macros
                          (case pprint-case)
                          (ccase pprint-case)
                          (ctypecase pprint-typecase)
                          (declaim pprint-declare)
                          (defconstant pprint-block)
                          (define-modify-macro pprint-defun)
                          (define-setf-expander pprint-defun)
                          (defmacro pprint-defun)
                          (defmethod pprint-defmethod)
                          (defpackage pprint-defpackage)
                          (defparameter pprint-block)
                          (defsetf pprint-defun)
                          (defstruct pprint-block)
                          (deftype pprint-defun)
                          (defun pprint-defun)
                          (defvar pprint-block)
                          (destructuring-bind pprint-destructuring-bind)
                          (do pprint-do)
                          (do* pprint-do)
                          (do-all-symbols pprint-dolist)
                          (do-external-symbols pprint-dolist)
                          (do-symbols pprint-dolist)
                          (dolist pprint-dolist)
                          (dotimes pprint-dolist)
                          (ecase pprint-case)
                          (etypecase pprint-typecase)
                          #+nil (handler-bind ...)
                          #+nil (handler-case ...)
                          (loop pprint-loop)
                          (multiple-value-bind pprint-prog2)
                          (multiple-value-setq pprint-block)
                          (pprint-logical-block pprint-block)
                          (print-unreadable-object pprint-block)
                          (prog pprint-prog)
                          (prog* pprint-prog)
                          (prog1 pprint-block)
                          (prog2 pprint-prog2)
                          (psetf pprint-setq)
                          (psetq pprint-setq)
                          #+nil (restart-bind ...)
                          #+nil (restart-case ...)
                          (setf pprint-setq)
                          (step pprint-progn)
                          (time pprint-progn)
                          (typecase pprint-typecase)
                          (unless pprint-block)
                          (when pprint-block)
                          (with-compilation-unit pprint-block)
                          #+nil (with-condition-restarts ...)
                          (with-hash-table-iterator pprint-block)
                          (with-input-from-string pprint-block)
                          (with-open-file pprint-block)
                          (with-open-stream pprint-block)
                          (with-output-to-string pprint-block)
                          (with-package-iterator pprint-block)
                          (with-simple-restart pprint-block)
                          (with-standard-io-syntax pprint-progn)

                          ;; sbcl specific
                          (sb!int:dx-flet pprint-flet)
                          ))

      (set-pprint-dispatch `(cons (eql ,(first magic-form)))
                           (symbol-function (second magic-form))))

    ;; other pretty-print init forms
    (/show0 "about to call !BACKQ-PP-COLD-INIT")
    (sb!impl::!backq-pp-cold-init)
    (/show0 "leaving !PPRINT-COLD-INIT"))

  (setf *standard-pprint-dispatch-table*
        (copy-pprint-dispatch *initial-pprint-dispatch-table*))
  (setf *print-pprint-dispatch* *initial-pprint-dispatch-table*)
  (setf *print-pretty* t))
