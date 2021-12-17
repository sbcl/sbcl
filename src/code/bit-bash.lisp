;;;; functions to implement bitblt-ish operations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;;; support routines

(declaim (inline start-mask end-mask))

;;; Produce a mask that contains 1's for the COUNT "start" bits and
;;; 0's for the remaining "end" bits. Only the lower 5 bits of COUNT
;;; are significant (KLUDGE: because of hardwired implicit dependence
;;; on 32-bit word size -- WHN 2001-03-19).
(defun start-mask (count)
  (declare (fixnum count))
  (shift-towards-start most-positive-word (- count)))

;;; Produce a mask that contains 1's for the COUNT "end" bits and 0's
;;; for the remaining "start" bits. Only the lower 5 bits of COUNT are
;;; significant (KLUDGE: because of hardwired implicit dependence on
;;; 32-bit word size -- WHN 2001-03-19).
(defun end-mask (count)
  (declare (fixnum count))
  (shift-towards-end most-positive-word (- count)))


;;; the actual bashers and common uses of same

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant min-bytes-c-call-threshold
    ;; mostly just guessing here
    #+(or x86 x86-64 ppc ppc64) 128
    #-(or x86 x86-64 ppc ppc64) 256))

(defmacro verify-src/dst-bits-per-elt (source destination expect-bits-per-element)
  (declare (ignorable source destination expect-bits-per-element))
  #+(and sb-devel (not sb-devel-no-errors))
  `(let ((src-bits-per-element
          (ash 1 (aref %%simple-array-n-bits-shifts%%
                       (%other-pointer-widetag ,source))))
         (dst-bits-per-element
          (ash 1 (aref %%simple-array-n-bits-shifts%%
                       (%other-pointer-widetag ,destination)))))
    (when (or (/= src-bits-per-element ,expect-bits-per-element)
              (/= dst-bits-per-element ,expect-bits-per-element))
      ;; Why enforce this: because since the arrays are lisp objects
      ;; maybe we can be clever "somehow" (I'm not sure how)
      ;; and/or maybe we have to unpoison the memory for #+ubsan.
      ;; Whereas BYTE-BLT takes SAPs (and/or arrays) and so it has to
      ;; be more strictly like memmove(). Because it is exactly that.
      (error "Misuse of bash-copy: bits-per-elt=~D but src=~d and dst=~d"
             ,expect-bits-per-element src-bits-per-element dst-bits-per-element))))

;;; 1, 2, 4, and 8 bytes per element can be handled with memmove()
;;; or, if it's easy enough, a loop over VECTOR-RAW-BITS.
(defmacro define-byte-blt-copier
    (bytes-per-element
     &aux (bits-per-element (* bytes-per-element 8))
          (vtype `(simple-array (unsigned-byte ,bits-per-element) (*)))
          (elements-per-word (/ n-word-bytes bytes-per-element))
          (always-call-out-p ; memmove() is _always_ asymptotically faster than this
           ;; code, which can't make any use of vectorization that C libraries
           ;; typically do. It's a question of the overhead of a C call.
           `(>= nelements ,(/ min-bytes-c-call-threshold bytes-per-element))))
  (flet ((backward-p ()
           ;; Iterate backwards if there is overlap and byte transfer is toward higher
           ;; addresses. Technically (> dst-start src-start) is a necessary
           ;; but not sufficient condition for overlap, but it's fine.
           '(and (eq src dst) (> dst-start src-start)))
         (down ()
           ;; We could reduce the number of loop variables by 1 by computing
           ;; the distance between src-start and dst-start, and adding it in
           ;; to each array reference. Probably it would be worse though.
           '(do ((dst-index (the (or (eql -1) index) (+ dst-start nwords -1))
                            (1- dst-index))
                 (src-index (the (or (eql -1) index) (+ src-start nwords -1))
                            (1- src-index)))
                ((< dst-index dst-start))
              (declare (type (or (eql -1) index) dst-index src-index))
              ;; Assigning into SRC is right, because DST and SRC are the same array.
              ;; We don't need "both" arrays to be in registers.
              (%set-vector-raw-bits src dst-index
               (%vector-raw-bits src (the index src-index)))))
         (up ()
           '(do ((dst-index dst-start (the index (1+ dst-index)))
                 (src-index src-start (the index (1+ src-index))))
                ((>= dst-index dst-end))
              (%set-vector-raw-bits dst dst-index (%vector-raw-bits src src-index))))
         (use-memmove ()
           ;; %BYTE-BLT wants the end as an index, which it converts back to a count
           ;; by subtracting the start. Regardless, the args are way too confusing,
           ;; so let's go directly to memmove. Cribbed from (DEFTRANSFORM %BYTE-BLT)
           `(with-pinned-objects (dst src)
              (memmove (sap+ (vector-sap (the ,vtype dst))
                             (the signed-word (* dst-start ,bytes-per-element)))
                       (sap+ (vector-sap (the ,vtype src))
                             (the signed-word (* src-start ,bytes-per-element)))
                      (the word (* nelements ,bytes-per-element))))))
    ;; The arguments are array element indices.
    `(defun ,(intern (format nil "UB~D-BASH-COPY" bits-per-element)
                     (find-package "SB-KERNEL"))
         (src src-start dst dst-start nelements)
      (declare (type index src-start dst-start nelements))
      (verify-src/dst-bits-per-elt src dst ,bits-per-element)
      (locally
         (declare (optimize (safety 0)
                            (sb-c::alien-funcall-saves-fp-and-pc 0)))
      #+cheneygc (when (> nelements 0)
                   ;; cheneygc can't handle a WP fault in memcpy()
                   ;; because "if(!foreign_function_call_active ..."
                   (let ((last (truly-the index (+ dst-start (1- nelements)))))
                     (data-vector-set (truly-the ,vtype dst) last
                                      (data-vector-ref (truly-the ,vtype dst) last))))
       ,(if (= bytes-per-element sb-vm:n-word-bytes)
          `(if ,always-call-out-p
               ,(use-memmove)
               (let ((nwords nelements))
                 (if ,(backward-p)
                     ,(down)
                     (let ((dst-end (the index (+ dst-start nelements))))
                       ,(up)))))
          `(let ((dst-subword (mod dst-start ,elements-per-word))
                 (src-subword (mod src-start ,elements-per-word))
                 (dst (truly-the ,vtype dst))
                 (src (truly-the ,vtype src)))
             (cond ((or ,always-call-out-p
                        (/= dst-subword src-subword)) ; too complicated
                    ,(use-memmove))
                   (,(backward-p)
                    ;; Using the primitive-type-specific data-vector-set,
                    ;; process at most (1- ELEMENTS-PER-WORD) elements
                    ;; until aligned to a word.
                    (let ((dst-end (+ dst-start nelements))
                          (src-end (+ src-start nelements))
                          (original-nelements nelements))
                      ,@(let (initial)
                          (loop for i downfrom (- elements-per-word 1)
                                repeat (1- elements-per-word)
                                do (setq initial
                                         ;; Test NELEMENTS first because it should be in a register
                                         ;; from the preceding DECF.
                                         `((when (and (/= nelements 0)
                                                      (logtest dst-end ,(1- elements-per-word)))
                                             (data-vector-set dst (1- dst-end)
                                                              (data-vector-ref src (- src-end ,i)))
                                             (decf (the index dst-end))
                                             (decf (the index nelements))
                                             ,@initial))))
                          initial)
                      (decf src-end (the (mod 8) (- original-nelements nelements)))
                      ;; Now DST-END and SRC-END are element indices that start a word.
                      ;; Scan backwards by whole words.
                      (let ((nwords (truncate nelements ,elements-per-word)))
                        (when (plusp nwords)
                          ;; Convert to word indices
                          (let* ((dst-start (- (truncate dst-end ,elements-per-word) nwords))
                                 (src-start (- (truncate src-end ,elements-per-word) nwords)))
                            ,(down))
                          (decf (the index dst-end) (* nwords ,elements-per-word))
                          (decf (the index src-end) (* nwords ,elements-per-word))
                          (decf nelements (* nwords ,elements-per-word))))
                      ;; If there are elements remaining after the last full word copied,
                      ;; process element by element.
                      ,@(let (final)
                          (loop for i from (1- elements-per-word) downto 1
                                do (setq final
                                         `((unless (= nelements 0)
                                             (data-vector-set
                                              dst (- dst-end ,i)
                                              (data-vector-ref src (- src-end ,i)))
                                             ,@(unless (= i (1- elements-per-word))
                                                 '((decf (the index nelements))))
                                             ,@final))))
                        final)))
                   (t
                    ;; Same as above
                    (let ((original-nelements nelements))
                      ,@(let (initial)
                          (loop for i downfrom (- elements-per-word 2)
                                repeat (1- elements-per-word)
                                do (setq initial
                                         `((when (and (/= nelements 0)
                                                      (logtest dst-start ,(1- elements-per-word)))
                                             (data-vector-set
                                              dst dst-start
                                              (data-vector-ref src (+ src-start ,i)))
                                             (incf (the index dst-start))
                                             (decf (the index nelements))
                                             ,@initial))))
                          initial)
                      (incf (the index src-start) (- original-nelements nelements)))
                    (let ((nwords (truncate nelements ,elements-per-word)))
                      (when (plusp nwords)
                        (let* ((src-start (truncate src-start ,elements-per-word))
                               (dst-start (truncate dst-start ,elements-per-word))
                               (dst-end (the index (+ dst-start nwords))))
                          ,(up))
                        (incf dst-start (* nwords ,elements-per-word))
                        (incf src-start (* nwords ,elements-per-word))
                        (decf nelements (* nwords ,elements-per-word))))
                    ;; Same as above
                    ,@(let (final)
                        (loop for i from (- elements-per-word 2) downto 0
                              do (setq final
                                       `((unless (= nelements 0)
                                           (data-vector-set
                                            dst (+ dst-start ,i)
                                            (data-vector-ref src (+ src-start ,i)))
                                           ,@(unless (= i (- elements-per-word 2))
                                               '((decf (the index nelements))))
                                           ,@final))))
                        final)))))
       (values)))))

(define-byte-blt-copier 1)
(define-byte-blt-copier 2)
(define-byte-blt-copier 4)
#+64-bit (define-byte-blt-copier 8)

;;; We cheat a little bit by using TRULY-THE in the copying function to
;;; force the compiler to generate good code in the (= BITSIZE
;;; N-WORD-BITS) case.  We don't use TRULY-THE in the other cases
;;; to give the compiler freedom to generate better code.
(defmacro !define-byte-bashers (bitsize)
  (let* ((bytes-per-word (/ n-word-bits bitsize))
         (byte-offset `(integer 0 (,bytes-per-word)))
         (word-offset `(integer 0 ,(ceiling array-dimension-limit bytes-per-word)))
         (constant-bash-name (intern (format nil "CONSTANT-UB~D-BASH" bitsize) (find-package "SB-KERNEL")))
         (array-fill-name (intern (format nil "UB~D-BASH-FILL" bitsize) (find-package "SB-KERNEL")))
         (unary-bash-name (intern (format nil "UNARY-UB~D-BASH" bitsize) (find-package "SB-KERNEL")))
         (array-copy-name (intern (format nil "UB~D-BASH-COPY" bitsize) (find-package "SB-KERNEL"))))
    `(progn
      (declaim (inline ,constant-bash-name))
      ;; Fill DST with VALUE starting at DST-OFFSET and continuing
      ;; for LENGTH bytes (however bytes are defined).
      (defun ,constant-bash-name (dst dst-offset length value)
        (declare (type word value) (type index dst-offset length))
        (multiple-value-bind (dst-word-offset dst-byte-offset)
            (floor dst-offset ,bytes-per-word)
          (declare (type ,word-offset dst-word-offset)
                   (type ,byte-offset dst-byte-offset))
          (multiple-value-bind (n-words final-bytes)
              (floor (+ dst-byte-offset length) ,bytes-per-word)
            (declare (type ,word-offset n-words)
                     (type ,byte-offset final-bytes))
            (if (zerop n-words)
                ,(unless (= bytes-per-word 1)
                  `(unless (zerop length)
                     (%set-vector-raw-bits dst dst-word-offset
                              (if (>= length ,bytes-per-word)
                                  value
                                  (let ((mask (shift-towards-end
                                               (start-mask (* length ,bitsize))
                                               (* dst-byte-offset ,bitsize))))
                                    (word-logical-or (word-logical-and value mask)
                                                     (word-logical-andc2 (%vector-raw-bits dst dst-word-offset)
                                                                         mask)))))))
                (let ((interior (floor (- length final-bytes) ,bytes-per-word)))
                  ,@(unless (= bytes-per-word 1)
                     `((unless (zerop dst-byte-offset)
                         (let ((mask (end-mask (* (- dst-byte-offset) ,bitsize))))
                           (%set-vector-raw-bits dst dst-word-offset
                                    (word-logical-or (word-logical-and value mask)
                                                     (word-logical-andc2 (%vector-raw-bits dst dst-word-offset)
                                                                         mask))))
                         (incf dst-word-offset))))
                  (let ((end (+ dst-word-offset interior)))
                    (declare (type ,word-offset end))
                    (do ()
                        ((>= dst-word-offset end))
                      (%set-vector-raw-bits dst dst-word-offset value)
                      (incf dst-word-offset)))
                  #+nil
                  (dotimes (i interior)
                    (%set-vector-raw-bits dst dst-word-offset value)
                    (incf dst-word-offset))
                  ,@(unless (= bytes-per-word 1)
                     `((unless (zerop final-bytes)
                         (let ((mask (start-mask (* final-bytes ,bitsize))))
                           (%set-vector-raw-bits dst dst-word-offset
                                    (word-logical-or (word-logical-and value mask)
                                                     (word-logical-andc2 (%vector-raw-bits dst dst-word-offset)
                                                                         mask)))))))))))
        (values))

      ;; common uses for constant-byte-bashing
      (defknown ,array-fill-name (word simple-unboxed-array index index)
          simple-unboxed-array
          ()
        :result-arg 1
        :derive-type (sb-c::result-type-nth-arg 1))
      (defun ,array-fill-name (value dst dst-offset length)
        (declare (type word value) (type index dst-offset length))
        (declare (optimize (speed 3) (safety 1)))
        (,constant-bash-name dst dst-offset length value)
        dst)

      ;; Copying. Never use this for 8, 16, 32, 64
      ,@(when (member bitsize '(1 2 4))
       `((declaim (inline ,unary-bash-name))
         (defun ,unary-bash-name (src src-offset dst dst-offset length)
           (declare (type index src-offset dst-offset length))
           (verify-src/dst-bits-per-elt src dst ,bitsize)
           (multiple-value-bind (dst-word-offset dst-byte-offset)
               (floor dst-offset ,bytes-per-word)
             (declare (type ,word-offset dst-word-offset)
                      (type ,byte-offset dst-byte-offset))
             (multiple-value-bind (src-word-offset src-byte-offset)
                 (floor src-offset ,bytes-per-word)
               (declare (type ,word-offset src-word-offset)
                        (type ,byte-offset src-byte-offset))
               (cond
                 ((<= (+ dst-byte-offset length) ,bytes-per-word)
                  ;; We are only writing one word, so it doesn't matter what
                  ;; order we do it in.  But we might be reading from
                  ;; multiple words, so take care.
                  (cond
                    ((zerop length)
                     ;; We're not writing anything.  This is really easy.
                     )
                    ((>= length ,bytes-per-word)
                     ;; DST-BYTE-OFFSET must be equal to zero, or we would be
                     ;; writing multiple words.  If SRC-BYTE-OFFSET is also zero,
                     ;; the we just transfer the single word.  Otherwise we have
                     ;; to extract bytes from two source words.
                     (%set-vector-raw-bits dst dst-word-offset
                             (cond
                               ((zerop src-byte-offset)
                                (%vector-raw-bits src src-word-offset))
                               ,@(unless (= bytes-per-word 1)
                                  `((t (word-logical-or (shift-towards-start
                                                         (%vector-raw-bits src src-word-offset)
                                                         (* src-byte-offset ,bitsize))
                                        (shift-towards-end
                                          (%vector-raw-bits src (1+ src-word-offset))
                                          (* (- src-byte-offset) ,bitsize)))))))))
                    ,@(unless (= bytes-per-word 1)
                       `((t
                          ;; We are only writing some portion of the destination word.
                          ;; We still don't know whether we need one or two source words.
                          (let ((mask (shift-towards-end (start-mask (* length ,bitsize))
                                                         (* dst-byte-offset ,bitsize)))
                                (orig (%vector-raw-bits dst dst-word-offset))
                                (value (if (> src-byte-offset dst-byte-offset)
                                           ;; The source starts further
                                           ;; into the word than does the
                                           ;; destination, so the source
                                           ;; could extend into the next
                                           ;; word.  If it does, we have
                                           ;; to merge the two words, and
                                           ;; it not, we can just shift
                                           ;; the first word.
                                           (let ((src-byte-shift (- src-byte-offset
                                                                    dst-byte-offset)))
                                             (if (> (+ src-byte-offset length) ,bytes-per-word)
                                                 (word-logical-or
                                                  (shift-towards-start
                                                   (%vector-raw-bits src src-word-offset)
                                                   (* src-byte-shift ,bitsize))
                                                  (shift-towards-end
                                                   (%vector-raw-bits src (1+ src-word-offset))
                                                   (* (- src-byte-shift) ,bitsize)))
                                                 (shift-towards-start (%vector-raw-bits src src-word-offset)
                                                                      (* src-byte-shift ,bitsize))))
                                           ;; The destination starts further
                                           ;; into the word than does the
                                           ;; source, so we know the source
                                           ;; cannot extend into a second
                                           ;; word (or else the destination
                                           ;; would too, and we wouldn't be
                                           ;; in this branch).
                                           (shift-towards-end
                                            (%vector-raw-bits src src-word-offset)
                                            (* (- dst-byte-offset src-byte-offset) ,bitsize)))))
                            (declare (type word mask orig value))
                            (%set-vector-raw-bits dst dst-word-offset
                                     (word-logical-or (word-logical-and value mask)
                                                      (word-logical-andc2 orig mask)))))))))
                 ((= src-byte-offset dst-byte-offset)
                  ;; The source and destination are aligned, so shifting
                  ;; is unnecessary.  But we have to pick the direction
                  ;; of the copy in case the source and destination are
                  ;; really the same object.
                  (multiple-value-bind (words final-bytes)
                      (floor (+ dst-byte-offset length) ,bytes-per-word)
                    (declare (type ,word-offset words)
                             (type ,byte-offset final-bytes))
                    (let ((interior (floor (- length final-bytes) ,bytes-per-word)))
                      (declare (type ,word-offset interior))
                      (cond
                        ((<= dst-offset src-offset)
                         ;; We need to loop from left to right.
                         ,@(unless (= bytes-per-word 1)
                            `((unless (zerop dst-byte-offset)
                                ;; We are only writing part of the first word, so mask
                                ;; off the bytes we want to preserve.
                                (let ((mask (end-mask (* (- dst-byte-offset) ,bitsize)))
                                      (orig (%vector-raw-bits dst dst-word-offset))
                                      (value (%vector-raw-bits src src-word-offset)))
                                  (declare (type word mask orig value))
                                  (%set-vector-raw-bits dst dst-word-offset
                                           (word-logical-or (word-logical-and value mask)
                                                            (word-logical-andc2 orig mask))))
                                (incf src-word-offset)
                                (incf dst-word-offset))))
                         ;; Copy the interior words.
                         (let ((end ,(if (= bytes-per-word 1)
                                         `(truly-the ,word-offset
                                           (+ dst-word-offset interior))
                                         `(+ dst-word-offset interior))))
                           (declare (type ,word-offset end))
                           (do ()
                               ((>= dst-word-offset end))
                             (%set-vector-raw-bits dst dst-word-offset
                                      (%vector-raw-bits src src-word-offset))
                             ,(if (= bytes-per-word 1)
                                  `(setf src-word-offset (truly-the ,word-offset (+ src-word-offset 1)))
                                  `(incf src-word-offset))
                             (incf dst-word-offset)))
                         ,@(unless (= bytes-per-word 1)
                            `((unless (zerop final-bytes)
                                ;; We are only writing part of the last word.
                                (let ((mask (start-mask (* final-bytes ,bitsize)))
                                      (orig (%vector-raw-bits dst dst-word-offset))
                                      (value (%vector-raw-bits src src-word-offset)))
                                  (declare (type word mask orig value))
                                  (%set-vector-raw-bits dst dst-word-offset
                                           (word-logical-or (word-logical-and value mask)
                                                            (word-logical-andc2 orig mask))))))))
                        (t
                         ;; We need to loop from right to left.
                         ,(if (= bytes-per-word 1)
                              `(setf dst-word-offset (truly-the ,word-offset
                                                      (+ dst-word-offset words)))
                              `(incf dst-word-offset words))
                         ,(if (= bytes-per-word 1)
                              `(setf src-word-offset (truly-the ,word-offset
                                                      (+ src-word-offset words)))
                              `(incf src-word-offset words))
                         ,@(unless (= bytes-per-word 1)
                            `((unless (zerop final-bytes)
                                (let ((mask (start-mask (* final-bytes ,bitsize)))
                                      (orig (%vector-raw-bits dst dst-word-offset))
                                      (value (%vector-raw-bits src src-word-offset)))
                                  (declare (type word mask orig value))
                                  (%set-vector-raw-bits dst dst-word-offset
                                           (word-logical-or (word-logical-and value mask)
                                                            (word-logical-andc2 orig mask)))))))
                         (let ((end (- dst-word-offset interior)))
                           (do ()
                               ((<= dst-word-offset end))
                             (decf src-word-offset)
                             (decf dst-word-offset)
                             (%set-vector-raw-bits dst dst-word-offset
                                      (%vector-raw-bits src src-word-offset))))
                         ,@(unless (= bytes-per-word 1)
                            `((unless (zerop dst-byte-offset)
                                ;; We are only writing part of the last word.
                                (decf src-word-offset)
                                (decf dst-word-offset)
                                (let ((mask (end-mask (* (- dst-byte-offset) ,bitsize)))
                                      (orig (%vector-raw-bits dst dst-word-offset))
                                      (value (%vector-raw-bits src src-word-offset)))
                                  (declare (type word mask orig value))
                                  (%set-vector-raw-bits dst dst-word-offset
                                           (word-logical-or (word-logical-and value mask)
                                                            (word-logical-andc2 orig mask))))))))))))
                 (t
                  ;; Source and destination are not aligned.
                  (multiple-value-bind (words final-bytes)
                      (floor (+ dst-byte-offset length) ,bytes-per-word)
                    (declare (type ,word-offset words)
                             (type ,byte-offset final-bytes))
                    (let ((src-shift (mod (- src-byte-offset dst-byte-offset)
                                          ,bytes-per-word))
                          (interior (floor (- length final-bytes) ,bytes-per-word)))
                      (declare (type ,word-offset interior)
                               (type ,byte-offset src-shift))
                      (cond
                        ((<= dst-offset src-offset)
                         ;; We need to loop from left to right.
                         (let ((prev 0)
                               (next (%vector-raw-bits src src-word-offset)))
                           (declare (type word prev next))
                           (flet ((get-next-src ()
                                    (setf prev next)
                                    (setf next (%vector-raw-bits src
                                                        (incf src-word-offset)))))
                             (declare (inline get-next-src))
                             ,@(unless (= bytes-per-word 1)
                                `((unless (zerop dst-byte-offset)
                                    (when (> src-byte-offset dst-byte-offset)
                                      (get-next-src))
                                    (let ((mask (end-mask (* (- dst-byte-offset) ,bitsize)))
                                          (orig (%vector-raw-bits dst dst-word-offset))
                                          (value (word-logical-or (shift-towards-start prev (* src-shift ,bitsize))
                                                                  (shift-towards-end next (* (- src-shift) ,bitsize)))))
                                      (declare (type word mask orig value))
                                      (%set-vector-raw-bits dst dst-word-offset
                                               (word-logical-or (word-logical-and value mask)
                                                                (word-logical-andc2 orig mask))))
                                    (incf dst-word-offset))))
                             (let ((end (+ dst-word-offset interior)))
                               (declare (type ,word-offset end))
                               (do ()
                                   ((>= dst-word-offset end))
                                 (get-next-src)
                                 (let ((value (word-logical-or
                                               (shift-towards-end next (* (- src-shift) ,bitsize))
                                               (shift-towards-start prev (* src-shift ,bitsize)))))
                                   (declare (type word value))
                                   (%set-vector-raw-bits dst dst-word-offset value)
                                   (incf dst-word-offset))))
                             ,@(unless (= bytes-per-word 1)
                                `((unless (zerop final-bytes)
                                    (let ((value
                                           (if (> (+ final-bytes src-shift) ,bytes-per-word)
                                               (progn
                                                 (get-next-src)
                                                 (word-logical-or
                                                  (shift-towards-end next (* (- src-shift) ,bitsize))
                                                  (shift-towards-start prev (* src-shift ,bitsize))))
                                               (shift-towards-start next (* src-shift ,bitsize))))
                                          (mask (start-mask (* final-bytes ,bitsize)))
                                          (orig (%vector-raw-bits dst dst-word-offset)))
                                      (declare (type word mask orig value))
                                      (%set-vector-raw-bits dst dst-word-offset
                                               (word-logical-or (word-logical-and value mask)
                                                                (word-logical-andc2 orig mask))))))))))
                        (t
                         ;; We need to loop from right to left.
                         (incf dst-word-offset words)
                         (incf src-word-offset (1- (ceiling (+ src-byte-offset length) ,bytes-per-word)))
                         (let ((next 0)
                               (prev (%vector-raw-bits src src-word-offset)))
                           (declare (type word prev next))
                           (flet ((get-next-src ()
                                    (setf next prev)
                                    (setf prev (%vector-raw-bits src (decf src-word-offset)))))
                             (declare (inline get-next-src))
                             ,@(unless (= bytes-per-word 1)
                                `((unless (zerop final-bytes)
                                    (when (> final-bytes (- ,bytes-per-word src-shift))
                                      (get-next-src))
                                    (let ((value (word-logical-or
                                                  (shift-towards-end next (* (- src-shift) ,bitsize))
                                                  (shift-towards-start prev (* src-shift ,bitsize))))
                                          (mask (start-mask (* final-bytes ,bitsize)))
                                          (orig (%vector-raw-bits dst dst-word-offset)))
                                      (declare (type word mask orig value))
                                      (%set-vector-raw-bits dst dst-word-offset
                                               (word-logical-or (word-logical-and value mask)
                                                                (word-logical-andc2 orig mask)))))))
                             (decf dst-word-offset)
                             (let ((end (- dst-word-offset interior)))
                               (do ()
                                   ((<= dst-word-offset end))
                                 (get-next-src)
                                 (let ((value (word-logical-or
                                               (shift-towards-end next (* (- src-shift) ,bitsize))
                                               (shift-towards-start prev (* src-shift ,bitsize)))))
                                   (declare (type word value))
                                   (%set-vector-raw-bits dst dst-word-offset value)
                                   (decf dst-word-offset))))
                             ,@(unless (= bytes-per-word 1)
                                `((unless (zerop dst-byte-offset)
                                    (if (> src-byte-offset dst-byte-offset)
                                        (get-next-src)
                                        (setf next prev prev 0))
                                    (let ((mask (end-mask (* (- dst-byte-offset) ,bitsize)))
                                          (orig (%vector-raw-bits dst dst-word-offset))
                                          (value (word-logical-or
                                                  (shift-towards-start prev (* src-shift ,bitsize))
                                                  (shift-towards-end next (* (- src-shift) ,bitsize)))))
                                      (declare (type word mask orig value))
                                      (%set-vector-raw-bits dst dst-word-offset
                                              (word-logical-or (word-logical-and value mask)
                                                               (word-logical-andc2 orig mask)))))))))))))))))
           (values))

         ;; common uses for unary-byte-bashing
         (defun ,array-copy-name (src src-offset dst dst-offset length)
           (declare (type index src-offset dst-offset length))
           (locally (declare (optimize (speed 3) (safety 1)))
             (,unary-bash-name src src-offset dst dst-offset length))))))))

;;; We would normally do this with a MACROLET, but then we run into
;;; problems with the lexical environment being too hairy for the
;;; cross-compiler and it cannot inline the basic basher functions.
#.(loop for i = 1 then (* i 2)
        collect `(!define-byte-bashers ,i) into bashers
        until (= i n-word-bits)
        finally (return `(progn ,@bashers)))

(defmacro !define-constant-byte-bashers (bitsize type value-transformer &optional (name type))
  (let ((constant-bash-name (intern (format nil "CONSTANT-UB~D-BASH" bitsize) (find-package "SB-KERNEL")))
        (array-fill-name (intern (format nil "UB~D-BASH-FILL-WITH-~A" bitsize name) (find-package "SB-KERNEL"))))
    `(progn
       (defknown ,array-fill-name (,type simple-unboxed-array index index)
           simple-unboxed-array
           ()
         :result-arg 1
         :derive-type (sb-c::result-type-nth-arg 1))
       (defun ,array-fill-name (value dst dst-offset length)
         (declare (type ,type value) (type index dst-offset length))
         (declare (optimize (speed 3) (safety 1)))
         (,constant-bash-name dst dst-offset length (,value-transformer value))
         dst))))

(macrolet ((def ()
             `(progn
                ,@(loop for n-bits = 1 then (* n-bits 2)
                        until (= n-bits n-word-bits)
                        collect
                        `(!define-constant-byte-bashers ,n-bits
                             (unsigned-byte ,n-bits)
                             (lambda (value)
                               ,@(loop for i = n-bits then (* 2 i)
                                       until (= i sb-vm:n-word-bits)
                                       collect
                                       `(setf value (dpb value (byte ,i ,i) value))))
                             ,(format nil "UB~A" n-bits))
                        collect
                        `(!define-constant-byte-bashers ,n-bits
                             (signed-byte ,n-bits)
                             (lambda (value)
                               (let ((value (ldb (byte ,n-bits 0) value)))
                                 ,@(loop for i = n-bits then (* 2 i)
                                         until (= i sb-vm:n-word-bits)
                                         collect
                                         `(setf value (dpb value (byte ,i ,i) value)))))
                             ,(format nil "SB~A" n-bits)))
                (!define-constant-byte-bashers ,n-word-bits
                    (signed-byte ,n-word-bits)
                    (lambda (value)
                      (ldb (byte ,n-word-bits 0) value))
                    ,(format nil "SB~A" n-word-bits)))))
  (def))

(!define-constant-byte-bashers #.n-word-bits
    fixnum
    (lambda (value)
      (ldb (byte #.n-word-bits 0) (ash value n-fixnum-tag-bits))))

(!define-constant-byte-bashers 32
    single-float
    (lambda (value)
      (let ((bits (ldb (byte 32 0) (single-float-bits value))))
        #+64-bit
        (dpb bits (byte 32 32) bits)
        #-64-bit
        bits)))

#+64-bit
(!define-constant-byte-bashers 64
    double-float
    (lambda (value)
      (ldb (byte 64 0) (double-float-bits value))))

#+64-bit
(!define-constant-byte-bashers 64
    (complex single-float)
    (lambda (item)
      #+big-endian
      (logior (ash (ldb (byte 32 0)
                        (single-float-bits (realpart item))) 32)
              (ldb (byte 32 0)
                   (single-float-bits (imagpart item))))
      #+little-endian
      (logior (ash (ldb (byte 32 0)
                        (single-float-bits (imagpart item))) 32)
              (ldb (byte 32 0)
                   (single-float-bits (realpart item)))))
    complex-single-float)


;;;; Bashing-Style search for bits
;;;;
;;;; Similar search would work well for base-strings as well.
;;;; (Technically for all unboxed sequences of sub-word size elements,
;;;; but somehow I doubt eg. octet vectors get POSITION or FIND used
;;;; as much on them.)
(defconstant +bit-position-base-mask+ (1- n-word-bits))
(defconstant +bit-position-base-shift+ (integer-length +bit-position-base-mask+))
(macrolet ((compute-start-mask (index)
             `(let ((first-bits (logand ,index +bit-position-base-mask+)))
                #+little-endian (ash -1 first-bits)
                #+big-endian (lognot (ash -1 (- n-word-bits first-bits)))))
           (compute-end-mask (index)
             `(let ((last-bits (logand ,index +bit-position-base-mask+)))
                #+little-endian (lognot (ash -1 last-bits))
                #+big-endian (logand (ash -1 (- n-word-bits last-bits))
                                     most-positive-word)))
           (calc-index (bit-index)
             `(logior (the index ,bit-index)
                      (truly-the fixnum
                                 (ash word-index +bit-position-base-shift+))))
           (def (name from-end frob)
             `(defun ,name (vector start end)
                (declare (simple-bit-vector vector)
                         (index start end)
                         (optimize (speed 3) (safety 0)))
                ;; The END parameter is an exclusive limit as is customary.
                ;; It's somewhat subjective whether the algorithm below
                ;; would become simpler by subtracting 1 from END initially.
                (let* ((first-word (ash start (- +bit-position-base-shift+)))
                       (last-word (ash end (- +bit-position-base-shift+)))
                       ;; These mask out everything but the interesting parts.
                       (start-mask (compute-start-mask start))
                       (end-mask (compute-end-mask end)))
                  (declare (index last-word first-word))
                  (flet ((#+little-endian start-bit #+big-endian end-bit (x)
                          (declare (word x))
                          #+(or x86-64 x86)
                          (truly-the (mod #.n-word-bits)
                                     (%primitive unsigned-word-find-first-bit x))
                          #-(or x86-64 x86)
                          (- #+big-endian n-word-bits
                             (integer-length (logand x (- x)))
                             #+little-endian 1))
                         (#+little-endian end-bit #+big-endian start-bit (x)
                          (declare (word x))
                          (- #+big-endian n-word-bits
                             (integer-length x)
                             #+little-endian 1))
                         (get-word (offset)
                           (,@frob (%vector-raw-bits vector offset))))
                    (declare (inline start-bit end-bit get-word))

                    (unless (< first-word last-word)
                      ;; Both masks pertain to a single word. This also catches
                      ;; START = END. In that case the masks have no bits in common.
                      (return-from ,name
                        (let ((mask (logand start-mask end-mask)))
                          (unless (zerop mask)
                            (let ((word (logand mask (get-word first-word))))
                              (unless (zerop word)
                                (let ((word-index first-word)) ; for the macro to see
                                  ,(if from-end
                                       `(calc-index (end-bit word))
                                       `(calc-index (start-bit word))))))))))

                    ;; Since the start and end words differ, there is no word
                    ;; to which both masks pertain.
                    ;; We use a fairly traditional algorithm:
                    ;;  (1) scan some number (0 <= N <= n-word-bits) of bits initially,
                    ;;  (2) then a whole number of intervening words,
                    ;;  (3) then some number (0 < N < n-word-bits) of trailing bits
                    ;; Steps (1) and (3) use the START and END masks respectively.
                    ;; The START mask has between 1 and N-WORD-BITS (inclusive) consecutive
                    ;; 1s, starting from the appropriate end.
                    ;; END-MASK instead of getting all 1s in the limiting case,
                    ;; gets all 0s, and a LAST-WORD value that is 1 too high
                    ;; which is semantically correct - it is an "inclusive" limit
                    ;; of a word in which no bits should be examined.
                    ;; When that occurs, we avoid reading the final word
                    ;; to avoid a buffer overrun bug.
                    ,(if from-end

                         ;; Reverse scan:
                         `(let ((word-index last-word)) ; trailing chunk
                            (declare (index word-index))
                            (unless (zerop end-mask)
                              ;; If no bits are set, then this is off the end of the subsequence.
                              ;; Do not read the word at all.
                              (let ((word (logand end-mask (get-word word-index))))
                                (unless (zerop word)
                                  (return-from ,name (calc-index (end-bit word))))))
                            (decf word-index)
                            ;; middle chunks
                            (loop while (> word-index first-word) ; might execute 0 times
                                  do (let ((word (get-word word-index)))
                                       (unless (zerop word)
                                         (return-from ,name (calc-index (end-bit word)))))
                                     (decf word-index))
                            ;; leading chunk - always executed
                            (let ((word (logand start-mask (get-word first-word))))
                              (unless (zerop word)
                                (calc-index (end-bit word)))))

                         ;; Forward scan:
                         `(let* ((word-index first-word)
                                 (word (logand start-mask (get-word word-index))))
                            (declare (index word-index))
                            (unless (zerop word)
                              (return-from ,name (calc-index (start-bit word))))
                            (incf word-index)
                            ;; Scan full words up to but excluding LAST-WORD
                            (loop while (< word-index last-word) ; might execute 0 times
                                  do (let ((word (get-word word-index)))
                                       (unless (zerop word)
                                         (return-from ,name (calc-index (start-bit word)))))
                                     (incf word-index))
                            ;; Scan last word unless no bits in mask
                            (unless (zerop end-mask)
                              (let ((word (logand end-mask (get-word word-index))))
                                (unless (zerop word)
                                  (calc-index (start-bit word))))))))))))

  (defun run-bit-position-assertions ()
    ;; Check the claim in the comment at "(unless (< first-word last-word)"
    (loop for i from 0 to (* 2 n-word-bits)
          do (let ((start-mask (compute-start-mask i))
                   (end-mask (compute-end-mask i)))
               (assert (= (logand start-mask end-mask) 0)))))

  (def %bit-pos-fwd/1 nil (identity))
  (def %bit-pos-rev/1   t (identity))
  (def %bit-pos-fwd/0 nil (logandc2 most-positive-word))
  (def %bit-pos-rev/0   t (logandc2 most-positive-word)))

;; Known direction, unknown item to find
(defun %bit-pos-fwd (bit vector start end)
  (case bit
    (0 (%bit-pos-fwd/0 vector start end))
    (1 (%bit-pos-fwd/1 vector start end))
    (otherwise nil)))
(defun %bit-pos-rev (bit vector start end)
  (case bit
    (0 (%bit-pos-rev/0 vector start end))
    (1 (%bit-pos-rev/1 vector start end))
    (otherwise nil)))

;; Known item to find, unknown direction
(declaim (maybe-inline %bit-position/0 %bit-position/1))
(defun %bit-position/0 (vector from-end start end)
  (if from-end
      (%bit-pos-rev/0 vector start end)
      (%bit-pos-fwd/0 vector start end)))
(defun %bit-position/1 (vector from-end start end)
  (if from-end
      (%bit-pos-rev/1 vector start end)
      (%bit-pos-fwd/1 vector start end)))

(defun %bit-position (bit vector from-end start end)
  (declare (inline %bit-position/0 %bit-position/1))
  (case bit
    (0 (%bit-position/0 vector from-end start end))
    (1 (%bit-position/1 vector from-end start end))
    (otherwise nil)))
(clear-info :function :inlinep '%bit-position/0)
(clear-info :function :inlinep '%bit-position/1)

(run-bit-position-assertions)
