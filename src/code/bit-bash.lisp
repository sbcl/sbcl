;;;; functions to implement bitblt-ish operations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; types

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype bit-offset () '(integer 0 (#.sb!vm:n-word-bits))))

;;;; support routines

;;; A particular implementation must offer either VOPs to translate
;;; these, or DEFTRANSFORMs to convert them into something supported
;;; by the architecture.
(macrolet ((def (name &rest args)
             `(defun ,name ,args
                (,name ,@args))))
  (def word-logical-not x)
  (def word-logical-and x y)
  (def word-logical-or x y)
  (def word-logical-xor x y)
  (def word-logical-nor x y)
  (def word-logical-eqv x y)
  (def word-logical-nand x y)
  (def word-logical-andc1 x y)
  (def word-logical-andc2 x y)
  (def word-logical-orc1 x y)
  (def word-logical-orc2 x y))

;;; Shift NUMBER by the low-order bits of COUNTOID, adding zero bits
;;; at the "end" and removing bits from the "start". On big-endian
;;; machines this is a left-shift and on little-endian machines this
;;; is a right-shift.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shift-towards-start (number countoid)
    (declare (type sb!vm:word number) (fixnum countoid))
    (let ((count (ldb (byte (1- (integer-length sb!vm:n-word-bits)) 0) countoid)))
      (declare (type bit-offset count))
      (if (zerop count)
          number
          (ecase sb!c:*backend-byte-order*
            (:big-endian
               (ash (ldb (byte (- sb!vm:n-word-bits count) 0) number) count))
            (:little-endian
               (ash number (- count))))))))

;;; Shift NUMBER by COUNT bits, adding zero bits at the "start" and
;;; removing bits from the "end". On big-endian machines this is a
;;; right-shift and on little-endian machines this is a left-shift.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun shift-towards-end (number count)
    (declare (type sb!vm:word number) (fixnum count))
    (let ((count (ldb (byte (1- (integer-length sb!vm:n-word-bits)) 0) count)))
      (declare (type bit-offset count))
      (if (zerop count)
          number
          (ecase sb!c:*backend-byte-order*
            (:big-endian
               (ash number (- count)))
            (:little-endian
               (ash (ldb (byte (- sb!vm:n-word-bits count) 0) number) count)))))))

#!-sb-fluid (declaim (inline start-mask end-mask))

;;; Produce a mask that contains 1's for the COUNT "start" bits and
;;; 0's for the remaining "end" bits. Only the lower 5 bits of COUNT
;;; are significant (KLUDGE: because of hardwired implicit dependence
;;; on 32-bit word size -- WHN 2001-03-19).
(defun start-mask (count)
  (declare (fixnum count))
  (shift-towards-start (1- (ash 1 sb!vm:n-word-bits)) (- count)))

;;; Produce a mask that contains 1's for the COUNT "end" bits and 0's
;;; for the remaining "start" bits. Only the lower 5 bits of COUNT are
;;; significant (KLUDGE: because of hardwired implicit dependence on
;;; 32-bit word size -- WHN 2001-03-19).
(defun end-mask (count)
  (declare (fixnum count))
  (shift-towards-end (1- (ash 1 sb!vm:n-word-bits)) (- count)))

#!-sb-fluid (declaim (inline word-sap-ref %set-word-sap-ref))
(defun word-sap-ref (sap offset)
  (declare (type system-area-pointer sap)
           (type index offset)
           (values sb!vm:word)
           (optimize (speed 3) (safety 0) #-sb-xc-host (inhibit-warnings 3)))
  (sap-ref-word sap (the index (ash offset sb!vm:word-shift))))
(defun %set-word-sap-ref (sap offset value)
  (declare (type system-area-pointer sap)
           (type index offset)
           (type sb!vm:word value)
           (values sb!vm:word)
           (optimize (speed 3) (safety 0) (inhibit-warnings 3)))
  (setf (sap-ref-word sap (the index (ash offset sb!vm:word-shift)))
        value))


;;; the actual bashers and common uses of same

;;; This is a little ugly.  Fixing bug 188 would bring the ability to
;;; wrap a MACROLET or something similar around this whole thing would
;;; make things significantly less ugly.  --njf, 2005-02-23
;;;
;;; Well, it turns out that we *could* wrap a MACROLET around this,
;;; but it's quite ugly in a different way: when you write
;;;  (MACROLET ((GENERATOR () `(DEFUN F (X) ,@(INSANITY ...))) (GENERATOR)))
;;; and then view the inline expansion of F, you'll see it has actually
;;; captured the entirety of the MACROLET that surrounded it.
;;; With respect to building SBCL, this means, among other things, that
;;; it'd hang onto all the "!" symbols that would otherwise disappear,
;;; as well as kilobytes of completely useless s-expressions.
(eval-when (:compile-toplevel :load-toplevel :execute)

;;; Align the SAP to a word boundary, and update the offset accordingly.
(defmacro !define-sap-fixer (bitsize)
  (let ((name (intern (format nil "FIX-SAP-AND-OFFSET-UB~D" bitsize))))
    `(progn
      (declaim (inline ,name))
      (defun ,name (sap offset)
        (declare (type system-area-pointer sap)
                 (type index offset)
                 (values system-area-pointer index))
        (let ((address (sap-int sap))
              (word-mask (1- (ash 1 word-shift))))
          (values (int-sap #!-alpha (word-logical-andc2 address word-mask)
                           ;; KLUDGE: WORD-LOGICAL-ANDC2 is defined in
                           ;; terms of n-word-bits.  On all systems
                           ;; where n-word-bits is not equal to
                           ;; n-machine-word-bits we have to do this
                           ;; another way.  At this time, these
                           ;; systems are alphas, though there was
                           ;; some talk about an x86-64 build option.
                           #!+alpha (ash (ash address (- word-shift)) word-shift))
                  (+ ,(ecase bitsize
                       ((1 2 4) `(* (logand address word-mask)
                                    (/ n-byte-bits ,bitsize)))
                       ((8 16 32 64) '(logand address word-mask)))
                     offset)))))))

;;; We cheat a little bit by using TRULY-THE in the copying function to
;;; force the compiler to generate good code in the (= BITSIZE
;;; SB!VM:N-WORD-BITS) case.  We don't use TRULY-THE in the other cases
;;; to give the compiler freedom to generate better code.
(defmacro !define-byte-bashers (bitsize)
  (let* ((bytes-per-word (/ n-word-bits bitsize))
         (byte-offset `(integer 0 (,bytes-per-word)))
         (byte-count `(integer 1 (,bytes-per-word)))
         (max-bytes (ash sb!xc:most-positive-fixnum
                         ;; FIXME: this reflects code contained in the
                         ;; original bit-bash.lisp, but seems very
                         ;; nonsensical.  Why shouldn't we be able to
                         ;; handle M-P-FIXNUM bits?  And if we can't,
                         ;; are these other shift amounts bogus, too?
                         (ecase bitsize
                           (1 -2)
                           (2 -1)
                           (4  0)
                           (8  0)
                           (16 0)
                           (32 0)
                           (64 0))))
         (offset `(integer 0 ,max-bytes))
         (max-word-offset (ceiling max-bytes bytes-per-word))
         (word-offset `(integer 0 ,max-word-offset))
         (fix-sap-and-offset-name (intern (format nil "FIX-SAP-AND-OFFSET-UB~D" bitsize)))
         (constant-bash-name (intern (format nil "CONSTANT-UB~D-BASH" bitsize) (find-package "SB!KERNEL")))
         (array-fill-name (intern (format nil "UB~D-BASH-FILL" bitsize) (find-package "SB!KERNEL")))
         (system-area-fill-name (intern (format nil "SYSTEM-AREA-UB~D-FILL" bitsize) (find-package "SB!KERNEL")))
         (unary-bash-name (intern (format nil "UNARY-UB~D-BASH" bitsize) (find-package "SB!KERNEL")))
         (array-copy-name (intern (format nil "UB~D-BASH-COPY" bitsize) (find-package "SB!KERNEL")))
         (system-area-copy-name (intern (format nil "SYSTEM-AREA-UB~D-COPY" bitsize) (find-package "SB!KERNEL")))
         (array-copy-to-system-area-name
          (intern (format nil "COPY-UB~D-TO-SYSTEM-AREA" bitsize) (find-package "SB!KERNEL")))
         (system-area-copy-to-array-name
          (intern (format nil "COPY-UB~D-FROM-SYSTEM-AREA" bitsize)
                  (find-package "SB!KERNEL"))))
    `(progn
      (declaim (inline ,constant-bash-name ,unary-bash-name))
      ;; Fill DST with VALUE starting at DST-OFFSET and continuing
      ;; for LENGTH bytes (however bytes are defined).
      (defun ,constant-bash-name (dst dst-offset length value
                                      dst-ref-fn dst-set-fn)
        (declare (type word value) (type index dst-offset length))
        (declare (ignorable dst-ref-fn))
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
                    (locally (declare (type ,byte-count length))
                      (funcall dst-set-fn dst dst-word-offset
                               (if (= length ,bytes-per-word)
                                   value
                                   (let ((mask (shift-towards-end
                                                (start-mask (* length ,bitsize))
                                                (* dst-byte-offset ,bitsize))))
                                     (word-logical-or (word-logical-and value mask)
                                                      (word-logical-andc2 (funcall dst-ref-fn dst dst-word-offset)
                                                                          mask))))))))
                (let ((interior (floor (- length final-bytes) ,bytes-per-word)))
                  ,@(unless (= bytes-per-word 1)
                     `((unless (zerop dst-byte-offset)
                         (let ((mask (end-mask (* (- dst-byte-offset) ,bitsize))))
                           (funcall dst-set-fn dst dst-word-offset
                                    (word-logical-or (word-logical-and value mask)
                                                     (word-logical-andc2 (funcall dst-ref-fn dst dst-word-offset)
                                                                         mask))))
                         (incf dst-word-offset))))
                  (let ((end (+ dst-word-offset interior)))
                    (declare (type ,word-offset end))
                    (do ()
                        ((>= dst-word-offset end))
                      (funcall dst-set-fn dst dst-word-offset value)
                      (incf dst-word-offset)))
                  #+nil
                  (dotimes (i interior)
                    (funcall dst-set-fn dst dst-word-offset value)
                    (incf dst-word-offset))
                  ,@(unless (= bytes-per-word 1)
                     `((unless (zerop final-bytes)
                         (let ((mask (start-mask (* final-bytes ,bitsize))))
                           (funcall dst-set-fn dst dst-word-offset
                                    (word-logical-or (word-logical-and value mask)
                                                     (word-logical-andc2 (funcall dst-ref-fn dst dst-word-offset)
                                                                         mask)))))))))))
        (values))

      ;; common uses for constant-byte-bashing
      (defknown ,array-fill-name (word simple-unboxed-array ,offset ,offset)
          simple-unboxed-array
          ()
        :result-arg 1)
      (defun ,array-fill-name (value dst dst-offset length)
        (declare (type word value) (type ,offset dst-offset length))
        (declare (optimize (speed 3) (safety 1)))
        (,constant-bash-name dst dst-offset length value
                             #'%vector-raw-bits #'%set-vector-raw-bits)
        dst)
      (defun ,system-area-fill-name (value dst dst-offset length)
        (declare (type word value) (type ,offset dst-offset length))
        (declare (optimize (speed 3) (safety 1)))
        (multiple-value-bind (dst dst-offset) (,fix-sap-and-offset-name dst dst-offset)
          (,constant-bash-name dst dst-offset length value
                               #'word-sap-ref #'%set-word-sap-ref)))

         ;; unary byte bashing (copying)
         (defun ,unary-bash-name (src src-offset dst dst-offset length
                                      dst-ref-fn dst-set-fn src-ref-fn)
           (declare (type index src-offset dst-offset length)
                    (type function dst-ref-fn dst-set-fn src-ref-fn)
                    (ignorable dst-ref-fn))
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
                    ((= length ,bytes-per-word)
                     ;; DST-BYTE-OFFSET must be equal to zero, or we would be
                     ;; writing multiple words.  If SRC-BYTE-OFFSET is also zero,
                     ;; the we just transfer the single word.  Otherwise we have
                     ;; to extract bytes from two source words.
                     (funcall dst-set-fn dst dst-word-offset
                             (cond
                               ((zerop src-byte-offset)
                                (funcall src-ref-fn src src-word-offset))
                               ,@(unless (= bytes-per-word 1)
                                  `((t (word-logical-or (shift-towards-start
                                                         (funcall src-ref-fn src src-word-offset)
                                                         (* src-byte-offset ,bitsize))
                                        (shift-towards-end
                                          (funcall src-ref-fn src (1+ src-word-offset))
                                          (* (- src-byte-offset) ,bitsize)))))))))
                    ,@(unless (= bytes-per-word 1)
                       `((t
                          ;; We are only writing some portion of the destination word.
                          ;; We still don't know whether we need one or two source words.
                          (locally (declare (type ,byte-count length))
                            (let ((mask (shift-towards-end (start-mask (* length ,bitsize))
                                                           (* dst-byte-offset ,bitsize)))
                                  (orig (funcall dst-ref-fn dst dst-word-offset))
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
                                                     (funcall src-ref-fn src src-word-offset)
                                                     (* src-byte-shift ,bitsize))
                                                    (shift-towards-end
                                                     (funcall src-ref-fn src (1+ src-word-offset))
                                                     (* (- src-byte-shift) ,bitsize)))
                                                   (shift-towards-start (funcall src-ref-fn src src-word-offset)
                                                                        (* src-byte-shift ,bitsize))))
                                             ;; The destination starts further
                                             ;; into the word than does the
                                             ;; source, so we know the source
                                             ;; cannot extend into a second
                                             ;; word (or else the destination
                                             ;; would too, and we wouldn't be
                                             ;; in this branch).
                                             (shift-towards-end
                                              (funcall src-ref-fn src src-word-offset)
                                              (* (- dst-byte-offset src-byte-offset) ,bitsize)))))
                              (declare (type word mask orig value))
                              (funcall dst-set-fn dst dst-word-offset
                                       (word-logical-or (word-logical-and value mask)
                                                        (word-logical-andc2 orig mask))))))))))
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
                                      (orig (funcall dst-ref-fn dst dst-word-offset))
                                      (value (funcall src-ref-fn src src-word-offset)))
                                  (declare (type word mask orig value))
                                  (funcall dst-set-fn dst dst-word-offset
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
                             (funcall dst-set-fn dst dst-word-offset
                                      (funcall src-ref-fn src src-word-offset))
                             ,(if (= bytes-per-word 1)
                                  `(setf src-word-offset (truly-the ,word-offset (+ src-word-offset 1)))
                                  `(incf src-word-offset))
                             (incf dst-word-offset)))
                         ,@(unless (= bytes-per-word 1)
                            `((unless (zerop final-bytes)
                                ;; We are only writing part of the last word.
                                (let ((mask (start-mask (* final-bytes ,bitsize)))
                                      (orig (funcall dst-ref-fn dst dst-word-offset))
                                      (value (funcall src-ref-fn src src-word-offset)))
                                  (declare (type word mask orig value))
                                  (funcall dst-set-fn dst dst-word-offset
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
                                      (orig (funcall dst-ref-fn dst dst-word-offset))
                                      (value (funcall src-ref-fn src src-word-offset)))
                                  (declare (type word mask orig value))
                                  (funcall dst-set-fn dst dst-word-offset
                                           (word-logical-or (word-logical-and value mask)
                                                            (word-logical-andc2 orig mask)))))))
                         (let ((end (- dst-word-offset interior)))
                           (do ()
                               ((<= dst-word-offset end))
                             (decf src-word-offset)
                             (decf dst-word-offset)
                             (funcall dst-set-fn dst dst-word-offset
                                      (funcall src-ref-fn src src-word-offset))))
                         ,@(unless (= bytes-per-word 1)
                            `((unless (zerop dst-byte-offset)
                                ;; We are only writing part of the last word.
                                (decf src-word-offset)
                                (decf dst-word-offset)
                                (let ((mask (end-mask (* (- dst-byte-offset) ,bitsize)))
                                      (orig (funcall dst-ref-fn dst dst-word-offset))
                                      (value (funcall src-ref-fn src src-word-offset)))
                                  (declare (type word mask orig value))
                                  (funcall dst-set-fn dst dst-word-offset
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
                               (next (funcall src-ref-fn src src-word-offset)))
                           (declare (type word prev next))
                           (flet ((get-next-src ()
                                    (setf prev next)
                                    (setf next (funcall src-ref-fn src
                                                        (incf src-word-offset)))))
                             (declare (inline get-next-src))
                             ,@(unless (= bytes-per-word 1)
                                `((unless (zerop dst-byte-offset)
                                    (when (> src-byte-offset dst-byte-offset)
                                      (get-next-src))
                                    (let ((mask (end-mask (* (- dst-byte-offset) ,bitsize)))
                                          (orig (funcall dst-ref-fn dst dst-word-offset))
                                          (value (word-logical-or (shift-towards-start prev (* src-shift ,bitsize))
                                                                  (shift-towards-end next (* (- src-shift) ,bitsize)))))
                                      (declare (type word mask orig value))
                                      (funcall dst-set-fn dst dst-word-offset
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
                                   (funcall dst-set-fn dst dst-word-offset value)
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
                                          (orig (funcall dst-ref-fn dst dst-word-offset)))
                                      (declare (type word mask orig value))
                                      (funcall dst-set-fn dst dst-word-offset
                                               (word-logical-or (word-logical-and value mask)
                                                                (word-logical-andc2 orig mask))))))))))
                        (t
                         ;; We need to loop from right to left.
                         (incf dst-word-offset words)
                         (incf src-word-offset (1- (ceiling (+ src-byte-offset length) ,bytes-per-word)))
                         (let ((next 0)
                               (prev (funcall src-ref-fn src src-word-offset)))
                           (declare (type word prev next))
                           (flet ((get-next-src ()
                                    (setf next prev)
                                    (setf prev (funcall src-ref-fn src (decf src-word-offset)))))
                             (declare (inline get-next-src))
                             ,@(unless (= bytes-per-word 1)
                                `((unless (zerop final-bytes)
                                    (when (> final-bytes (- ,bytes-per-word src-shift))
                                      (get-next-src))
                                    (let ((value (word-logical-or
                                                  (shift-towards-end next (* (- src-shift) ,bitsize))
                                                  (shift-towards-start prev (* src-shift ,bitsize))))
                                          (mask (start-mask (* final-bytes ,bitsize)))
                                          (orig (funcall dst-ref-fn dst dst-word-offset)))
                                      (declare (type word mask orig value))
                                      (funcall dst-set-fn dst dst-word-offset
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
                                   (funcall dst-set-fn dst dst-word-offset value)
                                   (decf dst-word-offset))))
                             ,@(unless (= bytes-per-word 1)
                                `((unless (zerop dst-byte-offset)
                                    (if (> src-byte-offset dst-byte-offset)
                                        (get-next-src)
                                        (setf next prev prev 0))
                                    (let ((mask (end-mask (* (- dst-byte-offset) ,bitsize)))
                                          (orig (funcall dst-ref-fn dst dst-word-offset))
                                          (value (word-logical-or
                                                  (shift-towards-start prev (* src-shift ,bitsize))
                                                  (shift-towards-end next (* (- src-shift) ,bitsize)))))
                                      (declare (type word mask orig value))
                                      (funcall dst-set-fn dst dst-word-offset
                                              (word-logical-or (word-logical-and value mask)
                                                               (word-logical-andc2 orig mask)))))))))))))))))
           (values))

         ;; common uses for unary-byte-bashing
         (defun ,array-copy-name (src src-offset dst dst-offset length)
           (declare (type ,offset src-offset dst-offset length))
           (locally (declare (optimize (speed 3) (safety 1)))
             (,unary-bash-name src src-offset dst dst-offset length
                               #'%vector-raw-bits
                               #'%set-vector-raw-bits
                               #'%vector-raw-bits)))

         (defun ,system-area-copy-name (src src-offset dst dst-offset length)
           (declare (type ,offset src-offset dst-offset length))
           (locally (declare (optimize (speed 3) (safety 1)))
             (multiple-value-bind (src src-offset) (,fix-sap-and-offset-name src src-offset)
               (declare (type system-area-pointer src))
               (multiple-value-bind (dst dst-offset) (,fix-sap-and-offset-name dst dst-offset)
                 (declare (type system-area-pointer dst))
                 (,unary-bash-name src src-offset dst dst-offset length
                                   #'word-sap-ref #'%set-word-sap-ref
                                   #'word-sap-ref)))))

         (defun ,array-copy-to-system-area-name (src src-offset dst dst-offset length)
           (declare (type ,offset src-offset dst-offset length))
           (locally (declare (optimize (speed 3) (safety 1)))
             (multiple-value-bind (dst dst-offset) (,fix-sap-and-offset-name  dst dst-offset)
               (,unary-bash-name src src-offset dst dst-offset length
                                 #'word-sap-ref #'%set-word-sap-ref
                                 #'%vector-raw-bits))))

         (defun ,system-area-copy-to-array-name (src src-offset dst dst-offset length)
           (declare (type ,offset src-offset dst-offset length))
           (locally (declare (optimize (speed 3) (safety 1)))
             (multiple-value-bind (src src-offset) (,fix-sap-and-offset-name src src-offset)
               (,unary-bash-name src src-offset dst dst-offset length
                                 #'%vector-raw-bits
                                 #'%set-vector-raw-bits
                                 #'word-sap-ref)))))))
) ; EVAL-WHEN

;;; We would normally do this with a MACROLET, but then we run into
;;; problems with the lexical environment being too hairy for the
;;; cross-compiler and it cannot inline the basic basher functions.
#.(loop for i = 1 then (* i 2)
        collect `(!define-sap-fixer ,i) into fixers
        collect `(!define-byte-bashers ,i) into bashers
        until (= i sb!vm:n-word-bits)
        ;; FIXERS must come first so their inline expansions are available
        ;; for the bashers.
        finally (return `(progn ,@fixers ,@bashers)))

;;; a common idiom for calling COPY-TO-SYSTEM-AREA
;;;
;;; Copy the entire contents of the vector V to memory starting at SAP+OFFSET.
(defun copy-byte-vector-to-system-area (bv sap &optional (offset 0))
  ;; FIXME: There should be a type like SB!VM:BYTE so that we can write this
  ;; type as (SIMPLE-ARRAY SB!VM:BYTE 1). Except BYTE is an external symbol of
  ;; package CL, and shadowing it would be too ugly; so maybe SB!VM:VMBYTE?
  ;; (And then N-BYTE-BITS would be N-VMBYTE-BITS and so forth?)
  (declare (type (simple-array (unsigned-byte 8) 1) bv))
  (declare (type system-area-pointer sap))
  (declare (type fixnum offset))
  (copy-ub8-to-system-area bv 0 sap offset (length bv)))


;;;; Bashing-Style search for bits
;;;;
;;;; Similar search would work well for base-strings as well.
;;;; (Technically for all unboxed sequences of sub-word size elements,
;;;; but somehow I doubt eg. octet vectors get POSITION or FIND used
;;;; as much on them.)
(defconstant +bit-position-base-mask+ (1- n-word-bits))
(defconstant +bit-position-base-shift+ (integer-length +bit-position-base-mask+))
(macrolet ((def (name frob)
             `(defun ,name (vector from-end start end)
                (declare (simple-bit-vector vector)
                         (index start end)
                         (optimize (speed 3) (safety 0)))
                (unless (= start end)
                  (let* ((last-word (ash end (- +bit-position-base-shift+)))
                         (last-bits (logand end +bit-position-base-mask+))
                         (first-word (ash start (- +bit-position-base-shift+)))
                         (first-bits (logand start +bit-position-base-mask+))
                         ;; These mask out everything but the interesting parts.
                         (end-mask #!+little-endian (lognot (ash -1 last-bits))
                                   #!+big-endian (ash -1 (- sb!vm:n-word-bits last-bits)))
                         (start-mask #!+little-endian (ash -1 first-bits)
                                     #!+big-endian (lognot (ash -1 (- sb!vm:n-word-bits first-bits)))))
                    (declare (index last-word first-word))
                    (flet ((#!+little-endian start-bit
                            #!+big-endian end-bit (x)
                             (declare (word x))
                             #!+(or x86-64 x86)
                             (truly-the (mod #.n-word-bits)
                                 (%primitive unsigned-word-find-first-bit x))
                             #!-(or x86-64 x86)
                             (- #!+big-endian sb!vm:n-word-bits
                                (integer-length (logand x (- x)))
                                #!+little-endian 1))
                           (#!+little-endian end-bit
                            #!+big-endian start-bit (x)
                             (declare (word x))
                             (- #!+big-endian sb!vm:n-word-bits
                                (integer-length x)
                                #!+little-endian 1))
                           (found (i word-offset)
                             (declare (index i word-offset))
                             (return-from ,name
                               (logior i (truly-the
                                          fixnum
                                          (ash word-offset +bit-position-base-shift+)))))
                           (get-word (offset)
                             (,@frob (%vector-raw-bits vector offset))))
                      ;; Inline FOUND makes the code smaller, as well as faster,
                      ;; because it becomes clear that there is no establishing
                      ;; of an exit point and hence no call to UNWIND.
                      (declare (inline start-bit end-bit found get-word))
                      (if from-end
                          ;; Back to front
                          (let* ((word-offset last-word)
                                 (word (logand end-mask (get-word word-offset))))
                            (declare (word word)
                                     (index word-offset))
                            (unless (zerop word)
                              (when (= word-offset first-word)
                                (setf word (logand word start-mask)))
                              (unless (zerop word)
                                (found (end-bit word) word-offset)))
                            (decf word-offset)
                            (loop
                              (when (< word-offset first-word)
                                (return-from ,name nil))
                              (setf word (get-word word-offset))
                              (unless (zerop word)
                                (when (= word-offset first-word)
                                  (setf word (logand word start-mask)))
                                (unless (zerop word)
                                  (found (end-bit word) word-offset)))
                              (decf word-offset)))
                          ;; Front to back
                          (let* ((word-offset first-word)
                                 (word (logand start-mask (get-word word-offset))))
                            (declare (word word)
                                     (index word-offset))
                            (unless (zerop word)
                              (when (= word-offset last-word)
                                (setf word (logand word end-mask)))
                              (unless (zerop word)
                                (found (start-bit word) word-offset)))
                            (incf word-offset)
                            (loop
                              (when (> word-offset last-word)
                                (return-from ,name nil))
                              (setf word (get-word word-offset))
                              (unless (zerop word)
                                (when (= word-offset last-word)
                                  (setf word (logand word end-mask)))
                                (unless (zerop word)
                                  (found (start-bit word) word-offset)))
                              (incf word-offset))))))))))
  (def %bit-position/0 (logandc2 #.(1- (expt 2 n-word-bits))))
  (def %bit-position/1 (identity)))
(defun %bit-position (bit vector from-end start end)
  (case bit
    (0 (%bit-position/0 vector from-end start end))
    (1 (%bit-position/1 vector from-end start end))
    (otherwise nil)))
