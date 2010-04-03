;;;; FOP definitions

(in-package "SB!FASL")

;;; Sometimes we want to skip over any FOPs with side-effects (like
;;; function calls) while executing other FOPs. *SKIP-UNTIL* will
;;; either contain the position where the skipping will stop, or
;;; NIL if we're executing normally.
(defvar *skip-until* nil)

;;; Define NAME as a fasl operation, with op-code FOP-CODE. PUSHP
;;; describes what the body does to the fop stack:
;;;   T
;;;     The body might pop the fop stack. The result of the body is
;;;     pushed on the fop stack.
;;;   NIL
;;;     The body might pop the fop stack. The result of the body is
;;;     discarded.
;;; STACKP describes whether or not the body interacts with the fop stack.
(defmacro define-fop ((name fop-code &key (pushp t) (stackp t)) &rest forms)
  `(progn
     (defun ,name ()
       ,(if stackp
            `(with-fop-stack ,pushp ,@forms)
            `(progn ,@forms)))
     (%define-fop ',name ,fop-code)))

(defun %define-fop (name code)
  (let ((oname (svref *fop-names* code)))
    (when (and oname (not (eq oname name)))
      (error "multiple names for fop code ~D: ~S and ~S" code name oname)))
  ;; KLUDGE: It's mnemonically suboptimal to use 'FOP-CODE as the name of the
  ;; tag which associates names with codes when it's also used as one of
  ;; the names. Perhaps the fops named FOP-CODE and FOP-SMALL-CODE could
  ;; be renamed to something more mnemonic? -- WHN 19990902
  (let ((ocode (get name 'fop-code)))
    (when (and ocode (/= ocode code))
      (error "multiple codes for fop name ~S: ~D and ~D" name code ocode)))
  (setf (svref *fop-names* code) name
        (get name 'fop-code) code
        (svref *fop-funs* code) (symbol-function name))
  (values))

;;; Define a pair of fops which are identical except that one reads
;;; a four-byte argument while the other reads a one-byte argument. The
;;; argument can be accessed by using the CLONE-ARG macro.
;;;
;;; KLUDGE: It would be nice if the definition here encapsulated which
;;; value ranges went with which fop variant, and chose the correct
;;; fop code to use. Currently, since such logic isn't encapsulated,
;;; we see callers doing stuff like
;;;     (cond ((and (< num-consts #x100) (< total-length #x10000))
;;;            (dump-fop 'sb!impl::fop-small-code file)
;;;            (dump-byte num-consts file)
;;;            (dump-integer-as-n-bytes total-length 2 file))
;;;           (t
;;;            (dump-fop 'sb!impl::fop-code file)
;;;            (dump-word num-consts file)
;;;            (dump-word total-length file))))
;;; in several places. It would be cleaner if this could be replaced with
;;; something like
;;;     (dump-fop file fop-code num-consts total-length)
;;; Some of this logic is already in DUMP-FOP*, but that still requires the
;;; caller to know that it's a 1-byte-arg/4-byte-arg cloned fop pair, and to
;;; know both the 1-byte-arg and the 4-byte-arg fop names. -- WHN 19990902
(defmacro define-cloned-fops ((name code &key (pushp t) (stackp t))
                              (small-name small-code) &rest forms)
  (aver (member pushp '(nil t)))
  (aver (member stackp '(nil t)))
  `(progn
     (macrolet ((clone-arg () '(read-word-arg)))
       (define-fop (,name ,code :pushp ,pushp :stackp ,stackp) ,@forms))
     (macrolet ((clone-arg () '(read-byte-arg)))
       (define-fop (,small-name ,small-code :pushp ,pushp :stackp ,stackp) ,@forms))))

;;; a helper function for reading string values from FASL files: sort
;;; of like READ-SEQUENCE specialized for files of (UNSIGNED-BYTE 8),
;;; with an automatic conversion from (UNSIGNED-BYTE 8) into CHARACTER
;;; for each element read
(declaim (ftype (function (stream simple-string &optional index) (values))
                read-string-as-bytes
                #!+sb-unicode read-string-as-unsigned-byte-32))
(defun read-string-as-bytes (stream string &optional (length (length string)))
  (dotimes (i length)
    (setf (aref string i)
          (sb!xc:code-char (read-byte stream))))
  ;; FIXME: The classic CMU CL code to do this was
  ;;   (READ-N-BYTES FILE STRING START END).
  ;; It was changed for SBCL because we needed a portable version for
  ;; bootstrapping. Benchmark the non-portable version and see whether it's
  ;; significantly better than the portable version here. If it is, then use
  ;; it as an alternate definition, protected with #-SB-XC-HOST.
  (values))
#!+sb-unicode
(defun read-string-as-unsigned-byte-32
    (stream string &optional (length (length string)))
  #+sb-xc-host (bug "READ-STRING-AS-UNSIGNED-BYTE-32 called")
  (dotimes (i length)
    (setf (aref string i)
          (let ((code 0))
            (dotimes (k 4 (sb!xc:code-char code))
              (setf code (logior code (ash (read-byte stream)
                                           (* k sb!vm:n-byte-bits))))))))
  (values))

;;;; miscellaneous fops

;;; FIXME: POP-STACK should be called something more mnemonic. (POP-FOP-STACK?
;;; But that would conflict with PUSH-FOP-TABLE. Something, anyway..)

;;; Setting this variable causes execution of a FOP-NOP4 to produce
;;; output to *DEBUG-IO*. This can be handy when trying to follow the
;;; progress of FASL loading.
#!+sb-show
(defvar *show-fop-nop4-p* nil)

;;; CMU CL had a single no-op fop, FOP-NOP, with fop code 0. Since 0
;;; occurs disproportionately often in fasl files for other reasons,
;;; FOP-NOP is less than ideal for writing human-readable patterns
;;; into fasl files for debugging purposes. There's no shortage of
;;; unused fop codes, so we add this second NOP, which reads 4
;;; arbitrary bytes and discards them.
(define-fop (fop-nop4 137 :stackp nil)
  (let ((arg (read-arg 4)))
    (declare (ignorable arg))
    #!+sb-show
    (when *show-fop-nop4-p*
      (format *debug-io* "~&/FOP-NOP4 ARG=~W=#X~X~%" arg arg))))

(define-fop (fop-nop 0 :stackp nil))
(define-fop (fop-pop 1 :pushp nil) (push-fop-table (pop-stack)))
(define-fop (fop-push 2) (svref *current-fop-table* (read-word-arg)))
(define-fop (fop-byte-push 3) (svref *current-fop-table* (read-byte-arg)))

(define-fop (fop-empty-list 4) ())
(define-fop (fop-truth 5) t)
;;; CMU CL had FOP-POP-FOR-EFFECT as fop 65, but it was never used and seemed
;;; to have no possible use.
(define-fop (fop-misc-trap 66)
  #+sb-xc-host ; since xc host doesn't know how to compile %PRIMITIVE
  (error "FOP-MISC-TRAP can't be defined without %PRIMITIVE.")
  #-sb-xc-host
  (%primitive sb!c:make-other-immediate-type 0 sb!vm:unbound-marker-widetag))

(define-cloned-fops (fop-character 68) (fop-short-character 69)
  (code-char (clone-arg)))

(define-cloned-fops (fop-struct 48) (fop-small-struct 49)
  (let* ((size (clone-arg))
         (res (%make-instance size)))
    (declare (type index size))
    (let* ((layout (pop-stack))
           (nuntagged (layout-n-untagged-slots layout))
           (ntagged (- size nuntagged)))
      (setf (%instance-ref res 0) layout)
      (dotimes (n (1- ntagged))
        (declare (type index n))
        (setf (%instance-ref res (1+ n)) (pop-stack)))
      (dotimes (n nuntagged)
        (declare (type index n))
        (setf (%raw-instance-ref/word res (- nuntagged n 1)) (pop-stack))))
    res))

(define-fop (fop-layout 45)
  (let ((nuntagged (pop-stack))
        (length (pop-stack))
        (depthoid (pop-stack))
        (inherits (pop-stack))
        (name (pop-stack)))
    (find-and-init-or-check-layout name length inherits depthoid nuntagged)))

(define-fop (fop-end-group 64 :stackp nil)
  (/show0 "THROWing FASL-GROUP-END")
  (throw 'fasl-group-end t))

;;; We used to have FOP-NORMAL-LOAD as 81 and FOP-MAYBE-COLD-LOAD as
;;; 82 until GENESIS learned how to work with host symbols and
;;; packages directly instead of piggybacking on the host code.

(define-fop (fop-verify-table-size 62 :stackp nil)
  (let ((expected-index (read-word-arg)))
    (unless (= *current-fop-table-index* expected-index)
      (bug "fasl table of improper size"))))
(define-fop (fop-verify-empty-stack 63 :stackp nil)
  (unless (zerop (length *fop-stack*))
    (bug "fasl stack not empty when it should be")))

;;;; fops for loading symbols

(macrolet (;; FIXME: Should all this code really be duplicated inside
           ;; each fop? Perhaps it would be better for this shared
           ;; code to live in FLET FROB1 and FLET FROB4 (for the
           ;; two different sizes of counts).
           (frob (name code name-size package)
             (let ((n-package (gensym))
                   (n-size (gensym))
                   (n-buffer (gensym)))
               `(define-fop (,name ,code)
                  (prepare-for-fast-read-byte *fasl-input-stream*
                    (let ((,n-package ,package)
                          (,n-size (fast-read-u-integer ,name-size)))
                      (when (> ,n-size (length *fasl-symbol-buffer*))
                        (setq *fasl-symbol-buffer*
                              (make-string (* ,n-size 2))))
                      (done-with-fast-read-byte)
                      (let ((,n-buffer *fasl-symbol-buffer*))
                        #+sb-xc-host
                        (read-string-as-bytes *fasl-input-stream*
                                              ,n-buffer
                                              ,n-size)
                        #-sb-xc-host
                        (#!+sb-unicode read-string-as-unsigned-byte-32
                         #!-sb-unicode read-string-as-bytes
                         *fasl-input-stream*
                         ,n-buffer
                         ,n-size)
                        (push-fop-table (without-package-locks
                                         (intern* ,n-buffer
                                                  ,n-size
                                                  ,n-package))))))))))

  ;; Note: CMU CL had FOP-SYMBOL-SAVE and FOP-SMALL-SYMBOL-SAVE, but
  ;; since they made the behavior of the fasloader depend on the
  ;; *PACKAGE* variable, not only were they a pain to support (because
  ;; they required various hacks to handle *PACKAGE*-manipulation
  ;; forms) they were basically broken by design, because ANSI gives
  ;; the user so much flexibility in manipulating *PACKAGE* at
  ;; load-time that no reasonable hacks could possibly make things
  ;; work right. The ones used in CMU CL certainly didn't, as shown by
  ;; e.g.
  ;;   (IN-PACKAGE :CL-USER)
  ;;     (DEFVAR CL::*FOO* 'FOO-VALUE)
  ;;     (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  ;;       (SETF *PACKAGE* (FIND-PACKAGE :CL)))
  ;; which in CMU CL 2.4.9 defines a variable CL-USER::*FOO* instead of
  ;; defining CL::*FOO*. Therefore, we don't use those fops in SBCL.
  ;;(frob fop-symbol-save               6 4 *package*)
  ;;(frob fop-small-symbol-save   7 1 *package*)

  (frob fop-lisp-symbol-save          75 #.sb!vm:n-word-bytes *cl-package*)
  (frob fop-lisp-small-symbol-save    76 1 *cl-package*)
  (frob fop-keyword-symbol-save       77 #.sb!vm:n-word-bytes *keyword-package*)
  (frob fop-keyword-small-symbol-save 78 1 *keyword-package*)

  ;; FIXME: Because we don't have FOP-SYMBOL-SAVE any more, an enormous number
  ;; of symbols will fall through to this case, probably resulting in bloated
  ;; fasl files. A new
  ;; FOP-SYMBOL-IN-LAST-PACKAGE-SAVE/FOP-SMALL-SYMBOL-IN-LAST-PACKAGE-SAVE
  ;; cloned fop pair could undo some of this bloat.
  (frob fop-symbol-in-package-save 8 #.sb!vm:n-word-bytes
    (svref *current-fop-table* (fast-read-u-integer #.sb!vm:n-word-bytes)))
  (frob fop-small-symbol-in-package-save 9 1
    (svref *current-fop-table* (fast-read-u-integer #.sb!vm:n-word-bytes)))
  (frob fop-symbol-in-byte-package-save 10 #.sb!vm:n-word-bytes
    (svref *current-fop-table* (fast-read-u-integer 1)))
  (frob fop-small-symbol-in-byte-package-save 11 1
    (svref *current-fop-table* (fast-read-u-integer 1))))

(define-cloned-fops (fop-uninterned-symbol-save 12)
                    (fop-uninterned-small-symbol-save 13)
  (let* ((arg (clone-arg))
         (res (make-string arg)))
    #!-sb-unicode
    (read-string-as-bytes *fasl-input-stream* res)
    #!+sb-unicode
    (read-string-as-unsigned-byte-32 *fasl-input-stream* res)
    (push-fop-table (make-symbol res))))

(define-fop (fop-package 14)
  (find-undeleted-package-or-lose (pop-stack)))

(define-cloned-fops (fop-named-package-save 156 :stackp nil)
                    (fop-small-named-package-save 157)
  (let* ((arg (clone-arg))
         (package-name (make-string arg)))
    #+sb-xc-host
    (read-string-as-bytes *fasl-input-stream* package-name)
    #-sb-xc-host
    (#!-sb-unicode read-string-as-bytes
     #!+sb-unicode read-string-as-unsigned-byte-32
     *fasl-input-stream* package-name)
    (push-fop-table (find-undeleted-package-or-lose package-name))))

;;;; fops for loading numbers

;;; Load a signed integer LENGTH bytes long from *FASL-INPUT-STREAM*.
(defun load-s-integer (length)
  (declare (fixnum length))
  ;; #+cmu (declare (optimize (inhibit-warnings 2)))
  (do* ((index length (1- index))
        (byte 0 (read-byte *fasl-input-stream*))
        (result 0 (+ result (ash byte bits)))
        (bits 0 (+ bits 8)))
       ((= index 0)
        (if (logbitp 7 byte)    ; look at sign bit
            (- result (ash 1 bits))
            result))
    (declare (fixnum index byte bits))))

(define-cloned-fops (fop-integer 33) (fop-small-integer 34)
  (load-s-integer (clone-arg)))

(define-fop (fop-word-integer 35)
  (prepare-for-fast-read-byte *fasl-input-stream*
    (prog1
     (fast-read-s-integer #.sb!vm:n-word-bytes)
     (done-with-fast-read-byte))))

(define-fop (fop-byte-integer 36)
  (prepare-for-fast-read-byte *fasl-input-stream*
    (prog1
     (fast-read-s-integer 1)
     (done-with-fast-read-byte))))

(define-fop (fop-ratio 70)
  (let ((den (pop-stack)))
    (%make-ratio (pop-stack) den)))

(define-fop (fop-complex 71)
  (let ((im (pop-stack)))
    (%make-complex (pop-stack) im)))

(macrolet ((fast-read-single-float ()
             '(make-single-float (fast-read-s-integer 4)))
           (fast-read-double-float ()
             '(let ((lo (fast-read-u-integer 4)))
               (make-double-float (fast-read-s-integer 4) lo))))
  (macrolet ((define-complex-fop (name fop-code type)
               (let ((reader (symbolicate "FAST-READ-" type)))
                 `(define-fop (,name ,fop-code)
                      (prepare-for-fast-read-byte *fasl-input-stream*
                        (prog1
                            (complex (,reader) (,reader))
                          (done-with-fast-read-byte))))))
             (define-float-fop (name fop-code type)
               (let ((reader (symbolicate "FAST-READ-" type)))
                 `(define-fop (,name ,fop-code)
                      (prepare-for-fast-read-byte *fasl-input-stream*
                        (prog1
                            (,reader)
                          (done-with-fast-read-byte)))))))
    (define-complex-fop fop-complex-single-float 72 single-float)
    (define-complex-fop fop-complex-double-float 73 double-float)
    #!+long-float
    (define-complex-fop fop-complex-long-float 67 long-float)
    (define-float-fop fop-single-float 46 single-float)
    (define-float-fop fop-double-float 47 double-float)
    #!+long-float
    (define-float-fop fop-long-float 52 long-float)))


;;;; loading lists

(define-fop (fop-list 15)
  (do ((res () (cons (pop-stack) res))
       (n (read-byte-arg) (1- n)))
      ((zerop n) res)
    (declare (type index n))))

(define-fop (fop-list* 16)
  (do ((res (pop-stack) (cons (pop-stack) res))
       (n (read-byte-arg) (1- n)))
      ((zerop n) res)
    (declare (type index n))))

(macrolet ((frob (name op fun n)
             `(define-fop (,name ,op)
                (call-with-popped-args ,fun ,n))))

  (frob fop-list-1 17 list 1)
  (frob fop-list-2 18 list 2)
  (frob fop-list-3 19 list 3)
  (frob fop-list-4 20 list 4)
  (frob fop-list-5 21 list 5)
  (frob fop-list-6 22 list 6)
  (frob fop-list-7 23 list 7)
  (frob fop-list-8 24 list 8)

  (frob fop-list*-1 25 list* 2)
  (frob fop-list*-2 26 list* 3)
  (frob fop-list*-3 27 list* 4)
  (frob fop-list*-4 28 list* 5)
  (frob fop-list*-5 29 list* 6)
  (frob fop-list*-6 30 list* 7)
  (frob fop-list*-7 31 list* 8)
  (frob fop-list*-8 32 list* 9))

;;;; fops for loading arrays

(define-cloned-fops (fop-base-string 37) (fop-small-base-string 38)
  (let* ((arg (clone-arg))
         (res (make-string arg :element-type 'base-char)))
    (read-string-as-bytes *fasl-input-stream* res)
    res))

#!+sb-unicode
(progn
  #+sb-xc-host
  (define-cloned-fops (fop-character-string 161) (fop-small-character-string 162)
    (bug "CHARACTER-STRING FOP encountered"))

  #-sb-xc-host
  (define-cloned-fops (fop-character-string 161) (fop-small-character-string 162)
    (let* ((arg (clone-arg))
           (res (make-string arg)))
      (read-string-as-unsigned-byte-32 *fasl-input-stream* res)
      res)))

(define-cloned-fops (fop-vector 39) (fop-small-vector 40)
  (let* ((size (clone-arg))
         (res (make-array size)))
    (declare (fixnum size))
    (do ((n (1- size) (1- n)))
        ((minusp n))
      (setf (svref res n) (pop-stack)))
    res))

(define-fop (fop-array 83)
  (let* ((rank (read-word-arg))
         (vec (pop-stack))
         (length (length vec))
         (res (make-array-header sb!vm:simple-array-widetag rank)))
    (declare (simple-array vec)
             (type (unsigned-byte #.(- sb!vm:n-word-bits sb!vm:n-widetag-bits)) rank))
    (set-array-header res vec length nil 0
                      (do ((i rank (1- i))
                           (dimensions () (cons (pop-stack) dimensions)))
                          ((zerop i) dimensions)
                        (declare (type index i)))
                      nil
                      t)
    res))

(define-fop (fop-single-float-vector 84)
  (let* ((length (read-word-arg))
         (result (make-array length :element-type 'single-float)))
    (read-n-bytes *fasl-input-stream* result 0 (* length 4))
    result))

(define-fop (fop-double-float-vector 85)
  (let* ((length (read-word-arg))
         (result (make-array length :element-type 'double-float)))
    (read-n-bytes *fasl-input-stream* result 0 (* length 8))
    result))

(define-fop (fop-complex-single-float-vector 86)
  (let* ((length (read-word-arg))
         (result (make-array length :element-type '(complex single-float))))
    (read-n-bytes *fasl-input-stream* result 0 (* length 8))
    result))

(define-fop (fop-complex-double-float-vector 87)
  (let* ((length (read-word-arg))
         (result (make-array length :element-type '(complex double-float))))
    (read-n-bytes *fasl-input-stream* result 0 (* length 16))
    result))

;;; CMU CL comment:
;;;   *** NOT *** the FOP-INT-VECTOR as currently documented in rtguts.
;;;   Size must be a directly supported I-vector element size, with no
;;;   extra bits. This must be packed according to the local
;;;   byte-ordering, allowing us to directly read the bits.
(define-fop (fop-int-vector 43)
  (prepare-for-fast-read-byte *fasl-input-stream*
    (let* ((len (fast-read-u-integer #.sb!vm:n-word-bytes))
           (size (fast-read-byte))
           (res (case size
                  (0 (make-array len :element-type 'nil))
                  (1 (make-array len :element-type 'bit))
                  (2 (make-array len :element-type '(unsigned-byte 2)))
                  (4 (make-array len :element-type '(unsigned-byte 4)))
                  (7 (prog1 (make-array len :element-type '(unsigned-byte 7))
                       (setf size 8)))
                  (8 (make-array len :element-type '(unsigned-byte 8)))
                  (15 (prog1 (make-array len :element-type '(unsigned-byte 15))
                        (setf size 16)))
                  (16 (make-array len :element-type '(unsigned-byte 16)))
                  (31 (prog1 (make-array len :element-type '(unsigned-byte 31))
                        (setf size 32)))
                  (32 (make-array len :element-type '(unsigned-byte 32)))
                  #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
                  (63 (prog1 (make-array len :element-type '(unsigned-byte 63))
                        (setf size 64)))
                  (64 (make-array len :element-type '(unsigned-byte 64)))
                  (t (bug "losing i-vector element size: ~S" size)))))
      (declare (type index len))
      (done-with-fast-read-byte)
      (read-n-bytes *fasl-input-stream*
                    res
                    0
                    (ceiling (the index (* size len)) sb!vm:n-byte-bits))
      res)))

;;; This is the same as FOP-INT-VECTOR, except this is for signed
;;; SIMPLE-ARRAYs.
(define-fop (fop-signed-int-vector 50)
  (prepare-for-fast-read-byte *fasl-input-stream*
    (let* ((len (fast-read-u-integer #.sb!vm:n-word-bytes))
           (size (fast-read-byte))
           (res (case size
                  (8 (make-array len :element-type '(signed-byte 8)))
                  (16 (make-array len :element-type '(signed-byte 16)))
                  #!+#.(cl:if (cl:= 32 sb!vm:n-word-bits) '(and) '(or))
                  (29 (prog1 (make-array len :element-type '(unsigned-byte 29))
                        (setf size 32)))
                  #!+#.(cl:if (cl:= 32 sb!vm:n-word-bits) '(and) '(or))
                  (30 (prog1 (make-array len :element-type '(signed-byte 30))
                        (setf size 32)))
                  (32 (make-array len :element-type '(signed-byte 32)))
                  #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
                  (60 (prog1 (make-array len :element-type '(unsigned-byte 60))
                        (setf size 64)))
                  #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
                  (61 (prog1 (make-array len :element-type '(signed-byte 61))
                        (setf size 64)))
                  #!+#.(cl:if (cl:= 64 sb!vm:n-word-bits) '(and) '(or))
                  (64 (make-array len :element-type '(signed-byte 64)))
                  (t (bug "losing si-vector element size: ~S" size)))))
      (declare (type index len))
      (done-with-fast-read-byte)
      (read-n-bytes *fasl-input-stream*
                    res
                    0
                    (ceiling (the index (* size len)) sb!vm:n-byte-bits))
      res)))

(define-fop (fop-eval 53)
  (if *skip-until*
      (pop-stack)
      (let ((result (eval (pop-stack))))
        ;; FIXME: CMU CL had this code here:
        ;;   (when *load-print*
        ;;     (load-fresh-line)
        ;;     (prin1 result)
        ;;     (terpri))
        ;; Unfortunately, this dependence on the *LOAD-PRINT* global
        ;; variable is non-ANSI, so for now we've just punted printing in
        ;; fasl loading.
        result)))

(define-fop (fop-eval-for-effect 54 :pushp nil)
  (if *skip-until*
      (pop-stack)
      (let ((result (eval (pop-stack))))
        ;; FIXME: See the comment about *LOAD-PRINT* in FOP-EVAL.
        (declare (ignore result))
        #+nil (when *load-print*
                (load-fresh-line)
                (prin1 result)
                (terpri)))))

(define-fop (fop-funcall 55)
  (let ((arg (read-byte-arg)))
    (if *skip-until*
        (dotimes (i (1+ arg))
          (pop-stack))
        (if (zerop arg)
            (funcall (pop-stack))
            (do ((args () (cons (pop-stack) args))
                 (n arg (1- n)))
                ((zerop n) (apply (pop-stack) args))
              (declare (type index n)))))))

(define-fop (fop-funcall-for-effect 56 :pushp nil)
  (let ((arg (read-byte-arg)))
    (if *skip-until*
        (dotimes (i (1+ arg))
          (pop-stack))
        (if (zerop arg)
            (funcall (pop-stack))
            (do ((args () (cons (pop-stack) args))
                 (n arg (1- n)))
                ((zerop n) (apply (pop-stack) args))
              (declare (type index n)))))))

;;;; fops for fixing up circularities

(define-fop (fop-rplaca 200 :pushp nil)
  (let ((obj (svref *current-fop-table* (read-word-arg)))
        (idx (read-word-arg))
        (val (pop-stack)))
    (setf (car (nthcdr idx obj)) val)))

(define-fop (fop-rplacd 201 :pushp nil)
  (let ((obj (svref *current-fop-table* (read-word-arg)))
        (idx (read-word-arg))
        (val (pop-stack)))
    (setf (cdr (nthcdr idx obj)) val)))

(define-fop (fop-svset 202 :pushp nil)
  (let* ((obi (read-word-arg))
         (obj (svref *current-fop-table* obi))
         (idx (read-word-arg))
         (val (pop-stack)))
    (if (%instancep obj)
        (setf (%instance-ref obj idx) val)
        (setf (svref obj idx) val))))

(define-fop (fop-structset 204 :pushp nil)
  (setf (%instance-ref (svref *current-fop-table* (read-word-arg))
                       (read-word-arg))
        (pop-stack)))

;;; In the original CMUCL code, this actually explicitly declared PUSHP
;;; to be T, even though that's what it defaults to in DEFINE-FOP.
(define-fop (fop-nthcdr 203)
  (nthcdr (read-word-arg) (pop-stack)))

;;;; fops for loading functions

;;; (In CMU CL there was a FOP-CODE-FORMAT (47) which was
;;; conventionally placed at the beginning of each fasl file to test
;;; for compatibility between the fasl file and the CMU CL which
;;; loaded it. In SBCL, this functionality has been replaced by
;;; putting the implementation and version in required fields in the
;;; fasl file header.)

(define-fop (fop-code 58 :stackp nil)
  (load-code (read-word-arg) (read-word-arg)))

(define-fop (fop-small-code 59 :stackp nil)
  (load-code (read-byte-arg) (read-halfword-arg)))

(define-fop (fop-fdefinition 60)
  (fdefinition-object (pop-stack) t))

(define-fop (fop-sanctify-for-execution 61)
  (let ((component (pop-stack)))
    (sb!vm:sanctify-for-execution component)
    component))

(define-fop (fop-fset 74 :pushp nil)
  ;; Ordinary, not-for-cold-load code shouldn't need to mess with this
  ;; at all, since it's only used as part of the conspiracy between
  ;; the cross-compiler and GENESIS to statically link FDEFINITIONs
  ;; for cold init.
  (warn "~@<FOP-FSET seen in ordinary load (not cold load) -- quite strange! ~
If you didn't do something strange to cause this, please report it as a ~
bug.~:@>")
  ;; Unlike CMU CL, we don't treat this as a no-op in ordinary code.
  ;; If the user (or, more likely, developer) is trying to reload
  ;; compiled-for-cold-load code into a warm SBCL, we'll do a warm
  ;; assignment. (This is partly for abstract tidiness, since the warm
  ;; assignment is the closest analogy to what happens at cold load,
  ;; and partly because otherwise our compiled-for-cold-load code will
  ;; fail, since in SBCL things like compiled-for-cold-load %DEFUN
  ;; depend more strongly than in CMU CL on FOP-FSET actually doing
  ;; something.)
  (let ((fn (pop-stack))
        (name (pop-stack)))
    (setf (fdefinition name) fn)))

(define-fop (fop-note-debug-source 174 :pushp nil)
  (warn "~@<FOP-NOTE-DEBUG-SOURCE seen in ordinary load (not cold load) -- ~
very strange!  If you didn't do something to cause this, please report it as ~
a bug.~@:>")
  ;; as with COLD-FSET above, we are going to be lenient with coming
  ;; across this fop in a warm SBCL.
  (let ((debug-source (pop-stack)))
    (setf (sb!c::debug-source-compiled debug-source) (get-universal-time)
          (sb!c::debug-source-created debug-source)
          (file-write-date (sb!c::debug-source-namestring debug-source)))))

;;; Modify a slot in a CONSTANTS object.
(define-cloned-fops (fop-alter-code 140 :pushp nil) (fop-byte-alter-code 141)
  (let ((value (pop-stack))
        (code (pop-stack)))
    (setf (code-header-ref code (clone-arg)) value)
    (values)))

(define-fop (fop-fun-entry 142)
  #+sb-xc-host ; since xc host doesn't know how to compile %PRIMITIVE
  (error "FOP-FUN-ENTRY can't be defined without %PRIMITIVE.")
  #-sb-xc-host
  (let ((info (pop-stack))
        (type (pop-stack))
        (arglist (pop-stack))
        (name (pop-stack))
        (code-object (pop-stack))
        (offset (read-word-arg)))
    (declare (type index offset))
    (unless (zerop (logand offset sb!vm:lowtag-mask))
      (bug "unaligned function object, offset = #X~X" offset))
    (let ((fun (%primitive sb!c:compute-fun code-object offset)))
      (setf (%simple-fun-self fun) fun)
      (setf (%simple-fun-next fun) (%code-entry-points code-object))
      (setf (%code-entry-points code-object) fun)
      (setf (%simple-fun-name fun) name)
      (setf (%simple-fun-arglist fun) arglist)
      (setf (%simple-fun-type fun) type)
      (setf (%simple-fun-info fun) info)
      ;; FIXME: See the comment about *LOAD-PRINT* in FOP-EVAL.
      #+nil (when *load-print*
              (load-fresh-line)
              (format t "~S defined~%" fun))
      fun)))

;;;; Some Dylan FOPs used to live here. By 1 November 1998 the code
;;;; was sufficiently stale that the functions it called were no
;;;; longer defined, so I (William Harold Newman) deleted it.
;;;;
;;;; In case someone in the future is trying to make sense of FOP layout,
;;;; it might be worth recording that the Dylan FOPs were
;;;;    100 FOP-DYLAN-SYMBOL-SAVE
;;;;    101 FOP-SMALL-DYLAN-SYMBOL-SAVE
;;;;    102 FOP-DYLAN-KEYWORD-SAVE
;;;;    103 FOP-SMALL-DYLAN-KEYWORD-SAVE
;;;;    104 FOP-DYLAN-VARINFO-VALUE

;;;; assemblerish fops

(define-fop (fop-assembler-code 144)
  (error "cannot load assembler code except at cold load"))

(define-fop (fop-assembler-routine 145)
  (error "cannot load assembler code except at cold load"))

(define-fop (fop-foreign-fixup 147)
  (let* ((kind (pop-stack))
         (code-object (pop-stack))
         (len (read-byte-arg))
         (sym (make-string len :element-type 'base-char)))
    (read-n-bytes *fasl-input-stream* sym 0 len)
    (sb!vm:fixup-code-object code-object
                             (read-word-arg)
                             (foreign-symbol-address sym)
                             kind)
    code-object))

(define-fop (fop-assembler-fixup 148)
  (let ((routine (pop-stack))
        (kind (pop-stack))
        (code-object (pop-stack)))
    (multiple-value-bind (value found) (gethash routine *assembler-routines*)
      (unless found
        (error "undefined assembler routine: ~S" routine))
      (sb!vm:fixup-code-object code-object (read-word-arg) value kind))
    code-object))

(define-fop (fop-code-object-fixup 149)
  (let ((kind (pop-stack))
        (code-object (pop-stack)))
    ;; Note: We don't have to worry about GC moving the code-object after
    ;; the GET-LISP-OBJ-ADDRESS and before that value is deposited, because
    ;; we can only use code-object fixups when code-objects don't move.
    (sb!vm:fixup-code-object code-object (read-word-arg)
                             (get-lisp-obj-address code-object) kind)
    code-object))

#!+linkage-table
(define-fop (fop-foreign-dataref-fixup 150)
  (let* ((kind (pop-stack))
         (code-object (pop-stack))
         (len (read-byte-arg))
         (sym (make-string len :element-type 'base-char)))
    (read-n-bytes *fasl-input-stream* sym 0 len)
    (sb!vm:fixup-code-object code-object
                             (read-word-arg)
                             (foreign-symbol-address sym t)
                             kind)
    code-object))

;;; FOPs needed for implementing an IF operator in a FASL

;;; Skip until a FOP-MAYBE-STOP-SKIPPING with the same POSITION is
;;; executed. While skipping, we execute most FOPs normally, except
;;; for ones that a) funcall/eval b) start skipping. This needs to
;;; be done to ensure that the fop table gets populated correctly
;;; regardless of the execution path.
(define-fop (fop-skip 151 :pushp nil)
  (let ((position (pop-stack)))
    (unless *skip-until*
      (setf *skip-until* position)))
  (values))

;;; As before, but only start skipping if the top of the FOP stack is NIL.
(define-fop (fop-skip-if-false 152 :pushp nil)
  (let ((condition (pop-stack))
        (position (pop-stack)))
    (unless (or condition
                *skip-until*)
      (setf *skip-until* position)))
  (values))

;;; If skipping, pop the top of the stack and discard it. Needed for
;;; ensuring that the stack stays balanced when skipping.
(define-fop (fop-drop-if-skipping 153 :pushp nil)
  (when *skip-until*
    (pop-stack))
  (values))

;;; If skipping, push a dummy value on the stack. Needed for
;;; ensuring that the stack stays balanced when skipping.
(define-fop (fop-push-nil-if-skipping 154 :pushp nil)
  (when *skip-until*
    (push-stack nil))
  (values))

;;; Stop skipping if the top of the stack matches *SKIP-UNTIL*
(define-fop (fop-maybe-stop-skipping 155 :pushp nil)
  (let ((label (pop-stack)))
    (when (eql *skip-until* label)
      (setf *skip-until* nil)))
  (values))
