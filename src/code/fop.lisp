;;;; FOP definitions

(in-package "SB!FASL")

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
;;;	(cond ((and (< num-consts #x100) (< total-length #x10000))
;;;	       (dump-fop 'sb!impl::fop-small-code file)
;;;	       (dump-byte num-consts file)
;;;	       (dump-integer-as-n-bytes total-length 2 file))
;;;	      (t
;;;	       (dump-fop 'sb!impl::fop-code file)
;;;	       (dump-unsigned-32 num-consts file)
;;;	       (dump-unsigned-32 total-length file))))
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
     (macrolet ((clone-arg () '(read-arg 4)))
       (define-fop (,name ,code :pushp ,pushp :stackp ,stackp) ,@forms))
     (macrolet ((clone-arg () '(read-arg 1)))
       (define-fop (,small-name ,small-code :pushp ,pushp :stackp stackp) ,@forms))))

;;; a helper function for reading string values from FASL files: sort
;;; of like READ-SEQUENCE specialized for files of (UNSIGNED-BYTE 8),
;;; with an automatic conversion from (UNSIGNED-BYTE 8) into CHARACTER
;;; for each element read
(declaim (ftype (function (stream simple-string &optional index) (values)) read-string-as-bytes))
(defun read-string-as-bytes (stream string &optional (length (length string)))
  (dotimes (i length)
    (setf (aref string i)
	  (code-char (read-byte stream))))
  ;; FIXME: The classic CMU CL code to do this was
  ;;   (READ-N-BYTES FILE STRING START END).
  ;; It was changed for SBCL because we needed a portable version for
  ;; bootstrapping. Benchmark the non-portable version and see whether it's
  ;; significantly better than the portable version here. If it is, then use
  ;; add as an alternate definition, protected with #-SB-XC-HOST.
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
(define-fop (fop-push 2) (svref *current-fop-table* (read-arg 4)))
(define-fop (fop-byte-push 3) (svref *current-fop-table* (read-arg 1)))

(define-fop (fop-empty-list 4) ())
(define-fop (fop-truth 5) t)
;;; CMU CL had FOP-POP-FOR-EFFECT as fop 65, but it was never used and seemed
;;; to have no possible use.
(define-fop (fop-misc-trap 66)
  #+sb-xc-host ; since xc host doesn't know how to compile %PRIMITIVE
  (error "FOP-MISC-TRAP can't be defined without %PRIMITIVE.")
  #-sb-xc-host
  (%primitive sb!c:make-other-immediate-type 0 sb!vm:unbound-marker-widetag))

;;; CMU CL had FOP-CHARACTER as fop 68, but it's not needed in current
;;; SBCL as we have no extended characters, only 1-byte characters.
;;; (Ditto for CMU CL, actually: FOP-CHARACTER was speculative generality.)
(define-fop (fop-short-character 69)
  (code-char (read-arg 1)))

(define-cloned-fops (fop-struct 48) (fop-small-struct 49)
  (let* ((size (clone-arg))
	 (res (%make-instance size)))
    (declare (type index size))
    (do ((n (1- size) (1- n)))
	((minusp n))
      (declare (type index-or-minus-1 n))
      (setf (%instance-ref res n) (pop-stack)))
    res))

(define-fop (fop-layout 45)
  (let ((length (pop-stack))
	(depthoid (pop-stack))
	(inherits (pop-stack))
	(name (pop-stack)))
    (find-and-init-or-check-layout name length inherits depthoid)))

(define-fop (fop-end-group 64 :stackp nil)
  (/show0 "THROWing FASL-GROUP-END")
  (throw 'fasl-group-end t))

;;; In the normal loader, we just ignore these. GENESIS overwrites
;;; FOP-MAYBE-COLD-LOAD with something that knows whether to revert to
;;; cold-loading or not.
(define-fop (fop-normal-load 81 :stackp nil))
(define-fop (fop-maybe-cold-load 82 :stackp nil))

(define-fop (fop-verify-table-size 62 :stackp nil)
  (let ((expected-index (read-arg 4)))
    (unless (= *current-fop-table-index* expected-index)
      (bug "fasl table of improper size"))))
(define-fop (fop-verify-empty-stack 63 :stackp nil)
  (unless (zerop (length *fop-stack*))
    (bug "fasl stack not empty when it should be")))

;;;; fops for loading symbols

(defvar *load-symbol-buffer* (make-string 100))
(declaim (simple-string *load-symbol-buffer*))
(defvar *load-symbol-buffer-size* 100)
(declaim (type index *load-symbol-buffer-size*))
;;; FIXME:
;;;   (1) *LOAD-SYMBOL-BUFFER-SIZE* is redundant, should just be
;;;       (LENGTH *LOAD-SYMBOL-BUFFER*).
;;;   (2) *LOAD-SYMBOL-BUFFER* should not have a global value, but should
;;;       be bound on entry to FASL loading, and it should be renamed to
;;;       *FASL-SYMBOL-BUFFER*.

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
		      (when (> ,n-size *load-symbol-buffer-size*)
			(setq *load-symbol-buffer*
			      (make-string (setq *load-symbol-buffer-size*
						 (* ,n-size 2)))))
		      (done-with-fast-read-byte)
		      (let ((,n-buffer *load-symbol-buffer*))
			(read-string-as-bytes *fasl-input-stream*
					      ,n-buffer
					      ,n-size)
			(push-fop-table (intern* ,n-buffer
						 ,n-size
						 ,n-package)))))))))

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
  ;;(frob fop-symbol-save		6 4 *package*)
  ;;(frob fop-small-symbol-save	  7 1 *package*)

  (frob fop-lisp-symbol-save	      75 4 *cl-package*)
  (frob fop-lisp-small-symbol-save    76 1 *cl-package*)
  (frob fop-keyword-symbol-save       77 4 *keyword-package*)
  (frob fop-keyword-small-symbol-save 78 1 *keyword-package*)

  ;; FIXME: Because we don't have FOP-SYMBOL-SAVE any more, an enormous number
  ;; of symbols will fall through to this case, probably resulting in bloated
  ;; fasl files. A new
  ;; FOP-SYMBOL-IN-LAST-PACKAGE-SAVE/FOP-SMALL-SYMBOL-IN-LAST-PACKAGE-SAVE
  ;; cloned fop pair could undo some of this bloat.
  (frob fop-symbol-in-package-save 8 4
    (svref *current-fop-table* (fast-read-u-integer 4)))
  (frob fop-small-symbol-in-package-save 9 1
    (svref *current-fop-table* (fast-read-u-integer 4)))
  (frob fop-symbol-in-byte-package-save 10 4
    (svref *current-fop-table* (fast-read-u-integer 1)))
  (frob fop-small-symbol-in-byte-package-save 11 1
    (svref *current-fop-table* (fast-read-u-integer 1))))

(define-cloned-fops (fop-uninterned-symbol-save 12)
		    (fop-uninterned-small-symbol-save 13)
  (let* ((arg (clone-arg))
	 (res (make-string arg)))
    (read-string-as-bytes *fasl-input-stream* res)
    (push-fop-table (make-symbol res))))

(define-fop (fop-package 14)
  (find-undeleted-package-or-lose (pop-stack)))

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
	(if (logbitp 7 byte)	; look at sign bit
	    (- result (ash 1 bits))
	    result))
    (declare (fixnum index byte bits))))

(define-cloned-fops (fop-integer 33) (fop-small-integer 34)
  (load-s-integer (clone-arg)))

(define-fop (fop-word-integer 35)
  (prepare-for-fast-read-byte *fasl-input-stream*
    (prog1
     (fast-read-s-integer 4)
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
               (make-double-float (fast-read-s-integer 4) lo)))
           #!+long-float
           (fast-read-long-float ()
             '(let ((lo (fast-read-u-integer 4))
                    #!+sparc (mid (fast-read-u-integer 4))
                    (hi (fast-read-u-integer 4)) ; XXX
                    (exp (fast-read-s-integer #!+x86 2 #!+sparc 4)))
               (make-long-float exp hi #!+sparc mid lo))))
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
       (n (read-arg 1) (1- n)))
      ((zerop n) res)
    (declare (type index n))))

(define-fop (fop-list* 16)
  (do ((res (pop-stack) (cons (pop-stack) res))
       (n (read-arg 1) (1- n)))
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

(define-cloned-fops (fop-string 37) (fop-small-string 38)
  (let* ((arg (clone-arg))
	 (res (make-string arg)))
    (read-string-as-bytes *fasl-input-stream* res)
    res))

(define-cloned-fops (fop-vector 39) (fop-small-vector 40)
  (let* ((size (clone-arg))
	 (res (make-array size)))
    (declare (fixnum size))
    (do ((n (1- size) (1- n)))
	((minusp n))
      (setf (svref res n) (pop-stack)))
    res))

(define-fop (fop-array 83)
  (let* ((rank (read-arg 4))
	 (vec (pop-stack))
	 (length (length vec))
	 (res (make-array-header sb!vm:simple-array-widetag rank)))
    (declare (simple-array vec)
	     (type (unsigned-byte 24) rank))
    (set-array-header res vec length nil 0
		      (do ((i rank (1- i))
			   (dimensions () (cons (pop-stack) dimensions)))
			  ((zerop i) dimensions)
			(declare (type index i)))
		      nil)
    res))

(define-fop (fop-single-float-vector 84)
  (let* ((length (read-arg 4))
	 (result (make-array length :element-type 'single-float)))
    (read-n-bytes *fasl-input-stream* result 0 (* length sb!vm:n-word-bytes))
    result))

(define-fop (fop-double-float-vector 85)
  (let* ((length (read-arg 4))
	 (result (make-array length :element-type 'double-float)))
    (read-n-bytes *fasl-input-stream* result 0 (* length sb!vm:n-word-bytes 2))
    result))

#!+long-float
(define-fop (fop-long-float-vector 88)
  (let* ((length (read-arg 4))
	 (result (make-array length :element-type 'long-float)))
    (read-n-bytes *fasl-input-stream*
		  result
		  0
		  (* length sb!vm:n-word-bytes #!+x86 3 #!+sparc 4))
    result))

(define-fop (fop-complex-single-float-vector 86)
  (let* ((length (read-arg 4))
	 (result (make-array length :element-type '(complex single-float))))
    (read-n-bytes *fasl-input-stream* result 0 (* length sb!vm:n-word-bytes 2))
    result))

(define-fop (fop-complex-double-float-vector 87)
  (let* ((length (read-arg 4))
	 (result (make-array length :element-type '(complex double-float))))
    (read-n-bytes *fasl-input-stream*
		  result
		  0
		  (* length sb!vm:n-word-bytes 2 2))
    result))

#!+long-float
(define-fop (fop-complex-long-float-vector 89)
  (let* ((length (read-arg 4))
	 (result (make-array length :element-type '(complex long-float))))
    (read-n-bytes *fasl-input-stream* result 0
		  (* length sb!vm:n-word-bytes #!+x86 3 #!+sparc 4 2))
    result))

;;; CMU CL comment:
;;;   *** NOT *** the FOP-INT-VECTOR as currently documented in rtguts.
;;;   Size must be a directly supported I-vector element size, with no
;;;   extra bits. This must be packed according to the local
;;;   byte-ordering, allowing us to directly read the bits.
(define-fop (fop-int-vector 43)
  (prepare-for-fast-read-byte *fasl-input-stream*
    (let* ((len (fast-read-u-integer 4))
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
		  (t (bug "losing i-vector element size: ~S" size)))))
      (declare (type index len))
      (done-with-fast-read-byte)
      (read-n-bytes *fasl-input-stream*
		    res
		    0
		    (ceiling (the index (* size len))
			     sb!vm:n-byte-bits))
      res)))

;;; This is the same as FOP-INT-VECTOR, except this is for signed
;;; SIMPLE-ARRAYs.
(define-fop (fop-signed-int-vector 50)
  (prepare-for-fast-read-byte *fasl-input-stream*
    (let* ((len (fast-read-u-integer 4))
	   (size (fast-read-byte))
	   (res (case size
 		  (8 (make-array len :element-type '(signed-byte 8)))
 		  (16 (make-array len :element-type '(signed-byte 16)))
		  (29 (make-array len :element-type '(unsigned-byte 29)))
 		  (30 (make-array len :element-type '(signed-byte 30)))
 		  (32 (make-array len :element-type '(signed-byte 32)))
 		  (t (bug "losing si-vector element size: ~S" size)))))
      (declare (type index len))
      (done-with-fast-read-byte)
      (read-n-bytes *fasl-input-stream*
		    res
		    0
 		    (ceiling (the index (* (if (or (= size 30) (= size 29))
					       32 ; Adjust for (signed-byte 30)
					       size) len)) sb!vm:n-byte-bits))
      res)))

(define-fop (fop-eval 53)
  (let ((result (eval (pop-stack))))
    ;; FIXME: CMU CL had this code here:
    ;;   (when *load-print*
    ;;     (load-fresh-line)
    ;;     (prin1 result)
    ;;     (terpri))
    ;; Unfortunately, this dependence on the *LOAD-PRINT* global
    ;; variable is non-ANSI, so for now we've just punted printing in
    ;; fasl loading.
    result))

(define-fop (fop-eval-for-effect 54 :pushp nil)
  (let ((result (eval (pop-stack))))
    ;; FIXME: See the comment about *LOAD-PRINT* in FOP-EVAL.
    (declare (ignore result))
    #+nil (when *load-print*
	    (load-fresh-line)
	    (prin1 result)
	    (terpri))))

(define-fop (fop-funcall 55)
  (let ((arg (read-arg 1)))
    (if (zerop arg)
	(funcall (pop-stack))
	(do ((args () (cons (pop-stack) args))
	     (n arg (1- n)))
	    ((zerop n) (apply (pop-stack) args))
	  (declare (type index n))))))

(define-fop (fop-funcall-for-effect 56 :pushp nil)
  (let ((arg (read-arg 1)))
    (if (zerop arg)
	(funcall (pop-stack))
	(do ((args () (cons (pop-stack) args))
	     (n arg (1- n)))
	    ((zerop n) (apply (pop-stack) args))
	  (declare (type index n))))))

;;;; fops for fixing up circularities

(define-fop (fop-rplaca 200 :pushp nil)
  (let ((obj (svref *current-fop-table* (read-arg 4)))
	(idx (read-arg 4))
	(val (pop-stack)))
    (setf (car (nthcdr idx obj)) val)))

(define-fop (fop-rplacd 201 :pushp nil)
  (let ((obj (svref *current-fop-table* (read-arg 4)))
	(idx (read-arg 4))
	(val (pop-stack)))
    (setf (cdr (nthcdr idx obj)) val)))

(define-fop (fop-svset 202 :pushp nil)
  (let* ((obi (read-arg 4))
	 (obj (svref *current-fop-table* obi))
	 (idx (read-arg 4))
	 (val (pop-stack)))
    (if (typep obj 'instance)
	(setf (%instance-ref obj idx) val)
	(setf (svref obj idx) val))))

(define-fop (fop-structset 204 :pushp nil)
  (setf (%instance-ref (svref *current-fop-table* (read-arg 4))
		       (read-arg 4))
	(pop-stack)))

;;; In the original CMUCL code, this actually explicitly declared PUSHP
;;; to be T, even though that's what it defaults to in DEFINE-FOP.
(define-fop (fop-nthcdr 203)
  (nthcdr (read-arg 4) (pop-stack)))

;;;; fops for loading functions

;;; (In CMU CL there was a FOP-CODE-FORMAT (47) which was
;;; conventionally placed at the beginning of each fasl file to test
;;; for compatibility between the fasl file and the CMU CL which
;;; loaded it. In SBCL, this functionality has been replaced by
;;; putting the implementation and version in required fields in the
;;; fasl file header.)

(define-fop (fop-code 58 :stackp nil)
  (load-code (read-arg 4) (read-arg 4)))

(define-fop (fop-small-code 59 :stackp nil)
  (load-code (read-arg 1) (read-arg 2)))

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
  (let ((type (pop-stack))
	(arglist (pop-stack))
	(name (pop-stack))
	(code-object (pop-stack))
	(offset (read-arg 4)))
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

(define-fop (fop-foreign-fixup 147)
  (let* ((kind (pop-stack))
	 (code-object (pop-stack))
	 (len (read-arg 1))
	 (sym (make-string len)))
    (read-n-bytes *fasl-input-stream* sym 0 len)
    (sb!vm:fixup-code-object code-object
			     (read-arg 4)
			     (foreign-symbol-address-as-integer sym)
			     kind)
    code-object))

(define-fop (fop-assembler-code 144)
  (error "cannot load assembler code except at cold load"))

(define-fop (fop-assembler-routine 145)
  (error "cannot load assembler code except at cold load"))

(define-fop (fop-assembler-fixup 148)
  (let ((routine (pop-stack))
	(kind (pop-stack))
	(code-object (pop-stack)))
    (multiple-value-bind (value found) (gethash routine *assembler-routines*)
      (unless found
	(error "undefined assembler routine: ~S" routine))
      (sb!vm:fixup-code-object code-object (read-arg 4) value kind))
    code-object))

(define-fop (fop-code-object-fixup 149)
  (let ((kind (pop-stack))
	(code-object (pop-stack)))
    ;; Note: We don't have to worry about GC moving the code-object after
    ;; the GET-LISP-OBJ-ADDRESS and before that value is deposited, because
    ;; we can only use code-object fixups when code-objects don't move.
    (sb!vm:fixup-code-object code-object (read-arg 4)
			     (get-lisp-obj-address code-object) kind)
    code-object))
