;;;; Utilities for separating an SBCL core file into two pieces:
;;;; 1. An assembly language file containing the immobile code space
;;;; 2. A '.o' file wrapping a core file containing everything else
;;;; We operate as a "tool" that processes external files rather than
;;;; operating on the in-process data, but it is also possible to dump
;;;; the current image by creating a straight-through translation
;;;; of internal/external code addresses.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(load (merge-pathnames "corefile.lisp" *load-pathname*))

(defpackage "SB-EDITCORE"
  (:use "CL" "SB-ALIEN" "SB-COREFILE" "SB-INT" "SB-EXT"
        "SB-KERNEL" "SB-SYS" "SB-VM")
  (:export #:move-dynamic-code-to-text-space #:redirect-text-space-calls
           #:split-core #:copy-to-elf-obj #:reorganize-core)
  (:import-from "SB-ALIEN-INTERNALS"
                #:alien-type-bits #:parse-alien-type
                #:alien-value-sap #:alien-value-type)
  (:import-from "SB-C" #:+backend-page-bytes+)
  (:import-from "SB-VM" #:map-objects-in-range #:reconstitute-object
                #:%closure-callee #:code-object-size)
  (:import-from "SB-DISASSEM" #:get-inst-space #:find-inst
                #:make-dstate #:%make-segment #:make-code-segment
                #:seg-virtual-location #:seg-length #:seg-sap-maker
                #:map-segment-instructions #:inst-name
                #:dstate-next-addr #:dstate-cur-offs
                #:dstate-cur-addr #:sign-extend)
  #+x86-64
  (:import-from "SB-X86-64-ASM" #:near-jump-displacement
                #:near-cond-jump-displacement #:mov #:call #:jmp
                #:get-gpr #:reg-name
                #:machine-ea #:machine-ea-base #:machine-ea-index #:machine-ea-disp)
  (:import-from "SB-IMPL" #:symbol-table #:package-%name
                #:symtbl-%cells
                #:hash-table-pairs #:hash-table-%count))

(in-package "SB-EDITCORE")

(declaim (muffle-conditions compiler-note))

(eval-when (:compile-toplevel :execute)
  (when (member :immobile-space sb-impl:+internal-features+)
    (pushnew :immobile-space *features*)))
(eval-when (:execute)
  (setq *evaluator-mode* :compile))

;;; Some high address that won't conflict with any of the ordinary spaces
;;; It's more-or-less arbitrary, but we must be able to discern whether a
;;; pointer looks like it points to code in case coreparse has to walk the heap.
(defconstant +code-space-nominal-address+ #x550000000000)

(defstruct (core-space ; "space" is a CL symbol
            (:conc-name space-)
            (:constructor make-space (id addr data-page page-adjust nwords)))
  (page-table nil :type (or null simple-vector))
  id addr data-page page-adjust nwords)
(defmethod print-object ((self core-space) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~d" (space-id self))))
(defun space-size (space) (* (space-nwords space) n-word-bytes))
(defun space-end (space) (+  (space-addr space) (space-size space)))
(defun space-nbytes-aligned (space)
  (align-up (space-size space) +backend-page-bytes+))
(defun space-physaddr (space spacemap)
  (sap+ (car spacemap) (* (space-data-page space) +backend-page-bytes+)))

;;; Given VADDR which is an address in the target core, return the address at which
;;; VADDR is currently mapped while performing the split.
;;; SPACEMAP is a cons of a SAP and an alist whose elements are (ADDR . CORE-SPACE)
(defun translate-ptr (vaddr spacemap)
  (let ((space (find vaddr (cdr spacemap) :key #'space-addr :test #'>=)))
    ;; FIXME: duplicates SPACE-PHYSADDR to avoid consing a SAP.
    ;; macroize or something.
    (+ (sap-int (car spacemap)) (* (space-data-page space) +backend-page-bytes+)
       (- vaddr (space-addr space)))))

;;;
(defun get-space (id spacemap)
  (find id (cdr spacemap) :key #'space-id))
(defun compute-nil-addr (spacemap)
  (let ((space (get-space static-core-space-id spacemap)))
    ;; TODO: The core should store its address of NIL in the initial function entry
    ;; so this kludge can be removed.
    (logior (space-addr space) #x117))) ; SUPER KLUDGE
(defun compute-nil-object (spacemap) ; terrible, don't use!
  (%make-lisp-obj (compute-nil-addr spacemap)))

;;; Given OBJ which is tagged pointer into the target core, translate it into
;;; the range at which the core is now mapped during execution of this tool,
;;; so that host accessors can dereference its slots.
;;; Use extreme care: while it works to use host accessors on the target core,
;;; we must avoid type checks on instances because LAYOUTs need translation.
;;; Printing boxed objects from the target core will almost always crash.
(defun translate (obj spacemap)
  (%make-lisp-obj (translate-ptr (get-lisp-obj-address obj) spacemap)))

(defstruct (core-sym (:copier nil) (:predicate nil)
                     (:constructor make-core-sym (package name external)))
  (package nil)
  (name nil :read-only t)
  (external nil :read-only t))

(defstruct (bounds (:constructor make-bounds (low high)))
  (low 0 :type word) (high 0 :type word))

(defstruct (core (:predicate nil)
                 (:copier nil)
                 (:constructor %make-core))
  (spacemap)
  (nil-object)
  ;; mapping from small integer ID to package
  (pkg-id->package)
  ;; mapping from string naming a package to list of symbol names (strings)
  ;; that are external in the package.
  (packages (make-hash-table :test 'equal))
  ;; hashset of symbol names (as strings) that should be package-qualified.
  ;; (Prefer not to package-qualify unambiguous names)
  (nonunique-symbol-names)
  (code-bounds nil :type bounds :read-only t)
  (fixedobj-bounds nil :type bounds :read-only t)
  (linkage-bounds nil :type bounds :read-only t)
  (linkage-space-info)
  (alien-linkage-symbols nil)
  (alien-linkage-symbol-usedp nil)
  (alien-linkage-entry-size nil)
  (new-fixups (make-hash-table))
  (new-fixup-words-used 0)
  ;; For assembler labels that we want to invent at random
  (label-counter 0)
  (enable-pie nil)
  (dstate (make-dstate nil) :read-only t)
  (seg (%make-segment :sap-maker (lambda () (error "Bad sap maker"))
                      :virtual-location 0) :read-only t)
  (fixup-addrs nil)
  (call-inst nil :read-only t)
  (jmp-inst nil :read-only t)
  (pop-inst nil :read-only t))

(defglobal *editcore-ppd*
  ;; copy no entries for macros/special-operators (flet, etc)
  (let ((ppd (sb-pretty::make-pprint-dispatch-table #() nil nil)))
    (set-pprint-dispatch 'string
                         ;; Write strings without string quotes
                         (lambda (stream string) (write-string string stream))
                         0
                         ppd)
    ppd))

(defmethod print-object ((sym core-sym) stream)
  (format stream "~(~:[~*~;~:*~A~:[:~;~]:~]~A~)"
          (core-sym-package sym)
          (core-sym-external sym)
          (core-sym-name sym)))

(defun space-bounds (id spacemap)
  (let ((space (get-space id spacemap)))
    (if space
        (make-bounds (space-addr space) (space-end space))
        (make-bounds 0 0))))
(defun in-bounds-p (addr bounds)
  (and (>= addr (bounds-low bounds)) (< addr (bounds-high bounds))))

(defun make-string-hashset (contents count)
  (let ((hs (sb-int:make-hashset count #'string= #'sxhash)))
    (dolist (string contents hs)
      (sb-int:hashset-insert hs string))))

(defun scan-symbol-table (function table core)
  (let* ((spacemap (core-spacemap core))
         (nil-object (core-nil-object core))
         (cells (translate (symtbl-%cells (truly-the symbol-table
                                                     (translate table spacemap)))
                           spacemap)))
    (dovector (x (translate (cdr cells) spacemap))
      (unless (fixnump x)
        (funcall function
                 (if (eq x nil-object) ; any random package can export NIL. wow.
                     "NIL"
                     (translate (symbol-name (translate x spacemap)) spacemap))
                 x)))))

(defun core-package-from-id (id core)
  (if (/= id 0)
      (let ((package (aref (core-pkg-id->package core) id)))
        (translate (package-%name (truly-the package package))
                   (core-spacemap core)))))

(defun remove-name-junk (name)
  (setq name
        (named-let recurse ((x name))
          (cond ((typep x '(cons (eql lambda)))
                 (let ((args (second x)))
                   `(lambda ,(if args #\# "()")
                      ,@(recurse (cddr x)))))
                ((eq x :in) "in")
                ((and (typep x '(or string symbol))
                      (let ((mismatch (mismatch (string x) "CLEANUP-FUN-")))
                        (or (eql mismatch nil) (= mismatch (length "CLEANUP-FUN-")))))
                 '#:cleanup-fun)
                ;; Try to chop off all the directory names in strings resembling
                ;; (lambda () in "/some/very/long/pathname/to/a/thing.lisp")
                ((stringp x)
                 (let ((p (position #\/ x :from-end t)))
                   (if p (subseq x (1+ p)) x)))
                ((consp x) (recons x (recurse (car x)) (recurse (cdr x))))
                (t x))))
  ;; Shorten obnoxiously long printed representations of methods.
  (flet ((unpackageize (thing)
           (when (typep thing 'core-sym)
             (setf (core-sym-package thing) nil))
           thing))
    (when (typep name '(cons (member sb-pcl::slow-method sb-pcl::fast-method
                                     sb-pcl::slot-accessor)))
      (setq name `(,(case (car name)
                      (sb-pcl::fast-method "method")
                      (sb-pcl::slow-method "Method") ; something visually distinct
                      (sb-pcl::slot-accessor "accessor"))
                   ,@(cdr name)))
      (setf (second name) (unpackageize (second name)))
      (let ((last (car (last name))))
        (when (listp last)
          (dolist (qual last)
            (unpackageize qual))))))
  name)

(defstruct (descriptor (:constructor make-descriptor (bits)))
  (bits 0 :type word))
(defmethod print-object ((self descriptor) stream)
  (format stream "#<ptr ~x>" (descriptor-bits self)))
(defun descriptorize (obj)
  (if (is-lisp-pointer (get-lisp-obj-address obj))
      (make-descriptor (get-lisp-obj-address obj))
      obj))
(defun undescriptorize (target-descriptor)
  (%make-lisp-obj (descriptor-bits target-descriptor)))

(defun target-hash-table-alist (table spacemap)
  (let ((table (truly-the hash-table (translate table spacemap))))
    (let ((cells (the simple-vector (translate (hash-table-pairs table) spacemap))))
      (collect ((pairs))
        (do ((count (hash-table-%count table) (1- count))
             (i 2 (+ i 2)))
            ((zerop count)
             (pairs))
          (pairs (cons (descriptorize (svref cells i))
                       (descriptorize (svref cells (1+ i))))))))))

(defmacro package-id (name) (sb-impl::package-id (find-package name)))

;;; Return either the physical or logical address of the specified symbol.
(defun %find-target-symbol (package-id symbol-name spacemap
                            &optional (address-mode :physical))
  (dolist (id `(,immobile-fixedobj-core-space-id
                ,static-core-space-id
                ,dynamic-core-space-id))
    (binding* ((space (get-space id spacemap) :exit-if-null)
               (start (translate-ptr (space-addr space) spacemap))
               (end (+ start (space-size space)))
               (physaddr start))
     (loop
       (when (>= physaddr end) (return))
       (let* ((word (sap-ref-word (int-sap physaddr) 0))
              (size
               (if (= (logand word widetag-mask) filler-widetag)
                   (ash (ash word -32) word-shift)
                   (let ((obj (reconstitute-object (ash physaddr (- n-fixnum-tag-bits)))))
                     (when (and (symbolp obj)
                                (string= symbol-name (translate (symbol-name obj) spacemap))
                                (= (symbol-package-id obj) package-id))
                       (return-from %find-target-symbol
                         (%make-lisp-obj
                          (logior (ecase address-mode
                                    (:physical physaddr)
                                    (:logical (+ (space-addr space) (- physaddr start))))
                                  other-pointer-lowtag))))
                     (primitive-object-size obj)))))
         (incf physaddr size))))))
(defun find-target-symbol (package-id symbol-name spacemap &optional (address-mode :physical))
  (or (%find-target-symbol package-id symbol-name spacemap address-mode)
      (bug "Can't find symbol ~A::~A" package-id symbol-name)))

(defparameter label-prefix (if (member :darwin *features*) "_" ""))
(defun labelize (x) (concatenate 'string label-prefix x))

(defun compute-alien-linkage-symbols (spacemap)
  (let* ((alien-linkage-info
          (symbol-global-value
           (find-target-symbol (package-id "SB-SYS") "*LINKAGE-INFO*"
                               spacemap :physical)))
         (hashtable (car (translate alien-linkage-info spacemap)))
         (pairs (target-hash-table-alist hashtable spacemap))
         (min (reduce #'min pairs :key #'cdr))
         (max (reduce #'max pairs :key #'cdr))
         (n (1+ (- max min)))
         (vector (make-array n)))
    (dolist (entry pairs vector)
      (let* ((key (undescriptorize (car entry)))
             (entry-index (- (cdr entry) min))
             (string (labelize (translate (if (consp key) (car (translate key spacemap)) key)
                                          spacemap))))
        (setf (aref vector entry-index)
              (if (consp key) (list string) string))))))

(defun make-core (spacemap code-bounds fixedobj-bounds &key enable-pie linkage-space-info)
  (let* ((linkage-bounds
          (let ((text-space (get-space immobile-text-core-space-id spacemap)))
            (if text-space
                (let ((linkage-spaces-size
                       (+ #+linkage-space (ash 1 (+ n-linkage-index-bits word-shift))
                          alien-linkage-space-size))
                      (text-addr (space-addr text-space)))
                  (make-bounds (- text-addr linkage-spaces-size) text-addr))
                (make-bounds 0 0))))
         (alien-linkage-entry-size
          (symbol-global-value
           (find-target-symbol (package-id "SB-VM") "ALIEN-LINKAGE-TABLE-ENTRY-SIZE"
                               spacemap :physical)))
         (alien-linkage-symbols (compute-alien-linkage-symbols spacemap))
         (nil-object (compute-nil-object spacemap))
         (ambiguous-symbols (make-hash-table :test 'equal))
         (core
          (%make-core
           :spacemap spacemap
           :nil-object nil-object
           :nonunique-symbol-names ambiguous-symbols
           :code-bounds code-bounds
           :fixedobj-bounds fixedobj-bounds
           :linkage-bounds linkage-bounds
           :alien-linkage-entry-size alien-linkage-entry-size
           :alien-linkage-symbols alien-linkage-symbols
           :alien-linkage-symbol-usedp (make-array (length alien-linkage-symbols)
                                                   :element-type 'bit
                                                   :initial-element 0)
           :linkage-space-info linkage-space-info
           :enable-pie enable-pie)))
    (let ((package-table
           (symbol-global-value
            (find-target-symbol (package-id "SB-IMPL") "*ALL-PACKAGES*" spacemap :physical)))
          (package-alist)
          (symbols (make-hash-table :test 'equal)))
      (labels ((scan-symtbl (table)
                 (scan-symbol-table
                    (lambda (str sym)
                      (pushnew (get-lisp-obj-address sym) (gethash str symbols)))
                    table core))
               (scan-package (x)
                 (let ((package (truly-the package (translate x spacemap))))
                   ;; a package can appear in *ALL-PACKAGES* under each of its nicknames
                   (unless (assoc (sb-impl::package-id package) package-alist)
                     (push (cons (sb-impl::package-id package) package) package-alist)
                     (scan-symtbl (package-external-symbols package))
                     (scan-symtbl (package-internal-symbols package))))))
        (dovector (x (translate package-table spacemap))
          (cond ((%instancep x) (scan-package x))
                ((listp x) (loop (if (eq x nil-object) (return))
                                 (setq x (translate x spacemap))
                                 (scan-package (car x))
                                 (setq x (cdr x)))))))
      (let ((package-by-id (make-array (1+ (reduce #'max package-alist :key #'car))
                                       :initial-element nil)))
        (loop for (id . package) in package-alist
              do (setf (aref package-by-id id) package))
        (setf (core-pkg-id->package core) package-by-id))
      (dohash ((string symbols) symbols)
        (when (cdr symbols)
          (setf (gethash string ambiguous-symbols) t))))
    core))

(defun code-fixup-locs (code spacemap)
  (let ((locs (sb-vm::%code-fixups code)))
    ;; Return only the absolute fixups
    ;; Ensure that a bignum LOCS is translated before using it.
    (values (sb-c::unpack-code-fixup-locs
             (if (fixnump locs) locs (translate locs spacemap))))))

(declaim (ftype function extract-object-from-core))
(defun extract-fun-map (code core)
  ;; Pointers to target objects should be SAPified before passing them,
  ;; so that this is safe under precise GC. Consider what happens if you pass an object
  ;; via its tagged pointer that looks like it's into the host's heap, but it's physically
  ;; mapped elsewhere. GC sees the bits of the alleged object and thinks you mean to refer
  ;; to the host's heap. That's completely wrong, but it mostly does no harm on
  ;; conservative GC. However, it _does_ do harm even on conservative GC if we actually
  ;; store such pointer somewhere that pointer tracing sees it. So we're technically
  ;; in the clear only as long as the pointer is _always_ ambiguous (i.e. on the stack)
  ;; or else made into a proper SAP. And all deref operations should read via the SAP
  ;; and return a SAP. I didn't feel up to the task of emulating every single primitive object
  ;; reader and structure slot reader needed in this file. Though maybe I'll get around
  ;; to it some day, as all the emulations could be autogenerated somehow.
  (let* ((di-sap (int-sap (get-lisp-obj-address (%code-debug-info code))))
         (proxy-di (extract-object-from-core di-sap core)))
    (sb-di::uncompact-fun-map proxy-di)))

;;; Examine CODE, returning a list of lists describing how to emit
;;; the contents into the assembly file.
;;;   ({:data | :padding} . N) | (start-pc . end-pc)
;;; CODE is supplied as a _physical_ object, i.e. whever it is currently
;;; mapped into memory which on AMD64 Linux is typically around #x7F.........F
(defun get-text-ranges (code core)
  (let* ((fun-map (extract-fun-map code core))
         (next-simple-fun-pc-offs (%code-fun-offset code 0))
         (start-pc (code-n-unboxed-data-bytes code))
         (simple-fun-index -1)
         (simple-fun)
         (blobs)
         (i 1)
         (len (length fun-map)))
    (when (plusp start-pc)
      (aver (zerop (rem start-pc n-word-bytes)))
      (push `(:data . ,(ash start-pc (- word-shift))) blobs))
    (loop
      (let* ((end-pc (if (= i (length fun-map))
                         (%code-text-size code)
                         (aref fun-map i))))
        (cond
          ((= start-pc end-pc)) ; crazy shiat. do not add to blobs
          ((<= start-pc next-simple-fun-pc-offs (1- end-pc))
           (incf simple-fun-index)
           (setq simple-fun (%code-entry-point code simple-fun-index))
           (let ((padding (- next-simple-fun-pc-offs start-pc)))
             (when (plusp padding)
               ;; Assert that SIMPLE-FUN always begins at an entry
               ;; in the fun-map, and not somewhere in the middle:
               ;;   |<--  fun  -->|<--  fun  -->|
               ;;   ^- start (GOOD)      ^- alleged start (BAD)
               (cond ((eq simple-fun (%code-entry-point code 0))
                      (bug "Misaligned fun start"))
                     (t   ; sanity-check the length of the filler
                      (aver (< padding (* 2 n-word-bytes)))))
               (push `(:pad . ,padding) blobs)
               (incf start-pc padding)))
           (push `(,start-pc . ,end-pc) blobs)
           (setq next-simple-fun-pc-offs
                 (if (< (1+ simple-fun-index) (code-n-entries code))
                     (%code-fun-offset code (1+ simple-fun-index))
                     -1)))
          (t
           (let ((current-blob (car blobs)))
             (setf (cdr current-blob) end-pc)))) ; extend this blob
        (setq start-pc end-pc))
      (when (= i len)
        (return (nreverse blobs)))
      (incf i 2))))

(defun %widetag-of (word) (logand word widetag-mask))

(defun make-code-obj (addr spacemap)
  (let ((translation (translate-ptr addr spacemap)))
    (aver (= (%widetag-of (sap-ref-word (int-sap translation) 0))
             code-header-widetag))
    (%make-lisp-obj (logior translation other-pointer-lowtag))))

(defun copy-bytes (in-stream out-stream nbytes
                             &optional (buffer
                                        (make-array 1024 :element-type '(unsigned-byte 8))))
  (loop (let ((chunksize (min (length buffer) nbytes)))
          (aver (eql (read-sequence buffer in-stream :end chunksize) chunksize))
          (write-sequence buffer out-stream :end chunksize)
          (when (zerop (decf nbytes chunksize)) (return)))))

;;;;

(defun read-core-header (input core-header &optional verbose &aux (core-offset 0))
  (read-sequence core-header input)
  (cond ((= (%vector-raw-bits core-header 0) core-magic))
        (t ; possible embedded core
         (file-position input (- (file-length input)
                                 (* 2 n-word-bytes)))
         (aver (eql (read-sequence core-header input) (* 2 n-word-bytes)))
         (aver (= (%vector-raw-bits core-header 1) core-magic))
         (setq core-offset (%vector-raw-bits core-header 0))
         (when verbose
           (format t "~&embedded core starts at #x~x into input~%" core-offset))
         (file-position input core-offset)
         (read-sequence core-header input)
         (aver (= (%vector-raw-bits core-header 0) core-magic))))
  core-offset)

(defmacro do-core-header-entry (((id-var len-var ptr-var) buffer) &body body)
  `(let ((,ptr-var 1))
     (loop
       (let ((,id-var (%vector-raw-bits ,buffer ,ptr-var))
             (,len-var (%vector-raw-bits ,buffer (1+ ,ptr-var))))
         ;; (format t "~&entry type ~D @ ~d len ~d words~%" id ptr len)
         (incf ,ptr-var 2)
         (decf ,len-var 2)
         (when (= ,id-var end-core-entry-type-code)
           (aver (not (find 0 ,buffer :start (ash ,ptr-var word-shift) :test #'/=)))
           (return ,ptr-var))
         ,@body
         (incf ,ptr-var ,len-var)))))

(defmacro do-directory-entry (((index-var start-index input-nbytes) buffer) &body body)
  `(let ((words-per-dirent 5))
     (multiple-value-bind (n-entries remainder)
         (floor ,input-nbytes words-per-dirent)
       (aver (zerop remainder))
       (symbol-macrolet ((id        (%vector-raw-bits ,buffer index))
                         (nwords    (%vector-raw-bits ,buffer (+ index 1)))
                         (data-page (%vector-raw-bits ,buffer (+ index 2)))
                         (addr      (%vector-raw-bits ,buffer (+ index 3)))
                         (npages    (%vector-raw-bits ,buffer (+ index 4))))
         (do ((,index-var ,start-index (+ ,index-var words-per-dirent)))
             ((= ,index-var (+ ,start-index (* n-entries words-per-dirent))))
           ,@body)))))

#+win32
(defun win32-binary-open (pathname)
  (alien-funcall (extern-alien "_open" (function int c-string int &optional int))
                 (native-namestring pathname)
                 #x8000 ; _O_BINARY
                 0))

(defmacro with-mapped-core ((sap-var start npages stream) &body body)
  (let ((fd (gensym "FD")))
    `(let (,sap-var
           (,fd #-win32 (sb-sys:fd-stream-fd ,stream)
                ;; on Windows, FD-STREAM-FD is a HANDLE rather than an FD.
                #+win32 (win32-binary-open (sb-int:file-name ,stream))))
       (unwind-protect
            (progn
              (setq ,sap-var
                    (alien-funcall
                     (extern-alien "load_core_bytes"
                                   (function system-area-pointer
                                             int int unsigned unsigned int))
                     ,fd
                     (+ ,start +backend-page-bytes+)  ; Skip the core header
                     0                                ; place it anywhere
                     (* ,npages +backend-page-bytes+) ; len
                     0))
              ,@body)
         #+win32 (sb-win32::crt-close ,fd)
         (when ,sap-var
           (alien-funcall
            (extern-alien "os_deallocate"
                          (function void system-area-pointer unsigned))
            ,sap-var (* ,npages +backend-page-bytes+)))))))

(defun core-header-nwords (core-header &aux (sum 2))
  ;; SUM starts as 2, as the core's magic number occupies 1 word
  ;; and the ending tag of END-CORE-ENTRY-TYPE-CODE counts as 1.
  (do-core-header-entry ((id len ptr) core-header)
    ;; LEN as bound by the macro does not count 1 for the
    ;; the entry identifier or LEN itself so add them in.
    (incf sum (+ len 2)))
  sum)

(defun change-dynamic-space-size (core-header new-size) ; expressed in MiB
  (unless new-size
    (return-from change-dynamic-space-size core-header))
  (let ((new (copy-seq core-header)))
    ;; memsize options if present must immediately follow the core magic number
    ;; so it might require a byte-blt to move other entries over.
    (unless (= (%vector-raw-bits new 1) runtime-options-magic)
      ;; slide the header to right by 5 words
      (replace new core-header :start1 (* 6 n-word-bytes) :start2 (* 1 n-word-bytes))
      ;; see write_memsize_options for the format of this entry
      ;; All words have to be stored since we're creating it from nothing.
      (setf (%vector-raw-bits new 1) runtime-options-magic
            (%vector-raw-bits new 2) 5 ; number of words in this entry
            (%vector-raw-bits new 4) (extern-alien "thread_control_stack_size" unsigned)
            (%vector-raw-bits new 5) (extern-alien "dynamic_values_bytes" (unsigned 32))))
    (setf (%vector-raw-bits new 3) (* new-size 1024 1024))
    new))

;; These will get set to 0 if the target is not using mark-region-gc
(defglobal *bitmap-bits-per-page* (/ gencgc-page-bytes (* cons-size n-word-bytes)))
(defglobal *bitmap-bytes-per-page* (/ *bitmap-bits-per-page* n-byte-bits))

(defstruct page
  words-used
  (single-obj-p 0 :type bit)
  type
  scan-start
  bitmap)

(defun read-page-table (stream n-ptes nbytes data-page &optional (print nil))
  (declare (ignore nbytes))
  (let ((table (make-array n-ptes)))
    (file-position stream (* (1+ data-page) +backend-page-bytes+))
    (dotimes (i n-ptes)
      (let* ((bitmap (make-array *bitmap-bits-per-page* :element-type 'bit))
             (temp (make-array *bitmap-bytes-per-page* :element-type '(unsigned-byte 8))))
        (when (plusp *bitmap-bits-per-page*)
          (read-sequence temp stream))
        (dotimes (i (/ (length bitmap) n-word-bits))
          (setf (%vector-raw-bits bitmap i) (%vector-raw-bits temp i)))
        (setf (aref table i) (make-page :bitmap bitmap))))
    ;; a PTE is a lispword and a uint16_t
    (let ((buf (make-array 10 :element-type '(unsigned-byte 8))))
      (with-pinned-objectS (buf)
        (dotimes (i n-ptes)
          (read-sequence buf stream)
          (let ((sso (sap-ref-word (vector-sap buf) 0))
                (words-used (sap-ref-16 (vector-sap buf) 8))
                (p (aref table i)))
            (setf (page-words-used p) (logandc2 words-used 1)
                  (page-single-obj-p p) (logand words-used 1)
                  (page-scan-start p) (logandc2 sso 7)
                  (page-type p) (logand sso 7))
            (when (and print (plusp (page-words-used p)))
              (format t "~4d: ~4x ~2x~:[~; -~x~]~%"
                      i (ash (page-words-used p) word-shift)
                      (page-type p)
                      (if (= (page-single-obj-p p) 0) nil 1)
                      (page-scan-start p)))))))
    table))

(defglobal page-type-symbols
    #(:free :unboxed :boxed :mixed :small-mixed :cons nil :code))
(defun encode-page-type (keyword)
  (the (not null) (position (the keyword keyword) page-type-symbols)))
(defun decode-page-type (type) (svref page-type-symbols type))

(defun calc-page-index (vaddr space)
  (let ((vaddr (if (system-area-pointer-p vaddr) (sap-int vaddr) vaddr)))
    (floor (- vaddr (space-addr space)) gencgc-page-bytes)))
(defun calc-page-base (vaddr)
  (logandc2 vaddr (1- gencgc-page-bytes)))
(defun calc-object-index (vaddr)
  (ash (- vaddr (calc-page-base vaddr)) (- n-lowtag-bits)))

(defun page-bytes-used (index ptes)
  (ash (page-words-used (svref ptes index)) word-shift))

(defun find-ending-page (index ptes)
  ;; A page ends a contiguous block if it is not wholly used,
  ;; or if there is no next page,
  ;; or the next page starts its own contiguous block
  (if (or (< (page-bytes-used index ptes) gencgc-page-bytes)
          (= (1+ index) (length ptes))
          (zerop (page-scan-start (svref ptes (1+ index)))))
      index
      (find-ending-page (1+ index) ptes)))

(defun page-addr (index space) (+ (space-addr space) (* index gencgc-page-bytes)))

(defun walk-dynamic-space (page-type spacemap function)
  (do* ((space (get-space dynamic-core-space-id spacemap))
        (ptes (space-page-table space))
        (nptes (length ptes))
        (page-ranges)
        (first-page 0))
       ((>= first-page nptes) (nreverse page-ranges))
    #+gencgc
    (let* ((last-page (find-ending-page first-page ptes))
           (pte (aref (space-page-table space) first-page))
           (start-vaddr (page-addr first-page space))
           (end-vaddr (+ (page-addr last-page space) (page-bytes-used last-page ptes))))
      (when (and (plusp (page-type pte))
                 (or (null page-type) (eq page-type (decode-page-type (page-type pte)))))
        ;; Because gencgc has page-spanning objects, it's easiest to zero-fill later
        ;; if we track the range boundaries now.
        (push (list nil first-page last-page) page-ranges) ; NIL = no funcallable-instance
        (do ((vaddr (int-sap start-vaddr))
             (paddr (int-sap (translate-ptr start-vaddr spacemap))))
            ((>= (sap-int vaddr) end-vaddr))
          (let* ((word (sap-ref-word paddr 0))
                 (widetag (logand word widetag-mask))
                 (size (if (eq widetag filler-widetag)
                           (ash (ash word -32) word-shift) ; -> words -> bytes
                           (let* ((obj (reconstitute-object (%make-lisp-obj (sap-int paddr))))
                                  (size (primitive-object-size obj)))
                             ;; page types codes are never defined for Lisp
                             (when (eq page-type 7) ; KLUDGE: PAGE_TYPE_CODE
                               (aver (or (= widetag code-header-widetag)
                                         (= widetag funcallable-instance-widetag))))
                             (when (= widetag funcallable-instance-widetag)
                               (setf (caar page-ranges) t)) ; T = has funcallable-instance
                             (funcall function obj vaddr size :ignore)
                             size))))
            (setq vaddr (sap+ vaddr size)
                  paddr (sap+ paddr size)))))
      (setq first-page (1+ last-page)))
    #+mark-region-gc
    (let* ((vaddr (int-sap (+ (space-addr space) (* first-page gencgc-page-bytes))))
           (paddr (int-sap (translate-ptr (sap-int vaddr) spacemap)))
           (pte (aref (space-page-table space) first-page))
           (bitmap (page-bitmap pte)))
      (cond ((= (page-single-obj-p pte) 1)
             ;; last page is located by doing some arithmetic
             (let* ((obj (reconstitute-object (%make-lisp-obj (sap-int paddr))))
                    (size (primitive-object-size obj))
                    (last-page (calc-page-index (sap+ vaddr (1- size)) space)))
               #+nil (format t "~&Page ~4d..~4d ~A LARGE~%" first-page last-page (decode-page-type (page-type pte)))
               (funcall function obj vaddr size t)
               (setq first-page last-page)))
            ((plusp (page-type pte))
             #+nil (format t "~&Page ~4D : ~A~%" first-page (decode-page-type (page-type pte)))
             (when (or (null page-type) (eq page-type (decode-page-type (page-type pte))))
               (do ((object-offset-in-dualwords 0))
                   ((>= object-offset-in-dualwords *bitmap-bits-per-page*))
                 (let ((size
                        (cond ((zerop (sbit bitmap object-offset-in-dualwords))
                               (unless (and (zerop (sap-ref-word paddr 0))
                                            (zerop (sap-ref-word paddr 8)))
                                 (error "Unallocated object @ ~X: ~X ~X"
                                        vaddr (sap-ref-word paddr 0) (sap-ref-word paddr 8)))
                               (* 2 n-word-bytes))
                              (t
                               (let* ((obj (reconstitute-object (%make-lisp-obj (sap-int paddr))))
                                      (size (primitive-object-size obj)))
                                 (funcall function obj vaddr size nil)
                                 size)))))
                   (setq vaddr (sap+ vaddr size)
                         paddr (sap+ paddr size))
                   (incf object-offset-in-dualwords (ash size (- (1+ word-shift)))))))))
      (incf first-page))))

;;; Unfortunately the idea of using target features to decide whether to
;;; read a bitmap from PAGE_TABLE_CORE_ENTRY_TYPE_CODE falls flat,
;;; because we can't scan for symbols until the core is read, but we can't
;;; read the core until we decide whether there is a bitmap, which needs the
;;; feature symbols. Some possible solutions (and there are others too):
;;; 1) make a separate core entry for the bitmap
;;; 2) add a word to that core entry indicating that it has a bitmap
;;; 3) make a different entry type code for PTES_WITH_BITMAP
(defun detect-target-features (spacemap &aux result)
  (flet ((scan (symbol)
           (let ((list (symbol-global-value symbol))
                 (target-nil (compute-nil-object spacemap)))
             (loop
               (when (eq list target-nil) (return))
               (setq list (translate list spacemap))
               (let ((feature (translate (car list) spacemap)))
                 (aver (symbolp feature))
                 ;; convert keywords and only keywords into host keywords
                 (when (eq (symbol-package-id feature) (symbol-package-id :sbcl))
                   (let ((string (translate (symbol-name feature) spacemap)))
                     (push (intern string "KEYWORD") result))))
               (setq list (cdr list))))))
    (walk-dynamic-space
     nil
     spacemap
     (lambda (obj vaddr size large)
       (declare (ignore vaddr size large))
       (when (symbolp obj)
         (when (or (and (eq (symbol-package-id obj) #.(symbol-package-id 'sb-impl:+internal-features+))
                        (string= (translate (symbol-name obj) spacemap) "+INTERNAL-FEATURES+"))
                   (and (eq (symbol-package-id obj) #.(symbol-package-id '*features*))
                        (string= (translate (symbol-name obj) spacemap) "*FEATURES*")))
           (scan obj))))))
  ;;(format t "~&Target-features=~S~%" result)
  result)

(defun transport-code (from-vaddr from-paddr to-vaddr to-paddr size)
  (%byte-blt from-paddr 0 to-paddr 0 size)
  (let* ((new-physobj (%make-lisp-obj (logior (sap-int to-paddr) other-pointer-lowtag)))
         (header-bytes (ash (code-header-words new-physobj) word-shift))
         (new-insts (code-instructions new-physobj)))
    ;; fix the jump table words which, if present, start at NEW-INSTS
    (let ((wordcount (code-jump-table-words new-physobj))
          (disp (sap- to-vaddr from-vaddr)))
      (loop for i from 1 below wordcount
            do (let ((w (sap-ref-word new-insts (ash i word-shift))))
                 (unless (zerop w)
                   (setf (sap-ref-word new-insts (ash i word-shift)) (+ w disp))))))
    ;; fix the simple-fun pointers
    (dotimes (i (code-n-entries new-physobj))
      (let ((fun-offs (%code-fun-offset new-physobj i)))
        ;; Assign the address that each simple-fun will have assuming
        ;; the object will reside at its new logical address.
        (setf (sap-ref-sap new-insts (+ fun-offs n-word-bytes))
              (sap+ to-vaddr (+ header-bytes fun-offs (* 2 n-word-bytes))))))))

(defun transport-dynamic-space-code (codeblobs spacemap new-space free-ptr)
  (do ((list codeblobs (cdr list))
       (offsets-vector-data (sap+ new-space (* 2 n-word-bytes)))
       (object-index 0 (1+ object-index)))
      ((null list))
    ;; FROM-VADDR is the original logical (virtual) address, and FROM-PADDR
    ;; is where the respective object is currently resident in memory now.
    ;; Similarly-named "TO-" values correspond to the location in new space.
    (destructuring-bind (from-vaddr . size) (car list)
      (let ((from-paddr (int-sap (translate-ptr (sap-int from-vaddr) spacemap)))
            (to-vaddr (+ +code-space-nominal-address+ free-ptr))
            (to-paddr (sap+ new-space free-ptr)))
        (setf (sap-ref-32 offsets-vector-data (ash object-index 2)) free-ptr)
        (transport-code from-vaddr from-paddr (int-sap to-vaddr) to-paddr size)
        (incf free-ptr size)))))

(defun remap-to-quasi-static-code (val spacemap fwdmap)
  (when (is-lisp-pointer (get-lisp-obj-address val))
    (binding* ((translated (translate val spacemap))
               (vaddr (get-lisp-obj-address val))
               (code-base-addr
                (cond ((simple-fun-p translated)
                       ;; the code component has to be computed "by hand" because FUN-CODE-HEADER
                       ;; would return the physically mapped object, but we need
                       ;; to get the logical address of the code.
                       (- (- vaddr fun-pointer-lowtag)
                          (ash (ldb (byte 24 8)
                                    (sap-ref-word (int-sap (get-lisp-obj-address translated))
                                                  (- fun-pointer-lowtag)))
                               word-shift)))
                      ((code-component-p translated)
                       (- vaddr other-pointer-lowtag)))
                :exit-if-null)
               (new-code-offset (gethash code-base-addr fwdmap) :exit-if-null))
      (%make-lisp-obj (+ (if (functionp translated)
                             (- vaddr code-base-addr) ; function tag is in the difference
                             other-pointer-lowtag)
                         +code-space-nominal-address+
                         new-code-offset)))))

;;; It's not worth trying to use the host's DO-REFERENCED-OBJECT because it requires
;;; completely different behavior for INSTANCE and FUNCALLABLE-INSTANCE to avoid using
;;; the layout pointers as-is. And closures don't really work either. So unfortunately
;;; this is essentially a reimplementation. Thankfully we only have to deal with pointers
;;; that could possibly point to code.
(defun update-quasi-static-code-ptrs
    (obj spacemap fwdmap displacement &optional print
     &aux (sap (int-sap (logandc2 (get-lisp-obj-address obj) lowtag-mask))))
  (when print
    (format t "paddr ~X vaddr ~X~%" (get-lisp-obj-address obj)
            (+ (get-lisp-obj-address obj) displacement)))
  (macrolet ((visit (place)
               `(let* ((oldval ,place) (newval (remap oldval)))
                  (when newval
                    (setf ,place newval)))))
    (flet ((fun-entrypoint (fun)
             (+ (get-lisp-obj-address fun) (- fun-pointer-lowtag) (ash 2 word-shift)))
           (remap (x)
             (remap-to-quasi-static-code x spacemap fwdmap)))
      (cond
        ((listp obj) (visit (car obj)) (visit (cdr obj)))
        ((simple-vector-p obj)
         (dotimes (i (length obj)) (visit (svref obj i))))
        ((%instancep obj)
         (let ((type (truly-the layout (translate (%instance-layout obj) spacemap))))
           (do-layout-bitmap (i taggedp type (%instance-length obj))
             (when taggedp (visit (%instance-ref obj i))))))
        ((functionp obj)
         (let ((start
                (cond ((funcallable-instance-p obj)
                       ;; The trampoline points to the function itself (so is ignorable)
                       ;; and following that word are 2 words of machine code.
                       4)
                      (t
                       (aver (closurep obj))
                       (let ((fun (remap (%closure-fun obj))))
                         ;; there is no setter for closure-fun
                         (setf (sap-ref-word sap n-word-bytes) (fun-entrypoint fun)))
                       2))))
           (loop for i from start to (logior (get-closure-length obj) 1)
                 do (visit (sap-ref-lispobj sap (ash i word-shift))))))
        ((code-component-p obj)
         (loop for i from 2 below (code-header-words obj)
               do (visit (code-header-ref obj i))))
        ((symbolp obj)
         (visit (sap-ref-lispobj sap (ash symbol-value-slot word-shift))))
        ((weak-pointer-p obj)
         (visit (sap-ref-lispobj sap (ash weak-pointer-value-slot word-shift))))
        ((fdefn-p obj)
         (let ((raw (sap-ref-word sap (ash fdefn-raw-addr-slot word-shift))))
           (unless (in-bounds-p raw (space-bounds static-core-space-id spacemap))
             (awhen (remap (%make-lisp-obj (+ raw (ash -2 word-shift) fun-pointer-lowtag)))
               (setf (sap-ref-word sap (ash fdefn-raw-addr-slot word-shift))
                     (fun-entrypoint it)))))
         (visit (sap-ref-lispobj sap (ash fdefn-fun-slot word-shift))))
        ((= (%other-pointer-widetag obj) value-cell-widetag)
         (visit (sap-ref-lispobj sap (ash value-cell-value-slot word-shift))))))))

;;; Clear all the old objects.  Funcallable instances can be co-mingled with
;;; code, so a code page might not be empty but most will be. Free those pages.
(defun zerofill-old-code (spacemap codeblobs page-ranges)
  (declare (ignorable page-ranges))
  (with-alien ((memset (function void unsigned int unsigned) :extern))
    (flet ((reset-pte (pte)
             (setf (page-words-used pte) 0
                   (page-single-obj-p pte) 0
                   (page-type pte) 0
                   (page-scan-start pte) 0)))
      (let ((space (get-space dynamic-core-space-id spacemap)))
        #+gencgc
        (dolist (range page-ranges (aver (null codeblobs)))
          (destructuring-bind (in-use first last) range
            ;;(format t "~&Working on range ~D..~D~%" first last)
            (loop while codeblobs
                  do (destructuring-bind (vaddr . size) (car codeblobs)
                       (let ((page (calc-page-index vaddr space)))
                         (cond ((> page last) (return))
                               ((< page first) (bug "Incorrect sort"))
                               (t
                                (let ((paddr (translate-ptr (sap-int vaddr) spacemap)))
                                  (alien-funcall memset paddr 0 size)
                                  (when in-use ; store a filler widetag
                                    (let* ((nwords (ash size (- word-shift)))
                                           (header (logior (ash nwords 32) filler-widetag)))
                                      (setf (sap-ref-word (int-sap paddr) 0) header))))
                                (pop codeblobs))))))
            (unless in-use
              (loop for page-index from first to last
                    do (reset-pte (svref (space-page-table space) page-index))))))
        #+mark-region-gc
        (dolist (code codeblobs)
          (destructuring-bind (vaddr . size) code
            (alien-funcall memset (translate-ptr (sap-int vaddr) spacemap) 0 size)
            (let* ((page-index (calc-page-index vaddr space))
                   (pte (aref (space-page-table space) page-index))
                   (object-index (calc-object-index (sap-int vaddr))))
              (setf (sbit (page-bitmap pte) object-index) 0)
              (cond ((= (page-single-obj-p pte) 1)
                     ;(format t "~&Cleared large-object pages @ ~x~%" (sap-int vaddr))
                     (loop for p from page-index to (calc-page-index (sap+ vaddr (1- size)) space)
                           do (let ((pte (svref (space-page-table space) p)))
                                (aver (not (find 1 (page-bitmap pte))))
                                (reset-pte pte))))
                    ((not (find 1 (page-bitmap pte)))
                     ;; is the #+gencgc logic above actually more efficient?
                     ;;(format t "~&Code page ~D is now empty~%" page-index)
                     (reset-pte pte))))))))))

(defmacro linkage-space-header-ptr (x) `(svref ,x 0))
(defmacro linkage-space-data-page (x) `(svref ,x 1))
(defmacro linkage-space-npages (x) `(svref ,x 2))
(defmacro linkage-space-count (x) `(svref ,x 3))
(defmacro linkage-space-cells (x) `(svref ,x 4))

(defun read-linkage-cells (stream linkage-space-info)
  (let ((savepos (file-position stream))
        (words (linkage-space-cells linkage-space-info)))
    (file-position stream (* (1+ (linkage-space-data-page linkage-space-info))
                             +backend-page-bytes+))
    (with-pinned-objects (words)
      (sb-unix:unix-read (sb-sys:fd-stream-fd stream) (vector-sap words)
                         (ash (length words) word-shift)))
    (file-position stream savepos)))

(defstruct core-header
  dir-start ; offset in words
  total-npages
  pte-nbytes
  space-list
  linkage-space-info
  initfun
  card-mask-nbits)

(defun parse-core-header (input core-header)
  (let ((space-list)
        (total-npages 0) ; excluding core header page
        (linkage-space-info)
        (pte-nbytes)
        (card-mask-nbits)
        (core-dir-start)
        (initfun))
    (do-core-header-entry ((id len ptr) core-header)
      (ecase id
        (#.directory-core-entry-type-code
         (setq core-dir-start (- ptr 2))
         (do-directory-entry ((index ptr len) core-header)
           (incf total-npages npages)
           (push (make-space id addr data-page 0 nwords) space-list)))
        (#.lisp-linkage-space-core-entry-type-code
         (symbol-macrolet ((count (%vector-raw-bits core-header (+ ptr 0)))
                           (data-page (%vector-raw-bits core-header (+ ptr 1))))
           (let ((npages (ceiling (ash count word-shift) +backend-page-bytes+)))
             (setq linkage-space-info
                   (vector (+ ptr 2) data-page npages count
                           (make-array count :element-type 'word)))
             (read-linkage-cells input linkage-space-info)
             (incf total-npages npages))))
        (#.page-table-core-entry-type-code
         (aver (= len 4))
         (symbol-macrolet ((n-ptes (%vector-raw-bits core-header (+ ptr 1)))
                           (nbytes (%vector-raw-bits core-header (+ ptr 2)))
                           (data-page (%vector-raw-bits core-header (+ ptr 3))))
           (aver (= data-page total-npages))
           (setf pte-nbytes nbytes)
           (setf card-mask-nbits (%vector-raw-bits core-header ptr))
           (format nil "~&card-nbits = ~D~%" card-mask-nbits)
           (let ((space (get-space dynamic-core-space-id (cons nil space-list))))
             (setf (space-page-table space) (read-page-table input n-ptes nbytes data-page)))))
        (#.build-id-core-entry-type-code
         (let ((string (make-string (%vector-raw-bits core-header ptr)
                                    :element-type 'base-char)))
           (%byte-blt core-header (* (1+ ptr) n-word-bytes) string 0 (length string))
           (format nil "Build ID [~a] len=~D ptr=~D actual-len=~D~%" string len ptr (length string))))
        (#.runtime-options-magic) ; ignore
        (#.initial-fun-core-entry-type-code
         (setq initfun (%vector-raw-bits core-header ptr)))))
    (make-core-header :dir-start core-dir-start
                      :total-npages total-npages
                      :pte-nbytes pte-nbytes
                      :space-list (nreverse space-list)
                      :linkage-space-info linkage-space-info
                      :initfun initfun
                      :card-mask-nbits card-mask-nbits)))

(defconstant +lispwords-per-corefile-page+ (/ +backend-page-bytes+ n-word-bytes))

(defun rewrite-core (directory core-header parsed-header spacemap output
                     &aux (offset (core-header-dir-start parsed-header))
                          (dynamic-space (get-space dynamic-core-space-id spacemap))
                          (initfun (core-header-initfun parsed-header))
                          (fd (sb-impl::fd-stream-fd output)))
  (aver (= (%vector-raw-bits core-header offset) directory-core-entry-type-code))
  (let ((nwords (+ (* (length directory) 5) 2)))
    (setf (%vector-raw-bits core-header (incf offset)) nwords))
  (let ((page-count 0)
        (n-ptes (length (space-page-table dynamic-space))))
    (dolist (dir-entry directory)
      (setf (car dir-entry) page-count)
      (destructuring-bind (id paddr vaddr nwords) (cdr dir-entry)
        (declare (ignore paddr))
        (let ((npages (ceiling nwords +lispwords-per-corefile-page+)))
          (when (= id dynamic-core-space-id)
            (aver (= npages n-ptes)))
          (dolist (word (list id nwords page-count vaddr npages))
            (setf (%vector-raw-bits core-header (incf offset)) word))
          (incf page-count npages))))
    (let* ((sizeof-corefile-pte (+ n-word-bytes 2))
           (pte-bytes (align-up (* sizeof-corefile-pte n-ptes) n-word-bytes)))
      (dolist (word (list  page-table-core-entry-type-code
                           6 ; = number of words in this core header entry
                           (core-header-card-mask-nbits parsed-header)
                           n-ptes (+ (* n-ptes *bitmap-bytes-per-page*) pte-bytes)
                           page-count))
        (setf (%vector-raw-bits core-header (incf offset)) word)))
    (dolist (word (list initial-fun-core-entry-type-code 3 initfun
                        end-core-entry-type-code 2))
      (setf (%vector-raw-bits core-header (incf offset)) word))
    (write-sequence core-header output)
    ;; write out the data from each space
    (dolist (dir-entry directory)
      (destructuring-bind (page id paddr vaddr nwords) dir-entry
        (declare (ignore id vaddr))
        (aver (= (file-position output) (* +backend-page-bytes+ (1+ page))))
        (let* ((npages (ceiling nwords +lispwords-per-corefile-page+))
               (nbytes (* npages +backend-page-bytes+))
               (wrote (sb-unix:unix-write fd paddr 0 nbytes)))
          (aver (= wrote nbytes)))))
    (aver (= (file-position output) (* +backend-page-bytes+ (1+ page-count))))
    #+mark-region-gc ; write the bitmap
    (dovector (pte (space-page-table dynamic-space))
      (let ((bitmap (page-bitmap pte)))
        (sb-sys:with-pinned-objects (bitmap)
          ;; WRITE-SEQUENCE on a bit vector would write one octet per bit
          (sb-unix:unix-write fd bitmap 0 (/ (length bitmap) 8)))))
    ;; write the PTEs
    (let ((buffer (make-array 10 :element-type '(unsigned-byte 8))))
      (sb-sys:with-pinned-objects (buffer)
        (let ((sap (vector-sap buffer)))
          (dovector (pte (space-page-table dynamic-space))
            (setf (sap-ref-64 sap 0) (logior (page-scan-start pte) (page-type pte))
                  (sap-ref-16 sap 8) (logior (page-words-used pte) (page-single-obj-p pte)))
            (write-sequence buffer output)))
        (let* ((bytes-written (* 10 (length (space-page-table dynamic-space))))
               (diff (- (align-up bytes-written n-word-bytes)
                        bytes-written)))
          (fill buffer 0)
          (write-sequence buffer output :end diff))))
    ;; write the trailer
    (let ((buffer (make-array 16 :element-type '(unsigned-byte 8)
                                 :initial-element 0)))
      (sb-sys:with-pinned-objects (buffer)
        (setf (%vector-raw-bits buffer 0) 0
              (%vector-raw-bits buffer 1) core-magic)
        (write-sequence buffer output)))
    (force-output output)))

(defun walk-target-space (function space-id spacemap)
  (let* ((space (get-space space-id spacemap))
         (paddr (space-physaddr space spacemap)))
    (map-objects-in-range function
                          (%make-lisp-obj
                           (if (= space-id static-core-space-id)
                               ;; must not visit NIL, bad things happen
                               (translate-ptr (+ static-space-start sb-vm::static-space-objects-offset)
                                              spacemap)
                               (sap-int paddr)))
                          (%make-lisp-obj (sap-int (sap+ paddr (space-size space)))))))

(defun find-target-asm-code (spacemap)
  (walk-target-space (lambda (obj widetag size)
                       (declare (ignore size))
                       (when (= widetag code-header-widetag)
                         (return-from find-target-asm-code
                           (let* ((space (get-space static-core-space-id spacemap))
                                  (vaddr (space-addr space))
                                  (paddr (space-physaddr space spacemap)))
                             (%make-lisp-obj
                              (+ vaddr (- (get-lisp-obj-address obj)
                                          (sap-int paddr))))))))
                     static-core-space-id spacemap))

(defconstant simple-array-uword-widetag
  #+64-bit simple-array-unsigned-byte-64-widetag
  #-64-bit simple-array-unsigned-byte-32-widetag)

(defun move-dynamic-code-to-text-space (input-pathname output-pathname)
  ;; Remove old files
  (ignore-errors (delete-file output-pathname))
  ;; Ensure that all files can be opened
  (with-open-file (input input-pathname :element-type '(unsigned-byte 8))
    (with-open-file (output output-pathname :direction :output
                                            :element-type '(unsigned-byte 8) :if-exists :supersede)
      ;; KLUDGE: see comment above DETECT-TARGET-FEATURES
      #+gencgc (setq *bitmap-bits-per-page* 0 *bitmap-bytes-per-page* 0)
      (let* ((core-header (make-array +backend-page-bytes+ :element-type '(unsigned-byte 8)))
             (core-offset (read-core-header input core-header))
             (parsed-header (parse-core-header input core-header))
             (space-list (core-header-space-list parsed-header)))
        ;; Map the core file to memory
        (with-mapped-core (sap core-offset (core-header-total-npages parsed-header) input)
          (let* ((spacemap (cons sap (sort (copy-list space-list) #'> :key #'space-addr)))
                 (target-features (detect-target-features spacemap))
                 (codeblobs nil)
                 (fwdmap (make-hash-table))
                 (n-objects)
                 (offsets-vector-size)
                 ;; We only need enough space to write C linkage call redirections from the
                 ;; assembler routine codeblob, because those are the calls which assume that
                 ;; asm code can call the alien linkage table using rel32 form.
                 ;; Dynamic-space calls do not assume that - they use "CALL [ea]" form.
                 (c-linkage-reserved-words 12) ; arbitrary overestimate
                 (reserved-amount)
                 ;; text space will contain a copy of the asm code so it can use call rel32 form
                 (asm-code (find-target-asm-code spacemap))
                 (asm-code-size (primitive-object-size (translate asm-code spacemap)))
                 (freeptr asm-code-size)
                 (page-ranges
                  (walk-dynamic-space
                   :code spacemap
                   (lambda (obj vaddr size large)
                     (declare (ignore large))
                     (when (code-component-p obj)
                       (push (cons vaddr size) codeblobs)
                       ;; new object will be at FREEPTR bytes from new space start
                       (setf (gethash (sap-int vaddr) fwdmap) freeptr)
                       (incf freeptr size))))))
            ;; FIXME: this _still_ doesn't work, because if the buid has :IMMOBILE-SPACE
            ;; then the symbols CL:*FEATURES* and SB-IMPL:+INTERNAL-FEATURES+
            ;; are not in dynamic space.
            (when (member :immobile-space target-features)
              (error "Can't relocate code to text space since text space already exists"))
            (setq codeblobs
                  (acons (int-sap (logandc2 (get-lisp-obj-address asm-code) lowtag-mask))
                         asm-code-size
                         (nreverse codeblobs))
                  n-objects (length codeblobs))
            ;; Preceding the code objects are two vectors:
            ;; (1) a vector of uint32_t indicating the starting offset (from the space start)
            ;;    of each code object.
            ;; (2) a vector of uint64_t which embeds a JMP instruction to a C linkage table entry.
            ;;    These instructions are near enough to be called via 'rel32' form. (The ordinary
            ;;    alien linkage space is NOT near enough, after code is moved to text space)
            ;; The size of the new text space has to account for the sizes of the vectors.
            (let* ((n-vector1-data-words (ceiling n-objects 2)) ; two uint32s fit in a lispword
                   (vector1-size (ash (+ (align-up n-vector1-data-words 2) ; round to even
                                         vector-data-offset)
                                      word-shift))
                   (n-vector2-data-words c-linkage-reserved-words)
                   (vector2-size (ash (+ n-vector2-data-words vector-data-offset)
                                      word-shift)))
              (setf offsets-vector-size vector1-size
                    reserved-amount (+ vector1-size vector2-size))
              ;; Adjust all code offsets upward to avoid doing more math later
              (maphash (lambda (k v)
                         (setf (gethash k fwdmap) (+ v reserved-amount)))
                       fwdmap)
              (incf freeptr reserved-amount)
              (format nil "~&Code: ~D objects, ~D bytes~%" (length codeblobs) freeptr))
            (let* ((new-space-nbytes (align-up freeptr +backend-page-bytes+))
                   (new-space (sb-sys:allocate-system-memory new-space-nbytes)))
              ;; Write header of "vector 1"
              (setf (sap-ref-word new-space 0) simple-array-unsigned-byte-32-widetag
                    (sap-ref-word new-space n-word-bytes) (fixnumize n-objects))
              ;; write header of "vector 2"
              (setf (sap-ref-word new-space offsets-vector-size) simple-array-uword-widetag
                    (sap-ref-word new-space (+ offsets-vector-size n-word-bytes))
                    (fixnumize c-linkage-reserved-words))
              ;; Transport code contiguously into new space
              (transport-dynamic-space-code codeblobs spacemap new-space reserved-amount)
              ;; Walk spaces except for newspace, changing any pointers that
              ;; should point to new space.
              (dolist (space-id `(,dynamic-core-space-id ,static-core-space-id
                                  ,permgen-core-space-id))
                (binding* ((space (get-space space-id spacemap) :exit-if-null)
                           (vaddr (space-addr space))
                           (paddr (space-physaddr space spacemap))
                           (diff (+ (- (sap-int paddr)) vaddr)))
                  (format nil "~&Fixing ~A~%" space)
                  (walk-target-space
                   (lambda (object widetag size)
                     (declare (ignore widetag size))
                     (unless (and (code-component-p object) (= space-id dynamic-core-space-id))
                       (update-quasi-static-code-ptrs object spacemap fwdmap diff)))
                   space-id spacemap)))
              ;; Walk new space and fix pointers into itself
              (format nil "~&Fixing newspace~%")
              (map-objects-in-range
               (lambda (object widetag size)
                   (declare (ignore widetag size))
                   (update-quasi-static-code-ptrs object spacemap fwdmap 0))
                 (%make-lisp-obj (sap-int new-space))
                 (%make-lisp-obj (sap-int (sap+ new-space freeptr))))
              ;; don't zerofill asm code in static space
              (zerofill-old-code spacemap (cdr codeblobs) page-ranges)
              ;; Update the core header to contain newspace
              (let ((spaces (nconc
                             (mapcar (lambda (space)
                                       (list 0 (space-id space)
                                             (int-sap (translate-ptr (space-addr space) spacemap))
                                             (space-addr space)
                                             (space-nwords space)))
                                     space-list)
                             `((0 ,immobile-text-core-space-id ,new-space
                                  ,+code-space-nominal-address+
                                  ,(ash freeptr (- word-shift)))))))
                (rewrite-core spaces core-header parsed-header spacemap output)))))))))

;;; Processing a core without immobile-space

;;; This file provides a recipe which gets a little bit closer to being able to
;;; emulate #+immobile-space in so far as producing an ELF core is concerned.
;;; The recipe is a bit more complicated than I'd like, but it works.
;;; Let's say you want a core with contiguous text space containing the code
;;; of a quicklisp system.

;;; $ run-sbcl.sh
;;; * (ql:quickload :one-more-re-nightmare-tests)
;;; * (save-lisp-and-die "step1.core")
;;; $ run-sbcl.sh
;;; * (load "tools-for-build/editcore")
;;; * (sb-editcore:move-dynamic-code-to-text-space "step1.core" "step2.core")
;;; * (sb-editcore:redirect:text-space-calls "step2.core")
;;; Now "step2.core" has a text space, and all lisp-to-lisp calls bypass their FDEFN.
;;; At this point split-core on "step2.core" can run in the manner of elfcore.test.sh

(defun get-code-segments (code vaddr core)
  (let ((di (%code-debug-info code))
        (spacemap (core-spacemap core))
        (inst-base (+ vaddr (ash (code-header-words code) word-shift)))
        (result))
    (aver (%instancep di))
    (if (zerop (code-n-entries code)) ; assembler routines
        (dolist (entry (target-hash-table-alist di spacemap))
          (let* ((val (translate (undescriptorize (cdr entry)) spacemap))
                 ;; VAL is (start end . index)
                 (start (the fixnum (car val)))
                 (end (the fixnum (car (translate (cdr val) spacemap)))))
            (push (make-code-segment code start (- (1+ end) start)
                                     :virtual-location (+ inst-base start))
                  result)))
        (dolist (range (get-text-ranges code core))
          (let ((car (car range)))
            (when (integerp car)
              (push (make-code-segment code car (- (cdr range) car)
                                       :virtual-location (+ inst-base car))
                    result)))))
    (sort result #'< :key #'sb-disassem:seg-virtual-location)))

(defstruct (range (:constructor make-range (labeled vaddr bytecount)))
  labeled vaddr bytecount)

(defun inst-vaddr (inst) (range-vaddr (car inst)))
(defun inst-length (inst) (range-bytecount (car inst)))
(defun inst-end (inst &aux (range (car inst)))
  (+ (range-vaddr range) (range-bytecount range)))

(defmethod print-object ((self range) stream)
  (format stream "~A~x,~x"
          (if (range-labeled self) "L:" "  ")
          (range-vaddr self)
          (range-bytecount self)))
(defun get-code-instruction-model (code vaddr core)
  (let* ((segments (get-code-segments code vaddr core))
         (insts-vaddr (+ vaddr (ash (code-header-words code) word-shift)))
         (dstate (sb-disassem:make-dstate))
         (fun-header-locs
          (loop for i from 0 below (code-n-entries code)
                collect (+ insts-vaddr (%code-fun-offset code i))))
         (labels))
    (sb-disassem:label-segments segments dstate)
    ;; are labels not already sorted?
    (setq labels (sort (sb-disassem::dstate-labels dstate) #'< :key #'car))
    (sb-int:collect ((result))
      (dolist (seg segments (coerce (result) 'vector))
        (setf (sb-disassem:dstate-segment dstate) seg
              (sb-disassem:dstate-segment-sap dstate)
              (funcall (sb-disassem:seg-sap-maker seg)))
        (setf (sb-disassem:dstate-cur-offs dstate) 0)
        (loop
          (when (eql (sb-disassem:dstate-cur-addr dstate) (car fun-header-locs))
            (incf (sb-disassem:dstate-cur-offs dstate) (* simple-fun-insts-offset n-word-bytes))
            (pop fun-header-locs))
          (let* ((pc (sb-disassem:dstate-cur-addr dstate))
                 (labeled (when (and labels (= pc (caar labels)))
                            (pop labels)
                            t))
                 (inst (sb-disassem:disassemble-instruction dstate))
                 (nbytes (- (sb-disassem:dstate-cur-addr dstate) pc)))
            (result (cons (make-range labeled pc nbytes) inst)))
          (when (>= (sb-disassem:dstate-cur-offs dstate) (sb-disassem:seg-length seg))
            (return)))))))

;;; TODO: can this be combined with the preceding? (Why does that one use labels anyway?)
(defun simple-collect-inst-model (sap length load-addr)
  (sb-disassem:get-inst-space) ; for effect
  (let* ((segment (sb-disassem:make-memory-segment nil (sb-sys:sap-int sap) length
                                                   :virtual-location load-addr))
         (dstate (sb-disassem:make-dstate nil)))
    (setf (sb-disassem:dstate-segment dstate) segment
          (sb-disassem:dstate-segment-sap dstate) (funcall (sb-disassem:seg-sap-maker segment)))
    (sb-int:collect ((result))
      (loop (let ((pc (sb-disassem:dstate-cur-offs dstate)))
              (result (cons pc (sb-disassem:disassemble-instruction dstate))))
            (when (>= (sb-disassem:dstate-cur-offs dstate) (sb-disassem:seg-length segment))
              (result (list (sb-disassem:dstate-cur-offs dstate) 'nop)) ; so next-pc exists
              (return)))
      (result))))

(defun get-text-space-asm-code-replica (space spacemap)
  (let* ((physaddr (sap-int (space-physaddr space spacemap)))
         (offsets-vector (%make-lisp-obj (logior physaddr other-pointer-lowtag)))
         (offset (aref offsets-vector 0)))
    (values (+ (space-addr space) offset)
            (%make-lisp-obj (+ physaddr offset other-pointer-lowtag)))))

(defun get-static-space-asm-code (space spacemap)
  (let ((found
         (block nil
           (sb-editcore::walk-target-space
            (lambda (x widetag size)
              (declare (ignore widetag size))
              (when (code-component-p x)
                (return x)))
            static-core-space-id spacemap))))
    (values (+ (- (get-lisp-obj-address found)
                  (sap-int (space-physaddr space spacemap))
                  other-pointer-lowtag)
               (space-addr space))
            found)))

(defun persist-to-file (spacemap core-offset stream)
  (aver (zerop core-offset))
  (dolist (space-id `(,static-core-space-id
                      ,immobile-text-core-space-id
                      ,dynamic-core-space-id))
    (let ((space (get-space space-id spacemap)))
      (file-position stream (* (1+ (space-data-page space)) +backend-page-bytes+))
      (sb-unix:unix-write (sb-impl::fd-stream-fd stream)
                          (space-physaddr space spacemap)
                          0
                          (align-up (* (space-nwords space) n-word-bytes)
                                    +backend-page-bytes+)))))

;;;; Offline mark-region compactor
(declaim (inline load-bits-wordindexed))
(defun load-bits-wordindexed (sap index)
  (declare (type (signed-byte 32) index))
  (sap-ref-word sap (ash index word-shift)))
(defun load-wordindexed (sap index)
  (let ((word (load-bits-wordindexed sap index)))
    (if (not (is-lisp-pointer word))
        (%make-lisp-obj word) ; fixnum, character, single-float, unbound-marker
        (make-descriptor word))))

(defun physical-sap (taggedptr spacemap)
  (let ((bits (if (descriptor-p taggedptr) (descriptor-bits taggedptr) taggedptr)))
    (int-sap (translate-ptr (logandc2 bits lowtag-mask) spacemap))))

(defun size-of (sap)
  (with-alien ((primitive-object-size (function unsigned system-area-pointer) :extern))
    (alien-funcall primitive-object-size sap)))

(defmacro get-layout (sap widetag)
  (declare (ignorable widetag))
  ;; FIXME: should depend on target feature, not host feature
  #.(if (member :compact-instance-header sb-impl:+internal-features+)
        '`(sap-ref-32 ,sap 4)
        '`(sap-ref-word ,sap (ash (ecase ,widetag
                                    (,funcallable-instance-widetag 5) ; KLUDGE
                                    (,instance-widetag 1))
                                  word-shift))))
(defun set-layout (instance-sap widetag layout-bits)
  (declare (ignorable widetag))
  (setf (get-layout instance-sap widetag) layout-bits))

;;; Return T if WIDETAG is for a pointerless object.
(defun leafp (widetag)
  (declare ((integer 0 255) widetag))
  (macrolet ((compute-leaves (&aux (result 0))
               (loop for w in ; these are the nonleaves
                     `(,closure-widetag ,code-header-widetag ,symbol-widetag ,value-cell-widetag
                       ,instance-widetag ,funcallable-instance-widetag ,weak-pointer-widetag
                       ,fdefn-widetag ,ratio-widetag ,complex-rational-widetag
                       ,simple-vector-widetag ,simple-array-widetag ,complex-array-widetag
                       ,complex-base-string-widetag #+sb-unicode ,complex-character-string-widetag
                       ,complex-vector-widetag ,complex-bit-vector-widetag)
                     do (setf result (logior result (ash 1 (ash w -2)))))
               (lognot result)))
    (logbitp (ash widetag -2) (compute-leaves))))

(defun instance-slot-count (sap widetag)
  (let ((header (sap-ref-word sap 0)))
    (ecase widetag
      (#.funcallable-instance-widetag
       (ldb (byte 8 n-widetag-bits) header))
      (#.instance-widetag
       ;; See instance_length() in src/runtime/instance.inc
       (+ (logand (ash header (- instance-length-shift)) instance-length-mask)
          (logand (ash header -10) (ash header -9) 1))))))

(defun target-listp (taggedptr)
  (= (logand taggedptr lowtag-mask) list-pointer-lowtag))

(defun target-widetag-of (descriptor spacemap)
  (if (target-listp (descriptor-bits descriptor))
      list-pointer-lowtag
      (logand (sap-ref-word (physical-sap descriptor spacemap) 0) widetag-mask)))

(defun fun-entry->descriptor (addr)
  (make-descriptor (+ addr (* -2 n-word-bytes) fun-pointer-lowtag)))

;;; Target objects will be represented by a DESCRIPTOR, making this safe even for
;;; precise GC. i.e. we never load into a register the bits of a target pointer that
;;; could be mistaken for something in the host's dynamic-space.
;;; DESCRIPTOR-BITS has a lowtag so that we can easily discriminate the 4 pointer types.
(macrolet ((scan-slot (index &optional value)
             (if value
                 `(funcall function sap ,index ,value widetag)
                 `(let ((.i. ,index))
                    (funcall function sap .i. (load-wordindexed sap .i.) widetag))))
           (asm-call-p (x name)
             `(eq ,x (load-time-value (sb-fasl:get-asm-routine ,name) t))))

(defun trace-symbol (function sap &aux (widetag symbol-widetag))
  (scan-slot symbol-value-slot)
  (scan-slot symbol-fdefn-slot)
  (scan-slot symbol-info-slot)
  (scan-slot symbol-name-slot ; decode the packed NAME word
   (make-descriptor (ldb (byte 48 0) (load-bits-wordindexed sap symbol-name-slot)))))

;;; This is a less general variant of do-referenced-object, but more efficient.
;;; I think it's the most concisely an object slot visitor can be expressed.
(defun trace-obj (function descriptor spacemap
                  &optional (layout-translator
                             (lambda (ptr) (translate-ptr ptr spacemap)))
                  &aux (widetag (target-widetag-of descriptor spacemap))
                       (sap (physical-sap descriptor spacemap)))
  (declare (function function layout-translator))
  (multiple-value-bind (first last)
      (cond ((= widetag list-pointer-lowtag) (values 0 cons-size))
            ((member widetag `(,instance-widetag ,funcallable-instance-widetag))
             ;; These two primitive types have a bitmap
             (let* ((ld (get-layout sap widetag)) ; layout descriptor
                    (layout
                     (truly-the layout
                      (%make-lisp-obj (funcall layout-translator ld)))))
               (scan-slot 0 (make-descriptor ld))
               (do-layout-bitmap (i taggedp layout (instance-slot-count sap widetag))
                 (when taggedp (scan-slot (1+ i))))
               (return-from trace-obj)))
            ((leafp widetag) (return-from trace-obj))
            ((= widetag symbol-widetag)
             (return-from trace-obj (trace-symbol function sap)))
            ((= widetag code-header-widetag)
             (values 2 ; code_header_words() can't be called from Lisp, so emulate it
                     (ash (ldb (byte 32 0) (load-bits-wordindexed sap code-boxed-size-slot))
                          (- word-shift))))
            (t
             (let ((first 1) (last (ash (size-of sap) (- word-shift))))
               (case widetag
                 (#.fdefn-widetag ; wordindex 3 is an untagged simple-fun entry address
                  (let ((bits (load-bits-wordindexed sap fdefn-raw-addr-slot)))
                    (unless (or (eq bits 0)
                                (asm-call-p bits 'sb-vm::undefined-tramp)
                                (asm-call-p bits 'sb-vm::closure-tramp))
                      (scan-slot fdefn-raw-addr-slot (fun-entry->descriptor bits))))
                  (setq last 3))
                 (#.closure-widetag ; wordindex 1 is an untagged simple-fun entry address
                  (let ((bits (load-bits-wordindexed sap closure-fun-slot)))
                    (scan-slot closure-fun-slot (fun-entry->descriptor bits)))
                  (setq first 2)))
               (values first last))))
    (loop for i from first below last do (scan-slot i))))
) ; end MACROLET

;;; Convert the object at SAP (which represents a tagged lispobj)
;;; into a host proxy for that object, with a few caveats:
;;; - Structure types *must* match the host's type for the classoid,
;;;   or bad things happen.
;;; - Symbols can optionally be returned as instances of CORE-SYM
;;;
;;; Structures use the host's LAYOUT instances. The addresses don't
;;; have to match, but the slots do have to.
;;;
;;; Shared substructure / circularity are OK (I think)
;;;
(defparameter *allowed-instance-types*
  '(sb-c::compiled-debug-info sb-c::debug-source))
(defparameter *ignored-instance-types*
  '("CORE-DEBUG-SOURCE"))

(dolist (type *allowed-instance-types*)
  (let ((dd (find-defstruct-description type)))
    (assert (= (sb-kernel::dd-bitmap dd) +layout-all-tagged+))))

(defun extract-object-from-core (sap core &optional proxy-symbols
                                 &aux (spacemap (core-spacemap core))
                                      (targ-nil (compute-nil-addr spacemap))
                                      ;; address (an integer) -> host object
                                      (seen (make-hash-table)))
  (declare (ignorable proxy-symbols)) ; not done
  (macrolet ((word (i)
               `(sap-ref-word sap (ash ,i word-shift)))
             (memoize (result)
               `(setf (gethash addr seen) ,result)))
    (labels ((recurse (addr)
               (unless (is-lisp-pointer addr)
                 (return-from recurse (%make-lisp-obj addr)))
               (awhen (gethash addr seen) ; NIL is not recorded
                 (return-from recurse it))
               (when (eql addr targ-nil)
                 (return-from recurse nil))
               (let ((sap (int-sap (translate-ptr (logandc2 addr lowtag-mask)
                                                  spacemap))))
                 (flet ((translated-obj ()
                          (%make-lisp-obj (translate-ptr addr spacemap))))
                   (case (logand addr lowtag-mask)
                     (#.list-pointer-lowtag
                      (let ((new (memoize (cons 0 0))))
                        (rplaca new (recurse (word 0)))
                        (rplacd new (recurse (word 1)))
                        new))
                     (#.instance-pointer-lowtag
                      (let* ((layout
                              (truly-the layout
                               (translate (%instance-layout (translated-obj)) spacemap)))
                             (classoid
                              (truly-the classoid
                                (translate (layout-classoid layout) spacemap)))
                             (classoid-name
                              (truly-the symbol
                              (translate (classoid-name classoid) spacemap)))
                             (classoid-name-string
                              (translate (symbol-name classoid-name) spacemap))
                             (allowed
                              (find classoid-name-string *allowed-instance-types*
                                    :test 'string=)))
                        ;; In general, I want to correctly intern the symbol into the host
                        ;; and then perform FIND-LAYOUT on that symbol.
                        ;; These few cases are enough to get by.
                        (cond
                          (allowed
                           (let* ((nslots (%instance-length (translated-obj)))
                                  (new (memoize (%make-instance nslots)))
                                  (exclude-slot-mask
                                   (logior
                                    ;; skip the layout slot if #-compact-instance-header
                                    (if (= instance-data-start 1) 1 0)
                                    0)))
                             (setf (%instance-layout new) (find-layout allowed))
                             (dotimes (i nslots new)
                               (unless (logbitp i exclude-slot-mask)
                                 (setf (%instance-ref new i)
                                       (recurse (word (+ instance-slots-offset i))))))))
                          ((string= classoid-name-string "PACKAGE")
                           ;; oh dear, this is completely wrong
                           (let ((package-name
                                  (translate
                                   (sb-impl::package-%name (truly-the package (translated-obj)))
                                   spacemap)))
                             (memoize (or (find-package package-name)
                                          (make-package package-name)))))
                          ((member classoid-name-string *ignored-instance-types* :test 'string=)
                           (sb-kernel:make-unbound-marker))
                          (t
                           (error "Not done: type ~s" classoid-name-string)))))
                     (#.fun-pointer-lowtag
                      ;; CORE-DEBUG-SOURCE has a :FUNCTION but don't care the value
                      #'error)
                     (#.other-pointer-lowtag
                      (let ((widetag (logand (word 0) widetag-mask)))
                        (cond ((= widetag simple-vector-widetag)
                               (let* ((len (ash (word 1) (- n-fixnum-tag-bits)))
                                      (new (memoize (make-array len))))
                                 (dotimes (i len new)
                                   (setf (aref new i)
                                         (recurse (word (+ vector-data-offset i)))))))
                              ((and (>= widetag #x80) (typep (translated-obj) 'simple-array))
                               (memoize (translated-obj))) ; unboxed array is OK in place
                              ((= widetag symbol-widetag)
                               (let* ((sym (translated-obj))
                                      (name (translate (symbol-name sym) spacemap))
                                      (pkg-name (core-package-from-id (symbol-package-id sym)
                                                                      core)))
                                 (memoize (if (null pkg-name)
                                              (make-symbol name)
                                              (without-package-locks
                                                  (intern name
                                                          (or (find-package pkg-name)
                                                              (make-package pkg-name))))))))
                              ((< widetag symbol-widetag) ; a number of some kind
                               (copy-number-to-heap (translated-obj)))
                              (t
                               (error "can't translate other fancy stuff yet"))))))))))
      (recurse (sap-int sap)))))

(defun compute-nil-symbol-sap (spacemap)
  (let ((space (get-space static-core-space-id spacemap)))
    ;; TODO: The core should store its address of NIL in the initial function entry
    ;; so this kludge can be removed.
    (int-sap (translate-ptr (logior (space-addr space) #x108) spacemap))))

(defun is-code (taggedptr spacemap)
  (and (= (logand taggedptr lowtag-mask) other-pointer-lowtag)
       (= (logand (sap-ref-word (physical-sap taggedptr spacemap) 0) widetag-mask)
          code-header-widetag)))

(defun is-simple-fun (descriptor spacemap)
  (and (= (logand (descriptor-bits descriptor) lowtag-mask) fun-pointer-lowtag)
       (= (logand (sap-ref-word (physical-sap descriptor spacemap) 0) widetag-mask)
          simple-fun-widetag)))

(defun fun-ptr-to-code-ptr (descriptor spacemap)
  (let ((backptr (ldb (byte 24 n-widetag-bits)
                      (sap-ref-word (physical-sap descriptor spacemap) 0))))
    (+ (- (descriptor-bits descriptor) (ash backptr word-shift))
       (- other-pointer-lowtag fun-pointer-lowtag))))

(defun maybe-fun-ptr-to-code-ptr (descriptor spacemap)
  (if (is-simple-fun descriptor spacemap)
      (make-descriptor (fun-ptr-to-code-ptr descriptor spacemap))
      descriptor))

(defun widetag-name (i)
  (if (> i 1) (deref (extern-alien "widetag_names" (array c-string 64)) i) "cons"))

(defun summarize-object-counts (spacemap seen)
  (let ((widetags (make-array 64 :initial-element 0)))
    (maphash (lambda (taggedptr v)
               (declare (ignore v))
               (assert (integerp taggedptr))
               ;; FIXME: should use GET-SPACE to find the vaddr
               (when (>= taggedptr dynamic-space-start)
                 (let* ((descriptor (make-descriptor taggedptr))
                        (widetag
                         (if (target-listp taggedptr)
                             list-pointer-lowtag
                             (logand (sap-ref-word (physical-sap descriptor spacemap) 0)
                                     widetag-mask))))
                   (incf (aref widetags (ash widetag -2))))))
             seen)
    (let ((tot 0))
      (dotimes (i 64)
        (let ((ct (aref widetags i)))
          (when (plusp ct)
            (incf tot ct)
            (format t "~8d ~a~%" ct (widetag-name i)))))
      (format t "~8d TOTAL~%" tot))))

(defun make-visited-table () (make-hash-table))
(defun visited (hashset obj) (setf (gethash (descriptor-bits obj) hashset) t))
(defun unvisited (hashset obj) (remhash (descriptor-bits obj) hashset))
(defun was-visitedp (hashset obj) (gethash (descriptor-bits obj) hashset))

(defun call-with-each-static-object (function spacemap)
  (declare (function function))
  (dolist (id `(,static-core-space-id ,permgen-core-space-id))
    (binding* ((space (get-space id spacemap) :exit-if-null)
               (physaddr (space-physaddr space spacemap))
               (limit (sap+ physaddr (ash (space-nwords space) word-shift))))
      (do ((object (if (= id static-core-space-id)
                       (sap+ (compute-nil-symbol-sap spacemap) (ash 7 word-shift)) ; KLUDGE
                       (sap+ physaddr (ash (+ 256 2) word-shift))) ; KLUDGE
                   (sap+ object (size-of object))))
          ((sap>= object limit))
        ;; There are no static cons cells
        (let ((lowtag (logand (deref (extern-alien "widetag_lowtag" (array char 256))
                                     (sap-ref-8 object 0))
                              lowtag-mask)))
          (funcall function (make-descriptor (+ (space-addr space) (sap- object physaddr)
                                                lowtag))))))))

;;; Gather all the objects in the order we want to reallocate them in.
;;; This relies on MAPHASH in SBCL iterating in insertion order.
(defun visit-everything (spacemap initfun
                         &optional print
                         &aux (seen (make-visited-table))
                              (defer-debug-info
                                  (make-array 10000 :fill-pointer 0 :adjustable t))
                              stack)
  (visited seen (make-descriptor nil-value))
  (labels ((root (descriptor)
             (visited seen descriptor)
             (trace-obj #'visit descriptor spacemap))
           (visit (sap slot value widetag)
             (declare (ignorable sap slot widetag))
             (when print (format t "~&  slot ~d = ~a" slot value))
             (when (and (= widetag code-header-widetag) (< slot 4) defer-debug-info)
               (unless (and (plusp (fill-pointer defer-debug-info))
                            (sap= (aref defer-debug-info (1- (fill-pointer defer-debug-info)))
                                  sap))
                 (vector-push-extend sap defer-debug-info))
               (return-from visit))
             (when (descriptor-p value)
               (let ((value (maybe-fun-ptr-to-code-ptr value spacemap)))
                 (unless (was-visitedp seen value)
                   (when print (format t " (pushed)"))
                   (visited seen value)
                   (push value stack)))))
           (transitive-closure ()
             (loop while stack
                   do (let ((descriptor (pop stack)))
                        (when print (format t "~&Popped ~x~%" descriptor))
                        (trace-obj #'visit descriptor spacemap)))))
    (root (make-descriptor initfun))
    (trace-symbol #'visit (compute-nil-symbol-sap spacemap))
    (call-with-each-static-object #'root spacemap)
    (transitive-closure)
    (dovector (sap (prog1 defer-debug-info (setq defer-debug-info nil)))
      (visit sap 2 (load-wordindexed sap 2) 0)
      (visit sap 3 (load-wordindexed sap 3) 0))
    (transitive-closure))
  seen)

(defstruct (newspace (:include core-space))
  ;; alist of page type to list of pages with any space
  (available-ranges (mapcar 'list '(:cons :boxed :unboxed :mixed :code))))

(defun unboxed-like-simple-vector (sap)
  (declare (ignore sap))
  ;; TODO: return T if the vector has the 'shareable' header bit (is effectively
  ;; a constant) and contains no pointers.
  nil)

(defun vector-alloc-mixed-p (sap)
  (logtest (logior (ash (logior vector-weak-flag vector-hashing-flag) array-flags-position)
                   sb-vm::+vector-alloc-mixed-region-bit+)
           (sap-ref-word sap 0)))

(defun instance-strictly-boxed-p (sap spacemap)
  (let ((layout (translate-ptr (get-layout sap instance-widetag) spacemap)))
    (logtest (layout-flags (truly-the layout (%make-lisp-obj layout)))
             +strictly-boxed-flag+)))

(defun pick-page-type (descriptor sap largep spacemap)
  (when (target-listp (descriptor-bits descriptor))
    (return-from pick-page-type :cons))
  (let ((widetag (target-widetag-of descriptor spacemap)))
    (if largep
        ;; Choose from among {boxed, mixed, code}
        (cond ((= widetag code-header-widetag) :code)
              ((or (/= widetag simple-vector-widetag) (vector-alloc-mixed-p sap))
               :mixed)
              (t
               :boxed))
        ;; Choose from among {raw, boxed, mixed, code}
        (cond ((member widetag `(,code-header-widetag ,funcallable-instance-widetag))
               :code)
              ((or (leafp widetag)
                   (and (member widetag `(,ratio-widetag ,complex-rational-widetag))
                        (fixnump (load-wordindexed sap 1))
                        (fixnump (load-wordindexed sap 2)))
                   (unboxed-like-simple-vector sap))
               :unboxed)
              ((or (member widetag `(,symbol-widetag ,weak-pointer-widetag ,fdefn-widetag))
                   (and (= widetag instance-widetag)
                        (not (instance-strictly-boxed-p sap spacemap)))
                   (and (= widetag simple-vector-widetag)
                        (vector-alloc-mixed-p sap)))
               :mixed)
              (t
               :boxed)))))

(defun find-sufficient-gap (gaps size)
  (dolist (gap gaps)
    (when (>= (cdr gap) size)
      (return gap))))

(defun space-next-free-page (space)
  (let ((words-per-page (/ gencgc-page-bytes n-word-bytes)))
    (ceiling (space-nwords space) words-per-page)))

(defconstant-eqx instance-len-byte
    (byte (integer-length instance-length-mask) instance-length-shift)
  #'equal)
(defun reallocate (descriptor old-spacemap new-spacemap)
  (let* ((sap (physical-sap descriptor old-spacemap))
         (old-size (size-of sap))
         (size old-size)
         ;;; Prevent page-spanning small objects for gencgc. Reorganizing is
         ;;; primarily for mark-region GC which disallows page-spanning other than
         ;;; for large objects. With gencgc we'd have to compute the scan-start
         ;;; on subsequent pages, and put the end-of-page free space in a list.
         ;;; It's not worth the hassle.
         (largep #+gencgc (>= size gencgc-page-bytes)
                 #-gencgc (>= size large-object-size))
         (page-type (pick-page-type descriptor sap largep old-spacemap))
         (newspace (get-space dynamic-core-space-id new-spacemap))
         (new-nslots) ; only set if it's a resized instance
         (new-vaddr))
    (when (and (= (logand (descriptor-bits descriptor) lowtag-mask)
                  instance-pointer-lowtag)
               (= (ldb (byte 2 8) (sap-ref-word sap 0)) 1)) ; hashed not moved
      (setf new-nslots (1+ (ldb instance-len-byte (sap-ref-word sap 0)))
            ;; size change can't affect largep
            size (ash (1+ (logior new-nslots 1)) word-shift)))
    (flet ((claim-page (scan-start words-used)
             (let ((index (space-next-free-page newspace)))
               ;(format t "next-free-page=~s~%" index)
               ;; should be free
               (aver (not (aref (space-page-table newspace) index)))
               ;; previous should be used or nonexistent
               (aver (or (= index 0) (aref (space-page-table newspace) (1- index))))
               (incf (space-nwords newspace) (/ gencgc-page-bytes n-word-bytes))
               (setf (aref (space-page-table newspace) index)
                     (make-page :words-used words-used
                                :single-obj-p (if largep 1 0)
                                :type page-type
                                :scan-start scan-start
                                :bitmap (make-array *bitmap-bits-per-page*
                                                    :element-type 'bit))))))
      (if largep
          (let* ((npages (ceiling size gencgc-page-bytes))
                 (page (space-next-free-page newspace))
                 (bytes-to-go size)
                 (scan-start 0))
            (setq new-vaddr (+ dynamic-space-start (* page gencgc-page-bytes)))
            (dotimes (i npages)
              (let ((bytes-this-page (min gencgc-page-bytes bytes-to-go)))
                (claim-page scan-start (ash bytes-this-page (- word-shift)))
                (incf scan-start gencgc-page-bytes)
                (decf bytes-to-go bytes-this-page)))
            (setf (bit (page-bitmap (aref (newspace-page-table newspace) page)) 0) 1))
          (let* ((list (assoc page-type (newspace-available-ranges newspace)))
                 (nwords (ash size (- word-shift)))
                 (gap (find-sufficient-gap (cdr list) size)))
            (if gap
                (let* ((page (floor (- (setq new-vaddr (car gap)) dynamic-space-start)
                                    gencgc-page-bytes))
                       (pte (aref (newspace-page-table newspace) page))
                       (page-base (+ dynamic-space-start (* page gencgc-page-bytes)))
                       (object-index (ash (- new-vaddr page-base) (- n-lowtag-bits))))
                  (incf (page-words-used pte) nwords)
                  (setf (sbit (page-bitmap pte) object-index) 1)
                  (aver (>= (cdr gap) size))
                  (if (plusp (decf (cdr gap) size))
                      (incf (car gap) size) ; the gap is moved upward by SIZE
                      (setf (cdr list) (delq1 gap (cdr list)))))
                ;; claim a page, use initial portion of it, append rest onto gaps
                (let ((page (space-next-free-page newspace)))
                  (setq new-vaddr (+ dynamic-space-start (* page gencgc-page-bytes)))
                  (setf (bit (page-bitmap (claim-page 0 nwords)) 0) 1)
                  (let ((gap (cons (+ new-vaddr size) (- gencgc-page-bytes size))))
                    (aver (> (cdr gap) 0))
                    (nconc list (list gap))))))))
    (let ((new-sap (physical-sap new-vaddr new-spacemap))
          (old-sap (physical-sap descriptor old-spacemap))
          (widetag (target-widetag-of descriptor old-spacemap)))
      (%byte-blt old-sap 0 new-sap 0 old-size)
      (case widetag
        (#.instance-widetag
         (when new-nslots
           (setf (sap-ref-word new-sap 0)
                 (logior (sap-ref-word new-sap 0) (ash 1 hash-slot-present-flag)))
           (let* ((prehash (sb-impl::murmur3-fmix-word (descriptor-bits descriptor)))
                  (hash (ash (logand (ash prehash (1+ n-fixnum-tag-bits)) most-positive-word)
                             -1)))
             (setf (sap-ref-word new-sap (ash new-nslots word-shift)) hash))))
        (#.code-header-widetag ; redundantly performs a memmove first, but that's fine
         (transport-code (int-sap (logandc2 (descriptor-bits descriptor) lowtag-mask)) old-sap
                         (int-sap new-vaddr) new-sap size))
        (#.funcallable-instance-widetag
         (setf (sap-ref-sap new-sap (ash 1 word-shift)) ; set the entry point
               (sap+ (int-sap new-vaddr) (ash 2 word-shift))))))
    new-vaddr))

(defun fixup-compacted (old-spacemap new-spacemap seen &optional print)
  (labels
      ((visit (sap slot value widetag)
           (unless (descriptor-p value) (return-from visit))
           (let ((newspace-ptr (forward value)))
             (unless newspace-ptr (return-from visit))
             ;; handle special cases of index + widetag
             (case (logand most-positive-word (logior (ash slot 8) widetag))
               ((#.instance-widetag #.funcallable-instance-widetag)
                (set-layout sap widetag newspace-ptr))
               ((#.(logior (ash fdefn-raw-addr-slot 8) fdefn-widetag)
                 #.(logior (ash closure-fun-slot 8) closure-widetag))
                (setf (sap-ref-word sap (ash slot word-shift))
                      (+ newspace-ptr (- (ash simple-fun-insts-offset word-shift)
                                         fun-pointer-lowtag))))
               ((#.(logior (ash symbol-name-slot 8) symbol-widetag))
                ;; symbol names are in readonly space now
                (bug "symbol name change"))
               (t
                (setf (sap-ref-word sap (ash slot word-shift)) newspace-ptr)))
             #+nil
             (format t "~& - ~x[~x] + ~d ~x -> ~X" (sap-int sap) widetag index value
                     (sap-ref-word sap (ash slot word-shift)))
             ))
       (forward (value)
         (cond ((is-simple-fun value old-spacemap)
                (let* ((old-code (fun-ptr-to-code-ptr value old-spacemap))
                       (new-code (gethash old-code seen)))
                  (+ new-code (- (descriptor-bits value) old-code))))
               ((gethash (descriptor-bits value) seen))))
       ;; Use _oldspace_ layouts when scanning bitmaps.
       (layout-vaddr->paddr (ptr)
           (translate-ptr ptr old-spacemap)))
    (when print (format t "~&Fixing static space~%"))
    (trace-symbol #'visit (compute-nil-symbol-sap old-spacemap))
    (call-with-each-static-object
       (lambda (descriptor) (trace-obj #'visit descriptor old-spacemap))
       old-spacemap)
    (when print (format t "~&Fixing dynamic space~%"))
    (dohash ((old-taggedptr new-taggedptr) seen)
      (declare (ignorable old-taggedptr))
      #+nil
      (format t "~&Fixing @ old-vaddr=~x new-vaddr=~x new-paddr=~x~%"
              old-taggedptr new-taggedptr (sap-int (physical-sap new-taggedptr new-spacemap)))
      (trace-obj #'visit (make-descriptor new-taggedptr) new-spacemap
                 #'layout-vaddr->paddr)
      ;; mark every address-sensitive hash-table as needing rehash
      (let* ((sap (physical-sap new-taggedptr new-spacemap))
             (header (sap-ref-word sap 0)))
        (when (and (= (logand header widetag-mask) simple-vector-widetag)
                   (logtest header (ash vector-addr-hashing-flag array-flags-position)))
          (setf (sap-ref-word sap (ash 3 word-shift)) (fixnumize 1)))))))

(defun reorganize-core (input-pathname output-pathname &optional print)
  (with-open-file (input input-pathname :element-type '(unsigned-byte 8))
    (let* ((core-header (make-array +backend-page-bytes+ :element-type '(unsigned-byte 8)))
           (core-offset (read-core-header input core-header))
           (parsed-header (parse-core-header input core-header))
           (space-list (core-header-space-list parsed-header)))
      (with-mapped-core (sap core-offset (core-header-total-npages parsed-header) input)
        ;; FIXME: WITH-MAPPED-CORE should bind spacemap
        (let* ((spacemap (cons sap (sort (copy-list space-list) #'> :key #'space-addr)))
               (seen (visit-everything spacemap
                                       (core-header-initfun parsed-header)
                                       print))
               (oldspace (get-space dynamic-core-space-id spacemap)))
          ;;(summarize-object-counts spacemap seen)
          (let* ((oldspace-size (ash (space-nwords oldspace) word-shift))
                 (newspace-mem (sb-sys:allocate-system-memory oldspace-size))
                 (newspace (make-newspace
                            :id dynamic-core-space-id
                            :addr dynamic-space-start
                            :page-table (make-array (length (space-page-table oldspace))
                                                    :initial-element nil)
                            :data-page 0
                            :nwords 0))
                 (new-spacemap (list newspace-mem newspace)))
            (alien-funcall (extern-alien "memset" (function void system-area-pointer int unsigned))
                           newspace-mem 0 oldspace-size)
            ;; pass 1: assign new address per object, also copy the object over.
            ;; But actually do two sub-passes, one to copy everything except code,
            ;; one for code, so that all code is at the end of the space.
            ;; FIXME: linearize lists (of conses and lockfree) just like gencgc does.
            (dotimes (sub-pass 2)
              (dohash ((taggedptr dummy) seen)
                (declare (ignore dummy))
                (when (<= (space-addr oldspace) taggedptr (space-end oldspace))
                  (when (eq (is-code taggedptr spacemap) (eql sub-pass 1))
                    (let* ((new-addr (reallocate (make-descriptor taggedptr)
                                                 spacemap new-spacemap))
                           (new-taggedptr (logior new-addr (logand taggedptr lowtag-mask))))
                      (setf (gethash taggedptr seen) new-taggedptr)))))
              ;; Prevent sharing of funinstance pages with code from next pass
              (let ((avail (assoc :code (newspace-available-ranges (second new-spacemap)))))
                (setf (cdr avail) nil)))
            ;; pass 2: visit every object again, fixing pointers
            ;; Start by removing objects from SEEN that were not forwarded
            (maphash (lambda (key value) (if (eq value t) (remhash key seen))) seen)
            (fixup-compacted spacemap new-spacemap seen)
            (setf (core-header-initfun parsed-header)
                  (gethash (core-header-initfun parsed-header) seen))
            (flet ((n (spaces)
                     (space-next-free-page (get-space dynamic-core-space-id spaces))))
              (when print
                (format t "Compactor: n-pages was ~D, is ~D~%" (n spacemap) (n new-spacemap))))
            ;; Shrink the page table and encode the page types as required
            (setf (space-page-table newspace)
                  (subseq (space-page-table newspace) 0 (space-next-free-page newspace)))
            (dovector (pte (space-page-table newspace))
              (setf (page-type pte) (encode-page-type (page-type pte))))
            ;; Switch dynamic space in spacemap to that of compacted space
            (let* ((cell (member dynamic-core-space-id (cdr spacemap) :key #'space-id))
                   (oldspace (car cell))
                   (oldspace-mem (space-physaddr oldspace spacemap))
                   (nbytes (ash (space-nwords newspace) word-shift)))
              (%byte-blt newspace-mem 0 oldspace-mem 0 nbytes)
              ;; don't clobber the physical address translation of oldspace
              (setf (space-data-page newspace) (space-data-page oldspace))
              (rplaca cell newspace)))
          (with-open-file (output output-pathname :direction :output
                                  :element-type '(unsigned-byte 8) :if-exists :supersede)
            (rewrite-core
             (mapcar (lambda (space)
                       (list 0 (space-id space) (space-physaddr space spacemap)
                             (space-addr space) (space-nwords space)))
                     (cdr spacemap))
             core-header parsed-header spacemap output)))))))
