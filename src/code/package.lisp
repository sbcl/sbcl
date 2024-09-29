;;;; that part of the CMU CL package.lisp file which can run on the
;;;; cross-compilation host

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;;; the SYMBOL-TABLE structure

;;; Packages are implemented using a special kind of hashtable -
;;; the storage is a single vector in which each cell is both key and value.
;;; While previously we also used a parallel vector of 8-bit hash codes,
;;; that turned out to be a pessimization on modern systems, where memory
;;; can be be read faster than it was 27 years ago. (That's when it was
;;; discovered - so it is claimed - that without a comparison for hash,
;;; the majority of time loading fasls was due to paging in symbol names)
;;;
;;; But hardware-based prefetch makes it so that it costs no more to compare
;;; a symbol's hash code than to first compare a smaller hash that doesn't
;;; require dereferencing the symbol, only to use it as the guard condition
;;; to compare the full symbol-hash, and only then the symbol-name.
;;;
;;; In fairness to the former implementation, for technical reasons
;;; it was not possible to use SYMBOL-HASH as the hash value by which
;;; to avoid spurious STRING= calls that would definitely fail.
;;; Prior to change 06ccf13ed3, there were two reasons for that:
;;;   (1) we didn't force interned symbols to have a precomputed hash
;;;   (2) NIL has a strange hash. That was resolved by making any random
;;;       symbol whose name is spelled "NIL" have the identical strange hash
;;;       so that the hash is a pure function of the name's characters.

(defconstant package-id-bits 16)
(defconstant +package-id-overflow+ (1- (ash 1 package-id-bits)))
(defconstant +package-id-none+     0)
(defconstant +package-id-lisp+     1)
(defconstant +package-id-keyword+  2)
(defconstant +package-id-user+     3)
(defconstant +package-id-kernel+   4)

(sb-xc:defstruct (symtbl-magic (:conc-name "SYMTBL-")
                  (:copier nil)
                  (:predicate nil)
                  (:constructor make-symtbl-magic (hash1-mask hash1-c hash2-mask)))
  (hash1-mask 0 :type (unsigned-byte 32))
  (hash1-c    0 :type (unsigned-byte 32))
  ;; These values were both needed for the secondary hash but they aren't now
  ;; because the secondary hash is not computed by taking a remainder. It's just a mask.
  (hash2-mask 0 :type (unsigned-byte 32))
  ;(hash2-c    0 :type (unsigned-byte 32))
  ;; Every extant package iterator (in any thread) can vote to make a table immutable.
  ;; This affects ADD-SYMBOL but not NUKE-SYMBOL, the latter being informed by
  ;; *CLEAR-RESIZED-SYMBOL-TABLES* as to zero-filling or not.
  (immutable 0 :type sb-vm:word) ; copy-on-write if immutable > 0
  )

(sb-xc:defstruct (symbol-table
                  (:conc-name "SYMTBL-")
                  (:predicate nil)
                  (:constructor %make-symbol-table
                                (%cells size &aux (free size)))
                  (:copier nil))
  ;; An extra indirection to the symbol vector allows atomically changing the symbols
  ;; and the division magic parameters.
  (%cells (missing-arg) :type (cons (or symtbl-magic (function (hash-code) index))
                                    simple-vector))
  (modified nil :type boolean)
  (package nil :type (or null package)) ; backpointer, only if externals
  ;; SIZE is roughly related to the number of symbols the client code asked to be
  ;; able to store. We increase that to a prime number, then scale it down to compute
  ;; a rehash trigger level. The only reason we need to remember this number is that
  ;; computing the count of things in the table has to be done by subtracting FREE
  ;; and DELETED from the originally computed size.
  (size (missing-arg) :type index)
  ;; The remaining number of entries that can be made before we have to rehash.
  (free (missing-arg) :type index)
  ;; The number of deleted entries.
  (deleted 0 :type index))

(sb-xc:defstruct (pkg-iter (:constructor pkg-iter (pkglist enable)))
  (symbols #() :type simple-vector)
  (cur-index 0 :type index)
  (snapshot nil :type list) ; immutable view of internals
  (exclude nil :type list) ; shadowing symbols, when and only when in state 2
  ;; The BITS slot is composed of 2 packed fields:
  ;;  [0:1] = state {-1=initial,0=externals,1=internals,2=inherited}
  ;;  [2:]  = index into 'package-tables'
  (bits -1 :type fixnum)
  (enable 0 :type (unsigned-byte 3) :read-only t) ; 1 bit per {external,internal,inherited}
  (pkglist nil :type list))

;;;; the PACKAGE structure

(sb-xc:defstruct (package
                  (:constructor %make-package
                                (internal-symbols external-symbols))
                  (:copier nil)
                  (:predicate packagep))
  "the standard structure for the description of a package"
  ;; the name of the package, or NIL for a deleted package
  ;; This is redundant with the 0th element of KEYS
  (%name nil :type (or simple-string null))
  ;; A small integer ID, unless it overflows the global package index
  (id nil :type (or (unsigned-byte #.package-id-bits) null))
  ;; Unlike the slot formerly known as %NICKNAMES, the KEYS
  ;; vector includes the primary name and all nicknames
  (keys #() :type simple-vector) ; alternating STRING + HASH
  ;; This is essentially the same as the USE-LIST, but it points
  ;; to the external symbol hashtables directly.
  ;; From it we can obtain PACKAGE-USE-LIST on demand.
  (tables #() :type simple-vector)
  ;; index into TABLES of the table in which an inherited symbol was most
  ;; recently found. On the next %FIND-SYMBOL operation, the indexed table
  ;; is tested first.
  (mru-table-index 0 :type index)
  ;; packages that use this package
  (%used-by nil :type (or null weak-pointer))
  ;; SYMBOL-TABLEs of internal & external symbols
  (internal-symbols nil :type symbol-table)
  (external-symbols nil :type symbol-table)
  ;; shadowing symbols
  ;; Todo: dynamically changeover to a SYMBOL-TABLE if list gets long
  (%shadowing-symbols () :type list)
  ;; documentation string for this package
  (doc-string nil :type (or simple-string null))
  ;; package locking
  (%bits 0 :type (and fixnum unsigned-byte))
  (%implementation-packages nil :type list)
  ;; Mapping of local nickname to actual package.
  ;; One vector stores the mapping sorted by string, the other stores it by
  ;; integer nickname ID so that we can binary search in either.
  ;; This looks liks a more complicated representation than it has to be.
  ;; Strictly speaking, that's true. But it helps optimize (find-symbol x "constant")
  ;; where *PACKAGE* may specify a package-local nickname involving "constant".
  ;; By translating that string to a small integer at load-time, we can seek the integer
  ;; in a nickname mapping more quickly than looking for a string. However, there is
  ;; redundancy now that each package gets an integer ID (to compresss the backlinks
  ;; from symbols to packages). Nickname IDs were invented first, and it is certainly
  ;; confusing that there are 2 distinct spaces of small integer IDs.
  (%local-nicknames nil :type (or null (cons simple-vector weak-vector)))
  ;; Definition source location
  (source-location nil :type (or null sb-c:definition-source-location)))
(proclaim '(freeze-type symbol-table package))

(defconstant +initial-package-bits+ 2) ; for genesis

#-sb-xc-host
(defmacro system-package-p (package) ; SBCL stuff excluding CL and KEYWORD
  `(logbitp 1 (package-%bits ,package)))

(defmacro package-lock (package) `(logbitp 0 (package-%bits ,package)))

(defmacro without-package-locks (&body body)
  "Ignores all runtime package lock violations during the execution of
body. Body can begin with declarations."
  `(let ((*ignored-package-locks* t))
    ,@body))

(defmacro with-loader-package-names (&body body)
  #+sb-xc-host
  `(progn ,@body)
  #-sb-xc-host
  `(call-with-loader-package-names (lambda () ,@body)))

;;;; IN-PACKAGE
(proclaim '(special *package*))
(sb-xc:defmacro in-package (string-designator)
  (let ((string (string string-designator)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setq *package* (find-undeleted-package-or-lose ,string)))))
