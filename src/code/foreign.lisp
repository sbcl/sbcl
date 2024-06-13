;;;; Foreign symbol linkage

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

(define-alien-routine arch-write-linkage-table-entry void
  (index int) (real-address unsigned) (datap int))
(define-alien-variable undefined-alien-address unsigned)

(define-load-time-global *linkage-info*
    ;; CDR of the cons is the list of undefineds
    (list (make-hash-table :test 'equal :synchronized t)))
(declaim (type (cons hash-table) *linkage-info*))

(macrolet ((dlsym-wrapper (&optional warn)
             ;; Produce two values: an indicator of whether the foreign symbol was
             ;; found; and the address as an integer if found, or a guard address
             ;; which when accessed will result in an UNDEFINED-ALIEN-ERROR.
             `(let ((addr (find-dynamic-foreign-symbol-address name)))
                (cond (addr
                       (values t addr))
                      (t
                       (when ,warn
                         ;; If we can report the actual name when an undefined
                         ;; alien is called don't warn.
                         #-(or arm arm64 x86-64)
                         (style-warn 'sb-kernel:undefined-alien-style-warning
                                     :symbol name))
                       (values
                        nil
                        (if datap
                            undefined-alien-address
                            (or
                             (sb-fasl:get-asm-routine 'sb-vm::undefined-alien-tramp)
                             (find-foreign-symbol-address "undefined_alien_function")
                             (unreachable)))))))))

;;; Return the index of NAME+DATAP in the table, adding it if it doesn't exist.
(defun ensure-alien-linkage-index (name datap)
  (let* ((key (if datap (list name) name))
         (info *linkage-info*)
         (ht (car info)))
    (or (with-system-mutex ((hash-table-lock ht))
          (or (gethash key ht)
              (let* ((index (hash-table-count ht))
                     (capacity (floor sb-vm:alien-linkage-space-size
                                      sb-vm:alien-linkage-table-entry-size)))
                (when (< index capacity)
                  (multiple-value-bind (defined real-address) (dlsym-wrapper t)
                    (unless defined (push key (cdr info)))
                    (arch-write-linkage-table-entry index real-address (if datap 1 0))
                    (logically-readonlyize name)
                    (setf (gethash key ht) index))))))
        (error "Linkage-table full (~D entries): cannot link ~S."
               (hash-table-count ht) name))))

;;; Update the linkage-table. Called during initialization after all
;;; shared libraries have been reopened, and after a previously loaded
;;; shared object is reloaded.
;;;
;;; FIXME: Should figure out how to write only those entries that need
;;; updating.
;;; The problem is that when unloading a library, lacking any way to know which
;;; symbols came from it, we have to try to find every symbol again.
;;; If the shared-object-handle in which each symbol was originally found were
;;; stored in linkage-info, we could know which will become undefined on unload.
;;; The only "problem" is my lack of motivation to change this further.
(defun update-alien-linkage-table (full-scan)
  ;; This symbol is of course itself a prelinked symbol.
  (let* ((n-prelinked (extern-alien "alien_linkage_table_n_prelinked" int))
         (info *linkage-info*)
         (ht (car info))
         ;; for computing anew the list of undefined symbols
         (notdef))
    (flet ((recheck (key index)
             (let* ((datap (listp key))
                    (name (if datap (car key) key)))
               ;; Symbols required for Lisp startup
               ;; will not be re-pointed to a different address ever.
               ;; Nor will those referenced by ELF core.
               (when (>= index n-prelinked)
                 (multiple-value-bind (defined real-address) (dlsym-wrapper)
                   (unless defined (push key notdef))
                   (arch-write-linkage-table-entry index real-address
                                                   (if datap 1 0)))))))
    (with-system-mutex ((hash-table-lock ht))
      (if full-scan
          ;; Look up everything; this is for image restart or library unload.
          (dohash ((key index) ht)
            (recheck key index))
          ;; Look up only the currently undefined foreign symbols
          (dolist (key (cdr info))
            (recheck key (the (not null) (gethash key ht)))))
      (setf (cdr info) notdef)))))
)

(defun find-foreign-symbol-address (name)
  "Returns the address of the foreign symbol NAME, or NIL. Does not enter the
symbol in the linkage table, and never returns an address in the linkage-table."
  (find-dynamic-foreign-symbol-address name))

;;; Note that much conditionalization is for nothing at this point, because all
;;; platforms that we care about implement dlopen(). But if one did not, only
;;; supporting static linking, we could still implement the entirety of the feature
;;; formerly known as "dynamic core" by mocking out dlsym() within the SBCL runtime
;;; as a lookup table translating strings to functions needed in our runtime.
;;; It's not our problem that shared objects aren't loadable, but we get the
;;; flexibility of recompiling C without recompiling Lisp.
;;;
;;; This function is somewhat badly named, because when DATAP is true,
;;; the answer is not really the address of NAME, but rather the address
;;; of the word in the alien-linkage-table holding the address of NAME.
;;; (This would be better off named ALIEN-LINKAGE-ADDRESS)
;;; Unfortunately we can not rename it, because CFFI uses it, which is weird
;;; because the use is from a function named %FOREIGN-SYMBOL-POINTER which is
;;; documented to return "a pointer to a foreign symbol NAME."
;;; which it certainly does not do in all cases.
(defun foreign-symbol-address (name &optional datap)
  "Returns the address of the foreign symbol NAME. DATAP must be true if the
symbol designates a variable.
Returns a secondary value T for historical reasons.

The returned address is always a linkage-table address.
Symbols are entered into the linkage-table if they aren't there already."
  (declare (ignorable datap))
  (let ((index (ensure-alien-linkage-index name datap)))
    (values (sb-vm::alien-linkage-table-entry-address index) t)))

(defun foreign-symbol-sap (symbol &optional datap)
  "Returns a SAP corresponding to the foreign symbol. DATAP must be true if the
symbol designates a variable. May enter the symbol into the linkage-table."
  (let ((addr (foreign-symbol-address symbol datap)))
    (if datap ; return the real answer, not an address in the linkage table
        (sap-ref-sap (int-sap addr) 0)
        (int-sap addr))))

(defun foreign-reinit ()
  #+os-provides-dlopen (reopen-shared-objects)
  (update-alien-linkage-table t))

;;; Cleanups before saving a core
(defun foreign-deinit ()
  ;; Clobber list of undefineds. Reinit will figure it all out again.
  (setf (cdr *linkage-info*) nil)
  #+os-provides-dlopen
  (close-shared-objects))

(defun alien-linkage-index-to-name (index)
  ;; Entries are never removed from the linkage-table, so we can take advantage
  ;; of the fact that indices are inserted in order, and hash-table growth
  ;; preserves order. So we know where the entry is.
  ;; INDEX should be of type HASH-TABLE-INDEX but this might be called
  ;; on an arbitrary (bogus) value which exceeds that.
  (declare (fixnum index))
  (let* ((table (car *linkage-info*))
         (pairs (sb-impl::hash-table-pairs table))
         (pair-index (+ (* index 2) 2))
         (key
          (cond ((>= index (hash-table-count table)) nil)
                ((eql (aref pairs (1+ pair-index)) index) (aref pairs pair-index))
                (t (block found ; "shouldn't happen"
                     (dohash ((key value) table) ; no lock necessary
                       (when (= value index) (return-from found key))))))))
    (if (listp key) (car key) key)))

(declaim (maybe-inline sap-foreign-symbol))
(defun sap-foreign-symbol (sap)
  (declare (ignorable sap))
  (let ((addr (sap-int sap)))
    (declare (ignorable addr))
    (when (<= sb-vm:alien-linkage-space-start
              addr
              (+ sb-vm:alien-linkage-space-start sb-vm:alien-linkage-space-size))
      (return-from sap-foreign-symbol
        (alien-linkage-index-to-name
         (sb-vm::alien-linkage-table-index-from-address addr))))
    #+os-provides-dladdr
    (with-alien ((info (struct dl-info
                               (filename c-string)
                               (base unsigned)
                               (symbol c-string)
                               (symbol-address unsigned)))
                 (dladdr (function unsigned unsigned (* (struct dl-info)))
                         :extern "dladdr"))
      ;; A comment in rev 7143001bbe7d50c6 said: "Darwin GC could otherwise
      ;; interrupt the call while dladdr is holding a lock." which makes little
      ;; sense, as GC doesn't acquire locks other than its own allocator locks.
      ;; How exactly was this deadlocking?  A reductio ad absurdum argument
      ;; says that every call into a system API could potentially acquire
      ;; a lock, and therefore every one should inhibit GC. But they don't.
      ;; However: We now try to allow libdl to acquire its internal locks in a GCing
      ;; thread, which means that we need all user threads to agree not to stop
      ;; for GC in the midst of _any_ libdl call.
      (let ((err (sb-vm:with-pseudo-atomic-foreign-calls
                     (alien-funcall dladdr addr (addr info)))))
        (if (zerop err)
            nil
            (slot info 'symbol))))
    ;; FIXME: Even in the absence of dladdr we could search the
    ;; static foreign symbols (and *linkage-info*, for that matter).
    ))

(defun !foreign-cold-init ()
  (declare (special *runtime-dlhandle* *shared-objects*))
  (loop for table-offset from 0
        and reference across (symbol-value 'sb-vm::+required-foreign-symbols+)
        do (setf (gethash reference (car *linkage-info*)) table-offset))
  #+os-provides-dlopen
  (setf *runtime-dlhandle* (dlopen-or-lose))
  #+os-provides-dlopen
  (setf *shared-objects* nil))

;;; Helpers for defining error-signalling NOP's for "not supported
;;; here" operations.
(defmacro define-unsupported-fun (name &optional
                                  (doc "Unsupported on this platform.")
                                  (control
                                   "~S is unsupported on this platform ~
                                    (OS, CPU, whatever)."
                                   controlp)
                                  arguments)
  `(defun ,name (&rest args)
     ,doc
     (declare (ignore args))
     (error 'unsupported-operator
            :format-control ,control
            :format-arguments (if ,controlp ',arguments (list ',name)))))

#-os-provides-dlopen
(define-unsupported-fun load-shared-object)
