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

(defun find-foreign-symbol-address (name)
  "Returns the address of the foreign symbol NAME, or NIL. Does not enter the
symbol in the linkage table, and never returns an address in the linkage-table."
  (or #-linkage-table
      (find-foreign-symbol-in-table name *static-foreign-symbols*)
      (find-dynamic-foreign-symbol-address name)))

;;; Note that much conditionalization is for nothing at this point, because all
;;; platforms that we care about implement dlopen(). But if one did not, only
;;; supporting static linking, we could still implement the entirety of the feature
;;; known as :linkage-table and the feature formerly known as "dynamic core"
;;; (folded into :linkage-table now) by mocking out dlsym() within the SBCL runtime
;;; as a lookup table translating strings to functions needed in our runtime.
;;; It's not our problem that shared objects aren't loadable, but we get the
;;; flexibility of recompiling C without recompiling Lisp.
(defun foreign-symbol-address (name &optional datap)
  "Returns the address of the foreign symbol NAME. DATAP must be true if the
symbol designates a variable (used only on linkage-table platforms).
Returns a secondary value T if the symbol is a dynamic foreign symbol.

On linkage-table ports the returned address is always static: either direct
address of a static symbol, or the linkage-table address of a dynamic one.
Dynamic symbols are entered into the linkage-table if they aren't there already.

On non-linkage-table ports signals an error if the symbol isn't found."
  (declare (ignorable datap))
  #+linkage-table
  (values (ensure-foreign-symbol-linkage name datap) t)
  #-linkage-table
  (let ((static (find-foreign-symbol-in-table name *static-foreign-symbols*)))
    (if static
        (values static nil)
        #+os-provides-dlopen (values (ensure-dynamic-foreign-symbol-address name) t)
        #-os-provides-dlopen (error 'undefined-alien-error :name name))))

(defun foreign-symbol-sap (symbol &optional datap)
  "Returns a SAP corresponding to the foreign symbol. DATAP must be true if the
symbol designates a variable (used only on linkage-table platforms). May enter
the symbol into the linkage-table. On non-linkage-table ports signals an error
if the symbol isn't found."
  (declare (ignorable datap))
  #-linkage-table
  (int-sap (foreign-symbol-address symbol))
  #+linkage-table
  (multiple-value-bind (addr sharedp)
      (foreign-symbol-address symbol datap)
    ;; If the address is from linkage-table and refers to data
    ;; we need to do a bit of juggling. It is not the address of the
    ;; variable, but the address where the real address is stored.
    (if (and sharedp datap)
        (int-sap (sap-ref-word (int-sap addr) 0))
        (int-sap addr))))

(defun foreign-reinit ()
  #+os-provides-dlopen (reopen-shared-objects)
  #+linkage-table (update-linkage-table t))

;;; Cleanups before saving a core
(defun foreign-deinit ()
  #+(and os-provides-dlopen (not linkage-table)) ; at most HPPA and Alpha now
  (when (dynamic-foreign-symbols-p)
    (warn "~@<Saving cores with alien definitions referring to non-static ~
           foreign symbols is unsupported on this platform: references to ~
           such foreign symbols from the restarted core will not work. You ~
           may be able to work around this limitation by reloading all ~
           foreign definitions and code using them in the restarted core, ~
           but no guarantees.~%~%Dynamic foreign symbols in this core: ~
           ~{~A~^, ~}~:@>" (list-dynamic-foreign-symbols)))
  ;; Clobber list of undefineds. Reinit will figure it all out again.
  #+linkage-table (setf (cdr *linkage-info*) nil)
  #+os-provides-dlopen
  (close-shared-objects))

(declaim (maybe-inline sap-foreign-symbol))
(defun sap-foreign-symbol (sap)
  (declare (ignorable sap))
  (let ((addr (sap-int sap)))
    (declare (ignorable addr))
    #+linkage-table
    (when (<= sb-vm:linkage-table-space-start
              addr
              sb-vm:linkage-table-space-end)
      (let ((table-index (sb-vm::linkage-table-index-from-address addr)))
        (dohash ((key value) (car *linkage-info*) :locked t)
          (when (= value table-index)
            (return-from sap-foreign-symbol (if (listp key) (car key) key))))))
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
      (let ((err (alien-funcall dladdr addr (addr info))))
        (if (zerop err)
            nil
            (slot info 'symbol))))
    ;; FIXME: Even in the absence of dladdr we could search the
    ;; static foreign symbols (and *linkage-info*, for that matter).
    ))

;;; How we learn about foreign symbols and dlhandles initially
(defvar *!initial-foreign-symbols*)

(defun !foreign-cold-init ()
  (declare (special *runtime-dlhandle* *shared-objects*))
  #-linkage-table
  (dovector (symbol *!initial-foreign-symbols*)
    (setf (gethash (car symbol) *static-foreign-symbols*) (cdr symbol)))
  #+linkage-table
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
