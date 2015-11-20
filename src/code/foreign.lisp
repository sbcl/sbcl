;;;; Foreign symbol linkage

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

#!-(or elf mach-o win32)
(error "Not an ELF, Mach-O, or Win32 platform?")

(defun extern-alien-name (name)
  (handler-case
      (coerce name 'base-string)
    (error ()
      (error "invalid external alien name: ~S" name))))

;;; *STATIC-FOREIGN-SYMBOLS* are static as opposed to "dynamic" (not
;;; as opposed to C's "extern"). The table contains symbols known at
;;; the time that the program was built, but not symbols defined in
;;; object files which have been loaded dynamically since then.
#!-sb-dynamic-core
(declaim (type hash-table *static-foreign-symbols*))
#!-sb-dynamic-core
(defvar *static-foreign-symbols* (make-hash-table :test 'equal))

(declaim
 (ftype (sfunction (string hash-table) (or integer null)) find-foreign-symbol-in-table))
(defun find-foreign-symbol-in-table (name table)
  (let ((extern (extern-alien-name name)))
    (values
     (or (gethash extern table)
         (gethash (concatenate 'base-string "ldso_stub__" extern) table)))))

#-sb-xc-host
(defun find-foreign-symbol-address (name)
  #!+sb-doc
  "Returns the address of the foreign symbol NAME, or NIL. Does not enter the
symbol in the linkage table, and never returns an address in the linkage-table."
  (or #!-sb-dynamic-core
      (find-foreign-symbol-in-table name *static-foreign-symbols*)
      (find-dynamic-foreign-symbol-address name)))

#-sb-xc-host
(defun foreign-symbol-address (name &optional datap)
  #!+sb-doc
  "Returns the address of the foreign symbol NAME. DATAP must be true if the
symbol designates a variable (used only on linkage-table platforms). Returns a
secondary value that is true if DATAP was true and the symbol is a dynamic
foreign symbol.

On linkage-table ports the returned address is always static: either direct
address of a static symbol, or the linkage-table address of a dynamic one.
Dynamic symbols are entered into the linkage-table if they aren't there already.

On non-linkage-table ports signals an error if the symbol isn't found."
  (declare (ignorable datap))
  #!+sb-dynamic-core
  (values (ensure-foreign-symbol-linkage name datap) t)
  #!-sb-dynamic-core
  (let ((static (find-foreign-symbol-in-table name *static-foreign-symbols*)))
    (if static
        (values static nil)
        #!+os-provides-dlopen
        (progn
          #-sb-xc-host
          (values #!-linkage-table
                  (ensure-dynamic-foreign-symbol-address name)
                  #!+linkage-table
                  (ensure-foreign-symbol-linkage name datap)
                  t)
          #+sb-xc-host
          (error 'undefined-alien-error :name name))
        #!-os-provides-dlopen
        (error 'undefined-alien-error :name name))))

#-sb-xc-host ; SAPs don't exist
(defun foreign-symbol-sap (symbol &optional datap)
  #!+sb-doc
  "Returns a SAP corresponding to the foreign symbol. DATAP must be true if the
symbol designates a variable (used only on linkage-table platforms). May enter
the symbol into the linkage-table. On non-linkage-table ports signals an error
if the symbol isn't found."
  (declare (ignorable datap))
  #!-linkage-table
  (int-sap (foreign-symbol-address symbol))
  #!+linkage-table
  (multiple-value-bind (addr sharedp)
      (foreign-symbol-address symbol datap)
    #+sb-xc-host #!-sb-dynamic-core (aver (not sharedp)) ()
    ;; If the address is from linkage-table and refers to data
    ;; we need to do a bit of juggling. It is not the address of the
    ;; variable, but the address where the real address is stored.
    (if (and sharedp datap)
        (int-sap (sap-ref-word (int-sap addr) 0))
        (int-sap addr))))

#-sb-xc-host
(defun foreign-reinit ()
  #!+os-provides-dlopen
  (reopen-shared-objects)
  #!+linkage-table
  ;; Don't warn about undefined aliens on startup. The same core can
  ;; reasonably be expected to work with different versions of the
  ;; same library.
  (handler-bind ((style-warning #'muffle-warning))
    (update-linkage-table)))

;;; Cleanups before saving a core
#-sb-xc-host
(defun foreign-deinit ()
  #!+(and os-provides-dlopen (not linkage-table))
  (when (dynamic-foreign-symbols-p)
    (warn "~@<Saving cores with alien definitions referring to non-static ~
           foreign symbols is unsupported on this platform: references to ~
           such foreign symbols from the restarted core will not work. You ~
           may be able to work around this limitation by reloading all ~
           foreign definitions and code using them in the restarted core, ~
           but no guarantees.~%~%Dynamic foreign symbols in this core: ~
           ~{~A~^, ~}~:@>" (list-dynamic-foreign-symbols)))
  #!+os-provides-dlopen
  (close-shared-objects))

(declaim (maybe-inline sap-foreign-symbol))
(defun sap-foreign-symbol (sap)
  (declare (ignorable sap))
  #-sb-xc-host
  (let ((addr (sap-int sap)))
    (declare (ignorable addr))
    #!+linkage-table
    (when (<= sb!vm:linkage-table-space-start
              addr
              sb!vm:linkage-table-space-end)
      (dohash ((name-and-datap table-addr) *linkage-info* :locked t)
        (when (and (<= table-addr addr)
                   (< addr (+ table-addr sb!vm:linkage-table-entry-size)))
          (return-from sap-foreign-symbol (car name-and-datap)))))
    #!+os-provides-dladdr
    (with-alien ((info (struct dl-info
                               (filename c-string)
                               (base unsigned)
                               (symbol c-string)
                               (symbol-address unsigned)))
                 (dladdr (function unsigned unsigned (* (struct dl-info)))
                         :extern "dladdr"))
      (let ((err (without-gcing
                   ;; On eg. Darwin GC can could otherwise interrupt
                   ;; the call while dladdr is holding a lock.
                   (alien-funcall dladdr addr (addr info)))))
        (if (zerop err)
            nil
            (slot info 'symbol))))
    ;; FIXME: Even in the absence of dladdr we could search the
    ;; static foreign symbols (and *linkage-info*, for that matter).
    ))

;;; How we learn about foreign symbols and dlhandles initially
(defvar *!initial-foreign-symbols*)

#-sb-xc-host
(defun !foreign-cold-init ()
  #!-sb-dynamic-core
  (dolist (symbol *!initial-foreign-symbols*)
    (setf (gethash (car symbol) *static-foreign-symbols*) (cdr symbol)))
  #!+sb-dynamic-core
  (loop for table-address from sb!vm::linkage-table-space-start
          by sb!vm::linkage-table-entry-size
          and reference in sb!vm::*required-runtime-c-symbols*
        do (setf (gethash reference *linkage-info*) table-address))
  #!+os-provides-dlopen
  (setf *runtime-dlhandle* (dlopen-or-lose))
  #!+os-provides-dlopen
  (setf *shared-objects* nil))

#!-os-provides-dlopen
(define-unsupported-fun load-shared-object)
