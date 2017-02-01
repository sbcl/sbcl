;;; Do warm init without compiling files.
(progn
  (defvar *compile-files-p* nil)
  "about to LOAD warm.lisp (with *compile-files-p* = NIL)")

(progn
  (load "src/cold/warm.lisp")

  ;;; Remove docstrings that snuck in, as will happen with
  ;;; any file compiled in warm load.
  #-sb-doc
  (let ((count 0))
    (macrolet ((clear-it (place)
                 `(when ,place
                    (setf ,place nil)
                    (incf count))))
      ;; 1. Functions, macros, special operators
      (sb-vm::map-allocated-objects
       (lambda (obj type size)
         (declare (ignore size))
         (case type
          (#.sb-vm:code-header-widetag
           (dotimes (i (sb-kernel:code-n-entries obj))
             (let ((f (sb-kernel:%code-entry-point obj i)))
               (clear-it (sb-kernel:%simple-fun-doc f)))))
          (#.sb-vm:instance-header-widetag
           (when (typep obj 'class)
             (when (slot-boundp obj 'sb-pcl::%documentation)
               (clear-it (slot-value obj 'sb-pcl::%documentation)))))
          (#.sb-vm:funcallable-instance-header-widetag
           (when (typep obj 'standard-generic-function)
             (when (slot-boundp obj 'sb-pcl::%documentation)
               (clear-it (slot-value obj 'sb-pcl::%documentation)))))))
       :all)
      ;; 2. Variables, types, and anything else
      (do-all-symbols (s)
        (dolist (category '(:variable :type :typed-structure :setf))
          (clear-it (sb-int:info category :documentation s)))
        (clear-it (sb-int:info :random-documentation :stuff s))))
    (when (plusp count)
      (format t "~&Removed ~D doc string~:P" count)))

  ;; Share identical FUN-INFOs
  sb-int::
  (let ((ht (make-hash-table :test 'equalp))
        (old-count 0))
    (sb-c::call-with-each-globaldb-name
     (lambda (name)
       (binding* ((info (info :function :info name) :exit-if-null)
                  (shared-info (gethash info ht info)))
         (incf old-count)
         (if (eq info shared-info)
             (setf (gethash info ht) info)
           (setf (info :function :info name) shared-info)))))
    (format t "~&FUN-INFO: Collapsed ~D -> ~D~%"
            old-count (hash-table-count ht)))

  ;; Share identical FUN-TYPEs.
  (let ((ht (make-hash-table :test 'equal))
        (raw-accessor
         (compile nil '(lambda (f) (sb-vm::%%simple-fun-type f)))))
    (sb-vm::map-allocated-objects
     (lambda (obj type size)
       (declare (ignore type size))
       (when (sb-kernel:code-component-p obj)
         (dotimes (i (sb-kernel:code-n-entries obj))
           (let* ((f (sb-kernel:%code-entry-point obj i))
                  (type (funcall raw-accessor f)))
             (setf (sb-kernel:%simple-fun-type f)
                   (or (gethash type ht) (setf (gethash type ht) type)))))))
     :all))

  (sb-disassem::!compile-inst-printers)

  ;; Unintern no-longer-needed stuff before the possible PURIFY in
  ;; SAVE-LISP-AND-DIE.
  #-sb-fluid (sb-impl::!unintern-init-only-stuff)

  ;; A symbol whose INFO slot underwent any kind of manipulation
  ;; such that it now has neither properties nor globaldb info,
  ;; can have the slot set back to NIL if it wasn't already.
  (do-all-symbols (symbol)
    (when (and (sb-kernel:symbol-info symbol)
               (null (sb-kernel:symbol-info-vector symbol))
               (null (symbol-plist symbol)))
      (setf (sb-kernel:symbol-info symbol) nil)))

  ;; Set doc strings for the standard packages.
  #+sb-doc
  (setf (documentation (find-package "COMMON-LISP") t)
        "public: home of symbols defined by the ANSI language specification"
        (documentation (find-package "COMMON-LISP-USER") t)
        "public: the default package for user code and data"
        (documentation (find-package "KEYWORD") t)
        "public: home of keywords")

  "done with warm.lisp, about to GC :FULL T")

(sb-ext:gc :full t)

;;; resetting compilation policy to neutral values in preparation for
;;; SAVE-LISP-AND-DIE as final SBCL core (not in warm.lisp because
;;; SB-C::*POLICY* has file scope)
(setq sb-c::*policy* (copy-structure sb-c::**baseline-policy**))

;;; Adjust READTABLE-BASE-CHAR-PREFERENCE back to the advertised default.
(dolist (rt (list sb-impl::*standard-readtable* *debug-readtable*))
  (setf (readtable-base-char-preference rt) :symbols))
;;; Change the internal constructor's default too.
sb-kernel::(setf (dsd-default
                  (find 'sb-impl::%readtable-string-preference
                        (dd-slots (find-defstruct-description 'readtable))
                        :key #'dsd-name)) 'character)

;;; Lock internal packages
#+sb-package-locks
(dolist (p (list-all-packages))
  (unless (member p (mapcar #'find-package '("KEYWORD" "CL-USER")))
    (sb-ext:lock-package p)))

"done with warm.lisp, about to SAVE-LISP-AND-DIE"
;;; Even if /SHOW output was wanted during build, it's probably
;;; not wanted by default after build is complete. (And if it's
;;; wanted, it can easily be turned back on.)
#+sb-show (setf sb-int:*/show* nil)
;;; The system is complete now, all standard functions are
;;; defined.
;;; The call to CTYPE-OF-CACHE-CLEAR is probably redundant.
;;; SAVE-LISP-AND-DIE calls DEINIT which calls DROP-ALL-HASH-CACHES.
(sb-kernel::ctype-of-cache-clear)
(setq sb-c::*flame-on-necessarily-undefined-thing* t)

;;; Clean up stray symbols from the CL-USER package.
(with-package-iterator (iter "CL-USER" :internal :external)
  (loop (multiple-value-bind (winp symbol) (iter)
          (if winp (unintern symbol "CL-USER") (return)))))

;;; In case there is xref data for internals, repack it here to
;;; achieve a more compact encoding.
;;;
;;; However, repacking changes
;;; SB-C::**MOST-COMMON-XREF-NAMES-BY-{INDEX,NAME}** thereby changing
;;; the interpretation of xref data written into and loaded from
;;; fasls. Since fasls should be compatible between images originating
;;; from the same SBCL build, REPACK-XREF is of no use after the
;;; target image has been built.
#+sb-xref-for-internals (sb-c::repack-xref :verbose t)
(with-unlocked-packages (#:sb-c)
  (fmakunbound 'sb-c::repack-xref))

#+immobile-code (setq sb-c::*compile-to-memory-space* :dynamic)
#+sb-fasteval (setq sb-ext:*evaluator-mode* :interpret)
(sb-ext:save-lisp-and-die
 (progn
   ;; See comment in 'readtable.lisp'
   (setf (readtable-base-char-preference *readtable*) :symbols)
   ;; This is a base string since the flag wasn't set to NIL yet.
   "output/sbcl.core"))
