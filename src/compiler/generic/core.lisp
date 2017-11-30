;;;; stuff that knows how to load compiled code directly into core

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;; A CORE-OBJECT structure holds the state needed to resolve cross-component
;;; references during in-core compilation.
(defstruct (core-object
            (:constructor make-core-object ())
            #-no-ansi-print-object
            (:print-object (lambda (x s)
                             (print-unreadable-object (x s :type t :identity t))))
            (:copier nil))
  ;; A hashtable translating ENTRY-INFO structures to the corresponding actual
  ;; FUNCTIONs for functions in this compilation.
  (entry-table (make-hash-table :test 'eq) :type hash-table)
  ;; A hashtable translating ENTRY-INFO structures to a list of pairs
  ;; (<code object> . <offset>) describing the places that need to be
  ;; backpatched to point to the function for ENTRY-INFO.
  (patch-table (make-hash-table :test 'eq) :type hash-table)
  ;; A list of all the DEBUG-INFO objects created, kept so that we can
  ;; backpatch with the source info.
  (debug-info () :type list))

;;; Note the existence of FUNCTION.
#-sb-xc-host ; There is no (SETF CODE-HEADER-REF) so this can't work.
(defun note-fun (info function object)
  (declare (type function function)
           (type core-object object))
  (let ((patch-table (core-object-patch-table object)))
    (dolist (patch (gethash info patch-table))
      (setf (code-header-ref (car patch) (the index (cdr patch))) function))
    (remhash info patch-table))
  (setf (gethash info (core-object-entry-table object)) function)
  (values))

;;; Do "load-time" fixups on the code vector.
;;; But the host never compiles to core, and there is no GET-LISP-OBJ-ADDRESS,
;;; FIXUP-CODE-OBJECT, or ENSURE-SYMBOL-TLS-INDEX.
#-sb-xc-host
(defun do-core-fixups (code fixup-notes)
  (declare (list fixup-notes))
  (dolist (note fixup-notes)
    (let* ((kind (fixup-note-kind note))
           (fixup (fixup-note-fixup note))
           (position (fixup-note-position note))
           (name (fixup-name fixup))
           (flavor (fixup-flavor fixup))
           (value (ecase flavor
                    (:assembly-routine
                     (or (get-asm-routine name)
                         (error "undefined assembler routine: ~S" name)))
                    (:foreign
                     (aver (stringp name))
                     ;; FOREIGN-SYMBOL-ADDRESS signals an error
                     ;; if the symbol isn't found.
                     (foreign-symbol-address name))
                    #!+linkage-table
                    (:foreign-dataref
                     (aver (stringp name))
                     (foreign-symbol-address name t))
                    #!+(or x86 x86-64)
                    (:code-object
                     (aver (null name))
                     (get-lisp-obj-address code))
                    #!+immobile-space
                    ((:immobile-object :layout)
                     (get-lisp-obj-address (the (or layout symbol) name)))
                    #!+immobile-code
                    (:named-call
                     (sb!vm::fdefn-entry-address name))
                    #!+immobile-code
                    (:static-call
                     (sb!vm::function-raw-address name))
                    (:symbol-tls-index
                     (ensure-symbol-tls-index (the symbol name))))))
      (sb!vm:fixup-code-object code position value kind flavor))))

;;; Stick a reference to the function FUN in CODE-OBJECT at index I. If the
;;; function hasn't been compiled yet, make a note in the patch table.
#-sb-xc-host ; no (SETF CODE-HEADER-REF)
(defun reference-core-fun (code-obj i fun object)
  (declare (type core-object object) (type functional fun)
           (type index i))
  (let* ((info (leaf-info fun))
         (found (gethash info (core-object-entry-table object))))
    (if found
        (setf (code-header-ref code-obj i) found)
        (push (cons code-obj i)
              (gethash info (core-object-patch-table object)))))
  (values))

;;; Call the top level lambda function dumped for ENTRY, returning the
;;; values. ENTRY may be a :TOPLEVEL-XEP functional.
(defun core-call-toplevel-lambda (entry object)
  (declare (type functional entry) (type core-object object))
  (funcall (or (gethash (leaf-info entry)
                        (core-object-entry-table object))
               (error "Unresolved forward reference."))))

#!+(and immobile-code (host-feature sb-xc))
(progn
  ;; Use FDEFINITION because it strips encapsulations - whether that's
  ;; the right behavior for it or not is a separate concern.
  ;; If somebody tries (TRACE LENGTH) for example, it should not cause
  ;; compilations to fail on account of LENGTH becoming a closure.
  (defun sb!vm::function-raw-address (name &aux (fun (fdefinition name)))
    (cond ((not fun)
           (error "Can't statically link to undefined function ~S" name))
          ((not (immobile-space-obj-p fun))
           (error "Can't statically link to ~S: code is movable" name))
          ((neq (fun-subtype fun) sb!vm:simple-fun-widetag)
           (error "Can't statically link to ~S: non-simple function" name))
          (t
           (let ((addr (get-lisp-obj-address fun)))
             (sap-ref-word (int-sap addr)
                           (- (ash sb!vm:simple-fun-self-slot sb!vm:word-shift)
                              sb!vm:fun-pointer-lowtag))))))

  ;; Return the address to which to jump when calling NAME through its fdefn.
  (defun sb!vm::fdefn-entry-address (name)
    (let ((fdefn (find-or-create-fdefn name)))
      (+ (get-lisp-obj-address fdefn)
         (ash sb!vm:fdefn-raw-addr-slot sb!vm:word-shift)
         (- sb!vm:other-pointer-lowtag)))))
