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
