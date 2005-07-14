;;;; fixups, extracted from codegen.lisp by WHN 19990227 in order
;;;; to help with cross-compiling bootstrapping

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;; a fixup of some kind
(defstruct (fixup
            (:constructor make-fixup (name flavor &optional offset))
            (:copier nil))
  ;; the name and flavor of the fixup. The assembler makes no
  ;; assumptions about the contents of these fields; their semantics
  ;; are imposed by the dumper.
  name
  flavor
  ;; OFFSET is an optional offset from whatever external label this
  ;; fixup refers to. Or in the case of the :CODE-OBJECT flavor of
  ;; fixups on the :X86 architecture, NAME is always NIL, so this
  ;; fixup doesn't refer to an external label, and OFFSET is an offset
  ;; from the beginning of the current code block.
  offset)

(defstruct (fixup-note
             (:constructor make-fixup-note (kind fixup position))
             (:copier nil))
  kind
  fixup
  position)

(defvar *fixup-notes*)

;;; Setting this variable lets you see what's going on as items are
;;; being pushed onto *FIXUPS*.
#!+sb-show (defvar *show-fixups-being-pushed-p* nil)

;;; This function is called by assembler instruction emitters when
;;; they find themselves trying to deal with a fixup.
(defun note-fixup (segment kind fixup)
  (sb!assem:emit-back-patch segment
                            0
                            (lambda (segment posn)
                              (declare (ignore segment))
                              ;; Why use EMIT-BACK-PATCH to cause this PUSH to
                              ;; be done later, instead of just doing it now?
                              ;; I'm not sure. Perhaps there's some concern
                              ;; that POSN isn't known accurately now? Perhaps
                              ;; there's a desire for all fixing up to go
                              ;; through EMIT-BACK-PATCH whether it needs to or
                              ;; not? -- WHN 19990905
                              #!+sb-show
                              (when *show-fixups-being-pushed-p*
                                (/show "PUSHING FIXUP" kind fixup posn))
                              (push (make-fixup-note kind fixup posn) *fixup-notes*)))
  (values))
