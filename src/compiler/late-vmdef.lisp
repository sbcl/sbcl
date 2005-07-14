;;;; a few things from the classic CMU CL "src/compiler/vmdef.lisp"
;;;; file which couldn't be compiled early

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

(defun note-this-location (vop kind)
  #!+sb-doc
  "NOTE-THIS-LOCATION VOP Kind
  Note that the current code location is an interesting (to the debugger)
  location of the specified Kind. VOP is the VOP responsible for this code.
  This VOP must specify some non-null :SAVE-P value (perhaps :COMPUTE-ONLY) so
  that the live set is computed."
  (let ((lab (gen-label)))
    (emit-label lab)
    (note-debug-location vop lab kind)))

(defun note-next-instruction (vop kind)
  #!+sb-doc
  "NOTE-NEXT-INSTRUCTION VOP Kind
   Similar to NOTE-THIS-LOCATION, except the use the location of the next
   instruction for the code location, wherever the scheduler decided to put
   it."
  (let ((loc (note-debug-location vop nil kind)))
    (sb!assem:emit-postit (lambda (segment posn)
                            (declare (ignore segment))
                            (setf (location-info-label loc) posn))))
  (values))
