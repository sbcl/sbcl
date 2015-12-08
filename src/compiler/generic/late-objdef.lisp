;;;; late machine-independent aspects of the object representation

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

(macrolet ((frob ()
             `(progn ,@*!late-primitive-object-forms*)))
  (frob))

#!+sb-thread
(dolist (slot (primitive-object-slots
               (find 'thread *primitive-objects* :key #'primitive-object-name)))
  (when (slot-special slot)
    (setf (info :variable :wired-tls (slot-special slot))
          (ash (slot-offset slot) word-shift))))

#!+gencgc
(defconstant large-object-size
  (* 4 (max *backend-page-bytes* gencgc-card-bytes
            gencgc-alloc-granularity)))
