;;;; needed-early stuff for the loader

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(file-comment
  "$Header$")

;;; information about non-Lisp-level linkage
;;;
;;; Note:
;;;   Assembler routines are named by full Lisp symbols: they
;;;     have packages and that sort of native Lisp stuff associated
;;;     with them. We can compare them with EQ.
;;;   Foreign symbols are named by Lisp strings: the Lisp package
;;;     system doesn't extend out to symbols in languages like C.
;;;     We want to use EQUAL to compare them.
;;;   *STATIC-FOREIGN-SYMBOLS* are static as opposed to "dynamic" (not
;;;     as opposed to "extern"). The table contains symbols known at 
;;;     the time that the program was built, but not symbols defined
;;;     in object files which have been loaded dynamically since then.
(declaim (type hash-table *assembler-routines* *static-foreign-symbols*))
(defvar *assembler-routines* (make-hash-table :test 'eq))
(defvar *static-foreign-symbols* (make-hash-table :test 'equal))

;;; the FOP database
(defvar *fop-names* (make-array 256 :initial-element nil)
  #!+sb-doc
  "a vector indexed by a FaslOP that yields the FOP's name")
(defvar *fop-functions*
  (make-array 256
	      :initial-element (lambda ()
				 (error "corrupt fasl file: losing FOP")))
  #!+sb-doc
  "a vector indexed by a FaslOP that yields a function of 0 arguments which
  will perform the operation")
(declaim (simple-vector *fop-names* *fop-functions*))

(defvar *load-code-verbose* nil)

;;; Moving native code during a GC or purify is not trivial on the x86
;;; port, so there are a few options for code placement.
;;;
;;; Byte-compiled code objects can always be moved so can be place in
;;; the dynamics heap. This is enabled with
;;; *load-byte-compiled-code-to-dynamic-space*.
;;;   FIXME: See whether this really works. Also, now that we have gencgc
;;;	  and all code moves, perhaps we could just remove this conditional
;;;	  and make this fixed behavior.
;;;
;;; Native code top level forms only have a short life so can be
;;; safely loaded into the dynamic heap (without fixups) so long as
;;; the GC is not active. This could be handy during a world load to
;;; save core space without the need to enable the support for moving
;;; x86 native code. Enable with *load-x86-tlf-to-dynamic-space*.
;;;   FIXME: Yikes! Could we punt this?
;;;
;;; One strategy for allowing the loading of x86 native code into the
;;; dynamic heap requires that the addresses of fixups be saved for
;;; all these code objects. After a purify these fixups can be
;;; dropped. This is enabled with *enable-dynamic-space-code*.
;;;
;;; A little analysis of the header information is used to determine
;;; if a code object is byte compiled, or native code.
(defvar *load-byte-compiled-code-to-dynamic-space* t)
(defvar *load-x86-tlf-to-dynamic-space* nil)  ; potentially dangerous with CGC.
					      ; KLUDGE: Yikes squared!
(defvar *enable-dynamic-space-code* #!-gencgc nil #!+gencgc t)
;;; FIXME: I think all of these should go away. I can't see a good reason
;;; not to just make everything relocatable.
