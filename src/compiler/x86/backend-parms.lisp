;;;; that part of the parms.lisp file from original CMU CL which is defined in
;;;; terms of the BACKEND structure
;;;;
;;;; FIXME: When we break up the BACKEND structure, this might be mergeable
;;;; back into the parms.lisp file.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; compiler constants

(setf *backend-fasl-file-type* "x86f")
(setf *backend-fasl-file-implementation* :x86)
(setf *backend-fasl-file-version* 7)
;;; 2 = sbcl-0.6.4 uses COMPILE-OR-LOAD-DEFGENERIC.
;;; 3 = sbcl-0.6.6 uses private symbol, not :EMPTY, for empty HASH-TABLE slot.
;;; 4 = sbcl-0.6.7 uses HAIRY-DATA-VECTOR-REF and HAIRY-DATA-VECTOR-SET
;;;     when array headers or data element type uncertainty exist, and
;;;     uses DATA-VECTOR-REF and DATA-VECTOR-SET only for VOPs. (Thus,
;;;     full calls to DATA-VECTOR-REF and DATA-VECTOR-SET from older
;;;     fasl files would fail, because there are no DEFUNs for these
;;;     operations any more.)
;;; 5 = sbcl-0.6.8 has rearranged static symbols.
;;; 6 = sbcl-0.6.9, got rid of non-ANSI %DEFCONSTANT/%%DEFCONSTANT stuff
;;;     and deleted a slot from DEBUG-SOURCE structure.
;;; 7 = around sbcl-0.6.9.8, merged SB-CONDITIONS package into SB-KERNEL

(setf *backend-register-save-penalty* 3)

(setf *backend-byte-order* :little-endian)

(setf *backend-page-size* 4096)
;;; comment from CMU CL:
;;;
;;;   in case we ever wanted to do this for Windows NT..
;;;
;;;   Windows NT uses a memory system granularity of 64K, which means
;;;   everything that gets mapped must be a multiple of that. The real
;;;   page size is 512, but that doesn't do us a whole lot of good.
;;;   Effectively, the page size is 64K.
;;;
;;;   would be: (setf *backend-page-size* 65536)
