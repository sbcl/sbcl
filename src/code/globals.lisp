;;;; This file contains special proclamations for variables that are
;;;; referenced in the code sources before they are defined. There are
;;;; also function proclamations to make some common functions be
;;;; known, avoiding work in recording the calls that are done before
;;;; the definition.

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

;;; FIXME: The COMMON-LISP specials here are already handled in
;;; cl-specials.lisp.
(declaim (special *keyword-package* *cl-package* *package* *query-io*
		  *terminal-io* *error-output* *trace-output* *debug-io*
		  *standard-input* *standard-output*
		  *evalhook* *applyhook*
		  original-lisp-environment
		  *read-default-float-format*
		  *read-suppress* *readtable* *print-base* *print-radix*
		  *print-length* *print-level* *print-pretty* *print-escape*
		  *print-case* *print-circle* *print-gensym* *print-array*
		  *standard-readtable*
		  sb!debug:*in-the-debugger*
		  sb!debug:*stack-top-hint*
		  sb!conditions::*handler-clusters*
		  sb!conditions::*restart-clusters*
		  *gc-inhibit* *need-to-collect-garbage*
		  *software-interrupt-vector* *load-verbose*
		  *load-print-stuff* *in-compilation-unit*
		  *aborted-compilation-unit-count* *char-name-alist*
		  *default-pathname-defaults* *beep-function*
		  *gc-notify-before* *gc-notify-after*
		  *posix-argv*))

(declaim (ftype (function * *)
		find-keyword keyword-test assert-error
		assert-prompt check-type-error case-body-error print-object
		describe-object sb!pcl::check-wrapper-validity))

;;; Gray streams functions not defined until after PCL is loaded.
(declaim (ftype (function * *)
		stream-advance-to-column stream-clear-input
		stream-clear-output stream-finish-output stream-force-output
		stream-fresh-line stream-line-column stream-line-length
		stream-listen stream-peek-char stream-read-byte
		stream-read-char stream-read-char-no-hang stream-read-line
		stream-start-line-p stream-terpri stream-unread-char
		stream-write-byte stream-write-char stream-write-string))
