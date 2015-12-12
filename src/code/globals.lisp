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

(declaim (special sb!debug:*in-the-debugger*
                  sb!debug:*stack-top-hint*
                  *handler-clusters*
                  *restart-clusters*
                  *in-without-gcing* *gc-inhibit* *gc-pending*
                  #!+sb-thread *stop-for-gc-pending*
                  #!+sb-dynamic-core sb!vm::*required-runtime-c-symbols*
                  *load-verbose*
                  *posix-argv*))
