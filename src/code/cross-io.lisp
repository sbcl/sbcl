;;;; cross-compiler-only versions of I/O-related stuff

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!FASL")

;;;; fast-read operations
;;;;
;;;; (Portable versions of these are needed at cross-compile time because
;;;; genesis implements some of its cold fops by cloning ordinary fop
;;;; implementations, and the ordinary fop implementations are defined in terms
;;;; of fast-read operations.)

(defmacro prepare-for-fast-read-byte (stream &body forms)
  `(let ((%frc-stream% ,stream))
     ,@forms))

(defmacro fast-read-byte (&optional (eof-error-p t) (eof-value nil) any-type)
  (declare (ignore any-type))
  `(read-byte %frc-stream% ,eof-error-p ,eof-value))

(defmacro done-with-fast-read-byte ()
  `(values))
