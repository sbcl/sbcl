;;; -*- lisp -*-

;;; This code is in the public domain.

;;; The cmucl implementation of simple-streams was done by Paul Foley,
;;; who placed the code in the public domain.  Sbcl port by Rudi
;;; Schlatte.


(defpackage sb-simple-streams
  (:use #:common-lisp)
  (:import-from #:sb-kernel #:ansi-stream #:charpos #:line-length)
  (:import-from #:sb-gray #:fundamental-stream)
  (:import-from #:sb-impl
                #:in-stream-from-designator #:out-stream-from-designator)
  ;; FIXME: Using deffoo! or equivalent might be nicer.
  (:implement #:common-lisp #:sb-impl)
  (:export ;; Stream classes
   #:STREAM
   #:SIMPLE-STREAM
   #:PROBE-SIMPLE-STREAM
   #:SINGLE-CHANNEL-SIMPLE-STREAM
   #:DIRECT-SIMPLE-STREAM
   #:BUFFER-INPUT-SIMPLE-STREAM
   #:BUFFER-OUTPUT-SIMPLE-STREAM
   #:NULL-SIMPLE-STREAM
   #:FILE-SIMPLE-STREAM
   #:MAPPED-FILE-SIMPLE-STREAM
   #:DUAL-CHANNEL-SIMPLE-STREAM
   #:TERMINAL-SIMPLE-STREAM
   #:SOCKET-SIMPLE-STREAM
   #:SOCKET-BASE-SIMPLE-STREAM
   #:HIPER-SOCKET-SIMPLE-STREAM
   #:STRING-SIMPLE-STREAM
   #:COMPOSING-STREAM
   #:STRING-INPUT-SIMPLE-STREAM
   #:STRING-OUTPUT-SIMPLE-STREAM
   #:FILL-POINTER-OUTPUT-SIMPLE-STREAM
   #:LIMITED-STRING-OUTPUT-SIMPLE-STREAM
   #:XP-SIMPLE-STREAM
   #:ANNOTATION-OUTPUT-SIMPLE-STREAM
   ;; Streams
   #:*STANDARD-INPUT* #:*STANDARD-OUTPUT* #:*ERROR-OUTPUT*
   #:*QUERY-IO* #:*DEBUG-IO* #:*TRACE-OUTPUT* #:*TERMINAL-IO*
   ;; Slot accessors
   #:STREAM-INPUT-HANDLE #:STREAM-OUTPUT-HANDLE
   #:STREAM-PLIST
   ;; Device-level functions
   #:DEVICE-OPEN #:DEVICE-CLOSE #:DEVICE-BUFFER-LENGTH
   #:DEVICE-FILE-POSITION #:DEVICE-FILE-LENGTH #:DEVICE-READ
   #:DEVICE-CLEAR-INPUT #:DEVICE-WRITE #:DEVICE-CLEAR-OUTPUT
   #:DEVICE-EXTEND #:DEVICE-FINISH-RECORD
   ;; Implementation functions/macros
   #:WITH-STREAM-CLASS #:SM #:FUNCALL-STM-HANDLER
   #:FUNCALL-STM-HANDLER-2 #:ADD-STREAM-INSTANCE-FLAGS
   #:REMOVE-STREAM-INSTANCE-FLAGS
   ;; User-level functions (mostly reexported from COMMON-LISP)
   #:PARSE-FILESPEC #:DEFINE-FILESPEC #:DEFAULT-OPEN-CLASS #:OPEN
   #:CLOSE #:READ-BYTE #:READ-CHAR #:READ-CHAR-NO-HANG #:UNREAD-CHAR
   #:PEEK-CHAR #:LISTEN #:READ-LINE #:READ-SEQUENCE #:CLEAR-INPUT
   #:WRITE-BYTE #:WRITE-CHAR #:WRITE-STRING #:WRITE-SEQUENCE #:TERPRI
   #:FRESH-LINE #:FINISH-OUTPUT #:FORCE-OUTPUT #:CLEAR-OUTPUT
   #:FILE-POSITION #:FILE-LENGTH #:LINE-LENGTH #:CHARPOS
   #:STREAM-ELEMENT-TYPE #:STREAM-EXTERNAL-FORMAT #:STREAMP
   #:OPEN-STREAM-P #:INPUT-STREAM-P #:OUTPUT-STREAM-P
   #:INTERACTIVE-STREAM-P #:READ-VECTOR #:WRITE-VECTOR #:READ-OCTETS
   #:WRITE-OCTETS #:DEF-STREAM-CLASS #:WAIT-FOR-INPUT-AVAILABLE
   ;; higher level things (reexported from COMMON-LISP)
   #:WITH-OPEN-FILE #:WITH-OPEN-STREAM #:FORMAT #:PPRINT #:PRIN1
   #:PRIN1-TO-STRING #:PRINC #:PRINC-TO-STRING #:PRINT #:READ
   #:READ-DELIMITED-LIST #:READ-FROM-STRING #:WRITE #:WRITE-LINE
   #:WRITE-TO-STRING #:READ-PRESERVING-WHITESPACE))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (sb-int:system-package-p (find-package "SB-SIMPLE-STREAMS")) t))

#||
(in-package "SB-EXT")
(defgeneric stream-file-position (stream &optional position))
(defgeneric stream-file-length (stream))
(export '(stream-file-position stream-file-length))
||#
