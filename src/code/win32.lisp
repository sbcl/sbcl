;;;; This file contains Win32 support routines that SBCL needs to
;;;; implement itself, in addition to those that apply to Win32 in
;;;; unix.lisp.  In theory, some of these functions might someday be
;;;; useful to the end user.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!WIN32")

;;; Alien definitions for commonly used Win32 types.  Woe unto whoever
;;; tries to untangle this someday for 64-bit Windows.
(define-alien-type int-ptr long)
(define-alien-type handle int-ptr)
(define-alien-type dword unsigned-long)
(define-alien-type bool int)
(define-alien-type UINT unsigned-int)

;;; HANDLEs are actually pointers, but an invalid handle is -1 cast
;;; to a pointer.
(defconstant invalid-handle -1)

;;;; Error Handling

;;; Retrieve the calling thread's last-error code value.  The
;;; last-error code is maintained on a per-thread basis.
(define-alien-routine ("GetLastError@0" get-last-error) dword)

;;; Flag constants for FORMAT-MESSAGE.
(defconstant format-message-from-system #x1000)

;;; Format an error message based on a lookup table.  See MSDN for the
;;; full meaning of the all options---most are not used when getting
;;; system error codes.
(define-alien-routine ("FormatMessageA@28" format-message) dword
  (flags dword)
  (source (* t))
  (message-id dword)
  (language-id dword)
  (buffer c-string)
  (size dword)
  (arguments (* t)))

;;;; File Handles

;;; Get the operating system handle for a C file descriptor.  Returns
;;; INVALID-HANDLE on failure.
(define-alien-routine ("_get_osfhandle" get-osfhandle) handle
  (fd int))

;;; Read data from a file handle into a buffer.  This may be used
;;; synchronously or with "overlapped" (asynchronous) I/O.
(define-alien-routine ("ReadFile@20" read-file) bool
  (file handle)
  (buffer (* t))
  (bytes-to-read dword)
  (bytes-read (* dword))
  (overlapped (* t)))

;;; Write data from a buffer to a file handle.  This may be used
;;; synchronously  or with "overlapped" (asynchronous) I/O.
(define-alien-routine ("WriteFile@20" write-file) bool
  (file handle)
  (buffer (* t))
  (bytes-to-write dword)
  (bytes-written (* dword))
  (overlapped (* t)))

;;; Copy data from a named or anonymous pipe into a buffer without
;;; removing it from the pipe.  BUFFER, BYTES-READ, BYTES-AVAIL, and
;;; BYTES-LEFT-THIS-MESSAGE may be NULL if no data is to be read.
;;; Return TRUE on success, FALSE on failure.
(define-alien-routine ("PeekNamedPipe@24" peek-named-pipe) bool
  (pipe handle)
  (buffer (* t))
  (buffer-size dword)
  (bytes-read (* dword))
  (bytes-avail (* dword))
  (bytes-left-this-message (* dword)))

;;; Flush the console input buffer if HANDLE is a console handle.
;;; Returns true on success, false if the handle does not refer to a
;;; console.
(define-alien-routine ("FlushConsoleInputBuffer@4" flush-console-input-buffer) bool
  (handle handle))

;;; Read data from the console input buffer without removing it,
;;; without blocking.  Buffer should be large enough for LENGTH *
;;; INPUT-RECORD-SIZE bytes.
(define-alien-routine ("PeekConsoleInputA@16" peek-console-input) bool
  (handle handle)
  (buffer (* t))
  (length dword)
  (nevents (* dword)))

;;; Listen for input on a Windows file handle.  Unlike UNIX, there
;;; isn't a unified interface to do this---we have to know what sort
;;; of handle we have.  Of course, there's no way to actually
;;; introspect it, so we have to try various things until we find
;;; something that works.  Returns true if there could be input
;;; available, or false if there is not.
(defun handle-listen (handle)
  (with-alien ((avail dword)
               (buf (array char #.input-record-size)))
    (unless (zerop (peek-named-pipe handle nil 0 nil (addr avail) nil))
      (return-from handle-listen (plusp avail)))

    (unless (zerop (peek-console-input handle (cast buf (* t)) input-record-size (addr avail)))
      (return-from handle-listen (plusp avail)))

    ;; FIXME-SOCKETS: Try again here with WSAEventSelect in case
    ;; HANDLE is a socket.
    t))

;;; Listen for input on a C runtime file handle.  Returns true if
;;; there could be input available, or false if there is not.
(defun fd-listen (fd)
  (let ((handle (get-osfhandle fd)))
    (if handle
        (handle-listen handle)
        t)))

;;; Clear all available input from a file handle.
(defun handle-clear-input (handle)
  (flush-console-input-buffer handle)
  (with-alien ((buf (array char 1024))
               (count dword))
    (loop
     (unless (handle-listen handle)
       (return))
     (when (zerop (read-file handle (cast buf (* t)) 1024 (addr count) nil))
       (return))
     (when (< count 1024)
       (return)))))

;;; Clear all available input from a C runtime file handle.
(defun fd-clear-input (fd)
  (let ((handle (get-osfhandle fd)))
    (when handle
      (handle-clear-input handle))))

;;;; System Functions

;;; Sleep for MILLISECONDS milliseconds.
(define-alien-routine ("Sleep@4" millisleep) void
  (milliseconds dword))

#!+sb-unicode (defvar *ANSI-CP* nil)
#!+sb-unicode (defvar *OEM-CP* nil)

#!+sb-unicode
(defparameter *cp-to-external-format* (make-hash-table))

#!+sb-unicode
(dolist (cp
  '(;;037       IBM EBCDIC - U.S./Canada
    (437 :CP437) ;; OEM - United States
    ;;500       IBM EBCDIC - International
    ;;708       Arabic - ASMO 708
    ;;709       Arabic - ASMO 449+, BCON V4
    ;;710       Arabic - Transparent Arabic
    ;;720       Arabic - Transparent ASMO
    ;;737       OEM - Greek (formerly 437G)
    ;;775       OEM - Baltic
    (850 :CP850) ;; OEM - Multilingual Latin I
    (852 :CP852) ;; OEM - Latin II
    (855 :CP855) ;; OEM - Cyrillic (primarily Russian)
    (857 :CP857) ;; OEM - Turkish
    ;;858       OEM - Multilingual Latin I + Euro symbol
    (860 :CP860) ;; OEM - Portuguese
    (861 :CP861) ;; OEM - Icelandic
    (862 :CP862) ;; OEM - Hebrew
    (863 :CP863) ;; OEM - Canadian-French
    (864 :CP864) ;; OEM - Arabic
    (865 :CP865) ;; OEM - Nordic
    (866 :CP866) ;; OEM - Russian
    (869 :CP869) ;; OEM - Modern Greek
    ;;870       IBM EBCDIC - Multilingual/ROECE (Latin-2)
    (874 :CP874) ;; ANSI/OEM - Thai (same as 28605, ISO 8859-15)
    ;;875       IBM EBCDIC - Modern Greek
    ;;932       ANSI/OEM - Japanese, Shift-JIS
    ;;936       ANSI/OEM - Simplified Chinese (PRC, Singapore)
    ;;949       ANSI/OEM - Korean (Unified Hangul Code)
    ;;950       ANSI/OEM - Traditional Chinese (Taiwan; Hong Kong SAR, PRC)
    ;;1026      IBM EBCDIC - Turkish (Latin-5)
    ;;1047      IBM EBCDIC - Latin 1/Open System
    ;;1140      IBM EBCDIC - U.S./Canada (037 + Euro symbol)
    ;;1141      IBM EBCDIC - Germany (20273 + Euro symbol)
    ;;1142      IBM EBCDIC - Denmark/Norway (20277 + Euro symbol)
    ;;1143      IBM EBCDIC - Finland/Sweden (20278 + Euro symbol)
    ;;1144      IBM EBCDIC - Italy (20280 + Euro symbol)
    ;;1145      IBM EBCDIC - Latin America/Spain (20284 + Euro symbol)
    ;;1146      IBM EBCDIC - United Kingdom (20285 + Euro symbol)
    ;;1147      IBM EBCDIC - France (20297 + Euro symbol)
    ;;1148      IBM EBCDIC - International (500 + Euro symbol)
    ;;1149      IBM EBCDIC - Icelandic (20871 + Euro symbol)
    ;;1200      Unicode UCS-2 Little-Endian (BMP of ISO 10646)
    ;;1201      Unicode UCS-2 Big-Endian
    (1250 :CP1250) ;; ANSI - Central European
    (1251 :CP1251) ;; ANSI - Cyrillic
    (1252 :CP1252) ;; ANSI - Latin I
    (1253 :CP1253) ;; ANSI - Greek
    (1254 :CP1254) ;; ANSI - Turkish
    (1255 :CP1255) ;; ANSI - Hebrew
    (1256 :CP1256) ;; ANSI - Arabic
    (1257 :CP1257) ;; ANSI - Baltic
    (1258 :CP1258) ;; ANSI/OEM - Vietnamese
    ;;1361      Korean (Johab)
    ;;10000 MAC - Roman
    ;;10001     MAC - Japanese
    ;;10002     MAC - Traditional Chinese (Big5)
    ;;10003     MAC - Korean
    ;;10004     MAC - Arabic
    ;;10005     MAC - Hebrew
    ;;10006     MAC - Greek I
    (10007 :X-MAC-CYRILLIC) ;; MAC - Cyrillic
    ;;10008     MAC - Simplified Chinese (GB 2312)
    ;;10010     MAC - Romania
    ;;10017     MAC - Ukraine
    ;;10021     MAC - Thai
    ;;10029     MAC - Latin II
    ;;10079     MAC - Icelandic
    ;;10081     MAC - Turkish
    ;;10082     MAC - Croatia
    ;;12000     Unicode UCS-4 Little-Endian
    ;;12001     Unicode UCS-4 Big-Endian
    ;;20000     CNS - Taiwan
    ;;20001     TCA - Taiwan
    ;;20002     Eten - Taiwan
    ;;20003     IBM5550 - Taiwan
    ;;20004     TeleText - Taiwan
    ;;20005     Wang - Taiwan
    ;;20105     IA5 IRV International Alphabet No. 5 (7-bit)
    ;;20106     IA5 German (7-bit)
    ;;20107     IA5 Swedish (7-bit)
    ;;20108     IA5 Norwegian (7-bit)
    ;;20127     US-ASCII (7-bit)
    ;;20261     T.61
    ;;20269     ISO 6937 Non-Spacing Accent
    ;;20273     IBM EBCDIC - Germany
    ;;20277     IBM EBCDIC - Denmark/Norway
    ;;20278     IBM EBCDIC - Finland/Sweden
    ;;20280     IBM EBCDIC - Italy
    ;;20284     IBM EBCDIC - Latin America/Spain
    ;;20285     IBM EBCDIC - United Kingdom
    ;;20290     IBM EBCDIC - Japanese Katakana Extended
    ;;20297     IBM EBCDIC - France
    ;;20420     IBM EBCDIC - Arabic
    ;;20423     IBM EBCDIC - Greek
    ;;20424     IBM EBCDIC - Hebrew
    ;;20833     IBM EBCDIC - Korean Extended
    ;;20838     IBM EBCDIC - Thai
    (20866 :KOI8-R) ;; Russian - KOI8-R
    ;;20871     IBM EBCDIC - Icelandic
    ;;20880     IBM EBCDIC - Cyrillic (Russian)
    ;;20905     IBM EBCDIC - Turkish
    ;;20924     IBM EBCDIC - Latin-1/Open System (1047 + Euro symbol)
    ;;20932     JIS X 0208-1990 & 0121-1990
    ;;20936     Simplified Chinese (GB2312)
    ;;21025     IBM EBCDIC - Cyrillic (Serbian, Bulgarian)
    ;;21027     (deprecated)
    (21866 :KOI8-U) ;; Ukrainian (KOI8-U)
    (28591 :LATIN-1) ;; ISO 8859-1 Latin I
    (28592 :ISO-8859-2) ;; ISO 8859-2 Central Europe
    (28593 :ISO-8859-3) ;; ISO 8859-3 Latin 3
    (28594 :ISO-8859-4) ;; ISO 8859-4 Baltic
    (28595 :ISO-8859-5) ;; ISO 8859-5 Cyrillic
    (28596 :ISO-8859-6) ;; ISO 8859-6 Arabic
    (28597 :ISO-8859-7) ;; ISO 8859-7 Greek
    (28598 :ISO-8859-8) ;; ISO 8859-8 Hebrew
    (28599 :ISO-8859-9) ;; ISO 8859-9 Latin 5
    (28605 :LATIN-9) ;; ISO 8859-15 Latin 9
    ;;29001     Europa 3
    (38598 :ISO-8859-8) ;; ISO 8859-8 Hebrew
    ;;50220     ISO 2022 Japanese with no halfwidth Katakana
    ;;50221     ISO 2022 Japanese with halfwidth Katakana
    ;;50222     ISO 2022 Japanese JIS X 0201-1989
    ;;50225     ISO 2022 Korean
    ;;50227     ISO 2022 Simplified Chinese
    ;;50229     ISO 2022 Traditional Chinese
    ;;50930     Japanese (Katakana) Extended
    ;;50931     US/Canada and Japanese
    ;;50933     Korean Extended and Korean
    ;;50935     Simplified Chinese Extended and Simplified Chinese
    ;;50936     Simplified Chinese
    ;;50937     US/Canada and Traditional Chinese
    ;;50939     Japanese (Latin) Extended and Japanese
    (51932 :EUC-JP) ;; EUC - Japanese
    ;;51936     EUC - Simplified Chinese
    ;;51949     EUC - Korean
    ;;51950     EUC - Traditional Chinese
    ;;52936     HZ-GB2312 Simplified Chinese
    ;;54936     Windows XP: GB18030 Simplified Chinese (4 Byte)
    ;;57002     ISCII Devanagari
    ;;57003     ISCII Bengali
    ;;57004     ISCII Tamil
    ;;57005     ISCII Telugu
    ;;57006     ISCII Assamese
    ;;57007     ISCII Oriya
    ;;57008     ISCII Kannada
    ;;57009     ISCII Malayalam
    ;;57010     ISCII Gujarati
    ;;57011     ISCII Punjabi
    ;;65000     Unicode UTF-7
    (65001 :UTF8))) ;; Unicode UTF-8
  (setf (gethash (car cp) *cp-to-external-format*) (cadr cp)))

#!+sb-unicode
(declaim (ftype (function () keyword) ansi-cp))
#!+sb-unicode
(defun ansi-cp ()
  (or *ANSI-CP*
      (setq *ANSI-CP*
        (or
          (gethash (alien-funcall (extern-alien "GetACP@0" (function UINT)))
                   *cp-to-external-format*)
          :LATIN-1))))

#!+sb-unicode
(declaim (ftype (function () keyword) oem-cp))
#!+sb-unicode
(defun oem-cp ()
  (or *OEM-CP*
      (setq *OEM-CP*
        (or
          (gethash (alien-funcall (extern-alien "GetOEMCP@0" (function UINT)))
                   *cp-to-external-format*)
          :LATIN-1))))
