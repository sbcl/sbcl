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
