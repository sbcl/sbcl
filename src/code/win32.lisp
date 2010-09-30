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
;;;
;;; FIXME: There used to be many more here, which are now groveled,
;;; but groveling HANDLE makes it unsigned, which currently breaks the
;;; build. --NS 2006-06-18
(define-alien-type handle int-ptr)
(define-alien-type system-string
                   #!-sb-unicode c-string
                   #!+sb-unicode (c-string :external-format :ucs-2))

(defconstant default-environment-length 1024)

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

    (unless (zerop (peek-console-input handle
                                       (cast buf (* t))
                                       1 (addr avail)))
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

#!+sb-unicode
(progn
  (defvar *ansi-codepage* nil)
  (defvar *oem-codepage* nil)
  (defvar *codepage-to-external-format* (make-hash-table)))

#!+sb-unicode
(dolist
    (cp '(;;037       IBM EBCDIC - U.S./Canada
          (437 :CP437) ;; OEM - United States
          ;;500       IBM EBCDIC - International
          ;;708       Arabic - ASMO 708
          ;;709       Arabic - ASMO 449+, BCON V4
          ;;710       Arabic - Transparent Arabic
          ;;720       Arabic - Transparent ASMO
          ;;737       OEM - Greek (formerly 437G)
          ;;775       OEM - Baltic
          (850 :CP850)     ;; OEM - Multilingual Latin I
          (852 :CP852)     ;; OEM - Latin II
          (855 :CP855)     ;; OEM - Cyrillic (primarily Russian)
          (857 :CP857)     ;; OEM - Turkish
          ;;858       OEM - Multilingual Latin I + Euro symbol
          (860 :CP860)     ;; OEM - Portuguese
          (861 :CP861)     ;; OEM - Icelandic
          (862 :CP862)     ;; OEM - Hebrew
          (863 :CP863)     ;; OEM - Canadian-French
          (864 :CP864)     ;; OEM - Arabic
          (865 :CP865)     ;; OEM - Nordic
          (866 :CP866)     ;; OEM - Russian
          (869 :CP869)     ;; OEM - Modern Greek
          ;;870       IBM EBCDIC - Multilingual/ROECE (Latin-2)
          (874 :CP874) ;; ANSI/OEM - Thai (same as 28605, ISO 8859-15)
          ;;875       IBM EBCDIC - Modern Greek
          (932 :CP932)     ;; ANSI/OEM - Japanese, Shift-JIS
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
          (1200 :UCS-2LE)    ;; Unicode UCS-2 Little-Endian (BMP of ISO 10646)
          (1201 :UCS-2BE)    ;; Unicode UCS-2 Big-Endian
          (1250 :CP1250)     ;; ANSI - Central European
          (1251 :CP1251)     ;; ANSI - Cyrillic
          (1252 :CP1252)     ;; ANSI - Latin I
          (1253 :CP1253)     ;; ANSI - Greek
          (1254 :CP1254)     ;; ANSI - Turkish
          (1255 :CP1255)     ;; ANSI - Hebrew
          (1256 :CP1256)     ;; ANSI - Arabic
          (1257 :CP1257)     ;; ANSI - Baltic
          (1258 :CP1258)     ;; ANSI/OEM - Vietnamese
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
          (21866 :KOI8-U)      ;; Ukrainian (KOI8-U)
          (28591 :LATIN-1)     ;; ISO 8859-1 Latin I
          (28592 :ISO-8859-2)  ;; ISO 8859-2 Central Europe
          (28593 :ISO-8859-3)  ;; ISO 8859-3 Latin 3
          (28594 :ISO-8859-4)  ;; ISO 8859-4 Baltic
          (28595 :ISO-8859-5)  ;; ISO 8859-5 Cyrillic
          (28596 :ISO-8859-6)  ;; ISO 8859-6 Arabic
          (28597 :ISO-8859-7)  ;; ISO 8859-7 Greek
          (28598 :ISO-8859-8)  ;; ISO 8859-8 Hebrew
          (28599 :ISO-8859-9)  ;; ISO 8859-9 Latin 5
          (28605 :LATIN-9)     ;; ISO 8859-15 Latin 9
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
  (setf (gethash (car cp) *codepage-to-external-format*) (cadr cp)))

#!+sb-unicode
;; FIXME: Something odd here: why are these two #+SB-UNICODE, whereas
;; the console just behave differently?
(progn
  (declaim (ftype (function () keyword) ansi-codepage))
  (defun ansi-codepage ()
    (or *ansi-codepage*
        (setq *ansi-codepage*
              (gethash (alien-funcall (extern-alien "GetACP@0" (function UINT)))
                       *codepage-to-external-format*
                       :latin-1))))

  (declaim (ftype (function () keyword) oem-codepage))
  (defun oem-codepage ()
    (or *oem-codepage*
        (setq *oem-codepage*
            (gethash (alien-funcall (extern-alien "GetOEMCP@0" (function UINT)))
                     *codepage-to-external-format*
                     :latin-1)))))

;; http://msdn.microsoft.com/library/en-us/dllproc/base/getconsolecp.asp
(declaim (ftype (function () keyword) console-input-codepage))
(defun console-input-codepage ()
  (or #!+sb-unicode
      (gethash (alien-funcall (extern-alien "GetConsoleCP@0" (function UINT)))
               *codepage-to-external-format*)
      :latin-1))

;; http://msdn.microsoft.com/library/en-us/dllproc/base/getconsoleoutputcp.asp
(declaim (ftype (function () keyword) console-output-codepage))
(defun console-output-codepage ()
  (or #!+sb-unicode
      (gethash (alien-funcall
                (extern-alien "GetConsoleOutputCP@0" (function UINT)))
               *codepage-to-external-format*)
      :latin-1))

(define-alien-routine ("LocalFree@4" local-free) void
  (lptr (* t)))

(defmacro cast-and-free (value &key (type 'system-string)
                                (free-function 'free-alien))
  `(prog1 (cast ,value ,type)
     (,free-function ,value)))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro with-funcname ((name description) &body body)
  `(let
     ((,name (etypecase ,description
               (string ,description)
               (cons (destructuring-bind (s &optional (l 0) c) ,description
                       (format nil "~A~A~A" s
                               (if c #!-sb-unicode "A@" #!+sb-unicode "W@" "@")
                               l))))))
     ,@body)))

(defmacro make-system-buffer (x)
 `(make-alien char #!+sb-unicode (ash ,x 1) #!-sb-unicode ,x))

;;; FIXME: The various FOO-SYSCALL-BAR macros, and perhaps some other
;;; macros in this file, are only used in this file, and could be
;;; implemented using SB!XC:DEFMACRO wrapped in EVAL-WHEN.

(defmacro syscall ((name ret-type &rest arg-types) success-form &rest args)
  (with-funcname (sname name)
    `(locally
       (declare (optimize (sb!c::float-accuracy 0)))
       (let ((result (alien-funcall
                       (extern-alien ,sname
                                     (function ,ret-type ,@arg-types))
                       ,@args)))
         (declare (ignorable result))
         ,success-form))))

;;; This is like SYSCALL, but if it fails, signal an error instead of
;;; returning error codes. Should only be used for syscalls that will
;;; never really get an error.
(defmacro syscall* ((name &rest arg-types) success-form &rest args)
  (with-funcname (sname name)
    `(locally
       (declare (optimize (sb!c::float-accuracy 0)))
       (let ((result (alien-funcall
                       (extern-alien ,sname (function bool ,@arg-types))
                       ,@args)))
         (when (zerop result)
           (win32-error ,sname))
         ,success-form))))

(defmacro with-sysfun ((func name ret-type &rest arg-types) &body body)
  (with-funcname (sname name)
    `(with-alien ((,func (function ,ret-type ,@arg-types)
                         :extern ,sname))
       ,@body)))

(defmacro void-syscall* ((name &rest arg-types) &rest args)
  `(syscall* (,name ,@arg-types) (values t 0) ,@args))

(defun get-last-error-message (err)
  "http://msdn.microsoft.com/library/default.asp?url=/library/en-us/debug/base/retrieving_the_last_error_code.asp"
  (with-alien ((amsg (* char)))
    (syscall (("FormatMessage" 28 t)
              dword dword dword dword dword (* (* char)) dword dword)
             (cast-and-free amsg :free-function local-free)
             (logior FORMAT_MESSAGE_ALLOCATE_BUFFER FORMAT_MESSAGE_FROM_SYSTEM)
             0 err 0 (addr amsg) 0 0)))

(defmacro win32-error (func-name &optional err)
  `(let ((err-code ,(or err `(get-last-error))))
     (declare (type (unsigned-byte 32) err-code))
     (error "~%Win32 Error [~A] - ~A~%~A"
            ,func-name
            err-code
            (get-last-error-message err-code))))

(defun get-folder-pathname (csidl)
  "http://msdn.microsoft.com/library/en-us/shellcc/platform/shell/reference/functions/shgetfolderpath.asp"
  (with-alien ((apath (* char) (make-system-buffer (1+ max_path))))
    (syscall (("SHGetFolderPath" 20 t) int handle int handle dword (* char))
             (parse-native-namestring
               (concatenate 'string (cast-and-free apath) "\\"))
             0 csidl 0 0 apath)))

(defun sb!unix:posix-getcwd ()
  (with-alien ((apath (* char) (make-system-buffer (1+ max_path))))
    (with-sysfun (afunc ("GetCurrentDirectory" 8 t) dword dword (* char))
      (let ((ret (alien-funcall afunc (1+ max_path) apath)))
        (when (zerop ret)
          (win32-error "GetCurrentDirectory"))
        (when (> ret (1+ max_path))
          (free-alien apath)
          (setf apath (make-system-buffer ret))
          (alien-funcall afunc ret apath))
        (cast-and-free apath)))))

(defun sb!unix:unix-mkdir (name mode)
  (declare (type sb!unix:unix-pathname name)
           (type sb!unix:unix-file-mode mode)
           (ignore mode))
  (void-syscall* (("CreateDirectory" 8 t) system-string dword) name 0))

(defun sb!unix:unix-rename (name1 name2)
  (declare (type sb!unix:unix-pathname name1 name2))
  (void-syscall* (("MoveFile" 8 t) system-string system-string) name1 name2))

(defun sb!unix::posix-getenv (name)
  (declare (type simple-string name))
  (with-alien ((aenv (* char) (make-system-buffer default-environment-length)))
    (with-sysfun (afunc ("GetEnvironmentVariable" 12 t)
                        dword system-string (* char) dword)
      (let ((ret (alien-funcall afunc name aenv default-environment-length)))
        (when (> ret default-environment-length)
          (free-alien aenv)
          (setf aenv (make-system-buffer ret))
          (alien-funcall afunc name aenv ret))
        (if (> ret 0)
            (cast-and-free aenv)
            (free-alien aenv))))))

;; GET-CURRENT-PROCESS
;; The GetCurrentProcess function retrieves a pseudo handle for the current
;; process.
;;
;; http://msdn.microsoft.com/library/en-us/dllproc/base/getcurrentprocess.asp
(declaim (inline get-current-process))
(define-alien-routine ("GetCurrentProcess@0" get-current-process) handle)

;;;; Process time information

(defconstant 100ns-per-internal-time-unit
  (/ 10000000 sb!xc:internal-time-units-per-second))

;; FILETIME
;; The FILETIME structure is a 64-bit value representing the number of
;; 100-nanosecond intervals since January 1, 1601 (UTC).
;;
;; http://msdn.microsoft.com/library/en-us/sysinfo/base/filetime_str.asp?
(define-alien-type FILETIME (sb!alien:unsigned 64))

(defmacro with-process-times ((creation-time exit-time kernel-time user-time)
                              &body forms)
  `(with-alien ((,creation-time filetime)
                (,exit-time filetime)
                (,kernel-time filetime)
                (,user-time filetime))
     (syscall* (("GetProcessTimes" 20) handle (* filetime) (* filetime)
                (* filetime) (* filetime))
               (progn ,@forms)
               (get-current-process)
               (addr ,creation-time)
               (addr ,exit-time)
               (addr ,kernel-time)
               (addr ,user-time))))

(declaim (inline system-internal-real-time))

(let ((epoch 0))
  (declare (unsigned-byte epoch))
  ;; FIXME: For optimization ideas see the unix implementation.
  (defun reinit-internal-real-time ()
    (setf epoch 0
          epoch (get-internal-real-time)))
  (defun get-internal-real-time ()
    (- (with-alien ((system-time filetime))
         (syscall (("GetSystemTimeAsFileTime" 4) void (* filetime))
                  (values (floor system-time 100ns-per-internal-time-unit))
                  (addr system-time)))
       epoch)))

(defun system-internal-run-time ()
  (with-process-times (creation-time exit-time kernel-time user-time)
    (values (floor (+ user-time kernel-time) 100ns-per-internal-time-unit))))

(define-alien-type hword (unsigned 16))

(define-alien-type systemtime
    (struct systemtime
            (year hword)
            (month hword)
            (weekday hword)
            (day hword)
            (hour hword)
            (minute hword)
            (second hword)
            (millisecond hword)))

;; Obtained with, but the XC can't deal with that -- but
;; it's not like the value is ever going to change...
;; (with-alien ((filetime filetime)
;;              (epoch systemtime))
;;   (setf (slot epoch 'year) 1970
;;         (slot epoch 'month) 1
;;         (slot epoch 'day) 1
;;         (slot epoch 'hour) 0
;;         (slot epoch 'minute) 0
;;         (slot epoch 'second) 0
;;         (slot epoch 'millisecond) 0)
;;   (syscall (("SystemTimeToFileTime" 8) void
;;             (* systemtime) (* filetime))
;;            filetime
;;            (addr epoch)
;;            (addr filetime)))
(defconstant +unix-epoch-filetime+ 116444736000000000)

#!-sb-fluid
(declaim (inline get-time-of-day))
(defun get-time-of-day ()
  "Return the number of seconds and microseconds since the beginning of the
UNIX epoch: January 1st 1970."
  (with-alien ((system-time filetime))
    (syscall (("GetSystemTimeAsFileTime" 4) void (* filetime))
             (multiple-value-bind (sec 100ns)
                 (floor (- system-time +unix-epoch-filetime+)
                        (* 100ns-per-internal-time-unit
                           internal-time-units-per-second))
               (values sec (floor 100ns 10)))
             (addr system-time))))

;; SETENV
;; The SetEnvironmentVariable function sets the contents of the specified
;; environment variable for the current process.
;;
;; http://msdn.microsoft.com/library/en-us/dllproc/base/setenvironmentvariable.asp
(defun setenv (name value)
  (declare (type simple-string name value))
  (void-syscall* (("SetEnvironmentVariable" 8 t) system-string system-string)
                 name value))

(defmacro c-sizeof (s)
  "translate alien size (in bits) to c-size (in bytes)"
  `(/ (alien-size ,s) 8))

;; OSVERSIONINFO
;; The OSVERSIONINFO data structure contains operating system version
;; information. The information includes major and minor version numbers,
;; a build number, a platform identifier, and descriptive text about
;; the operating system. This structure is used with the GetVersionEx function.
;;
;; http://msdn.microsoft.com/library/en-us/sysinfo/base/osversioninfo_str.asp
(define-alien-type nil
  (struct OSVERSIONINFO
    (dwOSVersionInfoSize dword)
    (dwMajorVersion dword)
    (dwMinorVersion dword)
    (dwBuildNumber dword)
    (dwPlatformId dword)
    (szCSDVersion (array char #!-sb-unicode 128 #!+sb-unicode 256))))

(defun get-version-ex ()
  (with-alien ((info (struct OSVERSIONINFO)))
    (setf (slot info 'dwOSVersionInfoSize) (c-sizeof (struct OSVERSIONINFO)))
    (syscall* (("GetVersionEx" 4 t) (* (struct OSVERSIONINFO)))
              (values (slot info 'dwMajorVersion)
                      (slot info 'dwMinorVersion)
                      (slot info 'dwBuildNumber)
                      (slot info 'dwPlatformId)
                      (cast (slot info 'szCSDVersion) system-string))
              (addr info))))

;; GET-COMPUTER-NAME
;; The GetComputerName function retrieves the NetBIOS name of the local
;; computer. This name is established at system startup, when the system
;; reads it from the registry.
;;
;; http://msdn.microsoft.com/library/en-us/sysinfo/base/getcomputername.asp
(declaim (ftype (function () simple-string) get-computer-name))
(defun get-computer-name ()
  (with-alien ((aname (* char) (make-system-buffer (1+ MAX_COMPUTERNAME_LENGTH)))
               (length dword (1+ MAX_COMPUTERNAME_LENGTH)))
    (with-sysfun (afunc ("GetComputerName" 8 t) bool (* char) (* dword))
      (when (zerop (alien-funcall afunc aname (addr length)))
        (let ((err (get-last-error)))
          (unless (= err ERROR_BUFFER_OVERFLOW)
            (win32-error "GetComputerName" err))
          (free-alien aname)
          (setf aname (make-system-buffer length))
          (alien-funcall afunc aname (addr length))))
      (cast-and-free aname))))
