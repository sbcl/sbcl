;;;; This is an automatically generated file, please do not hand-edit it.
;;;; See the program "grovel-headers.c".

(in-package "SB-WIN32")

(defconstant input-record-size 20) ; #x14
(defconstant MAX_PATH 260) ; #x104
;;; CSIDL
(defconstant CSIDL_DESKTOP 0) ; #x0
(defconstant CSIDL_INTERNET 1) ; #x1
(defconstant CSIDL_PROGRAMS 2) ; #x2
(defconstant CSIDL_CONTROLS 3) ; #x3
(defconstant CSIDL_PRINTERS 4) ; #x4
(defconstant CSIDL_PERSONAL 5) ; #x5
(defconstant CSIDL_FAVORITES 6) ; #x6
(defconstant CSIDL_STARTUP 7) ; #x7
(defconstant CSIDL_RECENT 8) ; #x8
(defconstant CSIDL_SENDTO 9) ; #x9
(defconstant CSIDL_BITBUCKET 10) ; #xa
(defconstant CSIDL_STARTMENU 11) ; #xb
(defconstant CSIDL_DESKTOPDIRECTORY 16) ; #x10
(defconstant CSIDL_DRIVES 17) ; #x11
(defconstant CSIDL_NETWORK 18) ; #x12
(defconstant CSIDL_NETHOOD 19) ; #x13
(defconstant CSIDL_FONTS 20) ; #x14
(defconstant CSIDL_TEMPLATES 21) ; #x15
(defconstant CSIDL_COMMON_STARTMENU 22) ; #x16
(defconstant CSIDL_COMMON_PROGRAMS 23) ; #x17
(defconstant CSIDL_COMMON_STARTUP 24) ; #x18
(defconstant CSIDL_COMMON_DESKTOPDIRECTORY 25) ; #x19
(defconstant CSIDL_APPDATA 26) ; #x1a
(defconstant CSIDL_PRINTHOOD 27) ; #x1b
(defconstant CSIDL_LOCAL_APPDATA 28) ; #x1c
(defconstant CSIDL_ALTSTARTUP 29) ; #x1d
(defconstant CSIDL_COMMON_ALTSTARTUP 30) ; #x1e
(defconstant CSIDL_COMMON_FAVORITES 31) ; #x1f
(defconstant CSIDL_INTERNET_CACHE 32) ; #x20
(defconstant CSIDL_COOKIES 33) ; #x21
(defconstant CSIDL_HISTORY 34) ; #x22
(defconstant CSIDL_COMMON_APPDATA 35) ; #x23
(defconstant CSIDL_WINDOWS 36) ; #x24
(defconstant CSIDL_SYSTEM 37) ; #x25
(defconstant CSIDL_PROGRAM_FILES 38) ; #x26
(defconstant CSIDL_MYPICTURES 39) ; #x27
(defconstant CSIDL_PROFILE 40) ; #x28
(defconstant CSIDL_SYSTEMX86 41) ; #x29
(defconstant CSIDL_PROGRAM_FILESX86 42) ; #x2a
(defconstant CSIDL_PROGRAM_FILES_COMMON 43) ; #x2b
(defconstant CSIDL_PROGRAM_FILES_COMMONX86 44) ; #x2c
(defconstant CSIDL_COMMON_TEMPLATES 45) ; #x2d
(defconstant CSIDL_COMMON_DOCUMENTS 46) ; #x2e
(defconstant CSIDL_COMMON_ADMINTOOLS 47) ; #x2f
(defconstant CSIDL_ADMINTOOLS 48) ; #x30
(defconstant CSIDL_CONNECTIONS 49) ; #x31
(defconstant CSIDL_COMMON_MUSIC 53) ; #x35
(defconstant CSIDL_COMMON_PICTURES 54) ; #x36
(defconstant CSIDL_COMMON_VIDEO 55) ; #x37
(defconstant CSIDL_RESOURCES 56) ; #x38
(defconstant CSIDL_RESOURCES_LOCALIZED 57) ; #x39
(defconstant CSIDL_COMMON_OEM_LINKS 58) ; #x3a
(defconstant CSIDL_CDBURN_AREA 59) ; #x3b
(defconstant CSIDL_COMPUTERSNEARME 61) ; #x3d
(defconstant CSIDL_FLAG_DONT_VERIFY 16384) ; #x4000
(defconstant CSIDL_FLAG_CREATE 32768) ; #x8000
(defconstant CSIDL_FLAG_MASK 65280) ; #xff00
;;; Exceptions
(defconstant +exception-access-violation+ 3221225477) ; #xc0000005
(defconstant +exception-array-bounds-exceeded+ 3221225612) ; #xc000008c
(defconstant +exception-breakpoint+ 2147483651) ; #x80000003
(defconstant +exception-datatype-misalignment+ 2147483650) ; #x80000002
(defconstant +exception-flt-denormal-operand+ 3221225613) ; #xc000008d
(defconstant +exception-flt-divide-by-zero+ 3221225614) ; #xc000008e
(defconstant +exception-flt-inexact-result+ 3221225615) ; #xc000008f
(defconstant +exception-flt-invalid-operation+ 3221225616) ; #xc0000090
(defconstant +exception-flt-overflow+ 3221225617) ; #xc0000091
(defconstant +exception-flt-stack-check+ 3221225618) ; #xc0000092
(defconstant +exception-flt-underflow+ 3221225619) ; #xc0000093
(defconstant +exception-heap-corruption+ 3221226356) ; #xc0000374
(defconstant +exception-illegal-instruction+ 3221225501) ; #xc000001d
(defconstant +exception-in-page-error+ 3221225478) ; #xc0000006
(defconstant +exception-int-divide-by-zero+ 3221225620) ; #xc0000094
(defconstant +exception-int-overflow+ 3221225621) ; #xc0000095
(defconstant +exception-invalid-disposition+ 3221225510) ; #xc0000026
(defconstant +exception-noncontinuable-exception+ 3221225509) ; #xc0000025
(defconstant +exception-priv-instruction+ 3221225622) ; #xc0000096
(defconstant +exception-single-step+ 2147483652) ; #x80000004
(defconstant +exception-stack-overflow+ 3221225725) ; #xc00000fd
(defconstant +dbg-printexception-c+ 1073807366) ; #x40010006
(defconstant +dbg-printexception-wide-c+ 1073807370) ; #x4001000a
(defconstant +exception-maximum-parameters+ 15) ; #xf
;;; FormatMessage
(defconstant format-message-allocate-buffer 256) ; #x100
(defconstant format-message-from-system 4096) ; #x1000
(defconstant format-message-max-width-mask 255) ; #xff
(defconstant format-message-ignore-inserts 512) ; #x200
;;; Errors
;;; Errors
(defconstant ERROR_ENVVAR_NOT_FOUND 203) ; #xcb
(defconstant ERROR_ALREADY_EXISTS 183) ; #xb7
(defconstant ERROR_FILE_EXISTS 80) ; #x50
(defconstant ERROR_FILE_NOT_FOUND 2) ; #x2
(defconstant ERROR_ACCESS_DENIED 5) ; #x5
(defconstant error-io-pending 997) ; #x3e5
(defconstant error-broken-pipe 109) ; #x6d
(defconstant error-no-data 232) ; #xe8
(defconstant error-handle-eof 38) ; #x26
;;; GetComputerName
(defconstant MAX_COMPUTERNAME_LENGTH 15) ; #xf
(defconstant ERROR_BUFFER_OVERFLOW 111) ; #x6f
;;; Windows Types
(define-alien-type int-ptr (signed 64))
(define-alien-type dword (unsigned 32))
(define-alien-type bool (signed 32))
(define-alien-type uint (unsigned 32))
(define-alien-type ulong (unsigned 32))
;;; File Desired Access
(defconstant FILE_GENERIC_READ 1179785) ; #x120089
(defconstant FILE_GENERIC_WRITE 1179926) ; #x120116
(defconstant FILE_GENERIC_EXECUTE 1179808) ; #x1200a0
(defconstant FILE_SHARE_READ 1) ; #x1
(defconstant FILE_SHARE_WRITE 2) ; #x2
(defconstant FILE_SHARE_DELETE 4) ; #x4
;;; File Creation Dispositions
(defconstant CREATE_NEW 1) ; #x1
(defconstant CREATE_ALWAYS 2) ; #x2
(defconstant OPEN_EXISTING 3) ; #x3
(defconstant OPEN_ALWAYS 4) ; #x4
(defconstant TRUNCATE_EXISTING 5) ; #x5
;;; Desired Access
(defconstant ACCESS_GENERIC_READ 2147483648) ; #x80000000
(defconstant ACCESS_GENERIC_WRITE 1073741824) ; #x40000000
(defconstant ACCESS_GENERIC_EXECUTE 536870912) ; #x20000000
(defconstant ACCESS_GENERIC_ALL 268435456) ; #x10000000
(defconstant ACCESS_FILE_APPEND_DATA 4) ; #x4
(defconstant ACCESS_DELETE 65536) ; #x10000
;;; Handle Information Flags
(defconstant HANDLE_FLAG_INHERIT 1) ; #x1
(defconstant HANDLE_FLAG_PROTECT_FROM_CLOSE 2) ; #x2
;;; Standard Handle Keys
(defconstant STD_INPUT_HANDLE 4294967286) ; #xfffffff6
(defconstant STD_OUTPUT_HANDLE 4294967285) ; #xfffffff5
(defconstant STD_ERROR_HANDLE 4294967284) ; #xfffffff4
;;; WinCrypt
(defconstant crypt-verifycontext 4026531840) ; #xf0000000
(defconstant crypt-silent 64) ; #x40
(defconstant prov-rsa-full 1) ; #x1
(defconstant pipe-access-duplex 3) ; #x3
(defconstant pipe-access-inbound 1) ; #x1
(defconstant pipe-access-outbound 2) ; #x2
(defconstant pipe-type-byte 0) ; #x0
(defconstant wait-abandoned 128) ; #x80
(defconstant wait-object-0 0) ; #x0
(defconstant wait-timeout 258) ; #x102
(defconstant wait-failed 4294967295) ; #xffffffff
(defconstant still-active 259) ; #x103
(in-package "SB-UNIX")

;;; Unix-like constants and types on Windows
(defconstant o_rdonly 0) ; #x0
(defconstant o_wronly 1) ; #x1
(defconstant o_rdwr 2) ; #x2
(defconstant o_creat 256) ; #x100
(defconstant o_trunc 512) ; #x200
(defconstant o_append 8) ; #x8
(defconstant o_excl 1024) ; #x400
(defconstant o_binary 32768) ; #x8000
(defconstant o_noinherit 128) ; #x80
(defconstant enoent 2) ; #x2
(defconstant eexist 17) ; #x11
(defconstant eintr 4) ; #x4
(defconstant eagain 11) ; #xb
(defconstant ebadf 9) ; #x9
(defconstant s-ifmt 61440) ; #xf000
(defconstant s-ifdir 16384) ; #x4000
(defconstant s-ifreg 32768) ; #x8000
(define-alien-type ino-t (unsigned 16))
(define-alien-type time-t (signed 64))
(define-alien-type off-t (signed 32))
(define-alien-type size-t (unsigned 64))
(define-alien-type ssize-t (unsigned 64))
(define-alien-type mode-t (unsigned 16))
(define-alien-type wst-dev-t (unsigned 32))
(define-alien-type wst-ino-t (unsigned 16))
(define-alien-type wst-off-t (unsigned 32))
(define-alien-type wst-blksize-t (unsigned 32))
(define-alien-type wst-blkcnt-t (unsigned 32))
(define-alien-type wst-nlink-t (signed 16))
(define-alien-type wst-uid-t (signed 16))
(define-alien-type wst-gid-t (signed 16))
(defconstant fd-setsize 1024) ; #x400

(defconstant sizeof-sigset_t 4) ; #x4
(defconstant sig_block 1) ; #x1
(defconstant sig_unblock 2) ; #x2
(defconstant sig_setmask 3) ; #x3

;;; structures
(define-alien-type nil
  (struct timeval
          (tv-sec (signed 32))
          (tv-usec (signed 32))))
(define-alien-type nil
  (struct timespec
          (tv-sec (signed 64))
          (tv-nsec (signed 32))))

(defconstant sizeof-timespec 16) ; #x10
(defconstant sizeof-timeval 16) ; #x10

(in-package "SB-KERNEL")

;;; GENCGC related
(define-alien-type page-index-t (signed 64))
(define-alien-type generation-index-t (signed 8))

;;; Our runtime types
(define-alien-type os-vm-size-t (unsigned 64))
