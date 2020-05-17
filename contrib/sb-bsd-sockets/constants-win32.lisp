;;; -*- Lisp -*-

;;; This isn't really lisp, but it's definitely a source file.  we
;;; name it thus to avoid having to mess with the clc lpn translations

;;; first, the headers necessary to find definitions of everything
("winsock2.h" "errno.h")

;;; then the stuff we're looking for
((:function ioctl ("ioctlsocket" int
                   (socket int)
                   (cmd int)
                   (argp (unsigned 32) :in-out)))

 (:function wsa-socket ("WSASocketA" int
                        (af int)
                        (type int)
                        (protocol int)
                        (lpProtocolInfo (* t))
                        (g int)
                        (flags int)))

 (:structure wsa-data ("struct WSAData"
                       (integer version "u_int16_t" "wVersion")
                       (integer high-version "u_int16_t" "wHighVersion")
                       (c-string description "char" "szDescription")
                       (c-string system-status "char" "szSystemStatus")
                       (integer max-sockets "unsigned short" "iMaxSockets")
                       (integer max-udp-dg "unsigned short" "iMaxUdpDg")
                       (c-string-pointer vendor-info "char *" "lpVendorInfo")))

 (:function wsa-startup ("WSAStartup" int
                        (wVersionRequested (unsigned 16))
                        (lpWSAData wsa-data :out)))

 (:function wsa-get-last-error ("WSAGetLastError" int))

 (:integer FIONBIO "FIONBIO")

 (:integer SHUT_RD "SD_RECEIVE")
 (:integer SHUT_WR "SD_SEND")
 (:integer SHUT_RDWR "SD_BOTH")

 (:integer EADDRINUSE "WSAEADDRINUSE")
 (:integer EAGAIN "WSAEWOULDBLOCK")
 (:integer EBADF "WSAEBADF")
 (:integer ECONNREFUSED "WSAECONNREFUSED")
 (:integer ETIMEDOUT "WSAETIMEDOUT")
 (:integer EINTR "WSAEINTR")
 (:integer EINVAL "WSAEINVAL")
 (:integer ENOBUFS "WSAENOBUFS")
 (:integer ENOMEM "WSAENOBUFS")
 (:integer EOPNOTSUPP "WSAEOPNOTSUPP")
 (:integer EPERM "WSAENETDOWN")
 (:integer EPROTONOSUPPORT "WSAEPROTONOSUPPORT")
 (:integer ESOCKTNOSUPPORT "WSAESOCKTNOSUPPORT")
 (:integer ENETUNREACH "WSAENETUNREACH")
 (:integer ENOTCONN "WSAENOTCONN")
 (:integer EAFNOSUPPORT "WSAEAFNOSUPPORT")
 (:integer EINPROGRESS "WSAEINPROGRESS"))
