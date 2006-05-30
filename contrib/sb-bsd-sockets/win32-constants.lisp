;;; -*- Lisp -*-

;;; This isn't really lisp, but it's definitely a source file.  we
;;; name it thus to avoid having to mess with the clc lpn translations

;;; first, the headers necessary to find definitions of everything
("winsock2.h")

;;; then the stuff we're looking for
((:integer af-inet "AF_INET" "IP Protocol family")
 (:integer af-unspec "AF_UNSPEC" "Unspecified")
 (:integer sock-stream "SOCK_STREAM"
           "Sequenced, reliable, connection-based byte streams.")
 (:integer sock-dgram "SOCK_DGRAM"
           "Connectionless, unreliable datagrams of fixed maximum length.")
 (:integer sock-raw "SOCK_RAW"
           "Raw protocol interface.")
 (:integer sock-rdm "SOCK_RDM"
           "Reliably-delivered messages.")
 (:integer sock-seqpacket "SOCK_SEQPACKET"
           "Sequenced, reliable, connection-based, datagrams of fixed maximum length.")

 (:integer sol-socket "SOL_SOCKET")

 ;; some of these may be linux-specific
 (:integer so-debug "SO_DEBUG"
   "Enable debugging in underlying protocol modules")
 (:integer so-reuseaddr "SO_REUSEADDR" "Enable local address reuse")
 (:integer so-type "SO_TYPE")                  ;get only
 (:integer so-error "SO_ERROR")                 ;get only (also clears)
 (:integer so-dontroute "SO_DONTROUTE"
           "Bypass routing facilities: instead send direct to appropriate network interface for the network portion of the destination address")
 (:integer so-broadcast "SO_BROADCAST" "Request permission to send broadcast datagrams")
 (:integer so-sndbuf "SO_SNDBUF")
 (:integer so-rcvbuf "SO_RCVBUF")
 (:integer so-keepalive "SO_KEEPALIVE"
           "Send periodic keepalives: if peer does not respond, we get SIGPIPE")
 (:integer so-oobinline "SO_OOBINLINE"
           "Put out-of-band data into the normal input queue when received")
 (:integer so-linger "SO_LINGER"
           "For reliable streams, pause a while on closing when unsent messages are queued")
 (:integer so-sndlowat "SO_SNDLOWAT")
 (:integer so-rcvlowat "SO_RCVLOWAT")
 (:integer so-sndtimeo "SO_SNDTIMEO")
 (:integer so-rcvtimeo "SO_RCVTIMEO")

 (:integer tcp-nodelay "TCP_NODELAY")

 (:integer HOST-NOT-FOUND "HOST_NOT_FOUND" "Authoritative Answer Host not found.")
 (:integer TRY-AGAIN "TRY_AGAIN" "Non-Authoritative Host not found, or SERVERFAIL.")
 (:integer NO-RECOVERY "NO_RECOVERY" "Non recoverable errors, FORMERR, REFUSED, NOTIMP.")
 (:integer NO-DATA "NO_DATA" "Valid name, no data record of requested type.")
 (:integer NO-ADDRESS "NO_ADDRESS" "No address, look for MX record.")

 (:integer msg-oob "MSG_OOB")
 (:integer msg-peek "MSG_PEEK")
 (:integer msg-dontroute "MSG_DONTROUTE")


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
 (:integer inaddr-any "INADDR_ANY")


 ;; for socket-receive
 (:type socklen-t "int")
 (:type size-t "size_t")
 (:type ssize-t "ssize_t")

 (:structure in-addr ("struct in_addr"
                      ((array (unsigned 8)) addr "u_int32_t" "s_addr")))

 (:structure sockaddr-in ("struct sockaddr_in"
                          (integer family "sa_family_t" "sin_family")
                          ;; These two could be in-port-t and
                          ;; in-addr-t, but then we'd throw away the
                          ;; convenience (and byte-order agnosticism)
                          ;; of the old sb-grovel scheme.
                          ((array (unsigned 8)) port "u_int16_t" "sin_port")
                          ((array (unsigned 8)) addr "struct in_addr" "sin_addr")))

 (:structure hostent ("struct hostent"
                      (c-string-pointer name "char *" "h_name")
                      ((* c-string) aliases "char **" "h_aliases")
                      (integer type "int" "h_addrtype")
                      (integer length "int" "h_length")
                      ((* (* (unsigned 8))) addresses "char **" "h_addr_list")))

 (:structure protoent ("struct protoent"
                       (c-string-pointer name "char *" "p_name")
                       ((* (* t)) aliases "char **" "p_aliases")
                       (integer proto "int" "p_proto")))

 (:function getprotobyname ("getprotobyname" (* protoent)
                                             (name c-string)))

 (:function getprotobynumber ("getprotobynumber" (* protoent)
                                                 (proto int)))

 (:function win32-bind
            ("bind" int
             (sockfd int)
             (my-addr (* t))  ; KLUDGE: sockaddr-in or sockaddr-un?
             (addrlen socklen-t)))

 (:function win32-listen ("listen" int
                    (socket int)
                    (backlog int)))

 (:function win32-accept ("accept" int
                    (socket int)
                    (my-addr (* t)) ; KLUDGE: sockaddr-in or sockaddr-un?
                    (addrlen int :in-out)))

 (:function win32-getpeername ("getpeername" int
                         (socket int)
                         (her-addr (* t)) ; KLUDGE: sockaddr-in or sockaddr-un?
                         (addrlen socklen-t :in-out)))

 (:function win32-getsockname ("getsockname" int
                         (socket int)
                         (my-addr (* t)) ; KLUDGE: sockaddr-in or sockaddr-un?
                         (addrlen socklen-t :in-out)))

 (:function win32-connect ("connect" int
                           (socket int)
                           (his-addr (* t)) ; KLUDGE: sockaddr-in or sockaddr-un?
                           (addrlen socklen-t)))

 (:function win32-close ("closesocket" int
                         (fd int)))

 (:function win32-recvfrom ("recvfrom" ssize-t
                            (socket int)
                            (buf (* t))
                            (len integer)
                            (flags int)
                            (sockaddr (* t)) ; KLUDGE: sockaddr-in or sockaddr-un?
                            (socklen (* socklen-t))))

 (:function win32-recv ("recv" int
                        (socket int)
                        (buf (* t))
                        (len integer)
                        (flags integer)))

 (:function win32-send ("send" ssize-t
                        (socket int)
                        (buf (* t))
                        (len size-t)
                        (flags int)))

 (:function win32-sendto ("sendto" int
                          (socket int)
                          (buf (* t))
                          (len size-t)
                          (flags int)
                          (sockaddr (* t)) ; KLUDGE: sockaddr-in or sockaddr-un?
                          (socklen socklen-t)))

 (:function gethostbyname ("gethostbyname" (* hostent) (name c-string)))

 (:function gethostbyaddr ("gethostbyaddr" (* hostent)
                                           (addr (* t))
                                           (len int)
                                           (af int)))

;;; should be using getaddrinfo instead?

 (:function win32-setsockopt ("setsockopt" int
                        (socket int)
                        (level int)
                        (optname int)
                        (optval (* t))
                        (optlen int))) ;;; should be socklen-t!

 (:function win32-getsockopt ("getsockopt" int
                        (socket int)
                        (level int)
                        (optname int)
                        (optval (* t))
                        (optlen int :in-out))) ;;; should be socklen-t!

 (:function win32-ioctl ("ioctlsocket"  int
                         (socket int)
                         (cmd int)
                         (argp (unsigned 32) :in-out)))


;;; Win32 specific cruft
 (:function wsa-socket ("WSASocketA" int
                        (af int)
                        (type int)
                        (protocol int)
                        (lpProtocolInfo (* t))
                        (g int)
                        (flags int)))

 (:function fd->handle ("_get_osfhandle" int
                        (fd int)))

 (:function handle->fd ("_open_osfhandle" int
                        (osfhandle int)
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

)