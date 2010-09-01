;;; -*- Lisp -*-

;;; This isn't really lisp, but it's definitely a source file.  we
;;; name it thus to avoid having to mess with the clc lpn translations

;;; first, the headers necessary to find definitions of everything
("sys/types.h" "sys/socket.h" "sys/stat.h" "unistd.h" "sys/un.h"
 "netinet/in.h" "netinet/in_systm.h" "netinet/ip.h" "net/if.h"
 "netdb.h" "errno.h" "netinet/tcp.h" "fcntl.h" )

;;; then the stuff we're looking for
((:integer af-inet "AF_INET" "IP Protocol family")
 (:integer af-unspec "AF_UNSPEC" "Unspecified")
 (:integer af-local
           #+(or sunos solaris hpux) "AF_UNIX"
           #-(or sunos solaris hpux) "AF_LOCAL"
           "Local to host (pipes and file-domain).")
 #+linux (:integer af-inet6 "AF_INET6"   "IP version 6")
 #+linux (:integer af-route "AF_NETLINK" "Alias to emulate 4.4BSD ")

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
 (:integer so-type "SO_TYPE")                   ;get only
 (:integer so-error "SO_ERROR")         ;get only (also clears)
 (:integer so-dontroute "SO_DONTROUTE"
           "Bypass routing facilities: instead send direct to appropriate network interface for the network portion of the destination address")
 (:integer so-broadcast "SO_BROADCAST" "Request permission to send broadcast datagrams")
 (:integer so-sndbuf "SO_SNDBUF")
 #+linux (:integer so-passcred "SO_PASSCRED")
 (:integer so-rcvbuf "SO_RCVBUF")
 (:integer so-keepalive "SO_KEEPALIVE"
           "Send periodic keepalives: if peer does not respond, we get SIGPIPE")
 (:integer so-oobinline "SO_OOBINLINE"
           "Put out-of-band data into the normal input queue when received")
 #+linux
 (:integer so-no-check "SO_NO_CHECK")
 #+linux (:integer so-priority "SO_PRIORITY")
 (:integer so-linger "SO_LINGER"
           "For reliable streams, pause a while on closing when unsent messages are queued")
 #+linux (:integer so-bsdcompat "SO_BSDCOMPAT")
 (:integer so-sndlowat "SO_SNDLOWAT")
 (:integer so-rcvlowat "SO_RCVLOWAT")
 (:integer so-sndtimeo "SO_SNDTIMEO")
 (:integer so-rcvtimeo "SO_RCVTIMEO")

 (:integer tcp-nodelay "TCP_NODELAY")
 #+linux (:integer so-bindtodevice "SO_BINDTODEVICE")
 (:integer ifnamsiz "IFNAMSIZ")

 (:integer EADDRINUSE "EADDRINUSE")
 (:integer EAGAIN "EAGAIN")
 (:integer EBADF "EBADF")
 (:integer ECONNREFUSED "ECONNREFUSED")
 (:integer ETIMEDOUT "ETIMEDOUT")
 (:integer EINTR "EINTR")
 (:integer EINVAL "EINVAL")
 (:integer ENOBUFS "ENOBUFS")
 (:integer ENOMEM "ENOMEM")
 (:integer EOPNOTSUPP "EOPNOTSUPP")
 (:integer EPERM "EPERM")
 (:integer EPROTONOSUPPORT "EPROTONOSUPPORT")
 (:integer ERANGE "ERANGE")
 (:integer ESOCKTNOSUPPORT "ESOCKTNOSUPPORT")
 (:integer ENETUNREACH "ENETUNREACH")
 (:integer ENOTCONN "ENOTCONN")

 (:integer NETDB-INTERNAL #+hpux "h_NETDB_INTERNAL" #-hpux "NETDB_INTERNAL" "See errno.")
 (:integer NETDB-SUCCESS #+hpux "h_NETDB_SUCCESS" #-hpux "NETDB_SUCCESS" "No problem.")
 (:integer HOST-NOT-FOUND "HOST_NOT_FOUND" "Authoritative Answer Host not found.")
 (:integer TRY-AGAIN "TRY_AGAIN" "Non-Authoritative Host not found, or SERVERFAIL.")
 (:integer NO-RECOVERY "NO_RECOVERY" "Non recoverable errors, FORMERR, REFUSED, NOTIMP.")
 (:integer NO-DATA "NO_DATA" "Valid name, no data record of requested type.")
 (:integer NO-ADDRESS "NO_ADDRESS" "No address, look for MX record.")
 #-(or hpux sunos) (:function h-strerror ("hstrerror" c-string (errno int)))

 (:integer O-NONBLOCK "O_NONBLOCK")
 (:integer f-getfl "F_GETFL")
 (:integer f-setfl "F_SETFL")

 (:integer msg-oob "MSG_OOB")
 (:integer msg-peek "MSG_PEEK")
 (:integer msg-trunc "MSG_TRUNC")
 (:integer msg-waitall "MSG_WAITALL")
 (:integer msg-eor "MSG_EOR")
 (:integer msg-dontroute "MSG_DONTROUTE")
 (:integer msg-dontwait "MSG_DONTWAIT")
 #+linux (:integer msg-nosignal "MSG_NOSIGNAL")
 #+linux (:integer msg-confirm "MSG_CONFIRM")
 #+linux (:integer msg-more "MSG_MORE")

 ;; for socket-receive
 (:type socklen-t "socklen_t")
 (:type size-t "size_t")
 (:type ssize-t "ssize_t")

                                        #|
 ;;; stat is nothing to do with sockets, but I keep it around for testing
 ;;; the ffi glue
 (:structure stat ("struct stat"
 (t dev "dev_t" "st_dev")
 ((alien:integer 32) atime "time_t" "st_atime")))
 (:function stat ("stat" (integer 32)
 (file-name (* t))
 (buf (* t))))
 |#
 (:structure protoent ("struct protoent"
                       (c-string-pointer name "char *" "p_name")
                       ((* (* t)) aliases "char **" "p_aliases")
                       (integer proto "int" "p_proto")))
 (:function getprotobyname ("getprotobyname" (* protoent)
                                             (name c-string)))

;; getprotobyname_r is a thread-safe reentrant version of getprotobyname
 #+os-provides-getprotoby-r
 (:function getprotobyname-r ("getprotobyname_r" int
                                                 (name c-string)
                                                 (result_buf (* protoent))
                                                 (buffer (* char))
                                                 (buffer-len size-t)
                                                 #-solaris
                                                 (result (* (* protoent)))))


 (:function getprotobynumber ("getprotobynumber" (* protoent)
                                                 (proto int)))
 ;;ditto, save for the getprotobynumber part
 #+os-provides-getprotoby-r
 (:function getprotobynumber-r ("getprotobynumber_r" int
                                                 (proto int)
                                                 (result_buf (* protoent))
                                                 (buffer (* char))
                                                 (buffer-len size-t)
                                                 #-solaris
                                                 (result (* (* protoent)))))

 (:integer inaddr-any "INADDR_ANY")
 (:structure in-addr ("struct in_addr"
                      ((array (unsigned 8)) addr "u_int32_t" "s_addr")))
 (:structure sockaddr-in ("struct sockaddr_in"
                          #+darwin ((unsigned 8) len "__uint8_t" "sin_len")
                          (integer family "sa_family_t" "sin_family")
                          ;; These two could be in-port-t and
                          ;; in-addr-t, but then we'd throw away the
                          ;; convenience (and byte-order agnosticism)
                          ;; of the old sb-grovel scheme.
                          ((array (unsigned 8)) port "u_int16_t" "sin_port")
                          ((array (unsigned 8)) addr "struct in_addr" "sin_addr")))
 (:structure sockaddr-un ("struct sockaddr_un"
                          (integer family "sa_family_t" "sun_family")
                          (c-string path "char" "sun_path")))
 (:structure sockaddr-un-abstract ("struct sockaddr_un"
                              (integer family "sa_family_t" "sun_family")
                              ((array (unsigned 8)) path "char" "sun_path")))
 (:structure hostent ("struct hostent"
                      (c-string-pointer name "char *" "h_name")
                      ((* c-string) aliases "char **" "h_aliases")
                      (integer type "int" "h_addrtype")
                      (integer length "int" "h_length")
                      ((* (* (unsigned 8))) addresses "char **" "h_addr_list")))
 (:structure msghdr ("struct msghdr"
                      (c-string-pointer name "void *" "msg_name")
                      (integer namelen "socklen_t" "msg_namelen")
                      ((* t) iov "struct iovec" "msg_iov")
                      (integer iovlen "size_t" "msg_iovlen")
                      ((* t) control "void *" "msg_control")
                      (integer controllen "socklen_t" "msg_controllen")
                      (integer flags "int" "msg_flags")))
 (:function socket (#-netbsd "socket" #+netbsd "_socket" int
                    (domain int)
                    (type int)
                    (protocol int)))
 (:function bind ("bind" int
                  (sockfd int)
                  (my-addr (* t))  ; KLUDGE: sockaddr-in or sockaddr-un?
                  (addrlen socklen-t)))
 (:function listen ("listen" int
                    (socket int)
                    (backlog int)))
 (:function accept ("accept" int
                    (socket int)
                    (my-addr (* t)) ; KLUDGE: sockaddr-in or sockaddr-un?
                    (addrlen socklen-t :in-out)))
 (:function getpeername ("getpeername" int
                         (socket int)
                         (her-addr (* t)) ; KLUDGE: sockaddr-in or sockaddr-un?
                         (addrlen socklen-t :in-out)))
 (:function getsockname ("getsockname" int
                         (socket int)
                         (my-addr (* t)) ; KLUDGE: sockaddr-in or sockaddr-un?
                         (addrlen socklen-t :in-out)))
 (:function connect ("connect" int
                    (socket int)
                    (his-addr (* t)) ; KLUDGE: sockaddr-in or sockaddr-un?
                    (addrlen socklen-t)))
 (:function close ("close" int
                   (fd int)))
 (:function recvfrom ("recvfrom" ssize-t
                                 (socket int)
                                 (buf (* t))
                                 (len integer)
                                 (flags int)
                                 (sockaddr (* t)) ; KLUDGE: sockaddr-in or sockaddr-un?
                                 (socklen (* socklen-t))))
 (:function recvmsg ("recvmsg" ssize-t
                               (socket int)
                               (msg (* msghdr))
                               (flags int)))
 (:function send ("send" ssize-t
                         (socket int)
                         (buf (* t))
                         (len size-t)
                         (flags int)))
 (:function sendto ("sendto" int
                             (socket int)
                             (buf (* t))
                             (len size-t)
                             (flags int)
                             (sockaddr (* t)) ; KLUDGE: sockaddr-in or sockaddr-un?
                             (socklen socklen-t)))
 (:function sendmsg ("sendmsg" int
                               (socket int)
                               (msg (* msghdr))
                               (flags int)))
 (:function gethostbyname ("gethostbyname" (* hostent) (name c-string)))
 #+darwin
 (:function gethostbyname2 ("gethostbyname2" (* hostent)
                                             (name c-string)
                                             (af int)))
 (:function gethostbyaddr ("gethostbyaddr" (* hostent)
                                           (addr (* t))
                                           (len int)
                                           (af int)))

 ;; Re-entrant gethostbyname

 #+linux
 (:function gethostbyname-r ("gethostbyname_r"
                             int
                             (name c-string)
                             (ret (* hostent))
                             (buf (* char))
                             (buflen long)
                             (result (* (* hostent)))
                             (h-errnop (* int))))
 ;; getaddrinfo / getnameinfo

 #+sb-bsd-sockets-addrinfo
 (:structure addrinfo ("struct addrinfo"
                       (integer flags "int" "ai_flags")
                       (integer family "int" "ai_family")
                       (integer socktype "int" "ai_socktype")
                       (integer protocol "int" "ai_protocol")
                       ;; CLH 20070306 FIXME: ai_addrlen should really
                       ;; be a socklen_t, but I'm not sure if this the
                       ;; case on other platforms. I'm setting this to
                       ;; socklen_t on darwin and hoping that other
                       ;; platform maintainers will do the right thing
                       ;; here.
                       #+darwin (integer addrlen "socklen_t" "ai_addrlen")
                       #-darwin (integer addrlen "size_t" "ai_addrlen")
                       ((* sockaddr-in) addr "struct sockaddr*" "ai_addr")
                       (c-string canonname "char *" "ai_canonname")
                       ((* t) next "struct addrinfo*" "ai_next")))

 #+sb-bsd-sockets-addrinfo
 (:function getaddrinfo ("getaddrinfo"
                         int
                         (node c-string)
                         (service c-string)
                         (hints (* addrinfo))
                         (res (* (* addrinfo)))))

 #+sb-bsd-sockets-addrinfo
 (:function freeaddrinfo ("freeaddrinfo"
                          void
                          (res (* addrinfo))))

 #+sb-bsd-sockets-addrinfo
 (:function gai-strerror ("gai_strerror"
                         c-string
                         (error-code int)))

 #+sb-bsd-sockets-addrinfo
 (:function getnameinfo ("getnameinfo"
                         int
                         (address (* sockaddr-in))
                         (address-length size-t)
                         (host (* char))
                         (host-len size-t)
                         (service (* char))
                         (service-len size-t)
                         (flags int)))

 (:integer EAI-FAMILY "EAI_FAMILY")
 (:integer EAI-SOCKTYPE "EAI_SOCKTYPE")
 (:integer EAI-BADFLAGS "EAI_BADFLAGS")
 (:integer EAI-NONAME "EAI_NONAME")
 (:integer EAI-SERVICE "EAI_SERVICE")
 #-freebsd
 (:integer EAI-ADDRFAMILY "EAI_ADDRFAMILY")
 (:integer EAI-MEMORY "EAI_MEMORY")
 (:integer EAI-FAIL "EAI_FAIL")
 (:integer EAI-AGAIN "EAI_AGAIN")
 (:integer EAI-SYSTEM "EAI_SYSTEM")

 (:integer NI-NAMEREQD "NI_NAMEREQD")

 ;; Socket options

 (:function setsockopt ("setsockopt" int
                        (socket int)
                        (level int)
                        (optname int)
                        (optval (* t))
                        (optlen int))) ;;; should be socklen-t!
 (:function fcntl ("fcntl" int
                   (fd int)
                   (cmd int)
                   (arg long)))
 (:function getsockopt ("getsockopt" int
                        (socket int)
                        (level int)
                        (optname int)
                        (optval (* t))
                        (optlen (* int)))) ;;; should be socklen-t!
 )
