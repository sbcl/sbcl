;;; -*- Lisp -*-

;;; This isn't really lisp, but it's definitely a source file.  we
;;; name it thus to avoid having to mess with the clc lpn translations

;;; first, the headers necessary to find definitions of everything
#-win32 ("sys/types.h" "sys/socket.h" "sys/stat.h" "unistd.h" "sys/un.h"
 "netinet/in.h" "netinet/in_systm.h" "netinet/ip.h" "net/if.h"
 "arpa/inet.h" ; inet_{ntop,pton}
 "netdb.h" "errno.h" "netinet/tcp.h" "fcntl.h")
#+win32 ("winsock2.h" "errno.h" "ws2tcpip.h")

;;; then the stuff we're looking for
((:integer af-inet "AF_INET" "IP Protocol family")
 (:integer af-unspec "AF_UNSPEC" "Unspecified")
 (:integer af-inet6 "AF_INET6" "IP version 6")
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
           "Send periodic keepalives.  If peer does not respond, we get SIGPIPE.")
 #+linux (:integer tcp-keepcnt "TCP_KEEPCNT"
                   "Number of unacknowledged probes before the connection is considered dead.")
 #+linux (:integer tcp-keepidle "TCP_KEEPIDLE"
                   "Seconds between the last data packet sent and the first keepalive probe.")
 #+linux (:integer tcp-keepintvl "TCP_KEEPINTVL" "Seconds between keepalive probes.")
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

 (:integer msg-oob "MSG_OOB")
 (:integer msg-peek "MSG_PEEK")
 (:integer msg-dontroute "MSG_DONTROUTE")
 ;; for socket-receive
 (:type socklen-t "socklen_t")

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
 (:structure in6-addr ("struct in6_addr"
                       ((array (unsigned 8)) addr "unsigned char" "s6_addr[16]")))
 (:structure sockaddr-in6 ("struct sockaddr_in6"
                           #+darwin ((unsigned 8) len "__uint8_t" "sin6_len")
                           (integer family "sa_family_t" "sin6_family")
                           ;; Like in IN-ADDR, port and addr could be
                           ;; in-port-t and in6-addr-t, but then we'd
                           ;; throw away the convenience (and
                           ;; byte-order agnosticism) of the old
                           ;; sb-grovel scheme.
                           ((array (unsigned 8)) port "u_int16_t" "sin6_port")
                           ((array (unsigned 8)) flowinfo "u_int32_t" "sin6_flowinfo")
                           ((array (unsigned 8)) addr "struct in_addr6" "sin6_addr")
                           ((array (unsigned 8)) scope-id "u_int32_t" "sin6_scope_id")))
 #-win32
 (:structure sockaddr-un ("struct sockaddr_un"
                          (integer family "sa_family_t" "sun_family")
                          (c-string path "char" "sun_path")))
 #-win32
 (:structure sockaddr-un-abstract ("struct sockaddr_un"
                              (integer family "sa_family_t" "sun_family")
                              ((array (unsigned 8)) path "char" "sun_path")))
 (:integer INET-ADDRSTRLEN "INET_ADDRSTRLEN")
 (:integer INET6-ADDRSTRLEN "INET6_ADDRSTRLEN")
 (:function inet-ntop ("inet_ntop" c-string ; TODO external-format?
                                   (af int)
                                   (src (* t))
                                   (dst c-string)
                                   ;; size is of type socklen_t in
                                   ;; recent Glibc's, butlast size-t
                                   ;; seems to be the more portable
                                   ;; variant.
                                   (size size-t)))
 (:function inet-pton ("inet_pton" int
                                   (af int)
                                   (src c-string) ; TODO external-format?
                                   (dst (* t))))
 (:structure hostent ("struct hostent"
                      (c-string-pointer name "char *" "h_name")
                      ((* c-string) aliases "char **" "h_aliases")
                      (integer type "int" "h_addrtype")
                      (integer length "int" "h_length")
                      ((* (* (unsigned 8))) addresses "char **" "h_addr_list")))

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
 (:function close (#-win32 "close" #+win32 "closesocket" int
                   (fd int)))
 (:function shutdown ("shutdown" int
                      (fd int) (how int)))
 (:function recvfrom ("recvfrom" ssize-t
                                 (socket int)
                                 (buf (* t))
                                 (len integer)
                                 (flags int)
                                 (sockaddr (* t)) ; KLUDGE: sockaddr-in or sockaddr-un?
                                 (socklen (* socklen-t))))

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
                   &optional
                   (arg long)))
 (:function getsockopt ("getsockopt" int
                        (socket int)
                        (level int)
                        (optname int)
                        (optval (* t))
                        (optlen (* int)))) ;;; should be socklen-t!
 ;; Protocols
 ;; Android have those as enums, foiling #ifdef checks
 (#-android :integer #+android :integer-no-check IPPROTO_IP "IPPROTO_IP")
 (#-android :integer #+android :integer-no-check IPPROTO_IPV6 "IPPROTO_IPV6")
 (#-android :integer #+android :integer-no-check IPPROTO_ICMP "IPPROTO_ICMP")
 (#-android :integer #+android :integer-no-check IPPROTO_IGMP "IPPROTO_IGMP")
 (#-android :integer #+android :integer-no-check IPPROTO_TCP "IPPROTO_TCP")
 (#-android :integer #+android :integer-no-check IPPROTO_UDP "IPPROTO_UDP")
 (#-android :integer #+android :integer-no-check IPPROTO_RAW "IPPROTO_RAW"))
