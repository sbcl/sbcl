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
	   #+(or sunos solaris) "AF_UNIX"
	   #-(or sunos solaris) "AF_LOCAL"
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
 (:integer so-type "SO_TYPE")                  ;get only
 (:integer so-error "SO_ERROR")                 ;get only (also clears)
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
 (:integer so-no-check 11)            
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
 (:integer EINTR "EINTR")
 (:integer EINVAL "EINVAL")
 (:integer ENOBUFS "ENOBUFS")
 (:integer ENOMEM "ENOMEM")
 (:integer EOPNOTSUPP "EOPNOTSUPP")
 (:integer EPERM "EPERM")
 (:integer EPROTONOSUPPORT "EPROTONOSUPPORT")
 (:integer ESOCKTNOSUPPORT "ESOCKTNOSUPPORT")
 (:integer ENETUNREACH "ENETUNREACH")

 (:integer NETDB-INTERNAL "NETDB_INTERNAL" "See errno.")
 (:integer NETDB-SUCCESS "NETDB_SUCCESS" "No problem.")
 (:integer HOST-NOT-FOUND "HOST_NOT_FOUND" "Authoritative Answer Host not found.")
 (:integer TRY-AGAIN "TRY_AGAIN" "Non-Authoritative Host not found, or SERVERFAIL.")
 (:integer NO-RECOVERY "NO_RECOVERY" "Non recoverable errors, FORMERR, REFUSED, NOTIMP.")
 (:integer NO-DATA "NO_DATA" "Valid name, no data record of requested type.")
 (:integer NO-ADDRESS "NO_ADDRESS" "No address, look for MX record.")

 (:integer O-NONBLOCK "O_NONBLOCK")
 (:integer f-getfl "F_GETFL")
 (:integer f-setfl "F_SETFL")

 #+linux (:integer msg-nosignal "MSG_NOSIGNAL")
 (:integer msg-oob "MSG_OOB")
 (:integer msg-peek "MSG_PEEK")
 (:integer msg-trunc "MSG_TRUNC")
 (:integer msg-waitall "MSG_WAITALL")

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
                       ((* t) name "char *" "p_name")
                       ((* (* t)) aliases "char **" "p_aliases")
		       (integer proto "int" "p_proto")))
 (:function getprotobyname ("getprotobyname" (* t)
					     (name c-string)))
 (:integer inaddr-any "INADDR_ANY")
 (:structure in-addr ("struct in_addr"
		      ((array (unsigned 8) 4) addr "u_int32_t" "s_addr")))
 (:structure sockaddr-in ("struct sockaddr_in"
                          (integer family "sa_family_t" "sin_family")
                          ((array (unsigned 8) 2) port "u_int16_t" "sin_port")
                          ((array (unsigned 8) 4) addr "struct in_addr" "sin_addr")))
 (:structure sockaddr-un ("struct sockaddr_un"
                          (integer family "sa_family_t" "sun_family")
                          ((array (unsigned 8) 108) path "char" "sun_path")))
 (:structure hostent ("struct hostent"
                      ((* t) name "char *" "h_name")
                      ((* c-string) aliases "char **" "h_aliases")
                      (integer type "int" "h_addrtype")
                      (integer length "int" "h_length")
                      ((* (* (unsigned 8))) addresses "char **" "h_addr_list")))
 (:function socket ("socket" integer
                    (domain integer)
                    (type integer)
                    (protocol integer)))
 (:function bind ("bind" integer
                  (sockfd integer)
                  (my-addr (* t))
                  (addrlen integer)))
 (:function listen ("listen" integer
                    (socket integer)
                    (backlog integer)))
 (:function accept ("accept" integer
                    (socket integer)
                    (my-addr (* t))
                    (addrlen integer :in-out)))
 (:function getpeername ("getpeername" integer
                         (socket integer)
                         (her-addr (* t))
                         (addrlen integer :in-out)))
 (:function getsockname ("getsockname" integer
                         (socket integer)
                         (my-addr (* t))
                         (addrlen integer :in-out)))
 (:function connect ("connect" integer
                    (socket integer)
                    (his-addr (* t))
                    (addrlen integer )))
 
 (:function close ("close" integer
                   (fd integer)))
 (:function recvfrom ("recvfrom" integer
				 (socket integer)
				 (buf (* t))
				 (len integer)
				 (flags integer)
				 (sockaddr (* t))
				 (socklen (* integer))))
 (:function gethostbyname ("gethostbyname" (* t ) (name c-string)))
 (:function gethostbyaddr ("gethostbyaddr" (* t )
					   (addr (* t))
					   (len integer)
					   (af integer)))
 (:function setsockopt ("setsockopt" integer
                        (socket integer)
                        (level integer)
                        (optname integer)
                        (optval (* t))
                        (optlen integer)))
 (:function fcntl ("fcntl" integer
                   (fd integer)
                   (cmd integer)
                   (arg integer)))
 (:function getsockopt ("getsockopt" integer
                        (socket integer)
                        (level integer)
                        (optname integer)
                        (optval (* t))
                        (optlen integer :in-out))))
)
