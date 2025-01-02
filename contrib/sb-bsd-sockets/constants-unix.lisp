("sys/socket.h" "errno.h" "fcntl.h")

((:integer af-local
           #+(or sunos solaris) "AF_UNIX"
           #-(or sunos solaris) "AF_LOCAL"
           "Local to host (pipes and file-domain).")

 (:integer ifnamsiz "IFNAMSIZ")

;; socket shutdown flags
 (:integer SHUT_RD "SHUT_RD")
 (:integer SHUT_WR "SHUT_WR")
 (:integer SHUT_RDWR "SHUT_RDWR")

 (:integer O-NONBLOCK "O_NONBLOCK")
 (:integer f-getfl "F_GETFL")
 (:integer f-setfl "F_SETFL")

 (:integer msg-trunc "MSG_TRUNC")
 (:integer msg-waitall "MSG_WAITALL")
 (:integer msg-eor "MSG_EOR")
 (:integer msg-dontwait "MSG_DONTWAIT")
 #+linux (:integer msg-nosignal "MSG_NOSIGNAL")
 #+linux (:integer msg-confirm "MSG_CONFIRM")
 #+linux (:integer msg-more "MSG_MORE")

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
 #-haiku (:integer ESOCKTNOSUPPORT "ESOCKTNOSUPPORT")
 (:integer ENETUNREACH "ENETUNREACH")
 (:integer ENOTCONN "ENOTCONN")
 (:integer EAFNOSUPPORT "EAFNOSUPPORT")
 (:integer EINPROGRESS "EINPROGRESS"))
