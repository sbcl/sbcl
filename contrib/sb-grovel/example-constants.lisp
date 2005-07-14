;;; -*- Lisp -*- - well, that's stretching a point.  code=data != data=code

;;; first, the headers necessary to find definitions of everything
("sys/types.h" "sys/socket.h" "sys/stat.h" "unistd.h" "sys/un.h"
 "netinet/in.h" "netinet/in_systm.h" "netinet/ip.h" "net/if.h"
 "netdb.h" "errno.h" "netinet/tcp.h" "fcntl.h" "signal.h" )

;;; then the stuff we're looking for
((:integer af-inet "AF_INET" "IP Protocol family")
 (:integer af-unspec "AF_UNSPEC" "Unspecified.")
 (:integer af-local
           #+(or sunos solaris) "AF_UNIX"
           #-(or sunos solaris) "AF_LOCAL"
           "Local to host (pipes and file-domain).")
 (:integer sigterm "SIGTERM")
 (:structure stat ("struct stat"
                   (integer dev "dev_t" "st_dev")
                   (integer atime "time_t" "st_atime")))


 (:function accept ("accept" int
                             (socket int)
                             (my-addr (* t))
                             (addrlen int :in-out)))
 (:function bind ("bind" int
                         (sockfd int)
                         (my-addr (* t))
                         (addrlen int)))
 (:function getpid ("getpid" int ))
 (:function getppid ("getppid" int))
 (:function kill ("kill" int
                         (pid int) (signal int)))
 (:function mkdir ("mkdir" int
                           (name c-string))))

