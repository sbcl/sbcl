;;; -*- Lisp -*-

;;; This isn't really lisp, but it's definitely a source file.

;;; first, the headers necessary to find definitions of everything
(#||#
 "sys/types.h"
 "unistd.h"
 "sys/stat.h"
 
 "sys/socket.h" "sys/un.h" "netinet/in.h" "netinet/in_systm.h"
 "netinet/ip.h" "net/if.h" "netdb.h" "errno.h" "netinet/tcp.h"
 "fcntl.h" )

;;; then the stuff we're looking for
((:integer af-inet "AF_INET" "IP Protocol family")

 (:type uid-t "uid_t")
 (:type gid-t "gid_t")

 (:type pid-t "pid_t")

 ;; mode_t
 (:type mode-t "mode_t")
 (:integer s-isuid "S_ISUID")
 (:integer s-isgid "S_ISGID")
 (:integer s-isvtx "S_ISVTX")
 (:integer s-irusr "S_IRUSR")
 (:integer s-iwusr "S_IWUSR")
 (:integer s-ixusr "S_IXUSR")
 (:integer s-iread "S_IRUSR")
 (:integer s-iwrite "S_IWUSR")
 (:integer s-iexec "S_IXUSR")
 (:integer s-irgrp "S_IRGRP")
 (:integer s-iwgrp "S_IWGRP")
 (:integer s-ixgrp "S_IXGRP")
 (:integer s-iroth "S_IROTH")
 (:integer s-iwoth "S_IWOTH")
 (:integer s-ixoth "S_IXOTH")

 ;; access()
 (:integer r-ok "R_OK")
 (:integer w-ok "W_OK")
 (:integer x-ok "X_OK")
 (:integer f-ok "F_OK")
 )