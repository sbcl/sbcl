;;; -*- Lisp -*-

#-win32 ("sys/types.h" "sys/socket.h" "netdb.h")
#+win32 ("winsock2.h" "ws2tcpip.h")

((:structure addrinfo ("struct addrinfo"
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
                       ;; This can be a void pointer since it has to
                       ;; be cast to the respectively appropriate
                       ;; address structure anyway.
                       ((* t) addr "struct sockaddr*" "ai_addr")
                       (c-string-pointer canonname "char *" "ai_canonname")
                       ((* (struct addrinfo)) next "struct addrinfo*" "ai_next")))

 (:function getaddrinfo ("getaddrinfo"
                         int
                         (node c-string)
                         (service c-string)
                         (hints (* addrinfo))
                         (res (* (* addrinfo)))))


 (:function freeaddrinfo ("freeaddrinfo"
                          void
                          (res (* addrinfo))))


 (:function gai-strerror ("gai_strerror"
                         c-string
                         (error-code int)))

 (:function getnameinfo ("getnameinfo"
                         int
                         ;; This can be a void pointer since it has to
                         ;; be cast to the respectively appropriate
                         ;; address structure anyway.
                         (address (* t))
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
 #-(or freebsd dragonfly win32)
 (:integer EAI-ADDRFAMILY "EAI_ADDRFAMILY")
 (:integer EAI-MEMORY "EAI_MEMORY")
 (:integer EAI-FAIL "EAI_FAIL")
 (:integer EAI-AGAIN "EAI_AGAIN")
 #-win32
 (:integer EAI-SYSTEM "EAI_SYSTEM")

 (:integer NI-NAMEREQD "NI_NAMEREQD"))
