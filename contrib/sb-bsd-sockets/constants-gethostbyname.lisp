;;; -*- Lisp -*-

("sys/socket.h" "netdb.h")

((:function gethostbyname ("gethostbyname" (* hostent) (name c-string)))
 (:function gethostbyaddr ("gethostbyaddr" (* hostent)
                                           (addr (* t))
                                           (len int)
                                           (af int)))
 (:integer NETDB-INTERNAL "NETDB_INTERNAL" "See errno.")
 (:integer NETDB-SUCCESS "NETDB_SUCCESS" "No problem.")
 (:integer HOST-NOT-FOUND "HOST_NOT_FOUND" "Authoritative Answer Host not found.")
 (:integer TRY-AGAIN "TRY_AGAIN" "Non-Authoritative Host not found, or SERVERFAIL.")
 (:integer NO-RECOVERY "NO_RECOVERY" "Non recoverable errors, FORMERR, REFUSED, NOTIMP.")
 (:integer NO-DATA "NO_DATA" "Valid name, no data record of requested type.")
 (:integer NO-ADDRESS "NO_ADDRESS" "No address, look for MX record.")
 #-sunos
 (:function h-strerror ("hstrerror" c-string (errno int))))
