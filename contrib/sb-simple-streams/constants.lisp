;;; -*- Lisp -*-

;;; This code is in the public domain.

;;; The cmucl implementation of simple-streams was done by Paul Foley,
;;; who placed the code in the public domain.  Sbcl port by Rudi
;;; Schlatte.

;;; Some constants that are not (yet?) in sbcl itself.  Basically,
;;; constants needed for calling mmap from sbcl.

;;; TODO (Rudi 2003-05-12): The contents of this file, along with
;;; unix.lisp, should presumably end up somewhere else, either in sbcl
;;; itself or in sb-posix.

("stdio.h" "sys/types.h" "sys/stat.h" "fcntl.h" "asm/errno.h" "sys/mman.h")

((:integer eacces "EACCES" "Error code for access error")
 (:integer prot-none "PROT_NONE" "mmap: no protection")
 (:integer prot-read "PROT_READ" "mmap: read protection")
 (:integer prot-write "PROT_WRITE" "mmap: write protection")
 (:integer prot-exec "PROT_EXEC" "mmap: execute protection")
 (:integer map-shared "MAP_SHARED" "mmap: shared memory")
 (:integer map-private "MAP_PRIVATE" "mmap: private mapping")
 (:integer map-fixed "MAP_FIXED" "mmap: map at given location"))


