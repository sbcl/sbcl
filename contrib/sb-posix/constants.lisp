;;; -*- Lisp -*-

;;; This isn't really lisp, but it's definitely a source file.

;;; first, the headers necessary to find definitions of everything
(#||#
 "sys/types.h"
 "unistd.h"
 "sys/stat.h"
 
 "sys/socket.h" "sys/un.h" "netinet/in.h" "netinet/in_systm.h"
 "netinet/ip.h" "net/if.h" "netdb.h" "errno.h" "netinet/tcp.h"
 "fcntl.h" "sys/mman.h"
 "dirent.h" "signal.h")

;;; then the stuff we're looking for
((:integer af-inet "AF_INET" "IP Protocol family")

 (:type uid-t "uid_t")
 (:type gid-t "gid_t")

 (:type pid-t "pid_t")

 ;; signals
 (:integer SIGHUP "SIGHUP" "terminal line hangup.")
 (:integer SIGINT "SIGINT" "interrupt program.")
 (:integer SIGQUIT "SIGQUIT" "quit program.")
 (:integer SIGILL "SIGILL" "illegal instruction.")
 (:integer SIGTRAP "SIGTRAP" "trace trap.")
 (:integer SIGABRT "SIGABRT" "abort program (formerly SIGIOT).")
 (:integer SIGEMT "SIGEMT" "emulate instruction executed.")
 (:integer SIGFPE "SIGFPE" "floating-point exception.")
 (:integer SIGKILL "SIGKILL" "kill program.")
 (:integer SIGBUS "SIGBUS" "bus error.")
 (:integer SIGSEGV "SIGSEGV" "segmentation violation.")
 (:integer SIGSYS "SIGSYS" "non-existent system call invoked.")
 (:integer SIGPIPE "SIGPIPE" "write on a pipe with no reader.")
 (:integer SIGALRM "SIGALRM" "real-time timer expired.")
 (:integer SIGTERM "SIGTERM" "software termination signal.")
 (:integer SIGURG "SIGURG" "urgent condition present on socket.")
 (:integer SIGSTOP "SIGSTOP" "stop (cannot be caught or ignored).")
 (:integer SIGTSTP "SIGTSTP" "stop signal generated from keyboard.")
 (:integer SIGCONT "SIGCONT" "continue after stop.")
 (:integer SIGCHLD "SIGCHLD" "child status has changed.")
 (:integer SIGTTIN "SIGTTIN" "background read attempted from control terminal.")
 (:integer SIGTTOU "SIGTTOU" "background write attempted to control terminal.")
 (:integer SIGIO "SIGIO" "I/O is possible on a descriptor (see fcntl(2)).")
 (:integer SIGXCPU "SIGXCPU" "cpu time limit exceeded (see setrlimit(2)).")
 (:integer SIGXFSZ "SIGXFSZ" "file size limit exceeded (see setrlimit(2)).")
 (:integer SIGVTALRM "SIGVTALRM" "virtual time alarm (see setitimer(2)).")
 (:integer SIGPROF "SIGPROF" "profiling timer alarm (see setitimer(2)).")
 (:integer SIGWINCH "SIGWINCH" "Window size change.")
 (:integer SIGINFO "SIGINFO" "status request from keyboard.")
 (:integer SIGUSR1 "SIGUSR1" "User defined signal 1.")
 (:integer SIGUSR2 "SIGUSR2" "User defined signal 2.")
 (:integer SIGRTMIN "SIGRTMIN" "Smallest real-time signal number.")
 (:integer SIGRTMAX "SIGRTMAX" "Largest real-time signal number.")
 
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

 ;; mmap()
 (:type off-t "off_t")
 (:integer prot-none "PROT_NONE" "mmap: no protection")
 (:integer prot-read "PROT_READ" "mmap: read protection")
 (:integer prot-write "PROT_WRITE" "mmap: write protection")
 (:integer prot-exec "PROT_EXEC" "mmap: execute protection")
 (:integer map-shared "MAP_SHARED" "mmap: shared memory")
 (:integer map-private "MAP_PRIVATE" "mmap: private mapping")
 (:integer map-fixed "MAP_FIXED" "mmap: map at given location")

 ;; msync()
 (:integer ms-async "MS_ASYNC" "msync: return immediately")
 (:integer ms-sync "MS_SYNC" "msync: perform synchronous writes")
 (:integer ms-invalidate "MS_INVALIDATE" "msync: invalidate all cached data")

 ;; opendir()
 (:structure dirent
	     ("struct dirent"
	      (:c-string name "char *" "d_name"
			 :distrust-length #+solaris t #-solaris nil)))
 )
