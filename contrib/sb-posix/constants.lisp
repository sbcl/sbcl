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
 (:integer SIGHUP "SIGHUP" #+sb-doc "terminal line hangup.")
 (:integer SIGINT "SIGINT" #+sb-doc "interrupt program.")
 (:integer SIGQUIT "SIGQUIT" #+sb-doc "quit program.")
 (:integer SIGILL "SIGILL" #+sb-doc "illegal instruction.")
 (:integer SIGTRAP "SIGTRAP" #+sb-doc "trace trap.")
 (:integer SIGABRT "SIGABRT" #+sb-doc "abort program (formerly SIGIOT).")
 (:integer SIGEMT "SIGEMT" #+sb-doc "emulate instruction executed.")
 (:integer SIGFPE "SIGFPE" #+sb-doc "floating-point exception.")
 (:integer SIGKILL "SIGKILL" #+sb-doc "kill program.")
 (:integer SIGBUS "SIGBUS" #+sb-doc "bus error.")
 (:integer SIGSEGV "SIGSEGV" #+sb-doc "segmentation violation.")
 (:integer SIGSYS "SIGSYS" #+sb-doc "non-existent system call invoked.")
 (:integer SIGPIPE "SIGPIPE" #+sb-doc "write on a pipe with no reader.")
 (:integer SIGALRM "SIGALRM" #+sb-doc "real-time timer expired.")
 (:integer SIGTERM "SIGTERM" #+sb-doc "software termination signal.")
 (:integer SIGURG "SIGURG" #+sb-doc "urgent condition present on socket.")
 (:integer SIGSTOP "SIGSTOP" #+sb-doc "stop (cannot be caught or ignored).")
 (:integer SIGTSTP "SIGTSTP" #+sb-doc "stop signal generated from keyboard.")
 (:integer SIGCONT "SIGCONT" #+sb-doc "continue after stop.")
 (:integer SIGCHLD "SIGCHLD" #+sb-doc "child status has changed.")
 (:integer SIGTTIN "SIGTTIN"
           #+sb-doc "background read attempted from control terminal.")
 (:integer SIGTTOU "SIGTTOU"
           #+sb-doc "background write attempted to control terminal.")
 (:integer SIGIO "SIGIO"
           #+sb-doc "I/O is possible on a descriptor (see fcntl(2)).")
 (:integer SIGXCPU "SIGXCPU"
           #+sb-doc "cpu time limit exceeded (see setrlimit(2)).")
 (:integer SIGXFSZ "SIGXFSZ"
           #+sb-doc "file size limit exceeded (see setrlimit(2)).")
 (:integer SIGVTALRM "SIGVTALRM"
           #+sb-doc "virtual time alarm (see setitimer(2)).")
 (:integer SIGPROF "SIGPROF"
           #+sb-doc "profiling timer alarm (see setitimer(2)).")
 (:integer SIGWINCH "SIGWINCH" #+sb-doc "Window size change.")
 (:integer SIGINFO "SIGINFO" #+sb-doc "status request from keyboard.")
 (:integer SIGUSR1 "SIGUSR1" #+sb-doc "User defined signal 1.")
 (:integer SIGUSR2 "SIGUSR2" #+sb-doc "User defined signal 2.")
 (:integer SIGRTMIN "SIGRTMIN" #+sb-doc "Smallest real-time signal number.")
 (:integer SIGRTMAX "SIGRTMAX" #+sb-doc "Largest real-time signal number.")
 
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
 (:integer prot-none "PROT_NONE" #+sb-doc "mmap: no protection")
 (:integer prot-read "PROT_READ" #+sb-doc "mmap: read protection")
 (:integer prot-write "PROT_WRITE" #+sb-doc "mmap: write protection")
 (:integer prot-exec "PROT_EXEC" #+sb-doc "mmap: execute protection")
 (:integer map-shared "MAP_SHARED" #+sb-doc "mmap: shared memory")
 (:integer map-private "MAP_PRIVATE" #+sb-doc "mmap: private mapping")
 (:integer map-fixed "MAP_FIXED" #+sb-doc "mmap: map at given location")

 ;; msync()
 (:integer ms-async "MS_ASYNC" #+sb-doc "msync: return immediately")
 (:integer ms-sync "MS_SYNC" #+sb-doc "msync: perform synchronous writes")
 (:integer ms-invalidate "MS_INVALIDATE"
           #+sb-doc "msync: invalidate all cached data")

 ;; opendir()
 (:structure dirent
	     ("struct dirent"
	      (:c-string name "char *" "d_name"
			 :distrust-length #+sunos t #-sunos nil)))
 )
