/*
 * See if SA_NODEFER makes sigaction ignore sa_mask
 * altogether. According to POSIX SA_NODEFER means: 'don't add the
 * handler's signal to the mask'.
 */

/*
 * This software is part of the SBCL system. See the README file for
 * more information.
 *
 * While most of SBCL is derived from the CMU CL system, many
 * utilities for the build process (like this one) were written from
 * scratch after the fork from CMU CL.
 *
 * This software is in the public domain and is provided with
 * absolutely no warranty. See the COPYING and CREDITS files for
 * more information.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

void
handler(int signal, siginfo_t *info, void *void_context)
{
    sigset_t empty, current;
    int i;
    sigemptyset(&empty);
    sigprocmask(SIG_BLOCK, &empty, &current);
    for(i = 1; i < NSIG; i++)
        if (sigismember(&current, i) != ((i == SIGABRT) ? 1 : 0))
            exit(128 + i);
    exit(104);
}

int
main (int argc, char *argv[])
{
    struct sigaction sa;

    sa.sa_flags = SA_SIGINFO | SA_NODEFER;
    sa.sa_sigaction = handler;
    sigemptyset(&sa.sa_mask);
    sigaddset(&sa.sa_mask, SIGABRT);
    sigaction(SIGTRAP, &sa, NULL);
    kill(getpid(), SIGTRAP);
    while (1) sleep(1);
}
