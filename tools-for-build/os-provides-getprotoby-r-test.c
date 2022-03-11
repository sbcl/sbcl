/* test to build and run so that we know if we have getprotobyname_r
 * and getprotobynumber_r */

#include <netdb.h>

#define BUFSIZE 1024

int main ()
{
#if __NetBSD__
    /*
     * For me the code below compiles, but it segfaults when actually run:
     * (gdb) where
     *  #0  0x00007f7fc20050a9 in _rtld_symlook_obj () from /usr/libexec/ld.elf_so
     *  #1  0x00007f7fc20059e0 in _rtld_symlook_default () from /usr/libexec/ld.elf_so
     *  #2  0x00007f7fc20060d3 in _rtld_find_plt_symdef () from /usr/libexec/ld.elf_so
     *  #3  0x00007f7fc2000bc5 in _rtld_bind () from /usr/libexec/ld.elf_so
     *  #4  0x00007f7fc20007fd in _rtld_bind_start () from /usr/libexec/ld.elf_so
     *  #5  0x0000000000000000 in ?? ()
     *
     * I guess that somebody decided it would be clever to pretend that the calls exist,
     * but then /usr/libexec/ld.elf_so can't figure out what to do.
     *
     * This is EXTREMELY disconcerting, as it occurs during 'make-config.sh'
     * and so you're not sure (without a nontrivial amount of debugging)
     * whether configuration completed without error.
     */
    return 1; // just say "no"
#else
    struct protoent result_buf;
    struct protoent *result;
    char buf[BUFSIZE];
    getprotobyname_r("", &result_buf, buf, BUFSIZE, &result);
    getprotobynumber_r(0, &result_buf, buf, BUFSIZE, &result);
    return 104;
#endif
}
