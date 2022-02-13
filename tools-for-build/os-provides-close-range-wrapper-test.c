/* We're really only interested in if this builds. If it does, the OS provides
 * close_range as a libc wrapper for the close_range syscall. If it doesn't
 * build, we fall back to syscall(2) if __NR_close_range is defined.
 */

/* glibc won't give us close_range without this */
#define _GNU_SOURCE
#include <unistd.h>

int main ()
{
    close_range(3, ~0U, 0);
    return 104;
}
