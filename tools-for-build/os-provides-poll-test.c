/* test to build and run so that we know if we have poll that works on
 * stdin and /dev/zero -- which is hopefully a sufficient sample to weed
 * out crappy versions like that on Darwin.
 */

#include <fcntl.h>
#include <poll.h>

int main ()
{
    struct pollfd fds;

    fds.fd = 0;
    fds.events = POLLIN|POLLPRI;
    fds.revents = 0;
    if (!((1 == poll(&fds, 1, -1)) && ((POLLIN|POLLPRI) & fds.revents)))
        return 0;

    fds.fd = open("/dev/zero", O_RDONLY);
    fds.events = POLLIN|POLLPRI;
    fds.revents = 0;
    if (!((1 == poll(&fds, 1, -1)) && ((POLLIN|POLLPRI) & fds.revents)))
        return 0;

    return 104;
}
