/* test to build and run so that we know if we have blksize_t */

#include <sys/types.h>

int main ()
{
    blksize_t s = 0;
    s = s + s;
    return 104;
}
