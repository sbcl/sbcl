#include <features.h>

int main ()
{
#if defined __linux__ && ((__GLIBC__ > 2) || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 34))
    return 104;
#else
    return 1;
#endif
}
