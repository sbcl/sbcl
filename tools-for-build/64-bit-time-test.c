#include <time.h>

int main ()
{
    if (sizeof( time_t) == 8)
        return 104;
    return 1;
}
