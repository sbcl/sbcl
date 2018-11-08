#include <sys/mman.h>

int
main()
{
        if (mmap(0, 1, PROT_EXEC|PROT_READ|PROT_WRITE,
                MAP_PRIVATE|MAP_ANON, -1, 0) == MAP_FAILED)
                return 1;
        return 0;
}
