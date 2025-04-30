/* test for presence of __riscv_flush_icache */

#include <sys/cachectl.h>

int main ()
{
    __riscv_flush_icache(&main, 512, 0);
    return 104;
}
