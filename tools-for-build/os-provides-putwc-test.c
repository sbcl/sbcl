/* test to build and run so that we know if we have putwc */

#include <stdio.h>
#include <wchar.h>

int main ()
{
    wchar_t a = 'a';
    putwc(a, stdout);
    return 104;
}
