#include <windows.h>

int main()
{
    WakeByAddressAll((void*)&main);
    return 104;
}
