#include <windows.h>

int main ()
{
#ifdef _UCRT
  return 104;
#else
    return 1;
#endif
}
