#include <string.h>
#include <stdlib.h>

#define SIZE 128*1024 // twice the largest page size
void alloca_test()
{
  void* foo = alloca(SIZE);
  memset(foo, 0xff, SIZE);
}
