/* test to build and run so that we know if we have dlopen
 */

#include <dlfcn.h>

int main ()
{
   void * handle = dlopen((void*)0, RTLD_GLOBAL | RTLD_NOW);
   void * addr = dlsym(handle, "printf");
   if (addr) {
       return 104;
   } else {
       return 0;
   }
}
