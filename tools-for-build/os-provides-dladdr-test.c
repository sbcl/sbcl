/* test to build and run so that we know if we have dladdr
 */

#include <stdlib.h>

/* bloody FSF dlcfn.h won't give us dladdr without this */
#define _GNU_SOURCE

#include <dlfcn.h>

int main ()
{
   void * handle = dlopen((void*)0, RTLD_GLOBAL | RTLD_NOW);
   void * addr = dlsym(handle, "printf");
   Dl_info * info = (Dl_info*) malloc(sizeof(Dl_info));
   dladdr(addr, info);
   if (strcmp(info->dli_sname, "printf")) {
       return 1;
   } else {
       return 104;
   }
}
