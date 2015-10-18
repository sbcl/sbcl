/* test to build and run so that we know if we have dladdr
 */

/* bloody FSF dlcfn.h won't give us dladdr without this */
#define _GNU_SOURCE

#include <stdlib.h>
#include <dlfcn.h>

#ifdef __NetBSD__
#include <sys/param.h>
#endif

int main ()
{
#if defined(__NetBSD_Version__) && __NetBSD_Version__ < 700000001
  /* dladdr(3) is broken on some NetBSD versions before 7.0 */
  return 1;
#else
   void * handle = dlopen((void*)0, RTLD_GLOBAL | RTLD_NOW);
   void * addr = dlsym(handle, "printf");
   Dl_info * info = (Dl_info*) malloc(sizeof(Dl_info));
   if (dladdr(addr, info) == 0) {
       return 1;
   } else {
       return 104;
   }
#endif
}
