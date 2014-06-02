/*
 * The Android incarnation of OS-dependent routines. See also
 * $(sbcl_arch)-android-os.c. Most of the os-dependent routines are
 * actually implemented in linux-os.c
 *
 * This file (along with os.h) exports an OS-independent interface to
 * the operating system VM facilities. Surprise surprise, this
 * interface looks a lot like the Mach interface (but simpler in some
 * places). For some operating systems, a subset of these functions
 * will have to be emulated.
 */

#include <sys/utsname.h>

extern char * software_version () {
  struct utsname u;
  int result = uname(&u);
  if (result < 0)
    return 0;
  return strdup(u.release);
}
