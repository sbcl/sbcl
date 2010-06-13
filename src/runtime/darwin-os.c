/*
 * This is the Darwin incarnation of OS-dependent routines. See also
 * "bsd-os.c".
 */

/*
 * This software is part of the SBCL system. See the README file for
 * more information.
 *
 * This software is derived from the CMU CL system, which was
 * written at Carnegie Mellon University and released into the
 * public domain. The software is in the public domain and is
 * provided with absolutely no warranty. See the COPYING and CREDITS
 * files for more information.
 */

#include "sbcl.h"
#include "globals.h"
#include "runtime.h"
#include <signal.h>
#include <limits.h>
#include <mach-o/dyld.h>
#include "bsd-os.h"
#include <errno.h>
#include <dlfcn.h>

char *
os_get_runtime_executable_path(int external)
{
    char path[PATH_MAX + 1];
    uint32_t size = sizeof(path);

    if (_NSGetExecutablePath(path, &size) == -1)
        return NULL;
    else
        path[size] = '\0';

    return copied_string(path);
}

