/*
 * Wrapper functions for SUSv2 large file support. Linux defaults to a
 * 32-bit off_t and hides the largefile-capable versions of the
 * syscalls behind preprocessor magic, rather than making them
 * reliably available using dlsym.
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

#include "genesis/config.h"

#ifdef LISP_FEATURE_LARGEFILE

#include <sys/mman.h>
#include <sys/types.h>
#include <dirent.h>
#include <unistd.h>
#include <sys/stat.h>

off_t
lseek_largefile(int fildes, off_t offset, int whence) {
    return lseek(fildes, offset, whence);
}

int
truncate_largefile(const char *path, off_t length) {
    return truncate(path, length);
}

int
ftruncate_largefile(int fd, off_t length) {
    return ftruncate(fd, length);
}

void*
mmap_largefile(void *start, size_t length, int prot, int flags, int fd, off_t offset) {
    return mmap(start, length, prot, flags, fd, offset);
}

int
stat_largefile(const char *file_name, struct stat *buf) {
    return stat(file_name, buf);
}

int
fstat_largefile(int filedes, struct stat *buf) {
    return fstat(filedes, buf);
}

int
lstat_largefile(const char *file_name, struct stat *buf) {
    return lstat(file_name, buf);
}

struct dirent64 *
readdir_largefile(DIR *dir) {
    return readdir64(dir);
}

#endif
