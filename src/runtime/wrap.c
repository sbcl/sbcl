/*
 * wrappers around low-level operations to provide a simpler interface
 * to the operations that Lisp (and some contributed modules) needs.
 *
 * The functions in this file are typically called directly from Lisp.
 * Thus, when their signature changes, they don't need updates in a .h
 * file somewhere, but they do need updates in the Lisp code. FIXME:
 * It would be nice to enforce this at compile time. It mighn't even
 * be all that hard: make the cross-compiler versions of DEFINE-ALIEN-FOO
 * macros accumulate strings in a list which then gets written out at
 * the end of sbcl2.h at the end of cross-compilation, then rerun
 * 'make' in src/runtime/ using the new sbcl2.h as sbcl.h (and make
 * sure that all the files in src/runtime/ include sbcl.h). */

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

#include <sys/types.h>
#include <dirent.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <errno.h>
#include <limits.h>
#include <fcntl.h>

#ifndef LISP_FEATURE_WIN32
#include <pwd.h>
#include <sys/wait.h>
#include <netdb.h>
#endif
#include <stdio.h>

#if defined(LISP_FEATURE_WIN32)
#define WIN32_LEAN_AND_MEAN
#include <errno.h>
#endif

#include "runtime.h"
#include "util.h"
#include "wrap.h"

/* Although it might seem as though this should be in some standard
   Unix header, according to Perry E. Metzger, in a message on
   sbcl-devel dated 2004-03-29, this is the POSIXly-correct way of
   using environ: by an explicit declaration.  -- CSR, 2004-03-30 */
extern char **environ;

/*
 * stuff needed by CL:DIRECTORY and other Lisp directory operations
 */


/*
 * readlink(2) stuff
 */

#ifndef LISP_FEATURE_WIN32
/* a wrapped version of readlink(2):
 *   -- If path isn't a symlink, or is a broken symlink, return 0.
 *   -- If path is a symlink, return a newly allocated string holding
 *      the thing it's linked to. */
char *
wrapped_readlink(char *path)
{
    int bufsiz = strlen(path) + 16;
    while (1) {
        char *result = malloc(bufsiz);
        int n_read = readlink(path, result, bufsiz);
        if (n_read < 0) {
            free(result);
            return 0;
        } else if (n_read < bufsiz) {
            result[n_read] = 0;
            return result;
        } else {
            free(result);
            bufsiz *= 2;
        }
    }
}
#endif

/*
 * realpath(3), including a wrapper for Windows.
 */
char * sb_realpath (char *path)
{
#ifndef LISP_FEATURE_WIN32
    char *ret;
    int errnum;

    if ((ret = calloc(PATH_MAX, sizeof(char))) == NULL)
        return NULL;
    if (realpath(path, ret) == NULL) {
        errnum = errno;
        free(ret);
        errno = errnum;
        return NULL;
    }
    return(ret);
#else
    char *ret;
    char *cp;
    int errnum;

    if ((ret = calloc(MAX_PATH, sizeof(char))) == NULL)
        return NULL;
    if (GetFullPathName(path, MAX_PATH, ret, cp) == 0) {
        errnum = errno;
        free(ret);
        errno = errnum;
        return NULL;
    }
    return(ret);
#endif
}

/* readdir, closedir, and dirent name accessor. The first three are not strictly
 * necessary, but should save us some #!+netbsd in the build, and this also allows
 * building Windows versions using the non-ANSI variants of FindFirstFile &co
 * under the same API. (Use a structure that appends the handle to the WIN32_FIND_DATA
 * as the return value from sb_opendir, on sb_readdir grab the name from the previous
 * call and save the new one.) Nikodemus thought he would have to do that to support
 * DIRECTORY on UNC paths, but turns out opendir &co do TRT on Windows already -- so
 * leaving that bit of tedium for a later date, once we figure out the whole *A vs. *W
 * issue out properly. ...FIXME, obviously, as per above.
 *
 * Once that is done, the lisp side functions are best named OS-OPENDIR, etc.
 */
extern DIR *
sb_opendir(char * name)
{
    return opendir(name);
}

extern struct dirent *
sb_readdir(DIR * dirp)
{
    return readdir(dirp);
}

extern int
sb_closedir(DIR * dirp)
{
    return closedir(dirp);
}

extern char *
sb_dirent_name(struct dirent * ent)
{
    return ent->d_name;
}

/*
 * stat(2) stuff
 */

static void
copy_to_stat_wrapper(struct stat_wrapper *to, struct stat *from)
{
#define FROB(stem) to->wrapped_st_##stem = from->st_##stem
#ifndef LISP_FEATURE_WIN32
#define FROB2(stem) to->wrapped_st_##stem = from->st_##stem
#else
#define FROB2(stem) to->wrapped_st_##stem = 0;
#endif
    FROB(dev);
    FROB2(ino);
    FROB(mode);
    FROB(nlink);
    FROB2(uid);
    FROB2(gid);
    FROB(rdev);
    FROB(size);
    FROB2(blksize);
    FROB2(blocks);
    FROB(atime);
    FROB(mtime);
    FROB(ctime);
#undef FROB
}

int
stat_wrapper(const char *file_name, struct stat_wrapper *buf)
{
    struct stat real_buf;
    int ret;

#ifdef LISP_FEATURE_WIN32
    /*
     * Windows won't match the last component of a pathname if there
     * is a trailing #\/ or #\\, except if it's <drive>:\ or <drive>:/
     * in which case it behaves the other way around. So we remove the
     * trailing directory separator unless we are being passed just a
     * drive name (e.g. "c:\\").  Some, but not all, of this
     * strangeness is documented at Microsoft's support site (as of
     * 2006-01-08, at
     * <http://support.microsoft.com/default.aspx?scid=kb;en-us;168439>)
     */
    char file_buf[MAX_PATH];
    strcpy(file_buf, file_name);
    int len = strlen(file_name);
    if (len != 0 && (file_name[len-1] == '/' || file_name[len-1] == '\\') &&
        !(len == 3 && file_name[1] == ':' && isalpha(file_name[0])))
        file_buf[len-1] = '\0';
    file_name = file_buf;
#endif

    if ((ret = stat(file_name,&real_buf)) >= 0)
        copy_to_stat_wrapper(buf, &real_buf);
    return ret;
}

#ifndef LISP_FEATURE_WIN32
int
lstat_wrapper(const char *file_name, struct stat_wrapper *buf)
{
    struct stat real_buf;
    int ret;
    if ((ret = lstat(file_name,&real_buf)) >= 0)
        copy_to_stat_wrapper(buf, &real_buf);
    return ret;
}
#else
/* cleaner to do it here than in Lisp */
int lstat_wrapper(const char *file_name, struct stat_wrapper *buf)
{
    return stat_wrapper(file_name, buf);
}
#endif

int
fstat_wrapper(int filedes, struct stat_wrapper *buf)
{
    struct stat real_buf;
    int ret;
    if ((ret = fstat(filedes,&real_buf)) >= 0)
        copy_to_stat_wrapper(buf, &real_buf);
    return ret;
}

/* A wrapper for mkstemp(3), for two reasons: (1) mkstemp does not
   exist on Windows; (2) by passing down a mode_t, we don't need a
   binding to chmod in SB-UNIX, and need not concern ourselves with
   umask issues if we want to use mkstemp to make new files in
   OPEN. */
int sb_mkstemp (char *template, mode_t mode) {
#ifdef LISP_FEATURE_WIN32
#define PATHNAME_BUFFER_SIZE MAX_PATH
#define MKTEMP _mktemp
#else
#define PATHNAME_BUFFER_SIZE PATH_MAX
#define MKTEMP mktemp
#endif
  int fd;
  char buf[PATHNAME_BUFFER_SIZE];

  while (1) {
    /* Fruit fallen from the tree: for people who like
       microoptimizations, we might not need to copy the whole
       template on every loop, but only the last several characters.
       But I didn't feel like testing the boundary cases in Windows's
       _mktemp. */
    strncpy(buf, template, PATHNAME_BUFFER_SIZE);
    buf[PATHNAME_BUFFER_SIZE-1]=0; /* force NULL-termination */
    if (MKTEMP(buf)) {
      if ((fd=open(buf, O_CREAT|O_EXCL|O_RDWR, mode))!=-1) {
        strcpy(template, buf);
        return (fd);
      } else
        if (errno != EEXIST)
          return (-1);
    } else
      return (-1);
  }
#undef MKTEMP
#undef PATHNAME_BUFFER_SIZE
}


/*
 * getpwuid() stuff
 */

#ifndef LISP_FEATURE_WIN32
/* Return a newly-allocated string holding the username for "uid", or
 * NULL if there's no such user.
 *
 * KLUDGE: We also return NULL if malloc() runs out of memory
 * (returning strdup() result) since it's not clear how to handle that
 * error better. -- WHN 2001-12-28 */
char *
uid_username(int uid)
{
    struct passwd *p = getpwuid(uid);
    if (p) {
        /* The object *p is a static struct which'll be overwritten by
         * the next call to getpwuid(), so it'd be unsafe to return
         * p->pw_name without copying. */
        return strdup(p->pw_name);
    } else {
        return 0;
    }
}

char *
uid_homedir(uid_t uid)
{
    struct passwd *p = getpwuid(uid);
    if(p) {
        /* Let's be careful about this, shall we? */
        size_t len = strlen(p->pw_dir);
        if (p->pw_dir[len-1] == '/') {
            return strdup(p->pw_dir);
        } else {
            char *result = malloc(len + 2);
            if (result) {
                unsigned int nchars = sprintf(result,"%s/",p->pw_dir);
                if (nchars == len + 1) {
                    return result;
                } else {
                    return 0;
                }
            } else {
                return 0;
            }
        }
    } else {
        return 0;
    }
}
#endif /* !LISP_FEATURE_WIN32 */

/*
 * functions to get miscellaneous C-level variables
 *
 * (Doing this by calling functions lets us borrow the smarts of the C
 * linker, so that things don't blow up when libc versions and thus
 * variable locations change between compile time and run time.)
 */

char **
wrapped_environ()
{
    return environ;
}

#ifdef LISP_FEATURE_WIN32
#include <windows.h>
#include <time.h>
/*
 * faked-up implementation of select(). Right now just enough to get through
 * second genesis.
 */
int select(int top_fd, DWORD *read_set, DWORD *write_set, DWORD *except_set, time_t *timeout)
{
    /*
     * FIXME: Going forward, we may want to use MsgWaitForMultipleObjects
     * in order to support a windows message loop inside serve-event.
     */
    HANDLE handles[MAXIMUM_WAIT_OBJECTS];
    int fds[MAXIMUM_WAIT_OBJECTS];
    int num_handles;
    int i;
    DWORD retval;
    int polling_write;
    DWORD win_timeout;

    num_handles = 0;
    polling_write = 0;
    for (i = 0; i < top_fd; i++) {
        if (except_set) except_set[i >> 5] = 0;
        if (write_set && (write_set[i >> 5] & (1 << (i & 31)))) polling_write = 1;
        if (read_set[i >> 5] & (1 << (i & 31))) {
            read_set[i >> 5] &= ~(1 << (i & 31));
            fds[num_handles] = i;
            handles[num_handles++] = (HANDLE) _get_osfhandle(i);
        }
    }

    win_timeout = INFINITE;
    if (timeout) win_timeout = (timeout[0] * 1000) + timeout[1];

    /* Last parameter here is timeout in milliseconds. */
    /* retval = WaitForMultipleObjects(num_handles, handles, 0, INFINITE); */
    retval = WaitForMultipleObjects(num_handles, handles, 0, win_timeout);

    if (retval < WAIT_ABANDONED) {
        /* retval, at this point, is the index of the single live HANDLE/fd. */
        read_set[fds[retval] >> 5] |= (1 << (fds[retval] & 31));
        return 1;
    }
    return polling_write;
}

/*
 * Windows doesn't have gettimeofday(), and we need it for the compiler,
 * for serve-event, and for a couple other things. We don't need a timezone
 * yet, however, and the closest we can easily get to a timeval is the
 * seconds part. So that's what we do.
 */
int gettimeofday(long *timeval, long *timezone)
{
    timeval[0] = time(NULL);
    timeval[1] = 0;

    return 0;
}
#endif


/* We will need to define these things or their equivalents for Win32
   eventually, but for now let's get it working for everyone else. */
#ifndef LISP_FEATURE_WIN32
/* From SB-BSD-SOCKETS, to get h_errno */
int get_h_errno()
{
    return h_errno;
}

/* From SB-POSIX, wait-macros */
int wifexited(int status) {
    return WIFEXITED(status);
}
int wexitstatus(int status) {
    return WEXITSTATUS(status);
}
int wifsignaled(int status) {
    return WIFSIGNALED(status);
}
int wtermsig(int status) {
    return WTERMSIG(status);
}
int wifstopped(int status) {
    return WIFSTOPPED(status);
}
int wstopsig(int status) {
    return WSTOPSIG(status);
}
/* FIXME: POSIX also defines WIFCONTINUED, but that appears not to
   exist on at least Linux... */
#endif  /* !LISP_FEATURE_WIN32 */

/* From SB-POSIX, stat-macros */
int s_isreg(mode_t mode)
{
    return S_ISREG(mode);
}
int s_isdir(mode_t mode)
{
    return S_ISDIR(mode);
}
int s_ischr(mode_t mode)
{
    return S_ISCHR(mode);
}
int s_isblk(mode_t mode)
{
    return S_ISBLK(mode);
}
int s_isfifo(mode_t mode)
{
    return S_ISFIFO(mode);
}
#ifndef LISP_FEATURE_WIN32
int s_islnk(mode_t mode)
{
#ifdef S_ISLNK
    return S_ISLNK(mode);
#else
    return ((mode & S_IFMT) == S_IFLNK);
#endif
}
int s_issock(mode_t mode)
{
#ifdef S_ISSOCK
    return S_ISSOCK(mode);
#else
    return ((mode & S_IFMT) == S_IFSOCK);
#endif
}
#endif /* !LISP_FEATURE_WIN32 */
