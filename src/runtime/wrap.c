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
#ifndef LISP_FEATURE_WIN32
#include <pwd.h>
#include <sys/wait.h>
#include <netdb.h>
#endif
#include <stdio.h>

#include "runtime.h"
#include "util.h"

/* Although it might seem as though this should be in some standard
   Unix header, according to Perry E. Metzger, in a message on
   sbcl-devel dated 2004-03-29, this is the POSIXly-correct way of
   using environ: by an explicit declaration.  -- CSR, 2004-03-30 */
extern char **environ;

/*
 * stuff needed by CL:DIRECTORY and other Lisp directory operations
 */

/* Unix directory operations think of "." and ".." as filenames, but
 * Lisp directory operations do not. */
int
is_lispy_filename(const char *filename)
{
    return strcmp(filename, ".") && strcmp(filename, "..");
}

/* Return a zero-terminated array of strings holding the Lispy filenames
 * (i.e. excluding the Unix magic "." and "..") in the named directory. */
char**
alloc_directory_lispy_filenames(const char *directory_name)
{
    DIR *dir_ptr = opendir(directory_name);
    char **result = 0;

    if (dir_ptr) { /* if opendir success */

        struct voidacc va;

        if (0 == voidacc_ctor(&va)) { /* if voidacc_ctor success */
            struct dirent *dirent_ptr;

            while ( (dirent_ptr = readdir(dir_ptr)) ) { /* until end of data */
                char* original_name = dirent_ptr->d_name;
                if (is_lispy_filename(original_name)) {
                    /* strdup(3) is in Linux and *BSD. If you port
                     * somewhere else that doesn't have it, it's easy
                     * to reimplement. */
                    char* dup_name = strdup(original_name);
                    if (!dup_name) { /* if strdup failure */
                        goto dtors;
                    }
                    if (voidacc_acc(&va, dup_name)) { /* if acc failure */
                        goto dtors;
                    }
                }
            }
            result = (char**)voidacc_give_away_result(&va);
        }

    dtors:
        voidacc_dtor(&va);
        /* ignoring closedir(3) return code, since what could we do?
         *
         * "Never ask questions you don't want to know the answer to."
         * -- William Irving Zumwalt (Rich Cook, _The Wizardry Quested_) */
        closedir(dir_ptr);
    }

    return result;
}

/* Free a result returned by alloc_directory_lispy_filenames(). */
void
free_directory_lispy_filenames(char** directory_lispy_filenames)
{
    char** p;

    /* Free the strings. */
    for (p = directory_lispy_filenames; *p; ++p) {
        free(*p);
    }

    /* Free the table of strings. */
    free(directory_lispy_filenames);
}

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
 * stat(2) stuff
 */

/* As of 0.6.12, the FFI can't handle 64-bit values. For now, we use
 * these munged-to-32-bits values for might-be-64-bit slots of
 * stat_wrapper as a workaround, so that at least we can still work
 * when values are small.
 *
 * FIXME: But of course we should fix the FFI so that we can use the
 * actual 64-bit values instead.  In fact, we probably have by now
 * (2003-10-03) on all working platforms except MIPS and HPPA; if some
 * motivated spark would simply fix those, this hack could go away.
 * -- CSR, 2003-10-03
 *
 * Some motivated spark fixed MIPS. -- ths, 2005-10-06 */

#if defined (LISP_FEATURE_LARGEFILE)
typedef dev_t ffi_dev_t;
typedef off_t ffi_off_t;
#elif defined(LISP_FEATURE_MIPS)
typedef unsigned long ffi_dev_t; /* Linux/MIPS struct stat doesn't use dev_t */
typedef off_t ffi_off_t;
#elif defined(LISP_FEATURE_DARWIN)
typedef dev_t ffi_dev_t;
typedef off_t ffi_off_t;
#else
typedef u32 ffi_dev_t; /* since Linux dev_t can be 64 bits */
typedef u32 ffi_off_t; /* since OpenBSD 2.8 st_size is 64 bits */
#endif

#if defined(LISP_FEATURE_DARWIN)
typedef blksize_t ffi_blksize_t;
#else
typedef unsigned long ffi_blksize_t;
#endif

/* a representation of stat(2) results which doesn't depend on CPU or OS */
struct stat_wrapper {
    /* KLUDGE: The verbose wrapped_st_ prefixes are to protect us from
     * the C preprocessor as wielded by the fiends of OpenBSD, who do
     * things like
     *    #define st_atime        st_atimespec.tv_sec
     * I remember when I was young and innocent, I read about how the
     * C preprocessor isn't to be used to globally munge random
     * lowercase symbols like this, because things like this could
     * happen, and I nodded sagely. But now I know better.:-| This is
     * another entry for Dan Barlow's ongoing episodic rant about C
     * header files, I guess.. -- WHN 2001-05-10 */
    ffi_dev_t     wrapped_st_dev;         /* device */
    ino_t         wrapped_st_ino;         /* inode */
    mode_t        wrapped_st_mode;        /* protection */
#ifndef LISP_FEATURE_WIN32
    nlink_t       wrapped_st_nlink;       /* number of hard links */
    uid_t         wrapped_st_uid;         /* user ID of owner */
    gid_t         wrapped_st_gid;         /* group ID of owner */
#else
    short         wrapped_st_nlink;       /* Win32 doesn't have nlink_t */
    short         wrapped_st_uid;         /* Win32 doesn't have st_uid */
    short         wrapped_st_gid;         /* Win32 doesn't have st_gid */
#endif
    ffi_dev_t     wrapped_st_rdev;        /* device type (if inode device) */
    ffi_off_t     wrapped_st_size;        /* total size, in bytes */
    ffi_blksize_t wrapped_st_blksize;     /* blocksize for filesystem I/O */
    unsigned long wrapped_st_blocks;      /* number of blocks allocated */
    time_t        wrapped_st_atime;       /* time_t of last access */
    time_t        wrapped_st_mtime;       /* time_t of last modification */
    time_t        wrapped_st_ctime;       /* time_t of last change */
};

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
                int nchars = sprintf(result,"%s/",p->pw_dir);
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
#define WIN32_LEAN_AND_MEAN
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
