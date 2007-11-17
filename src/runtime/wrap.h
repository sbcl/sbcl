/*
 * Data structures used in wrap.c in this directory, moved here from
 * wrap.c in November 2007 so that
 * src/tools-for-build/grovel-headers.c can grovel the sizes and
 * offsets of things.
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
/* It would seem as though the FFI would have to be able to handle
 * 64-bit values in order for the LARGEFILE && !MIPS case below to
 * work, so can the comment above still be right? If FFI can only
 * handle 64-bit aliens on some platforms, maybe there should be a
 * distinct Lisp feature for 64-bit aliens support? -- RMK,
 * 2007-11-14 */

#include "sbcl.h"
#include "runtime.h"

#if defined(LISP_FEATURE_LARGEFILE) && !defined(LISP_FEATURE_MIPS)
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

#ifdef LISP_FEATURE_OS_PROVIDES_BLKSIZE_T
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
