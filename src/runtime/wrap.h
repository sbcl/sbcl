/*
 * Data structures used in wrap.c in this directory, moved here from
 * wrap.c in November 2007 so that
 * src/tools-for-build/grovel-headers.c can grovel the sizes of
 * things.
 */
#ifndef _SBCL_WRAP_H_
#define _SBCL_WRAP_H_

/* As of 0.6.12, the FFI can't handle 64-bit values. For now, we use
 * these munged-to-32-bits values for might-be-64-bit slots of
 * stat_wrapper as a workaround, so that at least we can still work
 * when values are small.
 *
 * FIXME: But of course we should fix the FFI so that we can use the
 * actual 64-bit values instead.  In fact, we probably have by now
 * (2003-10-03) on all working platforms except MIPS; if some
 * motivated spark would simply fix those, this hack could go away.
 * -- CSR, 2003-10-03
 *
 * Some motivated spark fixed MIPS. -- ths, 2005-10-06 */

/* It would seem as though the FFI would have to be able to handle
 * 64-bit values in order for the LARGEFILE && !MIPS case below to
 * work, so can the comment above still be right? If FFI can only
 * handle 64-bit aliens on some platforms, maybe there should be a
 * distinct Lisp feature for 64-bit aliens support? -- RMK,
 * 2007-11-14
 *
 * In any case, since the types defined here exist to give sizes to
 * potentially munged or faked data in our stat wrapper, these
 * shouldn't be used for any purpose for which the real type can be
 * employed. */

#include "sbcl.h"

/* We use an extra layer of aliasing because Linux/MIPS struct stat
   doesn't use dev_t. This type is not defined on the Lisp side. */
#ifdef LISP_FEATURE_MIPS
typedef unsigned long aliased_dev_t;
#else
typedef dev_t aliased_dev_t;
#endif

#ifdef LISP_FEATURE_ANDROID
typedef unsigned long long wst_ino_t;
typedef long long wst_off_t;
typedef unsigned long long wst_dev_t;
#elif defined(LISP_FEATURE_LARGEFILE) || defined(LISP_FEATURE_DARWIN)
typedef ino_t wst_ino_t;
typedef aliased_dev_t wst_dev_t;
typedef off_t wst_off_t;
#else
/* These wrappers shouldn't exist, and since pulling in runtime.h caused
 * problems on Win32, we don't use the u32 typedef. */
typedef ino_t wst_ino_t;
typedef unsigned int wst_dev_t; /* since Linux dev_t can be 64 bits */
typedef unsigned int wst_off_t; /* since OpenBSD 2.8 st_size is 64 bits */
#endif

#ifdef LISP_FEATURE_OS_PROVIDES_BLKSIZE_T
typedef blksize_t wst_blksize_t;
typedef blkcnt_t wst_blkcnt_t;
#elif defined(LISP_FEATURE_ANDROID)
typedef unsigned long wst_blksize_t;
typedef unsigned long long wst_blkcnt_t;
#else
typedef unsigned long wst_blksize_t;
typedef unsigned long wst_blkcnt_t;
#endif

#ifdef LISP_FEATURE_WIN32 /* Win32 lacks nlink_t, st_uid, st_gid.*/
typedef short wst_nlink_t;
typedef short wst_uid_t;
typedef short wst_gid_t;
#else
typedef nlink_t wst_nlink_t;
typedef uid_t wst_uid_t;
typedef gid_t wst_gid_t;
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
    wst_dev_t     wrapped_st_dev;         /* device */
    wst_ino_t     wrapped_st_ino;         /* inode */
    mode_t        wrapped_st_mode;        /* protection */
    wst_nlink_t   wrapped_st_nlink;       /* number of hard links */
    wst_uid_t     wrapped_st_uid;         /* user ID of owner */
    wst_gid_t     wrapped_st_gid;         /* group ID of owner */
    wst_dev_t     wrapped_st_rdev;        /* device type (if inode device) */
    wst_off_t     wrapped_st_size;        /* total size, in bytes */
    wst_blksize_t wrapped_st_blksize;     /* blocksize for filesystem I/O */
    wst_blkcnt_t  wrapped_st_blocks;      /* number of blocks allocated */
    time_t        wrapped_st_atime;       /* time_t of last access */
    time_t        wrapped_st_mtime;       /* time_t of last modification */
    time_t        wrapped_st_ctime;       /* time_t of last change */
};

#endif /* _SBCL_WRAP_H_ */
