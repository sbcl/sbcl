/*
 * wrappers around low-level operations to provide a simpler interface
 * to the operations that Lisp needs
 *
 * The functions in this file are typically called directly from Lisp.
 * Thus, when their signature changes, they don't need updates in a .h
 * file somewhere, but they do need updates in the Lisp code. FIXME:
 * It would be nice to enforce this at compile time. It mighn't even
 * be all that hard: make the cross-compiler versions of DEF-ALIEN-FOO
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

#include <sys/types.h>
#include <dirent.h>
#include <sys/stat.h>
#include <string.h>
#include <unistd.h>

#include "runtime.h"
#include "sbcl.h"
#include "util.h"
   
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
    DIR *dir_ptr;
    char **result = 0;

    if (dir_ptr = opendir(directory_name)) { /* if opendir success */

	struct voidacc va;

	if (0 == voidacc_ctor(&va)) { /* if voidacc_ctor success */
	    struct dirent *dirent_ptr;

	    while (dirent_ptr = readdir(dir_ptr)) { /* until end of data */
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
 * stat(2) stuff
 */

/* As of 0.6.12, the FFI can't handle 64-bit values. For now, we use
 * these munged-to-32-bits values for might-be-64-bit slots of
 * stat_wrapper as a workaround, so that at least we can still work
 * when values are small.
 *
 * FIXME: But of course we should fix the FFI so that we can use the
 * actual 64-bit values instead. */
typedef long ffi_dev_t; /* since Linux dev_t can be 64 bits */
typedef u32 ffi_off_t; /* since OpenBSD 2.8 st_size is 64 bits */

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
    nlink_t       wrapped_st_nlink;       /* number of hard links */
    uid_t         wrapped_st_uid;         /* user ID of owner */
    gid_t         wrapped_st_gid;         /* group ID of owner */
    ffi_dev_t     wrapped_st_rdev;        /* device type (if inode device) */
    ffi_off_t     wrapped_st_size;        /* total size, in bytes */
    unsigned long wrapped_st_blksize;     /* blocksize for filesystem I/O */
    unsigned long wrapped_st_blocks;      /* number of blocks allocated */
    time_t        wrapped_st_atime;       /* time_t of last access */
    time_t        wrapped_st_mtime;       /* time_t of last modification */
    time_t        wrapped_st_ctime;       /* time_t of last change */
};

static void 
copy_to_stat_wrapper(struct stat_wrapper *to, struct stat *from)
{
#define FROB(stem) to->wrapped_st_##stem = from->st_##stem
    FROB(dev);
    FROB(ino);
    FROB(mode);
    FROB(nlink);
    FROB(uid);
    FROB(gid);
    FROB(rdev);
    FROB(size);
    FROB(blksize);
    FROB(blocks);
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
    if ((ret = stat(file_name,&real_buf)) >= 0)
	copy_to_stat_wrapper(buf, &real_buf); 
    return ret;
}

int
lstat_wrapper(const char *file_name, struct stat_wrapper *buf)
{
    struct stat real_buf;
    int ret;
    if ((ret = lstat(file_name,&real_buf)) >= 0) 
	copy_to_stat_wrapper(buf, &real_buf); 
    return ret;
}

int
fstat_wrapper(int filedes, struct stat_wrapper *buf)
{
    struct stat real_buf;
    int ret;
    if ((ret = fstat(filedes,&real_buf)) >= 0)
	copy_to_stat_wrapper(buf, &real_buf); 
    return ret;
}
