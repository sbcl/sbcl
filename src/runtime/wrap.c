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
#include <string.h>

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

/* Free a result returned by alloc_directory_lispy_filenames. */
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
