/*
 * These functions emulate a small subset of the dlopen / dlsym
 * functionality under Darwin's Mach-O dyld system.
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


#include <mach-o/dyld.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ppc-darwin-dlshim.h"

/* Darwin does not define the standard ELF
 * dlopen/dlclose/dlsym/dlerror interface to shared libraries, so this
 * is an attempt at a minimal wrapper to allow SBCL to work without
 * external dependency on pogma's dlcompat library.
 */

/* For now, there is no RTLD_GLOBAL emulation either. */

static char dl_self; /* I'm going to abuse this */

static int callback_count;
static struct mach_header* last_header;

#define DLSYM_ERROR 1
#define DLOPEN_ERROR 2

static int last_error = 0;

void 
dlshim_image_callback(struct mach_header* ptr, unsigned long phooey)
{
    callback_count++;
    last_header = ptr;
}

int 
lib_path_count(void)
{
    char* libpath;
    int i;
    int count;
    libpath = getenv("DYLD_LIBRARY_PATH");
    count = 1;
    if (libpath) {
	for (i = 0; libpath[i] != '\0'; i++) {
	    if (libpath[i] == ':') count++;
	}
    }
    return count;
}

const char* 
lib_path_prefixify(int index, const char* filename)
{
    static char* retbuf = NULL;
    int fi, li, i, count;
    char* libpath;
    if (!retbuf) {
	retbuf = (char*) malloc(1024*sizeof(char));
    }
    count = 0;
    fi = 0;
    li = -1;
    libpath = getenv("DYLD_LIBRARY_PATH");
    if (libpath) {
	i = 0;
	while (count != index && libpath[i] != '\0') {
	    if (libpath[i] == ':') count++;
	    i++;
	}
	fi = i;
	while (libpath[i] != '\0' && libpath[i] != ':') {
	    i++;
	}
	li = i - 1;
    }
    if (li - fi > 0) {
	if (li - fi + 1 > 1022 - strlen(filename)) {
	    retbuf = 
		(char*) realloc(retbuf, (li - fi + 3 + strlen(filename))*sizeof(char));
	}
	memcpy(retbuf, libpath + fi, (li - fi + 1)*sizeof(char));
	retbuf[li - fi + 1] = '/';
	memcpy(retbuf + li - fi + 2, filename, strlen(filename) + 1);
	return retbuf;
    } else {
	return filename;
    }
}

const void* 
dlopen(const char* filename, int flags)
{
    static char has_callback = 0;
    if (!has_callback) {
	_dyld_register_func_for_add_image(dlshim_image_callback);
    }
    if (!filename) {
	return &dl_self;
    } else {
	const struct mach_header* img = NULL;
	if (!img) 
	    img = NSAddImage(filename, NSADDIMAGE_OPTION_RETURN_ON_ERROR);
	if (!img) 
	    img = NSAddImage(filename, 
			     NSADDIMAGE_OPTION_RETURN_ON_ERROR | 
			     NSADDIMAGE_OPTION_WITH_SEARCHING);
	if (!img) {
	    NSObjectFileImage fileImage;
	    callback_count = 0;
	    last_header = NULL;
	    if (NSCreateObjectFileImageFromFile(filename, &fileImage) 
		== NSObjectFileImageSuccess) {
		NSLinkModule(fileImage, filename, 
			     NSLINKMODULE_OPTION_BINDNOW | 
			     ((flags & RTLD_GLOBAL)?NSLINKMODULE_OPTION_PRIVATE:0) | 
			     NSLINKMODULE_OPTION_RETURN_ON_ERROR);
		if (callback_count && last_header) 
		    img = last_header;
	    }
	}
	if (!img) {
	    NSObjectFileImage fileImage;
	    int i, maxi;
	    const char* prefixfilename;
	    maxi = lib_path_count();
	    for (i = 0; i < maxi && !img; i++) {
		prefixfilename = lib_path_prefixify(i, filename);
		callback_count = 0;
		last_header = NULL;
		if (NSCreateObjectFileImageFromFile(prefixfilename, &fileImage) 
		    == NSObjectFileImageSuccess) {
		    NSLinkModule(fileImage, filename, 
				 NSLINKMODULE_OPTION_BINDNOW | 
				 ((flags & RTLD_GLOBAL)?NSLINKMODULE_OPTION_PRIVATE:0) | 
				 NSLINKMODULE_OPTION_RETURN_ON_ERROR);
		    if (callback_count && last_header) 
			img = last_header;
		}
	    }
	}
	if (img) {
	    if (flags & RTLD_NOW) {
		NSLookupSymbolInImage(img, "", 
				      NSLOOKUPSYMBOLINIMAGE_OPTION_BIND_FULLY | 
				      NSLOOKUPSYMBOLINIMAGE_OPTION_RETURN_ON_ERROR);
	    }
	    if (NSIsSymbolNameDefinedInImage(img, "__init")) {
		NSSymbol* initsymbol;
		void (*initfunc) (void);
		initsymbol = NSLookupSymbolInImage(img, "__init", 0);
		initfunc = NSAddressOfSymbol(initsymbol);
		initfunc();
	    }
	} else
	    last_error = DLOPEN_ERROR;
	return img;
    }
}

const char* 
dlerror()
{
    NSLinkEditErrors c;
    int errorNumber;
    const char *fileName, *errorString;
    char *result = NULL;

    if (last_error) {
	NSLinkEditError(&c, &errorNumber, &fileName, &errorString);
	/* The errorString obtained by the above is too verbose for
	 * our needs, so we just translate the errno. 
	 *
	 * We also have simple fallbacks in case we've somehow lost
	 * the context before this point. */
	if (errorNumber) {
	    result = strerror(errorNumber);
	} else if (DLSYM_ERROR == last_error) {
	    result = "dlsym(3) failed";
	} else if (DLOPEN_ERROR == last_error) {
	    result = "dlopen(3) failed";
	}
	last_error = 0;
    }
	
    return result;
}

void* 
dlsym(void* handle, char* symbol)
{
    if (handle == &dl_self) {
	if (NSIsSymbolNameDefined(symbol)) {
	    NSSymbol* retsym;
	    retsym = NSLookupAndBindSymbol(symbol);
	    return NSAddressOfSymbol(retsym);
	} else {
            last_error = DLSYM_ERROR;
	    return NULL;
	}
    } else {
	if (NSIsSymbolNameDefinedInImage(handle, symbol)) {
	    NSSymbol* retsym;
	    retsym = NSLookupSymbolInImage(handle, symbol, 0);
	    return NSAddressOfSymbol(retsym);
	} else {
            last_error = DLSYM_ERROR;
	    return NULL;
	}
    }
}

int 
dlclose(void *handle)
{
    /* dlclose is not implemented, and never will be for dylibs.
     * return -1 to signal an error; it's not used by SBCL anyhow */
    return -1;
}
