/**
 * xmalloc.c - malloc with error checking
 * @copyright (c) 2007-2015, Tohoku University.
 * @author UENO Katsuhiro
 */

#include "smlsharp.h"
#include <stdlib.h>

void *
sml_xmalloc(size_t size)
{
        void *p = malloc(size);
        if (p == NULL)
                sml_sysfatal("malloc");
        return p;
}

void *
sml_xrealloc(void *p, size_t size)
{
        p = realloc(p, size);
        if (p == NULL)
                sml_sysfatal("realloc");
        return p;
}
