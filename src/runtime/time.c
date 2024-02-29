/*
 * time support routines that are easier to do in C than in Lisp
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

#include <stdio.h>
#include <time.h>
#include "genesis/sbcl.h"
#include "runtime.h"

int get_timezone(time_t when, unsigned int *dst)
{
    struct tm ltm;

#ifdef LISP_FEATURE_WIN32
    /* No _r versions on Windows, but the API documentation also
     * doesn't warn them about being non-reentrant... So here's
     * hoping they actually are.
     *
     * The Windows versions also don't support times before the
     * epoch, so we kludge it. */
    if (when < 0)
        when = 0;
    ltm = *localtime(&when);
    struct tm gtm = *gmtime(&when);
#else
    localtime_r(&when, &ltm);
#endif
    *dst = ltm.tm_isdst;

#if defined(LISP_FEATURE_LINUX) || defined(LISP_FEATURE_BSD)
    return -(int)ltm.tm_gmtoff;
#else
#ifndef LISP_FEATURE_WIN32
    struct tm gtm;
    gmtime_r(&when, &gtm);
#endif
    int sw = (((gtm.tm_hour*60)+gtm.tm_min)*60+gtm.tm_sec) - (((ltm.tm_hour*60)+ltm.tm_min)*60+ltm.tm_sec);
    if ((gtm.tm_wday + 1) % 7 == ltm.tm_wday)
        sw -= 24*3600;
    else if (gtm.tm_wday == (ltm.tm_wday + 1) % 7)
        sw += 24*3600;
    return sw;
#endif

}
