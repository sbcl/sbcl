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
#include "sbcl.h"
#include "runtime.h"

int get_timezone(time_t when, boolean *dst)
{
    struct tm ltm, gtm;
    int sw;

#ifdef LISP_FEATURE_WIN32
    /* No _r versions on Windows, but the API documentation also
     * doesn't warn them about being non-reentrant... So here's
     * hoping they actually are -- once Windows grows threads
     * this better be checked, though.
     *
     * The Windows versions also don't support times before the
     * epoch, so we kludge it. */
    if (when < 0)
        when = 0;
    ltm = *localtime(&when);
    gtm = *gmtime(&when);
#else
    ltm = *localtime_r(&when, &ltm);
    gtm = *gmtime_r(&when, &gtm);
#endif

    sw = (((gtm.tm_hour*60)+gtm.tm_min)*60+gtm.tm_sec) - (((ltm.tm_hour*60)+ltm.tm_min)*60+ltm.tm_sec);
    if ((gtm.tm_wday + 1) % 7 == ltm.tm_wday)
        sw -= 24*3600;
    else if (gtm.tm_wday == (ltm.tm_wday + 1) % 7)
        sw += 24*3600;
    *dst = ltm.tm_isdst;
    return sw;
}
