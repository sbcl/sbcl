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

/*
 * $Header$
 */

#include <stdio.h>
#include <time.h>
#include "runtime.h"

void get_timezone(time_t when, int *minwest, boolean *dst)
{
    struct tm ltm, gtm;
    int mw;

    ltm = *localtime(&when);
    gtm = *gmtime(&when);

    mw = ((gtm.tm_hour*60)+gtm.tm_min) - ((ltm.tm_hour*60)+ltm.tm_min);
    if ((gtm.tm_wday + 1) % 7 == ltm.tm_wday)
	mw -= 24*60;
    else if (gtm.tm_wday == (ltm.tm_wday + 1) % 7)
	mw += 24*60;
    *minwest = mw;
    *dst = ltm.tm_isdst;
}
