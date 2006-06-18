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

void get_timezone(time_t when, int *secwest, boolean *dst)
{
    struct tm ltm, gtm;
    int sw;

    ltm = *localtime_r(&when, &ltm);
    gtm = *gmtime_r(&when, &gtm);

    sw = (((gtm.tm_hour*60)+gtm.tm_min)*60+gtm.tm_sec) - (((ltm.tm_hour*60)+ltm.tm_min)*60+ltm.tm_sec);
    if ((gtm.tm_wday + 1) % 7 == ltm.tm_wday)
        sw -= 24*3600;
    else if (gtm.tm_wday == (ltm.tm_wday + 1) % 7)
        sw += 24*3600;
    *secwest = sw;
    *dst = ltm.tm_isdst;
}
