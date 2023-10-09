/*
 * timer.h - benchmark timer
 * @copyright (c) 2010, Tohoku University.
 * @author UENO Katsuhiro
 */

#ifndef SMLSHARP__TIMER_H__
#define SMLSHARP__TIMER_H__

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#ifndef HAVE_CONFIG_H
#define HAVE_GETTIMEOFDAY
#endif /* !HAVE_CONFIG_H */

#if defined HAVE_CLOCK_GETTIME

#include <time.h>
typedef struct timespec sml_timer_t;
#if 0
#define sml_timer_now(timer)  clock_gettime(CLOCK_THREAD_CPUTIME_ID, &(timer))
#define TIMERTYPE "clock_gettime CLOCK_THREAD_CPUTIME_ID"
#elif 0
#define sml_timer_now(timer)  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &(timer))
#define TIMERTYPE "clock_gettime CLOCK_PROCESS_CPUTIME_ID"
#elif HAVE_DECL_CLOCK_MONOTONIC
#define sml_timer_now(timer)  clock_gettime(CLOCK_MONOTONIC, &(timer))
#define TIMERTYPE "clock_gettime CLOCK_MONOTONIC"
#else
#define sml_timer_now(timer)  clock_gettime(CLOCK_REALTIME, &(timer))
#define TIMERTYPE "clock_gettime CLOCK_REALTIME"
#endif

struct sml_time { long sec, nsec; };
typedef struct sml_time sml_time_t;
#define TIMEINIT  { 0, 0 }
#define TIMEFMT  "%ld.%09ld"
#define TIMEARG(time)  (time).sec, (time).nsec
#define TIMEFLOAT(time)  ((time).sec + (time).nsec / 1000000000.f)

#define sml_timer_dif(timer1, timer2, time) do { \
        (time).sec = (timer2).tv_sec - (timer1).tv_sec; \
        (time).nsec = (timer2).tv_nsec - (timer1).tv_nsec; \
        if ((time).nsec < 0) (time).nsec += 1000000000, (time).sec--; \
} while (0)
#define sml_timer_accum(timer1, timer2, time) do { \
        (time).sec += (timer2).tv_sec - (timer1).tv_sec; \
        (time).nsec += (timer2).tv_nsec - (timer1).tv_nsec; \
        if ((time).nsec < 0) \
                (time).nsec += 1000000000, (time).sec--; \
        else if ((time).nsec > 1000000000) \
                (time).nsec -= 1000000000, (time).sec++; \
} while (0)
#define sml_time_accum(time1, time) do { \
        (time).sec += (time1).sec; \
        (time).nsec += (time1).nsec; \
        if ((time).nsec > 1000000000) (time).nsec -= 1000000000, (time).sec++; \
} while (0)

#elif defined HAVE_GETTIMEOFDAY

#include <sys/time.h>
typedef struct timeval sml_timer_t;
#define sml_timer_now(timer)  gettimeofday(&(timer), NULL)
#define TIMERTYPE "gettimeofday"

struct sml_time { long sec, usec; };
typedef struct sml_time sml_time_t;
#define TIMEINIT  { 0, 0 }
#define TIMEFMT  "%ld.%06ld"
#define TIMEARG(time)  (time).sec, (time).usec
#define TIMEFLOAT(time)  ((time).sec + (time).usec / 1000000.f)

#define sml_timer_dif(timer1, timer2, time) do { \
        (time).sec = (timer2).tv_sec - (timer1).tv_sec; \
        (time).usec = (timer2).tv_usec - (timer1).tv_usec; \
        if ((time).usec < 0) (time).usec += 1000000, (time).sec--; \
} while (0)
#define sml_timer_accum(timer1, timer2, time) do { \
        (time).sec += (timer2).tv_sec - (timer1).tv_sec; \
        (time).usec += (timer2).tv_usec - (timer1).tv_usec; \
        if ((time).usec < 0) (time).usec += 1000000, (time).sec--; \
        else if ((time).usec > 1000000) (time).usec -= 1000000, (time).sec++; \
} while (0)
#define sml_time_accum(time1, time) do { \
        (time).sec += (time1).sec; \
        (time).usec += (time1).usec; \
        if ((time).usec > 1000000) (time).usec -= 1000000, (time).sec++; \
} while (0)

#elif defined HAVE_GETRUSAGE

#include <sys/resource.h>
typedef struct rusage sml_timer_t;
#define sml_timer_now(timer)  getrusage(RUSAGE_SELF, &(timer))
#define TIMERTYPE "getrusage"

struct sml_time { long sec, usec; };
typedef struct sml_time sml_time_t;
#define TIMEINIT  { 0, 0 }
#define TIMEFMT  "%ld.%06ld"
#define TIMEARG(time)  (time).sec, (time).usec
#define TIMEFLOAT(time)  ((time).sec + (time).usec / 1000000.f)

#define sml_timer_dif(timer1, timer2, time) do { \
        (time).sec = (timer2).ru_utime.tv_sec - (timer1).ru_utime.tv_sec; \
        (time).usec = (timer2).ru_utime.tv_usec - (timer1).ru_utime.tv_usec; \
        if ((time).usec < 0) (time).usec += 1000000, (time).sec--; \
} while (0)
#define sml_timer_accum(timer1, timer2, time) do { \
        (time).sec += (timer2).ru_utime.tv_sec - (timer1).ru_utime.tv_sec; \
        (time).usec += (timer2).ru_utime.tv_usec - (timer1).ru_utime.tv_usec; \
        if ((time).usec < 0) (time).usec += 1000000, (time).sec--; \
        else if ((time).usec > 1000000) (time).usec -= 1000000, (time).sec++; \
} while (0)
#define sml_time_accum(time1, time) do { \
        (time).sec += (time1).sec; \
        (time).usec += (time1).usec; \
        if ((time).usec > 1000000) (time).usec -= 1000000, (time).sec++; \
} while (0)

#elif defined HAVE_TIMES

#include <sys/times.h>
#include <unistd.h>
typedef struct tms sml_timer_t;
#define sml_timer_now(timer)  times(&(timer))
#define TIMERTYPE "times"

typedef clock_t sml_time_t;
#define TIMEINIT  0
#define TIMEFMT  "%.3f"
#define TIMEARG(time)  TIMEFLOAT(time)
#define TIMEFLOAT(time)  ((double)(time) / sysconf(_SC_CLK_TCK))

#define sml_timer_dif(timer1, timer2, time) \
        ((time) = (timer2).tms_utime - (timer1).tms_utime)
#define sml_timer_accum(timer1, timer2, time) \
        ((time) += (timer2).tms_utime - (timer1).tms_utime)
#define sml_time_accum(time1, time) \
        ((time) += (time1))

#else

#include <time.h>
typedef clock_t sml_timer_t;
#define sml_timer_now(timer)  ((timer) = clock())
#define TIMERTYPE "clock"

typedef clock_t sml_time_t;
#define TIMEINIT  0
#define TIMEFMT  "%.3f"
#define TIMEARG(time)  TIMEFLOAT(time)
#define TIMEFLOAT(time)  ((double)(time) / CLOCKS_PER_SEC)

#define sml_timer_dif(timer1, timer2, time) ((time) = (timer2) - (timer1))
#define sml_timer_accum(timer1, timer2, time) ((time) += (timer2) - (timer1))
#define sml_time_accum(time1, time) ((time) += (time1))

#endif /* HAVE_CLOCK_GETTIME */

#endif /* SMLSHARP__TIMER_H__ */


#if 0
#include <stdio.h>

int fib(int n)
{
        if (n == 0) return 1;
        if (n == 1) return 1;
        return fib(n-1) + fib(n-2);
}

int main()
{
        sml_timer_t b1, b2;
        sml_time_t t1, t2, t3 = TIMEINIT, t4 = TIMEINIT;

        sml_timer_now(b1);
        fib(35);
        sml_timer_now(b2);

        sml_timer_dif(b1, b2, t1);
        sml_timer_accum(b1, b2, t3);
        sml_time_accum(t1, t4);
        printf("t1 = "TIMEFMT" sec\n", TIMEARG(t1));

        sml_timer_now(b1);
        fib(40);
        sml_timer_now(b2);

        sml_timer_dif(b1, b2, t2);
        sml_timer_accum(b1, b2, t3);
        sml_time_accum(t2, t4);
        printf("t2 = "TIMEFMT" sec\n", TIMEARG(t2));

        printf("timer_accum(t1 + t2) = "TIMEFMT" sec\n", TIMEARG(t3));
        printf("time_accum(t1 + t2) = "TIMEFMT" sec\n", TIMEARG(t4));
        printf("TIMEFLOAT(t3) = %.8f\n", TIMEFLOAT(t3));

        return 0;
}
#endif
