/**
 * error.c
 * @copyright (c) 2007, Tohoku University.
 * @author UENO Katsuhiro
 */

#include "smlsharp.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <errno.h>

enum sml_msg_level {
        MSG_FATAL,
        MSG_ERROR,
        MSG_WARN,
        MSG_NOTICE,
        MSG_DEBUG
};

#ifndef NDEBUG
#define DEFAULT_VERBOSE_LEVEL  MSG_DEBUG
#else
#define DEFAULT_VERBOSE_LEVEL  MSG_NOTICE
#endif /* NDEBUG */

static enum sml_msg_level verbose_level = DEFAULT_VERBOSE_LEVEL;
static FILE *logfile;
#ifndef WITHOUT_MULTITHREAD
static pthread_mutex_t msg_lock = PTHREAD_MUTEX_INITIALIZER;
#endif /* !WITHOUT_MULTITHREAD */

static FILE *
output()
{
        return logfile ? logfile : stderr;
}

static void
print_syserror(enum sml_msg_level level, int err,
               const char *format, va_list args)
{
        FILE *out;

        if (verbose_level < level)
                return;

        mutex_lock(&msg_lock);
        out = output();
        vfprintf(out, format, args);

        if (err > 0)
                fprintf(out, "%s\n", strerror(err));
        else if (err == 0)
                fprintf(out, ": Success\n");
        else
                fprintf(out, ": Failed (%d)\n", err);
        mutex_unlock(&msg_lock);
}

static void
print_error(enum sml_msg_level level, int err,
            const char *format, va_list args)
{
        FILE *out;

        if (verbose_level < level)
                return;

        if (err != 0) {
                print_syserror(level, err, format, args);
                return;
        }

        mutex_lock(&msg_lock);
        out = output();
        vfprintf(out, format, args);
        fputs("\n", out);
        mutex_unlock(&msg_lock);
}

void
sml_fatal(int err, const char *format, ...)
{
        va_list args;
        va_start(args, format);
        print_error(MSG_FATAL, err, format, args);
        va_end(args);
        abort();
}

void
sml_error(int err, const char *format, ...)
{
        va_list args;
        va_start(args, format);
        print_error(MSG_ERROR, err, format, args);
        va_end(args);
}

void
sml_warn(int err, const char *format, ...)
{
        va_list args;
        va_start(args, format);
        print_error(MSG_WARN, err, format, args);
        va_end(args);
}

void
sml_notice(const char *format, ...)
{
        va_list args;
        va_start(args, format);
        print_error(MSG_NOTICE, 0, format, args);
        va_end(args);
}

void
sml_sysfatal(const char *format, ...)
{
        va_list args;
        va_start(args, format);
        print_syserror(MSG_FATAL, errno, format, args);
        va_end(args);
        abort();
}

void
sml_syserror(const char *format, ...)
{
        va_list args;
        va_start(args, format);
        print_syserror(MSG_ERROR, errno, format, args);
        va_end(args);
}

void
sml_syswarn(const char *format, ...)
{
        va_list args;
        va_start(args, format);
        print_syserror(MSG_WARN, errno, format, args);
        va_end(args);
}

void
sml_debug(const char *format, ...)
{
        va_list args;
        FILE *out;

        if (verbose_level < MSG_DEBUG)
                return;

        out = output();
        va_start(args, format);
        mutex_lock(&msg_lock);
        vfprintf(out, format, args);
        mutex_unlock(&msg_lock);
        va_end(args);
}

void
sml_msg_init()
{
        char *s;
        FILE *f;

        s = getenv("SMLSHARP_VERBOSE");
        if (s)
                verbose_level = strtol(s, NULL, 10);

        s = getenv("SMLSHARP_LOGFILE");
        if (s) {
                f = fopen(s, "w");
                if (f == NULL) {
                        perror(s);
                } else {
                        setvbuf(f, NULL, _IONBF, 0);
                        logfile = f;
                }
        }
}
