/*
 * stat-macros.c
 *
 * Inspired mostly by section 4.3 and 4.21 of APUE
 *
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

int s_isreg(mode_t mode)
{
    return S_ISREG(mode);
}


int s_isdir(mode_t mode)
{
    return S_ISDIR(mode);
}


int s_ischr(mode_t mode)
{
    return S_ISCHR(mode);
}


int s_isblk(mode_t mode)
{
    return S_ISBLK(mode);
}


int s_isfifo(mode_t mode)
{
    return S_ISFIFO(mode);
}


int s_islnk(mode_t mode)
{
#ifdef S_ISLNK
    return S_ISLNK(mode);
#else
    return ((mode & S_IFMT) == S_IFLNK);
#endif
}


int s_issock(mode_t mode)
{
#ifdef S_ISSOCK
    return S_ISSOCK(mode);
#else
    return ((mode & S_IFMT) == S_IFSOCK);
#endif
}


