/*
 * stat-macros.c
 *
 * Inspired mostly by section 4.3 and 4.21 of APUE
 *
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

mode_t s_isreg(mode_t mode)
{
    return S_ISREG(mode);
}


mode_t s_isdir(mode_t mode)
{
    return S_ISDIR(mode);
}


mode_t s_ischr(mode_t mode)
{
    return S_ISCHR(mode);
}


mode_t s_isblk(mode_t mode)
{
    return S_ISBLK(mode);
}


mode_t s_isfifo(mode_t mode)
{
    return S_ISFIFO(mode);
}


mode_t s_islnk(mode_t mode)
{
#ifdef S_ISLNK
    return S_ISLNK(mode);
#else
    return ((mode & S_IFMT) == S_IFLNK);
#endif
}


mode_t s_issock(mode_t mode)
{
#ifdef S_ISSOCK
    return S_ISSOCK(mode);
#else
    return ((mode & S_IFMT) == S_IFSOCK);
#endif
}


