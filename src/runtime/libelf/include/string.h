#include_next <string.h>

#ifndef _STRING_H_
#define _STRING_H_
size_t strlcat(char * __restrict, const char * __restrict, size_t);
size_t strlcpy(char * __restrict, const char * __restrict, size_t);
void strmode(int, char *);
char *strnstr(const char *big, const char *little, size_t len);
#endif
