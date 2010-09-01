/* test to build and run so that we know if we have getprotobyname_r
 * and getprotobynumber_r */

#include <netdb.h>

#define BUFSIZE 1024

int main ()
{
    struct protoent result_buf;
    struct protoent *result;
    char buf[BUFSIZE];
    getprotobyname_r("", &result_buf, buf, BUFSIZE, &result);
    getprotobynumber_r("", &result_buf, buf, BUFSIZE, &result);
    return 104;
}
