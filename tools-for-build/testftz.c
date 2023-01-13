#include <stdint.h>
#include <math.h>

/* make sure we don't use builtin */
double (*myceil)(double x) = &ceil;

double ftz(int i) {
    union {
        uint64_t i;
        double f;
    } v;
    v.i = i;
    return myceil(v.f);
}
