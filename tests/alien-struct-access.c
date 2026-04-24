/*
 * Harness for detecting over-reads and writes in struct-by-value
 * argument handling.
 */

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

/*
 * Map two consecutive pages, make the second PROT_NONE, and return a
 * pointer so that [return ..  return+size) lies at the end of the
 * first page.
 */

static long pagesize = 0;

void *guarded_alloc(size_t size) {
    if (!pagesize)
        pagesize = sysconf(_SC_PAGESIZE);
    void *base = mmap(NULL, 2 * pagesize, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (base == MAP_FAILED) return NULL;
    if (mprotect((char *)base + pagesize, pagesize, PROT_NONE) != 0) {
        munmap(base, 2 * pagesize);
        return NULL;
    }
    memset(base, 0, pagesize);
    return (char *)base + pagesize - size;
}

void guarded_free(void *p, size_t size) {
    if (!p) return;
    munmap((char *)p - (pagesize - size), 2 * pagesize);
}

/*
 * Shape fixtures for test cases
 */

#define IDENTITY(NAME) \
  struct NAME NAME##_identity(struct NAME s) { return s; }

struct gp_i8    { int8_t  m0;      };
struct gp_i16   { int16_t m0;      };
struct gp_i32   { int32_t m0;      };
struct gp_1f    { float   m0;      };
struct gp_i8x7  { int8_t  m[7];    };
struct gp_i8x9  { int8_t  m[9];    };
struct gp_i8x15 { int8_t  m[15];   };
struct gp_3f    { float   a, b, c; };

int64_t gp_i8_sum  (struct gp_i8  s) { return s.m0;         }
int64_t gp_i16_sum (struct gp_i16 s) { return s.m0;         }
int64_t gp_i32_sum (struct gp_i32 s) { return s.m0;         }
double  gp_1f_sum  (struct gp_1f  s) { return (double)s.m0; }

int64_t gp_i8x7_sum (struct gp_i8x7 s) {
    long long t = 0; for (int i = 0; i < 7;  i++) t += s.m[i]; return t;
}
int64_t gp_i8x9_sum (struct gp_i8x9 s) {
    long long t = 0; for (int i = 0; i < 9;  i++) t += s.m[i]; return t;
}
int64_t gp_i8x15_sum (struct gp_i8x15 s) {
    long long t = 0; for (int i = 0; i < 15; i++) t += s.m[i]; return t;
}
double gp_3f_sum (struct gp_3f s) {
    return (double)s.a + (double)s.b + (double)s.c;
}

IDENTITY(gp_i8)   IDENTITY(gp_i16)  IDENTITY(gp_i32) IDENTITY(gp_1f)
IDENTITY(gp_i8x7) IDENTITY(gp_i8x9) IDENTITY(gp_i8x15) IDENTITY(gp_3f)
