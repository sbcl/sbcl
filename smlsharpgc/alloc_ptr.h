#ifndef SMLSHARP__ALLOC_PTR_H__
#define SMLSHARP__ALLOC_PTR_H__
#include <stdint.h>
typedef uint32_t sml_bmword_t;
struct sml_bitptr { const sml_bmword_t *ptr; sml_bmword_t mask; };
struct sml_bitptrw { sml_bmword_t *wptr; sml_bmword_t mask; };
typedef struct sml_bitptr sml_bitptr_t;
typedef struct sml_bitptrw sml_bitptrw_t;
/* sizeof(struct alloc_ptr) must be power of 2 for performance */
struct alloc_ptr {
        sml_bitptr_t freebit;
        char *free;
        unsigned int blocksize_bytes;
};
#endif
