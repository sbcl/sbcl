#ifndef _GC_TYPEDEFS_H_
#define _GC_TYPEDEFS_H_
/* These are for grovel-headers because depending on how pedantic your system linker is,
 * it might be an error to try to include gc.h and then not link in any file that
 * provides the inline functions, even if they're never used. */
typedef int page_index_t;
typedef signed char generation_index_t;
#endif
