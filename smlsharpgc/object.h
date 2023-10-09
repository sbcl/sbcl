/*
 * object.h - SML# heap object format
 * @copyright (c) 2007, Tohoku University.
 * @author UENO Katsuhiro
 */
#ifndef SMLSHARP__OBJECT_H__
#define SMLSHARP__OBJECT_H__

#include <limits.h>

/*
 * size of a bitmap word in heap objects and stack frames.
 */
#define SIZEOF_BITMAP     sizeof(uint32_t)
#define BITMAP_NUM_BITS   (SIZEOF_BITMAP * CHAR_BIT)

#define BITMAP_BIT(bitmaps, index) \
        (((bitmaps)[(index) / BITMAP_NUM_BITS] >> ((index) % BITMAP_NUM_BITS)) \
         & 0x1)

#define TAG_UNBOXED  0
#define TAG_BOXED    1

#define STRUCT_BITMAP(struct, field) \
        ((uint32_t)TAG_BOXED << (offsetof(struct, field) / sizeof(void*)))

/*
 * A pointer that the collector need to trace is either a null pointer or
 * one pointing to an object constructed in the following format:
 *
 *    32bit                                      32bit           32bit
 *  +-------+----------------------------------+-------+- ... -+-------+
 *  |header |             payload              | bm[0] |       |bm[N-1]|
 *  +-------+----------------------------------+-------+- ... -+-------+
 *          ^
 *          |
 *      object pointer
 *
 * The bitmap words exist only if the object is a record object.
 * Each bitmap indicates whether or not its corresponding word is a pointer;
 * i-th bit (from LSB) of n-th word corresponds to (n*32+i)-th word of the
 * payload.  If a bit in the bitmap is set, its corresponding word is a
 * pointer to be traced.
 * The bitmap consists of the minimum number of bits for covering the entire
 * payload area.
 * A record representing a closure has one additional bit in the bitmap;
 * the MSB bit of bm[0], which is not corresponding to any word in the
 * payload of closure records, is a flag indicating whether or not its
 * calling convention is rigid.
 *
 * An object header consists of a 28-bit integer SIZE, a 3-bit integer TYPE
 * and a 1-bit flag S as follows:
 *
 *  (MSB)  31 30  28                                   0  (LSB)
 *        +--+------+-----------------------------------+
 *        |S | TYPE |              SIZE                 |
 *        +--+------+-----------------------------------+
 *
 * SIZE is the size of the payload area in bytes.
 * TYPE indicates the type of the object, which is one of the following:
 *
 *   000   UNBOXED_VECTOR     no pointer,    no bitmap,  content equality
 *   001   BOXED_VECTOR       pointer array, no bitmap,  content equality
 *   010   UNBOXED_ARRAY      no pointer,    no bitmap,  identity equality
 *   011   BOXED_ARRAY        pointer array, no bitmap,  identity equality
 *   100   (unused)
 *   101   RECORD             record with bitmap words,  content equality
 *   110   INTINF             no pointer,    no bitmap,  bignum equality
 *   111   (reserved for forwarding pointers of Cheney's collectors)
 *
 * If TYPE is RECORD, SIZE must be multiple of 4 (32 bit).
 *
 * S indicates the fact that all objects reachable from the object including
 * itself are managed by a specific scheme other than GC.
 * The collector must skip the object during tracing if its S is set to 1.
 */

#define OBJ_HEADER_SIZE  sizeof(uint32_t)
#define OBJ_TYPE_MASK          0x70000000U
#define OBJ_SIZE_MASK          0x0fffffffU
#define OBJ_FLAG_SKIP          0x80000000U
#define OBJTYPE_UNBOXED_VECTOR 0x00000000U
#define OBJTYPE_BOXED_VECTOR   0x10000000U
#define OBJTYPE_UNBOXED_ARRAY  0x20000000U
#define OBJTYPE_BOXED_ARRAY    0x30000000U
#define OBJTYPE_RECORD         0x50000000U
#define OBJTYPE_INTINF         0x60000000U
#define OBJTYPE_FORWARDED      0x70000000U

#define OBJTYPE_IS_ARRAY(w)    (((w) & 0x60000000U) == 0x20000000U)

/* match with OBJTYPE_BOXED_VECTOR, OBJTYPE_BOXED_ARRAY and OBJTYPE_RECORD */
#define OBJ_FLAG_MAY_HAVE_PTR  0x10000000U

#define OBJ_DUMMY_HEADER   0U  /* valid header for dummy object */

#define OBJ_HEADER_WORD(objtype, size) \
        ((uint32_t)(objtype) | (uint32_t)(size))

#define OBJ_BEGIN(obj)   ((uint32_t*)(obj) - 1)
#define OBJ_HEADER(obj)  (*OBJ_BEGIN(obj))
#define OBJ_TYPE(obj)    (OBJ_HEADER(obj) & OBJ_TYPE_MASK)
#define OBJ_SIZE(obj)    (OBJ_HEADER(obj) & OBJ_SIZE_MASK)
#define OBJ_BITMAP(obj)  ((uint32_t*)((char*)(obj) + OBJ_SIZE(obj)))

/* OBJ_NUM_BITMAPS works only if the given object is a OBJTYPE_RECORD */
#define OBJ_BITMAPS_LEN(payload_size) \
        (((payload_size) / sizeof(void*) + BITMAP_NUM_BITS - 1) \
         / BITMAP_NUM_BITS)
#define OBJ_NUM_BITMAPS(obj) OBJ_BITMAPS_LEN(OBJ_SIZE(obj))

#define OBJ_TOTAL_SIZE(obj) \
        (OBJ_HEADER_SIZE + \
         OBJ_SIZE(obj) + (OBJ_TYPE(obj) == OBJTYPE_RECORD \
                          ? OBJ_NUM_BITMAPS(obj) * SIZEOF_BITMAP : 0))

/* A string object is a vector object terminated with a null character
 * sentinel.  OBJ_STR_SIZE returns the length of the string except for
 * the sentinel. */
#define OBJ_STR_SIZE(obj)  ((size_t)(OBJ_SIZE(obj) - 1))

#endif /* SMLSHARP__OBJECT_H__ */
