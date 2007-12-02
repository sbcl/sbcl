#ifndef _PPC_DARWIN_OS_H
#define _PPC_DARWIN_OS_H

typedef unsigned int os_context_register_t;

static inline os_context_t *arch_os_get_context(void **void_context)
{
    return (os_context_t *) *void_context;
}
/* As of XCode 3.0, the field names for the thread state have changed
 * and now are prepended with __. Use some #define hackery to deal
 * with this. MAC_OS_X_VERSION_10_5 seems to be a good test to see if
 * we need the new style field names.
 */
#if MAC_OS_X_VERSION_10_5

#define PPC_DARWIN_REGIFY(foo) __ ## foo

typedef ppc_thread_state_t ppc_ss_struct_t;

#else

#define PPC_DARWIN_REGIFY(foo) foo

typedef ppc_saved_state_t ppc_ss_struct_t;

#endif /* MAC_OS_X_VERSION_10_5 */

#endif /* _PPC_DARWIN_OS_H */
