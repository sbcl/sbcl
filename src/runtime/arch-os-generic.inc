/* -*- Mode: C -*- */

static inline os_context_t *arch_os_get_context(void **void_context)
{
    return (os_context_t *) *void_context;
}
