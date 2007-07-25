#ifndef _X86_DARWIN_OS_H
#define _X86_DARWIN_OS_H

#include <architecture/i386/table.h>
#include <i386/user_ldt.h>

#include "darwin-os.h"

typedef int os_context_register_t;

static inline os_context_t *arch_os_get_context(void **void_context)
{
    return (os_context_t *) *void_context;
}

void set_data_desc_size(data_desc_t* desc, unsigned long size);
void set_data_desc_addr(data_desc_t* desc, void* addr);

#define CONTEXT_ADDR_FROM_STEM(stem) &context->uc_mcontext->ss.stem
#define DARWIN_FIX_CONTEXT(context)

#endif /* _X86_DARWIN_OS_H */
