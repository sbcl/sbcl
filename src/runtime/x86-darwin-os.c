
#include <architecture/i386/table.h>
#include <i386/user_ldt.h>

void set_data_desc_size(data_desc_t* desc, unsigned long size) {
    desc->limit00 = (size - 1) & 0xffff;
    desc->limit16 = ((size - 1) >> 16) &0xf;
}

void set_data_desc_addr(data_desc_t* desc, void* addr) {
    desc->base00 = (unsigned int)addr & 0xffff;
    desc->base16 = ((unsigned int)addr & 0xff0000) >> 16;
    desc->base24 = ((unsigned int)addr & 0xff000000) >> 24;
}

