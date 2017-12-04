#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <elf.h>

// Return the offset to a file section named 'lisp.core' if there is one
size_t search_for_elf_core(int fd, off_t* core_start)
{
    Elf64_Ehdr ehdr;
    unsigned long result = 0;

    if (lseek(fd, 0, SEEK_SET) != 0 ||
        read(fd, &ehdr, sizeof ehdr) != sizeof ehdr) {
        fprintf(stderr, "failed to read elf header\n");
        return 0;
    }
    // Seek to and read the section header of the section header string table
    Elf64_Shdr shdr;
    off_t offset = ehdr.e_shoff + ehdr.e_shstrndx * ehdr.e_shentsize;
    char * shstrtab_strbuf = 0;
    if (lseek(fd, offset, SEEK_SET) != offset ||
        read(fd, &shdr, sizeof shdr) != sizeof shdr) {
        fprintf(stderr, "read failed\n");
        return 1;
    }
    // Read the section header string table
    if ((shstrtab_strbuf = malloc(shdr.sh_size)) == NULL ||
        lseek(fd, shdr.sh_offset, SEEK_SET) != (Elf64_Sxword)shdr.sh_offset ||
        read(fd, shstrtab_strbuf, shdr.sh_size) != (Elf64_Sxword)shdr.sh_size) {
        fprintf(stderr, "read failed\n");
        goto done;
    }
    // Seek back to the section headers and scan linearly
    if (lseek(fd, ehdr.e_shoff, SEEK_SET) != (Elf64_Sxword)ehdr.e_shoff) {
        fprintf(stderr, "lseek failed\n");
        goto done;
    }
    int i;
    for(i=0;i<ehdr.e_shnum;++i) {
        if (read(fd, &shdr, sizeof shdr) != sizeof shdr)
            break;
        if (!strcmp(&shstrtab_strbuf[shdr.sh_name], "lisp.core")) {
            *core_start = shdr.sh_offset;
            result = shdr.sh_size;
            break;
        }
    }
    done:
    if (shstrtab_strbuf)
        free(shstrtab_strbuf);
    return result;
}
