#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <elf.h>

// Return the offset to a file section named 'lisp.core' if there is one
off_t search_for_elf_core(int fd)
{
    Elf64_Ehdr ehdr;

    if (lseek(fd, 0, SEEK_SET) != 0 ||
        read(fd, &ehdr, sizeof ehdr) != sizeof ehdr) {
        fprintf(stderr, "failed to read elf header\n");
        return 0;
    }
    // Seek to and read the section header of the section header string table
    Elf64_Shdr shdr;
    off_t offset = ehdr.e_shoff + ehdr.e_shstrndx * ehdr.e_shentsize;
    if (lseek(fd, offset, SEEK_SET) != offset ||
        read(fd, &shdr, sizeof shdr) != sizeof shdr) {
        fprintf(stderr, "read failed\n");
        return 0;
    }
    // Scan the section header string table
    unsigned long result = 0;
    char * shstrtab_strbuf = 0;
    int i;
    if ((shstrtab_strbuf = malloc(shdr.sh_size)) == NULL ||
        lseek(fd, shdr.sh_offset, SEEK_SET) != (Elf64_Sxword)shdr.sh_offset ||
        read(fd, shstrtab_strbuf, shdr.sh_size) != (Elf64_Sxword)shdr.sh_size ||
        lseek(fd, ehdr.e_shoff, SEEK_SET) != (Elf64_Sxword)ehdr.e_shoff)
        fprintf(stderr, "read failed\n");
    else for(i=0;i<ehdr.e_shnum;++i)
        if (read(fd, &shdr, sizeof shdr) == sizeof shdr
            && !strcmp(&shstrtab_strbuf[shdr.sh_name], "lisp.core")) {
            result = shdr.sh_offset;
            break;
        }
    if (shstrtab_strbuf)
        free(shstrtab_strbuf);
    return result;
}
