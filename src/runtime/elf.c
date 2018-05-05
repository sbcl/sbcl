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
    unsigned long result = 0;
    char* shdrs = 0;
    char * shstrtab_strbuf = 0;

    // Slurp in all the section headers
    int nbytes = ehdr.e_shentsize * ehdr.e_shnum;
    if ((shdrs = malloc(nbytes)) == NULL ||
        lseek(fd, ehdr.e_shoff, SEEK_SET) != (Elf64_Sxword)ehdr.e_shoff ||
        read(fd, shdrs, nbytes) != nbytes)
        goto done;
    Elf64_Shdr* shdr = (Elf64_Shdr*)(shdrs + ehdr.e_shentsize * ehdr.e_shstrndx);
    // Slurp the string table
    if ((shstrtab_strbuf = malloc(shdr->sh_size)) == NULL ||
        lseek(fd, shdr->sh_offset, SEEK_SET) != (Elf64_Sxword)shdr->sh_offset ||
        read(fd, shstrtab_strbuf, shdr->sh_size) != (Elf64_Sxword)shdr->sh_size)
        goto done;
    // Scan the section header string table
    int i;
    for(i=1;i<ehdr.e_shnum;++i) { // skip section 0 which is the null section
        Elf64_Shdr* h = (Elf64_Shdr*)(shdrs + ehdr.e_shentsize * i);
        if  (!strcmp(&shstrtab_strbuf[h->sh_name], "lisp.core")) {
            result = h->sh_offset;
            break;
        }
    }
done:
    if (shstrtab_strbuf) free(shstrtab_strbuf);
    if (shdrs) free(shdrs);
    return result;
}
