#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <elf.h>
#include <assert.h>

static off_t lisp_rel_section_offset;
static ssize_t lisp_rel_section_size;

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
    // Scan the section header string table to locate both the 'lisp.core' and
    // 'lisp.rel' sections. There might not be a lisp.rel section, but don't stop
    // looking after seeing just one. The order is unspecified.
    int i;
    for(i=1;i<ehdr.e_shnum;++i) { // skip section 0 which is the null section
        Elf64_Shdr* h = (Elf64_Shdr*)(shdrs + ehdr.e_shentsize * i);
        if  (!strcmp(&shstrtab_strbuf[h->sh_name], "lisp.core")) {
            assert(!result); // there can be only 1
            result = h->sh_offset;
            if (lisp_rel_section_offset) break; // stop when both sections seen
        } else if (!strcmp(&shstrtab_strbuf[h->sh_name], "lisp.rel")) {
            assert(!lisp_rel_section_offset); // there can be only 1
            lisp_rel_section_offset = h->sh_offset;
            lisp_rel_section_size = h->sh_size;
            if (result) break; // stop when both sections seen
        }
    }
done:
    if (shstrtab_strbuf) free(shstrtab_strbuf);
    if (shdrs) free(shdrs);
    return result;
}

int apply_pie_relocs(long code_space_translation,
                     long dynamic_space_translation,
                     int fd)
{
    // If dynamic space was relocated, let coreparse fix everything by itself.
    // The entire heap must be walked anyway to fix intra-dynamic-space pointers.
    if (dynamic_space_translation != 0 || lisp_rel_section_size == 0)
        return 0;
    // Otherwise, we're going to make it appear that code space was supposed
    // to have been mapped where it actually was.
    int n_relocs = lisp_rel_section_size / sizeof (long);
    unsigned long **ptrs = malloc(n_relocs * sizeof (long));
    if (!ptrs) return 0;
    int success = 0;
    if (lseek(fd, lisp_rel_section_offset, SEEK_SET) == lisp_rel_section_offset &&
        read(fd, ptrs, lisp_rel_section_size) == lisp_rel_section_size) {
        int i;
        // element 0 of the array is not used
        for (i = 1; i<n_relocs; ++i) {
            unsigned long *vaddr = ptrs[i];
            *vaddr += code_space_translation;
        }
        success = 1;
    }
    free(ptrs);
    return success;
}
