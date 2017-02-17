/*
* Create an ELF object .
*/
#include <libelf.h>
#include <gelf.h>

typedef struct sbcl_buffer {
  char *data;
  size_t allocated_len;
  size_t size;
} sbcl_buffer;

typedef struct sbcl_elf_symtab {
  GElf_Sym *data;
  size_t data_size;
  size_t cur_offset;
  size_t num_local_entries; // Number of initial STT_LOCAL symtab entries.
  void *mangled_data;
} sbcl_elf_symtab;

typedef struct sbcl_elf {
  Elf* elf;
  sbcl_buffer shstrtab;
  sbcl_buffer strtab;
  sbcl_elf_symtab symtab;
} sbcl_elf;

typedef struct sbcl_elf_gc_area {
    char *name;
    int flags;
    char *zero_name;
    int zero_flags;
    uintptr_t start;
    uintptr_t free;
    uintptr_t end;
} sbcl_elf_gc_area;


void sbcl_buffer_init(sbcl_buffer *buf);

void sbcl_buffer_destroy(sbcl_buffer *buf);

size_t sbcl_buffer_add(sbcl_buffer *buf, void *data, size_t data_size);

size_t sbcl_elf_add_strtab_entry(sbcl_buffer *strtab, char *str, size_t len);

void sbcl_elf_strtab_init(sbcl_buffer *strtab);

void sbcl_elf_strtab_destroy(sbcl_buffer *strtab);

size_t sbcl_elf_add_strtab_entry(sbcl_buffer *strtab, char *str, size_t len);

void sbcl_elf_init_symtab(sbcl_elf_symtab *symtab, sbcl_buffer *strtab);

void sbcl_elf_destroy_symtab(sbcl_elf_symtab *symtab);

size_t sbcl_elf_add_symtab_entry2(
    sbcl_elf_symtab *symtab, sbcl_buffer *strtab,
    char *name, unsigned char bind, unsigned char type,
    Elf64_Section shndx, GElf_Addr value, GElf_Xword size);

size_t sbcl_elf_add_symtab_entry(
    sbcl_elf *e, char *name, unsigned char bind, unsigned char type,
    Elf64_Section shndx, GElf_Addr value, GElf_Xword size);

void sbcl_elf_open(sbcl_elf *e, int fd);

void sbcl_elf_close(sbcl_elf *e);

size_t sbcl_elf_output_space(sbcl_elf* e, char* name, void* data, size_t size, int flags);

/* Output a string table section, with the given section_name. Returns
 * the index of the section created. */
size_t sbcl_elf_output_strtab(sbcl_elf *e, char *section_name, sbcl_buffer *strtab);

size_t sbcl_elf_output_symtab(sbcl_elf *e);

void sbcl_elf_align_gc_areas(sbcl_elf_gc_area *areas, size_t size);

void sbcl_elf_output_gc_areas(sbcl_elf *e, sbcl_elf_gc_area *areas, size_t size);
