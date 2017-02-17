/*
 * Create an ELF object.
 */

#include "sbcl.h"
#if defined(LISP_FEATURE_SB_ELF_CORE)

#include "sbcl_elf.h"

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <err.h>

void
sbcl_buffer_init(sbcl_buffer *buf)
{
  buf->data = malloc(4096);
  if (buf->data == NULL)
    err(1, "malloc failed");
  buf->allocated_len = 4096;
  buf->size = 0;
}

void
sbcl_buffer_destroy(sbcl_buffer *buf)
{
  free(buf->data);
  buf->data = NULL;
  buf->allocated_len = 0;
  buf->size = 0;
}

size_t
sbcl_buffer_add(sbcl_buffer *buf, void *data, size_t data_size)
{
  if (buf->size + data_size > buf->allocated_len) {
    buf->allocated_len *= 2;
    buf->data = realloc(buf->data, buf->allocated_len);
    if (buf->data == NULL)
      err(1, "realloc failed");
  }

  memcpy(buf->data + buf->size, data, data_size);

  size_t return_value = buf->size;
  buf->size += data_size;
  return return_value;
}

void
sbcl_elf_strtab_init(sbcl_buffer *strtab)
{
  sbcl_buffer_init(strtab);
  // The first entry in the strtab is always a null string.
  sbcl_buffer_add(strtab, "", 1);
}

void
sbcl_elf_strtab_destroy(sbcl_buffer *strtab)
{
  sbcl_buffer_destroy(strtab);
}

size_t
sbcl_elf_add_strtab_entry(sbcl_buffer *strtab, char *str, size_t len)
{
  if (len == 0) {
    len = strlen(str);
  }

  if (len == 0)
    return 0; // offset 0 is the null string.

  return sbcl_buffer_add(strtab, str, len + 1);
}

void
sbcl_elf_init_symtab(sbcl_elf_symtab *symtab, sbcl_buffer *strtab)
{
  symtab->data_size = 16;
  symtab->data = malloc(symtab->data_size * sizeof(*symtab->data));
  if (symtab->data == NULL)
    err(1, "malloc failed");

  symtab->cur_offset = 0;

  // First entry is always a null symbol.
  sbcl_elf_add_symtab_entry2(symtab, strtab, "", STB_LOCAL, STT_NOTYPE,
                             SHN_UNDEF, 0, 0);
  // Second is always a FILE symbol. This entry is almost fully
  // useless (it can't actually be used to, let's say, determine what
  // file a symbol came from), so don't care what it says.
  sbcl_elf_add_symtab_entry2(symtab, strtab, "dummy", STB_LOCAL, STT_FILE,
                             SHN_ABS, 0, 0);
  symtab->num_local_entries = 2;
}

void
sbcl_elf_destroy_symtab(sbcl_elf_symtab *symtab)
{
  free(symtab->data);
  symtab->data = NULL;
  symtab->data_size = 0;
  symtab->cur_offset = 0;
}

size_t
sbcl_elf_add_symtab_entry2(
    sbcl_elf_symtab *symtab, sbcl_buffer *strtab,
    char *name, unsigned char bind, unsigned char type,
    Elf64_Section shndx, GElf_Addr value, GElf_Xword size)
{
  if (symtab->cur_offset >= symtab->data_size) {
    symtab->data_size *= 2;
    symtab->data = realloc(symtab->data,
                           symtab->data_size * sizeof(*symtab->data));
    if (symtab->data == NULL)
      err(1, "realloc failed");
  }

  GElf_Sym *sym = &symtab->data[symtab->cur_offset];
  sym->st_name = sbcl_elf_add_strtab_entry(strtab, name, 0);
  sym->st_info = GELF_ST_INFO(bind, type);
  sym->st_other = 0;
  sym->st_shndx = shndx;
  sym->st_value = value;
  sym->st_size = size;

  size_t return_value = symtab->cur_offset;
  symtab->cur_offset++;
  return return_value;
}

size_t
sbcl_elf_add_symtab_entry(
    sbcl_elf *e,
    char *name, unsigned char bind, unsigned char type,
    Elf64_Section shndx, GElf_Addr value, GElf_Xword size)
{
  return sbcl_elf_add_symtab_entry2(&e->symtab, &e->strtab, name, bind, type,
                                    shndx, value, size);
}


Elf_Scn*
sbcl_elf_new_section(sbcl_elf* e)
{
  Elf_Scn* scn = elf_newscn(e->elf);
  if (scn == NULL)
    errx(1, "elf_newscn() failed: %s.", elf_errmsg(-1));

  return scn;
}

size_t
sbcl_elf_output_space(sbcl_elf* e, char* name, void* data, size_t size,
                      int flags)
{
  Elf_Scn* scn = elf_newscn(e->elf);
  if (scn == NULL)
    errx(1, "elf_newscn() failed: %s.", elf_errmsg(-1));

  Elf_Data* elf_data = elf_newdata(scn);
  if (elf_data == NULL)
    errx(1, "elf_newdata() failed: %s.", elf_errmsg(-1));

  fprintf(stderr, "Outputting section \"%s\" at %p size=0x%x flags=0x%x\n", name, data, (int)size, flags);

  elf_data->d_align = 1;
  elf_data->d_off = 0LL;
  elf_data->d_buf = data;
  elf_data->d_type = ELF_T_BYTE;
  elf_data->d_size = size;

  GElf_Shdr shdr;

  if (gelf_getshdr(scn, &shdr) == NULL)
    errx(1, "gelf_getshdr() failed: %s.", elf_errmsg(-1));
  shdr.sh_name = sbcl_elf_add_strtab_entry(&e->shstrtab, name, 0);
  shdr.sh_type = data ? SHT_PROGBITS : SHT_NOBITS;
  shdr.sh_flags = flags;
  shdr.sh_entsize = 0;
  shdr.sh_size = size;
  shdr.sh_addralign = 16;
  shdr.sh_addr = 0x0;
  if (!gelf_update_shdr(scn, &shdr))
    errx(1, "gelf_update_shdr() failed: %s.", elf_errmsg(-1));

  return elf_ndxscn(scn);
}

void
sbcl_elf_add_space_data(sbcl_elf* e, size_t ndx, void* data, size_t size)
{
  Elf_Scn *scn = elf_getscn(e->elf, ndx);
  if (!scn)
    errx(1, "elf_getscn() failed: %s.", elf_errmsg(-1));

  Elf_Data* elf_data = elf_newdata(scn);
  if (elf_data == NULL)
    errx(1, "elf_newdata() failed: %s.", elf_errmsg(-1));

  elf_data->d_align = 1;
  elf_data->d_buf = data;
  elf_data->d_off = 0LL;
  elf_data->d_size = size;
  elf_data->d_type = ELF_T_BYTE;
}

size_t
sbcl_elf_output_strtab(sbcl_elf *e, char *section_name, sbcl_buffer *strtab)
{
  // Need to add entry up front; if we're outputting shstrtab, this'll
  // be the final modification before it's output.
  size_t sectionname_idx = sbcl_elf_add_strtab_entry(&e->shstrtab, section_name,
                                                      0);

  Elf_Scn* scn = elf_newscn(e->elf);
  if (scn == NULL)
    errx(1, "elf_newscn() failed: %s.", elf_errmsg(-1));

  Elf_Data* elf_data = elf_newdata(scn);
  if (elf_data == NULL)
    errx(1, "elf_newdata() failed: %s.", elf_errmsg(-1));

  elf_data->d_align = 1;
  elf_data->d_buf = strtab->data;
  elf_data->d_off = 0LL;
  elf_data->d_size = strtab->size;
  elf_data->d_type = ELF_T_BYTE;

  GElf_Shdr shdr;
  if (!gelf_getshdr(scn, &shdr))
    errx(1, "elf32_getshdr() failed: %s.", elf_errmsg(-1));
  shdr.sh_name = sectionname_idx;
  shdr.sh_type = SHT_STRTAB;
  shdr.sh_flags = 0;
  shdr.sh_entsize = 0;
  if (!gelf_update_shdr(scn, &shdr))
    errx(1, "gelf_update_shdr() failed: %s.", elf_errmsg(-1));

  return elf_ndxscn(scn);
}

size_t
sbcl_elf_output_symtab(sbcl_elf *e)
{
  size_t sectionname_idx = sbcl_elf_add_strtab_entry(&e->shstrtab, ".symtab",
                                                     0);

  Elf_Scn* scn = elf_newscn(e->elf);
  if (scn == NULL)
    errx(1, "elf_newscn() failed: %s.", elf_errmsg(-1));

  Elf_Data* elf_data = elf_newdata(scn);
  if (elf_data == NULL)
    errx(1, "elf_newdata() failed: %s.", elf_errmsg(-1));

  size_t sym_size;
  if (gelf_getclass(e->elf) == ELFCLASS32) {
    sym_size = sizeof(Elf32_Sym);
  } else {
    sym_size = sizeof(Elf64_Sym);
  }
  e->symtab.mangled_data = malloc(e->symtab.data_size * sym_size);
  if(e->symtab.mangled_data == NULL)
    err(1, "malloc failed");

  elf_data->d_align = 8;
  elf_data->d_buf = e->symtab.mangled_data;
  elf_data->d_off = 0LL;
  elf_data->d_size = e->symtab.cur_offset * sym_size;
  elf_data->d_type = ELF_T_SYM;

  GElf_Shdr shdr;
  if (!gelf_getshdr(scn, &shdr))
    errx(1, "elf32_getshdr() failed: %s.", elf_errmsg(-1));
  shdr.sh_name = sectionname_idx;
  shdr.sh_type = SHT_SYMTAB;
  shdr.sh_flags = 0;
  shdr.sh_entsize = sym_size;
  shdr.sh_info = e->symtab.num_local_entries;

  if (!gelf_update_shdr(scn, &shdr))
    errx(1, "gelf_update_shdr() failed: %s.", elf_errmsg(-1));

  unsigned i;
  for (i = 0; i < e->symtab.cur_offset; ++i) {
    if (!gelf_update_sym(elf_data, i, &e->symtab.data[i]))
      errx(1, "gelf_update_sym() failed: %s.", elf_errmsg(-1));
  }

  return elf_ndxscn(scn);
}

void
sbcl_elf_create_section_link(sbcl_elf *e, Elf64_Section from, Elf64_Section to)
{
  Elf_Scn *fromscn = elf_getscn(e->elf, from);
  if (!fromscn)
    errx(1, "elf_getscn() failed: %s.", elf_errmsg(-1));

  GElf_Shdr fromshdr;

  gelf_getshdr(fromscn, &fromshdr);
  fromshdr.sh_link = to;
  gelf_update_shdr(fromscn, &fromshdr);
}

void
sbcl_elf_open(sbcl_elf *e, int fd)
{
  sbcl_elf_strtab_init(&e->shstrtab);
  sbcl_elf_strtab_init(&e->strtab);
  sbcl_elf_init_symtab(&e->symtab, &e->strtab);

  if (elf_version(EV_CURRENT) == EV_NONE)
    errx(1, "ELF library initialization failed: %s ", elf_errmsg(-1));

  e->elf = elf_begin(fd, ELF_C_WRITE, NULL);
  if (e->elf == NULL) errx(1, "elf_begin() failed: %s.", elf_errmsg(-1));

  if (gelf_newehdr(e->elf, ELFCLASS64) == 0)
    errx(1, "gelf_newehdr() failed: %s.", elf_errmsg(-1));

  GElf_Ehdr ehdr;

  if (gelf_getehdr(e->elf, &ehdr) == NULL)
    errx(1, "gelf_getehdr() failed: %s.", elf_errmsg(-1));

  ehdr.e_ident[EI_DATA] = ELFDATA2LSB;
  ehdr.e_machine = EM_X86_64; /* x86-64 object */
  ehdr.e_type = ET_REL;
  ehdr.e_version = EV_CURRENT;

  if (!gelf_update_ehdr(e->elf, &ehdr))
    errx(1, "elf_update_ehdr() failed: %s.", elf_errmsg(-1));
}

void
sbcl_elf_close(sbcl_elf *e)
{
  size_t symtab_shndx = sbcl_elf_output_symtab(e);
  size_t strtab_shndx = sbcl_elf_output_strtab(e, ".strtab", &e->strtab);
  sbcl_elf_create_section_link(e, symtab_shndx, strtab_shndx);

  GElf_Ehdr ehdr;
  if (gelf_getehdr(e->elf, &ehdr) == NULL)
    errx(1, "gelf_getehdr() failed: %s.", elf_errmsg(-1));

  ehdr.e_shstrndx = sbcl_elf_output_strtab(e, ".shstrtab", &e->shstrtab);

  if (!gelf_update_ehdr(e->elf, &ehdr))
    errx(1, "elf_update_ehdr() failed: %s.", elf_errmsg(-1));

  if (elf_update(e->elf, ELF_C_WRITE) < 0)
    errx(1, "elf_update() failed: %s.", elf_errmsg(-1));

  (void)elf_end(e->elf);

  sbcl_elf_strtab_destroy(&e->shstrtab);
  sbcl_elf_strtab_destroy(&e->strtab);
  sbcl_elf_destroy_symtab(&e->symtab);
}

uintptr_t
align_and_zero_gap(uintptr_t addr, uintptr_t size)
{
  uintptr_t rem = addr % size;
  if (rem > 0) {
    return addr - rem + size;
    memset((void*)addr, 0, size - rem);
  }
  return addr;
}

// Align GC areas on page boundaries.
void
sbcl_elf_align_gc_areas(sbcl_elf_gc_area *areas, size_t size)
{
  uintptr_t pagesize = sysconf(_SC_PAGESIZE);
  size_t i;
  for (i = 0; i < size; i++) {
    areas[i].free = align_and_zero_gap(areas[i].free, pagesize);
    areas[i].end = align_and_zero_gap(areas[i].end, pagesize);
  }
  return;
}

void
sbcl_elf_output_gc_areas(sbcl_elf *e, sbcl_elf_gc_area *areas, size_t size)
{
  size_t i;
  for (i = 0; i < size; i++) {
    sbcl_elf_output_space(
        e, areas[i].name,
        (void*)areas[i].start,
        areas[i].free - areas[i].start,
        areas[i].flags);
    sbcl_elf_output_space(
        e, areas[i].zero_name,
        NULL,
        areas[i].end - areas[i].free,
        areas[i].zero_flags);
  }
  return;
}

#endif // LISP_FEATURE_SB_ELF_CORE
