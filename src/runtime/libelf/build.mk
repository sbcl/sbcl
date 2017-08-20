# $FreeBSD$

LIBELF_INCLUDES	:= -I$(LIBELF_DIR)/include
LIBELF_CPPFLAGS := $(CPPFLAGS) $(LIBELF_INCLUDES)
LIBELF_CFLAGS   := $(CFLAGS)
LIBELF_LDFLAGS  := $(LDFLAGS)
LIBELF_LIBNAME	:= elf_private
LIBELF_LIB      := $(LIBELF_DIR)/lib$(LIBELF_LIBNAME).a

LIBELF_BSDSRCS = 				\
	$(LIBELF_DIR)/strlcat.c			\
	$(LIBELF_DIR)/strlcpy.c

LIBELF_GENSRCS =				\
	$(LIBELF_DIR)/libelf_fsize.c		\
	$(LIBELF_DIR)/libelf_msize.c		\
	$(LIBELF_DIR)/libelf_convert.c

LIBELF_LIBSRCS =				\
	$(LIBELF_DIR)/elf_begin.c		\
	$(LIBELF_DIR)/elf_cntl.c		\
	$(LIBELF_DIR)/elf_end.c 		\
	$(LIBELF_DIR)/elf_errmsg.c		\
	$(LIBELF_DIR)/elf_errno.c		\
	$(LIBELF_DIR)/elf_data.c		\
	$(LIBELF_DIR)/elf_fill.c		\
	$(LIBELF_DIR)/elf_flag.c		\
	$(LIBELF_DIR)/elf_getarhdr.c		\
	$(LIBELF_DIR)/elf_getarsym.c		\
	$(LIBELF_DIR)/elf_getbase.c		\
	$(LIBELF_DIR)/elf_getident.c		\
	$(LIBELF_DIR)/elf_hash.c		\
	$(LIBELF_DIR)/elf_kind.c		\
	$(LIBELF_DIR)/elf_memory.c		\
	$(LIBELF_DIR)/elf_next.c		\
	$(LIBELF_DIR)/elf_rand.c		\
	$(LIBELF_DIR)/elf_rawfile.c		\
	$(LIBELF_DIR)/elf_phnum.c		\
	$(LIBELF_DIR)/elf_shnum.c		\
	$(LIBELF_DIR)/elf_shstrndx.c		\
	$(LIBELF_DIR)/elf_scn.c			\
	$(LIBELF_DIR)/elf_strptr.c		\
	$(LIBELF_DIR)/elf_update.c		\
	$(LIBELF_DIR)/elf_version.c		\
	$(LIBELF_DIR)/gelf_cap.c		\
	$(LIBELF_DIR)/gelf_checksum.c		\
	$(LIBELF_DIR)/gelf_dyn.c		\
	$(LIBELF_DIR)/gelf_ehdr.c		\
	$(LIBELF_DIR)/gelf_getclass.c		\
	$(LIBELF_DIR)/gelf_fsize.c		\
	$(LIBELF_DIR)/gelf_move.c		\
	$(LIBELF_DIR)/gelf_phdr.c		\
	$(LIBELF_DIR)/gelf_rel.c		\
	$(LIBELF_DIR)/gelf_rela.c		\
	$(LIBELF_DIR)/gelf_shdr.c		\
	$(LIBELF_DIR)/gelf_sym.c		\
	$(LIBELF_DIR)/gelf_syminfo.c		\
	$(LIBELF_DIR)/gelf_symshndx.c		\
	$(LIBELF_DIR)/gelf_xlate.c		\
	$(LIBELF_DIR)/libelf.c			\
	$(LIBELF_DIR)/libelf_align.c		\
	$(LIBELF_DIR)/libelf_allocate.c		\
	$(LIBELF_DIR)/libelf_ar.c		\
	$(LIBELF_DIR)/libelf_ar_util.c		\
	$(LIBELF_DIR)/libelf_checksum.c		\
	$(LIBELF_DIR)/libelf_data.c		\
	$(LIBELF_DIR)/libelf_ehdr.c		\
	$(LIBELF_DIR)/libelf_extended.c		\
	$(LIBELF_DIR)/libelf_phdr.c		\
	$(LIBELF_DIR)/libelf_shdr.c		\
	$(LIBELF_DIR)/libelf_xlate.c

LIBELF_SRCS = $(LIBELF_BSDSRCS) $(LIBELF_GENSRCS) $(LIBELF_LIBSRCS)

LIBELF_OBJS = $(LIBELF_SRCS:.c=.o)
LIBELF_CLEANFILES = $(LIBELF_GENSRCS) $(LIBELF_OBJS) $(LIBELF_LIB)

$(LIBELF_LIB): $(LIBELF_OBJS)
	$(AR) rcs $@ $^

%.o: %.c
	$(CC) -c $(LIBELF_CPPFLAGS) $(LIBELF_CFLAGS) $< -o $@

$(LIBELF_DIR)/libelf_convert.c:	$(LIBELF_DIR)/elf_types.m4 $(LIBELF_DIR)/libelf_convert.m4
$(LIBELF_DIR)/libelf_fsize.c:	$(LIBELF_DIR)/elf_types.m4 $(LIBELF_DIR)/libelf_fsize.m4
$(LIBELF_DIR)/libelf_msize.c:	$(LIBELF_DIR)/elf_types.m4 $(LIBELF_DIR)/libelf_msize.m4

.SUFFIXES: .m4 .c
.m4.c:
	m4 -D SRCDIR=$(LIBELF_DIR)/ $^ > $@

LIBELF_CLEAN	:= libelf_clean
$(LIBELF_CLEAN):
	-rm -f $(LIBELF_CLEANFILES)
