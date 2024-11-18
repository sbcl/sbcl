/*
 * This software is part of the SBCL system. See the README file for
 * more information.
 *
 * This software is derived from the CMU CL system, which was
 * written at Carnegie Mellon University and released into the
 * public domain. The software is in the public domain and is
 * provided with absolutely no warranty. See the COPYING and CREDITS
 * files for more information.
 */
# define _GNU_SOURCE /* needed for RTLD_DEFAULT from dlfcn.h */
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>

#include "genesis/sbcl.h"
#include "globals.h"
#include "runtime.h"
#include "genesis/cons.h"
#include "genesis/vector.h"
#include "genesis/symbol.h"
#include "genesis/static-symbols.h"
#include "thread.h"
#include "os.h"
#include "arch.h"
#include "interr.h"
#include "immobile-space.h"
#include "code.h"
#if defined(LISP_FEATURE_OS_PROVIDES_DLOPEN) && !defined(LISP_FEATURE_WIN32)
# include <dlfcn.h>
#endif
#if defined LISP_FEATURE_UNIX && defined LISP_FEATURE_SOFT_CARD_MARKS
#include "gc.h" // for find_page_index
#endif

/*
 * historically, this used sysconf to select the runtime page size
 * per recent changes on other arches and discussion on sbcl-devel,
 * however, this is not necessary -- the VM page size need not match
 * the OS page size (and the default backend page size has been
 * ramped up accordingly for efficiency reasons).
*/
os_vm_size_t os_vm_page_size = BACKEND_PAGE_BYTES;

/* Expose to Lisp the value of the preprocessor define. Don't touch! */
int install_sig_memory_fault_handler = INSTALL_SIG_MEMORY_FAULT_HANDLER;

/* Except for os_zero, these routines are only called by Lisp code.
 * These routines may also be replaced by os-dependent versions
 * instead. */

#ifdef LISP_FEATURE_CHENEYGC
void
os_zero(os_vm_address_t addr, os_vm_size_t length)
{
    os_vm_address_t block_start;
    os_vm_size_t block_size;

#ifdef DEBUG
    fprintf(stderr,";;; os_zero: addr: 0x%08x, len: 0x%08x\n",addr,length);
#endif

    block_start = os_round_up_to_page(addr);

    length -= block_start-addr;
    block_size = os_trunc_size_to_page(length);

    if (block_start > addr)
        bzero((char *)addr, block_start-addr);
    if (block_size < length)
        bzero((char *)block_start+block_size, length-block_size);

    if (block_size != 0) {
        /* Now deallocate and allocate the block so that it faults in
         * zero-filled. */

        os_deallocate(block_start, block_size);
        addr = os_alloc_gc_space(0, NOT_MOVABLE, block_start, block_size);

        if (addr == NULL || addr != block_start)
            lose("os_zero: block moved! %p ==> %p", block_start, addr);
    }
}
#endif

#include "sys_mmap.inc"
#ifdef LISP_FEATURE_USE_SYS_MMAP
os_vm_address_t os_allocate(os_vm_size_t len) {
    void* answer = sbcl_mmap(0, len, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, 0, 0);
    if (answer == MAP_FAILED) return 0;
    return answer;
}
void os_deallocate(os_vm_address_t addr, os_vm_size_t len) {
    sbcl_munmap(addr, len);
}
#else
os_vm_address_t
os_allocate(os_vm_size_t len)
{
    return os_alloc_gc_space(0, MOVABLE, (os_vm_address_t)NULL, len);
}

void
os_deallocate(os_vm_address_t addr, os_vm_size_t len)
{
#ifdef LISP_FEATURE_WIN32
    gc_assert(VirtualFree(addr, 0, MEM_RELEASE));
#else
    if (sbcl_munmap(addr, len) == -1) perror("munmap");
#endif
}
#endif

int
os_get_errno(void)
{
    return errno;
}

void
os_set_errno(int new_errno)
{
    errno = new_errno;
}

#if defined LISP_FEATURE_SB_THREAD && defined LISP_FEATURE_UNIX && !defined USE_DARWIN_GCD_SEMAPHORES && !defined CANNOT_USE_POSIX_SEM_T
void
os_sem_init(os_sem_t *sem, unsigned int value)
{
    if (-1==sem_init(sem, 0, value))
        lose("os_sem_init(%p, %u): %s", sem, value, strerror(errno));
}

void
os_sem_wait(os_sem_t *sem)
{
    while (-1 == sem_wait(sem))
        if (EINTR!=errno)
            lose("os_sem_wait(%p): %s", sem, strerror(errno));
}

void
os_sem_post(sem_t *sem)
{
    if (-1 == sem_post(sem))
        lose("os_sem_post(%p): %s", sem, strerror(errno));
}

void
os_sem_destroy(os_sem_t *sem)
{
    if (-1==sem_destroy(sem))
        lose("os_sem_destroy(%p): %s", sem, strerror(errno));
}

#endif

/* Genesis-time foreign fixups are resolved to linkage table locations
 * and for each of them a record is added to the REQUIRED_FOREIGN_SYMBOLS
 * vector, of the form "name" for a function reference,
 * or ("name") for a data reference. "name" is a base-string.
 *
 * Before any code in lisp image can be called, we have to resolve all
 * references to runtime foreign symbols that used to be static, adding linkage
 * table entry for each element of REQUIRED_FOREIGN_SYMBOLS.
 */

#ifndef LISP_FEATURE_WIN32
void *
os_dlsym_default(char *name)
{
    void *frob = dlsym(RTLD_DEFAULT, name);
    return frob;
}
#endif

int alien_linkage_table_n_prelinked;
extern lispobj* get_alien_linkage_table_initializer();
void os_link_runtime()
{
    lispobj* table = get_alien_linkage_table_initializer();
    if (table) {
        // Prefill the alien linkage table so that shrinkwrapped executables which
        // link in all their C library dependencies can avoid linking with -ldl
        // but extern-alien still works for newly compiled code.
        lispobj* ptr = table;
        int n = alien_linkage_table_n_prelinked = *ptr++;
        int index = 0;
        for ( ; n-- ; index++ ) {
            bool datap = *ptr == (lispobj)-1; // -1 can't be a function address
            if (datap) ++ptr;
            arch_write_linkage_table_entry(index, (void*)*ptr++, datap);
        }
        return;
    }

    struct vector* symbols = VECTOR(SymbolValue(REQUIRED_FOREIGN_SYMBOLS,0));
    int n = alien_linkage_table_n_prelinked = vector_len(symbols);
    int index;
    for (index = 0 ; index < n ; ++index)
    {
        lispobj item = symbols->data[index];
        bool datap = listp(item);
        lispobj symbol_name = datap ? CONS(item)->car : item;
        char *namechars = vector_sap(symbol_name);
        void* result = os_dlsym_default(namechars);

        if (result) {
            arch_write_linkage_table_entry(index, result, datap);
        } else { // startup might or might not work. ymmv
            fprintf(stderr, "Missing required foreign symbol '%s'\n", namechars);
        }
    }
}

void os_unlink_runtime()
{
}

bool gc_managed_heap_space_p(lispobj addr)
{
    if ((READ_ONLY_SPACE_START <= addr && addr < READ_ONLY_SPACE_END)
        || (STATIC_SPACE_START <= addr && addr < STATIC_SPACE_END)
#if defined LISP_FEATURE_GENERATIONAL
        || (DYNAMIC_SPACE_START <= addr &&
            addr < (DYNAMIC_SPACE_START + dynamic_space_size))
        || immobile_space_p(addr)
#else
        || (DYNAMIC_0_SPACE_START <= addr &&
            addr < DYNAMIC_0_SPACE_START + dynamic_space_size)
        || (DYNAMIC_1_SPACE_START <= addr &&
            addr < DYNAMIC_1_SPACE_START + dynamic_space_size)
#endif
#ifdef LISP_FEATURE_PERMGEN
        || (PERMGEN_SPACE_START <= addr && addr < (uword_t)permgen_space_free_pointer)
#endif
#ifdef LISP_FEATURE_DARWIN_JIT
        || (STATIC_CODE_SPACE_START <= addr && addr < STATIC_CODE_SPACE_END)
#endif
        )
        return 1;
    return 0;
}

#ifndef LISP_FEATURE_WIN32

#if defined LISP_FEATURE_MIPS
#include <sys/utsname.h>
#endif
/* Remap a part of an already existing memory mapping from a file,
 * and/or create a new mapping as need be */
void* load_core_bytes(int fd, os_vm_offset_t offset, os_vm_address_t addr, os_vm_size_t len,
                      int is_readonly_space)
{
#if defined LISP_FEATURE_MIPS
    /* Of the few MIPS machines I have access to, one definitely exhibits a
     * horrible bug that mmap() persists MAP_PRIVATE pages back to disk,
     * even though we alwayas open a core file as O_RDONLY. This is a kooky criterion
     * to restrict the test by, but I didn't want it to be more general */
    static int buggy_map_private;
    if (!buggy_map_private) {
        struct utsname name;
        uname(&name);
        if (!strcmp(name.version, "#1 SMP PREEMPT Mon Aug 3 14:22:54 PDT 2015") &&
            !strcmp(name.release, "4.1.4")) {
            buggy_map_private = 1;
            fprintf(stderr, "WARNING: assuming that MAP_PRIVATE does not work on this kernel\n");
        } else {
            // fprintf(stderr, "INFO: kernel looks OK: [%s] [%s]\n", name.release, name.version);
            buggy_map_private = -1;
        }
    }
    if (buggy_map_private == 1) {
        off_t old = lseek(fd, 0, SEEK_CUR);
        lseek(fd, offset, SEEK_SET);
        read(fd, addr, len);
        lseek(fd, old, SEEK_SET);
        return addr;
    }
#endif
    int fail = 0;
    os_vm_address_t actual;
    int protection = 0, sharing = MAP_PRIVATE;

#ifdef LISP_FEATURE_DARWIN_JIT
    protection = OS_VM_PROT_READ | (is_readonly_space ?  OS_VM_PROT_EXECUTE : OS_VM_PROT_WRITE);
#else
    /* If mapping to an OS-chosen address, then the assumption is that we're not going to
     * execute nor write at the mapped address. (Because why would we ? The spaces from
     * the core have a chosen address at this point) However, the addr=0 case is for
     * 'editcore' which unfortunately _does_ write the memory. I'd prefer that it not,
     * but that's not the concern here. */
    protection = (addr ? (is_readonly_space ? OS_VM_PROT_READ : OS_VM_PROT_ALL)
                  : OS_VM_PROT_READ | OS_VM_PROT_WRITE);
    if (is_readonly_space) sharing = MAP_SHARED;
#endif

#ifdef LISP_FEATURE_64_BIT
    actual = sbcl_mmap(
#else
    /* FIXME: why does using sbcl_mmap cause failure here? I would guess that it can't
     * pass 'offset' correctly if LARGEFILE is mandatory, which it isn't on 64-bit.
     * Deadlock should be impossible this early in core loading, I suppose, hence
     * on one hand I don't care; but on the other, it would be nice to not to see
     * any use of a potentially hooked mmap() API within this file. */
     actual = mmap(
#endif
                  addr, len, protection,
                  // Do not pass MAP_FIXED with addr of 0, because most OSes disallow that.
                  sharing | (addr ? MAP_FIXED : 0),
                  fd, (off_t) offset);
    if (actual == MAP_FAILED) {
        if (errno == ENOMEM)
            fprintf(stderr, "load_core_bytes: mmap(%p,%zu,...) failed with ENOMEM\n", addr, len);
        else
            perror("mmap");
        fail = 1;
    } else if (addr && (addr != actual)) {
        fail = 1;
    }
    if (fail)
        lose("load_core_bytes(%d,%p,%p,%zx) failed", fd, (void*)(uintptr_t)offset, addr, len);
    return (void*)actual;
}

#ifdef LISP_FEATURE_DARWIN_JIT
void* load_core_bytes_jit(int fd, os_vm_offset_t offset, os_vm_address_t addr, os_vm_size_t len)
{
    ssize_t count;

    lseek(fd, offset, SEEK_SET);

    size_t n_bytes = 65536;
    char* buf = malloc(n_bytes);

    while (len) {
        count = read(fd, buf, n_bytes);

        if (count <= -1) {
            perror("read");
        }

        memcpy(addr, buf, count);
        addr += count;
        len -= count;
    }
    free(buf);
    return (void*)0;
}
#endif

#endif

bool is_in_stack_space(lispobj ptr)
{
    struct thread *th;
    for_each_thread(th) {
        if ((th->control_stack_start <= (lispobj *)ptr) &&
            (th->control_stack_end >= (lispobj *)ptr)) {
            return 1;
        }
    }
    return 0;
}

bool gc_managed_addr_p(lispobj addr)
{
    struct thread *th;

    if (gc_managed_heap_space_p(addr))
        return 1;
    for_each_thread(th) {
        if(th->control_stack_start <= (lispobj*)addr
           && (lispobj*)addr < th->control_stack_end)
            return 1;
        if(th->binding_stack_start <= (lispobj*)addr
           && (lispobj*)addr < th->binding_stack_start + BINDING_STACK_SIZE)
            return 1;
    }
    return 0;
}

uword_t os_context_pc(os_context_t* context) {
    return OS_CONTEXT_PC(context);
}
void set_os_context_pc(os_context_t* context, uword_t pc) {
    OS_CONTEXT_PC(context) = pc;
}
os_context_register_t* os_context_pc_addr(os_context_t* context) {
    return (os_context_register_t*)&(OS_CONTEXT_PC(context));
}

void *checked_malloc(size_t size)
{
    void* result = malloc(size);
    if (0 == result) {
        lose("malloc(%zu) failure", size);
    } else {
        return result;
    }
    return (void *) NULL; /* dummy value: return something ... */
}

char *copied_string(char *string)
{
    return strcpy(checked_malloc(1+strlen(string)), string);
}

lispobj* duplicate_codeblob_offheap(lispobj code)
{
    int nwords = code_total_nwords((struct code*)(code-OTHER_POINTER_LOWTAG));
    lispobj* mem = malloc((nwords+1) * N_WORD_BYTES);
    if ((uword_t)mem & LOWTAG_MASK) lose("this is unexpected\n");
    // add 1 word if not dualword aligned
    lispobj* copy = (lispobj*)((uword_t)mem + ((uword_t)mem & N_WORD_BYTES));
    memcpy(copy, (char*)code-OTHER_POINTER_LOWTAG, nwords<<WORD_SHIFT);
    return mem;
}

#ifdef LISP_FEATURE_UNIX
void
os_protect(os_vm_address_t address, os_vm_size_t length, os_vm_prot_t prot)
{
#if defined LISP_FEATURE_SOFT_CARD_MARKS && !defined LISP_FEATURE_DARWIN_JIT
    // dynamic space should not have protections manipulated
    if (find_page_index(address) >= 0)
        lose("unexpected call to os_protect with software card marks");
#endif
    if (sbcl_mprotect(address, length, prot) < 0) {
#ifdef LISP_FEATURE_LINUX
        if (errno == ENOMEM) {
            lose("An mprotect call failed with ENOMEM. This probably means that the maximum amount\n"
                 "of separate memory mappings was exceeded. To fix the problem, either increase\n"
                 "the maximum with e.g. 'echo 262144 > /proc/sys/vm/max_map_count' or recompile\n"
                 "SBCL with a larger value for GENCGC-PAGE-BYTES in\n"
                 "'src/compiler/"SBCL_TARGET_ARCHITECTURE_STRING"/parms.lisp'.");
        }
#endif
        lose("mprotect(%p,%lx,%d) error %d", address, (long)length, prot, errno);
    }
}
#endif

#ifdef TRACE_MMAP_SYSCALLS
FILE* mmgr_debug_logfile;
// interceptors for debugging so I don't have to reinvent them every time
static void decode_protbits(int prot, char result[4]) {
    result[0] = (prot & PROT_READ) ? 'r' : '-';
    result[1] = (prot & PROT_WRITE) ? 'w' : '-';
    result[2] = (prot & PROT_EXEC) ? 'x' : '-';
    result[3] = 0;
}
static void decode_flagbits(int flags, char result[40]) {
    char *p = result;
    char delim = '{';
#define APPEND(str) { *p++ = delim; delim = '|'; strcpy(p, str); p += sizeof str-1; }
    if (flags & MAP_PRIVATE) APPEND("Pvt");
    if (flags & MAP_ANON) APPEND("Anon");
    if (flags & MAP_NORESERVE) APPEND("NoRsv");
    if (flags & MAP_JIT) APPEND("JIT");
#undef APPEND
    strcpy(p, "}");
}
void* traced_mmap(void* addr, size_t length, int prot, int flags, int fd, off_t offset) {
    char decoded_prot[4], decoded_flags[40];
    decode_protbits(prot, decoded_prot);
    decode_flagbits(flags, decoded_flags);
    void* result = mmap(addr, length, prot, flags, fd, offset);
    fprintf(mmgr_debug_logfile, "mmap(%p,%lx,%s,%s,%d,%llx)=%p\n", addr, length,
            decoded_prot, decoded_flags, fd, offset, result);
    return result;
}
int traced_munmap(void* addr, size_t length) {
    int result = munmap(addr, length);
    fprintf(mmgr_debug_logfile, "munmap(%p,%lx)=%d\n", addr, length, result);
    return result;
}
int sbcl_mprotect(void* addr, size_t length, int prot) {
    char decoded_prot[4];
    decode_protbits(prot, decoded_prot);
    int result = mprotect(addr, length, prot);
    fprintf(mmgr_debug_logfile, "mprotect(%p,%lx,%s)=%d\n", addr, length, decoded_prot, result);
    return result;
}
#endif

#ifdef LISP_FEATURE_ELF
#include <elf.h>

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
            gc_assert(!result); // there can be only 1
            result = h->sh_offset;
            if (lisp_rel_section_offset) break; // stop when both sections seen
        } else if (!strcmp(&shstrtab_strbuf[h->sh_name], "lisp.rel")) {
            gc_assert(!lisp_rel_section_offset); // there can be only 1
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
#endif
