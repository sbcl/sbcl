/* -*- Mode: C -*- */

#ifdef LISP_FEATURE_USE_SYS_MMAP

///
///                **********************************
///                *  GC MUST NOT ACQUIRE ANY LOCKS *
///                **********************************
///
/// It's normally fine to use the mmap() system call to obtain memory for the
/// hopscotch tables, unless your C runtime intercepts mmap() and causes it
/// to sometimes (or always?) need a spinlock. That lock may be owned already,
/// so GC will patiently wait forever; meanwhile the lock owner is also
/// waiting forever on GC to finish.
/// So bypass the C library routine and call the OS directly
/// in case of a non-signal-safe interceptor such as
///   https://chromium.googlesource.com/chromium/src/third_party/tcmalloc/chromium/+/refs/heads/master/src/malloc_hook_mmap_linux.h#146
///
static inline void* sbcl_mmap(void* addr, size_t length, int prot, int flags,
                              int fd, off_t offset) {
    // "linux-os.h" brings in <syscall.h>, others may need something different.
    // mmap2 allows large file access with 32-bit off_t. We don't care about that,
    // but _usually_ only one or the other of the syscalls exists depending on,
    // various factors. Basing it on word size will pick the right one.
#ifdef LISP_FEATURE_64_BIT
    return (void*)syscall(SYS_mmap, addr, length, prot, flags, fd, offset);
#else
    return (void*)syscall(SYS_mmap2, addr, length, prot, flags, fd, offset);
#endif
}
static inline int sbcl_munmap(void* addr, size_t length) {
    return syscall(__NR_munmap, addr, length);
}

#else

#define sbcl_mmap mmap
#define sbcl_munmap munmap

#endif
