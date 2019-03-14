#ifdef __linux__
#ifdef __amd64__
#ifdef __ASSEMBLER__
.symver memcpy,memcpy@GLIBC_2.2.5
#else
__asm__(".symver memcpy,memcpy@GLIBC_2.2.5");
#endif
#endif
#ifdef __i386__
#ifdef __ASSEMBLER__
.symver memcpy,memcpy@GLIBC_2.0
#else
__asm__(".symver memcpy,memcpy@GLIBC_2.0");
#endif
#endif
#endif
