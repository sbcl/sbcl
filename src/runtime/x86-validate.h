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

/*
 * $Header$
 */

/*
 * address map:
 *
 *  FreeBSD:
 *	0x00000000->0x0E000000 224M C program and memory allocation.
 *	0x0E000000->0x10000000  32M Foreign segment.
 *	0x10000000->0x20000000 256M Read-Only Space.
 *	0x20000000->0x30000000 256M Reserved for shared libraries.
 *	0x30000000->0x38000000 128M Static Space.
 *	0x38000000->0x40000000 128M Binding stack growing up.
 *	0x40000000->0x48000000 128M Control stack growing down.
 *	0x48000000->0xC8000000 2GB  Dynamic Space.
 *	0xE0000000->           256M C stack - Alien stack.
 *
 *  OpenBSD:
 *      almost the same as FreeBSD
 *
 *  Linux: Note that this map has some problems and requires some further
 *	   development so is not implemented below.
 *	0x00000000->0x08000000 128M Unused.
 *	0x08000000->0x10000000 128M C program and memory allocation.
 *	0x10000000->0x20000000 256M Read-Only Space.
 *	0x20000000->0x28000000 128M Binding stack growing up.
 *	0x28000000->0x38000000 256M Static Space.
 *	0x38000000->0x40000000 128M Control stack growing down.
 *	0x40000000->0x48000000 128M Reserved for shared libraries.
 *	0x48000000->0xB8000000 1.75G Dynamic Space.
 *
 * FIXME: There's something wrong with addressing maps which are so
 * brittle that they can be commented as fixed addresses. Try to
 * parameterize these so they can be set at build time.
 */

#if defined(__FreeBSD__) || defined(__OpenBSD__)
#define READ_ONLY_SPACE_SIZE    (0x0ffff000) /* 256MB - 1 page */

#if defined __FreeBSD__
#define STATIC_SPACE_SIZE	(0x07fff000) /* 128M - 1 page */
#elif defined __OpenBSD__
#define STATIC_SPACE_SIZE	(0x0ffff000) /* 256M - 1 page */
#else
#error unsupported BSD variant
#endif


#define BINDING_STACK_START	(0x38000000)
#define BINDING_STACK_SIZE	(0x07fff000) /* 128MB - 1 page */

#define CONTROL_STACK_START	(0x40000000)
#define CONTROL_STACK_SIZE	(0x08000000) /* 128MB */

#define DYNAMIC_0_SPACE_START	(0x48000000)
#ifdef GENCGC
#define DYNAMIC_SPACE_SIZE	(0x40000000) /* May be up to 2GB */
#else
#define DYNAMIC_SPACE_SIZE	(0x04000000) /* 64MB */
#endif
#endif

/* FIXME: It's gross to have numbers like 0x50000000 wired into the
 * code in multiple places like this. (Not only does this file know
 * about it, but Lisp code knows about it, because Lisp code is able
 * to generate absolute addresses for all the static symbols even
 * before it's read the map file.) I don't know whether I should
 * actually *fix* this, but I should at least document it some with a
 * KLUDGE marker. And it might even be fixable, by putting all this
 * memory space arbitrariness into an architecture-dependent Lisp
 * file. If so, perhaps I should write somewhere in a "design
 * principles" or "coding principles" file that information like this
 * always flows from Lisp code to C code, through sbcl.h. */
#ifdef __linux__
#define READ_ONLY_SPACE_SIZE    (0x02800000) /* 40MB */

#define STATIC_SPACE_SIZE	(0x02fff000) /* 48MB - 1 page */

#define BINDING_STACK_START	(0x60000000)
#define BINDING_STACK_SIZE	(0x07fff000) /* 128MB - 1 page */

#define CONTROL_STACK_START	(0x50000000)
#define CONTROL_STACK_SIZE	(0x07fff000) /* 128MB - 1 page */

#define DYNAMIC_0_SPACE_START	(0x09000000)
#ifdef GENCGC
#define DYNAMIC_SPACE_SIZE	(0x20000000) /* 512MB */
#else
#define DYNAMIC_SPACE_SIZE	(0x04000000) /* 64MB */
#endif
#endif

#define CONTROL_STACK_END	(CONTROL_STACK_START + CONTROL_STACK_SIZE)

/* Note that GENCGC only uses dynamic_space 0. */
#define DYNAMIC_1_SPACE_START	(DYNAMIC_0_SPACE_START + DYNAMIC_SPACE_SIZE)
