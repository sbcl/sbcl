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

#define DYNAMIC_SPACE_SIZE	(0x40000000) /* may be up to 2GB */
#endif

#ifdef __linux__
#define READ_ONLY_SPACE_SIZE    (0x02800000) /* 40MB */

#define STATIC_SPACE_SIZE	(0x02fff000) /* 48MB - 1 page */

#define BINDING_STACK_START	(0x60000000)
#define BINDING_STACK_SIZE	(0x07fff000) /* 128MB - 1 page */

#define DYNAMIC_SPACE_SIZE	(0x20000000) /* 512MB */
#endif

#define CONTROL_STACK_SIZE	(CONTROL_STACK_END - CONTROL_STACK_START)
