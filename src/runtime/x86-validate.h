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

#define   BINDING_STACK_SIZE (  BINDING_STACK_END -   BINDING_STACK_START)
#define   CONTROL_STACK_SIZE (  CONTROL_STACK_END -   CONTROL_STACK_START)
#define   DYNAMIC_SPACE_SIZE (  DYNAMIC_SPACE_END -   DYNAMIC_SPACE_START)
#define READ_ONLY_SPACE_SIZE (READ_ONLY_SPACE_END - READ_ONLY_SPACE_START)
#define    STATIC_SPACE_SIZE (   STATIC_SPACE_END -    STATIC_SPACE_START)
