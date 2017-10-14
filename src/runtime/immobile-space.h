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

#ifndef _MARKNSWEEPGC_H_
#define _MARKNSWEEPGC_H_
#include <limits.h>
#include "core.h"

void prepare_immobile_space_for_save(lispobj init_function, boolean verbose);

#endif // _MARKNSWEEPGC_H_
