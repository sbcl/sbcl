/*
 * This is a minimal nl_langinfo replacement that only handles CODESET.
 * By default, it returns UTF-8.  It checks if LC_CTYPE or LANG are set, and
 * uses LATIN-1 if it finds one set to C, or UTF-8 if it finds one set to
 * anything else.
 */

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

#include <stdlib.h>
#include <string.h>
#include "darwin-langinfo.h"

char *nl_langinfo(nl_item item)
{
  char *nada = "", *utf8 = "UTF-8", *latin1 = "LATIN-1";

  if (item != CODESET) {
    return nada;
  } else {
    char *ctype = getenv ("LC_CTYPE");

    if ((ctype != NULL) && (!strcmp(ctype, "C"))) {
      return latin1;
    } else if (ctype != NULL) {
      return utf8;
    } else {
      char *lang = getenv ("LANG");

      if ((lang != NULL) && (!strcmp(lang, "C"))) {
        return latin1;
      } else {
        return utf8;
      }
    }
  }
}
