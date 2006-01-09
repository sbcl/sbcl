#!/bin/sh

#       After the ordeal, we went back to the Factory. Greenblatt said he
# was gonna locate us in a cell. He said: "Kid, I'm gonna INTERN you in a
# cell. I want your manual and your mouse."
#       I said, "Greenblatt, I can understand your wantin' my manual, so
# I don't have any documentation about the cell, but what do you want my
# mouse for?" and he said, "Kid, we don't want any window system problems".
# I said, "Greenblatt, did you think I was gonna deexpose myself for
# litterin'?"
#       Greenblatt said he was makin' sure, and, friends, Greenblatt
# was, 'cause he took out the left Meta-key so I couldn't double bucky the
# rubout and cold-boot, and he took out the Inspector so I couldn't
# click-left on Modify, set the PROCESS-WARM-BOOT-ACTION on the window,
# *THROW around the UNWIND-PROTECT and have an escape. Greenblatt was
# makin' sure.
#                -- from Alice's Lispm (or MIT's AI Lab)

# This software is part of the SBCL system. See the README file for
# more information.
#
# This software is derived from the CMU CL system, which was
# written at Carnegie Mellon University and released into the
# public domain. The software is in the public domain and is
# provided with absolutely no warranty. See the COPYING and CREDITS
# files for more information.

rm -f *.include

echo "@menu" > top-menu.include
for texinfo in *.texinfo
do
  if ! [ $texinfo = sbcl-internals.texinfo ]; then
      grep @node $texinfo | head -n 1 | perl -p -e "s/\@node\ (.*)/* \$1::/" >> top-menu.include
      echo "@include $texinfo" >> top-include.include
  fi
done
echo "@end menu" >> top-menu.include
