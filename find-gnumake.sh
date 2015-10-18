#!/bin/false
# Not a shell script, but something intended to be sourced from shell scripts
find_gnumake() {
  # the GNU dialect of "make" -- easier to find or port it than to
  # try to figure out how to port to the local dialect...
  if [ "$GNUMAKE" != "" ] ; then
    # The user is evidently trying to tell us something.
    GNUMAKE="$GNUMAKE"
  elif [ "GNU Make" = "`make -v 2>/dev/null | head -n 1 | cut -b 1-8`" ]; then
    GNUMAKE=make
  elif [ -x "`command -v gmake`" ] ; then
    # "gmake" is the preferred name in *BSD.
    GNUMAKE=gmake
  elif [ -x "`command -v gnumake`" ] ; then
    # MacOS X aka Darwin
    GNUMAKE=gnumake
  else
    echo "GNU Make not found. Try setting the environment variable GNUMAKE."
    exit 1
  fi
  export GNUMAKE
  #echo "//GNUMAKE=\"$GNUMAKE\""
}
