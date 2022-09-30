vpath %.fasl ../obj/sbcl-home/contrib/

contribs = sb-posix sb-bsd-sockets sb-introspect sb-cltl2 sb-aclrepl \
     sb-sprof sb-capstone sb-md5 sb-capstone sb-executable sb-gmp sb-mpfr \
     sb-queue sb-rotate-byte sb-rt sb-simple-streams sb-concurrency sb-cover \
     sb-graph sb-simd sb-grovel asdf

active_contribs = $(filter-out $(SBCL_CONTRIB_BLOCKLIST),$(contribs))

fasls = $(active_contribs:=.fasl)

all: $(fasls)

# Inter-module dependencies are declared here but not in the per-directory Makefile.
# This way we don't have to state them twice.
sb-md5.fasl: sb-rotate-byte.fasl
sb-cover.fasl: sb-md5.fasl
sb-mpfr.fasl: sb-gmp.fasl
sb-queue.fasl: sb-concurrency.fasl
sb-simple-streams.fasl: sb-posix.fasl sb-bsd-sockets.fasl
sb-grovel.fasl: asdf.fasl # for building the module, not for our build

# Notes:
# 1. Not sure if we still need mkdir asdf-cache. Nothing goes there.
# 2. Invoking $(MAKE) for nested make eliminates a warning about jobserver mode.
# 3. This could be made to 'tee' into output/building-contrib as it used to,
#    but it would be more Lispy if not elegant to have 'make-contrib.lisp' bind
#    *ERROR-OUTPUT* to a broadcast stream of SYS:*STDERR* and a file stream.
%.fasl:
	mkdir -p ../obj/asdf-cache/$(basename $(@F))
	$(MAKE) MODULE_REQUIRES="$^" -C $(basename $(@F)) </dev/null 2>&1
