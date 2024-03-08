vpath %.fasl ../obj/sbcl-home/contrib/

contribs = sb-posix sb-bsd-sockets sb-introspect sb-cltl2 sb-aclrepl \
     sb-sprof sb-capstone sb-md5 sb-capstone sb-executable sb-gmp sb-mpfr \
     sb-queue sb-rotate-byte sb-rt sb-simple-streams sb-concurrency sb-cover \
     sb-simd sb-grovel sb-perf asdf

active_contribs = $(filter-out $(SBCL_CONTRIB_BLOCKLIST),$(contribs))

fasls = $(active_contribs:=.fasl)

all: $(fasls)

# Inter-module dependencies are declared here but not in the per-directory Makefile.
# This way we don't have to state them twice.
sb-md5.fasl: sb-rotate-byte.fasl
sb-cover.fasl: sb-md5.fasl
sb-perf.fasl: sb-posix.fasl
sb-mpfr.fasl: sb-gmp.fasl
sb-queue.fasl: sb-concurrency.fasl
sb-simple-streams.fasl: sb-posix.fasl sb-bsd-sockets.fasl
sb-grovel.fasl: asdf.fasl # for building the module, not for our build

# Notes:
# 1. Invoking $(MAKE) for nested make eliminates a warning about jobserver mode.
# 2. This could be made to 'tee' into output/{something} as it used to,
#    but it would be more Lispy if not elegant to have 'make-contrib.lisp' bind
#    *ERROR-OUTPUT* to a broadcast stream of SYS:*STDERR* and a file stream.
# 3. The massively overapproximates the set of inputs needed to compile any given contrib
#    which avoids using per-target lists of files, or any power-user tricks such
#    as .SECONDEXPANSION and $(wildcard) in this makefile.
#    I tried all kinds of variations on "%/*.lisp" and nothing worked easily.
#    I suspect the right solution involves eliminating the other level of Makefile.
#    Also: a motivated person might be able to figure out how to express a dependence
#    on src/runtime/sbcl or src/runtime/sbcl.exe depending on the OS.
#    I'm going to assume that the depending on the core is enough.
%.fasl: */*.lisp */*/*.lisp ../output/sbcl.core
	$(MAKE) MODULE_REQUIRES="$(filter %.fasl, $^)" -C $*
