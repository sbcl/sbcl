vpath %.fasl ../obj/sbcl-home/contrib/

all: asdf.fasl sb-posix.fasl sb-bsd-sockets.fasl sb-introspect.fasl sb-cltl2.fasl \
     sb-aclrepl.fasl sb-sprof.fasl sb-capstone.fasl sb-md5.fasl sb-capstone.fasl \
     sb-executable.fasl sb-gmp.fasl sb-mpfr.fasl sb-queue.fasl sb-rotate-byte.fasl \
     sb-simple-streams.fasl sb-concurrency.fasl sb-cover.fasl sb-graph.fasl
asdf.fasl:
	sh ./build-contrib $(basename $(@F))
sb-grovel.fasl: asdf.fasl
	sh ./build-contrib $(basename $(@F))
sb-rt.fasl: asdf.fasl
	sh ./build-contrib $(basename $(@F))
sb-posix.fasl: asdf.fasl sb-grovel.fasl sb-rt.fasl
	sh ./build-contrib $(basename $(@F))
sb-bsd-sockets.fasl: asdf.fasl sb-grovel.fasl sb-posix.fasl sb-rt.fasl
	sh ./build-contrib $(basename $(@F))
sb-cltl2.fasl: asdf.fasl sb-rt.fasl
	sh ./build-contrib $(basename $(@F))
sb-aclrepl.fasl: asdf.fasl sb-rt.fasl
	sh ./build-contrib $(basename $(@F))
sb-concurrency.fasl: asdf.fasl sb-rt.fasl
	sh ./build-contrib $(basename $(@F))
sb-sprof.fasl: asdf.fasl
	sh ./build-contrib $(basename $(@F))
sb-introspect.fasl: asdf.fasl sb-rt.fasl
	sh ./build-contrib $(basename $(@F))
sb-capstone.fasl: asdf.fasl sb-rt.fasl
	sh ./build-contrib $(basename $(@F))
sb-md5.fasl: asdf.fasl sb-rotate-byte.fasl sb-rt.fasl
	sh ./build-contrib $(basename $(@F))
sb-cover.fasl: asdf.fasl sb-md5.fasl
	sh ./build-contrib $(basename $(@F))
sb-executable.fasl: asdf.fasl
	sh ./build-contrib $(basename $(@F))
sb-gmp.fasl: asdf.fasl sb-rt.fasl
	sh ./build-contrib $(basename $(@F))
sb-mpfr.fasl: asdf.fasl sb-gmp.fasl sb-rt.fasl
	sh ./build-contrib $(basename $(@F))
sb-queue.fasl: asdf.fasl sb-concurrency.fasl
	sh ./build-contrib $(basename $(@F))
sb-rotate-byte.fasl: asdf.fasl
	sh ./build-contrib $(basename $(@F))
sb-simple-streams.fasl: asdf.fasl sb-posix.fasl sb-bsd-sockets.fasl sb-rt.fasl
	sh ./build-contrib $(basename $(@F))
sb-graph.fasl: asdf.fasl sb-rt.fasl
	sh ./build-contrib $(basename $(@F))
