# Set CFLAGS and LDFLAGS for sb-bsd-sockets.asd's invocation of gcc

UNAME:=$(shell uname -m)

export CFLAGS=-fPIC
ifeq (solaris,$(UNAME))
  export LDFLAGS=-shared -lresolv -lsocket -lnsl
else
  ifeq (Darwin,$(UNAME))
    export LDFLAGS=-bundle
  else
    ifeq (x86_64,$(UNAME))
      export LDFLAGS=-m32 -shared
      export CFLAGS+= -m32
    else
      export LDFLAGS=-shared
    endif
  endif
endif

SYSTEM=sb-bsd-sockets
include ../asdf-module.mk
