# FIXME I: This is more-or-less an exact copy of sb-bsd-sockets's
# Makefile.  Maybe we should have a vanilla-asdf-module.mk?
#
# FIXME II: The thing that is preventing this is the use of RT as a
# regression tester... since we ask that modules do regression tests,
# maybe we should provide a regression test framework (in SB-RT, perhaps)?

SYSTEM=sb-rotate-byte

all: 
	$(MAKE) -C ../asdf
	echo "(asdf:operate 'asdf:load-op :$(SYSTEM) :force t)" | \
	  $(SBCL) --eval '(load "../asdf/asdf")'

test: all
	echo "(asdf:operate 'asdf:load-op :$(SYSTEM)) \
	      (load (compile-file \"rotate-byte-tests.lisp\"))" | \
	  $(SBCL) --eval '(load "../asdf/asdf")'


install:
	tar cf - . | ( cd $(INSTALL_DIR) && tar xpvf - )
	( cd  $(SBCL_HOME)/systems && ln -fs ../$(SYSTEM)/$(SYSTEM).asd . )
