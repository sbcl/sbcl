SYSTEM=sb-bsd-sockets

all: 
	$(MAKE) -C ../asdf
	echo "(asdf:operate 'asdf:load-op :$(SYSTEM))" | \
	  $(SBCL) --eval '(load "../asdf/asdf")'

test: all
	echo "(asdf:operate 'asdf:load-op :$(SYSTEM)) \
	      (or (rt:do-tests) (error \"test failed, cannot install\"))" | \
	  $(SBCL) --eval '(load "../asdf/asdf")'


install:
	tar cf - . | ( cd $(INSTALL_DIR) && tar xpvf - )
	( cd  $(SBCL_HOME)/systems && ln -fs ../$(SYSTEM)/$(SYSTEM).asd . )
