CC=gcc
export CC

all: 
	$(MAKE) -C ../asdf
	echo "(asdf:operate 'asdf:load-op :$(SYSTEM) :force t)" | \
	  $(SBCL) --eval '(load "../asdf/asdf")'

test: all
	echo "(asdf:operate (quote asdf:load-op) :$(SYSTEM)) "\
	     "(asdf:operate (quote asdf:test-op) :$(SYSTEM))" | \
	  $(SBCL) --eval '(load "../asdf/asdf")'


install:
	tar cf - . | ( cd $(INSTALL_DIR) && tar xpvf - )
	( cd  $(SBCL_HOME)/systems && ln -fs ../$(SYSTEM)/$(SYSTEM).asd . )
