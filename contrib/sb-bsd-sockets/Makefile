SYSTEM=bsd-sockets

all: 
	$(MAKE) -C ../asdf
	echo "(asdf:operate 'asdf:load-op :$(SYSTEM))" | \
	  $(SBCL) --eval '(load "../asdf/asdf")'

test:
	true

install: all
	tar cf - . | ( cd $(INSTALL_DIR) && tar xpvf - )
	( cd  $(SBCL_HOME)/systems && ln -fs ../$(SYSTEM)/$(SYSTEM).asd . )
