CC=gcc
export CC SBCL

all: $(EXTRA_ALL_TARGETS)
	$(MAKE) -C ../asdf
	$(SBCL) --eval '(load "../asdf/asdf")' \
	  --eval "(setf asdf::*central-registry* '((MERGE-PATHNAMES \"systems/\" (TRUENAME (SB-EXT:POSIX-GETENV \"SBCL_HOME\")))))" \
	  --eval "(asdf:operate 'asdf:load-op :$(SYSTEM))" \
	  --eval "(quit)"

test: all
	echo "(asdf:operate (quote asdf:load-op) :$(SYSTEM)) "\
	     "(asdf:operate (quote asdf:test-op) :$(SYSTEM))" | \
	  $(SBCL) --eval '(load "../asdf/asdf")'


install: $(EXTRA_INSTALL_TARGETS)
	tar cf - . | ( cd $(INSTALL_DIR) && tar xpvf - )
	( cd  $(SBCL_HOME)/systems && ln -fs ../$(SYSTEM)/$(SYSTEM).asd . )
