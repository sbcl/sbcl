SYSTEM=asdf-install
EXTRA_INSTALL_TARGETS=asdf-install-install

include ../asdf-module.mk

asdf-install-install: asdf-install
	if test -f $(INSTALL_ROOT)/bin/sbcl-asdf-install ; then \
	 mv $(INSTALL_ROOT)/bin/sbcl-asdf-install $(INSTALL_ROOT)/bin/sbcl-asdf-install.old ; \
	 fi
	cp asdf-install $(INSTALL_ROOT)/bin/sbcl-asdf-install
