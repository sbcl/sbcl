SYSTEM=asdf-install
EXTRA_INSTALL_TARGETS=asdf-install-install

include ../asdf-module.mk

asdf-install-install: asdf-install
	if test -f $(BUILD_ROOT)$(INSTALL_ROOT)/bin/sbcl-asdf-install ; then \
	  mv $(BUILD_ROOT)$(INSTALL_ROOT)/bin/sbcl-asdf-install $(BUILD_ROOT)$(INSTALL_ROOT)/bin/sbcl-asdf-install.old ; \
	fi
# KLUDGE: mv rather than cp because keeping asdf-install in that
# directory interferes with REQUIRE, and this is done before the tar 
# in ../asdf-module.mk.  Better solutions welcome.
	mv asdf-install $(BUILD_ROOT)$(INSTALL_ROOT)/bin/sbcl-asdf-install
