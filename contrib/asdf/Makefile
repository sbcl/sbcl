DEST=$(SBCL_PWD)/obj/sbcl-home/contrib/
FASL=$(DEST)/asdf.fasl

fasl:: $(FASL)
$(FASL):: asdf.lisp ../../output/sbcl.core
	if [ -d asdf-upstream ] ; then rm -rf asdf-upstream ; fi
	mkdir -p $(DEST)
	$(SBCL) --eval '(compile-file #p"SYS:CONTRIB;ASDF;ASDF.LISP" :output-file (parse-native-namestring "$@"))' </dev/null

install::
	cp $(FASL) "$(BUILD_ROOT)$(INSTALL_DIR)"

test:: $(FASL)
	true

UPSTREAM=../../obj/asdf-upstream
up::
	sh pull-asdf.sh
	ln -f $(UPSTREAM)/build/asdf.lisp asdf.lisp
	ln -f $(UPSTREAM)/doc/asdf.texinfo asdf.texinfo
	ln -f $(UPSTREAM)/README README
