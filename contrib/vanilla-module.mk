
$(MODULE).fasl: $(MODULE).lisp ../../output/sbcl.core
	$(SBCL) --eval '(compile-file "$(MODULE)")' </dev/null

test:: $(MODULE).fasl

install:
	cp $(MODULE).fasl $(INSTALL_DIR)
