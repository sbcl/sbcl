
$(MODULE).fasl: $(MODULE).lisp
	$(SBCL) --eval '(compile-file "$(MODULE)")' </dev/null

test:: $(MODULE).fasl

install:
	cp $(MODULE).fasl $(INSTALL_DIR)
