asdf.fasl: asdf.lisp
	$(SBCL) --eval '(compile-file "asdf")' </dev/null

test:
	true

install: asdf.fasl
	cp $< $(INSTALL_DIR)
