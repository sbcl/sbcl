
$(MODULE).fasl: $(MODULE).lisp ../../output/sbcl.core
	$(SBCL) --eval '(compile-file (format nil "SYS:CONTRIB;~:@(~A~);~:@(~A~).LISP" "$(MODULE)" "$(MODULE)"))' </dev/null

test:: $(MODULE).fasl

install:
	cp $(MODULE).fasl "$(BUILD_ROOT)$(INSTALL_DIR)"
