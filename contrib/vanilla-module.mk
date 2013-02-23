DEST=$(SBCL_PWD)/obj/sbcl-home/contrib/
FASL=$(DEST)/$(MODULE).fasl
ASD=$(DEST)/$(MODULE).asd

fasl:: $(FASL)
$(FASL):: $(MODULE).lisp ../../output/sbcl.core
	$(SBCL) --eval '(compile-file (format nil "SYS:CONTRIB;~:@(~A~);~:@(~A~).LISP" "$(MODULE)" "$(MODULE)") :output-file (parse-native-namestring "$@"))' </dev/null

$(ASD)::
	echo "(defsystem :$(MODULE) :class require-system)" > $@

test:: $(FASL) $(ASD)

install:
	cp $(FASL) $(ASD) "$(BUILD_ROOT)$(INSTALL_DIR)"
