CC=gcc
export CC SBCL

all: $(EXTRA_ALL_TARGETS)
	$(MAKE) -C ../asdf
	$(SBCL) --eval '(load "../asdf/asdf")' \
	  --eval "(setf asdf::*central-registry* '((MERGE-PATHNAMES \"systems/\" (TRUENAME (SB-EXT:POSIX-GETENV \"SBCL_HOME\")))))" \
	  --eval "(push :sb-building-contrib *features*)" \
	  --eval "(asdf:operate 'asdf:load-op :$(SYSTEM))" \
	  --eval "(progn (when (probe-file \"$(SYSTEM).fasl\") (error \"fasl file exists\")) (with-open-file (s \"$(SYSTEM).lisp\" :direction :output :if-exists :error) (print (quote (require :asdf)) s) (print (quote (require :$(SYSTEM))) s)) (compile-file \"$(SYSTEM).lisp\") (delete-file \"$(SYSTEM).lisp\"))" \
	  --eval "(quit)"

test: all
	echo "(asdf:operate (quote asdf:load-op) :$(SYSTEM))" \
	     "(asdf:operate (quote asdf:test-op) :$(SYSTEM))" | \
	  $(SBCL) --eval '(load "../asdf/asdf")'


install: $(EXTRA_INSTALL_TARGETS)
	tar cf - . | ( cd $(BUILD_ROOT)$(INSTALL_DIR) && tar xpvf - )
	( cd  $(BUILD_ROOT)$(SBCL_HOME)/systems && ln -fs ../$(SYSTEM)/$(SYSTEM).asd . )
