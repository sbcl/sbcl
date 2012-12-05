MODULE=sb-sprof
include ../vanilla-module.mk

test::
	$(SBCL) --eval '(load (format nil "SYS:CONTRIB;~:@(~A~);TEST.LISP" "$(MODULE)"))' </dev/null
