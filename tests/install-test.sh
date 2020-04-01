#!/bin/sh
cd ..

SBCL_HOME=obj/sbcl-home/ ./src/runtime/sbcl --noinform --core output/sbcl.core --disable-debugger --eval '(print (require :asdf))' --disable-debugger --eval '(quit)'
./install.sh --prefix=/tmp/test-sbcl-install > /dev/null

/tmp/test-sbcl-install/bin/sbcl --noinform --disable-debugger --eval '(print (require :asdf))' --disable-debugger --eval '(quit)'
(cd /tmp/test-sbcl-install/bin/; ./sbcl --noinform --disable-debugger --eval '(print (require :asdf))' --disable-debugger --eval '(quit)')
PATH=/tmp/test-sbcl-install/bin sbcl --noinform --disable-debugger --eval '(print (require :asdf))' --disable-debugger --eval '(quit)'

/tmp/test-sbcl-install/bin/sbcl --noinform --core /tmp/test-sbcl-install/lib/sbcl/sbcl.core --disable-debugger --eval '(print (require :asdf))' --disable-debugger --eval '(quit)'
PATH=/tmp/test-sbcl-install/bin sbcl --noinform --core /tmp/test-sbcl-install/lib/sbcl/sbcl.core --disable-debugger --eval '(print (require :asdf))' --disable-debugger --eval '(quit)'
(cd /tmp/test-sbcl-install/bin; ./sbcl --noinform --core /tmp/test-sbcl-install/lib/sbcl/sbcl.core --disable-debugger --eval '(print (require :asdf))' --disable-debugger --eval '(quit)')

cd /tmp/test-sbcl-install/
ln -s bin/sbcl sym-sbcl
./sym-sbcl --noinform --disable-debugger --eval '(print (require :asdf))' --disable-debugger --eval '(quit)'
./sym-sbcl --noinform --core /tmp/test-sbcl-install/lib/sbcl/sbcl.core --disable-debugger --eval '(print (require :asdf))' --disable-debugger --eval '(quit)'

mv bin/sbcl .
ln -s /tmp/test-sbcl-install/sbcl ./bin/sbcl
./bin/sbcl --noinform --disable-debugger --eval '(print (require :asdf))' --disable-debugger --eval '(quit)'

mv lib/sbcl/sbcl.core lib/sbcl/contrib .
./sbcl --noinform --disable-debugger --eval '(print (require :asdf))' --disable-debugger --eval '(quit)'
PATH=. sbcl --noinform --disable-debugger --eval '(print (require :asdf))' --disable-debugger --eval '(quit)'
./sbcl --noinform --core sbcl.core --disable-debugger --eval '(print (require :asdf))' --disable-debugger --eval '(quit)'
PATH=. sbcl --noinform --core sbcl.core --disable-debugger --eval '(print (require :asdf))' --disable-debugger --eval '(quit)'

rm -r /tmp/test-sbcl-install
