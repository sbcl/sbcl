#!/bin/sh -e

# Get the current ASDF release from the upstream repo.

if test -d asdf-upstream
then
    if test -d ../../obj/asdf-upstream
    then
        rm -rf asdf-upstream
    else
        mv asdf-upstream ../../obj/asdf-upstream
    fi
fi

if test -d ../../obj/asdf-upstream
then
    (cd ../../obj/asdf-upstream/
    git checkout release
    git reset --hard release
    git pull -a origin release)
else
    (cd ../../obj/
    git clone --branch release git://common-lisp.net/projects/asdf/asdf.git asdf-upstream)
fi

( echo -n ";;; This is UIOP " ; eval echo $(cat ../../obj/asdf-upstream/version.lisp-expr) ;
  cd ../../obj/asdf-upstream && cat $(make --quiet driver-files) ;
  echo '(provide "UIOP")' ; echo '(provide "uiop")' ) > uiop.lisp
( echo -n ";;; This is ASDF " ; eval echo $(cat ../../obj/asdf-upstream/version.lisp-expr) ;
  echo '(eval-when (:compile-toplevel :load-toplevel :execute) (require :uiop))' ;
  cd ../../obj/asdf-upstream && cat $(make --quiet defsystem-files) ) > asdf.lisp
