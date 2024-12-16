#!/bin/sh -e

REPOSITORY=https://gitlab.common-lisp.net/asdf/asdf.git
BRANCH=${1:-release}

UPSTREAM_NAME=asdf-upstream
UPSTREAM_DIR="../../obj/${UPSTREAM_NAME}"

if [ ! -d "../../obj/" ];
then
    mkdir "../../obj/"
fi

# Get the current ASDF release from the upstream repo.

echo "Pulling ASDF branch '${BRANCH}' from '${REPOSITORY}'"

if test -d "${UPSTREAM_NAME}"
then
    if test -d "${UPSTREAM_DIR}"
    then
        rm -rf "${UPSTREAM_NAME}"
    else
        mv "${UPSTREAM_NAME}" "${UPSTREAM_DIR}"
    fi
fi

if test -d "${UPSTREAM_DIR}"
then
    (cd "${UPSTREAM_DIR}"
    git checkout "${BRANCH}"
    git reset --hard "${BRANCH}"
    git pull -a origin "${BRANCH}")
else
    (cd ../../obj/
    git clone --branch "${BRANCH}" "${REPOSITORY}" "${UPSTREAM_NAME}")
fi

# Determine new version
VERSION=$( eval echo $(cat "${UPSTREAM_DIR}/version.lisp-expr") )
echo "New ASDF version is ${VERSION}"

# Update UIOP files
(
    cd "${UPSTREAM_DIR}" \
        && echo ";;; This is UIOP ${VERSION}" \
        && cat $(make --quiet driver-files) \
        && echo '(provide "UIOP")' \
        && echo '(provide "uiop")'
) > uiop.lisp

# Update ASDF files
(
    cd "${UPSTREAM_DIR}" \
        && echo ";;; This is ASDF ${VERSION}" \
        && echo '(eval-when (:compile-toplevel :load-toplevel :execute) (require :uiop))' \
        && cat $(make --quiet defsystem-files)
) > asdf.lisp

# Updated documentation
cp "${UPSTREAM_DIR}/README.md" .
cp "${UPSTREAM_DIR}/doc/asdf.texinfo" .
