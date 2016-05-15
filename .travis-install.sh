#!/bin/sh

export CC=/usr/bin/gcc-4.9
export CXX=/usr/bin/g++-4.9
#sudo update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-4.9 50

# This is what we would do if we needed something more:
OPAM_DEPS="ocp-build lwt"
export OPAMYES=1 OPAMVERBOSE=1

echo System OCaml version
ocaml -version
echo OPAM versions
opam --version
opam --git-version

opam init
opam switch $OCAML_VERSION

if [ "${OPAM_DEPS}" != "" ] ; then
    opam install ${OPAM_DEPS}
fi

