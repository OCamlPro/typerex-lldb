AC_INIT(typerex-lldb,1.1.1)
CONFIGURE_ARGS=$*

AC_COPYRIGHT(Copyright 2016 OCamlPro SAS)
AC_CONFIG_MACRO_DIR([m4])

AC_ARG_WITH([typerex],[], [  
  OBUILD_ROOTDIR=$withval
 ], [
  OBUILD_ROOTDIR=.
  ])
OBUILD_DSTDIR=$OBUILD_ROOTDIR/_obuild
AC_SUBST(OBUILD_ROOTDIR)
AC_SUBST(OBUILD_DSTDIR)

AC_ARG_ENABLE(version-check,
 [  --disable-version-check            do not check OCaml version],
 [VERSION_CHECK="$enableval"],
 [VERSION_CHECK="yes"])

AC_PROG_CC

AC_PROG_OCAML

AX_SPLIT_VERSION(OCAMLVERSION)
OCAML_MAJOR_VERSION=$AX_MAJOR_VERSION
OCAML_MINOR_VERSION=$AX_MINOR_VERSION
OCAML_POINT_VERSION=$AX_POINT_VERSION

if test "$OCAMLC" = "no"; then
   AC_MSG_ERROR([You must install the OCaml compiler])
fi

  MINIMAL_VERSION=4.01.0
  AX_COMPARE_VERSION( [$OCAMLVERSION], [lt], [${MINIMAL_VERSION}],
     AC_MSG_ERROR([Your version of OCaml: $OCAMLVERSION is not supported. You need ${MINIMAL_VERSION} at least.]))

AC_PROG_CXX


case "$OCAMLLIB" in
  */lib/ocaml)
        OCAMLBIN=`dirname $OCAMLLIB`
        OCAMLBIN=`dirname $OCAMLBIN`
        OCAMLBIN=$OCAMLBIN/bin
        ;;
  */lib)
        OCAMLBIN=`dirname $OCAMLLIB`
        OCAMLBIN=$OCAMLBIN/bin
        ;;
  *) OCAMLBIN=/usr/local/bin
  ;;
esac

ROOTDIR=$(dirname $(pwd))

HAS_LLDB=false

for file in \
  "/usr/lib/llvm-${LLVM_VERSION}/include/lldb/API/SBDebugger.h" \
  "/usr/lib/llvm-${LLVM_VERSION}/lib/liblldb.so.1" \
  "/usr/lib/llvm-3.5/include/lldb/API/SBDebugger.h" \
  "/usr/lib/llvm-3.5/lib/liblldb.so.1" \
  "/usr/lib/llvm-3.6/include/lldb/API/SBDebugger.h" \
  "/usr/lib/llvm-3.6/lib/liblldb.so.1" \
  "/usr/lib/llvm-3.7/include/lldb/API/SBDebugger.h" \
  "/usr/lib/llvm-3.7/lib/liblldb.so.1" \
  "/usr/lib/llvm-3.8/include/lldb/API/SBDebugger.h" \
  "/usr/lib/llvm-3.8/lib/liblldb.so.1" \
  ; do
  echo -n Testing $file...
  (test -f $file && echo OK) || echo not found
done

if { test -f "/usr/lib/llvm-${LLVM_VERSION}/include/lldb/API/SBDebugger.h"; } &&
   { test -f "/usr/lib/llvm-${LLVM_VERSION}/lib/liblldb.so.1"; }; then {
  echo lldb-${LLVM_VERSION} found
  LLVM_INCLUDE=/usr/lib/llvm-${LLVM_VERSION}/include;
  LIB_LLDB=/usr/lib/llvm-${LLVM_VERSION}/lib/liblldb.so.1;
  HAS_LLDB=true;
  };
else
echo lldb-\$LLVM_VERSION not detected

if { test -f "/usr/lib/llvm-3.5/include/lldb/API/SBDebugger.h"; } &&
   { test -f "/usr/lib/llvm-3.5/lib/liblldb.so.1"; }; then {
  echo lldb-3.5 found
  LLVM_INCLUDE=/usr/lib/llvm-3.5/include;
  LIB_LLDB=/usr/lib/llvm-3.5/lib/liblldb.so.1;
  HAS_LLDB=true;
  };
else

echo lldb-3.5 not detected
if { test -f "/usr/lib/llvm-3.6/include/lldb/API/SBDebugger.h"; } &&
   { test -f "/usr/lib/llvm-3.6/lib/liblldb.so.1"; }; then {
  echo lldb-3.6 found
  LLVM_INCLUDE=/usr/lib/llvm-3.6/include;
  LIB_LLDB=/usr/lib/llvm-3.6/lib/liblldb.so.1;
  HAS_LLDB=true;
  };
else
  echo lldb-3.6 not detected
fi

fi
fi

# Maybe on XCode:
# /Applications/Xcode.app/Contents/SharedFrameworks/LLDB.framework/Versions/A/Resources/Python/lldb/_lldb.so

# Modify directly these lines with the locations for
# LLDB include files (in this directory, you should
# find lldb/API/SBDebugger.h)
# We cache a copy of lattest LLDB developments:
#LLVM_INCLUDE:=$(ROOTDIR)/lldb-linux-amd64-bins/2015-03-06+Debug+Asserts/include
#LLVM_INCLUDE:=$(HOME)/GIT/llvm.org/llvm/tools/lldb/include/

# -llldb won't work if there is no .so file (only .so.1 on my Debian)
# We cache a copy of lattest LLDB developments:
#LIB_LLDB:=$(ROOTDIR)/lldb-linux-amd64-bins/2015-03-06+Debug+Asserts/liblldb.so
#LIB_LLDB:=/home/lefessan/GIT/llvm.org/llvm/Debug+Asserts/lib/liblldb.so

ocamldir='${libdir}/ocaml'
metadir='${ocamldir}'

AC_PROG_FINDLIB
if test "${OCAMLFIND}" != "no"; then
  metadir="$(${OCAMLFIND} -printconf destdir)"
  echo "initializing metadir from ocamlfind config"
fi

if test "x${prefix}" = "xNONE"; then
  if test "x${OPAM_PREFIX}" != "x"; then
    echo inheriting OPAM settings from OPAM_PREFIX
    prefix="${OPAM_PREFIX}"
    ocamldir='${libdir}'
    metadir="${OPAM_PREFIX}/lib"
  else
    prefix="$(dirname ${OCAMLLIB})"
    base="$(basename ${prefix})"
    if test "x${base}" = "xlib"; then
      prefix="$(dirname ${prefix})"
    fi
  fi
fi

AC_ARG_WITH(ocamldir,
 [AS_HELP_STRING([--with-ocamldir=DIRNAME],
               [root storage of the OCaml distribution])],
 [ocamldir="${withval}"],
 [])

AC_ARG_WITH(metadir,
 [AS_HELP_STRING([--with-metadir=DIRNAME],
               [store meta files in DIRNAME])],
 [metadir="${withval}"],
 [])

AC_SUBST(ocamldir)
AC_SUBST(metadir)

# Since it is not required on my computer, I will let somebody who needs
# it add the corresponding test with AC_CHECK_LIB
HAS_LEDIT=false

AC_SUBST(CXX)
AC_SUBST(ROOTDIR)
AC_SUBST(CONFIGURE_ARGS)
AC_SUBST(OCAMLVERSION)
AC_SUBST(OCAMLLIB)
AC_SUBST(OCAMLOPT)
AC_SUBST(OCAMLDEP)
AC_SUBST(OCAMLBIN)
AC_SUBST(HAS_LLDB)
AC_SUBST(LLVM_INCLUDE)
AC_SUBST(LIB_LLDB)
AC_SUBST(HAS_LEDIT)

AC_CONFIG_FILES(Makefile.config config.ocpgen)
AC_OUTPUT

if test "x$HAS_LLDB" = "xtrue"; then

  echo ocp-lldb will be installed in ${bindir}

else

  echo Missing dep. ocp-lldb will not be compiled.
  exit 2
fi
