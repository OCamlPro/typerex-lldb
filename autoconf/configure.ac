AC_INIT(typerex-lldb,1.0)
CONFIGURE_ARGS=$*

AC_COPYRIGHT(Copyright 2016 OcamlPro SAS)
AC_CONFIG_MACRO_DIR([m4])
AC_PROG_OCAML
AC_PROG_CC
AC_PROG_CXX


AC_ARG_WITH([typerex],[], [  
  OBUILD_ROOTDIR=$withval
 ], [
  OBUILD_ROOTDIR=.
  ])
OBUILD_DSTDIR=$OBUILD_ROOTDIR/_obuild
AC_SUBST(OBUILD_ROOTDIR)
AC_SUBST(OBUILD_DSTDIR)

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

if { test -f "/usr/lib/llvm-3.5/include/lldb/API/SBDebugger.h"; } &&
   { test -f "/usr/lib/llvm-3.5/lib/liblldb.so.1"; }; then {
  LLVM_INCLUDE=/usr/lib/llvm-3.5/include;
  LIB_LLDB=/usr/lib/llvm-3.5/lib/liblldb.so.1;
  HAS_LLDB=true;
  };
else :
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

if test "x$prefix" = "xNONE"; then
#  prefix=$ac_default_prefix
  prefix="`eval dirname $OCAMLLIB`"
  libdir=$OCAMLLIB
  base="`eval basename $prefix`"
  echo base=$base
  if test "x$base" = "xlib"; then
    prefix="`eval dirname $prefix`"
  fi
else
  libdir=${libdir}/ocaml
fi
test "x$exec_prefix" = xNONE && exec_prefix='${prefix}'

libdir=${libdir}/typerex

bindir="`eval echo ${bindir}`"
bindir="`eval echo ${bindir}`"
mandir="`eval echo ${mandir}`"
mandir="`eval echo ${mandir}`"
libdir="`eval echo ${libdir}`"
libdir="`eval echo ${libdir}`"
datarootdir="`eval echo ${datarootdir}`"
datarootdir="`eval echo ${datarootdir}`"
datadir=${datarootdir}/typerex

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

  echo ocp-lldb will be installed in $OCAMLBIN

else

  echo Missing dep. ocp-lldb will not be compiled.

fi
