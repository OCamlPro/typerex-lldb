include autoconf/Makefile.config

all: build-ocps

build-ocps:
	cd $(OBUILD_ROOTDIR); ocp-build init
	cd $(OBUILD_ROOTDIR); ocp-build

clean:
	rm -rf _obuild
	rm -f *~ autoconf/*~ autoconf/m4/*~

distclean: clean
	rm -f autoconf/config.status
	rm -f autoconf/config.log
	rm -f autoconf/config.ocpgen
	rm -f autoconf/Makefile.config
	rm -rf autoconf/autom4te.cache/

view-doc:
	xdg-open docs/docs-lldb-html/cpp_reference/html/namespacelldb.html &

