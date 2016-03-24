all: build
build: python.asm gc_stats.asm alloc_prof.asm ocp-lldb

include ../config/Makefile

clean:
	rm -f ocp-lldb *.o *~ *.cm? readHH *.asm *.a *.cmxa core
	rm -f lldbGenerated_ml.h lldbClasses_ml.h lldbDelete_ml.h
	rm -f lldbIncludes_ml.h LLDBGenerated.ml graph.dot
	rm -f lldbBadStubs_ml.h lldbClasses.class

install:

LLDB_MLS := lldbTypes.ml lldbGcstats.ml lldbOCamlCommand.ml lldbMain.ml
LLDB_CMXS := $(LLDB_MLS:.ml=.cmx)


INCLUDES=-I ../lib
LIBCMXA=../lib/ocplldb.cmxa
LIBA=../lib/lldbOCaml.a

ocp-lldb: $(LLDB_CMXS) $(LIBCMXA)  $(LIBA)
	$(OCAMLOPT) -g -output-obj -o ocp-lldb-ocaml.o $(LIBCMXA) $(LLDB_CMXS)
	$(CXX) -g -o ocp-lldb -std=c++11 ocp-lldb-ocaml.o $(LIBA) -ledit  $(LIB_LLDB) $(OCAML_RUNTIME) -ldl


distclean: clean

depend:
	$(OCAMLDEP) $(INCLUDES) *.ml *.mli > .depend

view-doc:
	xdg-open ../docs/docs-lldb-html/cpp_reference/html/namespacelldb.html &

include .depend

python.asm:  $(LIBA) $(LIBCMXA)
gc_stats.asm:  $(LIBA) $(LIBCMXA)
alloc_prof.asm:  $(LIBA) $(LIBCMXA)
.SUFFIXES: .o .cpp .cmx .ml .asm

.cpp.o:
	$(CXX) -g -c -std=c++11 -I $(OCAMLLIB) -I $(LLVM_INCLUDE) $<

.ml.cmx:
	$(OCAMLOPT) $(INCLUDES) -g -c $<

.ml.asm: 
	$(OCAMLOPT) $(INCLUDES) -g -output-obj -o ocp-$*-ocaml.o $(LIBCMXA) $*.ml 
	$(CXX) -g -o $*.asm -std=c++11 ocp-$*-ocaml.o $(LIBA) -ledit  $(LIB_LLDB) $(OCAML_RUNTIME) -ldl
