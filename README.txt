
  This is a binding over LLDB, LLVM Debugger, to customize a debugger in OCaml.
The stubs are mostly automatically generated, and cover a large part of
the available API (around 1027 stubs for 1052 methods, i.e. 97%).

Build and install
=================

You need "autoconf" and "aclocal".

```
./configure
```

You can read "config/Makefile" to verify that it suits your configuration. In
particular, ./configure does not check for LLDB files location, so you should
fix "config/Makefile" if it is not correct there.

```
make
```

License
=======

For now, all files are distributed under GPL v3. OCamlPro keeps the right to
distribute other copies of these files under another license.

Binding philosophy
==================

Enumeration types are in LLDBEnums, but are only done manually. You
will probably have to create your own ones. The code from
lldb-enumerations.h is copied there as a comment, so you just have to
copy the typedef you are interested in, and translate it to OCaml.

Generated stubs are generated in LLDBGenerated, but you should not use
this module directly. Instead, you should use LLDBOCaml, that is supposed to
provide an upper layer on top of it, and to include manually written stubs.
For each class, a sub-module is generated, with all class methods replaced
by functions that take the object as first parameter.

Stubs in LLDBGenerated often don't export enumeration types, but use
"int" instead. LLDBOCaml is a good place to translate ints to
enumeration types and export a better type for the function.

Some stubs expects to receive an object as argument, that will contain the
reply. For this reason, most sub-modules define a "create" function that
will only allocate the corresponding object, that can later be passed to the
stub to be initialized. Each sub-module also defines a "is_null" function,
that can be used to check if the object is NULL (which can happen if the object
was returned by a stub, or if [LLDBOCaml.delete] was called on it.

Most objects are manipulated in OCaml through a "custom" value, with a
finalizer that calls the object destructor on it. If this behavior is
not the one expected (and it will probably generate weird errors in
your application), you should fill a bug report so that we can change
the behavior for that object in the stub that creates it.

Examples
========

Look at "lldb.ml" for an example of driver using this binding.

ocp-lldb
========

ocp-lldb [OPTIONS] COMMAND ARGS
OPTIONS:
-no-auto-start: do not start the program (default is to start the program
  with a breakpoint on 'caml_program')
-- COMMAND ARGS: stop interpretation of arguments
-d: debug output

OCaml extensions:
ocaml modules: list available modules
ocaml break MODULE.function: put a breakpoint on the corresponding function
ocaml heap: print heap related values
ocaml print 0xFFF: print information on value at 0xFFF

Useful lldb commands:
=====================
c : continue
register read: print registers

Extensions
==========

When used with the OCaml fork of OCamlPro (with runtime type
information used for memory profiling), the debugger is able to
display OCaml blocks with the correct type information.
