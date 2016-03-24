(**************************************************************************)
(*                                                                        *)
(*                              OCamlPro TypeRex                          *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the GPL v3.0             *)
(*      (GNU Public Licence version 3.0).                                 *)
(*                                                                        *)
(*     Contact: <typerex@ocamlpro.com> (http://www.ocamlpro.com/)         *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
(**************************************************************************)


(* LLDB API *)
open LLDBEnums
open LLDBOCaml


(* ocp-lldb modules *)
open LLDBTypes
open LLDBGlobals

      (*
        let target = SBDebugger.getSelectedTarget debugger in
        begin
        match funname.[0] with
        | 'A'..'Z' ->
        let regex =
        try
        let pos = String.index funname '.' in
        let modname = String.sub funname 0 pos in
        let funname = String.sub funname (pos+1)
        (String.length funname -pos-1) in
        Printf.sprintf "caml%s__%s_[0-9]+" modname funname
        with _ ->
        Printf.sprintf "caml%s__entry" funname
        in
        let main_bp = SBTarget.breakpointCreateByRegex target
        regex
        (Some (SBFileSpec.getFilename (SBTarget.getExecutable target))) in
        let nlocs = SBBreakpoint.getNumLocations main_bp in
        let id = SBBreakpoint.getID main_bp in
        Printf.printf "Breakpoint %d set on %d locations\n%!"
        id nlocs;
        let _locs =
        Array.init nlocs
        (fun i ->
        let addr = SBBreakpointLocation.getAddress
        (SBBreakpoint.getLocationAtIndex main_bp i) in
        let sym = SBAddress.getSymbol addr in
        Printf.printf "    (%d.%d) %s\n%!"
        id (1+i) (SBSymbol.getName sym))
        in
        ()
        | _ ->
        Printf.printf "'ocaml break' expects a fully qualified name\n%!";
        end

        | [| "b"; funname |] ->
      *)

let ocaml_break_narg1 debugger funname =
  let target = SBDebugger.getSelectedTarget debugger in
  let ima = LLDBOCamlCode.get_compilation_units target in
  let modname, funname =
    try
      let pos = String.index funname '.' in
      let modname = String.sub funname 0 pos in
      let funname = String.sub funname (pos+1)
        (String.length funname -pos-1) in
      (modname, funname)
    with _ ->
      Printf.kprintf failwith "%S is not a fully-qualified function"
        funname
  in
  try
    let cu = StringMap.find modname ima.ima_cus_by_name in
    let prefix = Printf.sprintf "caml%s__%s_" modname funname in
    let prefixlen = String.length prefix in
    Printf.eprintf "Need to match %S\n%!" prefix;
    StringMap.iter (fun sym line ->
      try
        let len = String.length sym in
        if len <= prefixlen || String.sub sym 0 prefixlen <> prefix then
          raise Exit;
        for i = prefixlen to len-1 do
          match sym.[i] with
            '0'..'9' -> ()
          | _ -> raise Exit
        done;
            (* MATCH *)
        Printf.eprintf "%s matches: setting %s:%d\n%!" sym
          cu.cu_basename line;
        let main_bp = SBTarget.breakpointCreateByLocation target
          cu.cu_basename              line in
        let nlocs = SBBreakpoint.getNumLocations main_bp in
        let id = SBBreakpoint.getID main_bp in
        Printf.printf "Breakpoint %d set on %d locations\n%!"
          id nlocs;
        let _locs =
          Array.init nlocs
            (fun i ->
              let addr = SBBreakpointLocation.getAddress
                (SBBreakpoint.getLocationAtIndex main_bp i) in
              let sym = SBAddress.getSymbol addr in
              Printf.printf "    (%d.%d) %s\n%!"
                id (1+i) (SBSymbol.getName sym))
        in
        ()
      with Exit -> ()
    ) cu.cu_symbols
  with Not_found ->
    Printf.eprintf "Error: could not find module %S\n%!" modname

let ocaml_gcstats debugger =
    let target = SBDebugger.getSelectedTarget debugger in
    let s = LLDBOCamlGcstats.compute_gc_stats target in
    LLDBOCamlGcstats.printf s


let ocaml_modules debugger =
  let target = SBDebugger.getSelectedTarget debugger in
  let ima = LLDBOCamlCode.get_compilation_units target in
  let units = ref [] in
  let nunits = ref 0 in
  Array.iter (fun cu ->
    match cu.cu_modname with
    | None -> ()
    | Some modname ->
      units := modname :: !units;
      incr nunits
  ) ima.ima_cus;
  Printf.printf "Found %d OCaml modules in executable\n" !nunits;
  List.iteri (fun i modname ->
    Printf.printf "[%3d] %s\n" i modname
  ) !units;
      (*
    (* These ones are only statically linked modules *)
        | [| "modules" |] ->
        let target = SBDebugger.getSelectedTarget debugger in
        let modules = LLDBOCamlCode.get_modules target in
        Printf.printf "%d statically linked modules:\n" (Array.length modules);
        Array.iteri (fun i { mod_name; mod_addr; mod_symbol } ->
        Printf.printf "module[%d] %s (addr = 0x%Lx, symbol = %s)\n%!" i
        mod_name mod_addr mod_symbol;
        ) modules
      *)
  ()

let ocaml_globals_map debugger =
  let target = SBDebugger.getSelectedTarget debugger in
  let (inited, globals_map) = LLDBOCamlGlobals.load_globals_map target in
  Array.iteri (fun i (modname, crc1, crc2, deps) ->
    Printf.printf "[%d] %c %-30s %s %s %s\n" i
      (if i = inited then '*' else '.')
      modname (Digest.to_hex crc1) (Digest.to_hex crc2)
      (String.concat "," deps)
  ) globals_map;
  ()

let ocaml_current debugger =
  let target = SBDebugger.getSelectedTarget debugger in
  let (inited, globals_map) = LLDBOCamlGlobals.load_globals_map target in
  for i = 0 to inited-1 do
    let (modname, _, _, _) = globals_map.(i) in
    Printf.printf "%s " modname;
  done;
  let (modname, _, _, _) = globals_map.(inited) in
  Printf.printf "[%s]\n" modname;
  ()

let ocaml_code debugger =
  let target = SBDebugger.getSelectedTarget debugger in
  let code_fragments = LLDBOCamlCode.get_code_info target in
  Array.iteri (fun i { code_start; code_end } ->
    Printf.printf "%d -> 0x%Lx - 0x%Lx (%Ld bytes)\n"
      i code_start code_end (Int64.sub code_end code_start)
  ) code_fragments;
  Printf.printf "%!";
  ()

let ocaml_heap debugger =
  let target = SBDebugger.getSelectedTarget debugger in
  let heap = LLDBOCamlHeap.get_heap_info target in
  Printf.printf "minor heap: 0x%Lx - 0x%Lx (size %Ld kB)\n%!"
    heap.caml_young_start
    heap.caml_young_end
    (Int64.div (Int64.sub heap.caml_young_end heap.caml_young_start) 1024L);
  let sizeL = ref 0L in
  List.iter (fun (chunk_begin, chunk_end) ->
    sizeL := Int64.add !sizeL (Int64.sub chunk_end chunk_begin)
  ) heap.caml_chunks;
  Printf.printf "major heap: size %Ld kB\n" (Int64.div !sizeL 1024L);
  List.iter (fun (chunk_begin, chunk_end) ->
    Printf.printf "   0x%Lx - 0x%Lx (size %Ld kB)\n%!"
      chunk_begin chunk_end
      (Int64.div (Int64.sub chunk_end chunk_begin) 1024L)
  ) heap.caml_chunks

let ocaml_targets debugger =
    let target = SBDebugger.getSelectedTarget debugger in
    let nm = SBTarget.getNumModules target in
    Printf.printf "%d components in executable\n%!" nm;
    let _modules = Array.init nm (fun i ->
      let m = SBTarget.getModuleAtIndex target i in
      Printf.printf "component %S\n" (SBModule.to_string m);
      let n = SBModule.getNumCompileUnits m in
      let _cus = Array.init n (fun i ->
        SBModule.getCompileUnitAtIndex m i
      ) in
      Printf.printf "%d compilation units in this component\n%!" n
    ) in
    ()

let ocaml_target debugger =
  let target = SBDebugger.getSelectedTarget debugger in
  let ima = LLDBOCamlCode.get_compilation_units target in
  Printf.printf "%d compilation units in this component\n%!"
    (Array.length ima.ima_cus);
  Array.iteri (fun i cu ->
    match cu.cu_modname with
    | None ->
      Printf.printf "{\n";
      Printf.printf "  cu_descr = %S\n" cu.cu_descr;
      Printf.printf "  cu_basename = %S\n" cu.cu_basename;
      Printf.printf "  cu_dirname = %S\n" cu.cu_dirname;
      Printf.printf "  cu_symbols = { %d symbols }\n"
        (StringMap.cardinal cu.cu_symbols);
      Printf.printf "}\n";
    | Some modname ->
      Printf.printf "%3d -> %s\n%!" i modname;
      Printf.printf "  %s ... %s\n" cu.cu_dirname cu.cu_basename;
      StringMap.iter (fun name line ->
        Printf.printf "  %s -> %s:%d\n"
          name cu.cu_basename line
      ) cu.cu_symbols;
  ) ima.ima_cus;
  Printf.printf "%!"

let ocaml_target_narg1 debugger modname =
  let target = SBDebugger.getSelectedTarget debugger in
  let ima = LLDBOCamlCode.get_compilation_units target in
  begin
    try
      let cu = StringMap.find modname ima.ima_cus_by_name in
      Printf.printf "  %s ... %s\n" cu.cu_dirname cu.cu_basename;
      StringMap.iter (fun name line ->
        Printf.printf "  %s -> %s:%d\n"
          name cu.cu_basename line
      ) cu.cu_symbols;
      Printf.printf "%!"
    with Not_found ->
      Printf.printf "Compilation unit not found\n%!"
  end

let ocaml_paths debugger =
  let target = SBDebugger.getSelectedTarget debugger in
  let ima = LLDBOCamlCode.get_compilation_units target in
  let paths = ref StringMap.empty in
  Array.iter (fun cu ->
    try
      let r = StringMap.find cu.cu_dirname !paths in
      r := cu.cu_basename :: !r
    with Not_found ->
      paths :=
        StringMap.add cu.cu_dirname (ref [cu.cu_basename]) !paths
  ) ima.ima_cus;
  StringMap.iter (fun path files ->
    Printf.printf "Path: %s\n%!" path
  ) !paths;
  Printf.printf "You can use 'settings set target.source-map PATH1 PATH2'\n";
  Printf.printf "to translate these paths to yours.\n%!"

let ocaml_print_narg1 debugger addr =
  let target = SBDebugger.getSelectedTarget debugger in
  let process = SBTarget.getProcess target in
  let addr = LLDBUtils.int64_of_string addr in
  let sbError = SBError.create () in
  let nRead = SBProcess.readMemory process addr buffer 8 sbError in
  if !verbose then
    Printf.printf "%d bytes read from memory\n%!" nRead;

  let heap = LLDBOCamlHeap.get_heap_info target in
  let mem = LLDBOCamlHeap.get_memory_info target in
  LLDBOCamlValue.print_value target mem heap addr [];
  ()

#ifndef OCAML_NON_OCP

let ocaml_locids debugger =
  let target = SBDebugger.getSelectedTarget debugger in
  let _locs = LLDBOCamlLocids.load target in
  Printf.printf "Location table loaded.\n%!"

let ocaml_globals_narg1 debugger modname =
  let target = SBDebugger.getSelectedTarget debugger in
  let heap = LLDBOCamlHeap.get_heap_info target in
  let mem = LLDBOCamlHeap.get_memory_info target in
  LLDBOCamlGlobals.print_module_globals target mem heap modname;
  ()

let ocaml_globals debugger =
  let target = SBDebugger.getSelectedTarget debugger in
  LLDBOCamlGlobals.print_globals target

let ocaml_global_narg1 debugger global =
  let (modname, ident) =
    try
      let pos = String.index global '.' in
      let modname = String.sub global 0 pos in
      let funname = String.sub global (pos+1)
        (String.length global -pos-1) in
      (modname, funname)
    with _ ->
      Printf.kprintf failwith "%S is not a fully-qualified function"
        global
  in

  let target = SBDebugger.getSelectedTarget debugger in
  let heap = LLDBOCamlHeap.get_heap_info target in
  let mem = LLDBOCamlHeap.get_memory_info target in
  LLDBOCamlGlobals.print_module_global target mem heap modname ident;
  ()

let ocaml_args_narg1 debugger nargs =
  let nargs = try
                int_of_string nargs
    with _ -> failwith "Argument of 'args' should be a number"
  in
  let target = SBDebugger.getSelectedTarget debugger in
  let heap = LLDBOCamlHeap.get_heap_info target in
  let mem = LLDBOCamlHeap.get_memory_info target in
  LLDBOCamlStack.print_args target mem heap nargs

let ocaml_reg_narg1 debugger reg =
  let target = SBDebugger.getSelectedTarget debugger in
  let heap = LLDBOCamlHeap.get_heap_info target in
  let mem = LLDBOCamlHeap.get_memory_info target in
  LLDBOCamlStack.print_reg target mem heap reg;
  ()

#endif

let ocaml_set variable values =
  try
    match variable, values with
    | "max_indent", [ v ] -> LLDBOCamlValue.max_indent := int_of_string v
    | _, _ -> raise Not_found
  with exn ->
    Printf.eprintf "Exception %S while setting variable %S\n%!"
      (Printexc.to_string exn) variable

let ocaml_command debugger args =
  match Array.to_list args with
  | [ ("break" | "b"); funname ] -> ocaml_break_narg1 debugger funname
  | [ "debug" ] ->
    verbose := true;
    Printf.printf "Debug mode set\n%!"

  | "set" :: variable :: values -> ocaml_set variable values
  | [ "modules" ] -> ocaml_modules debugger
  | [ "globals_map" ] -> ocaml_globals_map debugger
  | [ "current" ] -> ocaml_current debugger
  | [ "gcstats" ] -> ocaml_gcstats debugger
  | [ "code" ] -> ocaml_code debugger
  | [ "heap" ] -> ocaml_heap debugger
  | [ "targets" ] -> ocaml_targets debugger
  | [ "target" ] -> ocaml_target debugger
  | [ "target"; modname ] -> ocaml_target_narg1 debugger modname
  | [ "paths" ] -> ocaml_paths debugger
  | [ "print"; addr ] -> ocaml_print_narg1 debugger addr

#ifndef OCAML_NON_OCP
  | [ "locids" ] -> ocaml_locids debugger
  | [ "globals"; modname ] -> ocaml_globals_narg1 debugger modname
  | [ "globals" ] -> ocaml_globals debugger
  | [ "global"; global ] -> ocaml_global_narg1 debugger global
  | [ "args"; nargs ] -> ocaml_args_narg1 debugger nargs
  | [ "reg"; reg ] -> ocaml_reg_narg1 debugger reg
#endif

  | [ "help" ] | [] ->
    Printf.printf "Help on OCaml commands:\n";
    Printf.printf "  'ocaml modules': list OCaml modules in executable\n%!";
    Printf.printf "  'ocaml globals_map': extract and display globals_map\n%!";
    Printf.printf "  'ocaml current': list evaluated modules until now\n%!";
    Printf.printf "  'ocaml break Module.function': set a breakpoint on an OCaml function\n%!";
    Printf.printf "  'ocaml heap' : print heap information\n%!";
    Printf.printf "  'ocaml gcstats' : print GC stats\n%!";
    Printf.printf "  'ocaml code' : print code segments\n%!";
    Printf.printf "  'ocaml targets' : print information on binary components\n%!";
    Printf.printf "  'ocaml paths' : print paths to sources\n%!";
    Printf.printf "  'ocaml args NARGS' : print NARGS first arguments\n";
    Printf.printf "  'ocaml reg RAX' : print register as an OCaml value\n";
    Printf.printf "  'ocaml globals MODULE' : print all globals of MODULE\n";
    Printf.printf "  'ocaml global MODULE.VALUE' : print fully-qualified global\n";

    Printf.printf "  'ocaml target Module' : print target info on Module\n%!";

    Printf.printf "  'ocaml print 0xFFF' : print information on value at 0xFFF\n%!";
    Printf.printf "  'ocaml target' : print target info\n%!";

    Printf.printf "%!"

  | _ ->
    Printf.printf "No such command:\n";
    Array.iteri (fun i cmd ->
      Printf.printf "[%d] -> %S\n%!" i cmd
    ) args;
    Printf.printf "Use 'ocaml help' for help\n%!"

let ocaml_command debugger args =
  try
    ocaml_command debugger args;
    Printf.printf "%!" (* flush output *)
  with e ->
    Printf.printf "ocaml_command: exception %s\n%!"
      (Printexc.to_string e)
