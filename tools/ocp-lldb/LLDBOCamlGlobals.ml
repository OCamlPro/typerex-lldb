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

open Types


#ifndef OCAML_NON_OCP

open GcprofTypes
open Memprof

let map_of_globals target =
  let (loctbls, _locs) = LLDBOCamlLocids.load target in
  let map = ref StringMap.empty in
  Array.iter (function
  | Indirect _
  | V0 _
  | Locid _ -> ()
  | V1 m ->
    map := StringMap.add (Ident.name m.V1.modid) m !map
  ) loctbls;
  !map

let array_reviter f t =
  for i = Array.length t - 1 downto 0 do
    f t.(i)
  done

let array_reviteri f t =
  for i = Array.length t - 1 downto 0 do
    f i t.(i)
  done

let map_of_symbols symbols =
  let map = ref StringMap.empty in
  array_reviteri (fun i s ->
    if not (String.contains s '(') then
      try
        let pos = String.rindex s '_' in
        let sym = String.sub s 0 pos in
        map := StringMap.add sym i !map
      with Not_found -> ()
  ) symbols;
  !map

let print_globals target =
  let modules = map_of_globals target in
  StringMap.iter (fun name m ->
    Printf.printf "%s: %s\n" name
      (String.concat ", " (Array.to_list m.V1.globals));
  ) modules;
  Printf.printf "%!"

let print_module_globals target mem heap modname =
  let process = SBTarget.getProcess target in
  let modules = map_of_globals target in
  try
    let m = StringMap.find modname modules in
    let map = map_of_symbols m .V1.globals in
    (* TODO: fix mangling for module names *)
    let modaddr = LLDBUtils.symbol_address target ("caml"^modname) in

    StringMap.iter (fun name i ->
      Printf.printf "  %s.%s -> %d\n%!" modname name i;
      let addr = Int64.add modaddr (Int64.of_int (i*8)) in
      let v = LLDBUtils.getMem64 process addr in
      LLDBOCamlValue.print_value target mem heap v [];
    ) map
  with Not_found ->
    Printf.printf "Error: could not find locations for module %S\n%!" modname

let print_module_global target mem heap modname ident =
  let process = SBTarget.getProcess target in
  let modules = map_of_globals target in
  try
    let m = StringMap.find modname modules in
    let map = map_of_symbols m .V1.globals in
    try
    let i = StringMap.find ident map in
    (* TODO: fix mangling for module names *)
    let modaddr = LLDBUtils.symbol_address target ("caml"^modname) in
    Printf.printf "  %s.%s -> %d\n%!" modname ident i;
    let addr = Int64.add modaddr (Int64.of_int (i*8)) in
    let v = LLDBUtils.getMem64 process addr in
    LLDBOCamlValue.print_value target mem heap v [];
    with Not_found ->
      Printf.printf "Error: could not find ident %s in module %S\n%!" ident modname

  with Not_found ->
    Printf.printf "Error: could not find locations for module %S\n%!" modname

#endif



let load_globals_map target =
  let caml_globals_map_addr = LLDBUtils.symbol_address target
    "caml_globals_map" in
  (*  let heap = LLDBOCamlHeap.get_heap_info target in *)
  let mem = LLDBOCamlHeap.get_memory_info target in
  let process = SBTarget.getProcess target in
  let caml_globals_map = LLDBOCamlDatarepr.load_string process mem
    caml_globals_map_addr in
  let (globals_map :  (string * Digest.t * Digest.t * string list) list) =
    Marshal.from_string caml_globals_map 0 in
  let inited = LLDBUtils.long_symbol_value target "caml_globals_inited" in
  (Int64.to_int inited, Array.of_list globals_map)
