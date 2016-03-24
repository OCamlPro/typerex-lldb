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

let get_code_info target =
  let process = SBTarget.getProcess target in
  let caml_code_fragments_table =
    LLDBUtils.symbol_address target "caml_code_fragments_table" in
  let size = LLDBUtils.getMem32 process caml_code_fragments_table in
  let _capacity = LLDBUtils.getMem32 process
    (Int64.add caml_code_fragments_table 4L) in
  let contents = LLDBUtils.getMem64 process
    (Int64.add caml_code_fragments_table 8L) in
  Printf.printf "size = %d\n" size;
  let code_fragments = Array.init size (fun i ->
    let cf = LLDBUtils.getMem64 process
      (Int64.add contents (Int64.of_int (i*8))) in
    let code_start = LLDBUtils.getMem64 process cf in
    let code_end = LLDBUtils.getMem64 process (Int64.add cf 8L) in
    { code_start; code_end }
  ) in
  code_fragments

    (*
(* Here, we only find statically linked modules. In fact, we could probably
   use the code_fragments to find dynamically allocated modules. *)

let get_modules = LLDBUtils.get_cached (fun target ->
  let modules = ref [] in
  let syms = SBTarget.findSymbols target "caml_program"
    ESymbolTypeCode in
  let syms = SBSymbolContextList.to_array syms in
  Array.iter (fun sc ->
    let sym = SBSymbolContext.getSymbol sc in
    let _name = SBSymbol.getName sym in
    (*    Printf.printf "name=%S\n%!" name; *)

    let insts = SBSymbol.getInstructions sym target in
    let insts = SBInstructionList.to_array insts in
    Array.iteri (fun i ins ->
      let mne = SBInstruction.getMnemonic ins target in
      let ope = SBInstruction.getOperands ins target in
        (*          Printf.printf "i: %S %S\n%!" mne ope; *)
      match mne with
      | "callq" ->
        let addr = LLDBUtils.int64_of_string ope in
        let print_symbol sym =
          let name = SBSymbol.getName sym in
            (*            Printf.printf "sym = %S\n%!" name; *)
          let mod_name =
            let len = String.length name in
            if len > 10 &&
              name.[0] = 'c' &&
              name.[1] = 'a' &&
              name.[2] = 'm' &&
              name.[3] = 'l' then
              if Filename.check_suffix name "__entry" then
                String.sub name 4 (len - 11)
              else
                if Filename.check_suffix name "__code_begin" then
                  String.sub name 4 (len - 16)
                else
                  name
            else name
          in
          modules := {
            mod_name;
            mod_addr = addr;
            mod_symbol = name;
          } :: !modules

        in
        let symaddr = SBTarget.resolveLoadAddress target addr in
        let sym = SBAddress.getSymbol symaddr in

        if SBSymbol.isValid sym then
          print_symbol sym
        else
            (* Find the executable module so we can do a lookup
               inside it *)
          let  exe_file_spec = SBFileSpec.createByName
            (SBFileSpec.getFilename (SBTarget.getExecutable target)) true in
          let modul =
            SBTarget.findModule target exe_file_spec in

            (* // Take a file virtual address and resolve it to a
               section offset // address that can be used to do a
               symbol lookup by address *)
          let addr = SBModule.resolveFileAddress modul addr in
          let success = SBAddress.isValid addr  &&
            SBSection.isValid (SBAddress.getSection addr) in
          if success then begin
              (* // We can resolve a section offset address in the
                 module // and only ask for what we need. You can
                 logical or together // bits from the
                 SymbolContextItem enumeration found in //
                 lldb-enumeration.h to request only what you
                 want. Here we // are asking for everything.  // //
                 NOTE: the less you ask for, the less LLDB will parse
                 as // LLDB does partial parsing on just about
                 everything.  *)
            let sc = SBModule.resolveSymbolContextForAddress
              modul addr eSymbolContextEverything in
            let sym = SBSymbolContext.getSymbol sc in
            print_symbol sym;
          end;
          ()

      | _ -> ()
    ) insts
  ) syms;
  let modules = List.rev !modules in
  Array.of_list modules)
    *)

(* Assumptions:
   * OCaml modules are compiled from .ml files.
*)

let get_compilation_units = LLDBUtils.get_cached (fun target ->
  let cus_by_name = ref StringMap.empty in
  let exe_file_spec = SBFileSpec.createByName
    (SBFileSpec.getFilename (SBTarget.getExecutable target)) true in
  let m = SBTarget.findModule target exe_file_spec in
  let n = SBModule.getNumCompileUnits m in
  let cus = Array.init n (fun i ->
    SBModule.getCompileUnitAtIndex m i
  ) in
  Printf.printf "%d compilation units in this component\n%!" n;
  let cus = Array.mapi (fun i cu ->
    (*    Printf.printf "%3d -> %s\n%!" i (SBCompileUnit.to_string cu); *)
    let f = SBCompileUnit.getFileSpec cu in
    let cu_basename = SBFileSpec.getFilename f in
    let cu_dirname = SBFileSpec.getDirectory f in
    if !verbose then begin
      Printf.printf "  %s ... %s\n" cu_dirname cu_basename;
    end;
    let nl = SBCompileUnit.getNumLineEntries cu in
    let les = Array.init nl (fun j ->
      SBCompileUnit.getLineEntryAtIndex cu j
    ) in
    let symbols = ref StringMap.empty in
    Array.iteri (fun j le ->
      let fs = SBLineEntry.getFileSpec le in
      let fsname = SBFileSpec.getFilename fs in
      (* Check that this line belongs to this file, and not to some code
         inlined from another file: *)
      if cu_basename = fsname then
        let line = SBLineEntry.getLine le in
        let col = SBLineEntry.getColumn le in
        let addr = SBLineEntry.getStartAddress le in
        let sym = SBAddress.getSymbol addr in
        match
          try Some (SBSymbol.getName sym) with _ -> None
        with
        | None -> ()
        | Some name ->
        (*        let f = SBAddress.getFunction addr in
                  let name2 = try SBFunction.getName f with _ -> "" in  *)
          if !verbose then begin
            Printf.printf "%s:%d:%d %Ld -> %s\n" fsname line col
              (SBAddress.getLoadAddress addr target) name ;
          end;
          try
            let _ref_line = StringMap.find name !symbols in
          (*  Just keep the first one !
              if !ref_line > line then ref_line := line *)
            ()
          with Not_found ->
            symbols := StringMap.add name (ref line) !symbols
    ) les;
    let cu_symbols = StringMap.map (fun r -> !r) !symbols in
    let cu_descr = SBCompileUnit.to_string cu in
    let cu_modname =
      if Filename.check_suffix cu_basename ".ml" then begin
        let modname = String.capitalize (
          Filename.chop_extension cu_basename) in
        Some modname
      end else None in
    let cu ={
      cu_modname;
      cu_descr;
      cu_basename;
      cu_dirname;
      cu_symbols;
    } in
    begin
      match cu_modname with
        None -> ()
      | Some cu_modname ->
        cus_by_name := StringMap.add cu_modname cu !cus_by_name
    end;
    cu
  ) cus
  in
  {
    ima_cus = cus;
    ima_cus_by_name = !cus_by_name;
  }
)
