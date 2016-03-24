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


#ifndef OCAML_NON_OCP
open StringCompat

(* LLDB API *)
open LLDBEnums
open LLDBOCaml


(* ocp-lldb modules *)
open LLDBTypes
open LLDBGlobals

open GcprofLocations

  (*
    int caml_ocp_location_tables_max = 0;
    int caml_ocp_next_location_id = 0;
    char **caml_ocp_location_tables = NULL;
    mlsize_t *caml_ocp_location_tables_sizes = NULL;
    int caml_ocp_location_tables_next = 0;
  *)

let load target =
  Printf.eprintf "Loading locid information...\n%!";
  let location_tables_addr = LLDBUtils.symbol_address target
    "caml_ocp_location_tables" in
  let location_table_sizes_addr = LLDBUtils.symbol_address target
    "caml_ocp_location_tables_sizes" in
  let location_tables_size_addr = LLDBUtils.symbol_address target
    "caml_ocp_location_tables_next" in

  (*
    Printf.printf "location_tables_addr= 0x%Lx\n%!" location_tables_addr;
    Printf.printf "location_table_sizes_addr= 0x%Lx\n%!" location_table_sizes_addr;
    Printf.printf "location_tables_size_addr= 0x%Lx\n%!" location_tables_size_addr;
  *)
  let process = SBTarget.getProcess target in
  let size = Int64.to_int (LLDBUtils.getMem64 process location_tables_size_addr) in
  if !LLDBGlobals.verbose then
    Printf.printf "location table size=%d\n%!" size;


  let location_tables_ptr = LLDBUtils.getMem64 process location_tables_addr in
  let location_table_sizes_ptr = LLDBUtils.getMem64 process location_table_sizes_addr in
  let offset = ref 0 in
  let location_tables = Array.init size (fun i ->
    let addr = LLDBUtils.getMem64 process
      (Int64.add location_tables_ptr
         (Int64.of_int (8 * i))) in
    let size = Int64.to_int (LLDBUtils.getMem64 process
                               (Int64.add location_table_sizes_ptr
                                  (Int64.of_int (8 * i)))) in
    let buffer = Bytes.create size in
    let nRead = SBProcess.readMemory process addr buffer size LLDBUtils.sbError in
    if !LLDBGlobals.verbose then
      Printf.printf "table[%d]: read %d/%d\n%!" i nRead size;
    let (loctbl : Memprof.table_desc) = Marshal.from_bytes buffer 0 in
    Memprof.(begin
      if !LLDBGlobals.verbose then
        Printf.printf "Location table %d:\n%!" i;
      match loctbl with
      | Indirect _ ->
        if !LLDBGlobals.verbose then
          Printf.printf "  Indirect\n%!"
      | V0 m ->
        let size = Array.length m.V0.loc_tbl in
        if !LLDBGlobals.verbose then
          Printf.printf "  V0 [%d-%d[\n%!" !offset (!offset+size);
        offset := !offset + size
      | Locid l ->
        if !LLDBGlobals.verbose then
          Printf.printf "  Locid %d\n%!" !offset; incr offset
      | V1 m ->
        let size = Array.length m.V1.loc_tbl in
        if !LLDBGlobals.verbose then begin
          Printf.printf "  V1 [%d-%d[\n%!" !offset (!offset+size);
          Printf.printf "  V1 { locid_base=%S modid=%S }\n%!"
          m.V1.locid_base (Ident.name m.V1.modid)
        end;
        offset := !offset + size;
    end);
    loctbl
  ) in
  let loctbl = GcprofLocations.unmarshal_locs
    (Array.to_list location_tables) in
  Printf.eprintf "Loading locid information...DONE\n%!";
  Printf.eprintf "  Location tables: %d entries\n%!"
    (Array.length location_tables);
  Printf.eprintf "  Locids: %d\n%!"
    (Array.length loctbl.GcprofTypes.locs_info);
  (location_tables, loctbl)

let load = LLDBUtils.get_cached_with_symbol_check load
  "caml_ocp_location_tables_next"

#endif
