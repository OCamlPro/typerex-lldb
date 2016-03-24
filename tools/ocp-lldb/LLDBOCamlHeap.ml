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

let get_heap_info target =
  let caml_heap_start = LLDBUtils.long_symbol_value target "caml_heap_start" in
  let caml_young_end = LLDBUtils.long_symbol_value target "caml_young_end" in
  let caml_young_start = LLDBUtils.long_symbol_value target "caml_young_start" in
  if !verbose then begin
    Printf.printf "caml_heap_start = 0x%Lx\n" caml_heap_start;
    Printf.printf "caml_young_end = 0x%Lx\n" caml_young_end;
    Printf.printf "caml_young_start = 0x%Lx\n" caml_young_start;
  end;

  let process = SBTarget.getProcess target in
  let rec read_chunks chunk caml_chunks caml_major_heap =
    if !verbose then Printf.printf "chunk = 0x%Lx\n" chunk;
    if chunk <> 0L then
      let next = LLDBUtils.getMem64 process (Int64.sub chunk 8L) in
      if !verbose then Printf.printf "chunk[-8].next = 0x%Lx\n" next;

      let chunk_size =
        let next = LLDBUtils.getMem64 process (Int64.sub chunk 16L) in
        if !verbose then Printf.printf "chunk[-16].size = 0x%Lx\n" next;
        next
      in

      begin
        let next = LLDBUtils.getMem64 process (Int64.sub chunk 24L) in
        if !verbose then Printf.printf "chunk[-24].alloc = 0x%Lx\n" next;
      end;

      begin (* chunk_block *)
        let next = LLDBUtils.getMem64 process (Int64.sub chunk 32L) in
        if !verbose then Printf.printf "chunk[-32].block = 0x%Lx\n" next;
      end;
      let chunk_end = Int64.add chunk chunk_size in
      read_chunks next ( (chunk, chunk_end) :: caml_chunks )
        (ChunkSet.add (chunk, chunk_end) caml_major_heap)
    else (caml_chunks, caml_major_heap)
  in
  let (caml_chunks, caml_major_heap) =
    read_chunks caml_heap_start [] ChunkSet.empty
  in
  if !verbose then Printf.printf "\n%!";
  {
    caml_young_start;
    caml_young_end;
    caml_major_heap;
    caml_chunks;
    caml_heap_start;
  }

let get_memory_info target =

  let standard_header =
    try
      let _caml_memprof_register_table =
        LLDBUtils.symbol_address target "caml_memprof_register_table" in
      false
    with _ -> true
  in

  let ima = LLDBOCamlCode.get_compilation_units target in
  let code_fragments = ref ChunkMap.empty in
  let data_fragments = ref ChunkMap.empty in
  Array.iteri (fun i cu ->
    match cu.cu_modname with
    | None -> ()
    | Some name ->
      let code_begin = LLDBUtils.symbol_address target (
        Printf.sprintf "caml%s__code_begin" name) in
      let code_end = LLDBUtils.symbol_address target (
        Printf.sprintf "caml%s__code_end" name) in
      let data_begin = LLDBUtils.symbol_address target (
        Printf.sprintf "caml%s__data_begin" name) in
      let data_end = LLDBUtils.symbol_address target (
        Printf.sprintf "caml%s__data_end" name) in
      code_fragments := ChunkMap.add (code_begin, code_end) name !code_fragments;
      data_fragments := ChunkMap.add (data_begin, data_end) name !data_fragments;
      if !verbose then begin
        Printf.printf "module[%d] : %s -> code 0x%Lx - 0x%Lx (%Ld bytes)\n"
          i name code_begin code_end
          (Int64.sub code_end code_begin);
      end;
  ) ima.ima_cus;
  if !verbose then begin
    ChunkMap.iter (fun (code_begin, code_end) name ->
      Printf.printf "module[?] : %s -> code 0x%Lx - 0x%Lx\n"
        name code_begin code_end;
    ) !code_fragments;
    Printf.printf "Code fragments: %d\n%!" (ChunkMap.cardinal !code_fragments);
    Printf.printf "Data fragments: %d\n%!" (ChunkMap.cardinal !data_fragments);
  end;
  {
    standard_header;
    code_fragments = !code_fragments;
    data_fragments = !data_fragments;
  }
