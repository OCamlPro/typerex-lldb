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
#endif

open LLDBEnums
open LLDBOCaml

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

(* This assumes that the ranges have at least a size of 1 *)
let compare_range  (x1,y1) (x2,y2) =
  if x1 < x2 && y1 <= x2 then -1 else
    if x2 < x1 && y2 <= x1 then 1 else 0

module ChunkSet = Set.Make(struct
  type t = int64*int64
  let compare = compare_range
end)

module ChunkMap = Map.Make(struct
  type t = int64*int64
  let compare = compare_range
end)

type heap_info = {
  caml_young_start : int64;
  caml_young_end : int64;
  caml_heap_start : int64;
  caml_chunks : (int64 * int64) list;
  caml_major_heap : ChunkSet.t;
}

type header = {
  wosize : int;
  gc : int;
  tag : int;
  locid : int option;
}

let buffer = Bytes.create 8

  (*
type module_info = {
  mod_symbol : string;
  mod_name : string;
  mod_addr : int64;
}
  *)

  (*
struct code_fragment {
  char * code_start;
  char * code_end;
  unsigned char digest[16];
  char digest_computed;
};
  *)
(*
struct ext_table {
  int size;
  int capacity;
  void ** contents;
};
*)

type code_fragment = {
  code_start : int64;
  code_end : int64;
}

type mem_info = {
  standard_header : bool;
  code_fragments : string ChunkMap.t;
  data_fragments : string ChunkMap.t;

}

type compilation_unit = {
  cu_modname : string option;
  cu_descr : string;
  cu_basename : string;
  cu_dirname : string;
  cu_symbols : int StringMap.t;
}

type image = {
  ima_cus : compilation_unit array;
  ima_cus_by_name : compilation_unit StringMap.t;
}

type pointer_kind =
| Integer
| MinorAddress
| MajorAddress
| ModuleCode of string
| ModuleData of string
| External
