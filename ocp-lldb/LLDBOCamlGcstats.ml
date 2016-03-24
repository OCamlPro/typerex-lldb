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


open LLDBEnums
open LLDBOCaml

open LLDBTypes

let compute_gc_stats target =
  let caml_stat_minor_words = LLDBUtils.double_symbol_value target "caml_stat_minor_words" in
  let promoted_words = LLDBUtils.double_symbol_value target "caml_stat_promoted_words" in
  let caml_stat_major_words = LLDBUtils.double_symbol_value target "caml_stat_major_words" in
  let minor_collections = LLDBUtils.long_symbol_value target "caml_stat_minor_collections" in
  let major_collections = LLDBUtils.long_symbol_value target "caml_stat_major_collections" in
  let caml_young_end = LLDBUtils.long_symbol_value target "caml_young_end" in
  let caml_young_ptr = LLDBUtils.long_symbol_value target "caml_young_ptr" in
  let caml_allocated_words = LLDBUtils.long_symbol_value target "caml_allocated_words" in
  let compactions = LLDBUtils.long_symbol_value target "caml_stat_compactions" in
  let heap_chunks = LLDBUtils.long_symbol_value target "caml_stat_heap_chunks" in

  let heap_words =
    try LLDBUtils.long_symbol_value target "caml_stat_heap_wsz" with _ -> 0L in
  let top_heap_words =
    try LLDBUtils.long_symbol_value target "caml_stat_top_heap_wsz" with _ -> 0L in

  let minor_words =
    caml_stat_minor_words +.
    (Int64.to_float (Int64.div (Int64.sub caml_young_end caml_young_ptr) 8L)) in
  let major_words =
    caml_stat_major_words +.
    Int64.to_float caml_allocated_words in
  let minor_collections = Int64.to_int minor_collections in
  let major_collections = Int64.to_int major_collections in
  let heap_words = Int64.to_int heap_words in
  let top_heap_words = Int64.to_int top_heap_words in
  let heap_chunks = Int64.to_int heap_chunks in
  let compactions = Int64.to_int compactions in

  let stack_size = 0 in
  let live_words = 0 in
  let live_blocks = 0 in
  let free_words = 0 in
  let free_blocks = 0 in
  let largest_free = 0 in
  let fragments = 0 in
  Gc.(
    { minor_words;
      promoted_words;
      major_words;
      minor_collections;
      major_collections;
      heap_words;
      heap_chunks;
      live_words;
      live_blocks;
      free_words;
      free_blocks;
      largest_free;
      fragments;
      compactions;
      top_heap_words;
      stack_size }
  )

let printf s =
  Gc.(
    Printf.printf "minor_words: %.0f\n" s.minor_words;
    Printf.printf "promoted_words: %.0f\n" s.promoted_words;
    Printf.printf "major_words: %.0f\n" s.major_words;
    Printf.printf "minor_collections: %d\n" s.minor_collections;
    Printf.printf "major_collections: %d\n" s.major_collections;
    Printf.printf "heap_words: %d\n" s.heap_words;
    Printf.printf "heap_chunks: %d\n" s.heap_chunks;
    Printf.printf "top_heap_words: %d\n" s.top_heap_words;
    Printf.printf "compactions: %d\n" s.compactions;
    flush stdout
  )
