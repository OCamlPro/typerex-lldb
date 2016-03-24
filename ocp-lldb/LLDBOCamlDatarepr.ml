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


open StringCompat

(* LLDB API *)
open LLDBEnums
open LLDBOCaml


(* ocp-lldb modules *)
open LLDBTypes

let parse_header mem header =
  if mem.standard_header then
    (* standard parsing of a 64 bit header *)
    let gc = Int64.to_int (Int64.shift_right header 8) in
    let gc = gc land 0x3 in
    let wosize = Int64.to_int (Int64.shift_right header 10) in
    let tag =  Int64.to_int (Int64.logand header 0xFFL) in
    let locid = None in
    { tag; wosize; gc; locid }
  else
    (* LOCID parsing of a 64 bit header *)
    let gc = Int64.to_int (Int64.shift_right header 8) in
    let gc = gc land 0x3 in
    let wosize = Int64.to_int (Int64.shift_right header 32) in
    let locid = Int64.to_int
       (Int64.shift_right (Int64.logand header 0xFFFFFFFFL) 10) in
    let tag =  Int64.to_int (Int64.logand header 0xFFL) in
    let locid = Some locid in
    { tag; wosize; gc; locid }

let parse_string_length process h addr =
  assert (h.tag = 252);
  let trailer = LLDBUtils.getMem64 process
    (Int64.add addr
       (Int64.of_int
          (h.wosize*8 - 8))) in
            (* Printf.printf "trailer: 0x%08Lx\n" trailer; *)
  h.wosize*8-1 -
    (Int64.to_int (Int64.shift_right trailer 56))

let load_header process mem addr =
  parse_header mem (LLDBUtils.getMem64 process (Int64.sub addr 8L))

let load_string process mem addr =
  let h =
      parse_header mem (LLDBUtils.getMem64 process (Int64.sub addr 8L))
  in
  let len = parse_string_length process h addr in
  let b = Bytes.create len in
  let nRead = SBProcess.readMemory process addr b len LLDBUtils.sbError in
  if nRead < len then failwith "Could not load string from memory";
  Bytes.to_string b
