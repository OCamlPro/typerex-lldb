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
open MassifTypes

let rec fprintf_tree oc indent { part; position; children } =
  for i = 1 to indent do output_char oc ' '; done;
  let nchildren = Array.length children in
  Printf.fprintf oc "n%d: %Ld %s\n" nchildren part position;
  Array.iter (fun child ->
    fprintf_tree oc (indent+1) child
  ) children

let to_file filename { header; snapshots } =
  let oc = open_out filename in
  begin
    let { desc; cmd; time_unit } = header in
    Printf.fprintf oc "desc: %s\n" desc;
    Printf.fprintf oc "cmd: %s\n" cmd;
    Printf.fprintf oc "time_unit: %s\n" time_unit;
  end;
  Array.iter (fun { snapshot; time; mem_heap_B;
                    mem_heap_extra_B; mem_stacks_B;
                    heap_tree } ->
    Printf.fprintf oc "#-----------\n";
    Printf.fprintf oc "snapshot=%d\n" snapshot;
    Printf.fprintf oc "#-----------\n";
    Printf.fprintf oc "time=%Ld\n" time;
    Printf.fprintf oc "mem_heap_B=%Ld\n" mem_heap_B;
    Printf.fprintf oc "mem_heap_extra_B=%Ld\n" mem_heap_extra_B;
    Printf.fprintf oc "mem_stacks_B=%Ld\n" mem_stacks_B;
    Printf.fprintf oc "heap_tree=%s\n" (
      match heap_tree with
      | Empty -> "empty"
      | Peak _ -> "peak"
      | Detailed _ -> "detailed");
    begin match heap_tree with
    | Empty -> ()
    | Peak tree -> fprintf_tree oc 0 tree
    | Detailed tree -> fprintf_tree oc 0 tree
    end;
  ) snapshots;
  close_out oc
