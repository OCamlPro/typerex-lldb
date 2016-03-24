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

let int_of_string s = try int_of_string s with
  | _ -> Printf.kprintf failwith "int_of_string(%S) failed" s

let int64_of_string s = try Int64.of_string s with
  | _ -> Printf.kprintf failwith "int64_of_string(%S) failed" s

let rec tree_of_children indent0 list =
  match list with
    [] -> assert false
  | (indent1, nchildren, part, position) :: rem ->
    assert (indent0 = indent1);
    let (children, rem) = get_children (indent1+1) nchildren [] rem in
    assert (rem = []);
    { part; position; children }

and get_children indent0 nchildren0 children0 rem =
  if nchildren0 = 0 then (Array.of_list (List.rev children0), rem)
  else
    match rem with
    | [] -> assert false
    | (indent1, nchildren1, part1, position1) :: rem ->
      assert (indent0 = indent1);
      let (children1, rem) = get_children (indent1+1) nchildren1 [] rem in
      let child = { part = part1;
                    position = position1;
                    children = children1 } in
      get_children indent0 (nchildren0-1) (child :: children0) rem


let of_file filename =
  let header = ref None in
  let snapshots = ref [] in

  let desc = ref None in
  let cmd = ref None in
  let time_unit = ref None in

  let snapshot = ref None in
  let time = ref None in
  let mem_heap_B = ref None in
  let mem_heap_extra_B = ref None in
  let mem_stacks_B = ref None in
  let heap_tree = ref None in
  let children = ref [] in

  let get variable ref =
    match !ref with
    | None -> Printf.kprintf failwith "Variable %S not set" variable
    | Some v -> ref := None; v
  in

  let linenum = ref 0 in
  let discarded_lines = ref [] in
  let flush_snapshot () =
    match !snapshot with
    | None ->
      let desc = get "desc" desc in
      let cmd = get "cmd" cmd in
      let time_unit = get "time_unit" time_unit in
      header := Some { desc; cmd; time_unit }
    | Some snapshot_num ->
      Printf.eprintf "Reading snapshot %s\n%!" snapshot_num;
      let time = get "time" time in
      let mem_heap_B = get "mem_heap_B" mem_heap_B in
      let mem_heap_extra_B = get "mem_heap_extra_B" mem_heap_extra_B in
      let mem_stacks_B = get "mem_stacks_B" mem_stacks_B in
      let heap_tree = get "heap_tree" heap_tree in
      let tree = List.rev !children in children := [];
      let heap_tree = match heap_tree with
        | "empty" -> Empty
        | "peak" ->
          let tree = tree_of_children 0 tree in Peak tree
        | "detailed" ->
          let tree = tree_of_children 0 tree in Detailed tree
        | _ -> Printf.kprintf failwith "Invalid heap_tree value %S" heap_tree
      in
      snapshots := {
        snapshot = int_of_string snapshot_num;
        time = int64_of_string time;
        mem_heap_B = int64_of_string mem_heap_B;
        mem_heap_extra_B = int64_of_string mem_heap_extra_B;
        mem_stacks_B = int64_of_string mem_stacks_B;
        heap_tree = heap_tree;
      } :: !snapshots;
      snapshot := None;
      ()
  in

  let discard line reason =
    Printf.eprintf "Discarding line %d (reason %s) : %S\n%!"
      !linenum  line reason;
    discarded_lines := (!linenum, line, reason) :: !discarded_lines
  in

  let set line ref value =
    match !ref with
    | None -> ref := Some value
    | Some _ -> discard line "already set"
  in

  let rec find_children line nchildren pos len value =
    if pos = len then
      let (indent,value) = value in
      let part, position = OcpString.cut_at value ' ' in
      let part = int64_of_string part in
      children := (indent, nchildren, part, position) :: !children
    else
      match line.[pos] with
      | '0'..'9' as c ->
        let nchildren = (nchildren*10) + (int_of_char c - int_of_char '0') in
        find_children line nchildren (pos+1) len value
      | _ -> discard line "unknown header"
  in

  let rec find_indent line pos len value =
    if pos = len then
      discard line "empty header"
    else
      match line.[pos] with
      | ' ' -> find_indent line (pos+1) len value
      | 'n' ->
        find_children line 0 (pos+1) len (pos, value)
      | _ -> discard line "unknown header"
  in

  let rec find_sep line pos len =
    if pos >= len then
      discard line "no separator"
    else
      match line.[pos] with
      | ':' -> begin
        if line.[pos+1] <> ' ' then
          find_sep line (pos+1) len
        else
        let variable = String.sub line 0 pos in
        let value = String.sub line (pos+2) (len-pos-2) in
        match variable with
          | "desc" -> set line desc value
          | "cmd" -> set line cmd value
          | "time_unit" -> set line time_unit value
          | _ ->
            (* maybe a {[ ]*n[1-9]+} line ? *)
            find_indent line 0 (String.length variable) value
        end
      | '=' ->
         begin
        let variable = String.sub line 0 pos in
        let value = String.sub line (pos+1) (len-pos-1) in
        match variable with
          | "snapshot" ->
            flush_snapshot ();
            set line snapshot value
          | "time" -> set line time value
          | "mem_heap_B" -> set line mem_heap_B value
          | "mem_heap_extra_B" -> set line mem_heap_extra_B value
          | "mem_stacks_B" -> set line mem_stacks_B value
          | "heap_tree" -> set line heap_tree value
          | _ -> discard line "unknown variable"
        end
      | _ -> find_sep line (pos+1) len
  in

  let line_reader line =
    incr linenum;
    let len = String.length line in
    if len > 0 then
      if line.[0] = '#' then () else
        find_sep line 0 len
  in
  File.iter_lines line_reader filename;
  flush_snapshot ();
  let header = match !header with
      None -> failwith "Empty file"
    | Some header -> header in
  let snapshots = Array.of_list (List.rev !snapshots) in
  { header; snapshots }, List.rev !discarded_lines
