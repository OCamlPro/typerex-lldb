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

(* Author: Fabrice
   Status: not finished. We should use the parser in tests/cachegrindloader.cpp
     to better understand the semantics of each field.
*)



(* Positions: beware that they can either be simple integers, or an
   interval of two values:
N+M : from N to N+M
N-M : from N to M
N:M : from N to M

fl: set file + set current function file
fi|fe: set file, but not current function file
fn: set function name, and restore current file to current function file

cob: set called object
cfi|cfl: set called file
cfn: set called function


*)

open StringCompat
open CallgrindTypes

let int_of_string s = try int_of_string s with
  | _ -> Printf.kprintf failwith "int_of_string(%S) failed" s

let int64_of_string s = try Int64.of_string s with
  | _ -> Printf.kprintf failwith "int64_of_string(%S) failed" s

let of_file filename =

  let version = ref None in
  let creator = ref None in
  let pid = ref None in
  let cmd = ref None in
  let part = ref None in
  let desc = ref [] in
  let positions = ref None in
  let events = ref None in
  let summary = ref None in

  let totals = ref None in
  let linenum = ref 0 in
  let discarded_lines = ref [] in

  let get variable ref =
    match !ref with
    | None -> Printf.kprintf failwith "Variable %S not set" variable
    | Some v -> ref := None; v
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

  let rec find_num s pos len =
    if pos < len then
      match s.[pos] with
      | '0'..'9' -> find_num s (pos+1) len
      | ')' ->
        let num = int_of_string (String.sub s 1 (pos-1)) in
        num,
        if pos+1 = len then None else
          let str =
            if s.[pos+1] = ' ' then
              String.sub s (pos+2) (len-pos-2)
            else
              String.sub s (pos+1) (len-pos-1)
          in
          Some str
      | _ -> raise Not_found
    else raise Not_found
  in

  let rec find_string_number s =
    let len = String.length s in
    if len > 0 && s.[0] = '(' then
      find_num s 1 len
    else raise Not_found
  in

  let strings = Hashtbl.create 1111 in
  let set_string line ref value =
    try
      let (num, value) = find_string_number value in
      let value = match value with
        | None -> Hashtbl.find strings num
        | Some value ->
          Hashtbl.add strings num value;
          value
      in
      ref := value
    with Not_found ->
      ref := value
  in

  let subcall = ref false in
  let ob = ref "" in
  let fn = ref "" in
  let fl = ref "" in
  let fi = ref "" in
  let fe = ref "" in
  let cob = ref "" in
  let cfn = ref "" in
  let cfi = ref "" in

  let all_events = ref [] in
  let flush_event line =
    subcall := false;
    ()
  in

  let rec find_sep line pos len =
    if pos >= len then begin
    (* an event *)
      flush_event line
    end else
      match line.[pos] with
      | ':' -> begin
        if line.[pos+1] <> ' ' then
          find_sep line (pos+1) len
        else
          let variable = String.sub line 0 pos in
          let value = String.sub line (pos+2) (len-pos-2) in
          match variable with
          | "version" -> set line version value
          | "creator" -> set line creator value
          | "pid" -> set line pid value
          | "cmd" -> set line cmd value
          | "part" -> set line part value
          | "desc" -> desc := value :: !desc
          | "positions" -> set line positions value
          | "events" -> set line events value
          | "summary" -> set line summary value
          | "totals" -> set line totals value
          | _ ->
            discard line "unknown header"
      end
      | '=' ->
        begin
          let variable = String.sub line 0 pos in
          let value = String.sub line (pos+1) (len-pos-1) in
          match variable with
          | "ob" (* object file *) ->
            set_string line ob value;
            cob := ""; cfn := ""; cfi := ""; fi := ""; fe := "";
          | "fn" (* function name *) ->
            cob := ""; cfn := ""; cfi := ""; fi := ""; fe := "";
            set_string line fn value
          | "fl" (* file name *) ->
            cob := ""; cfn := ""; cfi := ""; fi := ""; fe := "";
            set_string line fl value

          | "calls" -> set_string line fl value; subcall := true
          | "cfn" (* called function name *) -> set_string line cfn value
          | "cfi" (* called file name *) -> set_string line cfi value
          | "cob" (* called object file *) -> set_string line cob value

          | "fi"  (* include file containing code *)
            -> set_string line fi value
          | "fe"  (* end of inlined code *)
            -> set_string line fe value; fi := ""
          | _ -> discard line "unknown variable"
        end
      | _ -> find_sep line (pos+1) len
  in

  let line_reader line =
    incr linenum;
    let len = String.length line in
    if len = 0 then ()
    else match line.[0] with
    | '#' -> ()
    | '*' | '+' | '-' | '0'..'9' -> flush_event line
    | _ -> find_sep line 0 len
  in
  File.iter_lines line_reader filename;
  flush_event ();
  ()
