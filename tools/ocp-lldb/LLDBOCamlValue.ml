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
open LLDBGlobals

open Longident
open Path
open Types

#ifndef OCAML_NON_OCP
open StringCompat
open GcprofTypes
#endif

let max_indent = ref 10

let string_of_tag tag =
  match tag with
  | 246 -> "lazy"
  | 247 -> "closure"
  | 248 -> "object"
  | 249 -> "infix"
  | 250 -> "forward"
  | 251 -> "abstract"
  | 252 -> "string"
  | 253 -> "double"
  | 254 -> "double[]"
  | 255 -> "custom"
  | _ -> "value"


    (* Print a constructor or label, giving it the same prefix as the type
       it comes from. Attempt to omit the prefix if the type comes from
       a module that has been opened. *)

let tree_of_qualified lookup_fun env ty_path name =
  match ty_path with
  | Pident id ->
    name
  | Pdot(p, s, pos) ->
    if try
         match (lookup_fun (Lident name) env).desc with
         | Tconstr(ty_path', _, _) -> Path.same ty_path ty_path'
         | _ -> false
      with Not_found -> false
        then name
        else Printf.sprintf "%s.%s" (Path.name p) name
      | Papply _ -> Path.name ty_path

    let tree_of_constr =
      tree_of_qualified
        (fun lid env -> (Env.lookup_constructor lid env).cstr_res)

    and tree_of_label =
      tree_of_qualified (fun lid env -> (Env.lookup_label lid env).lbl_res)


let pointer_kind mem heap addr =
  let range =  (addr,Int64.add addr 1L) in
  if Int64.logand addr 1L <> 0L then
    Integer
  else
    if
      addr >= heap.caml_young_start && addr <= heap.caml_young_end
    then
      MinorAddress
    else
      if ChunkSet.mem range heap.caml_major_heap then
        MajorAddress
      else
        try
          let m = ChunkMap.find range mem.data_fragments in
          ModuleData m
        with Not_found ->
          try
            let m = ChunkMap.find range mem.code_fragments in
            ModuleCode m
          with Not_found ->
            External

let debug_path s path =
  match s with
  | [] -> assert false
  | (indent, head, ty) :: tail ->
    let head = Printf.sprintf "%s (<- %s)" head path in
    (indent, head, ty) :: tail

let label_name_arg = function
#if OCAML_VERSION < "4.02"
   (lbl_name, _, lbl_arg) -> (lbl_name, lbl_arg)
#else
    { ld_id = lbl_name; ld_type = lbl_arg } -> (lbl_name, lbl_arg)
#endif
;;

let constructor_info
#if OCAML_VERSION < "4.02"
   (constr_name, constr_args,ret_type)
#else
    { cd_id =constr_name; cd_args = constr_args; cd_res = ret_type }
#endif
    = (constr_name, constr_args,ret_type)
;;

type context = {
  mem : LLDBTypes.mem_info;
  heap : LLDBTypes.heap_info;
  process : sbProcess;
#ifndef OCAML_NON_OCP
  locs : GcprofTypes.locations;
#endif
}

#ifdef OCAML_NON_OCP
let string_of_type_expr env ty = ""
#else
let string_of_type_expr env ty = GcprofLocations.string_of_type_expr env ty
#endif

let rec print_gen_value c indent addr types =
  if indent > !max_indent then [ indent, "...", ""] else
  match pointer_kind c.mem c.heap addr with
  | Integer ->
    print_typed_int c indent (Int64.to_int (Int64.shift_right addr 1)) types

  | ModuleCode m ->
    [ indent,
      Printf.sprintf "0x%Lx" addr,
      Printf.sprintf "code of module %S" m ]
  | External ->
    [ indent,
      Printf.sprintf "0x%Lx" addr,
      "external"
    ]
  | MinorAddress
  | MajorAddress
  | ModuleData _ as zone ->
    let header = LLDBUtils.getMem64 c.process (Int64.sub addr 8L) in
    let h = LLDBOCamlDatarepr.parse_header c.mem header in
    (* #ifndef OCAML_NON_OCP *)
    let zone = match zone with
      | MinorAddress -> "minor"
      | MajorAddress -> "major"
      | ModuleData m -> Printf.sprintf "data %s" m
      | _ -> assert false
    in
#ifndef OCAML_NON_OCP
    match h.locid with
    | None ->
#endif
      (*    Printf.eprintf
            "0x%Lx: 0x%Lx{ tag=%d wosize=%d gc=%x zone=%s } (%s)\n%!"
            addr header h.tag h.wosize h.gc
            zone
            (string_of_tag h.tag); *)
      print_typed_value c indent h zone addr types
#ifndef OCAML_NON_OCP
    | Some locid ->
      (*    Printf.eprintf
            "0x%Lx: 0x%Lx { tag=%d wosize=%d gc=%x zone=%s locid=%d } (%s)%s\n%!"
            addr header h.tag h.wosize h.gc zone locid
            (string_of_tag h.tag)
            (
            try
            let s = GcprofLocations.to_strings locs locid in
            GcprofTypes.(Printf.sprintf "%s@%s{%s,%s}"
            s.sloc_type s.sloc_loc s.sloc_kind
            (String.concat "." s.sloc_path))
            with _ ->
            "invalid-locid"
            ); *)
      let sloc = GcprofLocations.to_strings c.locs locid in
      let loc_info = try
                       Some GcprofTypes.(Printf.sprintf "%s{%s,%s}"
                         (*s.sloc_type*) sloc.sloc_loc sloc.sloc_kind
                                           (String.concat "." sloc.sloc_path))
        with _ -> None in
      let env = c.locs.locs_env in
      let li = c.locs.locs_info.(locid) in
      let types =
        match li.loc_type with
        | (Not_applicable | RecClosure _) -> types
        | Expr ty -> (env, ty) :: types
      in
      let s = print_typed_value c indent h zone addr types in
      match loc_info with
      | None -> s
      | Some loc_info ->
        match s with
        | [] -> assert false
        | (indent, descr, ty) :: tail ->
          let ty =Printf.sprintf "%s@%s"
            (match ty with
            | "" | "?" ->
              sloc.sloc_type
            | _ -> ty) loc_info in
          (indent, descr, ty) :: tail
#endif

and print_typed_int c indent n types =
  match types with
  | [] -> [ indent, Printf.sprintf "Integer %d" n, ""]
  | (env, ty) :: types ->
    let ty = Ctype.expand_head env ty in
    let s =
      match ty.desc with
      | Tlink _ -> assert false
      | Tvar _
      | Tarrow (_, _, _, _)
      | Ttuple _
      | Tobject (_, _)
      | Tfield (_, _, _, _)
      | Tnil
      | Tsubst _
      | Tvariant _
      | Tunivar _
      | Tpoly (_, _)
      | Tpackage (_, _, _) -> print_typed_int c indent n types

      | Tconstr (path, _, _) ->
        if Path.same path Predef.path_list then begin
            let s = match n with
            | 0 -> "[]"
            | _ -> Printf.sprintf "invalid(list,%d)" n in
          [ indent, s, "" ]
        end else
        if Path.same path Predef.path_int then begin
          [ indent, string_of_int n, "" ]
        end else
        if Path.same path Predef.path_char then begin
          let s =
            if n >= 0 && n < 256 then
              Printf.sprintf "%C" (char_of_int n)
            else Printf.sprintf "invalid(char,%d)" n in
          [ indent, s, "" ]
        end else
          if Path.same path Predef.path_bool then begin
            let s = match n with
            | 0 -> "false"
            | 1 -> "true"
            | _ -> Printf.sprintf "invalid(bool,%d)" n in
          [ indent, s, "" ]
        end else
        if Path.same path Predef.path_unit then begin
            let s = match n with
            | 0 -> "()"
            | _ -> Printf.sprintf "invalid(unit,%d)" n in
          [ indent, s, "" ]
        end else
        if Path.same path Predef.path_option then begin
            let s = match n with
            | 0 -> "None"
            | _ -> Printf.sprintf "invalid(option,%d)" n in
          [ indent, s, "" ]
        end else
          match
            (try `Value (Env.find_type path env) with exn -> `Exception exn)
          with
          | `Exception exn ->
            debug_path (print_typed_int c indent n types)
              (Printf.sprintf "WARNING: %S is not in env" (Path.name path))
            | `Value decl ->
              match decl.type_kind with
              | Type_variant constr_list ->
                let tag = Cstr_constant n in
                let (constr_name, constr_args,ret_type) =
                  constructor_info
                    (Datarepr.find_constr_by_tag tag constr_list) in
                [ indent, Ident.name constr_name, "" ]
#if OCAML_VERSION < "4.02"
#else
              | Type_open ->
               debug_path (print_typed_int c indent n types) "Type_open"
#endif
              | Type_abstract ->

               debug_path (print_typed_int c indent n types) "Type_abstract"
              | Type_record _ ->
               debug_path (print_typed_int c indent n types) "Type_record"
    in
    match s with
    | [] -> assert false
    | (indent, head, type_info) :: tail ->
      let type_info = if type_info = "" then
          let s = string_of_type_expr env ty in
          if s <> "'a" then s else ""
        else type_info in
      (indent, head, type_info) :: tail


and print_typed_value c indent h zone addr types =
  match types with
  | [] -> print_raw_value c indent h zone addr
  | (env, ty) :: types ->
    let ty = Ctype.expand_head env ty in
    match ty.desc with

    | Tlink _ -> assert false

    | Tvar _
      -> debug_path (print_typed_value c indent h zone addr types) "Tvar"
    | Tarrow _ (* label * type_expr * type_expr * commutable *)
      -> debug_path (print_typed_value c indent h zone addr types) "Tarrow"
    | Ttuple ty_args (*  type_expr list *)
      ->
      let ty_args = Array.of_list ty_args in
      let len = Array.length ty_args in
      if len <> h.wosize then
        debug_path
          (print_typed_value c indent h zone addr types)
          (Printf.sprintf
             "WARNING(tuple size %d <> block size %d)"
             len h.wosize)
      else
      (indent, "<tuple>", string_of_type_expr env ty) ::
           (print_tuple "tuple" env ty_args c indent addr 0 h.wosize)

    | Tobject _ (* type_expr * (Path.t * type_expr list) option ref *)
      -> debug_path (print_typed_value c indent h zone addr types) "Tobject"
    | Tfield _ (*  string * field_kind * type_expr * type_expr *)
      -> debug_path (print_typed_value c indent h zone addr types) "Tfield"
    | Tnil
      -> debug_path (print_typed_value c indent h zone addr types) "Tnil"
    | Tsubst _ (* type_expr, for copying *)
      -> debug_path (print_typed_value c indent h zone addr types) "Tsubst"
    | Tvariant _ (* row_desc *)
      -> debug_path (print_typed_value c indent h zone addr types) "Tvariant"
    | Tunivar _ (* string option *)
      -> debug_path (print_typed_value c indent h zone addr types) "Tunivar"
    | Tpoly _ (* type_expr * type_expr list *)
      -> debug_path (print_typed_value c indent h zone addr types) "Tpoly"
    | Tpackage _ (*  Path.t * Longident.t list * type_expr list *)
      -> debug_path (print_typed_value c indent h zone addr types) "Tpackage"

    (* | Tconstr _   Path.t * type_expr list * abbrev_memo ref *)

    | Tconstr(path, [], _) when Path.same path Predef.path_exn ->
      debug_path (print_typed_value c indent h zone addr types) "exn"

    | Tconstr(path, [ty_arg], _) when Path.same path Predef.path_array ->
      (indent,
       Printf.sprintf "<array[%d]>" h.wosize,
       string_of_type_expr env ty) ::
           (print_array (env,ty_arg) c indent addr 0 h.wosize)

    | Tconstr(path, [tys], _)
        when Path.same path Predef.path_list ->
      (indent, "<list>", "") ::
        print_list_value c indent env tys addr 0

    | Tconstr (path, [tys], _)
        when Path.same path Predef.path_lazy_t ->
      debug_path (print_typed_value c indent h zone addr types) "lazy"

    | Tconstr(path, ty_list, _) ->
      print_decl_value c indent h zone addr types ty env path ty_list

and print_decl_value c indent h zone addr types ty env path ty_list =
    match
      try `Value (Env.find_type path env) with exn -> `Exception exn with
 | `Exception exn ->
   (* TODO: Add a command to extend [env] with modules
      loaded from .cmi files. This can happen when a module is not
      linked, either because it is only a .mli file, or because a .ml
      file contains only type definitions.
   *)
   debug_path (print_typed_value c indent h zone addr types)
     (Printf.sprintf "WARNING: %S is not in env" (Path.name path))
 | `Value decl ->

   match decl.type_kind with
   | Type_abstract ->
     (* we could expand a potential manifest type... *)
     debug_path (print_typed_value c indent h zone addr types) "Type_abstract"
   | Type_variant constr_list ->
     let tag =
       Cstr_block h.tag
     (* Cstr_constant tag *)
     in
     let (constr_name, constr_args,ret_type) =
       constructor_info
         (Datarepr.find_constr_by_tag tag constr_list) in
     let constr_name = Ident.name constr_name in
     let type_params =
       match ret_type with
         Some t ->
           begin match (Ctype.repr t).desc with
             Tconstr (_,params,_) ->
               params
           | _ -> assert false end
       | None -> decl.type_params
     in
     begin
       match
         try `Value (List.map
                       (function ty ->
                         Ctype.apply env type_params ty ty_list )
                       constr_args)
         with  Ctype.Cannot_apply as exn -> `Exception exn
       with
       | `Exception exn ->
         debug_path (print_typed_value c indent h zone addr types)
           (Printf.sprintf "%s (Ctype.Cannot_apply)" constr_name)
       | `Value ty_args ->
         let ty_args = Array.of_list ty_args in
         let len = Array.length ty_args in
         if len <> h.wosize then
           debug_path
             (print_typed_value c indent h zone addr types)
             (Printf.sprintf
                "WARNING(%s size %d <> block size %d)" constr_name
                len h.wosize)
         else
           (indent, Printf.sprintf "<%s>" constr_name,
            string_of_type_expr env ty) ::
             (print_tuple constr_name env ty_args c indent addr 0 h.wosize)
     end

   | Type_record(lbl_list, rep) ->

     let fields = List.mapi (fun pos lbl ->
       let (lbl_name, lbl_arg) = label_name_arg lbl
       in
       (pos, lbl_name, lbl_arg)) lbl_list in


     (indent, "{",
      string_of_type_expr env ty
     ) ::
       (List.flatten
          (List.map (fun (pos, lbl_name, lbl_arg) ->
            let types =
              try
                [env,
                 Ctype.apply env decl.type_params
                   lbl_arg ty_list]
              with
              | Ctype.Cannot_apply -> []
            in
            let name = Ident.name lbl_name in
                 (* PR#5722: print full module path only
                    for first record field *)
            let lid =
              if pos = 0 then tree_of_label env path name
              else name in
            let v = LLDBUtils.getMem64 c.process
              (Int64.add addr (Int64.of_int (8 * pos))) in
            let s = print_gen_value c (indent+4) v types in
            match s with
            | [] -> assert false
            | (_, head, ty) :: tail ->
              (indent+2,
               Printf.sprintf "%s= %s" lid head,
               ty) :: tail
           )
             fields)) @
       [ indent, "}", "" ]

#if OCAML_VERSION < "4.02"
#else
   | Type_open ->
     debug_path (print_typed_value c indent h zone addr types) "Type_open"
#endif

 and print_list_value c indent env ty addr i =
     if i = 5 then
       [indent, "... </list>", ""]
     else
       let head_v = LLDBUtils.getMem64 c.process
         (Int64.add addr (Int64.of_int (8 * 0))) in
       let head = print_gen_value c (indent+2) head_v [env, ty] in
       let tail_addr = LLDBUtils.getMem64 c.process
         (Int64.add addr (Int64.of_int (8 * 1))) in
       if tail_addr = 1L then
         head @ [ indent, "</list>", "" ]
       else
         head @
           print_list_value c indent env ty tail_addr (i+1)

 and print_raw_value c indent h zone addr =
     if indent > !max_indent then
       [ indent, "...", "" ]
     else
       if h.tag < 251 then
         (indent,
          Printf.sprintf "<block[%d] tag=%d>" h.wosize h.tag
            , "?") ::
           (print_block c indent addr 0 h.wosize)
       else
         if h.tag = 252 then
           let len = LLDBOCamlDatarepr.parse_string_length c.process h addr in
           let maxlen = min 50 len in
           let b = Bytes.create maxlen in
           let nRead = SBProcess.readMemory c.process addr b maxlen
             LLDBUtils.sbError in
           if nRead > 0 then
             let b = Bytes.sub_string b 0 nRead in
             [ indent, Printf.sprintf "(len %d) %S" len b, "string" ]
           else
             [ indent, "<unreadable string>", "string" ]
         else
           let s =
             Printf.sprintf
               "<ptr>0x%Lx: { tag=%d wosize=%d gc=%x zone=%s } (%s)</ptr>"
               addr h.tag h.wosize h.gc
               zone
               (string_of_tag h.tag)
           in
           [ indent, s, "" ]

 and print_block c indent addr i len =
     if i = len then
       [ indent, "</block>", "?" ]
     else
       if i = 5 then
         [ indent+2, "...", "_"; indent, "</block>", "" ]
       else
         let v = LLDBUtils.getMem64 c.process
           (Int64.add addr (Int64.of_int (i * 8))) in
         let s = print_gen_value c (indent+4) v [] in
         let s = match s with
           | [] -> assert false
           | (_, head, ty) :: tail ->
             (indent+2, Printf.sprintf "%d: %s" i head, ty)
             :: tail
         in
         s @ print_block c indent addr (i+1) len

 and print_tuple constr_name env ty_args c indent addr i len =
     if i = len then
       [ indent, Printf.sprintf "</%s>" constr_name, "" ]
     else
       let v = LLDBUtils.getMem64 c.process
         (Int64.add addr (Int64.of_int (i * 8))) in
       let s = print_gen_value c (indent+4) v [env,ty_args.(i)] in
       let s = match s with
         | [] -> assert false
         | (_, head, ty) :: tail ->
           (indent+2, Printf.sprintf "%d: %s" i head, ty)
           :: tail
       in
       s @ print_tuple constr_name env ty_args c indent addr (i+1) len

 and print_array (env, ty_arg) c indent addr i len =
     if i = len then
       [ indent, "</array>", "" ]
     else
       if i = 5 then
         [ indent+2, "...", "_"; indent, "</array>", "" ]
       else
         let v = LLDBUtils.getMem64 c.process
           (Int64.add addr (Int64.of_int (i * 8))) in
         let s = print_gen_value c (indent+4) v [env,ty_arg] in
         let s = match s with
           | [] -> assert false
           | (_, head, ty) :: tail ->
             (indent+2, Printf.sprintf "%d: %s" i head, ty)
             :: tail
         in
         s @ print_array (env, ty_arg) c indent addr (i+1) len



let print_value target mem heap initial_addr types =
#ifndef OCAML_NON_OCP
  let loctbls, locs = if mem.standard_header then
      [||], { locs_env = Memprof_env.initial;
              locs_info = [||];
              locs_cache = [||];
            }
    else
      LLDBOCamlLocids.load target
  in
#endif
  let process = SBTarget.getProcess target in
  let c = {
    process; mem; heap;
#ifndef OCAML_NON_OCP
    locs;
#endif
  } in
  let lines = print_gen_value c 0 initial_addr types in
  List.iter (fun (indent, line, ty) ->
    for i = 1 to indent do print_char ' '; done;
    print_string line;
    if ty <> "" then begin
      print_string " : ";
      print_string ty;
    end;
    print_newline ()
  ) lines
