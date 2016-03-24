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

  let bad_stubs = [
    (* undefined for some reason... *)
    "SBFrame_WatchLocation_ml";
    "SBFrame_WatchValue_ml";
    "SBDebugger_GetSourceManager_ml";
    "SBTarget_GetSourceManager_ml";

    (* manually done *)
    "SBCommandInterpreter_AddCommand_ml";
    "SBModule_GetVersion_ml";
    "SBBreakpoint_SetCallback_ml";

    (* no constructor *)
    "SBTarget_GetLaunchInfo_ml";

    "SBLaunchInfo_new_ml";
    "SBSourceManager_new_ml";
    "SBPlatformShellCommand_new_ml";
    "SBPlatformConnectOptions_new_ml";

  ]

  let inttypes = [ "uint32_t"; "DescriptionLevel"; "size_t"; "int";
                   "int32_t";  "break_id_t";
                   "uint8_t"; "user_id_t"; "watch_id_t";
                   "int8_t"; "int16_t"; "offset_t";
                   "queue_id_t"; "uint16_t";
                   (* lldb-enumerations.h *)
                   "BasicType"; "ByteOrder"; "DynamicValueType";
                   "ErrorType"; "Format"; "LanguageType";
                   "QueueItemKind"; "ReturnStatus"; "ScriptLanguage";
                   "SymbolType"; "ValueType"; "StopReason"; "QueueKind";
                   "SectionType"; "StateType"; "TypeClass";
                   "TemplateArgumentKind"; "AddressClass";
                   "RunMode"; "char"; "ConnectionStatus";
                   "InstrumentationRuntimeType"; "MemberFunctionKind";
                   "TypeSummaryCapping";
                   "MatchType"; (* <- defined several types !!! :-( *)

                 ]
  let int64types = [ "uint64_t"; "int64_t";  "addr_t"; "tid_t";  ]
  let chartypes = [ "char"; "uint8_t" ]

  (*
    Unknown types:  char offset_t tid_t
    pid_t
    ReadThreadBytesReceived
    BreakpointHitCallback  LogOutputCallback ExpressionCancelCallback
    CommandOverrideCallback
  *)


(* This program reads the include files from lldb/API/ and automatically
  generate the OCaml stubs for a large majority of them, leaving only
  little manual work for the remaining functions.

   TODO:
   char** + const char**

2 class SBWatchpoint: 15/17
2 class SBValue: 54/56
1 class SBThread: 36/37
11 class SBTarget: 68/79
2 class SBLaunchInfo: 31/33
1 class SBStringList: 6/7
2 class SBStream: 6/8
1 class SBQueueItem: 7/8
6 class SBProcess: 48/54
1 class SBModule: 32/33
1 class SBListener: 16/17
1 class SBInstructionList: 7/8
1 class SBInstruction: 13/14
2 class SBFrame: 36/38
1 class SBFileSpecList: 6/7
1 class SBExpressionOptions: 21/22
2 class SBEvent: 8/10
1 class SBError: 12/13
15 class SBDebugger: 54/69
1 class SBInputReader: 2/3
6 class SBData: 24/30
2 class SBCompileUnit: 9/11
3 class SBCommunication: 11/14
  TODO: void* -> string

7 class SBCommandReturnObject: 17/24
  OK, 1 format
  TODO: 4 FILE*

1 class SBCommandPluginInterface: 0/1
  TODO: char**

3 class SBCommandInterpreter: 16/19
  OK, 3 callbacks

1 class SBBreakpoint: 30/31
  OK, 1 callback
*)

open Genlex

module StringSet = Set.Make(String)

let string_of_token token =
  match token with
  | Ident id -> Printf.sprintf "Ident \"%s\"" id
  | Kwd id   -> Printf.sprintf "Kwd \"%s\"" id
  | Int n    -> Printf.sprintf "Int %d" n
  | Char c   -> Printf.sprintf "Char '%c'" c
  | String s -> Printf.sprintf "String \"%s\"" (String.escaped s)
  | Float n  -> Printf.sprintf "Float %f" n

let tokens_of_string lexer s =
  let str1 = Stream.of_string s in
  let str2 = lexer str1 in
  let list = ref [] in
  let error = try
                Stream.iter (fun token ->
                  (*                  Printf.printf "(%s) " (string_of_token token); *)
                  list := token :: !list) str2;
                None
    with
      Stream.Error error ->
        Some (Stream.count str1, error)
  in
  List.rev !list, error


let lexer = Genlex.make_lexer [ "."; "{"; "}"; "("; ")"; "::"; ";"; ",";
                              "["; "]"; ":"; "&"; "*"; "="; "~"; "**" ]

let rec simplify_tokens in_class tokens rem =
  match tokens with
  | Ident "class" :: Ident name :: Kwd "{" :: tokens
  | Ident "class" :: Ident "LLDB_API" :: Ident name :: Kwd "{" :: tokens
    ->
    simplify_tokens 1 tokens (Kwd "{" :: Ident name :: Ident "class" :: rem)
  | Ident "virtual" :: tokens ->
    simplify_tokens in_class tokens rem
  | Ident "enum" :: tokens ->
    let rec iter tokens =
      match tokens with
      | Kwd "}" :: Kwd ";" :: tokens -> tokens
      | _ :: tokens -> iter tokens
      | []  -> assert false
    in
    simplify_tokens in_class (iter tokens) rem
  | Ident "__attribute__" :: tokens ->
    let rec iter tokens =
      match tokens with
      | Kwd ";" :: _ -> tokens
      | _ :: tokens -> iter tokens
      | []  -> assert false
    in
    simplify_tokens in_class (iter tokens) rem

  | Ident "char" :: Kwd "*" :: Ident name :: Kwd "=" :: Ident "NULL" :: tokens ->
    simplify_tokens in_class
      (Ident "char" :: Kwd "*?" :: Ident name :: Kwd "=" :: Ident "NULL" :: tokens)
      rem

  | Ident "lldb" :: Kwd "::" :: tokens ->
    simplify_tokens in_class tokens rem
(*  | Ident "const" :: tokens ->
    simplify_tokens in_class tokens rem *)
  | Ident "public" :: Kwd ":" :: tokens ->
    simplify_tokens (in_class+1) tokens rem
  | Ident "private" :: Kwd ":" :: tokens ->
    simplify_tokens 1 tokens rem
  | Ident "protected" :: Kwd ":" :: tokens ->
    simplify_tokens 1 tokens rem
  | Ident "operator" :: Kwd "=" :: tokens ->
    simplify_tokens in_class tokens (Kwd "=" :: Ident "operator" :: rem)
  | Kwd "=" :: tokens ->
    let rec iter tokens =
      match tokens with
      | Ident "lldb" :: Kwd "::" :: tokens -> iter tokens
      | (Ident _ | Int _ | Float _ | Char _) :: tokens -> iter tokens
      | _ -> tokens
    in
    simplify_tokens in_class (iter tokens) rem

  | Kwd "}" :: tokens ->
    simplify_tokens 0 tokens
      (if in_class > 0 then (Kwd "}" :: rem) else rem)
  | token :: tokens ->
    if in_class > 1 then
      simplify_tokens in_class tokens (token :: rem)
    else
      simplify_tokens in_class tokens rem
  | [] ->
    assert (in_class = 0);
    List.rev rem

let verbose_simplify = ref false

let parse_tokens basename classes links tokens =
  if !verbose_simplify then begin
    Printf.eprintf "Before simplify_tokens\n%!";
    List.iter (fun token ->
      Printf.eprintf "%s " (string_of_token token)) tokens;
    Printf.eprintf "\n%!";
  end;

  let tokens = simplify_tokens 0 tokens [] in

  if !verbose_simplify then begin
  Printf.eprintf "After simplify_tokens\n%!";
    List.iter (fun token ->
    Printf.eprintf "%s " (string_of_token token)) tokens;
    Printf.eprintf "\n%!";
  end;

  let classSB_add in_class name set =
    match in_class with
    | None -> Printf.kprintf failwith "classSB_add[%S]" name
    | Some class_name ->
      if class_name <> name &&
        String.length name > 2 && name.[0] = 'S' && name.[1] = 'B' then
        StringSet.add name set
      else set
  in
  begin
    let rec iter in_class in_fun set tokens =
      match tokens with
      | Ident "class" :: Ident name :: Kwd "{" :: tokens ->
        iter (Some name) None StringSet.empty tokens
      | Kwd "}" :: tokens ->
        iter None None StringSet.empty tokens
      | Ident name :: Kwd "(" :: Kwd "*" :: tokens ->
        iter in_class in_fun (classSB_add in_class name set) tokens
      | Ident name :: Kwd "(" :: tokens ->
        iter in_class (Some name) set tokens
      | Kwd ";" :: tokens ->
        begin match in_class, in_fun with
        | Some class_name, Some fun_name ->
          if set <> StringSet.empty then begin
            StringSet.iter (fun name ->
              links := StringSet.add
                (Printf.sprintf "%s -> %s" class_name name) !links
            ) set;
          end
        | _ -> ()
        end;
        iter in_class None StringSet.empty tokens
      | Ident name :: tokens ->
        iter in_class in_fun  (classSB_add in_class name set) tokens
      | _ :: tokens ->
        iter in_class in_fun set tokens
      | [] -> ()

    in
    iter None None StringSet.empty tokens;
  end;

  begin
    let rec get_functions in_class in_fun tokens =
      match tokens with
      | Ident "class" :: Ident name :: Kwd "{" :: tokens ->
        (*        Printf.eprintf "class %s\n%!" name; *)
        get_functions (Some (name, [])) [] tokens
      | Kwd "}" :: tokens ->
        begin match in_class with
        | None -> assert false
        | Some (name, methods) ->
          (*          Printf.eprintf "Adding class %S\n%!" name; *)
          classes := (name, List.rev methods, basename) :: !classes
        end;
        get_functions None [] tokens
      | Kwd ";" :: tokens ->
        if !verbose_simplify then
          List.iter (fun token ->
            Printf.eprintf " %s" (string_of_token token);
          ) in_fun;
        begin match in_class with
        | None ->
          if !verbose_simplify then
            Printf.eprintf "no class: discarding\n%!";
          assert false
        | Some (class_name, funs) ->
          let in_class =
            let in_fun = List.rev in_fun in
            match in_fun with
            | Ident name :: Kwd "(" :: _ when name = class_name ->
              if !verbose_simplify then
                Printf.eprintf "constructor: discarding\n%!";
              in_class
            | Kwd "~" :: _ ->
              if !verbose_simplify then
                Printf.eprintf "deconstructor: discarding\n%!";
              in_class
            | _ ->
              if not (List.mem (Ident "operator") in_fun) then begin
                if !verbose_simplify then
                  Printf.eprintf "good: adding\n%!";
                Some (class_name, in_fun :: funs)
              end else begin
                if !verbose_simplify then
                  Printf.eprintf "operator: discarding\n%!";
                in_class
              end
          in
          get_functions in_class [] tokens
        end;
      | token :: tokens ->
        if !verbose_simplify then
          Printf.eprintf "passing %s\n%!" (string_of_token token);
        get_functions in_class (token :: in_fun) tokens
      | [] ->
        match in_class with
        | None -> ()
        | Some (name, _) ->
          Printf.eprintf "unfinished class %s\n%!" name
    in
    get_functions None [] tokens;
  end;

  (*
  begin
    let rec print_tokens in_class tokens =
      match tokens with
      | Ident "class" :: Ident name :: Kwd "{" :: tokens ->
        let oc = open_out (name ^ ".class") in
        Printf.fprintf oc "class %s {\n" name;
        Printf.fprintf oc "public:\n";
        print_tokens (Some oc) tokens
      | Kwd "}" :: tokens ->
        begin match in_class with
        | None -> assert false
        | Some oc -> close_out oc
        end;
        print_tokens None tokens
      | Kwd ";" :: tokens ->
        begin match in_class with
        | None -> assert false
        | Some oc ->
          Printf.fprintf oc ";\n";
        end;
        print_tokens in_class tokens
      | token :: tokens ->
        begin match in_class with
        | None -> assert false
        | Some oc ->
          match token with
          | Ident name -> Printf.fprintf oc " %s" name
          | Kwd name -> Printf.fprintf oc " %s" name
          | Int _ | Float _ | String _ | Char _ ->
            Printf.eprintf "Error: unexpected token %S\n%!"
              (string_of_token token);
            assert false
        end;
        print_tokens in_class tokens
      | [] -> ()
    in
    print_tokens None tokens;
  end;
  *)
  ()

let read_file classes links filename =
  let basename = Filename.basename filename in
  (*  Printf.eprintf "Reading %S\n%!" filename; *)
  let s =
    let ic = open_in_bin filename in
    let size = in_channel_length ic in
    let s = Bytes.create size in
    really_input ic s 0 size;
    close_in ic;
    s
  in
  let rec clear_eol s pos len =
    if pos < len && Bytes.get s pos <> '\n' then begin
      s.[pos] <- ' ';
      clear_eol s (pos+1) len
    end

    in
  let rec clear_comment s pos len =
    let c = Bytes.get s pos in
    s.[pos] <- ' ';
    if pos < len-1 && (c <> '*' || Bytes.get s (pos+1) <> '/') then begin
      clear_comment s (pos+1) len
    end else
      s.[pos+1] <- ' '
  in
  let rec iter s pos len =
    if pos < len then
      let c = Bytes.get s pos in
      begin
        match c with
        | '#' ->
          clear_eol s pos len
      | '\r' -> s.[pos] <- ' '
      | '/' when pos +1 < len && Bytes.get s (pos+1) = '/' ->
        clear_eol s pos len
      | ' ' when pos +1 < len && Bytes.get s (pos+1) = '(' ->
        s.[pos] <- '('; s.[pos+1] <- ' '
      | '/' when pos +1 < len && Bytes.get s (pos+1) = '*'->
        Printf.eprintf "clear comment\n%!";
        clear_comment s pos len
      | _ -> ()
      end      ;
      iter s (pos+1) len
    else Bytes.to_string s
  in
  let s = iter s 0 (Bytes.length s) in
(*
  let oc = open_out_bin (filename ^ ".cleared") in
  output_string oc s;
  close_out oc;
*)
  let tokens, error = tokens_of_string lexer s in
  match error with
  | None -> parse_tokens basename classes links tokens
  | Some (pos, error) ->
    Printf.eprintf "Exception %S\n%!" error;
    Printf.eprintf "Error at pos %d\n%!" pos;
    exit 2

let isSBclass name =
  String.length name > 2 &&
    String.get name 0 = 'S' &&
  String.get name 1 = 'B'

let read_file classes links filename =
  try
    read_file classes links filename
  with exn ->
    Printf.eprintf "Warning: read_file %S: %s\n%!"
      filename (Printexc.to_string exn)

let _ =
  let classes = ref [] in
  let links = ref StringSet.empty in
  Arg.parse [
    "-d", Arg.Set verbose_simplify, " Debug mode";
  ] (read_file classes links) "";

    let oc = open_out "graph.dot" in
    Printf.fprintf oc "digraph G {\n";
    StringSet.iter (fun s -> Printf.fprintf oc "  %s\n" s) !links;
    Printf.fprintf oc "}\n";
    close_out oc;

  let oc = open_out "lldbClasses_ml.h" in
  List.iteri (fun i (class_name, methods, basename) ->
    Printf.fprintf oc "\n";
    Printf.fprintf oc "#define LLDB_%-25s %2d\n" class_name (i+1);
    Printf.fprintf oc "#define Val_%s(ptr) Val_final(LLDB_%s, ptr)\n"
      class_name class_name;
    Printf.fprintf oc "#define %s_val(v) (%s*)OCPLLDB_val(LLDB_%s, v)\n"
      class_name class_name class_name;
  ) !classes;
  close_out oc;

  let oc = open_out "lldbDelete_ml.h" in
  List.iteri (fun i (class_name, methods, basename) ->
    Printf.fprintf oc "    case LLDB_%s: delete( (%s*)ptr ); break;\n"
      class_name class_name
  ) !classes;
  close_out oc;

  let oc = open_out "lldbIncludes_ml.h" in
  List.iteri (fun i (class_name, methods, basename) ->
    Printf.fprintf oc "#include \"lldb/API/%s\"\n" basename
  ) !classes;
  close_out oc;

  let oc = open_out "lldbClasses.class" in
  List.iteri (fun i (class_name, methods, basename) ->
    Printf.fprintf oc "class %s {\n" class_name;
    Printf.fprintf oc "public:\n";
    List.iter (fun tokens ->
      List.iter (fun token ->
        match token with
        | Ident name -> Printf.fprintf oc " %s" name
        | Kwd name -> Printf.fprintf oc " %s" name
        | Int _ | Float _ | String _ | Char _ ->
          Printf.eprintf "Error: unexpected token %S\n%!"
            (string_of_token token);
          assert false
      ) tokens;
      Printf.fprintf oc ";\n";
    ) methods;
    Printf.fprintf oc "};\n";
  ) !classes;

  close_out oc;
  let arg_of_arg nargs (meth, _) tokens =
    match tokens with
    | [ _ ] -> Printf.sprintf "arg%d" (nargs+1), List.rev tokens
    | Ident name :: tokens -> name, List.rev tokens
    | _ ->
      Printf.sprintf "arg%d" (nargs+1), List.rev tokens
  (*
    Printf.eprintf "%s.%s:\n%!" class_name meth;
    List.iteri (fun i token ->
    Printf.eprintf "arg[%d] = %s\n%!" i (string_of_token token)
    ) arg;
    assert false
  *)
  in

  let parse_function tokens =
    let rec iter_reply reply tokens =
      match tokens with
      | Ident name :: Kwd "(" :: tokens ->
        iter_args (name, List.rev reply) [] [] tokens
      | token :: tokens ->
        iter_reply (token :: reply) tokens
      | [] -> assert false
    and iter_args in_fun args arg tokens =
      match tokens with
      | Kwd "," :: tokens ->
        iter_args in_fun ( (arg_of_arg (List.length args) in_fun arg) :: args ) [] tokens
      | Kwd ")" :: tokens ->
        if arg = [] then
          in_fun, []
        else
          in_fun, List.rev ( (arg_of_arg (List.length args) in_fun arg) :: args)
      | token :: tokens ->
        iter_args in_fun args (token :: arg) tokens
      | [] -> assert false
    in
    let (funname, reply), args = iter_reply [] tokens in
    (funname, reply, args)
  in
  let get_stubname () =
    let funset = ref StringSet.empty in
    let rec get funname i =
      let stubname =
        if i = 0 then funname else funname ^ string_of_int i in
      if StringSet.mem stubname !funset then
        get funname (i+1)
      else begin
        funset := StringSet.add stubname !funset; stubname
      end
    in
    (function funname -> get funname 0)
  in

  let sbClass class_name =
    let s = Bytes.of_string class_name in
    s.[0] <- Char.lowercase (String.get class_name 0);
    s.[1] <- Char.lowercase (String.get class_name 1);
    Bytes.to_string s
  in

  let ntotal_final = ref 0 in
  let ngood_final = ref 0 in

  let oc = open_out "lldbGenerated_ml.h" in
  let mloc = open_out "LLDBGenerated.ml" in
  let badoc = open_out "lldbBadStubs_ml.h" in
  (*  Printf.fprintf mloc "open LLDBOCaml\n"; *)

  Printf.fprintf mloc "module TYPES = struct\n";
  Printf.fprintf mloc "type 'a sb\n";
  Printf.fprintf mloc "type file\n";
  List.iter (fun (class_name, methods, basename) ->
    Printf.fprintf mloc "type %s = [ `%s ] sb\n"
      (sbClass class_name) class_name;
  ) !classes;
  Printf.fprintf mloc "end\n\n";
  Printf.fprintf mloc "open StringCompat\n";
  Printf.fprintf mloc "include TYPES\n";

  let unknown_types = ref StringSet.empty in
  List.iter (fun (class_name, methods, basename) ->
    let stubname = get_stubname () in
    let ntotal = ref 0 in
    let ngood = ref 0 in

    let stub_v = Printf.sprintf "%s_new_ml" class_name in

    Printf.fprintf oc "extern \"C\" {\n";
    List.iter (fun tokens ->
      match tokens with
      | Ident "static" :: _ -> ()
      | Ident "typedef" :: _ -> ()
      | _ ->
        incr ntotal;
        let (funname, reply, args) = parse_function tokens in
        let stubname = stubname funname in
        let stub_v = Printf.sprintf "%s_%s_ml" class_name stubname in
        if not (List.mem stub_v bad_stubs) then begin
          Printf.fprintf oc "  extern value %s(value self_v" stub_v;
          List.iter (fun (argname, type_of_arg) ->
            Printf.fprintf oc ", value %s_v" argname
          ) args;
          Printf.fprintf oc ");\n";
        end
    ) methods;
    Printf.fprintf oc "  extern value %s(value unit_v);\n" stub_v;
    Printf.fprintf oc "};\n";


    Printf.fprintf mloc "\nmodule %s = struct\n\n" class_name;

    Printf.fprintf mloc "  external isNull : %s -> bool = \"lldb_is_NULL\"\n"
      (sbClass class_name);

    if not (List.mem stub_v bad_stubs) then begin
      Printf.fprintf oc "  value %s(value unit_v){\n" stub_v;
      Printf.fprintf oc "    %s* ptr = new %s();\n" class_name class_name;
      Printf.fprintf oc "    return Val_%s(ptr);\n" class_name;
      Printf.fprintf oc "  }\n";
      Printf.fprintf oc "\n";

      Printf.fprintf mloc "  external create : unit -> %s = %S\n"
        (sbClass class_name) stub_v;
    end;

    let stubname = get_stubname () in
    List.iter (fun tokens ->
      let good = ref true in

      let b = Buffer.create 100 in
      begin
        match tokens with
        | Ident "static" :: _ ->
          Printf.bprintf b "// static method\n";
          good := false
        | Ident "typedef" :: _ ->
          Printf.bprintf b "// typedef\n";
          good := false
        | _ ->
          let (funname, reply, args) = parse_function tokens in
          let stubname = stubname funname in
          let stub_v = Printf.sprintf "%s_%s_ml" class_name stubname in
          if List.mem stub_v bad_stubs then begin
            Printf.bprintf b "// BLACKLISTED\n";
            incr ngood;
            good := false;
          end;

          Printf.bprintf b "value %s(value self_v" stub_v;
          List.iter (fun (name, type_of_arg) ->
            Printf.bprintf b ", value %s_v" name
          ) args;
          Printf.bprintf b "){\n";
          Printf.bprintf b "  %s* self = %s_val(self_v);\n" class_name class_name;
          Printf.bprintf b "  if( self == NULL ) caml_failwith(\"%s: self is NULL\");\n" stub_v;
          List.iteri (fun i (name, argtype) ->
            Printf.bprintf b "  // arg%d (%S) " (i+1) name;
            List.iter (fun token -> Printf.bprintf b "%s "
              (string_of_token token)) argtype;
            Printf.bprintf b "\n";
            match argtype with
            | [Ident int64type ] when List.mem int64type int64types ->
              Printf.bprintf b "  %s %s = Int64_val(%s_v);\n"
                int64type name name;
            | [Ident "pid_t" ] ->
              Printf.bprintf b "  lldb::pid_t %s = Int64_val(%s_v);\n"
                name name;
            | [ Ident "const"; Ident "char"; Kwd "*"; Ident _; Kwd "["; Kwd "]" ]
            | [Ident "const"; Ident "char"; Kwd "**" ]
            | [Ident "char"; Ident "const"; Kwd "**" ] ->
              Printf.bprintf b "  const char** %s = (const char**) StringsOption_val(%s_v);\n"
                name name;
            | [Ident "char"; Kwd "**" ] ->
              Printf.bprintf b "  char** %s = StringsOption_val(%s_v);\n"
                name name;
            | [Ident "FILE"; Kwd "*" ] ->
              Printf.bprintf b "  FILE* %s = (FILE*)OCPLLDB_val(0, %s_v);\n"
                name name;
            | [Ident "double"; Kwd "*" ] ->
              Printf.bprintf b "  double* %s = (double*)%s_v;\n"
                name name;
            | [Ident "double" ] ->
              Printf.bprintf b "  double %s = Double_val(%s_v);\n"
                name name;
            | [Ident "float" ] ->
              Printf.bprintf b "  float %s = (float)Double_val(%s_v);\n"
                name name;
            | [ Ident inttype ] when List.mem inttype ("bool" :: inttypes) ->
              Printf.bprintf b "  %s %s = (%s)Long_val(%s_v);\n" inttype name inttype name;
            | [ Ident chartype; Kwd "*" ] when List.mem chartype chartypes ->
              Printf.bprintf b "  %s* %s = (%s*)String_val(%s_v);\n"
                chartype name chartype name;
            | [ Ident chartype; Kwd "*?" ] when List.mem chartype chartypes ->
              Printf.bprintf b "  %s* %s = (%s*)StringOption_val(%s_v);\n"
                chartype name chartype name;
            | [ Ident "void"; Kwd "*" ]
            | [ Ident "const"; Ident "void"; Kwd "*" ]
                when name = "buf" ->
              Printf.bprintf b "  char* %s = String_val(%s_v);\n" name name;
            | [ Ident "const"; Ident chartype; Kwd "*?" ]
                when List.mem chartype chartypes ->
              Printf.bprintf b "  const %s* %s = (%s*)StringOption_val(%s_v);\n"
                chartype name chartype name;
            | [ Ident "const"; Ident chartype; Kwd "*" ]
                when List.mem chartype chartypes ->
              Printf.bprintf b "  const %s* %s = (%s*)String_val(%s_v);\n"
                chartype name chartype name;
            | [ Ident class_name; Kwd "*" ]
                when isSBclass class_name ->
              Printf.bprintf b "  %s* %s = %s_val(%s_v);\n"
                class_name name class_name name;
            | [ Ident class_name ]
            | [ Ident class_name; Kwd "&" ]
                when isSBclass class_name ->
              Printf.bprintf b "  %s* %s = %s_val(%s_v);\n"
                class_name name class_name name;
            | [ Ident "const"; Ident class_name; Kwd "&" ] when isSBclass class_name ->
              Printf.bprintf b "  const %s* %s = %s_val(%s_v);\n"
                class_name name class_name name;
            | _ ->
              Printf.bprintf b "  /* arg type not implemented */\n";
              good := false;
              match argtype with
              | [ Ident typ ] ->
                unknown_types := StringSet.add typ !unknown_types
              | _ -> ()
          ) args;
          let call_method () =
            Printf.bprintf b "self->%s(" funname;

            List.iteri (fun i (name, argtype) ->
              if i > 0 then
                Printf.bprintf b ", ";
              match argtype with
              | [ Ident "const"; Ident class_name; Kwd "&" ]
              | [ Ident class_name; Kwd "&" ]
              | [ Ident class_name ]
                  when isSBclass class_name ->
                Printf.bprintf b "*%s" name;
              | _ -> Printf.bprintf b "%s" name;
            ) args;

            Printf.bprintf b ");\n";
          in
          let call_and_return b fmt =
            call_method ();
            List.iter (fun (argname, argtype) ->
              match argtype with
              | [ Ident "const"; Ident "char"; Kwd "*"; Ident _;
                  Kwd "["; Kwd "]" ]
              | [Ident "const"; Ident "char"; Kwd "**" ]
              | [Ident "char"; Ident "const"; Kwd "**" ]
              | [Ident "char"; Kwd "**" ] ->
                Printf.bprintf b "  free(%s);\n" argname;
              | _ -> ()
            ) args;
            begin match reply with
            | [Ident "const"; Ident chartype; Kwd "*" ]
            | [Ident chartype; Kwd "*" ]
                when List.mem chartype chartypes ->
              Printf.bprintf b "  if( ret == NULL ) caml_failwith(\"%s returned NULL string\");\n" stub_v;
            | _ -> ()
            end;
            Printf.bprintf b "  return ";
            Printf.bprintf b fmt
          in
          begin
            match reply with
            | [Ident "FILE"; Kwd "*"] ->
              Printf.bprintf b "  FILE* ret = ";
              call_and_return b "Val_abstract(0, ret);\n"
            | [Ident "const"; Ident name] when isSBclass name ->
              Printf.bprintf b "  %s* ret = new %s();\n"
                name name;
              Printf.bprintf b "  *ret = ";
              call_and_return b "Val_%s(ret);\n" name
            | [Ident name] when isSBclass name ->
              Printf.bprintf b "  %s* ret = new %s();\n"
                name name;
              Printf.bprintf b "  *ret = ";
              call_and_return b "Val_%s(ret);\n" name
            | [Ident "void"] ->
              Printf.bprintf b "  ";
              call_and_return b "Val_unit;\n"
            | [Ident chartype; Kwd "*" ] when List.mem chartype chartypes ->
              Printf.bprintf b "  %s* ret = " chartype;
              call_and_return b "caml_copy_string((char*)ret);\n"
            | [Ident "const"; Ident chartype; Kwd "*" ]
                when List.mem chartype chartypes ->
              Printf.bprintf b "  const %s* ret = " chartype;
                  call_and_return b "caml_copy_string((char*)ret);\n"
            | [Ident int64type ] when List.mem int64type int64types ->
              Printf.bprintf b "  %s ret = " int64type;
              call_and_return b "caml_copy_int64(ret);\n"
            | [Ident "pid_t" ] ->
              Printf.bprintf b "  lldb::pid_t ret = ";
              call_and_return b "caml_copy_int64(ret);\n"
            | [Ident "double" ] ->
              Printf.bprintf b "  double ret = ";
              call_and_return b "caml_copy_double(ret);\n"
            | [Ident "float" ] ->
              Printf.bprintf b "  float ret = ";
              call_and_return b "caml_copy_double(ret);\n"
            | [Ident inttype ] when
                List.mem inttype inttypes ->
              Printf.bprintf b "  %s ret = " inttype;
                  call_and_return b "Val_long(ret);\n"
            | [Ident "bool" ] ->
              Printf.bprintf b "  bool ret = ";
              call_and_return b "ret ? Val_true : Val_false;\n"
            | _ ->
              good := false;
              call_and_return b "Val_unit; /* not implemented */\n";

              match reply with
              | [ Ident typ ] ->
                unknown_types := StringSet.add typ !unknown_types
              | _ -> ()

          end;
          Printf.bprintf b "  // reply: ";
          List.iter (fun token ->
            Printf.bprintf b "%s " (string_of_token token)) reply;
          Printf.bprintf b "\n";
          Printf.bprintf b "}\n\n";

          if !good then begin

            let nargs = List.length args + 1 in
            if nargs > 5 then begin
              Printf.bprintf b
                "\n\nvalue %s_bytecode(value * argv, int argn)\n{\n"
                stub_v;
              Printf.bprintf b "   return %s" stub_v;
              for i = 0 to nargs - 1 do
                Printf.bprintf b "%c argv[%d]"
                  (if i > 0 then ',' else '(') i
              done;
              Printf.bprintf b ");\n}\n";
            end;

            Printf.fprintf mloc "  external %s : %s -> "
              (String.uncapitalize stubname)
              (sbClass class_name)
            ;
            List.iter (fun (argname, argtype) ->
              Printf.fprintf mloc "(* %s *)" argname;
              match argtype with
              | [Ident int64type ] when List.mem int64type int64types ->
                Printf.fprintf mloc " int64 -> "
              | [Ident "pid_t" ] ->
                Printf.fprintf mloc " int64 -> "
              | [ Ident "const"; Ident "char"; Kwd "*"; Ident _;
                  Kwd "["; Kwd "]" ]
              | [Ident "char"; Ident "const"; Kwd "**" ]
              | [Ident "const"; Ident "char"; Kwd "**" ]
              | [Ident "char"; Kwd "**" ] ->
                Printf.fprintf mloc " string array option -> "
              | [Ident "FILE"; Kwd "*" ] ->
                Printf.fprintf mloc " file -> "
              | [Ident "double"; Kwd "*" ] ->
                Printf.fprintf mloc " float array -> "
              | [Ident "double" ] ->
                Printf.fprintf mloc " float -> "
              | [Ident "float" ] ->
                Printf.fprintf mloc " float -> "
              | [ Ident "bool" ] ->
                Printf.fprintf mloc " bool -> "
              | [ Ident inttype ] when List.mem inttype inttypes ->
                Printf.fprintf mloc " int -> "
              | [ Ident chartype; Kwd "*" ]
              | [ Ident "const"; Ident chartype; Kwd "*" ]
                  when List.mem chartype chartypes ->
                Printf.fprintf mloc " string -> "
              | [ Ident chartype; Kwd "*?" ]
              | [ Ident "const"; Ident chartype; Kwd "*?" ]
                  when List.mem chartype chartypes ->
                Printf.fprintf mloc " string option -> "
              | [ Ident "void"; Kwd "*" ]
                  when argname = "buf" ->
                Printf.fprintf mloc " bytes -> "
              | [ Ident "const"; Ident "void"; Kwd "*" ]
                  when argname = "buf" ->
                Printf.fprintf mloc " string -> "
              | [ Ident class_name; Kwd "*" ]
              | [ Ident class_name ]
              | [ Ident "const"; Ident class_name; Kwd "&" ]
              | [ Ident class_name; Kwd "&" ]
                  when isSBclass class_name ->
                Printf.fprintf mloc " %s -> " (sbClass class_name)
              | _ ->
                assert false
            ) args;
            begin match reply with
            | [Ident "FILE"; Kwd "*"] ->
              Printf.fprintf mloc " file "
            | [Ident "const"; Ident name]
            | [Ident name] when isSBclass name ->
              Printf.fprintf mloc " %s " (sbClass name)
            | [Ident "void"] ->
              Printf.fprintf mloc " unit "
            | [Ident chartype; Kwd "*" ]
            | [Ident "const"; Ident chartype; Kwd "*" ]
                when List.mem chartype chartypes ->
              Printf.fprintf mloc " string "
            | [Ident int64type ] when List.mem int64type int64types ->
              Printf.fprintf mloc " int64 "
            | [Ident "pid_t" ] ->
              Printf.fprintf mloc " int64 "
            | [Ident "double" ] ->
              Printf.fprintf mloc " float "
            | [Ident "float" ] ->
              Printf.fprintf mloc " float "
            | [Ident inttype ] when
                List.mem inttype inttypes ->
              Printf.fprintf mloc " int "
            | [Ident "bool" ] ->
              Printf.fprintf mloc " bool "
            | _ -> assert false
            end;
            Printf.fprintf mloc " = ";
            if nargs > 5 then
              Printf.fprintf mloc " \"%s_bytecode\"\n" stub_v;
            Printf.fprintf mloc " %S\n" stub_v;
          end;
      end;
      if !good then begin
        Printf.fprintf oc "// GOOD\n%s" (Buffer.contents b);
        incr ngood;
      end else
        Printf.fprintf badoc "\n%s\n" (Buffer.contents b)
    ) methods;

    if !ntotal <> !ngood then
      Printf.eprintf "%d class %s: %d/%d\n%!" (!ntotal - !ngood) class_name !ngood !ntotal;

    Printf.fprintf mloc " (* %d missing for class %s: %d/%d *)\n" (!ntotal - !ngood) class_name (!ngood) !ntotal;
    Printf.fprintf mloc "end\n\n";

    ntotal_final := !ntotal_final + !ntotal;
    ngood_final := !ngood_final + !ngood;

  ) !classes;

  Printf.fprintf mloc "(* total: %d/%d *)\n" !ngood_final !ntotal_final;
  Printf.eprintf "total: %d/%d\n%!" !ngood_final !ntotal_final;
  close_out oc;
  close_out mloc;
  close_out badoc;

  Printf.eprintf "Unknown types: ";
  StringSet.iter (fun s -> Printf.eprintf "%s " s) !unknown_types;
  Printf.eprintf "\n%!"

