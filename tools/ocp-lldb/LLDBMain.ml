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


(* Our config files are different: we use 'exec "CMD"' to execute
   a command in the interpreter. Is it ok ? *)


(* LLDB API *)
open LLDBEnums
open LLDBOCaml

(* ocp-lldb modules *)
open LLDBTypes
open LLDBGlobals

let version = "0.1"

let handleCommand debugger interp cmd =
  Printf.eprintf "handleCommand: %s\n%!" cmd;
  let result = SBCommandReturnObject.create () in
  let _ret = SBCommandInterpreter.handleCommand interp cmd result false in
  if not (SBCommandReturnObject.succeeded result) then
    Printf.eprintf "Command %S failed\n%!" cmd;
  if !verbose then
      let (_:int) =
        SBCommandReturnObject.putError result
          (SBDebugger.getErrorFileHandle debugger) in
      let (_:int) =
        SBCommandReturnObject.putOutput result
          (SBDebugger.getOutputFileHandle debugger) in
      ()


let auto_start = ref true
let core_file = ref None
let attach_pid = ref None
let attach_name = ref None

let arg_version () =
  Printf.printf "%s\n%!" version;
  exit 0
let arg_core filename =
  core_file := Some filename
let arg_attach_pid pid =
  attach_pid := Some pid
let arg_attach_name name =
  attach_name := Some name

let args = ref []
let arg_anon file = args := file :: !args
let arg_usage = "ocp-lldb [OPTIONS] COMMAND WITH ARGS"
let arg_batch = ref false
let arg_wait_for = ref false
let arg_editor = ref false
let arg_no_lldbinit = ref false
let arg_no_use_colors = ref false
let arg_python_path = ref false
let arg_no_init = ref false
let server_mode = ref false

let arg_script_language _ = failwith "--script-language not implemented"

let arg_arch _ = failwith "--arch not implemented"
let arg_file _ = failwith "--file not implemented"
let arg_source _ = failwith "--source not implemented"
let arg_source_before_file _ = failwith "--source-before-file not implemented"
let arg_one_line _ = failwith "--one-line not implemented"
let arg_one_line_before_file _ = failwith "--one-line-before-file not implemented"
let arg_one_line_on_crash _ = failwith "--one-line-on-crash not implemented"
let arg_source_on_crash _ = failwith "--source-on-crash not implemented"
let arg_source_quietly _ = failwith "--source-quietly not implemented"

let arg2_list =
  [
    ["--server"], Arg.Set server_mode, [ " Run in server mode"];

    ["-d"], Arg.Set verbose, [" Add debug output"];
    ["-no-auto-start"], Arg.Clear auto_start, [" Do not start the program"];
    ["--"], Arg.Rest arg_anon, ["ARGS Add remaining arguments to command"];
    ["-v"; "--version"], Arg.Unit arg_version, [" Print version and exit."];
    ["-no-init"], Arg.Set arg_no_init, [ " Do not execute ~/.ocp/lldbinit" ];
    ["-c"; "--core"], Arg.String arg_core,
    ["PATH Tells the debugger to use the fullpath to PATH as the core file."];
    ["-p"; "--attach-pid"], Arg.Int arg_attach_pid,
    ["PID Tells the debugger to attach to process with pid PID"];
    ["-w"; "--wait-for"], Arg.Set arg_wait_for, [
      " Tells the debugger to wait for a process with the given pid or name";
      " to launch before attaching." ];
    ["-n"; "--attach-name"], Arg.String arg_attach_name, [
      "NAME Tells the debugger to attach to a process with the given name." ];

    ["--last-implemented-option"], Arg.Unit (fun _ -> ()),
    [" Remaining options have not been implemented"];
    ["-a"; "--arch"], Arg.String arg_arch, [
      "ARCH Tells the debugger to use the specified architecture when starting";
      " and running the program. ARCH must be one of the architectures for";
      " which the program was compiled." ];
    ["-f"; "--file"], Arg.String arg_file, [
      "FILENAME Tells the debugger to use the file FILENAME as the program to";
      " be debugged." ];
    [ "-s"; "--source"], Arg.String arg_source, [
      "FILE Tells the debugger to read in and execute the lldb commands in the";
      " given file, after any file provided on the command line has been";
      " loaded."];
    ["-o"; "--one-line"], Arg.String arg_one_line, [
      "LINE Tells the debugger to execute this one-line lldb command after any";
      " file provided on the command line has been loaded." ];
    ["-S";"--source-before-file"], Arg.String arg_source_before_file, [
      "FILE Tells the debugger to read in and execute the lldb commands in the";
      " given file, before any file provided on the command line has been";
      " loaded."];
    ["O";"--one-line-before-file"], Arg.String arg_one_line_before_file, [
      "LINE Tells the debugger to execute this one-line lldb command before any";
      " file provided on the command line has been loaded."];
    [ "-k"; "--one-line-on-crash"], Arg.String arg_one_line_on_crash, [
      "LINE When in batch mode, tells the debugger to execute this one-line";
      " lldb command if the target crashes." ];
    ["-K"; "--source-on-crash"], Arg.String arg_source_on_crash, [
      "FILE When in batch mode, tells the debugger to source this file of lldb";
      " commands if the target crashes." ];
    [ "-Q"; "--source-quietly"], Arg.String arg_source_quietly, [
      "LINE Tells the debugger to execute this one-line lldb command before";
      " any file provided on the command line has been loaded." ];
    [ "-b"; "--batch"], Arg.Set arg_batch, [
      " Tells the debugger to running the commands from -s, -S, -o & -O, and";
      " then quit.  However if any run command stopped due to a signal or";
      " crash, the debugger will return to the interactive prompt at the";
      " place of the crash." ];
    ["-e"; "--editor"], Arg.Set arg_editor, [
      " Tells the debugger to open source files using the host's \"external";
      " editor\" mechanism." ];
    [ "-x"; "--no-lldbinit"], Arg.Set arg_no_lldbinit,
    [ " Do not automatically parse any '.ocp-lldbinit' files." ];
    [ "-X"; "--no-use-colors"], Arg.Set arg_no_use_colors,
    [" Do not use colors."];
    [ "-P"; "--python-path"], Arg.Set arg_python_path,
    [ " Prints out the path to the lldb.py file for this version of lldb." ];
    [ "-l"; "--script-language"], Arg.String arg_script_language, [
      "LANG Tells the debugger to use the specified scripting language for";
      " user-defined scripts, rather than the default. Valid scripting";
      " languages that can be specified include Python, Perl, Ruby and Tcl.";
      "  Currently only the Python extensions have been implemented." ];
  ]

let _ =
  let arg_list = List.fold_left (fun list (arg_list, arg_spec, arg_usage) ->
    List.map (fun arg -> (arg, arg_spec, String.concat "" arg_usage)) arg_list
    @ list
  ) [] arg2_list in
  let arg_list = Arg.align arg_list in
  Arg.parse arg_list arg_anon arg_usage;
  let cmd =
    let args = List.rev !args in
    match args with
    | [] -> None
    | cmd :: args -> Some (cmd, args)
  in

  if !server_mode then
    LLDBLooper.main ()
  else

  SBDebugger.initialize ();
  let debugger = SBDebugger.create false in
  let interp = SBDebugger.getCommandInterpreter debugger in

  if not !arg_no_init then begin

    let result = SBCommandReturnObject.create () in
    SBCommandInterpreter.sourceInitFileInHomeDirectory interp result;

    if !verbose then begin

      let (_:int) =
        SBCommandReturnObject.putError result
          (SBDebugger.getErrorFileHandle debugger) in
      let (_:int) =
        SBCommandReturnObject.putOutput result
          (SBDebugger.getOutputFileHandle debugger) in
      ()
    end;
  end;

  (* signal handling problem: signals are caught in OCaml, but if no
     OCaml code is executed, they will never by executed until an
     OCaml command is called. We should probably create our own event
     loop just for that. Can we ?
     let set_signal signo handler =
     let (_ : Sys.signal_behavior) = Sys.signal signo handler in
     ()
     in
     let interrupt_sent = ref false in
     set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
     Printf.printf "CTRL-C received\n%!";
     if not !interrupt_sent then begin
     interrupt_sent := true;
     Printf.printf "Sending CTRL-C to target...%!";
     SBDebugger.dispatchInputInterrupt debugger;
     Printf.printf "done\n%!";
     interrupt_sent := false
     end
     ));
     let rec before_signal f signo =
     f debugger;
     set_signal signo Sys.Signal_default;
     Unix.kill (Unix.getpid()) signo;
     set_signal signo (Sys.Signal_handle (before_signal f));
     ()
     in
     set_signal Sys.sigtstp (
     Sys.Signal_handle (before_signal SBDebugger.saveInputTerminalState));
     set_signal Sys.sigcont (
     Sys.Signal_handle (before_signal SBDebugger.restoreInputTerminalState));
  *)

  SBCommandInterpreter.addCommand interp
    "ocaml" (LLDBOCamlCommand.ocaml_command debugger) "help for ocaml";

  Printf.printf "***\n***    Welcome to the ocp-lldb debugger\n***\n";
  Printf.printf
    "***Use 'ocaml help' to get more information on OCaml commands\n***\n%!";
  (*  let target = SBDebugger.createTargetByName debugger cmd in *)

  begin match cmd with
  | Some (cmd, args) ->
    let create_target_cmd =
      let b = Buffer.create 1000 in

      Printf.bprintf b "target create";
    (* arch stuff:
       if arch <> SBDebugger.getDefaultArchitecture debugger then
       Printf.bprintf b " --arch=%S" arch;
    *)
      Printf.bprintf b " %S" cmd;
      begin match !core_file with
      | None -> ()
      | Some core_file ->
        Printf.bprintf b " --core %S" core_file
      end;

      Buffer.contents b in
    handleCommand debugger interp create_target_cmd;

    begin match args with
    | [] -> ()
    | _ ->
      handleCommand debugger interp (
        Printf.sprintf "settings set -- target.run-args \"%s\""
          (String.concat "\" \"" (List.map String.escaped args))
      )
    end;

  if !auto_start then begin
    let target = SBDebugger.getSelectedTarget debugger in
    let (_bp : LLDBOCaml.sbBreakpoint) = SBTarget.breakpointCreateByName
      target "caml_program" None in
    Printf.printf "Target: %s\n%!" (String.concat " " args);
    let _process = SBTarget.launchVerySimple target
      (Array.of_list args) (Sys.getcwd()) in
    ()
  end;


  | None ->
    match !attach_pid with
    | Some pid ->
      handleCommand debugger interp (
        Printf.sprintf "process attach --pid %d" pid)
    | None ->
      match !attach_name with
      | None ->
        Arg.usage arg_list arg_usage; exit 2
      | Some name ->
        handleCommand debugger interp (
          (Printf.sprintf "process attach --name %S%s" name)
            (if !arg_wait_for then " --waitfor" else ""))
  end;

  SBDebugger.runCommandInterpreter debugger
    (*~handle_events:*) true (* ~spawn_thread:*) false;
  SBDebugger.destroy debugger;
  SBDebugger.terminate ()
