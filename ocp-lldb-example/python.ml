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


open LLDBOCaml
open LLDBEnums

let _ = SBDebugger.initialize ()

(* Set the path to the executable to debug *)
let exe = Sys.argv.(1)

(* Create a new debugger instance *)
let debugger = SBDebugger.create false

(* When we step or continue, don't return from the function until the
   process stops. Otherwise we would have to handle the process events
   ourselves which, while doable is a little tricky.  We do this by
   setting the async mode to false.  *)
let _ =
  SBDebugger.setAsync debugger false;
  Printf.printf "Creating a target for %S\n%!" exe

let target =
  SBDebugger.createTargetWithFileAndArch
    debugger exe "systemArch" (* lldb.LLDB_ARCH_DEFAULT *)

let print_instrs instrs =
  let stream = SBStream.create () in
  let (_ : bool) = SBInstructionList.getDescription instrs stream in
  let data = SBStream.getData stream in
  Printf.printf "instructions: %s\n%!" data;
  ()

let _ =
  if isNull target then begin
    Printf.eprintf "Error: could not create target\n%!";
    exit 2
  end;
    (* If the target is valid set a breakpoint at main *)
  let main_bp = SBTarget.breakpointCreateByRegex target "caml.*__entry"
    (Some (SBFileSpec.getFilename (SBTarget.getExecutable target))) in

  let print_breakpoint bp =
    let id = SBBreakpoint.getID bp in
    Printf.printf "Breakpoint %d\n%!" id;
    let nlocs = SBBreakpoint.getNumLocations bp in
    Printf.printf "Breakpoint set at %d locations\n%!" nlocs;
    let locs = Array.init nlocs (fun i ->
      SBBreakpoint.getLocationAtIndex bp i
    ) in
    Array.iteri (fun i loc ->
      Printf.printf "data = %s\n%!"
        (SBBreakpointLocation.to_string loc 1);
      ()
    ) locs
  in
  print_breakpoint main_bp;

  let process = SBTarget.launchVerySimple target [| exe |] (Sys.getcwd()) in
 if isNull process then begin
    Printf.eprintf "Error: could not start process\n%!";
    exit 2
  end;

  let print_process p = () in

  (* Print some simple process info *)
  let state = SBProcess.getState process in
  print_process process;

  match state with
  | EStateStopped ->
    Printf.eprintf "Process stopped\n%!";
    (* Get the first thread *)
    let thread = SBProcess.getThreadAtIndex process 0 in
    if isNull thread then begin
      Printf.eprintf "Error: could not get thread\n%!";
      exit 2
    end;
                (* Get the first frame *)
    let frame = SBThread.getFrameAtIndex thread 0 in
    if isNull frame then begin
      Printf.eprintf "Error: could not get frame\n%!";
      exit 2
    end;
    let ffunction = SBFrame.getFunction frame in
    if SBFunction.isValid ffunction then begin

      let name = SBFunction.getName ffunction in
      Printf.eprintf "Function = %S\n%!" name;

      (* Now get all instructions for this function and print them *)
      let insts = SBFunction.getInstructions ffunction target in

      let ninstrs = SBInstructionList.getSize insts in
      Printf.eprintf "%d instructions\n%!" ninstrs;
      (*
      let insts = Array.init ninstrs (fun i ->
        let ins = SBInstructionList.getInstructionAtIndex insts i in

      ) in
      *)
      print_instrs insts;
      ()
    end else begin
      (* See if we have a symbol in the symbol table for where we
         stopped *)
      let symbol = SBFrame.getSymbol frame in
      if isNull symbol then begin
        Printf.eprintf "Error: could not get symbol\n%!";
        exit 2
      end;
      (* We do have a symbol, print some info for the symbol *)
      let print_symbol s =
        let valid = SBSymbol.isValid s in
        if valid then begin

          let name = SBSymbol.getName s in
          Printf.printf "Symbol name = %S\n%!" name;

          Printf.printf "Symbol descr: %s\n%!" (SBSymbol.to_string s);
          let symtype = SBSymbol.getType s in
          Printf.printf "Symbol type = %s\n%!"
            (string_of_SymbolType symtype);

          let is_external = SBSymbol.isExternal s in
          let is_synthetic = SBSymbol.isSynthetic s in
          Printf.printf "is_external=%b is_synthetic=%b\n%!"
            is_external is_synthetic;

          let size = SBSymbol.getPrologueByteSize s in
          Printf.printf "PrologueByteSize: %d\n%!" size;

          let begin_addr = SBSymbol.getStartAddress s in
          let end_addr = SBSymbol.getEndAddress s in
          Printf.printf "Addresses: %s-%s\n%!"
            (SBAddress.to_string begin_addr)
            (SBAddress.to_string end_addr);

          let insts = SBFunction.getInstructions ffunction target in
          print_instrs insts;

          let insts = SBTarget.readInstructions target begin_addr 30 in
          print_instrs insts;

        end else
          Printf.printf "Symbol is not valid\n%!"
      in
      print_symbol symbol
    end
  | _ ->  assert false
