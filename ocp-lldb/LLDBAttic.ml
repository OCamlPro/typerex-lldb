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

  (*
    | [| "resume" |] ->
    let nthreads = SBProcess.getNumThreads process in
    Printf.eprintf "Nthreads = %d\n%!" nthreads;
    let _threads = Array.init nthreads (fun i ->
    SBProcess.getThreadAtIndex process i) in
    (* SBThread.resume cannot be used to resume a thread ?

    Array.iter (fun thread ->
    Printf.eprintf "trying to resume thread...\n%!";
    ignore (SBThread.resume thread)) threads;
  *)
    if SBError.success (SBProcess.continue process) then
    Printf.eprintf "Process resumed\n%!"
    else
    Printf.eprintf "Error while resuming process\n%!";
  *)

(*
  | [| "list"; "modules" |] ->

    let modules = get_modules debugger in
    Array.iteri (fun i m ->
      Printf.printf "%d -> 0x%Lx, %S %s\n%!" i
        m.mod_loadaddr m.mod_name m.mod_addr
      ) modules
*)

          (*

    (*  handleCommand interp "break set -n main"; *)

        SBBreakpoint.setCallback bp (fun process t l ->
        Printf.eprintf "CALLBACK REACHED\n%!";

    (* It seems that we cannot restart the process from here !
        if SBError.success (SBProcess.continue process) then
        Printf.eprintf "Process resumed\n%!"
        else
        Printf.eprintf "Error while resuming process\n%!";
      *)
        false
        );

  (*  handleCommand interp "run"; *)
      *)

      (*
        let listener = SBDebugger.getListener dbg in

        let stopped = ref false in
        let event = SBEvent.create () in
        while not !stopped do
        let _ : bool = SBListener.waitForEvent listener 2 event in
        let t = SBEvent.getType event in
        match t with
        | EBroadcastBitStateChanged ->
        let state_type = SBProcess.getState process in
        begin match state_type with
        | EStateStopped ->
        begin
        Printf.eprintf "Process stopped in state %s\n%!"
        (string_of_StateType state_type);

        let nthreads = SBProcess.getNumThreads process in
        Printf.eprintf "Nthreads = %d\n%!" nthreads;
        let _threads = Array.init nthreads (fun i ->
        SBProcess.getThreadAtIndex process i) in
        if SBError.success (SBProcess.continue process) then
        Printf.eprintf "Process resumed\n%!"
        else
        Printf.eprintf "Error while resuming process\n%!";

    (*
        Array.iter (fun thread ->
        Printf.eprintf "trying to resume thread...\n%!";
        ignore (SBThread.resume thread)) threads;

        SBDebugger.runCommandInterpreter dbg
        ~handle_events:true ~spawn_thread:false;
      *)
        end

        | EStateCrashed
        | EStateDetached
        | EStateExited ->
        Printf.eprintf "Program exited !\n%!";
        stopped := true
        | st ->
        Printf.eprintf "Received %s\n%!"
        (string_of_StateType st)
        end
        | ENoEvent
        | EBroadcastBitInterrupt
        | EBroadcastBitSTDOUT
        | EBroadcastBitSTDERR
        | EBroadcastBitProfileData ->
        Printf.eprintf "event %s\n%!" (string_of_EventType t)
        done;
      *)

          (*

    (*  handleCommand interp "break set -n main"; *)

        SBBreakpoint.setCallback bp (fun process t l ->
        Printf.eprintf "CALLBACK REACHED\n%!";

    (* It seems that we cannot restart the process from here !
        if SBError.success (SBProcess.continue process) then
        Printf.eprintf "Process resumed\n%!"
        else
        Printf.eprintf "Error while resuming process\n%!";
      *)
        false
        );

  (*  handleCommand interp "run"; *)
      *)

      (*
        let listener = SBDebugger.getListener dbg in

        let stopped = ref false in
        let event = SBEvent.create () in
        while not !stopped do
        let _ : bool = SBListener.waitForEvent listener 2 event in
        let t = SBEvent.getType event in
        match t with
        | EBroadcastBitStateChanged ->
        let state_type = SBProcess.getState process in
        begin match state_type with
        | EStateStopped ->
        begin
        Printf.eprintf "Process stopped in state %s\n%!"
        (string_of_StateType state_type);

        let nthreads = SBProcess.getNumThreads process in
        Printf.eprintf "Nthreads = %d\n%!" nthreads;
        let _threads = Array.init nthreads (fun i ->
        SBProcess.getThreadAtIndex process i) in
        if SBError.success (SBProcess.continue process) then
        Printf.eprintf "Process resumed\n%!"
        else
        Printf.eprintf "Error while resuming process\n%!";

    (*
        Array.iter (fun thread ->
        Printf.eprintf "trying to resume thread...\n%!";
        ignore (SBThread.resume thread)) threads;

        SBDebugger.runCommandInterpreter dbg
        ~handle_events:true ~spawn_thread:false;
      *)
        end

        | EStateCrashed
        | EStateDetached
        | EStateExited ->
        Printf.eprintf "Program exited !\n%!";
        stopped := true
        | st ->
        Printf.eprintf "Received %s\n%!"
        (string_of_StateType st)
        end
        | ENoEvent
        | EBroadcastBitInterrupt
        | EBroadcastBitSTDOUT
        | EBroadcastBitSTDERR
        | EBroadcastBitProfileData ->
        Printf.eprintf "event %s\n%!" (string_of_EventType t)
        done;
      *)




    (*
    let bp = SBTarget.breakpointCreateByRegex target "caml.*__entry"
      (Some (SBFileSpec.getFilename (SBTarget.getExecutable target))) in

    (* let id = SBBreakpoint.getID bp in *)
    (*        Printf.printf "Breakpoint %d\n%!" id; *)
    let nlocs = SBBreakpoint.getNumLocations bp in
    (*        Printf.printf "Breakpoint set at %d locations\n%!" nlocs; *)
    let locs = Array.init nlocs (fun i ->
      SBBreakpoint.getLocationAtIndex bp i
    ) in
    let locs = Array.mapi (fun i loc ->
      (*          Printf.printf "data = %s\n%!"
                  (SBBreakpointLocation.to_string loc 1); *)

      let addr = SBBreakpointLocation.getAddress loc in
      let sym = SBAddress.getSymbol addr in
      let loadaddr = SBAddress.getOffset addr in
      let name = SBSymbol.getName sym in
      (loadaddr, name, SBAddress.to_string addr)
    ) locs in
    Array.sort compare locs;
    let modules = Array.mapi (fun i (mod_loadaddr, mod_name, mod_addr) ->
      { mod_loadaddr; mod_name; mod_addr }
    ) locs
    in
    SBBreakpoint.clearAllBreakpointSites bp;
    *)
