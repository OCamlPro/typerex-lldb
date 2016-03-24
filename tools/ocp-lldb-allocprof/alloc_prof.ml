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

let get_frames thread =
  let n = SBThread.getNumFrames thread in
  let frames = Array.init n (SBThread.getFrameAtIndex thread) in
  Array.map SBFrame.getFunctionName frames

module StringMap = Map.Make(String)

type stack_multi_set = {
  mutable count : int;
  mutable children : stack_multi_set StringMap.t;
}

let empty () = {
  count = 0;
  children = StringMap.empty;
}

let rec set_add l s = match l with
  | [] -> s.count <- s.count + 1
  | h :: t ->
    let child =
      try StringMap.find h s.children with
      | Not_found ->
        let child = empty () in
        s.children <- StringMap.add h child s.children;
        child
    in
    set_add t child

let rec set_list head acc s =
  let acc =
    if s.count > 0
    then (head, s.count) :: acc
    else acc in
  StringMap.fold (fun k v acc -> set_list (k::head) acc v) s.children acc


let _ =
  if isNull target then begin
    Printf.eprintf "Error: could not create target\n%!";
    exit 2
  end;
  (* If the target is valid set a breakpoint at main *)
  let main_bp = SBTarget.breakpointCreateByName target "caml_garbage_collection"
      (Some (SBFileSpec.getFilename (SBTarget.getExecutable target))) in

  let nlocs = SBBreakpoint.getNumLocations main_bp in
  if nlocs = 0 then begin
    Printf.eprintf "Not an OCaml program\n%!";
    exit 2
  end;

  let process = SBTarget.launchVerySimple target [| exe |] (Sys.getcwd()) in
  if isNull process then begin
    Printf.eprintf "Error: could not start process\n%!";
    exit 2
  end;

  let set = empty () in
  let count = ref 0 in

  while true do
    match SBProcess.getState process with
    | EStateStopped ->
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

      incr count;
      if !count mod 1000 = 0 then Printf.printf "break %i\n%!" !count;
      let frames = get_frames thread in
      set_add (Array.to_list frames) set;
      (* Array.iter (Printf.printf "  %s\n") frames; *)
      (* Printf.printf "\n%!"; *)

      let err = SBProcess.continue process in
      if not (SBError.success err)
      then begin
        Printf.eprintf "Error: on continue: %s\n%!" (SBError.getCString err);
        exit 2
      end

    | EStateExited ->
      let rc = SBProcess.getExitStatus process in
      Printf.printf "exit %i\n%!" rc;
      let l = set_list [] [] set in
      List.iter (fun (l, n) ->
          List.iter (Printf.printf "%s ") l;
          Printf.printf ": %i\n" n) l;
      exit rc

    | state ->
      Printf.eprintf "unexpected state %s\n%!" (string_of_StateType state);
      exit 2
  done

