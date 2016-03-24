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

let symbol_value target sym_name typ =
  let symcontextlist = SBTarget.findSymbols target sym_name ESymbolTypeAny in
  match SBSymbolContextList.getSize symcontextlist with
  | 0 ->
    Printf.printf "no symbol %s\n%!" sym_name;
    assert false
  | n ->
    let sctx = SBSymbolContextList.getContextAtIndex symcontextlist 0 in
    let sym = SBSymbolContext.getSymbol sctx in
    let addr = SBSymbol.getStartAddress sym in
    let typ = SBTarget.findFirstType target typ in
    SBTarget.createValueFromAddress target sym_name addr typ

let long_symbol_value target sym =
  SBValue.getValueAsUnsigned1 (symbol_value target sym "long") (-42L)

let double_symbol_value target sym =
  Int64.float_of_bits (long_symbol_value target sym)

let gc_stats target frame =
  let caml_stat_minor_words =
    double_symbol_value target "caml_stat_minor_words" in
  let caml_stat_promoted_words =
    double_symbol_value target "caml_stat_promoted_words" in
  let caml_stat_major_words =
    double_symbol_value target "caml_stat_major_words" in
  let caml_stat_minor_collections =
    long_symbol_value target "caml_stat_minor_collections" in
  let caml_stat_major_collections =
    long_symbol_value target "caml_stat_major_collections" in
  let caml_young_end = long_symbol_value target "caml_young_end" in
  let caml_young_ptr = long_symbol_value target "caml_young_ptr" in
  let caml_allocated_words =
    long_symbol_value target "caml_allocated_words" in

  let caml_stat_heap_wsz =
    long_symbol_value target "caml_stat_heap_wsz" in
  let caml_stat_top_heap_wsz =
    long_symbol_value target "caml_stat_top_heap_wsz" in
  let caml_stat_compactions =
    long_symbol_value target "caml_stat_compactions" in
  let caml_stat_heap_chunks =
    long_symbol_value target "caml_stat_heap_chunks" in

  let minwords =
    caml_stat_minor_words +.
    (Int64.to_float (Int64.div (Int64.sub caml_young_end caml_young_ptr) 8L)) in
  let majwords =
    caml_stat_major_words +.
    Int64.to_float caml_allocated_words in

  Printf.printf "minor_words: %.0f\n" minwords;
  Printf.printf "promoted_words: %.0f\n" caml_stat_promoted_words;
  Printf.printf "major_words: %.0f\n" majwords;
  Printf.printf "minor_collections: %Ld\n" caml_stat_minor_collections;
  Printf.printf "major_collections: %Ld\n" caml_stat_major_collections;
  Printf.printf "heap_words: %Ld\n" caml_stat_heap_wsz;
  Printf.printf "heap_chunks: %Ld\n" caml_stat_heap_chunks;
  Printf.printf "top_heap_words: %Ld\n" caml_stat_top_heap_wsz;
  Printf.printf "compactions: %Ld\n" caml_stat_compactions;
  flush stdout

let _ =
  if isNull target then begin
    Printf.eprintf "Error: could not create target\n%!";
    exit 2
  end;
  (* If the target is valid set a breakpoint at main *)
  let main_bp = SBTarget.breakpointCreateByName target "caml_sys_exit"
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

  let state = SBProcess.getState process in

  let () =
    match state with
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
      gc_stats target frame;
      let err = SBProcess.continue process in
      if not (SBError.success err)
      then begin
        Printf.eprintf "Error: on continue: %s\n%!" (SBError.getCString err);
        exit 2
      end

    | _ ->
      Printf.eprintf "unexpected state %s\n%!" (string_of_StateType state);
      exit 2
  in

  let state = SBProcess.getState process in
  match state with
  | EStateExited ->
    let rc = SBProcess.getExitStatus process in
    exit rc
  | _ ->
    Printf.eprintf "unexpected state %s\n%!" (string_of_StateType state);
    exit 2
