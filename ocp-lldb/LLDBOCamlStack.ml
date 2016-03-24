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


(* LLDB API *)
open LLDBEnums
open LLDBOCaml


(* ocp-lldb modules *)
open LLDBTypes
open LLDBGlobals

let arg_regs = [| "rax"; "rbx"; "rdi"; "rsi";
                  "rdx"; "rcx"; "r8" ; "r9";
                  "r12"; "r13" |]

let get_reg64_value regs name =
  let intregs = regs.(0) in
  let child = SBValue.getChildMemberWithName intregs name in
  let data = SBValue.getData child in
  SBData.getUnsignedInt64 data LLDBUtils.sbError 0

let print_args target mem heap nargs =
  let process = SBTarget.getProcess target in
  let thread = SBProcess.getSelectedThread process in
  let frame = SBThread.getSelectedFrame thread in
  let regs = SBFrame.getRegisters frame in
  let regs = SBValueList.to_array regs in

  let nargs = min (Array.length arg_regs) nargs in
  Printf.printf "Printing %d arguments:\n" nargs;

  for i = 0 to nargs - 1 do
    Printf.printf "arg[%d]=" i;
    LLDBOCamlValue.print_value target mem heap
      (get_reg64_value regs arg_regs.(i)) [];
    Printf.printf "\n%!";
  done;
  (*
  Array.iteri (fun i value ->
    let name = SBValue.getName value in
    let nchildren = SBValue.getNumChildren value in
    Printf.printf "reg %d = %S, %d children\n%!" i name nchildren;
    (*
reg 0 = "General Purpose Registers"
reg 1 = "Floating Point Registers"
reg 2 = "Advanced Vector Extensions"
    *)
    let children = Array.init nchildren (fun j ->
      let child = SBValue.getChildAtIndex value j in
      let name = SBValue.getName child in
      Printf.printf "reg %d = %S\n%!" i name;
      let v = SBValue.getData child in
      ()
    ) in
    ()
  ) regs;
  *)
  ()

let print_reg target mem heap reg =
  let process = SBTarget.getProcess target in
  let thread = SBProcess.getSelectedThread process in
  let frame = SBThread.getSelectedFrame thread in
  let regs = SBFrame.getRegisters frame in
  let regs = SBValueList.to_array regs in
  LLDBOCamlValue.print_value target mem heap
    (get_reg64_value regs reg) [];
