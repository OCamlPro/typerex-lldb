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


(* We should define a complete protocol between the client and the
   server.  The server starts the program (the target/process), or
   attach to it, so it is not clear yet how we control the
   stdin/stdout/stderr of the target.

DEBUGGER-INTERFACE
(socket communication)
DEBUGGER-SERVER
(thread communication)
DEBUGGER-LLDB
(debugger communication)
PROCESS

*)

module LLDBControlLoop : sig

end = struct

end

type config = {
  x : unit;
}

open Lwt
open TelnetServer

let main config =
  let accept, s = TelnetServer.start 0 (fun s line ->
    Printf.printf "Line = %S\n%!" line;
    Lwt.async (fun () ->
      TelnetServer.write_string s line >>= fun () ->
      TelnetServer.write_string s "\r\n" >>= fun () ->
      Lwt.return ());
  ) in
  Printf.printf "Telnet interface on port %d\n%!" s.server_port;
  Lwt_unix.run (accept >>= fun () ->
                Printf.eprintf "Terminated\n%!"; Lwt.return_unit)
