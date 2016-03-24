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
type t = {
  mutable x : int;
  mutable y : string;
  mutable state : state;
}

and state =
| Inactive
| Pending of string
| Active of t

*)
open Types

let y =x

let initial = {
  x = 42;
  y = "42";
  state = Inactive;
}

let pending = { initial with state = Pending "Connection" }

let main () =
  let rec iter i t =
    if i > 0 then begin
      t.state <- (
        match t.state with
        | Inactive -> Pending "New Connection"
        | Pending s ->
          t.y <- s;
          t.x <- t.x + 1;
          Active pending
        | Active _ -> Inactive);
      iter (i-1) t
    end

  in
  iter 10 pending;
  Printf.eprintf "x = %d\n%!" pending.x

let _ =
  main ()
