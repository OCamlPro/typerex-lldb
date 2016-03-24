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


#ifndef OCAML_NON_OCP
val print_module_globals :
  LLDBGenerated.sbTarget ->
  LLDBTypes.mem_info ->
  LLDBTypes.heap_info ->
  string ->
  unit

val print_globals : LLDBGenerated.sbTarget -> unit

val print_module_global :
  LLDBGenerated.sbTarget ->
  LLDBTypes.mem_info ->
  LLDBTypes.heap_info ->
  string ->
  string ->
  unit
#endif

val load_globals_map :
  LLDBGenerated.sbTarget ->
  int (* module currently being evaluated *)
  * (string * Digest.t * Digest.t * string list) array
