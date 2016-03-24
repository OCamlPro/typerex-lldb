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
  open Lwt

  type server = {
    server_port : int;
  }

  and sock = {
    sock_fd : Lwt_unix.file_descr;
    sock_buf : bytes;
    mutable sock_len : int;
    sock_addr : Unix.sockaddr;
    sock_server : server;
  }

  let lwt_unix_read fd s pos len =
    Lwt_unix.read fd (Bytes.unsafe_to_string s) pos len

  let verbose = ref false

  let sock_maxlen = 32_764

  let rec read_line s line_handler =
    lwt_unix_read s.sock_fd s.sock_buf s.sock_len (sock_maxlen - s.sock_len)
    >>= parse_line s line_handler

  and parse_line s line_handler nread =
    if nread = 0 then begin (* connection closed ? *)
      if !verbose then
        Printf.eprintf "get_http_header: closed\n%!";
      Lwt_unix.close s.sock_fd
    end else begin
      if !verbose then
        Printf.eprintf "read %d bytes\n%!" nread;
      let pos = max s.sock_len 0 in
      s.sock_len <- s.sock_len + nread;
      let rec iter pos =
        if pos = s.sock_len then
          read_line s line_handler
        else
          match Bytes.get s.sock_buf pos with
          | '\r' | '\n' ->
            let line = Bytes.sub_string s.sock_buf 0 pos in
            let newlen = s.sock_len - pos - 1 in
            if !verbose then
              Printf.eprintf "newlen=%d\n%!" newlen;
            Bytes.blit s.sock_buf (pos+1) s.sock_buf 0 newlen;
            s.sock_len <- newlen;
            if line <> "" then line_handler s line;
            iter 0
          | _ -> iter (pos+1)
      in
      iter pos
    end

  let connection_handler s line_handler =
    Lwt.catch (fun () ->
      read_line s line_handler)
      (fun exn ->
      if !verbose then
        Printf.eprintf "connection_handler: %s. Closing\n%!"
          (Printexc.to_string exn);
        Lwt_unix.close s.sock_fd
      )

  let start initial_port line_handler =
    let s = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let addr = Unix.inet_addr_of_string "127.0.0.1" in
    Lwt_unix.bind s (Unix.ADDR_INET (addr, initial_port));
    Lwt_unix.listen s 10;
    let server_port = match Lwt_unix.getsockname s with
      Unix.ADDR_INET (_, port) ->
        if !verbose then
          Printf.eprintf "Bound to port %d\n%!" port;
        port
    | _ -> assert false
    in
    let server = {server_port} in
    let rec connections () =
      Lwt_unix.accept s >>= fun (fd, sockaddr) ->
      let sock = {
        sock_fd = fd;
        sock_buf = Bytes.create sock_maxlen;
        sock_len = 0;
        sock_addr = sockaddr;
        sock_server = server;
      } in
      Lwt.join [connection_handler sock line_handler; connections ()]
    in
    connections (), server

  let write_string s ss =
    let len = String.length ss in
    Lwt_unix.write s.sock_fd ss 0 len >>=
      (fun wlen ->
        assert (wlen = len);
        Lwt.return ())
