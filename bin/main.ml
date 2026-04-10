open Mobil
open Lexing
open Types
open Parser
open Lexer
open Printf

let interpret (source : string) =
  let lexbuf = Lexing.from_string source in
  Parser.prog Lexer.read lexbuf

let usage () = sprintf "Usage: %s <FILE>" Sys.argv.(0) |> prerr_endline

let usage_and_exit () =
  usage ();
  exit (-1)

(* Driver *)
let _ =
  let argc = Array.length Sys.argv in
  if argc < 1 then usage_and_exit ()
  else
    let _ = In_channel.with_open_text Sys.argv.(1) In_channel.input_all |> interpret in
    print_endline "Finished parsing with no errors..."
