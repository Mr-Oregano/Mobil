open Mobil
open Lexing
open Types
open Parser
open Lexer

let () =
  let input = "10 + 32" in
  let lexbuf = Lexing.from_string input in
  let _ = Parser.prog Lexer.read lexbuf in
  print_endline "Finished"
