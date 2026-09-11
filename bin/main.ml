open Mobil.Frontend
open Mobil.Debug
open Mobil.Typing
open Printf

let interpret (source : string) =
  let lexbuf = Lexing.from_string source in
  let ast = Parser.prog Lexer.read lexbuf in
  let () = Graphviz.eval_graphviz stdout ast in
  let _ =
    try
      let et = Type_check.type_check ast in
      Some et
    with Failure msg ->
      eprintf "%s\n" msg;
      None
  in
  ()

let usage () = sprintf "Usage: %s <FILE>" Sys.argv.(0) |> prerr_endline

let usage_and_exit () =
  usage ();
  exit (-1)

(* Driver *)
let _ =
  let argc = Array.length Sys.argv in
  if argc < 2 then usage_and_exit ()
  else
    let _ = In_channel.with_open_text Sys.argv.(1) In_channel.input_all |> interpret in
    print_endline "Finished parsing with no errors..."
