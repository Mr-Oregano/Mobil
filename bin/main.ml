open Mobil
open Mobil.Frontend
open Mobil.Typing
open Mobil.Interpreter
open Printf

type output_mode =
  | Normal
  | PrintAST
  | PrintET

let interpret (source : string) (mode : output_mode) =
  let lexbuf = Lexing.from_string source in
  try
    let ast = Parser.prog Lexer.read lexbuf in
    (match mode with
    | PrintAST -> Syntax.Diagnostics.Graphviz.eval_graphviz stdout ast
    | PrintET ->
        let et = Type_check.type_check ast in
        Typing.Diagnostics.Graphviz.eval_graphviz stdout et
    | _ -> ());
    let et = Type_check.type_check ast in
    let res = Interpret.interpret et in
    Some res
  with Failure msg ->
    eprintf "Error: %s\n" msg;
    None

(* Driver *)
let () =
  let filename = ref "" in
  let mode = ref Normal in
  let speclist =
    [
      ( "--ast",
        Arg.Unit (fun () -> mode := PrintAST),
        "Output Graphviz representation of the untyped AST to stdout" );
      ( "--et",
        Arg.Unit (fun () -> mode := PrintET),
        "Output Graphviz representation of the type-checked AST (ET) to stdout" );
    ]
  in

  let usage_msg = sprintf "Usage: %s [OPTIONS] <FILE>" Sys.argv.(0) in

  let anon_arg arg =
    if !filename = "" then filename := arg else raise (Arg.Bad "Multiple input files provided")
  in

  Arg.parse speclist anon_arg usage_msg;

  if !filename = "" then begin
    eprintf "Error: No input file specified.\n\n";
    Arg.usage speclist usage_msg;
    exit 1
  end;

  let source = In_channel.with_open_text !filename In_channel.input_all in
  let result = interpret source !mode in

  Option.iter (eprintf "Result: %s\n") (Option.map Diagnostics.To_string.value_to_string result)
