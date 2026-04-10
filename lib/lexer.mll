{
  open Parser
}

let white = [' ' '\t' '\n' '\r']+
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let numba = digit+
let ident = letter (letter | digit | '_')*

rule read = parse
  | white            { read lexbuf }
  | "if"             { KW_IF }
  | "then"           { KW_THEN }
  | "else"           { KW_ELSE }
  | "true"           { KW_TRUE }
  | "false"          { KW_FALSE }
  | "let"            { KW_LET }
  | "in"             { KW_IN }
  | "marshal"        { KW_MARSHAL }
  | "unmarshal"      { KW_UNMARSHAL }
  | "mark"           { KW_MARK }
  | "num"            { KW_NUM }
  | "unit"           { KW_UNIT }
  | "->"             { ARROW }
  | '('              { LPAREN }
  | ')'              { RPAREN }
  | '!'              { EXCLAMATION }
  | '?'              { QUESTION }
  | ':'              { COLON }
  | '.'              { PERIOD }
  | '\\'             { BACKSLASH }
  | '+'              { PLUS }
  | '='              { EQUALS }
  | "()"             { UNIT }
  | ident as id      { IDENT id }
  | numba as n       { NUM (int_of_string n) }
  | eof              { EOF }
  | _                { failwith "Unexpected character" }