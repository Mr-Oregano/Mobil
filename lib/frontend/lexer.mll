{
  open Parser
}

let white = [' ' '\t' '\n' '\r']+
let comment = '#' [^ '\n' '\r']* 
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let numba = digit+
let ident = letter (letter | digit | '_')*

rule read = parse
  | white            { read lexbuf }
  | comment          { read lexbuf }
  | "if"             { KW_IF }
  | "then"           { KW_THEN }
  | "else"           { KW_ELSE }
  | "true"           { KW_TRUE }
  | "false"          { KW_FALSE }
  | "let"            { KW_LET }
  | "in"             { KW_IN }
  | "chan"           { KW_CHAN }
  | "marsh"          { KW_MARSH }
  | "marshal"        { KW_MARSHAL }
  | "unmarshal"      { KW_UNMARSHAL }
  | "num"            { KW_NUM }
  | "bool"           { KW_BOOL }
  | "unit"           { KW_UNIT }
  | "->"             { ARROW }
  | '('              { LPAREN }
  | ')'              { RPAREN }
  | '{'              { LBRACE }
  | '}'              { RBRACE }
  | '['              { LBRACK }
  | ']'              { RBRACK }
  | '!'              { EXCLAMATION }
  | '?'              { QUESTION }
  | ':'              { COLON }
  | '.'              { PERIOD }
  | ','              { COMMA }
  | '\\'             { BACKSLASH }
  | '+'              { PLUS }
  | '-'              { MINUS }
  | '='              { EQUALS }
  | "()"             { UNIT }
  | "_"              { WILDCARD }
  | ident as id      { IDENT id }
  | numba as n       { NUM (int_of_string n) }
  | eof              { EOF }
  | _                { failwith "Unexpected character" }