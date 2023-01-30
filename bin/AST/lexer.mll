
{
    open Parser
    open Lexing
    exception Error of string

    let int = '-'? ['0'-'9'] ['0'-'9']*
    let white = [' ' '\t']+
    let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
    let layout = ' ' | '\t' | '\n'
    let newline = ('\010' | '\013' | "\013\010")
    let blank   = [' ' '\009' '\012']

rule read =
  parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "\""     { string(Buffer.create 32) lexbuf}
  | '('      { LEFT_PAREN }
  | ')'      { RIGHT_PAREN }
  | ':'      { COLON }
  | ','      { COMMA }
  | "const"  { CONST }
  | "extract"{ EXTRACT }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | newline           { next_line_and token lexbuf  }
  | blank+            { token lexbuf                }
  | eof               { EOF                         }
  | eof      { EOF }


and string buffer = parse
|'"'                                { STRING (Buffer.contents buffer) }
|'\n'                               { new_line lexbuf; Buffer.add_char buffer '\n'; string buffer lexbuf}
|'\\' (ascii as ch)                 { Buffer.add_char buffer (Char.chr(int_of_string(ch))); string buffer lexbuf}
|'\\' (character_speciaux as ch)    { Buffer.add_char buffer (spe_char_switch ch); string buffer lexbuf}
|'\\' (digit digit digit as asc)    { Buffer.add_char buffer (is_a_valid_ascii(int_of_string(asc))); string buffer lexbuf}
|'\\' (layout)                      { string buffer lexbuf }
| _ as ch                         { Buffer.add_char buffer ch; string buffer lexbuf}
| eof                               { error lexbuf "Unterminated string."}
