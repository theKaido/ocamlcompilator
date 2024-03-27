{
  open Parser

  exception Error of char
}

let num = ['0'-'9']
let alpha = ['a' - 'z' 'A' - 'Z']
let ident = alpha ( alpha | num | '_')*

rule token = parse
| eof             { Lend }
| [ ' ' '\t' ]    { token lexbuf }
| '\n'            { Lexing.new_line lexbuf; token lexbuf }
| num+ as n       { Lint (int_of_string n) }
| "true"          { Ltrue (true)}
| "false"         { Lfalse (false)}
| "def"           { Ldef }
| "return"        { Lret }
| "if"            { Lif }
| "then"          { Lthen }
| "while"         { Lwhile }
| "do"            { Ldo }
| "else"          { Lelse }
| "("             { Lopen }
| ")"             { Lclose }
| ';'             { Lsc }
| "="             { Leq }
| "*"             { Lmul }
| "+"             { Ladd }
| "-"             { Lsub }
| "/"             { Ldiv }
| "!="            { Lneq }
| "printf"        { Lprint }
| ident as id     { Lident (id)}
| _ as c          { raise (Error c) }
