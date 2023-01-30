%token <int> INT
%token <string> ID
%token <string> STRING
%token TRUE
%token FALSE
%token NULL
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_GUILL
%token RIGHT_GUILL
%token COLON
%token COMMA
%token FORWARD
%token BACKWARD
%token EOF


%start <Json.value option> prog
%%

prog:
    |EOF        { None }
    |v = expression  { Some v }
;
expression:
    | k = ID; LEFT_PAREN; x1 = pos_exp; COMMA; x2 = pos_exp; RIGHT_PAREN
        { Extract( x1, x2 )  }
    | k = ID; LEFT_PAREN; LEFT_GUILL; s = literal; RIGHT_GUILL; RIGHT_PAREN
        { Const( s ) }
;

pos_exp:
    | FORWARD; LEFT_PAREN; val = literal; RIGHT_PAREN
        { PosExp(val) }
    | BACKWARD; LEFT_PAREN; val = literal; RIGHT_PAREN
        { PosExp(val) }

literal:
    i=INT     { LInt i }
  | c=CHAR    { LChar c }
  | s=STRING  { LString s }

identifier:
    id=ID { Id id }