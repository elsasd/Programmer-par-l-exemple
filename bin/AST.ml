type program = expression list

and expression =
| Const of string
| Extract of pos_expression * pos_expression

and pos_expression =
| Forward of int
| Backward of int


