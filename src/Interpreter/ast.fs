module ast
type Location = { Start: int; End: int; FileName: string; }

type Operation =
    | ADD | SUB | MUL | DIV
    | REM | EQ  | NEQ | LT
    | GT  | LTE | GTE | AND
    | OR  | NOT

[<RequireQualifiedAccess>]
type Term =
    | bool of Bool
    | int of Int
    | str of Str
    | var of Var
    | binary of Binary
    | call of Call
    | func of Func
    | let' of Let
    | if' of If
    | print of Print
    | first of First
    | second of Second
    | tuple of TupleTerm
    | file of FileAst
    | name of TermName
    | location of Location
    | expression of Term

and TermName = string
and Kind =  { kind: string; location: Location }
and Var = { text: string; kind: Kind }
and TupleTerm = { kind: Kind; first: Term; second: Term }
and First = { kind: Kind; value: Term }
and Second = { kind: Kind; value: Term }
and Print = { kind: Kind; value: Term }
and Func = { kind: Kind; value: Term; parameters: Term list }
and Call = { kind: Kind; callee: Term; arguments: Term list }
and Binary = { kind: Kind; lhs: Term; op: Operation; rhs: Term }
and Int = { kind: Kind; value: int }
and Bool = { kind: Kind; value: bool }
and Str = { kind: Kind; value: string }
and Let = { kind: Kind; name: Term; value: Term; next: Term  }
and If = { kind: Kind; condition: Term; then': Term; otherwise: Term  }
and FileAst =
    {
        name: Term
        expression: Term
        location: Term
    }