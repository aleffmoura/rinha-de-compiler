module Converter

open ast;
open Newtonsoft.Json.Linq

let operation =
    function
    | "add" -> Operation.ADD
    | "sub" -> Operation.SUB
    | "mul" -> Operation.MUL
    | "div" -> Operation.DIV
    | "rem" -> Operation.REM
    | "eq"  -> Operation.EQ
    | "neq" -> Operation.NEQ
    | "lt"  -> Operation.LT
    | "gt"  -> Operation.GT
    | "lte" -> Operation.LTE
    | "gte" -> Operation.GTE
    | "and" -> Operation.AND
    | "or"  -> Operation.OR
    | _ -> Operation.NOT

let rec parse (jObject: JObject) : Term =
    let kind = jObject.["kind"].Value<string>();
    let location = jObject.["location"].ToObject<Location>()

    match kind with
    | "Bool" -> Term.bool { kind = { kind = kind; location = location; }; value = jObject.["value"].Value<bool>() }
    | "Int" -> Term.int { kind = { kind = kind; location = location; }; value = jObject.["value"].Value<int>() }
    | "Str" -> Term.str { kind = { kind = kind; location = location; }; value = jObject.["value"].Value<string>() }
    | "Var" -> Term.var { kind = { kind = kind; location = location; }; text = jObject.["text"].Value<string>() }
    | "Binary" ->
        Term.binary {
            kind = { kind = kind; location = location; }
            lhs = jObject.["lhs"].ToObject<JObject>() |> parse
            op = jObject.["op"].Value<string>() |> operation
            rhs = jObject.["rhs"].ToObject<JObject>() |> parse
        }
    | _ -> Term.var { kind = { kind = kind; location = location; }; text = jObject.["text"].Value<string>() }