module Converter

open ast;
open Newtonsoft.Json.Linq
open System.Linq

let operation (op: string) =
    match op.ToLower()  with
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
    
let getJObject (str: string) (jObject: JToken) =
    jObject.[str].ToObject<JObject> ()

let asJObject (jObject: JToken) =
    jObject.ToObject<JObject> ()


let parseFile (jObject: JObject) (parse) : FileAst = 
    {
        name = jObject.["name"].Value<string>() |> Term.name
        expression = getJObject "expression" jObject |> parse
        location = jObject.["location"].ToObject<Location>() |> Term.location
    }

let rec parse (jObject: JObject) : Term =
    let kind = match (jObject.TryGetValue "kind") with
               | (true, kindValue) -> kindValue.Value<string>()
               | _ -> match jObject.TryGetValue "expression" with
                      | (true, _) -> "fileConverter"
                      | _ -> "Var"
    
    let location = jObject.["location"].ToObject<Location>()

    match kind with
    | "fileConverter" -> parseFile jObject parse |> Term.file
    | "name" ->  jObject.["name"].Value<string>() |> Term.name
    | "expression" ->  jObject.["expression"] |> asJObject |> parse
    | "location" -> location |> Term.location
    | "Bool" -> Term.bool { kind = { kind = kind; location = location; }; value = jObject.["value"].Value<bool>() }
    | "Int" -> Term.int { kind = { kind = kind; location = location; }; value = jObject.["value"].Value<int>() }
    | "Str" -> Term.str { kind = { kind = kind; location = location; }; value = jObject.["value"].Value<string>() }
    | "Var" -> Term.var { kind = { kind = kind; location = location; }; text = jObject.["text"].Value<string>() }
    | "Binary" ->
        Term.binary {
            kind = { kind = kind; location = location; }
            lhs = getJObject "lhs" jObject |> parse
            op = jObject.["op"].Value<string>() |> operation
            rhs =  getJObject "rhs" jObject |> parse
        }
    | "Call" -> 
        Term.call {
            kind = { kind = kind; location = location; }
            callee = getJObject "callee" jObject |> parse 
            arguments = jObject.["arguments"].Select(fun a -> asJObject a |> parse) |> Seq.toList
        }
    | "Function" ->
        Term.func {
            kind = { kind = kind; location = location; }
            parameters = jObject.["parameters"].Select(fun p -> asJObject p |> parse) |> Seq.toList
            value = getJObject "value" jObject  |> parse
        }
    | "Let" ->
        Term.let' {
            kind = { kind = kind; location = location; }
            name =  getJObject "name" jObject |> parse
            value = getJObject "value" jObject  |> parse
            next = jObject |>  getJObject "next" |> parse
        }
    | "If" ->
        Term.if' {
            kind = { kind = kind; location = location; }
            condition = getJObject "condition" jObject |> parse
            then' = getJObject "then" jObject |> parse
            otherwise = getJObject "otherwise" jObject |> parse
        }
    | "Print" ->
        Term.print {
            kind = { kind = kind; location = location; }
            value = getJObject "value" jObject |> parse
        }
    | "First" ->
        Term.first {
            kind = { kind = kind; location = location; }
            value = getJObject "value" jObject |> parse
        }
    | "Second" ->
        Term.second {
            kind = { kind = kind; location = location; }
            value = getJObject "value" jObject |> parse
        }
    | "Tuple" -> 
        Term.tuple {
            kind = { kind = kind; location = location; }
            first = getJObject "first" jObject |> parse
            second = getJObject "second" jObject |> parse
        }
    | _ -> Term.var { kind = { kind = kind; location = location; }; text = jObject.["text"].Value<string>() }
