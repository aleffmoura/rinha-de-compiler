module parser
open ast;
open System

let mutable variableEnvironment = Map.empty<string, obj>
let mutable functionEnvironment = Map.empty<string, Func> 

let interpretIf (ef: If) parse =
    if parse (Term.if' ef) |>  unbox<bool>
    then parse ef.then'
    else parse ef.otherwise
let interpretPrint (print: Print) parse println =
    let value = parse print.value
    println value
    value
let interpretLet (let' : Let) (parse: Term -> obj) =
    let txt = match let'.name with
              | Term.var var -> var.text
              | _ -> failwith "Variable is not defined."

    let value =
        match parse let'.value with
        | :? Func as func ->
            let updatedFunctionEnvironment = functionEnvironment.Add(txt, func)
            functionEnvironment <- updatedFunctionEnvironment
            box func
        | value -> value

    variableEnvironment |> Map.add txt value |> ignore

    parse let'.next
let interpretVar (var: Var) =
    match Map.tryFind var.text variableEnvironment with
    | Some some -> some
    | None -> failwith $"'{var.text}' is not defined."
let rec InterpretCall (call : Call) parse =
    match call.callee with
    | Term.var callee ->
        match functionEnvironment |> Map.tryFind callee.text with
        | Some func ->
            if func.parameters.Length <> call.arguments.Length then
                failwith ($"Function '{callee.text}' expects {func.parameters.Length} arguments but got {call.arguments.Length}.")
        
            let newVariableEnvironment =
                List.map2 (fun parameter argument ->
                    match parameter with
                    | Term.var varParameter ->
                        let newValue = parse argument
                        (varParameter.text, newValue)
                    | _ -> failwith ("Parameter must be a variable")
                ) call.arguments func.parameters
                |> List.fold (fun env (key, value) -> Map.add key value env) variableEnvironment

            let oldVariableEnvironment = variableEnvironment
            variableEnvironment <- newVariableEnvironment
            let result = parse func.value
            variableEnvironment <- oldVariableEnvironment
            result
        | None -> failwith $"Function '{callee.text}' is not defined."
    | _ -> failwith "Function call must have a variable callee."
let interpretFirst (fst: First) parse =
    match parse fst.value |> unbox with
    | Term.tuple tuple -> parse tuple.first
    | _ ->  failwith ""
let interpretSnd (fst: Second) parse =
    match parse fst.value |> unbox with
    | Term.tuple tuple -> parse tuple.second
    | _ ->  failwith ""
let interpretTuple (tuple: TupleTerm) parse =
    parse tuple.first, parse tuple.second
let binaryParser bin parse =
    let leftResult =  parse bin.lhs |> unbox<int>
    let rightResult = parse bin.rhs |> unbox<int>  

    match bin.op with 
    | ADD ->  leftResult +  rightResult |> box
    | SUB -> leftResult -  rightResult  |> box
    | MUL -> leftResult *  rightResult  |> box
    | DIV -> leftResult /  rightResult  |> box
    | REM -> leftResult %  rightResult  |> box
    | EQ -> leftResult = rightResult    |> box
    | NEQ -> leftResult <> rightResult  |> box
    | LT -> leftResult < rightResult    |> box
    | GT -> leftResult > rightResult    |> box
    | LTE -> leftResult <= rightResult  |> box
    | GTE ->  leftResult >= rightResult |> box
    | AND ->  unbox<bool> leftResult && unbox<bool> rightResult
    | OR -> unbox<bool> leftResult || unbox<bool> rightResult
    | NOT ->  unbox<bool> rightResult   |> not |> box

let rec interpreter term = 
    match term with
    | Term.var var -> interpretVar var
    | Term.binary bin -> binaryParser bin interpreter
    | Term.let' var -> interpretLet var interpreter
    | Term.tuple tuple -> interpretTuple tuple interpreter
    | Term.first fst -> interpretFirst fst interpreter
    | Term.second snd -> interpretSnd snd interpreter
    | Term.if' ef -> interpretIf ef interpreter
    | Term.call call ->  InterpretCall call interpreter
    | Term.func func -> func
    | Term.bool bool' -> bool'.value
    | Term.int int' -> int'.value
    | Term.str str -> str.value
    | Term.print print -> interpretPrint print interpreter Console.WriteLine
