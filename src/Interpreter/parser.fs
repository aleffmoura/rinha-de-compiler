module parser
open ast;
open System

let mutable variables = Map.empty<string, obj>
let mutable functionsVariables = Map.empty<string, Func> 

let parserIf (ef: If) parse =
    if parse ef.condition |>  unbox<bool>
    then parse ef.then'
    else parse ef.otherwise

let parserPrint (print: Print) parse println =
    let value = parse print.value
    println value
    value

let parseLet (let' : Let) (parse: Term -> obj) =
    let txt = match let'.name with
              | Term.var var -> var.text
              | _ -> failwith "Variable is not defined."
    let value = parse let'.value 

    variables <- variables.Add(txt, value) ;

    if value :? Func then
        let func = value :?> Func // Converte value para o tipo Func
        let updatedFunctionEnvironment = functionsVariables.Add(txt, func)
        functionsVariables <- updatedFunctionEnvironment
    else ()

    parse let'.next

let parserVar (var: Var) =
    match Map.tryFind var.text variables with
    | Some some -> some
    | None -> failwith $"'{var.text}' is not defined."

let rec parserCall (call : Call) (parse: Term -> obj) =
    match call.callee with
    | Term.var callee ->
        match functionsVariables |> Map.tryFind callee.text with
        | Some func ->
            if func.parameters.Length <> call.arguments.Length then
                failwith ($"'{callee.text}' expect {func.parameters.Length} arguments but only has {call.arguments.Length}.")
        
            let mutable newVariables = Map.ofList (Map.toList variables)

            List.map2 (fun (parameter: Term) argument ->
                match (parameter, argument) with
                | (Term.var varParameter,  varArgument) ->
                    varArgument
                    |> parse
                    |> (fun args -> newVariables.Add(varParameter.text, args))
                    |> (fun v -> newVariables <- v) 
                | _ -> ()
            ) func.parameters call.arguments 
            |> ignore
            
            let oldEnvironment = variables;
            variables <- newVariables;
            let result = parse func.value 
            variables <- oldEnvironment;
            result
        | None -> failwith $"'{callee.text}' is not defined."
    | _ -> failwith "Call doesnt have a variable callee."

let parserFirst (fst: First) parse =
    match parse fst.value |> unbox with
    | Term.tuple tuple -> parse tuple.first
    | _ ->  failwith ""

let parserSnd (snd: Second) parse =
    match parse snd.value |> unbox with
    | Term.tuple tuple -> parse tuple.second
    | _ ->  failwith ""

let interpretTuple (tuple: TupleTerm) parse =
    parse tuple.first, parse tuple.second

let castAsInt (obj: obj) =
    match obj with
    | :? bool as b -> if b then 1 else 0
    | _ -> obj |> unbox<int>

let castAsBool (value: int) =
    match value with
    | 0 -> false
    | _ -> true

let binaryParser bin parse =
    let leftResult =  parse bin.lhs |> castAsInt
    let rightResult = parse bin.rhs |> castAsInt

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
    | AND ->  castAsBool leftResult && castAsBool rightResult
    | OR -> castAsBool leftResult || castAsBool rightResult
    | NOT -> rightResult |> unbox<bool> |> not |> box

let interpreter term = 
    variables <- Map.empty<string, obj>
    functionsVariables <- Map.empty<string, Func> 

    let rec parser term = 
        match term with
        | Term.var var -> parserVar var
        | Term.binary bin -> binaryParser bin parser
        | Term.let' var -> parseLet var parser
        | Term.tuple tuple -> interpretTuple tuple parser
        | Term.first fst -> parserFirst fst parser
        | Term.second snd -> parserSnd snd parser
        | Term.if' ef -> parserIf ef parser
        | Term.call call ->  parserCall call parser
        | Term.func func -> func
        | Term.bool bool' -> bool'.value
        | Term.int int' -> int'.value
        | Term.str str -> str.value
        | Term.print print -> parserPrint print parser Console.WriteLine
        | _ -> failwith "fail on interpreter"

    parser term