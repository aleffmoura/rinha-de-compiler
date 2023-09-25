open ast
open System.IO
open System
open Newtonsoft.Json
open parser
open TermConverter
open System.Diagnostics

let measureExecutionTime action =
    let stopwatch = Stopwatch()
    stopwatch.Start()
    let result = action()
    stopwatch.Stop()
    printfn "Tempo decorrido: %f segundos" (stopwatch.Elapsed.TotalSeconds)
    result

let runFile filePath  =
    let serializerSettings = JsonSerializerSettings()
    serializerSettings.Converters.Add(TermConvert()) 
    let jsonSerializer = JsonSerializer.Create(serializerSettings)
    
    let fileContent = File.ReadAllText(filePath)
    use stringReader = new System.IO.StringReader(fileContent)
    let jsonReader = new JsonTextReader(stringReader)

    match jsonSerializer.Deserialize<Term>(jsonReader) with
    | Term.file file ->
       measureExecutionTime (fun () -> interpreter file.expression) 
    | _ -> new obj()
    |> ignore

let findAndRunRinhaFiles (args: string array) =
    let path =
        if Directory.Exists(args.[0]) then
            args.[0]
        else
            $"{AppContext.BaseDirectory}/src_json"

    Directory.GetFiles (path, "*.rinha.json")
    |> Array.iter runFile 


[<EntryPoint>]
let main args =
    findAndRunRinhaFiles args
    0