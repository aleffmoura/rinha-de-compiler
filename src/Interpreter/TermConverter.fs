module TermConverter

open Newtonsoft.Json
open Newtonsoft.Json.Linq
open System

type TermConvert() =
    inherit JsonConverter()

    override __.CanConvert(objectType: Type) =
        true

    override __.ReadJson(reader: JsonReader, objectType: Type, existingValue: obj, serializer: JsonSerializer) =
        let jsonObject = JObject.Load(reader)
        let term = Converter.parse jsonObject
        serializer.Populate(jsonObject.CreateReader(), term)
        term

    override __.WriteJson(writer: JsonWriter, value: obj, serializer: JsonSerializer) =
        failwith "Not implemented"