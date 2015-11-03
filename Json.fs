namespace Active.Net

/// helper fns operating on FSharp.Data.JsonValue
module Json =
    open FSharp.Data
    /// turn a into a json string in one step (w/o formatting)
    let toJsonString (toJson:'a -> JsonValue) (a:'a) =
        (toJson a).ToString(JsonSaveOptions.DisableFormatting)
    /// create an 'a from a json string in one step
    let fromJsonString (fromJson: JsonValue -> 'a) (s:string) =
        JsonValue.Parse(s) |> fromJson
    let toString (j:JsonValue) = j.ToString(JsonSaveOptions.DisableFormatting)
    let fromString (s:string) = JsonValue.Parse(s)
    /// <summary> convert JsonValue to string <para/>
    /// will fail if v is not a JsonValue.String  </summary>
    let asString (v:JsonValue): string = v.AsString()

    let asArray (v: JsonValue): JsonValue[] = v.AsArray()
    let asInt (v: JsonValue): int = v.AsInteger()
    let asDateTime (v: JsonValue): System.DateTime = v.AsDateTime()
    let asBoolean (v: JsonValue): bool = v.AsBoolean()

    module Record =
        let fromJson (j: JsonValue): Map<string, JsonValue> =
            match j with
            | JsonValue.Record props -> Map.ofArray props
            | _ -> failwith "JSON ist kein Objekt { ... }"

    /// helper fns to handle JsonValue of Maps
    module Map =
        /// convert map to JsonValue
        let toJson (map: Map<'K,'V>) keyToJson valueToJson =
            [| for i in map -> [|keyToJson i.Key; valueToJson i.Value|] |]
            |> Array.map JsonValue.Array
            |> JsonValue.Array

        /// convert JsonValue to a map
        let fromJson (j:JsonValue) (keyFromJson: JsonValue -> 'K) (valueFromJson: JsonValue -> 'V) : Map<'K,'V> =
            j.AsArray()
            |> Array.map
                (fun kvj -> let kv = kvj.AsArray()
                            (keyFromJson kv.[0], valueFromJson kv.[1]))
            |> Map.ofArray

    /// helper fns to handle JsonValue of Option
    module Option =
        /// convert option v to JsonValue
        let toJson valueToJson (v: 'a option) =
            match v with
            | None -> JsonValue.Array [||]
            | Some v -> JsonValue.Array [| valueToJson v |]

        /// convert JsonValue to Option
        let fromJson valueFromJson (j:JsonValue) =
            match j.AsArray() with
            | [||] -> None
            | [| vj |] -> valueFromJson vj |> Some
            | _ -> failwith "too many elements in Option json"