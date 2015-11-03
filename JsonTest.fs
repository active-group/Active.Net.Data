namespace Active.Net.Test.Data

/// test fns of module Active.Net.Json
module JsonTest =
    open Active.Net.Json
    open FsCheck
    open FsCheck.NUnit
    open FSharp.Data
    
    // properties are named self-explanatory

    [<Property(QuietOnSuccess = true)>]
    let ``toJsonString and fromJsonString are inverses for int arrays-Property`` (value:int[]) = 
        let toJson is = JsonValue.Array (is |> Array.map (fun (i:int) -> JsonValue.Number <| (decimal) i))
        let fromJson (j:JsonValue) = j.AsArray() |> Array.map (fun n -> n.AsInteger())
        (toJsonString toJson value
        |> fromJsonString fromJson) = value
    
    [<Property(QuietOnSuccess = true)>]
    let ``Map's toJson and fromJson are inverses for Map<int,string>-Property`` (value:Map<int,string>) =
        let json = Map.toJson value (fun i -> JsonValue.Number <| (decimal) i) JsonValue.String
        Map.fromJson json (fun (j:JsonValue) -> j.AsInteger()) (fun (j:JsonValue) -> j.AsString()) = value

    [<Property(QuietOnSuccess = true)>]
    let ``Option's toJson and fromJson are inverses for Option<decimal>`` (value:decimal option) =
        let json = Option.toJson JsonValue.Number value
        Option.fromJson (fun (j:JsonValue) -> j.AsDecimal()) json = value