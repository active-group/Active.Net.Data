namespace Active.Net

/// helper fns for functional API to System.Guid
module Guid =
    open FSharp.Data
    type T = System.Guid
    let newGuid = System.Guid.NewGuid
    
    [<Literal>]
    let GUID_STRING_FORMAT = "D"  // 32 digits separated by hyphens: 00000000-0000-0000-0000-000000000000 

    /// convert Guid to JsonValue
    let toJson (g:System.Guid) = g.ToString(GUID_STRING_FORMAT) |> JsonValue.String 
    /// convert JsonValue to Guid
    let fromJson (j:JsonValue) = System.Guid.ParseExact(j.AsString(), GUID_STRING_FORMAT)

    /// convert Guid to string, canonically (eg. '00000000-0000-0000-0000-000000000000')
    let toString (g:T) = g.ToString(GUID_STRING_FORMAT)
    /// parse string as Guid, canonically (eg. '00000000-0000-0000-0000-000000000000')
    let fromString (s:string) = System.Guid.ParseExact(s, GUID_STRING_FORMAT)

    /// bytes
    let toByteArray (g:T) = g.ToByteArray()
    /// from 16 bytes
    let fromByteArray (bs:byte[]) = new System.Guid(bs)
