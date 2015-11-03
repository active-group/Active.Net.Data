namespace Active.Net.Test.Data

/// test fns of module Active.Net.Guid
module GuidTest =
    open Active.Net
    open FsCheck
    open FsCheck.NUnit
    open FSharp.Data
    open Active.Net.TestUtil
    open FsUnit

    [<QuietProperty>]
    let ``Guid json roundtrip`` (guid:System.Guid) =
        Guid.toJson guid
        |> Guid.fromJson
        |> should equal guid

    [<QuietProperty>]
    let ``Guid string roundtrip`` (guid:System.Guid) =
        Guid.toString guid
        |> Guid.fromString
        |> should equal guid