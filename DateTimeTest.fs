namespace Active.Net.Test.Data

/// test fns of module DateTime
module DateTimeTest =
    open FsCheck
    open FsCheck.NUnit
    open Active.Net.TestUtil
    open Active.Net // DateTime
    open FsUnit

    // properties are named self-explanatory

    [<QuietProperty>]
    let ``DateTime ISO 8601 string round-trip`` (dt: DateTime.T) =
        DateTime.toIso8601String dt
        |> DateTime.fromIso8601String
        |> should equal dt