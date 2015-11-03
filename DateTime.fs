namespace Active.Net

module DateTime =
    type T = System.DateTimeOffset  // unlike DateTime, DateTimeOffset has no wierd 'Kind' property
    [<Literal>]
    let ISO8601_DATETIME_FORMAT = "o"

    let now () = System.DateTimeOffset.Now
    let utcNow () = System.DateTimeOffset.UtcNow

    let toIso8601String (dt:T) =
        dt.ToString(ISO8601_DATETIME_FORMAT)

    let fromIso8601String (s:string) : T =
        System.DateTimeOffset.ParseExact(s, ISO8601_DATETIME_FORMAT, System.Globalization.CultureInfo.InvariantCulture)
