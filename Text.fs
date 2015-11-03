namespace Active.Net

/// utility fns to manipulate text
module Text =
    let utf8Encoding = new System.Text.UTF8Encoding(false, true) // no BOM, throw on invalid bytes
    let utf8Bytes (s:string) = utf8Encoding.GetBytes(s)
    let utf8String (utf8bytes:byte[]) = utf8Encoding.GetString utf8bytes