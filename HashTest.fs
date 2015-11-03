namespace Active.Net.Test.Data

/// test fns of module Hash
module HashTest =
    open FsCheck
    open NUnit.Framework
    open FsCheck.NUnit
    open Active.Net.TestUtil
    open Active.Net // Hash
    open FsUnit

    // properties are named self-explanatory

    [<TestFixture>]
    type ``HashTest `` () =

        [<Test>]
        member x.``Hash.bits works`` () =
            let h = Hash.make [| 0x12uy; 0x34uy; 0x56uy; 0x78uy; 0x9auy |]
            Hash.bits h 0 4 |> should equal  0x1
            Hash.bits h 0 8 |> should equal 0x12
            Hash.bits h 1 7 |> should equal 0x12
            Hash.bits h 4 8 |> should equal 0x23
            Hash.bits h 4 12 |> should equal 0x234
            Hash.bits h 4 16 |> should equal 0x2345
            Hash.bits h 5 18 |> should equal 0x11a2b

        [<QuietProperty>]
        member x.``computeSeqHash equals computeHash`` (bytes:byte[][]) =
            let sha_1 () = new System.Security.Cryptography.SHA1Managed() :> Hash.Algorithm // don't know why cast is needed
            let sha_2 () = new System.Security.Cryptography.SHA1Managed()
            let byteSeq = seq { for b in bytes do yield b}
            let allBytes = Array.fold (fun res bytes -> Array.append res bytes) ([||]:byte[]) bytes
            Hash.computeSeqHash sha_1 byteSeq
            |> should equal (sha_2().ComputeHash(allBytes) |> Hash.fromByteArray)
