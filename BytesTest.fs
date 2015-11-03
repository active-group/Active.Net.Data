namespace Active.Net.Test.Data

/// test fns of module Bytes
module BytesTest =
    open FsCheck
    open NUnit.Framework
    open FsCheck.NUnit
    open Active.Net.TestUtil
    open Active.Net // Bytes
    open FsUnit

    // properties are named self-explanatory

    [<TestFixture>]
    type ``HashTest `` () =

        [<Test>]
        member x.``Bytes.bits works`` () =
            let x = [| 0x12uy; 0x34uy; 0x56uy; 0x78uy; 0x9auy; 0xbcuy; 0xdeuy; 0xf1uy; 0x23uy |]
            Bytes.bits x 0 4 |> should equal 0x1
            Bytes.bits x 0 8 |> should equal 0x12
            Bytes.bits x 1 7 |> should equal 0x12
            Bytes.bits x 4 8 |> should equal 0x23
            Bytes.bits x 4 12 |> should equal 0x234
            Bytes.bits x 4 16 |> should equal 0x2345
            Bytes.bits x 5 18 |> should equal 0x11a2b
            Bytes.bits x 5 35 |> should equal 0x23456789aUL
            Bytes.bits x 4 64 |> should equal 0x23456789abcdef12UL
            let x = [| 0x1fuy; 0x34uy; 0x56uy; 0x78uy; 0x9auy; 0xbcuy; 0xdeuy; 0xf1uy; 0x23uy |]
            // make sure we have an msb 
            Bytes.bits x 4 64 |> should equal 0xf3456789abcdef12UL