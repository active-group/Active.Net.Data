namespace Active.Net.Test.Data

module BinaryWriterTest =
    open Active.Net.BinaryWriterUtil
    open FsCheck
    open FsCheck.NUnit
    open FSharp.Data
    open Active.Net.TestUtil
    open FsUnit
    open System // BitConverter
    open System.IO

    let utf8Encoding = new System.Text.UTF8Encoding()
    let writerMem (doit: BinaryWriter -> BinaryWriter) =
        let mem = new MemoryStream()
        use writer = new BinaryWriter(mem, utf8Encoding)
        doit writer |> ignore
        mem.ToArray()

    [<QuietProperty>]
    let ``writeInt32`` (i:int32) =
        let buf = writerMem <| writeInt32 i
        BitConverter.ToInt32(buf,0) |> should equal i

    [<QuietProperty>]
    let ``writeBytes`` (bs:byte[]) =
        let buf = writerMem <| writeBytes bs
        buf |> should equal bs

    [<QuietProperty>]
    let ``writeBytesArray`` (bs:byte[]) =
        let buf = writerMem <| writeBytesArray bs
        BitConverter.ToInt32(buf,0) |> should equal bs.Length
        Array.sub buf 4 (buf.Length - 4) |> should equal bs

    [<QuietProperty>]
    let ``writeString`` (s:string) =
        s <> null ==> lazy
                      (let buf = writerMem <| writeString s
                       let byteCount = (utf8Encoding.GetByteCount s)
                       // last byteCount bytes are the string; don't know how Write(string) encodes string length
                       Array.sub buf (buf.Length - byteCount) byteCount |> should equal (utf8Encoding.GetBytes s))

    [<QuietProperty>]
    let ``writeBytesArrayOfFixedSize`` (count:int) (bs:byte[]) =
        count >= 0 ==> lazy
                       (let bbs = Array.create count bs
                        let buf = writerMem <| writeBytesArrayOfFixedSize bbs
                        BitConverter.ToInt32(buf,0) |> should equal count
                        if count <> 0
                        then
                            BitConverter.ToInt32(buf, 4) |> should equal bs.Length
                            for i in 0..count-1 do
                                Array.sub buf (8 + (bs.Length * i)) bs.Length |> should equal bs)
